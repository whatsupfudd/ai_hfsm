-- | Pure audit replay: executes planStep, never commitStep, never providers.
-- Historical signal clocks and causes come from hfsm.turn_input, not from
-- the current mutable hfsm.signal row.
module Hfsm.History.Replay (verifyTurn, verifyTimeline) where

import Data.Aeson (Value(..), ToJSON, object, toJSON, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as UUID

import Hfsm.Runtime.Engine.Step (StepPlan(..), StepInput(..), planStep)
import qualified Hfsm.Runtime.Model.Step as Mst
import qualified Hfsm.Runtime.Model.Snapshot as Msn
import qualified Hfsm.Runtime.Model.Signal as Msi
import Hfsm.Core.Name (machineNameText)
import Hfsm.Core.Version (machineVersionText)
import Hfsm.Runtime.Model (StepKind(..), SnapshotRw(..), StepRw(..), InstanceRw (..))
import Hfsm.Runtime.Registry (Registry, resolve, MachineKey (..))
import Hfsm.History.Types
  ( ReplayInput(..), asStepInput, HistoryTurn(..), MigrationEvidence(..)
  , ReplayErr(..), ReplayResult(..) )
import Hfsm.Store.Class (OutboxAppend(..), ChildAppend(..), BreakAppend(..), ProjBatch(..))
import Hfsm.Runtime.Engine.Step (StepPlan(..))


verifyTimeline :: Show hf
               => Registry st sg hf ctx cmd child
               -> [HistoryTurn]
               -> Either ReplayErr [ReplayResult]
verifyTimeline registry = go Nothing
 where
  go _ [] = Right []
  go previous (turn:rest) = do
    case previous of
      Nothing -> pure ()
      Just (rev, snap)
        | turn.revision /= rev + 1 ->
            Left (ReplayInvalidHistory turn.revision "non-contiguous revisions")
        | turn.sourceSnapshotId /= snap ->
            Left (ReplayInvalidHistory turn.revision "source snapshot not previous revision")
        | otherwise -> pure ()
    here <- verifyTurn registry turn
    later <- go (Just (turn.revision,turn.snapshot.uuid)) rest
    pure (here:later)

verifyTurn :: Show hf => Registry st sg hf ctx cmd child -> HistoryTurn -> Either ReplayErr ReplayResult
verifyTurn registry turn
  | turn.step.kind == MigrateSk = verifyMigration turn
  | otherwise = do
      input <- maybe (Left (ReplayMissingInput turn.revision)) Right turn.input
      insist turn.revision "source snapshot mismatch"
        turn.sourceSnapshotId input.snapshot.uuid
      insist turn.revision "input/step instance mismatch"
        turn.step.inst input.instance_.uuid
      insist turn.revision "input/step signal mismatch"
        turn.step.signal (Just input.signal.uuid)
      insist turn.revision "input expected revision mismatch"
        (input.instance_.revision + 1) turn.revision
      compiled <- maybe
        (Left (ReplayUnknownRelease turn.revision input.machine))
        Right (resolve input.machine registry)
      planned <- case planStep compiled (asStepInput input) of
        Left err -> Left (ReplayPlannerFailure turn.revision (T.pack (show err)))
        Right plan -> Right plan
      -- Generated relational uid is not part of the pure plan.
      insist turn.revision "step identity/handoff/route/note"
        (toJSON (planned.step { Mst.uid = turn.step.uid })) (toJSON turn.step)
      insist turn.revision "state/context/control movement"
        (toJSON (planned.snapshot { Msn.uid = turn.snapshot.uid }))
        (toJSON turn.snapshot)
      insist turn.revision "instance nextStatus versus durable step.note"
        (toJSON planned.nextStatus) (noteField "status" turn.step.note)
      insist turn.revision "outbox provider keys and encoded commands"
        (expectedOutbox planned.step.uuid planned.outbox) turn.outbox
      insist turn.revision "child spawn identities"
        (expectedChildren planned.child) turn.children
      insist turn.revision "breakpoint occurrences"
        (expectedBreaks planned.breaks) turn.breaks
      insist turn.revision "append-only projection events"
        (toJSON planned.proj) (toJSON turn.proj)
      case turn.migration of
        Nothing -> pure ()
        Just _ -> Left (ReplayInvalidHistory turn.revision "ordinary turn contains migration evidence")
      Right (ReplayVerified turn.revision)

verifyMigration :: HistoryTurn -> Either ReplayErr ReplayResult
verifyMigration turn = do
  ev <- maybe (Left (ReplayInvalidHistory turn.revision "migration missing audit"))
              Right turn.migration
  insist turn.revision "migration source snapshot" turn.sourceSnapshotId ev.sourceSnapshotId
  insist turn.revision "migration result snapshot" turn.snapshot.uuid ev.targetSnapshotId
  insist turn.revision "migration new state" turn.step.to turn.snapshot.state
  insist turn.revision "migration timestamp" ev.createdAt turn.step.createdAt
  insist turn.revision "migration snapshot timestamp" ev.createdAt turn.snapshot.createdAt
  insist turn.revision "migration source release note"
    (toJSON ev.sourceKey) (noteField "sourceKey" turn.step.note)
  insist turn.revision "migration destination release note"
    (toJSON ev.targetKey) (noteField "targetKey" turn.step.note)
  insist turn.revision "migration source/target machine name"
    ev.sourceKey.name ev.targetKey.name
  insist turn.revision "migration result context" ev.newCtx turn.snapshot.ctx
  insist turn.revision "migration result version" ev.targetKey.version turn.snapshot.version
  insist turn.revision "migration result digest" ev.targetKey.digest turn.snapshot.digest
  insist turn.revision "migration note operation"
    (toJSON ev.operationId) (noteField "operationId" turn.step.note)
  insist turn.revision "migration note transformation"
    (toJSON ev.mappingName) (noteField "mappingName" turn.step.note)
  case turn.input of
    Nothing -> pure ()
    Just _ -> Left (ReplayInvalidHistory turn.revision "migration cannot have ordinary turn input")
  insist turn.revision "migration effect-free" ([] :: [Value]) turn.outbox
  insist turn.revision "migration child-free" ([] :: [Value]) turn.children
  insist turn.revision "migration breakpoint-free" ([] :: [Value]) turn.breaks
  insist turn.revision "migration projections"
    (toJSON (migrationProjections turn ev)) (toJSON turn.proj)
  Right (ReplayMigrationVerified turn.revision)

migrationProjections :: HistoryTurn -> MigrationEvidence -> ProjBatch
migrationProjections turn ev = ProjBatch
  { active = [object
    [ "instance_" .= turn.step.inst
    , "machine" .= machineNameText ev.targetKey.name
    , "version" .= machineVersionText ev.targetKey.version
    , "status" .= noteField "status" turn.step.note
    , "state" .= turn.snapshot.state
    , "path" .= turn.snapshot.path
    , "wait" .= turn.snapshot.wait
    , "blockedOn" .= (Nothing :: Maybe Text)
    , "updatedAt" .= ev.createdAt ]]
  , trace = [object
    [ "inst" .= turn.step.inst, "step" .= turn.step.uuid
    , "signal" .= (Nothing :: Maybe UUID.UUID)
    , "from" .= turn.step.from, "to" .= turn.step.to
    , "route" .= (0 :: Int), "note" .= turn.step.note
    , "createdAt" .= ev.createdAt ]]
  , queue = [], tree = [] }

expectedOutbox :: UUID.UUID -> [OutboxAppend] -> [Value]
expectedOutbox stepId = zipWith toValue [0 :: Int ..]
  where
  toValue :: Int -> OutboxAppend -> Value
  toValue ix eff = object
    [ "effectKey" .= (UUID.toText stepId <> ":" <> T.pack (show ix))
    , "payload" .= eff.payload ]

expectedChildren :: [ChildAppend] -> [Value]
expectedChildren = map (\ch -> object
  [ "child" .= ch.child, "key" .= ch.key, "join" .= ch.join ])

expectedBreaks :: [BreakAppend] -> [Value]
expectedBreaks = map (\br -> object
  [ "kind" .= T.pack (show br.kind), "state" .= br.state, "note" .= br.note ])

noteField :: Text -> Value -> Value
noteField name (Object obj) = maybe Null id (KM.lookup (K.fromText name) obj)
noteField _ _ = Null

insist :: (Eq a, ToJSON a) => Int64 -> Text -> a -> a -> Either ReplayErr ()
insist revision area expected actual
  | expected == actual = Right ()
  | otherwise = Left (ReplayDivergence revision area (toJSON expected) (toJSON actual))
