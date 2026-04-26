module Hfsm.Migrate.Run
  ( migrateInstance
  , migrateBatch
  ) where

import Data.Aeson (Value, object, (.=))
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime, addUTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as U
import Data.Word (Word32)

import Hfsm.Compile (CompiledMachine (..))
import Hfsm.Core.Codec (CodecSet (..), renderCodecErr, validateWith)
import Hfsm.Core.Digest (specDigestText)
import Hfsm.Core.Name (machineNameText)
import Hfsm.Core.Ref (RouteRef, StateRef, mkRouteRef, stateRefWord32)
import Hfsm.Core.Version (machineVersionText)
import Hfsm.Graph.Def (MachineGraph (..), StateNd (..))
import qualified Hfsm.Migrate.Plan as Plan
import Hfsm.Migrate.Plan (MigrateErr, MigratePlan)
import Hfsm.Runtime.Model (InstanceRw (..), InstanceStatus(..), SnapshotRw (..), StepKind(..))
import qualified Hfsm.Runtime.Registry as Registry
import Hfsm.Runtime.Registry (Registry)
import Hfsm.Spec.Def (MachineSpec(..), StateKind(..))
import Hfsm.Store.Class (ProjBatch(..), SnapshotAppend(..), StepAppend(..), Store(..))

data PreparedMigration = PreparedMigration
  { step :: StepAppend
  , snapshot :: SnapshotAppend
  }


migrateInstance :: Monad m => Store m -> Registry st sg hf ctx cmd child -> MigratePlan -> UUID -> m (Either MigrateErr UUID)
migrateInstance store registry plan instUuid =
  case assertPlanSupported plan of
    Left err -> pure (Left err)
    Right () -> do
      maybeInst <- store.loadInstance instUuid
      case maybeInst of
        Nothing ->
          pure (Left (Plan.incompatibleErr ("instance not found: " <> renderUuid instUuid)))

        Just instRw ->
          if isTerminalStatus instRw.status
            then pure (Left (Plan.incompatibleErr ("cannot migrate a terminal instance: " <> renderUuid instUuid)))
            else do
              maybeSnap <- store.loadSnapshot instRw.snap
              case maybeSnap of
                Nothing ->
                  pure (Left (Plan.incompatibleErr ("current snapshot not found for instance " <> renderUuid instUuid <> ": " <> renderUuid instRw.snap)))

                Just snapRw ->
                  case prepareMigration registry plan instRw snapRw of
                    Left err ->
                      pure (Left err)

                    Right prepared -> do
                      _ <- store.appendSnapshot prepared.snapshot
                      _ <- store.appendStep prepared.step
                      store.updateProj emptyProjBatch
                      pure (Right instUuid)

migrateBatch :: Monad m => Store m -> Registry st sg hf ctx cmd child -> MigratePlan -> [UUID] -> m [Either MigrateErr UUID]
migrateBatch store registry plan =
  traverse (migrateInstance store registry plan)

prepareMigration :: Registry st sg hf ctx cmd child -> MigratePlan -> InstanceRw -> SnapshotRw -> Either MigrateErr PreparedMigration
prepareMigration registry plan instRw snapRw = do
  assertInstanceMatchesPlan plan instRw
  assertSnapshotMatchesInstance plan instRw snapRw
  sourceCm <- resolveCompiled "source" registry plan.from
  targetCm <- resolveCompiled "target" registry plan.to
  validateSourceCtx sourceCm plan snapRw.ctx
  migratedCtx <- Plan.applyCtx snapRw.ctx plan
  validateTargetCtx targetCm plan migratedCtx
  migratedState <- Plan.applyState snapRw.state migratedCtx plan
  targetStateNd <- lookupTargetState targetCm migratedState
  assertSupportedTargetState targetStateNd
  migratedWait <- Plan.applyWait snapRw.wait plan
  let now = migrationTime instRw snapRw
      snapshotAppend = buildSnapshotAppend plan instRw snapRw targetStateNd migratedCtx migratedWait now
      stepAppend = buildStepAppend plan instRw snapRw targetStateNd migratedCtx migratedWait now
  pure PreparedMigration
    { step = stepAppend
    , snapshot = snapshotAppend
    }

assertPlanSupported :: MigratePlan -> Either MigrateErr ()
assertPlanSupported plan
  | not (Plan.sameMachineName plan.from plan.to) =
      Left (Plan.incompatibleErr ("migration across machine names is unsupported under the current store contract: " <> Plan.renderMachineKey plan.from <> " -> " <> Plan.renderMachineKey plan.to))
  | not (Plan.compatiblePlan plan) =
      Left (incompatiblePlanErr plan)
  | otherwise =
      Right ()

assertInstanceMatchesPlan :: MigratePlan -> InstanceRw -> Either MigrateErr ()
assertInstanceMatchesPlan plan instRw
  | instRw.machine /= plan.from.name =
      Left (Plan.incompatibleErr ("instance machine does not match migration source; expected " <> Plan.renderMachineKey plan.from <> ", actual " <> renderInstanceKey instRw))
  | instRw.version /= plan.from.version =
      Left (Plan.incompatibleErr ("instance version does not match migration source; expected " <> Plan.renderMachineKey plan.from <> ", actual " <> renderInstanceKey instRw))
  | instRw.digest /= plan.from.digest =
      Left (Plan.incompatibleErr ("instance digest does not match migration source; expected " <> Plan.renderMachineKey plan.from <> ", actual " <> renderInstanceKey instRw))
  | otherwise =
      Right ()

assertSnapshotMatchesInstance :: MigratePlan -> InstanceRw -> SnapshotRw -> Either MigrateErr ()
assertSnapshotMatchesInstance plan instRw snapRw
  | snapRw.inst /= instRw.uuid =
      Left (Plan.incompatibleErr ("snapshot " <> renderUuid snapRw.uuid <> " does not belong to instance " <> renderUuid instRw.uuid))
  | snapRw.version /= instRw.version || snapRw.digest /= instRw.digest =
      Left (Plan.incompatibleErr ("instance header and current snapshot disagree on version or digest for instance " <> renderUuid instRw.uuid))
  | snapRw.version /= plan.from.version || snapRw.digest /= plan.from.digest =
      Left (Plan.incompatibleErr ("snapshot does not match migration source; expected " <> Plan.renderMachineKey plan.from <> ", actual version=" <> machineVersionText snapRw.version <> ", digest=" <> specDigestText snapRw.digest))
  | snapRw.state /= instRw.state || snapRw.path /= instRw.path =
      Left (Plan.incompatibleErr ("instance header and current snapshot disagree on control state for instance " <> renderUuid instRw.uuid))
  | otherwise =
      Right ()


resolveCompiled :: Text -> Registry st sg hf ctx cmd child -> Plan.MachineKey -> Either MigrateErr (CompiledMachine st sg hf ctx cmd child)
resolveCompiled role registry key =
  case Registry.resolve (toRegistryKey key) registry of
    Nothing -> Left (Plan.incompatibleErr ("missing " <> role <> " machine in registry: " <> Plan.renderMachineKey key))
    Just compiled -> Right compiled


validateSourceCtx :: CompiledMachine st sg hf ctx cmd child -> MigratePlan -> Value -> Either MigrateErr ()
validateSourceCtx sourceCm plan ctxVal =
  case validateWith sourceCm.spec.codecs.ctx ctxVal of
    Left err ->
      Left (Plan.ctxErr ("source context does not validate against " <> Plan.renderMachineKey plan.from <> ": " <> renderCodecErr err))

    Right () ->
      Right ()

validateTargetCtx :: CompiledMachine st sg hf ctx cmd child -> MigratePlan -> Value -> Either MigrateErr ()
validateTargetCtx targetCm plan ctxVal =
  case validateWith targetCm.spec.codecs.ctx ctxVal of
    Left err ->
      Left (Plan.ctxErr ("migrated context does not validate against " <> Plan.renderMachineKey plan.to <> ": " <> renderCodecErr err))

    Right () ->
      Right ()

lookupTargetState :: CompiledMachine st sg hf ctx cmd child -> StateRef -> Either MigrateErr StateNd
lookupTargetState targetCm stateRef =
  case lookupStateNd targetCm.graph stateRef of
    Nothing ->
      Left (Plan.stateErr stateRef ("target state does not exist in compiled target graph for " <> Plan.renderMachineKey (Plan.mkMachineKey targetCm.spec.name targetCm.spec.version targetCm.digest)))

    Just stateNd ->
      Right stateNd

assertSupportedTargetState :: StateNd -> Either MigrateErr ()
assertSupportedTargetState stateNd =
  case stateNd.kind of
    TerminalSk ->
      Left (Plan.stateErr stateNd.ref "migration to a terminal state is unsupported under the current store contract because instance terminal status cannot be rewritten through SnapshotAppend")

    AtomicSk ->
      Right ()

    CompositeSk ->
      Right ()

buildSnapshotAppend :: MigratePlan -> InstanceRw -> SnapshotRw -> StateNd -> Value -> Maybe Value -> UTCTime -> SnapshotAppend
buildSnapshotAppend plan instRw snapRw targetStateNd migratedCtx migratedWait now =
  SnapshotAppend
    { inst = instRw.uuid
    , state = targetStateNd.ref
    , path = targetStateNd.path
    , ctx = migratedCtx
    , wait = migratedWait
    , child = snapRw.child
    , version = plan.to.version
    , digest = plan.to.digest
    , now = now
    }

buildStepAppend :: MigratePlan -> InstanceRw -> SnapshotRw -> StateNd -> Value -> Maybe Value -> UTCTime -> StepAppend
buildStepAppend plan instRw snapRw targetStateNd migratedCtx migratedWait now =
  StepAppend
    { inst = instRw.uuid
    , signal = Nothing
    , kind = MigrateSk
    , from = snapRw.state
    , to = targetStateNd.ref
    , caseRef = Nothing
    , route = syntheticRouteRef
    , note = migrationNote plan instRw snapRw targetStateNd migratedCtx migratedWait
    , now = now
    }

migrationNote :: MigratePlan -> InstanceRw -> SnapshotRw -> StateNd -> Value -> Maybe Value -> Value
migrationNote plan instRw snapRw targetStateNd migratedCtx migratedWait =
  object
    [ "kind" .= ("migrate" :: Text)
    , "instance" .= renderUuid instRw.uuid
    , "fromMachine" .= machineKeyValue plan.from
    , "toMachine" .= machineKeyValue plan.to
    , "fromState" .= snapRw.state
    , "toState" .= targetStateNd.ref
    , "fromPath" .= snapRw.path
    , "toPath" .= targetStateNd.path
    , "ctxChanged" .= (snapRw.ctx /= migratedCtx)
    , "waitChanged" .= (snapRw.wait /= migratedWait)
    , "diffCount" .= length plan.diffs
    , "syntheticRouteRef" .= syntheticRouteRef
    ]

machineKeyValue :: Plan.MachineKey -> Value
machineKeyValue key =
  object
    [ "name" .= key.name
    , "version" .= key.version
    , "digest" .= key.digest
    ]

emptyProjBatch :: ProjBatch
emptyProjBatch =
  ProjBatch
    { active = []
    , trace = []
    , queue = []
    , tree = []
    }

toRegistryKey :: Plan.MachineKey -> Registry.MachineKey
toRegistryKey key =
  Registry.MachineKey
    { name = key.name
    , version = key.version
    , digest = key.digest
    }

lookupStateNd :: MachineGraph -> StateRef -> Maybe StateNd
lookupStateNd graph stateRef =
  IM.lookup (stateRefKey stateRef) graph.states

migrationTime :: InstanceRw -> SnapshotRw -> UTCTime
migrationTime instRw snapRw =
  addUTCTime 1 (max instRw.updatedAt snapRw.createdAt)

syntheticRouteRef :: RouteRef
syntheticRouteRef = mkRouteRef (maxBound :: Word32)

isTerminalStatus :: InstanceStatus -> Bool
isTerminalStatus status =
  case status of
    DoneIs -> True
    FailedIs -> True
    CancelledIs -> True
    RunningIs -> False
    WaitingIs -> False
    PausedIs -> False

incompatiblePlanErr :: MigratePlan -> MigrateErr
incompatiblePlanErr plan =
  case Plan.incompatibleDiffs plan of
    [] ->
      Plan.incompatibleErr ("migration plan is not compatible: " <> Plan.renderMachineKey plan.from <> " -> " <> Plan.renderMachineKey plan.to)

    diffs ->
      Plan.incompatibleErr ("migration plan contains incompatible diffs: " <> renderDiffs diffs)

renderDiffs :: Show a => [a] -> Text
renderDiffs xs =
  T.intercalate ", " (map (T.pack . show) xs)

renderInstanceKey :: InstanceRw -> Text
renderInstanceKey instRw =
  machineNameText instRw.machine <> "@" <> machineVersionText instRw.version <> "#" <> specDigestText instRw.digest

renderUuid :: UUID -> Text
renderUuid = U.toText

stateRefKey :: StateRef -> Int
stateRefKey = fromIntegral . stateRefWord32