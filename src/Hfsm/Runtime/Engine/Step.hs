{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE StrictData #-}

module Hfsm.Runtime.Engine.Step
  ( EngineErr(..)
  , StepPlan(..)
  , StepInput(..)
  , planStep
  , commitStep
  , runStep
  , renderEngineErr
  ) where

import Control.Arrow ((***))

import Data.Bifunctor (first)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (find, foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, isJust)
import qualified Data.Scientific as Sci
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import Data.Time (NominalDiffTime, UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as U
import qualified Data.Vector as V
import Data.Word (Word32, Word64)

import GHC.Generics (Generic)

import Data.Aeson (Value(..), object, (.=))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM

import Hfsm.Compile (CompiledMachine(..))
import Hfsm.Compile.Exec
  ( CaseExec(..)
  , findMatchingCase
  , lookupEntryExec
  , runCaseExec
  , runEntryExec
  )
import Hfsm.Core.Codec (Codec (..), CodecSet (..),CodecErr(..))
import Hfsm.Core.Digest (SpecDigest (..), specDigestText)
import Hfsm.Core.Name
  ( MachineName
  , JoinName
  , TimerName
  , machineNameText
  , joinNameText
  , timerNameText
  )
import Hfsm.Core.Path (StatePath (..), renderStatePath)
import Hfsm.Core.Ref
  ( CaseRef
  , JoinRef
  , RegionRef
  , RouteRef
  , StateRef
  , TimerRef
  , caseRefWord32
  , joinRefWord32
  , regionRefWord32
  , routeRefWord32
  , stateRefWord32
  , timerRefWord32
  )
import Hfsm.Core.Version (MachineVersion (..), machineVersionText)
import Hfsm.Graph.Def
  ( BreakPlanG(..)
  , JoinPlanG(..)
  , JoinNd(..)
  , MachineGraph(..)
  , RegionNd(..)
  , RouteEd(..)
  , RoutePlan(..)
  , RouteTarget(..)
  , SpawnPlanG(..)
  , StateNd(..)
  , TimerNd(..)
  , TimerPlanG(..)
  , WaitPlanG(..)
  )
import Hfsm.Runtime.Model
  ( BreakKind(..)
  , BreakRw(..)
  , BreakStatus(..)
  , ChildStatus(..)
  , InstanceRw(..)
  , InstanceStatus(..)
  , SignalRw(..)
  , SignalStatus(..)
  , SnapshotRw(..)
  , StepKind(..)
  , StepRw(..)
  )
import Hfsm.Runtime.Registry (MachineKey(..), machineKeyOf)
import Hfsm.Spec.Case (ReactEnv(..), ReactErr(..), ReactRez(..))
import Hfsm.Spec.Def (MachineSpec(..))
import Hfsm.Store.Class
  ( AckRq(..)
  , BreakAppend(..)
  , OutboxAppend(..)
  , ChildAppend(..)
  , ProjBatch(..)
  , SnapshotAppend(..)
  , StepAppend(..)
  , Store(..)
  , CommitRez(..)
  )
import Control.Applicative ((<|>))


data EngineErr =
    MachineKeyMismatchEr MachineKey MachineKey
  | InstanceMachineMismatchEr MachineName MachineVersion SpecDigest MachineName MachineVersion SpecDigest
  | SnapshotMachineMismatchEr MachineVersion SpecDigest MachineVersion SpecDigest
  | SnapshotInstanceMismatchEr UUID UUID
  | SignalInstanceMismatchEr UUID UUID
  | InstanceStateMismatchEr StateRef StateRef
  | InstancePathMismatchEr StatePath StatePath
  | MissingStateEr StateRef
  | MissingRegionEr RegionRef
  | MissingEntryHandlerEr StateRef
  | MissingEntryExecEr StateRef CaseRef
  | DecodeCtxEr CodecErr
  | DecodeSignalEr CodecErr
  | NoMatchingCaseEr StateRef
  | ReactEr ReactErr
  | MissingRouteEr StateRef Text
  deriving stock (Eq, Show, Read, Generic)

data StepPlan = StepPlan
  { step :: StepRw
  , snapshot :: SnapshotRw
  , outbox :: [OutboxAppend]
  , child :: [ChildAppend]
  , breaks :: [BreakAppend]
  , proj :: ProjBatch
  }
  deriving stock (Eq, Show, Read, Generic)

data StepInput = StepInput
  { machine :: MachineKey
  , instance_ :: InstanceRw
  , snapshot :: SnapshotRw
  , signal :: SignalRw
  }
  deriving stock (Eq, Show, Read, Generic)

data ReactionStep ctx hf cmd = ReactionStep
  { kind :: StepKind
  , caseRef :: Maybe CaseRef
  , rez :: ReactRez ctx hf cmd
  }

data PlannedChild = PlannedChild
  { uuid :: UUID
  , key :: Text
  , append :: ChildAppend
  , store :: Value
  , outbox :: OutboxAppend
  }

planStep :: Show hf => CompiledMachine st sg hf ctx cmd child -> StepInput -> Either EngineErr StepPlan
planStep compiled input = do
  ensureInputMatches compiled input

  currentSt <- lookupStateOrErr compiled.graph input.snapshot.state
  ctxVal <- first DecodeCtxEr $ compiled.spec.codecs.ctx.decode input.snapshot.ctx
  reaction <- resolveReaction compiled currentSt (mkReactEnvFrom input.snapshot input.signal) ctxVal input.signal

  let handoffTxt = renderHandoff reaction.rez.handoff
  routeEd <- resolveRoute compiled.graph currentSt reaction.caseRef handoffTxt
  nextSt <- resolveTargetState compiled.graph currentSt routeEd.plan.target

  let waitVal = buildWaitValue compiled.graph routeEd.plan.wait routeEd.plan.timers
      plannedChildren = buildChildPlans input routeEd
      breakAppends = buildBreakAppends input routeEd currentSt.ref nextSt.ref
      nextStatus = nextStatusOf routeEd.plan.target waitVal breakAppends
      noteVal = buildStepNote input handoffTxt routeEd reaction nextStatus waitVal breakAppends
      now = input.signal.updatedAt

      (childMap0, timerMap0) = decodeChildStore input.snapshot.child
      childMap1 = if isTerminalTarget routeEd.plan.target
        then M.empty
        else foldl' (\acc x -> M.insert x.key x.store acc) childMap0 plannedChildren
      timerMap1 = if isTerminalTarget routeEd.plan.target
        then M.empty
        else foldl' (\acc x -> M.insert (timerStoreKey compiled.graph x.ref) (timerStoreValue compiled.graph x) acc) timerMap0 routeEd.plan.timers

      nextSnapshot =
        SnapshotRw
          { uid = 0
          , uuid = U.nil
          , inst = input.instance_.uuid
          , state = nextSt.ref
          , path = nextSt.path
          , ctx = compiled.spec.codecs.ctx.encode reaction.rez.next
          , wait = waitVal
          , child = encodeChildStore childMap1 timerMap1
          , version = compiled.spec.version
          , digest = compiled.digest
          , createdAt = now
          }

      stepRw =
        StepRw
          { uid = 0
          , uuid = U.nil
          , inst = input.instance_.uuid
          , signal = Just input.signal.uuid
          , kind = reaction.kind
          , from = currentSt.ref
          , to = nextSt.ref
          , caseRef = reaction.caseRef
          , route = routeEd.ref
          , note = noteVal
          , createdAt = now
          }

      cmdOutbox = buildCmdOutboxAppends compiled now reaction.rez.cmds
      timerOutbox = buildTimerOutboxAppends compiled.graph now routeEd.plan.timers
      childOutbox = fmap (.outbox) plannedChildren
      projBatch = buildProjBatch stepRw nextSnapshot breakAppends
      childAppends = fmap (.append) plannedChildren

  pure
    StepPlan
      { step = stepRw
      , snapshot = nextSnapshot
      , outbox = cmdOutbox <> childOutbox <> timerOutbox
      , child = childAppends
      , breaks = breakAppends
      , proj = projBatch
      }

commitStep :: Monad m => Store m -> StepPlan -> m CommitRez
commitStep store plan = do
  stepRw <- store.appendStep (mkStepAppend plan.step)
  snapshotRw <- store.appendSnapshot (mkSnapshotAppend plan.snapshot)

  let outboxAppends = fmap (setOutboxStep stepRw.uuid) plan.outbox
      projBatch = buildProjBatch stepRw snapshotRw plan.breaks

  store.appendOutbox outboxAppends
  store.appendChild plan.child
  store.appendBreak plan.breaks
  store.updateProj projBatch

  pure
    CommitRez
      { step = stepRw
      , snapshot = snapshotRw
      }

runStep :: (Monad m, Show hf) => Store m -> CompiledMachine st sg hf ctx cmd child -> StepInput -> m (Either EngineErr CommitRez)
runStep store compiled input =
  case planStep compiled input of
    Left err -> do
      ackSignalIfLeased store input.signal (signalStatusForErr err) (Just (renderEngineErr err)) input.signal.updatedAt
      pure (Left err)

    Right plan -> do
      rez <- commitStep store plan
      ackSignalIfLeased store input.signal AppliedSs Nothing rez.step.createdAt
      pure (Right rez)

renderEngineErr :: EngineErr -> Text
renderEngineErr err =
  case err of
    MachineKeyMismatchEr expected actual ->
      "step input machine key does not match compiled machine; expected=" <> renderMachineKey expected <> ", actual=" <> renderMachineKey actual

    InstanceMachineMismatchEr expectedName expectedVersion expectedDigest actualName actualVersion actualDigest ->
      "instance machine identity does not match compiled machine; expected=("
        <> machineNameText expectedName <> ","
        <> machineVersionText expectedVersion <> ","
        <> specDigestText expectedDigest <> "), actual=("
        <> machineNameText actualName <> ","
        <> machineVersionText actualVersion <> ","
        <> specDigestText actualDigest <> ")"

    SnapshotMachineMismatchEr expectedVersion expectedDigest actualVersion actualDigest ->
      "snapshot machine identity does not match compiled machine; expected=("
        <> machineVersionText expectedVersion <> ","
        <> specDigestText expectedDigest <> "), actual=("
        <> machineVersionText actualVersion <> ","
        <> specDigestText actualDigest <> ")"

    SnapshotInstanceMismatchEr expectedInst actualInst ->
      "snapshot instance UUID does not match instance row; expected=" <> U.toText expectedInst <> ", actual=" <> U.toText actualInst

    SignalInstanceMismatchEr expectedInst actualInst ->
      "signal instance UUID does not match instance row; expected=" <> U.toText expectedInst <> ", actual=" <> U.toText actualInst

    InstanceStateMismatchEr expectedState actualState ->
      "instance state does not match snapshot state; expected=" <> renderStateRef expectedState <> ", actual=" <> renderStateRef actualState

    InstancePathMismatchEr expectedPath actualPath ->
      "instance path does not match snapshot path; expected=" <> renderStatePath expectedPath <> ", actual=" <> renderStatePath actualPath

    MissingStateEr ref ->
      "missing compiled state for ref " <> renderStateRef ref

    MissingRegionEr ref ->
      "missing compiled region for ref " <> renderRegionRef ref

    MissingEntryHandlerEr ref ->
      "signal requested entry execution, but state " <> renderStateRef ref <> " has no entry handler"

    MissingEntryExecEr stateRef entryRef ->
      "missing entry executable for state " <> renderStateRef stateRef <> ", entry case ref=" <> renderCaseRef entryRef

    DecodeCtxEr codecErr ->
      "failed to decode snapshot context: " <> renderCodecErr codecErr

    DecodeSignalEr codecErr ->
      "failed to decode signal payload: " <> renderCodecErr codecErr

    NoMatchingCaseEr stateRef ->
      "no matching case for current state " <> renderStateRef stateRef

    ReactEr reactErr ->
      "state reaction failed: " <> renderReactErr reactErr

    MissingRouteEr stateRef handoffTxt ->
      "missing route for state " <> renderStateRef stateRef <> " and handoff " <> quote handoffTxt

ensureInputMatches :: CompiledMachine st sg hf ctx cmd child -> StepInput -> Either EngineErr ()
ensureInputMatches compiled input = do
  let compiledKey = machineKeyOf compiled
  if input.machine == compiledKey
    then Right ()
    else Left (MachineKeyMismatchEr compiledKey input.machine)

  let expectedName = compiled.spec.name
      expectedVersion = compiled.spec.version
      expectedDigest = compiled.digest
      actualName = input.instance_.machine
      actualVersion = input.instance_.version
      actualDigest = input.instance_.digest

  if actualName == expectedName && actualVersion == expectedVersion && actualDigest == expectedDigest
    then Right ()
    else Left (InstanceMachineMismatchEr expectedName expectedVersion expectedDigest actualName actualVersion actualDigest)

  if input.snapshot.version == expectedVersion && input.snapshot.digest == expectedDigest
    then Right ()
    else Left (SnapshotMachineMismatchEr expectedVersion expectedDigest input.snapshot.version input.snapshot.digest)

  if input.snapshot.inst == input.instance_.uuid
    then Right ()
    else Left (SnapshotInstanceMismatchEr input.instance_.uuid input.snapshot.inst)

  if input.signal.inst == input.instance_.uuid
    then Right ()
    else Left (SignalInstanceMismatchEr input.instance_.uuid input.signal.inst)

  if input.instance_.state == input.snapshot.state
    then Right ()
    else Left (InstanceStateMismatchEr input.instance_.state input.snapshot.state)

  if input.instance_.path == input.snapshot.path
    then Right ()
    else Left (InstancePathMismatchEr input.instance_.path input.snapshot.path)

resolveReaction :: Show hf => CompiledMachine st sg hf ctx cmd child -> StateNd -> ReactEnv -> ctx -> SignalRw -> Either EngineErr (ReactionStep ctx hf cmd)
resolveReaction compiled currentSt env ctxVal signalRw =
  if signalRequestsEntry signalRw
    then runEntryReaction compiled currentSt env ctxVal
    else runCaseReaction compiled currentSt env ctxVal signalRw

runEntryReaction :: CompiledMachine st sg hf ctx cmd child -> StateNd -> ReactEnv -> ctx -> Either EngineErr (ReactionStep ctx hf cmd)
runEntryReaction compiled currentSt env ctxVal = do
  entryRef <- maybe (Left (MissingEntryHandlerEr currentSt.ref)) Right currentSt.entry
  entryExec <- maybe (Left (MissingEntryExecEr currentSt.ref entryRef)) Right (lookupEntryExec entryRef compiled.exec)
  rezVal <- first ReactEr $ runEntryExec env ctxVal entryExec
  pure
    ReactionStep
      { kind = EntrySk
      , caseRef = Just entryRef
      , rez = rezVal
      }

runCaseReaction :: CompiledMachine st sg hf ctx cmd child -> StateNd -> ReactEnv -> ctx -> SignalRw -> Either EngineErr (ReactionStep ctx hf cmd)
runCaseReaction compiled currentSt env ctxVal signalRw = do
  signalVal <- first DecodeSignalEr $ compiled.spec.codecs.signal.decode signalRw.payload
  caseExec <- maybe (Left (NoMatchingCaseEr currentSt.ref)) Right (findMatchingCase signalVal (V.toList currentSt.cases) compiled.exec)
  rezVal <- first ReactEr $ runCaseExec env signalVal ctxVal caseExec
  pure
    ReactionStep
      { kind = SignalSk
      , caseRef = Just caseExec.caseRef
      , rez = rezVal
      }

resolveRoute :: MachineGraph -> StateNd -> Maybe CaseRef -> Text -> Either EngineErr RouteEd
resolveRoute graph stateNd caseRef handoffTxt = do
  routeNds <- traverse (lookupRouteOrErr graph) (V.toList stateNd.routes)

  let exactRoutes = case caseRef of
        Nothing -> []
        Just caseRef' -> filter (\x -> x.handoff == handoffTxt && x.caseRef == Just caseRef') routeNds
      genericRoutes = filter (\x -> x.handoff == handoffTxt && not (isJust x.caseRef)) routeNds

  case exactRoutes <> genericRoutes of
    routeNd : _ -> Right routeNd
    [] -> Left (MissingRouteEr stateNd.ref handoffTxt)

resolveTargetState :: MachineGraph -> StateNd -> RouteTarget -> Either EngineErr StateNd
resolveTargetState graph currentSt target =
  case target of
    StayRt -> Right currentSt
    CompleteRt -> Right currentSt
    FailRt _ -> Right currentSt
    GotoRt ref -> lookupStateOrErr graph ref >>= descendInitialLeaf graph

descendInitialLeaf :: MachineGraph -> StateNd -> Either EngineErr StateNd
descendInitialLeaf graph stateNd =
  case stateNd.child of
    Nothing -> Right stateNd
    Just regionRef -> do
      regionNd <- lookupRegionOrErr graph regionRef
      initSt <- lookupStateOrErr graph regionNd.initial
      descendInitialLeaf graph initSt

mkReactEnvFrom :: SnapshotRw -> SignalRw -> ReactEnv
mkReactEnvFrom snapshotRw signalRw =
  let (childMap, timerMap) = decodeChildStore snapshotRw.child
  in
  ReactEnv
    { now = signalRw.updatedAt
    , attempt = causeAttempt signalRw.cause
    , cause = signalRw.cause
    , child = childMap
    , timer = M.keysSet timerMap
    }

buildChildPlans :: StepInput -> RouteEd -> [PlannedChild]
buildChildPlans input routeEd =
  zipWith (mkPlannedChild input routeEd) [0 :: Int ..] routeEd.plan.spawn

mkPlannedChild :: StepInput -> RouteEd -> Int -> SpawnPlanG -> PlannedChild
mkPlannedChild input routeEd ix spawnPlan =
  let childUuid = deterministicUuid ("hfsm-child:" <> U.toText input.instance_.uuid <> ":" <> U.toText input.signal.uuid <> ":" <> renderRouteRef routeEd.ref <> ":" <> tshow ix)
      childKey = normalizeSpawnKey spawnPlan.key childUuid
      childAppend =
        ChildAppend
          { parent = input.instance_.uuid
          , child = childUuid
          , key = Just childKey
          , status = SpawnedCs
          , join = spawnPlan.join
          , now = input.signal.updatedAt
          }

      storeVal =
        object
          [ "kind" .= ("child" :: Text)
          , "uuid" .= childUuid
          , "machine" .= spawnPlan.child
          , "key" .= childKey
          , "joinRef" .= spawnPlan.join
          , "status" .= ("spawned" :: Text)
          , "input" .= spawnPlan.input
          ]

      outboxAppend =
        OutboxAppend
          { inst = input.instance_.uuid
          , step = U.nil
          , payload =
              object
                [ "kind" .= ("spawn" :: Text)
                , "childUuid" .= childUuid
                , "machine" .= spawnPlan.child
                , "key" .= childKey
                , "joinRef" .= spawnPlan.join
                , "input" .= spawnPlan.input
                ]
          , now = input.signal.updatedAt
          }
  in
  PlannedChild
    { uuid = childUuid
    , key = childKey
    , append = childAppend
    , store = storeVal
    , outbox = outboxAppend
    }

buildBreakAppends :: StepInput -> RouteEd -> StateRef -> StateRef -> [BreakAppend]
buildBreakAppends input routeEd fromRef toRef =
  fmap (mkBreakAppend input routeEd fromRef toRef) routeEd.plan.break

mkBreakAppend :: StepInput -> RouteEd -> StateRef -> StateRef -> BreakPlanG -> BreakAppend
mkBreakAppend input routeEd fromRef toRef breakPlan =
  let stateRef =
        case breakPlan of
          BreakBeforeCaseBg -> Just fromRef
          BreakAfterCaseBg -> Just fromRef
          BreakBeforeRouteBg -> Just fromRef
          BreakAfterRouteBg -> Just toRef
          BreakBeforeCommitBg -> Just toRef

      kindVal =
        case breakPlan of
          BreakBeforeCaseBg -> BeforeCaseBk
          BreakAfterCaseBg -> AfterCaseBk
          BreakBeforeRouteBg -> BeforeRouteBk
          BreakAfterRouteBg -> AfterRouteBk
          BreakBeforeCommitBg -> BeforeCommitBk
  in
  BreakAppend
    { inst = Just input.instance_.uuid
    , kind = kindVal
    , state = stateRef
    , status = HitBs
    , note = Just ("route " <> renderRouteRef routeEd.ref <> " hit breakpoint " <> renderBreakPlan breakPlan)
    , now = input.signal.updatedAt
    }

buildCmdOutboxAppends :: CompiledMachine st sg hf ctx cmd child -> UTCTime -> [cmd] -> [OutboxAppend]
buildCmdOutboxAppends compiled now =
  fmap $ \cmdVal ->
    OutboxAppend
      { inst = U.nil
      , step = U.nil
      , payload =
          object
            [ "kind" .= ("cmd" :: Text)
            , "payload" .= compiled.spec.codecs.cmd.encode cmdVal
            ]
      , now = now
      }

buildTimerOutboxAppends :: MachineGraph -> UTCTime -> [TimerPlanG] -> [OutboxAppend]
buildTimerOutboxAppends graph now =
  fmap $ \timerPlan ->
    OutboxAppend
      { inst = U.nil
      , step = U.nil
      , payload =
          object
            [ "kind" .= ("timer" :: Text)
            , "timerRef" .= timerPlan.ref
            , "timerName" .= lookupTimerNameText graph timerPlan.ref
            , "delaySeconds" .= delaySeconds timerPlan.delay
            , "payload" .= timerPlan.payload
            ]
      , now = now
      }

buildWaitValue :: MachineGraph -> WaitPlanG -> [TimerPlanG] -> Maybe Value
buildWaitValue graph waitPlan timerPlans =
  case waitPlan of
    WaitNoneWg -> Nothing

    WaitSignalWg ->
      Just $
        object
          [ "kind" .= ("signal" :: Text)
          ]

    WaitJoinWg joinRef ->
      Just $
        object
          [ "kind" .= ("join" :: Text)
          , "joinRef" .= joinRef
          , "joinName" .= lookupJoinNameText graph joinRef
          ]

    WaitTimerWg timerRef ->
      let timerPlan = find (\x -> x.ref == timerRef) timerPlans
          delayVal = fmap (.delay) timerPlan
          payloadVal = timerPlan >>= (.payload)
      in
      Just $
        object
          [ "kind" .= ("timer" :: Text)
          , "timerRef" .= timerRef
          , "timerName" .= lookupTimerNameText graph timerRef
          , "delaySeconds" .= fmap delaySeconds delayVal
          , "payload" .= payloadVal
          ]

buildStepNote :: StepInput -> Text -> RouteEd -> ReactionStep ctx hf cmd -> InstanceStatus -> Maybe Value -> [BreakAppend] -> Value
buildStepNote input handoffTxt routeEd reaction statusVal waitVal breakAppends =
  let terminalTxt =
        case routeEd.plan.target of
          CompleteRt -> Just ("done" :: Text)
          FailRt _ -> Just ("failed" :: Text)
          StayRt -> Nothing
          GotoRt _ -> Nothing

      failMsg =
        case routeEd.plan.target of
          FailRt msg -> Just msg
          _ -> Nothing

      blockedOnVal = blockedOnText waitVal breakAppends
  in
  object
    [ "machine" .= machineNameText input.instance_.machine
    , "version" .= machineVersionText input.instance_.version
    , "handoff" .= handoffTxt
    , "routeRef" .= routeEd.ref
    , "notes" .= reaction.rez.note
    , "target" .= renderRouteTarget routeEd.plan.target
    , "wait" .= waitVal
    , "breaks" .= fmap renderBreakPlan routeEd.plan.break
    , "status" .= renderInstanceStatus statusVal
    , "blockedOn" .= blockedOnVal
    , "terminal" .= terminalTxt
    , "failure" .= failMsg
    ]

buildProjBatch :: StepRw -> SnapshotRw -> [BreakAppend] -> ProjBatch
buildProjBatch stepRw snapshotRw breakAppends =
  let machineTxt = fromMaybe "" (lookupTextField "machine" stepRw.note)
      versionTxt = fromMaybe "" (lookupTextField "version" stepRw.note)
      statusTxt = fromMaybe (renderInstanceStatus (statusFromSnapshot snapshotRw breakAppends)) (lookupTextField "status" stepRw.note)
      blockedOnVal = lookupMaybeTextField "blockedOn" stepRw.note <> blockedOnText snapshotRw.wait breakAppends

      activeVals =
        [ object
            [ "instance_" .= stepRw.inst
            , "machine" .= machineTxt
            , "version" .= versionTxt
            , "status" .= statusTxt
            , "state" .= snapshotRw.state
            , "path" .= snapshotRw.path
            , "wait" .= snapshotRw.wait
            , "blockedOn" .= blockedOnVal
            , "updatedAt" .= stepRw.createdAt
            ]
        ]

      traceVals =
        [ object
            [ "inst" .= stepRw.inst
            , "step" .= stepRw.uuid
            , "signal" .= stepRw.signal
            , "from" .= stepRw.from
            , "to" .= stepRw.to
            , "route" .= stepRw.route
            , "note" .= stepRw.note
            , "createdAt" .= stepRw.createdAt
            ]
        ]

      queueVals =
        if isJust snapshotRw.wait || not (null breakAppends)
          then
            [ object
                [ "kind" .= queueKindText snapshotRw.wait breakAppends
                , "item" .= stepRw.inst
                , "inst" .= stepRw.inst
                , "state" .= snapshotRw.state
                , "path" .= snapshotRw.path
                , "status" .= statusTxt
                , "lease" .= (Nothing :: Maybe UUID)
                , "wait" .= snapshotRw.wait
                , "payload" .= (Nothing :: Maybe Value)
                , "cause" .= (Nothing :: Maybe Value)
                , "note" .= blockedOnVal
                , "createdAt" .= stepRw.createdAt
                , "updatedAt" .= Just stepRw.createdAt
                ]
            ]
          else
            []
  in
  ProjBatch
    { active = activeVals
    , trace = traceVals
    , queue = queueVals
    , tree = []
    }

mkStepAppend :: StepRw -> StepAppend
mkStepAppend stepRw =
  StepAppend
    { inst = stepRw.inst
    , signal = stepRw.signal
    , kind = stepRw.kind
    , from = stepRw.from
    , to = stepRw.to
    , caseRef = stepRw.caseRef
    , route = stepRw.route
    , note = stepRw.note
    , now = stepRw.createdAt
    }

mkSnapshotAppend :: SnapshotRw -> SnapshotAppend
mkSnapshotAppend snapshotRw =
  SnapshotAppend
    { inst = snapshotRw.inst
    , state = snapshotRw.state
    , path = snapshotRw.path
    , ctx = snapshotRw.ctx
    , wait = snapshotRw.wait
    , child = snapshotRw.child
    , version = snapshotRw.version
    , digest = snapshotRw.digest
    , now = snapshotRw.createdAt
    }

setOutboxStep :: UUID -> OutboxAppend -> OutboxAppend
setOutboxStep stepUuid outboxAppend =
  outboxAppend { step = stepUuid }

ackSignalIfLeased :: Monad m => Store m -> SignalRw -> SignalStatus -> Maybe Text -> UTCTime -> m ()
ackSignalIfLeased store signalRw statusVal noteVal nowVal =
  case signalRw.lease of
    Nothing -> pure ()
    Just leaseUuid ->
      store.ackSignal
        AckRq
          { signal = signalRw.uuid
          , lease = leaseUuid
          , status = statusVal
          , note = noteVal
          , now = nowVal
          }

signalStatusForErr :: EngineErr -> SignalStatus
signalStatusForErr err =
  case err of
    DecodeSignalEr _ -> RejectedSs
    NoMatchingCaseEr _ -> RejectedSs
    ReactEr (RejectREr _) -> RejectedSs
    ReactEr (DecodeREr _) -> RejectedSs
    _ -> FailedSs

statusFromSnapshot :: SnapshotRw -> [BreakAppend] -> InstanceStatus
statusFromSnapshot snapshotRw breakAppends
  | not (null breakAppends) = PausedIs
  | isJust snapshotRw.wait = WaitingIs
  | otherwise = RunningIs

nextStatusOf :: RouteTarget -> Maybe Value -> [BreakAppend] -> InstanceStatus
nextStatusOf target waitVal breakAppends =
  case target of
    CompleteRt -> DoneIs
    FailRt _ -> FailedIs
    StayRt
      | not (null breakAppends) -> PausedIs
      | isJust waitVal -> WaitingIs
      | otherwise -> RunningIs
    GotoRt _
      | not (null breakAppends) -> PausedIs
      | isJust waitVal -> WaitingIs
      | otherwise -> RunningIs

signalRequestsEntry :: SignalRw -> Bool
signalRequestsEntry signalRw =
  case signalRw.cause of
    Just (Object obj) ->
      entryMarkerMatch (KM.lookup (K.fromText "kind") obj)
        || entryMarkerMatch (KM.lookup (K.fromText "stepKind") obj)
        || entryMarkerMatch (KM.lookup (K.fromText "hfsmKind") obj)

    _ -> False

entryMarkerMatch :: Maybe Value -> Bool
entryMarkerMatch val =
  case val of
    Just (String txt) -> normalizeAtom txt == "entry"
    _ -> False

causeAttempt :: Maybe Value -> Int
causeAttempt causeVal =
  case causeVal of
    Just (Object obj) ->
      case KM.lookup (K.fromText "attempt") obj of
        Just (Number sc) ->
          case Sci.toBoundedInteger sc of
            Just n | n >= (0 :: Int) -> n
            _ -> 0
        _ -> 0

    _ -> 0

decodeChildStore :: Value -> (Map Text Value, Map Text Value)
decodeChildStore value =
  case value of
    Object obj ->
      let hasEnvelope = KM.member (K.fromText "children") obj || KM.member (K.fromText "timers") obj
          childObj = if hasEnvelope then nestedObject "children" obj else obj
          timerObj = if hasEnvelope then nestedObject "timers" obj else KM.empty
      in (objectToMap childObj, objectToMap timerObj)

    _ -> (M.empty, M.empty)

encodeChildStore :: Map Text Value -> Map Text Value -> Value
encodeChildStore childMap timerMap =
  object
    [ "children" .= childMap
    , "timers" .= timerMap
    ]

timerStoreKey :: MachineGraph -> TimerRef -> Text
timerStoreKey graph timerRef =
  fromMaybe ("timer:" <> renderTimerRef timerRef) (lookupTimerNameText graph timerRef)

timerStoreValue :: MachineGraph -> TimerPlanG -> Value
timerStoreValue graph timerPlan =
  object
    [ "kind" .= ("timer" :: Text)
    , "timerRef" .= timerPlan.ref
    , "timerName" .= lookupTimerNameText graph timerPlan.ref
    , "delaySeconds" .= delaySeconds timerPlan.delay
    , "payload" .= timerPlan.payload
    ]

lookupStateOrErr :: MachineGraph -> StateRef -> Either EngineErr StateNd
lookupStateOrErr graph ref =
  maybe (Left (MissingStateEr ref)) Right (IM.lookup (stateRefKey ref) graph.states)

lookupRegionOrErr :: MachineGraph -> RegionRef -> Either EngineErr RegionNd
lookupRegionOrErr graph ref =
  maybe (Left (MissingRegionEr ref)) Right (IM.lookup (regionRefKey ref) graph.regions)

lookupRouteOrErr :: MachineGraph -> RouteRef -> Either EngineErr RouteEd
lookupRouteOrErr graph ref =
  case IM.lookup (routeRefKey ref) graph.routes of
    Just routeNd -> Right routeNd
    Nothing -> Left (MissingRouteEr (mkStateRef0) ("missing lowered route ref " <> renderRouteRef ref))
  where
    mkStateRef0 = inputlessStateRef

lookupJoinNameText :: MachineGraph -> JoinRef -> Maybe Text
lookupJoinNameText graph ref =
  IM.lookup (joinRefKey ref) graph.joins >>= \joinNd ->
    fmap joinNameText joinNd.name

lookupTimerNameText :: MachineGraph -> TimerRef -> Maybe Text
lookupTimerNameText graph ref =
  IM.lookup (timerRefKey ref) graph.timers >>= \timerNd ->
    fmap timerNameText timerNd.name

renderMachineKey :: MachineKey -> Text
renderMachineKey key =
  machineNameText key.name <> "@" <> machineVersionText key.version <> "#" <> specDigestText key.digest

renderCodecErr :: CodecErr -> Text
renderCodecErr err =
  case err of
    DecodeEr msg -> "codec decode error: " <> msg
    EncodeEr msg -> "codec encode error: " <> msg
    SchemaEr msg -> "codec schema error: " <> msg

renderReactErr :: ReactErr -> Text
renderReactErr err =
  case err of
    DecodeREr msg -> "decode error: " <> msg
    RejectREr msg -> "reject error: " <> msg
    DomainEr msg -> "domain error: " <> msg
    InvariantEr msg -> "invariant error: " <> msg

renderInstanceStatus :: InstanceStatus -> Text
renderInstanceStatus statusVal =
  case statusVal of
    RunningIs -> "running"
    WaitingIs -> "waiting"
    PausedIs -> "paused"
    DoneIs -> "done"
    FailedIs -> "failed"
    CancelledIs -> "cancelled"

renderRouteTarget :: RouteTarget -> Text
renderRouteTarget target =
  case target of
    StayRt -> "stay"
    GotoRt ref -> "goto:" <> renderStateRef ref
    CompleteRt -> "complete"
    FailRt msg -> "fail:" <> msg

renderBreakPlan :: BreakPlanG -> Text
renderBreakPlan breakPlan =
  case breakPlan of
    BreakBeforeCaseBg -> "before-case"
    BreakAfterCaseBg -> "after-case"
    BreakBeforeRouteBg -> "before-route"
    BreakAfterRouteBg -> "after-route"
    BreakBeforeCommitBg -> "before-commit"

blockedOnText :: Maybe Value -> [BreakAppend] -> Maybe Text
blockedOnText waitVal breakAppends
  | not (null breakAppends) = Just "break"
  | otherwise =
      case waitVal of
        Just (Object obj) ->
          case lookupObjectText "kind" obj of
            Just "signal" -> Just "signal"
            Just "join" ->
              lookupObjectText "joinName" obj <|> fmap ("join:" <>) (lookupObjectText "joinRef" obj) <|> Just "join"
            Just "timer" ->
              lookupObjectText "timerName" obj <|> fmap ("timer:" <>) (lookupObjectText "timerRef" obj) <|> Just "timer"
            Just txt -> Just txt
            Nothing -> Just "wait"
        Just _ -> Just "wait"
        Nothing -> Nothing

queueKindText :: Maybe Value -> [BreakAppend] -> Text
queueKindText waitVal breakAppends
  | not (null breakAppends) = "PausedBreakQk"
  | otherwise =
      case waitVal of
        Just (Object obj) ->
          case lookupObjectText "kind" obj of
            Just "signal" -> "WaitSignalQk"
            Just "join" -> "WaitJoinQk"
            Just "timer" -> "WaitTimerQk"
            _ -> "WaitOtherQk"
        _ -> "WaitOtherQk"

delaySeconds :: NominalDiffTime -> Double
delaySeconds = realToFrac

normalizeSpawnKey :: Maybe Text -> UUID -> Text
normalizeSpawnKey keyVal childUuid =
  case fmap (T.strip . normalizeAtom) keyVal of
    Just txt | not (T.null txt) -> txt
    _ -> "child:" <> U.toText childUuid

normalizeAtom :: Text -> Text
normalizeAtom = T.toLower . T.strip

renderHandoff :: Show hf => hf -> Text
renderHandoff = T.pack . show

isTerminalTarget :: RouteTarget -> Bool
isTerminalTarget target =
  case target of
    CompleteRt -> True
    FailRt _ -> True
    StayRt -> False
    GotoRt _ -> False

deterministicUuid :: Text -> UUID
deterministicUuid seedTxt =
  fromMaybe U.nil (U.fromText uuidTxt)
  where
    bytes = encodeUtf8 seedTxt
    h1 = hash64 14695981039346656037 bytes
    h2 = hash64 1099511628211 bytes
    rawHex = padHex16 h1 <> padHex16 h2
    uuidTxt =
      T.take 8 rawHex
        <> "-"
        <> T.take 4 (T.drop 8 rawHex)
        <> "-"
        <> T.take 4 (T.drop 12 rawHex)
        <> "-"
        <> T.take 4 (T.drop 16 rawHex)
        <> "-"
        <> T.take 12 (T.drop 20 rawHex)

hash64 :: Word64 -> BS.ByteString -> Word64
hash64 seed bytes =
  BS.foldl' step seed bytes
  where
    prime = 1099511628211
    step acc w = (acc `xor` fromIntegral w) * prime

padHex16 :: Word64 -> Text
padHex16 w =
  let raw = T.pack (showHexWord64 w)
      padLen = max 0 (16 - T.length raw)
  in T.replicate padLen "0" <> raw

showHexWord64 :: Word64 -> String
showHexWord64 w
  | w == 0 = "0"
  | otherwise = reverse (go w)
  where
    go 0 = []
    go x =
      let (q, r) = x `quotRem` 16
      in hexDigit r : go q

    hexDigit n
      | n < 10 = toEnum (fromEnum '0' + fromIntegral n)
      | otherwise = toEnum (fromEnum 'a' + fromIntegral n - 10)

lookupTextField :: Text -> Value -> Maybe Text
lookupTextField key value =
  case value of
    Object obj -> lookupObjectText key obj
    _ -> Nothing

lookupMaybeTextField :: Text -> Value -> Maybe Text
lookupMaybeTextField = lookupTextField

lookupObjectText :: Text -> KM.KeyMap Value -> Maybe Text
lookupObjectText key obj =
  case KM.lookup (K.fromText key) obj of
    Just (String txt) -> Just txt
    Just (Number sc) -> Just (T.pack (show sc))
    _ -> Nothing

nestedObject :: Text -> KM.KeyMap Value -> KM.KeyMap Value
nestedObject key obj =
  case KM.lookup (K.fromText key) obj of
    Just (Object nested) -> nested
    _ -> KM.empty

objectToMap :: KM.KeyMap Value -> Map Text Value
objectToMap =
  M.fromList . fmap (K.toText *** id) . KM.toList

renderStateRef :: StateRef -> Text
renderStateRef ref = tshow (stateRefWord32 ref)

renderRegionRef :: RegionRef -> Text
renderRegionRef ref = tshow (regionRefWord32 ref)

renderCaseRef :: CaseRef -> Text
renderCaseRef ref = tshow (caseRefWord32 ref)

renderRouteRef :: RouteRef -> Text
renderRouteRef ref = tshow (routeRefWord32 ref)

renderTimerRef :: TimerRef -> Text
renderTimerRef ref = tshow (timerRefWord32 ref)

renderJoinRef :: JoinRef -> Text
renderJoinRef ref = tshow (joinRefWord32 ref)

quote :: Text -> Text
quote txt = "\"" <> txt <> "\""

stateRefKey :: StateRef -> Int
stateRefKey = fromIntegral . stateRefWord32

regionRefKey :: RegionRef -> Int
regionRefKey = fromIntegral . regionRefWord32

routeRefKey :: RouteRef -> Int
routeRefKey = fromIntegral . routeRefWord32

joinRefKey :: JoinRef -> Int
joinRefKey = fromIntegral . joinRefWord32

timerRefKey :: TimerRef -> Int
timerRefKey = fromIntegral . timerRefWord32

inputlessStateRef :: StateRef
inputlessStateRef = toStateRef 0
  where
    toStateRef :: Word32 -> StateRef
    toStateRef = read . ("StateRef " <>) . show
    -- unreachable fallback path; only used in corrupted lowered-route case

tshow :: Show a => a -> Text
tshow = T.pack . show

