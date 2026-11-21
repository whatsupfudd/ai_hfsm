-- | CT-05 live-DB regression; no history records or user data are deleted.
-- Run on a dedicated clean database with migrations 001, 002, 003 applied.
module Demo.HistoryRegression (regression, regressionPg) where

import Control.Monad (unless)
import Control.Monad.Trans.Cont (runContT)
import Data.Aeson (Value(..), object, (.=))
import Data.Bifunctor (first)
import qualified Data.Text as T
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4

import qualified DB.Connect as Db
import Demo.ApprovalHfsm (ApprovalCtx, approvalSpec, Signal(..))
import Demo.ApprovalRuntime (initialCtx)
import qualified Demo.MemoryStore as Memory
import Hfsm.Compile (CompiledMachine(..), compile)
import Hfsm.Core.Version (mkMachineVersion)
import Hfsm.History.Types (ReplayInput(..), HistoryTurn(..), ReplayResult(..))
import qualified Hfsm.History.Types as Ht
import Hfsm.History.Replay (verifyTurn, verifyTimeline)
import Hfsm.History.Postgres (loadHistoryPage)
import Hfsm.History.Release (registerPinnedRelease)
import Hfsm.Migration.Replay (verifyMigrationTransform)
import Hfsm.Migration.Types (MigrationSpec(..), MigrationRequest(..), MigrationReceipt(..))
import Hfsm.Migration.Plan (planMigration)
import Hfsm.Migration.Postgres (applyMigration)
import Hfsm.Migration.PinnedEnqueue (enqueuePinned)
import Hfsm.Runtime.Engine.Step (StepInput(..), StepPlan(..), planStep)
import Hfsm.Runtime.Engine.Worker (WorkerCfg(..), runTick)
import Hfsm.Runtime.Model (InstanceRw(..), SnapshotRw(..), SignalRw(..), SignalStatus(..), StepRw(..))
import qualified Hfsm.Runtime.Model.Snapshot as Msn
import Hfsm.Runtime.Registry (emptyRegistry, register, machineKeyOf)
import Hfsm.Spec.Def (MachineSpec(..))
import Hfsm.Store.Class (Store(..), LeaseRq(..), LeaseRez(..), AckRq(..), OutboxAppend(..))
import qualified Hfsm.Store.Postgres as Pg
import Hfsm.Store.Postgres.Bootstrap (enqueueFor, newInstanceFor)
import Hfsm.Runtime.Registry (MachineKey(..))

assert :: Bool -> String -> IO ()
assert good message = unless good (fail message)

required :: Show e => String -> Either e a -> IO a
required step = either (fail . ((step <> ": ") <>) . show) pure

just :: String -> Maybe a -> IO a
just message = maybe (fail message) pure

approvalMachines = do
  sourceSpec <- required "approvalSpec" approvalSpec
  source <- required "compile v1" (compile sourceSpec)
  newVersion <- required "version 1.0.1" (mkMachineVersion "1.0.1")
  target <- required "compile v2" (compile (sourceSpec { version = newVersion }))
  pure (source,target)

migrationSpec :: MigrationSpec ApprovalCtx ApprovalCtx
migrationSpec = MigrationSpec
  { mappingName="ct05/approval-ctx-identity-v1-to-v2"
  , mapControl=Right
  , mapContext=Right
  , mapWait=Right
  , mapChild=Right
  , mapStatus=Right
  }

-- | Planner-only test, no database needed.
regression :: IO ()
regression = do
  (source,target) <- approvalMachines
  registry <- required "registry" (register source emptyRegistry)
  mem <- Memory.newMemoryStore
  instId <- Memory.createInstance mem source initialCtx
  _ <- Memory.enqueueSignal mem instId (SubmitSg "alice")
  worker <- UUID4.nextRandom
  clock <- getCurrentTime
  claimed <- (Memory.storeOf mem).leaseSignal LeaseRq
    { node=worker,batchSize=1,leaseSeconds=30,now=clock }
  sig <- case claimed of
    LeaseSomeLz [s] -> pure s
    other -> fail ("expected exactly one signal: " <> show other)
  inst <- just "missing in-memory instance" =<< (Memory.storeOf mem).loadInstance instId
  snap <- just "missing in-memory snapshot" =<< (Memory.storeOf mem).loadSnapshot inst.snap
  let input = ReplayInput (machineKeyOf source) inst snap sig
  plan <- required "pure plan" (planStep source StepInput
    {machine=machineKeyOf source,instance_=inst,snapshot=snap,signal=sig})
  let turn = HistoryTurn
        { revision=1, sourceSnapshotId=snap.uuid
        , step=plan.step, snapshot=plan.snapshot
        , input=Just input
        , outbox=zipWith (\n eff -> object
            ["effectKey" .= (UUID.toText plan.step.uuid <> ":" <> T.pack (show n)),
             "payload" .= eff.payload]) [0 :: Int ..] plan.outbox
        , children=[], breaks=[],proj=plan.proj,migration=Nothing }
  required "pure replay" (verifyTurn registry turn)
  let
    altered = (turn :: HistoryTurn) { Ht.snapshot = turn.snapshot { Msn.ctx = Null } }
  case verifyTurn registry altered of
    Left _ -> pure ()
    Right _ -> fail "tampered snapshot was not detected by replay"
  op <- UUID4.nextRandom
  step <- UUID4.nextRandom
  snapId <- UUID4.nextRandom
  planned <- required "pure migration" $
    planMigration source target migrationSpec op step snapId clock inst snap
  assert (planned.targetKey == machineKeyOf target) "migration target release mismatch"
  putStrLn "HistoryRegression (pure): PASS"

regressionPg :: Db.PgDbConfig -> IO ()
regressionPg config = runContT (Db.startPg config) $ \pool -> do
  (source,target) <- approvalMachines
  let runner = Pg.runWithPool pool
      api = Pg.postgresStore runner
  required "register old release" =<< Pg.registerMachine runner (machineKeyOf source)
  required "register pinned target release" =<<
    registerPinnedRelease runner target "ct05-approval-v101-build-001"
  required "idempotent pinned release" =<<
    registerPinnedRelease runner target "ct05-approval-v101-build-001"
  wrongBuild <- registerPinnedRelease runner target "ct05-approval-v101-build-002"
  case wrongBuild of
    Left _ -> pure ()
    Right _ -> fail "immutable release accepted a different implementation fingerprint"
  registry1 <- required "source registry" (register source emptyRegistry)
  registry2 <- required "target registry" (register target registry1)
  node <- UUID4.nextRandom
  let worker = WorkerCfg {batchSize=1,pollDelayMs=1,leaseSeconds=30,node=node}
      create compiled = do
        req <- required "new instance" =<< newInstanceFor compiled initialCtx Nothing
        required "initialize instance" =<< Pg.initializeInstance runner req
      submit compiled inst = do
        rq <- enqueueFor compiled inst (SubmitSg "alice") Nothing
        _ <- required "release-aware enqueue signal" =<<
          enqueuePinned runner (machineKeyOf compiled) rq
        runTick worker (Pg.postgresStoreForInstance runner inst) registry2
        pure ()

  -- Each newly committed step records the original planner input exactly.
  historyId <- create source
  submit source historyId
  turns <- required "load strict history" =<< loadHistoryPage runner historyId 1 10
  assert (length turns == 1) "expected one replayable turn"
  verified <- required "replay actual history" (verifyTimeline registry1 turns)
  assert (verified == [ReplayVerified 1]) "unexpected replay result"

  -- An explicit migration at revision 0, followed by a target-release turn.
  migrationId <- create source
  before <- just "migration instance missing" =<< api.loadInstance migrationId
  oldSnap <- just "migration snapshot missing" =<< api.loadSnapshot before.snap
  op <- UUID4.nextRandom
  st <- UUID4.nextRandom
  sn <- UUID4.nextRandom
  now <- getCurrentTime
  request <- required "plan cross-version migration" $
    planMigration source target migrationSpec op st sn now before oldSnap
  receipt <- required "apply migration" =<< applyMigration runner request
  assert (receipt.revision == 1) "migration did not advance to revision 1"
  duplicate <- required "idempotent migration request" =<< applyMigration runner request
  assert (receipt == duplicate) "idempotent migration receipt mismatch"
  after <- just "migrated instance missing" =<< api.loadInstance migrationId
  assert (after.revision == 1 && after.version == (machineKeyOf target).version)
    "migration did not pin target release"
  rejectedOld <- enqueueFor source migrationId (SubmitSg "alice") Nothing
  denied <- enqueuePinned runner (machineKeyOf source) rejectedOld
  case denied of
    Left _ -> pure ()
    Right _ -> fail "old-release signal was accepted after migration"
  unpinned <- enqueueFor target migrationId (SubmitSg "alice") Nothing
  raw <- Pg.enqueue runner unpinned
  case raw of
    Left _ -> pure ()
    Right _ -> fail "unpinned legacy enqueue bypassed migrated release guard"
  submit target migrationId
  afterTurns <- required "cross-version history" =<< loadHistoryPage runner migrationId 1 10
  case afterTurns of
    firstTurn : _ -> case firstTurn.migration of
      Just evidence -> required "replay migration transform" $
        verifyMigrationTransform source target migrationSpec oldSnap firstTurn.snapshot evidence
      Nothing -> fail "migration audit record missing"
    [] -> fail "migration history empty"
  checked <- required "cross-version replay" (verifyTimeline registry2 afterTurns)
  assert (checked == [ReplayMigrationVerified 1,ReplayVerified 2])
    "cross-version replay mismatch"

  -- Migration must fail atomically if any old-release signal is outstanding.
  unsafeInst <- create source
  pending <- enqueueFor source unsafeInst (SubmitSg "alice") Nothing
  _ <- required "pending signal enqueue" =<< Pg.enqueue runner pending
  oldInst <- just "pending instance missing" =<< api.loadInstance unsafeInst
  oldSource <- just "pending snapshot missing" =<< api.loadSnapshot oldInst.snap
  op2 <- UUID4.nextRandom
  step2 <- UUID4.nextRandom
  snap2 <- UUID4.nextRandom
  request2 <- required "plan pending migration" $
    planMigration source target migrationSpec op2 step2 snap2 now oldInst oldSource
  rejected <- applyMigration runner request2
  case rejected of
    Left _ -> pure ()
    Right _ -> fail "unsafe migration succeeded despite a pending signal"
  unchanged <- just "pending instance vanished" =<< api.loadInstance unsafeInst
  assert (unchanged.revision == 0 && unchanged.version == (machineKeyOf source).version)
    "failed migration unexpectedly mutated instance"
  -- Close the deliberately pending test signal through the public fenced API.
  let scoped = Pg.postgresStoreForInstance runner unsafeInst
  cleanupClaims <- scoped.leaseSignal LeaseRq
    { node=node,batchSize=1,leaseSeconds=30,now=now }
  cleanupSig <- case cleanupClaims of
    LeaseSomeLz [claimed] -> pure claimed
    other -> fail ("CT05 cleanup could not lease test signal: " <> show other)
  cleanupToken <- just "CT05 test cleanup missing lease" cleanupSig.lease
  _ <- required "CT05 cleanup ACK" =<< scoped.ackSignal AckRq
    { signal=cleanupSig.uuid,lease=cleanupToken,status=FailedSs
    , note=Just "CT05 intentional negative-test cleanup",now=now }
  putStrLn "HistoryRegression (PostgreSQL): PASS"
