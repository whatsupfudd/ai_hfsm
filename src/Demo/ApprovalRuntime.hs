{-# LANGUAGE QuasiQuotes #-}

module Demo.ApprovalRuntime (testA, initialCtx) where

import Control.Monad (forM_, unless)
import Control.Monad.Trans.Cont (runContT)
import Data.Aeson (Result(..), Value, fromJSON)
import Data.Int (Int64)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as UUID
import qualified Data.UUID.V4 as UUID4

import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import Hasql.TH (singletonStatement)

import qualified DB.Connect as Db
import Demo.ApprovalHfsm
  ( ApprovalCtx(..), ChildKey, Cmd, Handoff, Phase, Signal(..), approvalSpec )
import qualified Demo.MemoryStore as Memory
import Demo.Print (printRuntimeStore)
import Hfsm.Compile (CompiledMachine(..), compile)
import Hfsm.Runtime.Engine.Worker (WorkerCfg(..), runTick)
import Hfsm.Runtime.Model (InstanceRw(..), InstanceStatus(..), SnapshotRw(..))
import Hfsm.Runtime.Registry (Registry, emptyRegistry, machineKeyOf, register)
import Hfsm.Store.Class (Store(..))
import qualified Hfsm.Store.Postgres as Pg
import Hfsm.Store.Postgres.Bootstrap (enqueueFor, newInstanceFor)
import Hfsm.Validate.Report (ok, validate)
import Options.Cli (TestOpts(..))
import Options.Runtime (RunOptions(..))

-- The two branches execute the same compiled HFSM. The --db branch never
-- constructs or uses a Demo.MemoryStore.
testA :: TestOpts -> RunOptions -> IO ()
testA opts runtime = do
  spec <- requireRight "approvalSpec" approvalSpec
  compiled <- requireRight "compile" (compile spec)
  let report = validate compiled.graph
  unless (ok report) $ fail ("approval validation failed: " <> show report)
  registry <- requireRight "register" (register compiled emptyRegistry)

  if opts.doDb
    then runContT (Db.startPg runtime.pgDbConf) $ \pool ->
           testPostgres pool compiled registry
    else testMemory compiled registry


type ApprovalMachine = CompiledMachine Phase Signal Handoff ApprovalCtx Cmd ChildKey
type ApprovalRegistry = Registry Phase Signal Handoff ApprovalCtx Cmd ChildKey

-- The original in-memory smoke test is preserved for the default CLI mode.
testMemory :: ApprovalMachine -> ApprovalRegistry -> IO ()
testMemory compiled registry = do
  memory <- Memory.newMemoryStore
  instanceId <- Memory.createInstance memory compiled initialCtx
  forM_ demoSignals $ \(_, sig) -> do
    _ <- Memory.enqueueSignal memory instanceId sig
    pure ()
  cfg <- newWorkerCfg
  forM_ demoSignals $ \_ -> runTick cfg (Memory.storeOf memory) registry
  view <- Memory.loadInstanceView memory instanceId
  printRuntimeStore view

-- This is the real CT-02A test. Db.startPg brackets Hasql.Pool.acquire
-- and Hasql.Pool.release around this callback.
testPostgres :: Pool.Pool -> ApprovalMachine -> ApprovalRegistry -> IO ()
testPostgres pool compiled registry = do
  let runner = Pg.runWithPool pool
      store = Pg.postgresStore runner
  requireRight "PostgreSQL registerMachine" =<<
    Pg.registerMachine runner (machineKeyOf compiled)

  newInstance <- requireRight "newInstanceFor" =<<
    newInstanceFor compiled initialCtx Nothing
  instanceId <- requireRight "PostgreSQL initializeInstance" =<<
    Pg.initializeInstance runner newInstance
  putStrLn ("PostgreSQL approval instance: " <> UUID.toString instanceId)

  -- Each request gets a unique per-instance deduplication key. Enqueueing
  -- does not depend on the memory-store's ToJSON instance for signals.
  forM_ demoSignals $ \(label, sig) -> do
    request <- enqueueFor compiled instanceId sig (Just ("approval-demo/" <> label))
    _ <- requireRight ("PostgreSQL enqueue " <> show label) =<<
           Pg.enqueue runner request
    pure ()

  -- The PostgreSQL scheduler admits one signal per instance at a time.
  -- Four successive ticks therefore advance four separate durable turns.
  cfg <- newWorkerCfg
  forM_ demoSignals $ \_ -> runTick cfg store registry

  assertFinalState store instanceId
  (revision, applied, steps, outbox) <- requirePool =<<
    Pool.use pool (Session.statement instanceId approvalCounts)
  unless (revision == 4 && applied == 4 && steps == 4 && outbox == 4) $
    fail ("PostgreSQL approval persistence mismatch: " <>
          show (revision, applied, steps, outbox))

  putStrLn ("PostgreSQL OK: instance=" <> UUID.toString instanceId <>
            ", revision=" <> show revision <>
            ", applied=" <> show applied <>
            ", steps=" <> show steps <>
            ", outbox=" <> show outbox)
  view <- requirePool =<< Pool.use pool (Session.statement instanceId approvalView)
  printRuntimeStore view

-- Verify that the live Store successfully loads the actual persisted
-- instance/snapshot, and that the final typed application context is valid.
assertFinalState :: Store IO -> UUID -> IO ()
assertFinalState store instanceId = do
  mInstance <- store.loadInstance instanceId
  instanceRow <- maybe (fail "PostgreSQL approval instance missing") pure mInstance
  unless (instanceRow.status == DoneIs) $
    putStrLn ("PostgreSQL approval did not finish: " <> show instanceRow.status)
  mSnapshot <- store.loadSnapshot instanceRow.snap
  snapshotRow <- maybe (fail "PostgreSQL approval snapshot missing") pure mSnapshot
  ctx <- case fromJSON snapshotRow.ctx of
    Error err -> fail ("PostgreSQL approval context decode failed: " <> err)
    Success x -> pure (x :: ApprovalCtx)
  unless (ctx.author == "alice" && ctx.revision == 1 &&
          ctx.approvedBy == Just "carol" && ctx.rejectedBy == Nothing) $
    fail ("PostgreSQL approval unexpected final context: " <> show ctx)

newWorkerCfg :: IO WorkerCfg
newWorkerCfg = do
  workerId <- UUID4.nextRandom
  pure WorkerCfg
    { batchSize = 10
    , pollDelayMs = 100
    , leaseSeconds = 30
    , node = workerId
    }

requireRight :: Show err => String -> Either err a -> IO a
requireRight operation = either (fail . ((operation <> " failed: ") <>) . show) pure

requirePool :: Show err => Either err a -> IO a
requirePool = requireRight "Hasql.Pool.use"

-- An independent DB check; it does not use any in-memory store data.
approvalCounts :: Statement UUID (Int64, Int64, Int64, Int64)
approvalCounts = [singletonStatement|
  SELECT i.revision :: int8,
    (SELECT count(*) :: int8 FROM hfsm.signal s
     WHERE s.instance_id = i.id AND s.status = 'applied') :: int8,
    (SELECT count(*) :: int8 FROM hfsm.step t
     WHERE t.instance_id = i.id) :: int8,
    (SELECT count(*) :: int8 FROM hfsm.outbox o
     WHERE o.instance_id = i.id) :: int8
  FROM hfsm.instance i WHERE i.id = $1 :: uuid
|]

-- The existing demo printer accepts a JSON runtime view. PostgreSQL rows
-- are read back from the database, not reconstructed from execution inputs.
approvalView :: Statement UUID Value
approvalView = [singletonStatement|
 SELECT jsonb_build_object(
   'instance', (SELECT to_jsonb(i) FROM hfsm.instance i WHERE i.id = $1 :: uuid),
   'snapshot', (SELECT to_jsonb(s) FROM hfsm.snapshot s
                JOIN hfsm.instance i ON i.current_snapshot_id = s.id
                WHERE i.id = $1 :: uuid),
   'signals', (SELECT coalesce(jsonb_agg(to_jsonb(s) ORDER BY s.ordinal), '[]'::jsonb)
               FROM hfsm.signal s WHERE s.instance_id = $1 :: uuid),
   'steps', (SELECT coalesce(jsonb_agg(to_jsonb(t) ORDER BY t.revision), '[]'::jsonb)
             FROM hfsm.step t WHERE t.instance_id = $1 :: uuid),
   'outbox', (SELECT coalesce(jsonb_agg(to_jsonb(o) ORDER BY o.uid), '[]'::jsonb)
              FROM hfsm.outbox o WHERE o.instance_id = $1 :: uuid),
   'children', (SELECT coalesce(jsonb_agg(to_jsonb(c) ORDER BY c.uid), '[]'::jsonb)
                FROM hfsm.child_run c WHERE c.parent_instance_id = $1 :: uuid),
   'breaks', (SELECT coalesce(jsonb_agg(to_jsonb(b) ORDER BY b.uid), '[]'::jsonb)
              FROM hfsm.break_event b WHERE b.instance_id = $1 :: uuid),
   'projections', jsonb_build_object(
     'active', (SELECT coalesce(jsonb_agg(p.payload ORDER BY p.revision, p.ordinal), '[]'::jsonb)
                FROM hfsm.projection_event p WHERE p.instance_id = $1 :: uuid AND p.kind = 'active'),
     'trace', (SELECT coalesce(jsonb_agg(p.payload ORDER BY p.revision, p.ordinal), '[]'::jsonb)
               FROM hfsm.projection_event p WHERE p.instance_id = $1 :: uuid AND p.kind = 'trace'),
     'queue', (SELECT coalesce(jsonb_agg(p.payload ORDER BY p.revision, p.ordinal), '[]'::jsonb)
               FROM hfsm.projection_event p WHERE p.instance_id = $1 :: uuid AND p.kind = 'queue'),
     'tree', (SELECT coalesce(jsonb_agg(p.payload ORDER BY p.revision, p.ordinal), '[]'::jsonb)
              FROM hfsm.projection_event p WHERE p.instance_id = $1 :: uuid AND p.kind = 'tree')
   )
 ) :: jsonb
|]

demoSignals :: [(Text, Signal)]
demoSignals =
  [ ("submit", SubmitSg "alice")
  , ("reject", RejectSg "bob" "needs legal note")
  , ("resubmit", ResubmitSg "alice")
  , ("approve", ApproveSg "carol")
  ]

initialCtx :: ApprovalCtx
initialCtx = ApprovalCtx
  { title = "Items custody note"
  , author = ""
  , revision = 0
  , approvedBy = Nothing
  , rejectedBy = Nothing
  , rejectReason = Nothing
  }
