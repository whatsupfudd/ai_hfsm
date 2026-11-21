{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}

-- | DB regression: finite expansion, immutable partition lineage, deterministic
-- multi-level aggregation, stale fences, atomic failure and HFSM completion.
-- Run ONLY on a dedicated DB with Support/001..006 applied.
module Demo.FlowDynamicPostgresRegression (regressionPg) where

import Control.Concurrent (threadDelay, forkFinally, newEmptyMVar, putMVar, takeMVar)
import Control.Monad (unless)
import Data.List (sort)
import Control.Monad.Trans.Cont (runContT)
import Data.Aeson (Value(..), toJSON, object)
import qualified Data.Aeson.KeyMap as KM
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import Hasql.TH (singletonStatement)

import qualified DB.Connect as Db
import Demo.ApprovalHfsm (approvalSpec, Signal(..))
import Demo.ApprovalRuntime (initialCtx)
import Demo.FlowDynamicRegression (exampleScatter)
import Hfsm.Compile (compile)
import qualified Hfsm.Runtime.Engine.Worker as Engine
import Hfsm.Core.Name (machineNameText)
import Hfsm.Core.Version (machineVersionText)
import Hfsm.Core.Digest (specDigestText)
import Hfsm.Runtime.Model (InstanceRw(..))
import Hfsm.Runtime.Registry (emptyRegistry, machineKeyOf, register, MachineKey(..))
import Hfsm.Store.Class (Store(..))
import Hfsm.Store.Postgres.Bootstrap (newInstanceFor, enqueueFor)
import qualified Hfsm.Store.Postgres as Pg
import Hfsm.Flow.Spec (FlowSpec(..), TaskDef(..))
import Hfsm.Flow.Types
  ( StartRun(..), PublishArtifact(..), WorkerResources(..), FlowStore(..),
    FlowTaskLease(..), FlowInput(..), ArtifactRef(..), TaskOutputs(..))
import qualified Hfsm.Flow.Postgres as Flow
import qualified Hfsm.Flow.Worker as FlowWorker
import Hfsm.Flow.Dynamic.Provider (newScatterProvider)
import Hfsm.Flow.Dynamic.Spec (ScatterPlan(..))
import qualified Hfsm.Flow.Dynamic.Postgres as Dynamic
import Hfsm.Flow.Dynamic.Types (ScatterSummary(..))
import Hfsm.Operations.Types (WorkerRegistration(..), EnqueueOptions(..))
import qualified Hfsm.Operations.Postgres as Ops

assert :: Bool -> String -> IO ()
assert condition message = unless condition (fail message)

required :: Show err => String -> Either err result -> IO result
required label = either (fail . ((label <> ": ") <>) . show) pure

one :: Show item => String -> [item] -> IO item
one _ [item] = pure item
one label other = fail (label <> ": expected one lease, received " <> show other)

readStatus :: Value -> Maybe Text
readStatus (Object fields) = case KM.lookup "status" fields of
  Just (String value) -> Just value
  _ -> Nothing
readStatus _ = Nothing

assertRunStatus :: Pg.Runner -> UUID -> Text -> IO ()
assertRunStatus runner runId expectedStatus = do
  response <- required "flow summary" =<< Flow.loadRunSummary runner runId
  case response of
    Just value -> assert (readStatus value == Just expectedStatus)
      ("unexpected run status: " <> show value)
    Nothing -> fail "run missing"

regressionPg :: Db.PgDbConfig -> IO ()
regressionPg config = runContT (Db.startPg config) $ \pool -> do
  let runner = Pg.runWithPool pool
  tenant <- U4.nextRandom
  workerA <- U4.nextRandom
  workerB <- U4.nextRandom
  definitionId <- U4.nextRandom
  _ <- required "tenant" =<< Ops.createTenant runner tenant
    ("df02-" <> U.toText tenant) 30 200 20 20 5
  let capabilities = ["flow:task","flow:execute:df2.scatter",
                      "flow:execute:df2.partition","flow:execute:df2.reduce",
                      "hfsm:signal"]
      registerWorker worker = WorkerRegistration
        { workerId=worker,capabilities=capabilities,heartbeatTtlSeconds=60
        , maxSignals=10,maxActivities=10 }
      resources worker = WorkerResources
        {workerId=worker,cpuUnits=2,memoryMb=256,maxTasks=2}
  _ <- required "worker A" =<< Ops.registerWorker runner (registerWorker workerA)
  _ <- required "worker B" =<< Ops.registerWorker runner (registerWorker workerB)
  _ <- required "resources A" =<< Flow.registerResources runner (resources workerA)
  _ <- required "resources B" =<< Flow.registerResources runner (resources workerB)

  -- A zero-output scatter sink is part of the immutable DF-01 definition;
  -- DF-02 generates only its finite, one-shot child reduction graph.
  let staticGraph = FlowSpec
        { tasks=[TaskDef
          {key="scatter",executor="df2.scatter",inputs=[],outputs=[]
          ,cpu=1,memoryMb=64,capabilities=[],maxAttempts=3
          ,timeoutSeconds=20,priority=0}]
        , edges=[] }
  _ <- required "definition" =<< Flow.registerDefinition runner definitionId
    tenant "df02-finite" "1.0" staticGraph
  let
    standalone runId = StartRun
        {runId=runId,definitionId=definitionId,sourceEffectKey="df02/"<>U.toText runId
        ,sourceInstance=Nothing,sourceRelease=Nothing,runInput=object []
        ,successSignal=Nothing,failureSignal=Nothing}

    claim :: FlowStore IO -> UUID -> Int -> IO [FlowTaskLease]
    claim api worker ttl = api.claim worker 1 ttl >>= required "claim"

    finish :: FlowStore IO -> FlowTaskLease -> Text -> UUID -> IO Bool
    finish api lease outputPort artifactId = api.finish TaskOutputs {
          runId=lease.runId,taskKey=lease.taskKey,leaseId=lease.leaseId
        , workerId=lease.workerId,attempt=lease.attempt
        , outputs=[(outputPort,artifactId)] }
      >>= required "finish"

    publish marker = do
      artifactId <- U4.nextRandom
      required "artifact manifest" =<< Flow.publishArtifact runner PublishArtifact
        { artifactId=artifactId,tenantId=tenant,digest=T.replicate 64 marker
        , sizeBytes=1,storageUri="cas://df02-test/"<>marker
        , mediaType="application/octet-stream",metadata=object [] }

  inputArtifacts <- traverse publish ["a","b","c","d","e"]
  emptyResult <- publish "f"
  mainRun <- U4.nextRandom
  _ <- required "start standalone" =<< Flow.startRun runner (standalone mainRun)
  let
    api = Dynamic.dynamicFlowStoreForRun runner mainRun
    plan = exampleScatter (zip ["part-01","part-02","part-03","part-04","part-05"] inputArtifacts) 2
  source <- one "scatter source" =<< claim api workerA 10
  assert (source.taskKey=="scatter" && null source.outputs)
    "wrong static scatter contract"

  -- Invalid artifact ref must reject the ENTIRE expansion before any child
  -- task/edge appears. The same original lease then remains usable.
  let invalidPlan = exampleScatter [("missing",U.nil)] 2
  rejected <- Dynamic.expandScatter runner source invalidPlan
  assert (case rejected of Left _ -> True; _ -> False)
    "unpublished partition artifact was accepted"
  absent <- required "expansion rollback" =<< Dynamic.loadScatterSummary runner mainRun
  assert (absent==Nothing) "failed expansion partially committed group"
  initialCount <- required "no partial task insertion" =<<
    Pool.use pool (Session.statement mainRun countTasks)
  assert (initialCount==1) "failed expansion left dynamic tasks"

  groupId <- required "scatter expansion" =<< Dynamic.expandScatter runner source plan
  groupIdAgain <- required "same-plan idempotence" =<< Dynamic.expandScatter runner source plan
  assert (groupId==groupIdAgain) "duplicate scatter expanded again"
  conflicting <- Dynamic.expandScatter runner source (plan {fanIn=3})
  assert (case conflicting of Left _ -> True; _ -> False)
    "conflicting expansion contract accepted"
  recognized <- required "scatter acknowledgement" =<< api.finish TaskOutputs
    {runId=source.runId,taskKey=source.taskKey,leaseId=source.leaseId
    ,workerId=source.workerId,attempt=source.attempt,outputs=[]}
  assert recognized "already committed scatter not recognized by DF-01 adapter"
  summary <- required "group summary" =<< Dynamic.loadScatterSummary runner mainRun
  case summary of
    Just row -> assert (row.partitionCount==5 && row.fanIn==2)
      "unexpected scatter group summary"
    Nothing -> fail "group not materialized"
  assertRunStatus runner mainRun "running"

  -- Ordinary DF-01 claims now schedule partition and reducer tasks. The
  -- DF-02 adapter injects immutable item artifacts ONLY into leaves; the
  -- reducers receive ordered tokens i000001..i000064 from prior completions.
  let dispatch leased = case leased.executor of
        "df2.partition" -> do
          assert (map (.port) leased.inputs==["item"])
            "partition did not receive immutable item binding"
          let artifactId=(head leased.inputs).artifact.artifactId
          didFinish <- finish api leased "value" artifactId
          assert didFinish "partition completion rejected"
        "df2.reduce" -> do
          let ports = map (.port) leased.inputs
          assert (ports==sort ports) "aggregation ports are not deterministic"
          let artifactId=case leased.inputs of
                [] -> emptyResult
                firstInput:_ -> firstInput.artifact.artifactId
              outputPort=case leased.outputs of
                [port] -> port
                _ -> error "reducer expected exactly one output port"
          didFinish <- finish api leased outputPort artifactId
          assert didFinish "reducer completion rejected"
        other -> fail ("unexpected dynamic executor "<>T.unpack other)
      drain count
        | count>=40 = fail "bounded reduction failed to terminate"
        | otherwise = do
            current <- required "run state" =<< Flow.loadRunSummary runner mainRun
            case current of
              Just state | readStatus state==Just "succeeded" -> pure ()
              _ -> do
                first <- claim api workerA 1
                second <- claim api workerB 1
                assert (not (null first && null second))
                  "no runnable task while group remains running"
                mapM_ dispatch (first<>second)
                drain (count+1)
  drain (0 :: Int)
  assertRunStatus runner mainRun "succeeded"
  counts <- required "graph counts" =<< Pool.use pool
    (Session.statement mainRun countGraph)
  assert (counts==(12,10,11,12))
    ("wrong static/dynamic tasks, edges, outputs or attempts: "<>show counts)
  finishedGroup <- required "finished group" =<< Dynamic.loadScatterSummary runner mainRun
  case finishedGroup of
    Just row -> assert (row.rootStatus=="done" && row.rootResult/=Nothing)
      "reduction root missing immutable result artifact"
    Nothing -> fail "finished group missing"

  -- Validate the production provider path, not only direct SQL calls.
  -- DF-01 Worker calls finish a second time after the provider expands;
  -- dynamicFlowStore recognizes that exact prior fence as already committed.
  providerRun <- U4.nextRandom
  _ <- required "provider run" =<< Flow.startRun runner (standalone providerRun)
  let providerApi = Dynamic.dynamicFlowStoreForRun runner providerRun
      workerCfg = FlowWorker.FlowWorkerCfg
        { workerId=workerA,batchSize=1,leaseSeconds=10,pollDelayMs=10 }
      provider = newScatterProvider runner (\_ -> pure (Right
        (exampleScatter [("provider",head inputArtifacts)] 2)))
  providerReports <- required "scatter TaskProvider path" =<<
    FlowWorker.runTick workerCfg providerApi [provider]
  assert (length providerReports==1) "scatter provider failed its duplicate finish acknowledgement"
  providerGroup <- required "provider group" =<< Dynamic.loadScatterSummary runner providerRun
  assert (providerGroup/=Nothing) "provider did not atomically expand its graph"
  _ <- required "provider fixture cleanup" =<<
    Flow.cancelRun runner providerRun "DF-02 provider fixture cleanup"

  -- Two pooled clients retrying the SAME leased scatter simultaneously
  -- must observe one immutable expansion. The admission gate serializes them.
  racingRun <- U4.nextRandom
  _ <- required "racing run" =<< Flow.startRun runner (standalone racingRun)
  let racingApi = Dynamic.dynamicFlowStoreForRun runner racingRun
  racingLease <- one "racing source" =<< claim racingApi workerA 10
  raceLeft <- newEmptyMVar
  raceRight <- newEmptyMVar
  _ <- forkFinally (Dynamic.expandScatter runner racingLease plan) (putMVar raceLeft)
  _ <- forkFinally (Dynamic.expandScatter runner racingLease plan) (putMVar raceRight)
  firstRaceResult <- takeMVar raceLeft
  secondRaceResult <- takeMVar raceRight
  let unpackRace result = case result of
        Left exception -> fail ("concurrent expansion: "<>show exception)
        Right answer -> required "concurrent expansion" answer
  firstGroup <- unpackRace firstRaceResult
  secondGroup <- unpackRace secondRaceResult
  assert (firstGroup==secondGroup) "racing expanders created different groups"
  racingCount <- required "racing graph count" =<<
    Pool.use pool (Session.statement racingRun countTasks)
  assert (racingCount==12) "race duplicated dynamic tasks"
  _ <- required "racing fixture cleanup" =<<
    Flow.cancelRun runner racingRun "DF-02 race fixture cleanup"

  -- Expired source cannot create a group; the reclaimed fence alone may do so.
  staleRun <- U4.nextRandom
  _ <- required "stale run" =<< Flow.startRun runner (standalone staleRun)
  let staleApi = Dynamic.dynamicFlowStoreForRun runner staleRun
  expired <- one "stale original" =<< claim staleApi workerA 1
  threadDelay 1350000
  recovered <- one "stale recovered" =<< claim staleApi workerB 10
  assert (expired.leaseId/=recovered.leaseId) "expired fence was reused"
  expiredAttempt <- Dynamic.expandScatter runner expired plan
  assert (case expiredAttempt of Left _ -> True; _ -> False)
    "expired owner dynamically expanded graph"
  missing <- required "no stale graph" =<< Dynamic.loadScatterSummary runner staleRun
  assert (missing==Nothing) "stale owner created partial graph"
  _ <- required "reclaimed owner expansion" =<< Dynamic.expandScatter runner recovered plan
  _ <- required "stale fixture cleanup" =<< Flow.cancelRun runner staleRun "DF-02 cleanup"

  -- Zero partitions still produce one ready reducer with zero tokens; no
  -- special synthetic artifact and no coordinator-managed barrier is needed.
  zeroRun <- U4.nextRandom
  _ <- required "zero start" =<< Flow.startRun runner (standalone zeroRun)
  let zeroApi = Dynamic.dynamicFlowStoreForRun runner zeroRun
  zeroSource <- one "zero source" =<< claim zeroApi workerA 10
  _ <- required "zero expansion" =<< Dynamic.expandScatter runner zeroSource
    (exampleScatter [] 8)
  zeroReducer <- one "zero reducer" =<< claim zeroApi workerB 10
  assert (zeroReducer.executor=="df2.reduce" && null zeroReducer.inputs &&
          zeroReducer.outputs==["result"])
    "zero-partition identity reducer not ready"
  zeroDone <- finish zeroApi zeroReducer "result" emptyResult
  assert zeroDone "zero reducer failed"
  assertRunStatus runner zeroRun "succeeded"

  -- Actual HFSM boundary: the source revision produces an outbox intent;
  -- successful reduction emits one deduplicated completion signal.
  approval <- required "approval spec" approvalSpec
  compiled <- required "compile approval" (compile approval)
  registry <- required "approval registry" (register compiled emptyRegistry)
  let machineKey = machineKeyOf compiled
      release = machineNameText machineKey.name <> "@" <>
        machineVersionText machineKey.version <> "#" <> specDigestText machineKey.digest
  required "HFSM machine registration" =<< Pg.registerMachine runner machineKey
  initial <- required "HFSM initialization" =<< newInstanceFor compiled initialCtx Nothing
  sourceInstance <- required "HFSM tenant instance" =<<
    Ops.createTenantInstance runner tenant "default" initial
  submitted <- enqueueFor compiled sourceInstance (SubmitSg "alice")
     (Just "df02/hfsm/submit")
  correlation <- U4.nextRandom
  _ <- required "submit signal" =<< Ops.enqueueWithPolicy runner submitted EnqueueOptions
    { priority=0,correlationId=Just correlation,causationId=Nothing
    ,traceparent=Nothing,expectedRelease=Nothing }
  let engine = Engine.WorkerCfg
        {batchSize=1,pollDelayMs=1,leaseSeconds=10,node=workerA}
  Engine.runTick engine (Ops.productionStoreForInstance runner sourceInstance) registry
  current <- (Ops.productionStore runner).loadInstance sourceInstance
  case current of
    Just instanceRow -> assert (instanceRow.revision==1) "HFSM setup turn failed"
    Nothing -> fail "HFSM source missing"
  sourceEffectKey <- required "outbox source" =<< Pool.use pool
    (Session.statement sourceInstance readSourceEffect)
  hfsmRun <- U4.nextRandom
  _ <- required "HFSM flow start" =<< Flow.startRun runner StartRun
    {runId=hfsmRun,definitionId=definitionId,sourceEffectKey=sourceEffectKey
    ,sourceInstance=Just sourceInstance,sourceRelease=Just release
    ,runInput=object [],successSignal=Just (toJSON (ApproveSg "df02"))
    ,failureSignal=Just (toJSON (RejectSg "df02" "failed"))}
  let hfsmApi = Dynamic.dynamicFlowStoreForRun runner hfsmRun
  hfsmScatter <- one "HFSM scatter" =<< claim hfsmApi workerA 10
  _ <- required "HFSM scatter expansion" =<< Dynamic.expandScatter runner hfsmScatter
    (exampleScatter [("one",head inputArtifacts)] 8)
  hfsmLeaf <- one "HFSM partition" =<< claim hfsmApi workerA 10
  assert (hfsmLeaf.executor=="df2.partition") "expected HFSM flow partition"
  doneLeaf <- finish hfsmApi hfsmLeaf "value" (head inputArtifacts)
  assert doneLeaf "HFSM flow partition failed"
  hfsmReduce <- one "HFSM reducer" =<< claim hfsmApi workerA 10
  doneReduce <- finish hfsmApi hfsmReduce "result" (head inputArtifacts)
  assert doneReduce "HFSM flow reducer failed"
  assertRunStatus runner hfsmRun "succeeded"
  delivered <- required "HFSM completion" =<< Flow.deliverCompletion runner hfsmRun
  duplicate <- required "HFSM duplicate completion" =<< Flow.deliverCompletion runner hfsmRun
  assert (delivered/=Nothing && delivered==duplicate)
    "duplicate dataflow completion created a second signal"
  Engine.runTick engine (Ops.productionStoreForInstance runner sourceInstance) registry
  after <- (Ops.productionStore runner).loadInstance sourceInstance
  case after of
    Just row -> assert (row.revision==2) "HFSM completion not processed once"
    Nothing -> fail "HFSM source disappeared"
  putStrLn "FlowDynamicPostgresRegression: PASS"

countTasks :: Statement UUID Int32
countTasks = [singletonStatement|
 SELECT count(*)::int4 FROM hfsm.flow_task WHERE run_id=$1 :: uuid
|]

countGraph :: Statement UUID (Int32,Int32,Int32,Int32)
countGraph = [singletonStatement|
 SELECT (SELECT count(*)::int4 FROM hfsm.flow_task WHERE run_id=$1 :: uuid) :: int4,
        (SELECT count(*)::int4 FROM hfsm.flow_token WHERE run_id=$1 :: uuid) :: int4,
        (SELECT count(*)::int4 FROM hfsm.flow_task_output WHERE run_id=$1 :: uuid) :: int4,
        (SELECT count(*)::int4 FROM hfsm.flow_task_attempt WHERE run_id=$1 :: uuid) :: int4
|]

readSourceEffect :: Statement UUID Text
readSourceEffect = [singletonStatement|
 SELECT effect_key :: text FROM hfsm.outbox
 WHERE instance_id=$1 :: uuid ORDER BY uid LIMIT 1
|]
