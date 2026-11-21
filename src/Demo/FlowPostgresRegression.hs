{-# LANGUAGE QuasiQuotes #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
-- | DF-01 PostgreSQL diamond DAG and HFSM boundary regression.
-- Use a dedicated test DB with migrations 001..005.
module Demo.FlowPostgresRegression (regressionPg) where

import Control.Concurrent (threadDelay, forkFinally, newEmptyMVar, takeMVar, putMVar)
import Control.Exception (SomeException)
import Control.Monad (unless)
import Control.Monad.Trans.Cont (runContT)
import Data.Aeson (Value(..), toJSON, object)
import qualified Data.Aeson.KeyMap as KM
import Data.Int (Int32)
import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
import Data.Vector (Vector)
import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import Hasql.TH (singletonStatement)

import qualified DB.Connect as Db
import Demo.ApprovalHfsm (approvalSpec, Signal(..))
import Demo.ApprovalRuntime (initialCtx)
import Demo.FlowRegression (diamondSpec)
import Hfsm.Compile (compile)
import qualified Hfsm.Runtime.Engine.Worker as Engine
import Hfsm.Runtime.Model (InstanceRw(..))
import Hfsm.Core.Name (machineNameText)
import Hfsm.Core.Version (machineVersionText)
import Hfsm.Core.Digest (specDigestText)
import Hfsm.Runtime.Registry (emptyRegistry, machineKeyOf, register, MachineKey(..))
import Hfsm.Store.Class (Store(..))
import Hfsm.Store.Postgres.Bootstrap (newInstanceFor, enqueueFor)
import qualified Hfsm.Store.Postgres as Pg
import Hfsm.Flow.Types
  ( StartRun(..), PublishArtifact(..), WorkerResources(..), FlowStore(..)
  , FlowTaskLease(..), FlowInput(..), ArtifactRef(..), TaskOutputs(..), TaskFailure(..) )
import qualified Hfsm.Flow.Postgres as Flow
import Hfsm.Operations.Types (WorkerRegistration(..), EnqueueOptions(..))
import qualified Hfsm.Operations.Postgres as Ops

assert :: Bool -> String -> IO ()
assert yes message = unless yes (fail message)

required :: Show err => String -> Either err a -> IO a
required label = either (fail . ((label <> ": ") <>) . show) pure

one :: Show a => String -> [a] -> IO a
one _ [x] = pure x
one label other = fail (label <> ": expected one item: " <> show other)

valueStatus :: Value -> Maybe Text
valueStatus (Object fields) = case KM.lookup "status" fields of
  Just (String value) -> Just value
  _ -> Nothing
status _ = Nothing

assertStatus :: Pg.Runner -> UUID -> Text -> IO ()
assertStatus runner runId wanted = do
  row <- required "flow summary" =<< Flow.loadRunSummary runner runId
  case row of
    Just json -> assert (valueStatus json == Just wanted)
      ("expected flow status " <> T.unpack wanted <> ", got " <> show json)
    Nothing -> fail "missing flow run"

regressionPg :: Db.PgDbConfig -> IO ()
regressionPg config = runContT (Db.startPg config) $ \pool -> do
  let runner = Pg.runWithPool pool
  tenant <- U4.nextRandom
  nodeA <- U4.nextRandom
  nodeB <- U4.nextRandom
  defId <- U4.nextRandom
  _ <- required "create tenant" =<< Ops.createTenant runner tenant
    ("df01-" <> U.toText tenant) 30 200 10 10 5

  let reg wid = WorkerRegistration
        {workerId=wid,capabilities=["flow:task","flow:execute:fixture","hfsm:signal"],
         heartbeatTtlSeconds=60,maxSignals=10,maxActivities=10}
      resources wid = WorkerResources
        {workerId=wid,cpuUnits=1,memoryMb=128,maxTasks=1}
  _ <- required "register worker A" =<< Ops.registerWorker runner (reg nodeA)
  _ <- required "register worker B" =<< Ops.registerWorker runner (reg nodeB)
  _ <- required "resources A" =<< Flow.registerResources runner (resources nodeA)
  _ <- required "resources B" =<< Flow.registerResources runner (resources nodeB)
  persistedDef <- required "register definition" =<<
    Flow.registerDefinition runner defId tenant "diamond" "1.0" diamondSpec
  defAgain <- required "idempotent registration" =<<
    Flow.registerDefinition runner defId tenant "diamond" "1.0" diamondSpec
  assert (persistedDef == defAgain) "definition was not immutable/idempotent"

  spec <- required "approvalSpec" approvalSpec
  compiled <- required "compile" (compile spec)
  registry <- required "registry" (register compiled emptyRegistry)
  let machineKey = machineKeyOf compiled
      release = machineNameText machineKey.name <> "@" <>
        machineVersionText machineKey.version <> "#" <> specDigestText machineKey.digest
  required "register machine" =<< Pg.registerMachine runner machineKey
  newInst <- required "initial HFSM" =<< newInstanceFor compiled initialCtx Nothing
  sourceInstance <- required "HFSM source" =<<
    Ops.createTenantInstance runner tenant "default" newInst

  -- Produce a REAL CT-03 step/outbox intent before invoking the flow bridge.
  submitted <- enqueueFor compiled sourceInstance (SubmitSg "alice") (Just "df01/submit")
  correlation <- U4.nextRandom
  _ <- required "submit signal" =<< Ops.enqueueWithPolicy runner submitted EnqueueOptions
    { priority=0,correlationId=Just correlation,causationId=Nothing,
      traceparent=Nothing,expectedRelease=Nothing }
  let engineCfg = Engine.WorkerCfg
        {batchSize=1,pollDelayMs=1,leaseSeconds=10,node=nodeA}
  Engine.runTick engineCfg (Ops.productionStoreForInstance runner sourceInstance) registry
  firstStep <- (Ops.productionStore runner).loadInstance sourceInstance
  case firstStep of
    Nothing -> fail "missing HFSM source"
    Just row -> assert (row.revision==1) "HFSM source did not produce the flow-start effect"
  sourceEffectKey <- required "source outbox" =<<
    Pool.use pool (Session.statement sourceInstance readSourceEffect)

  runId <- U4.nextRandom
  let source = StartRun
        {runId=runId,definitionId=defId,
         sourceEffectKey=sourceEffectKey,
         sourceInstance=Just sourceInstance,sourceRelease=Just release,
         runInput=object [],successSignal=Just (toJSON (ApproveSg "flow-done")),
         failureSignal=Just (toJSON (RejectSg "flow" "failed"))}
  started <- required "start HFSM flow" =<< Flow.startRun runner source
  assert (started==runId) "start returned wrong run"
  proposedId <- U4.nextRandom
  same <- required "idempotent start" =<< Flow.startRun runner (source {runId=proposedId})
  assert (same==runId) "same source effect created duplicate flow"
  let
    api = Flow.flowStoreForRun runner runId
    claim wid = api.claim wid 1 10 >>= required "claim task"

    done :: FlowTaskLease -> [(Text, UUID)] -> IO Bool
    done lease refs = api.finish TaskOutputs {
        runId = lease.runId, taskKey = lease.taskKey ,leaseId = lease.leaseId,
        workerId = lease.workerId, attempt = lease.attempt, outputs = refs
        }
        >>= required "finish task"
    artifact key digest = do
      aid <- U4.nextRandom
      required "publish CAS manifest" =<< Flow.publishArtifact runner PublishArtifact {
          artifactId = aid,  tenantId = tenant,  digest = digest
          , sizeBytes = fromIntegral (T.length key)
          , storageUri = "cas://df01-fixture/" <> key
          , mediaType = "text/plain",  metadata = object []
          }
  alpha <- artifact "alpha" "8ed3f6ad685b959ead7022518e1af76cd816f8e8ec7ccdda1ed4018e8f2223f8"
  beta  <- artifact "beta"  "f44e64e75f3948e9f73f8dfa94721c4ce8cbb4f265c4790c702b2d41cfbf2753"
  gamma <- artifact "gamma" "be9d587defa1f0c09ef49eb17e206983a5f8f8289e4281860bd0ee5a19592c67"
  delta <- artifact "delta" "4f4a9410ffcdf895c4adb880659e9b5c0dd1f23a30790684340b3eaacb045398"

  root <- one "A" =<< claim nodeA
  assert (root.taskKey=="A" && null root.inputs) "root task readiness incorrect"
  stillBlocked <- claim nodeB
  assert (null stillBlocked) "downstream ran before source output committed"
  invalidBinding <- required "reject invalid port" =<< api.finish TaskOutputs
    {runId=root.runId,taskKey=root.taskKey,leaseId=root.leaseId,
     workerId=root.workerId,attempt=root.attempt,outputs=[("not-declared",alpha)]}
  assert (not invalidBinding) "unknown output port was accepted"
  preCounts <- required "no partial output commit" =<<
    Pool.use pool (Session.statement runId countRun)
  assert (preCounts==(4,0,0,1)) "invalid output left a token or output behind"
  completedA <- done root [("a",alpha)]
  assert completedA "A failed to finish"

  b <- one "B" =<< claim nodeA
  c <- one "C" =<< claim nodeB
  assert (b.taskKey=="B" && c.taskKey=="C") "parallel branches were not ready"
  assert (length b.inputs==1 && length c.inputs==1) "incorrect input token fan-out"
  assert (all (\v -> v.artifact.artifactId==alpha) (b.inputs<>c.inputs))
    "branch tokens did not reference the source artifact"
  noCapacity <- claim nodeA
  assert (null noCapacity) "worker resource capacity exceeded"
  completedB <- done b [("b",beta)]
  assert completedB "B failed to finish"
  stillWaiting <- claim nodeA
  assert (null stillWaiting) "D ran before second input token"
  completedC <- done c [("c",gamma)]
  assert completedC "C failed to finish"
  d <- one "D" =<< claim nodeA
  assert (d.taskKey=="D" && length d.inputs==2) "D did not collect two tokens"
  assert (map (.port) d.inputs==["b","c"]) "D ports are not deterministic"
  completedD <- done d [("result",delta)]
  assert completedD "D failed to finish"
  assertStatus runner runId "succeeded"
  counts <- required "flow counts" =<< Pool.use pool (Session.statement runId countRun)
  assert (counts==(4,4,4,4)) ("wrong tasks/tokens/outputs/attempts: " <> show counts)
  manifests <- required "read artifact lineage" =<< Flow.loadRunOutputs runner runId
  assert (length manifests==4) "published output manifest not inspectable"

  delivered <- Flow.deliverCompletion runner runId >>= required "deliver to HFSM"
  duplicate <- Flow.deliverCompletion runner runId >>= required "duplicate delivery"
  assert (delivered/=Nothing && delivered==duplicate)
    "completion produced duplicate HFSM signals"
  Engine.runTick engineCfg (Ops.productionStoreForInstance runner sourceInstance) registry
  sourceRow <- (Ops.productionStore runner).loadInstance sourceInstance
  case sourceRow of
    Nothing -> fail "missing HFSM source after completion"
    Just row -> assert (row.revision==2)
      "flow completion did not create exactly one additional HFSM turn"

  -- Competing independent pooled claimers cannot lease the same root.
  raceRun <- U4.nextRandom
  let standalone ident tag = StartRun
        {runId=ident,definitionId=defId,sourceEffectKey=tag,
         sourceInstance=Nothing,sourceRelease=Nothing,runInput=object [],
         successSignal=Nothing,failureSignal=Nothing}
  _ <- required "race run" =<< Flow.startRun runner
    (standalone raceRun ("df01/race/"<>U.toText raceRun))
  let raceApi = Flow.flowStoreForRun runner raceRun
  slotA <- newEmptyMVar
  slotB <- newEmptyMVar
  _ <- forkFinally (raceApi.claim nodeA 1 10) (putMVar slotA)
  _ <- forkFinally (raceApi.claim nodeB 1 10) (putMVar slotB)
  raceA <- takeMVar slotA
  raceB <- takeMVar slotB
  let unwrapRace result = case result of
        Left err -> fail ("claim thread: " <> show err)
        Right e -> required "claim race" e
  leasesA <- unwrapRace raceA
  leasesB <- unwrapRace raceB
  assert (length (leasesA<>leasesB)==1)
    "two workers leased the same task concurrently"
  _ <- required "race cleanup" =<< Flow.cancelRun runner raceRun "regression cleanup"

  -- Expiry/reclaim: two workers may calculate, but one token wins the fence.
  staleRun <- U4.nextRandom
  _ <- required "stale run" =<< Flow.startRun runner
    (standalone staleRun ("df01/stale/"<>U.toText staleRun))
  let staleApi = Flow.flowStoreForRun runner staleRun
  old <- one "old lease" =<< (staleApi.claim nodeA 1 1 >>= required "first stale claim")
  threadDelay 1400000
  recovered <- one "recovered lease" =<< (staleApi.claim nodeB 1 10 >>= required "recovery claim")
  assert (old.taskKey==recovered.taskKey && old.leaseId/=recovered.leaseId)
    "recovery did not fence expired owner"
  oldCommit <- required "old completion" =<< staleApi.finish TaskOutputs
    {runId=old.runId,taskKey=old.taskKey,leaseId=old.leaseId,
     workerId=old.workerId,attempt=old.attempt,outputs=[("a",alpha)]}
  assert (not oldCommit) "expired worker committed token"
  newCommit <- required "new completion" =<< staleApi.finish TaskOutputs
    {runId=recovered.runId,taskKey=recovered.taskKey,leaseId=recovered.leaseId,
     workerId=recovered.workerId,attempt=recovered.attempt,outputs=[("a",alpha)]}
  assert newCommit "recovered worker did not commit"
  _ <- required "stale test cleanup" =<< Flow.cancelRun runner staleRun "regression cleanup"

  failedRun <- U4.nextRandom
  _ <- required "failure fixture" =<< Flow.startRun runner
    (standalone failedRun ("df01/failure/"<>U.toText failedRun))
  let failedApi = Flow.flowStoreForRun runner failedRun
  attempted <- one "failure claim" =<< (failedApi.claim nodeA 1 10 >>= required "failure claim")
  failed <- required "failure acknowledgement" =<< failedApi.failTask TaskFailure
    {runId=attempted.runId,taskKey=attempted.taskKey,
     leaseId=attempted.leaseId,workerId=attempted.workerId,
     attempt=attempted.attempt,reason="permanent fixture failure",retryable=False}
  assert failed "permanent error not recorded"
  assertStatus runner failedRun "failed"
  deadLetters <- required "flow DLQ" =<< Pool.use pool
    (Session.statement failedRun countDeadLetters)
  assert (deadLetters==1) "failed flow did not produce a dead letter"
  putStrLn "FlowPostgresRegression: PASS"

countRun :: Statement UUID (Int32,Int32,Int32,Int32)
countRun = [singletonStatement|
 SELECT (SELECT count(*)::int4 FROM hfsm.flow_task WHERE run_id=$1 :: uuid) :: int4,
        (SELECT count(*)::int4 FROM hfsm.flow_token WHERE run_id=$1 :: uuid) :: int4,
        (SELECT count(*)::int4 FROM hfsm.flow_task_output WHERE run_id=$1 :: uuid) :: int4,
        (SELECT count(*)::int4 FROM hfsm.flow_task_attempt WHERE run_id=$1 :: uuid) :: int4
|]

countDeadLetters :: Statement UUID Int32
countDeadLetters = [singletonStatement|
 SELECT count(*)::int4 FROM hfsm.flow_dead_letter WHERE run_id=$1 :: uuid
|]

readSourceEffect :: Statement UUID Text
readSourceEffect = [singletonStatement|
 SELECT effect_key :: text FROM hfsm.outbox
 WHERE instance_id=$1 :: uuid ORDER BY uid LIMIT 1
|]
