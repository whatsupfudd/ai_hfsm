{-# LANGUAGE NoFieldSelectors #-}
-- | CT-06 deterministic policy, tenant isolation, producer dedupe, priority,
-- group/tenant admission, dead letters, projections, replay and crash fences.
-- DB regression should run on a dedicated test database (001..004 installed).
module Demo.OperationsRegression (regression, regressionPg) where

import Control.Exception (SomeException, try)
import Control.Monad (unless)
import Control.Monad.Trans.Cont (runContT)
import Data.Aeson (Value(..), object, (.=))
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4
import Data.UUID (UUID)

import qualified DB.Connect as Db
import Demo.ApprovalHfsm (approvalSpec, Signal(..))
import Demo.ApprovalRuntime (initialCtx)
import Hfsm.Activity.Types
  ( ActivityStore(..), Provider(..), DeliveryReport(..))
import qualified Hfsm.Activity.Types as AT
import qualified Hfsm.Activity.Worker as Activity
import Hfsm.Compile (compile)
import qualified Hfsm.Runtime.Engine.Worker as Engine
import Hfsm.History.Postgres (loadHistoryPage)
import Hfsm.History.Replay (verifyTimeline)
import Hfsm.Runtime.Model (InstanceRw(..), SignalRw(..), SignalStatus(..))
import Hfsm.Runtime.Registry (emptyRegistry, machineKeyOf, register)
import Hfsm.Store.Class (Store(..), LeaseRq(..), LeaseRez(..), AckRq(..))
import qualified Hfsm.Store.Postgres as Pg
import Hfsm.Store.Postgres.Bootstrap (newInstanceFor, enqueueFor)
import Hfsm.Operations.Trace
  (parseTraceparent, formatTraceparent, newRootContext)
import Hfsm.Operations.Types
  (WorkerRegistration(..), EnqueueOptions(..), CancelRequest(..), CancelResult(..), DeadLetter(..))
import Hfsm.Operations.Fault
  (Boundary(..), SimulatedCrash(..), newFaultHook, faultedStore, faultedActivityStore)
import qualified Hfsm.Operations.Postgres as Ops

assert :: Bool -> String -> IO ()
assert yes problem = unless yes (fail problem)

required :: Show e => String -> Either e a -> IO a
required label = either (fail . ((label <> ": ") <>) . show) pure

one :: [a] -> IO a
one [x] = pure x
one xs = fail ("wanted one item, got "<>show (length xs))

regression :: IO ()
regression = do
  assert (parseTraceparent "00-00000000000000000000000000000000-1111111111111111-01" == Nothing)
    "all-zero trace ID unexpectedly accepted"
  assert (parseTraceparent "00-11111111111111111111111111111111-0000000000000000-01" == Nothing)
    "all-zero span ID unexpectedly accepted"
  ctx <- newRootContext
  assert (parseTraceparent (formatTraceparent ctx) == Just ctx)
    "W3C traceparent round trip"
  fired <- newFaultHook [(BeforeClaim,2)]
  fired BeforeClaim
  failure <- try (fired BeforeClaim) :: IO (Either SimulatedCrash ())
  case failure of
    Left (SimulatedCrash BeforeClaim) -> pure ()
    _ -> fail "deterministic fault injection did not fire at second claim"
  putStrLn "OperationsRegression (pure): PASS"

regressionPg :: Db.PgDbConfig -> IO ()
regressionPg db = runContT (Db.startPg db) $ \pool -> do
  let runner = Pg.runWithPool pool
  tenant <- U4.nextRandom
  firstNode <- U4.nextRandom
  secondNode <- U4.nextRandom
  generated <- U4.nextRandom
  _ <- required "tenant registration" =<< Ops.createTenant runner tenant
    ("ct06-" <> U.toText generated) 20 50 1 1 4
  let capabilities = ["hfsm:signal", "activity:cmd"]
      makeWorker wid = WorkerRegistration
        {workerId=wid,capabilities=capabilities,heartbeatTtlSeconds=60,
         maxSignals=4,maxActivities=4}
  _ <- required "worker A" =<< Ops.registerWorker runner (makeWorker firstNode)
  _ <- required "worker B" =<< Ops.registerWorker runner (makeWorker secondNode)

  spec <- required "approval spec" approvalSpec
  compiled <- required "compile" (compile spec)
  registry <- required "registry" (register compiled emptyRegistry)
  required "machine release" =<< Pg.registerMachine runner (machineKeyOf compiled)

  let
    makeInstance = do
        initial <- required "instance data" =<< newInstanceFor compiled initialCtx Nothing
        required "tenant instance" =<< Ops.createTenantInstance runner tenant "default" initial
    opts priority correlation = EnqueueOptions {
        priority = priority, correlationId = Just correlation, causationId = Nothing,
        traceparent = Nothing, expectedRelease = Nothing
        }
    putSignal instanceId label priority = do
      rq <- enqueueFor compiled instanceId (SubmitSg "alice") (Just label)
      Ops.enqueueWithPolicy runner rq (opts priority rq.signalId) >>= required "managed enqueue"
    engineCfg node = Engine.WorkerCfg { batchSize = 1, pollDelayMs = 1, leaseSeconds = 30, node = node }
    claim node = LeaseRq {node = node, batchSize = 1, leaseSeconds = 30, now = read "2020-01-01 00:00:00 UTC"}

    checkedInstance :: Store IO -> UUID -> IO InstanceRw
    checkedInstance api instanceId = do
        observed <- api.loadInstance instanceId
        maybe (fail "missing instance") pure observed

  low <- makeInstance
  high <- makeInstance
  _ <- putSignal low "ct06/low" (-10)
  highId <- putSignal high "ct06/high" 10
  -- Idempotent enqueue of a duplicate producer request.
  dupReq <- enqueueFor compiled high (SubmitSg "alice") (Just "ct06/high")
  again <- Ops.enqueueWithPolicy runner dupReq (opts 10 dupReq.signalId)
    >>= required "deduplicated enqueue"
  assert (again==highId) "producer dedupe did not return original signal"

  let full = Ops.productionStore runner
  first <- full.leaseSignal (claim firstNode)
  highSignal <- case first of
    LeaseSomeLz [s] -> pure s
    other -> fail ("wanted one prioritized signal, got "<>show other)
  assert (highSignal.inst==high) "priority admission did not favor high-priority instance"
  other <- full.leaseSignal (claim secondNode)
  assert (other==LeaseNoneLz) "tenant concurrent signal cap was violated"
  lease <- maybe (fail "signal missing lease") pure highSignal.lease
  -- A failed signal produces durable dead-letter evidence immediately.
  required "failed signal ack" =<< full.ackSignal AckRq
    {signal=highSignal.uuid,lease=lease,status=FailedSs,
     note=Just "fault injection: non-retryable",now=read "2020-01-01 00:00:00 UTC"}
  dlq <- Ops.deadLetters runner tenant 20 >>= required "dead letters"
  assert (any (\d -> d.sourceId==highSignal.uuid) dlq)
    "failed signal missing from dead-letter log"
  let sourceDl = head (filter (\d -> d.sourceId==highSignal.uuid) dlq)


  -- The low instance can now advance. Replay still uses CT05 capture inputs.
  Engine.runTick (engineCfg firstNode) (Ops.productionStoreForInstance runner low) registry
  lowRow <- checkedInstance full low
  assert (lowRow.revision==1) "expected one persisted approval step"
  history <- required "history" =<< loadHistoryPage runner low 1 10
  _ <- required "historical replay" (verifyTimeline registry history)

  projected <- Ops.projectInstance runner "ct06-test" low 10 >>= required "projection"
  assert (projected==1) "projection failed to ingest revision one"
  before <- Ops.loadProjection runner "ct06-test" low >>= required "read active projection"
  assert (before/=Nothing) "materialized view missing"
  _ <- required "projection reset" =<< Ops.resetProjection runner "ct06-test" low
  againProjected <- Ops.projectInstance runner "ct06-test" low 10
    >>= required "projection rebuild"
  assert (againProjected==1) "projection not rebuildable"
  after <- Ops.loadProjection runner "ct06-test" low >>= required "reloaded active projection"
  assert (before==after) "projection payload changed after a rebuild"

  -- Activity finish may commit even when caller loses acknowledgement.
  let actStore = Ops.productionActivityStore runner
      actCfg = AT.WorkerCfg
        {node=firstNode,batchSize=1,pollDelayMs=1,
         leaseSeconds=10,instanceFilter=Just low}
      provider = Provider
        {kind="cmd",supportsIdempotency=True,deliver = \_ -> pure (Right (object ["ok" .= True]))}
  effectHook <- newFaultHook [(AfterActivityFinish,1)]
  seenFailure <- try $ Activity.runTick actCfg (faultedActivityStore effectHook actStore) [provider]
    :: IO (Either SomeException (Either AT.ActivityErr [DeliveryReport]))
  case seenFailure of
    Left _ -> pure ()
    Right otherResult -> fail ("did not inject post-effect-finish crash: "<>show otherResult)
  idle <- required "activity idempotency" =<< Activity.runTick actCfg actStore [provider]
  assert (null idle) "post-acknowledgment crash caused duplicate delivery"

  -- Redrive issues a NEW input, never edits the failed original.
  newId <- U4.nextRandom
  redriveOp <- U4.nextRandom
  redriven <- required "dead letter redrive" =<< Ops.redriveSignal runner
    sourceDl.id newId redriveOp "ct06-regression" "operator approved" Nothing
  assert (redriven==newId) "redrive failed to create a new signal"
  repeatRedrive <- required "idempotent redrive" =<< Ops.redriveSignal runner
    sourceDl.id newId redriveOp "ct06-regression" "operator approved" Nothing
  assert (repeatRedrive==newId) "redrive operation was not idempotent"
  highNow <- checkedInstance full high
  cancelHigh <- U4.nextRandom
  _ <- required "cancel redriven instance" =<< Ops.cancelInstance runner CancelRequest
    {instanceId=high,expectedRevision=highNow.revision,operationId=cancelHigh,
     actor="ct06-regression",reason="regression cleanup"}

  -- A commit succeeds, then the worker 'crashes' before it sees the ACK.
  crashInstance <- makeInstance
  _ <- putSignal crashInstance "ct06/crash" 0
  injected <- newFaultHook [(AfterCommit,1)]
  outcome <- try $ Engine.runTick (engineCfg firstNode)
    (faultedStore injected (Ops.productionStoreForInstance runner crashInstance)) registry
    :: IO (Either SomeException ())
  case outcome of
    Left _ -> pure ()
    Right _ -> fail "expected post-commit simulated crash"
  row <- checkedInstance full crashInstance
  assert (row.revision==1) "committed turn was lost after simulated crash"
  Engine.runTick (engineCfg secondNode)
    (Ops.productionStoreForInstance runner crashInstance) registry
  rowAgain <- checkedInstance full crashInstance
  assert (rowAgain.revision==1) "committed turn ran twice after crash recovery"

  -- Duplicate timer/activity completion MUST reuse one instance-local ordinal.
  timerA <- enqueueFor compiled low (ApproveSg "carol")
    (Just "hfsm/activity/test-timer-delivery")
  timerB <- enqueueFor compiled low (ApproveSg "carol")
    (Just "hfsm/activity/test-timer-delivery")
  idA <- required "timer first enqueue" =<< Ops.enqueueWithPolicy runner timerA
    (opts 0 timerA.signalId)
  idB <- required "timer duplicate enqueue" =<< Ops.enqueueWithPolicy runner timerB
    (opts 0 timerB.signalId)
  assert (idA==idB) "duplicate timer firing created a second signal"

  -- Cancellation is fenced by expected revision, and projection detects it.
  cancelOp <- U4.nextRandom
  verdict <- required "cancel" =<< Ops.cancelInstance runner CancelRequest
    {instanceId=low,expectedRevision=lowRow.revision,
     operationId=cancelOp,actor="ct06-regression",reason="operator test"}
  assert (verdict==Cancelled) "operator cancellation failed"
  _ <- required "project cancellation" =<< Ops.projectInstance runner "ct06-test" low 10
  final <- Ops.loadProjection runner "ct06-test" low >>= required "cancelled projection"
  assert (final/=before) "cancelled operational state not reflected in projection"
  putStrLn "OperationsRegression (PostgreSQL): PASS"
