{-# LANGUAGE QuasiQuotes #-}

-- | CT-04: pure retry-policy regression and isolated PostgreSQL delivery tests.
module Demo.ActivityRegression (regression, regressionPg) where

import Control.Concurrent (threadDelay, forkFinally, newEmptyMVar, takeMVar, putMVar)
import Control.Exception (SomeException)
import Control.Monad (unless)
import Control.Monad.Trans.Cont (runContT)
import Data.Aeson (Value(..), object, (.=))
import Data.IORef (newIORef, readIORef, modifyIORef')
import Data.Int (Int32)
import Data.Text (Text)
import Data.UUID (UUID)
import qualified Data.UUID as U
import Data.Time.Clock (getCurrentTime)
import qualified Data.UUID.V4 as U4

import qualified Hasql.Pool as Pool
import qualified Hasql.Session as Session
import Hasql.Statement (Statement)
import Hasql.TH (singletonStatement, resultlessStatement)

import qualified DB.Connect as Db
import Demo.ApprovalHfsm (approvalSpec, Signal(..))
import Demo.ApprovalRuntime (initialCtx)
import Hfsm.Activity.Policy (retryDelaySeconds, chooseFailure)
import Hfsm.Activity.Types
  ( ActivityLease(..), ActivityStore(..), ActivityErr(..), ClaimRq(..)
  , FinishRq(..), FinishDecision(..), Provider(..), ProviderFailure(..)
  , WorkerCfg(..), DeliveryReport(..) )
import qualified Hfsm.Activity.Worker as Activity
import Hfsm.Compile (compile)
import qualified Hfsm.Runtime.Engine.Worker as Engine
import Hfsm.Runtime.Registry (emptyRegistry, machineKeyOf, register)
import qualified Hfsm.Store.Postgres as Pg
import qualified Hfsm.Store.Postgres.Activity as PgActivity
import Hfsm.Store.Postgres.Bootstrap (enqueueFor, newInstanceFor)
import Hfsm.Store.Class (Store(..))
import Hfsm.Runtime.Model.Instance ( InstanceRw(..) )

assert :: Bool -> String -> IO ()
assert yes message = unless yes (fail message)

required :: Show err => String -> Either err a -> IO a
required tag = either (fail . ((tag <> ": ") <>) . show) pure

one :: [a] -> IO a
one [x] = pure x
one _ = fail "expected exactly one claimed outbox effect"

regression :: IO ()
regression = do
  assert (retryDelaySeconds 2 30 1 == 2) "first retry delay"
  assert (retryDelaySeconds 2 30 2 == 4) "second retry delay"
  assert (retryDelaySeconds 2 30 8 == 30) "capped retry delay"
  assert (retryDelaySeconds 0 30 3 == 0) "zero-delay policy"
  now <- getCurrentTime
  let sample = ActivityLease
        { effectId=U.nil, instanceId=U.nil, stepId=U.nil
        , effectKey="test:0", payload=Null, attempt=1, maxAttempts=3
        , timeoutSeconds=1, retryBaseSeconds=2, retryCapSeconds=8
        , replaySafe=False, leaseId=U.nil, leaseUntil=now }
      unknown = ProviderFailure
        { message="connection lost", retryable=True, ambiguous=True }
  case chooseFailure sample False unknown of
    PermanentFailure "unknown" _ -> pure ()
    other -> fail ("unsafe ambiguous failure was retried: " <> show other)
  case chooseFailure (sample { replaySafe=True }) True unknown of
    RetryAfter _ 2 -> pure ()
    other -> fail ("safe transient failure was not retried: " <> show other)
  putStrLn "ActivityRegression (pure): PASS"

-- | This test intentionally exercises pooled PostgreSQL connections, and
-- should run in a dedicated TEST DB with Support/002_hfsm_activity.sql loaded.
regressionPg :: Db.PgDbConfig -> IO ()
regressionPg cfg = runContT (Db.startPg cfg) $ \pool -> do
  spec <- required "approval spec" approvalSpec
  compiled <- required "compile" (compile spec)
  registry <- required "register" (register compiled emptyRegistry)
  let runner = Pg.runWithPool pool
      hfsm = Pg.postgresStore runner
      activities = PgActivity.activityStore runner
  required "machine release" =<< Pg.registerMachine runner (machineKeyOf compiled)
  engineId <- U4.nextRandom
  let
    machineWorker = Engine.WorkerCfg { batchSize=1, pollDelayMs=10, leaseSeconds=30, node=engineId }
    makeEffect = do
      new <- required "new instance" =<< newInstanceFor compiled initialCtx Nothing
      putStrLn ("New instance: " <> show new)
      instanceId <- required "initialize" =<< Pg.initializeInstance runner new
      rq <- enqueueFor compiled instanceId (SubmitSg "alice") Nothing
      _ <- required "enqueue" =<< Pg.enqueue runner rq
      Engine.runTick machineWorker (Pg.postgresStoreForInstance runner instanceId) registry
      mInstance <- hfsm.loadInstance instanceId
      case mInstance of
        Just row
          | row.revision == 1 -> pure ()
        _ -> fail ("makeEffect did not execute its own instance: " <> show instanceId)
      pure instanceId
    activityCfg node instanceId secs = WorkerCfg {
        node=node, batchSize=1, pollDelayMs=10
      , leaseSeconds=secs, instanceFilter=Just instanceId
      }

  -- First call returns a transient error; the same effectKey must be
  -- delivered on the second attempt, with an audit of BOTH attempts.
  instanceId <- makeEffect
  _ <- required "configure retry policy" =<< Pool.use pool
       (Session.statement instanceId testRetryPolicy)
  seen <- newIORef ([] :: [Text])
  let 
    provider = Provider {
          kind = "cmd", supportsIdempotency = True
        , deliver= \effect -> do
            modifyIORef' seen (effect.effectKey :)
            keys <- readIORef seen
            if length keys == 1
              then pure (Left ProviderFailure
                { message="transient provider error", retryable=True, ambiguous=True })
              else pure (Right (object ["provider" .= ("ok" :: Text)]))
        }
  deliveryNode <- U4.nextRandom
  let wc = activityCfg deliveryNode instanceId 3
  first <- required "first delivery" =<< Activity.runTick wc activities [provider]
  putStrLn ("First delivery: " <> show first)
  case first of
    [DeliveryReport { decision = RetryAfter _ _ }] -> pure ()
    other -> putStrLn ("expected scheduled retry: " <> show other)
  early <- required "delayed retry" =<< Activity.runTick wc activities [provider]
  assert (null early) "retry was claimed before available_at"
  threadDelay 1300000
  second <- required "second delivery" =<< Activity.runTick wc activities [provider]
  case second of
    [DeliveryReport { decision=Delivered _ }] -> pure ()
    other -> fail ("expected successful delivery: " <> show other)
  observed <- readIORef seen
  assert (length observed == 2 && head observed == observed !! 1)
    "provider did not receive stable idempotency key on retry"
  (status, attempts, histories, successful) <- required "persistence result" =<<
    Pool.use pool (Session.statement instanceId readSummary)
  assert (status == "done" && attempts == 2 && histories == 2 && successful == 1)
    ("unexpected persisted result: " <> show (status,attempts,histories,successful))

  -- Two live workers race on separate pooled connections. Exactly one
  -- may claim this newly committed effect.
  raceInstance <- makeEffect
  raceA <- U4.nextRandom
  raceB <- U4.nextRandom
  let raceClaim owner = ClaimRq
        { node=owner, batchSize=1, leaseSeconds=3
        , instanceFilter=Just raceInstance
        , safeKinds=["cmd"], supportedKinds=["cmd"] }
  slotA <- newEmptyMVar
  slotB <- newEmptyMVar
  _ <- forkFinally (activities.claim (raceClaim raceA)) (putMVar slotA)
  _ <- forkFinally (activities.claim (raceClaim raceB)) (putMVar slotB)
  resultA <- takeMVar slotA
  resultB <- takeMVar slotB
  let flatten :: Either SomeException (Either ActivityErr [ActivityLease])
              -> IO [ActivityLease]
      flatten result = case result of
        Left exception -> fail (show exception)
        Right (Left err) -> fail (show err)
        Right (Right leases) -> pure leases
  aClaims <- flatten resultA
  bClaims <- flatten resultB
  winner <- one (aClaims <> bClaims)
  let winnerId = if null aClaims then raceB else raceA
  required "race winner completion" =<< activities.finish FinishRq
    { effectId=winner.effectId, leaseId=winner.leaseId, node=winnerId
    , attempt=winner.attempt, decision=Delivered (object []) }

  -- Simulate a crashed worker: it has a token but never acknowledges.
  crashedInstance <- makeEffect
  oldOwner <- U4.nextRandom
  newOwner <- U4.nextRandom
  let claim owner safe = ClaimRq
        { node=owner, batchSize=1, leaseSeconds=1
        , instanceFilter=Just crashedInstance, safeKinds=safe, supportedKinds=["cmd"] }
  olds <- required "initial crash claim" =<< activities.claim (claim oldOwner ["cmd"])
  old <- one olds
  threadDelay 1300000
  recoveries <- required "reclaimed activity" =<<
    activities.claim (claim newOwner ["cmd"])
  recovered <- one recoveries
  assert (old.effectId == recovered.effectId && old.leaseId /= recovered.leaseId)
    "expired lease was not safely reclaimed with a fresh token"
  stale <- activities.finish FinishRq
    { effectId=old.effectId, leaseId=old.leaseId, node=oldOwner
    , attempt=old.attempt, decision=Delivered (object []) }
  case stale of
    Left (ActivityLeaseLost _) -> pure ()
    other -> fail ("stale worker committed after reclaim: " <> show other)
  required "recovered completion" =<< activities.finish FinishRq
    { effectId=recovered.effectId, leaseId=recovered.leaseId, node=newOwner
    , attempt=recovered.attempt, decision=Delivered (object []) }

  -- With an unsafe provider, expiry is classified as outcome-unknown.
  unsafeInstance <- makeEffect
  unsafeClaims <- required "unsafe claim" =<< activities.claim (claimUnsafe oldOwner unsafeInstance)
  _ <- one unsafeClaims
  threadDelay 1300000
  nothing <- required "unsafe sweep" =<< activities.claim (claimUnsafe newOwner unsafeInstance)
  assert (null nothing) "unsafe activity was automatically retried"
  (unsafeStatus, unsafeClass) <- required "unsafe status" =<<
    Pool.use pool (Session.statement unsafeInstance readUnknown)
  assert (unsafeStatus == "failed" && unsafeClass == Just "unknown")
    "non-idempotent crash failed to preserve ambiguous outcome"
  -- A hanging provider is interrupted at its durable per-effect timeout;
  -- the activity stays retryable only because this provider is replay-safe.
  timeoutInstance <- makeEffect
  _ <- required "configure timeout" =<<
    Pool.use pool (Session.statement timeoutInstance testTimeoutPolicy)
  timeoutNode <- U4.nextRandom
  let
    slow = Provider { 
          kind="cmd", supportsIdempotency=True
        , deliver= \_ -> do
            threadDelay 1800000
            pure (Right Null)
        }
  timed <- required "provider timeout" =<<
    Activity.runTick (activityCfg timeoutNode timeoutInstance 3) activities [slow]
  case timed of
    [DeliveryReport { decision=RetryAfter _ _ }] -> pure ()
    other -> fail ("timed activity did not schedule safe retry: " <> show other)
  (timeoutStatus, _timeoutCount, _hist, _succ) <- required "timeout status" =<<
    Pool.use pool (Session.statement timeoutInstance readSummary)
  assert (timeoutStatus == "ready") "timeout was not durably retried"

  putStrLn "ActivityRegression (PostgreSQL): PASS"
 where
  claimUnsafe owner instanceId = ClaimRq
    { node=owner, batchSize=1, leaseSeconds=1
    , instanceFilter=Just instanceId, safeKinds=[], supportedKinds=["cmd"] }

testRetryPolicy :: Statement UUID ()
testRetryPolicy = [resultlessStatement|
  UPDATE hfsm.outbox SET max_attempts=3, timeout_seconds=2,
    retry_base_seconds=1, retry_cap_seconds=1
  WHERE instance_id=$1 :: uuid
|]

testTimeoutPolicy :: Statement UUID ()
testTimeoutPolicy = [resultlessStatement|
  UPDATE hfsm.outbox SET max_attempts=2, timeout_seconds=1,
    retry_base_seconds=1, retry_cap_seconds=1
  WHERE instance_id=$1 :: uuid
|]

readSummary :: Statement UUID (Text, Int32, Int32, Int32)
readSummary = [singletonStatement|
 SELECT o.status :: text, o.attempts :: int4,
   (SELECT count(*)::int4 FROM hfsm.activity_attempt a WHERE a.outbox_id=o.id) :: int4,
   (SELECT count(*)::int4 FROM hfsm.activity_attempt a
    WHERE a.outbox_id=o.id AND a.outcome='succeeded') :: int4
 FROM hfsm.outbox o WHERE o.instance_id=$1 :: uuid LIMIT 1
|]

readUnknown :: Statement UUID (Text, Maybe Text)
readUnknown = [singletonStatement|
  SELECT status :: text, failure_class :: text?
  FROM hfsm.outbox WHERE instance_id=$1 :: uuid LIMIT 1
|]
