{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE ScopedTypeVariables #-}
-- | A DF-01 executor is idempotent and has no irreversible external effects.
-- It must publish immutable artifacts to durable CAS before returning refs.
-- Observable business effects belong in the CT-04 outbox, not a flow task.
module Hfsm.Flow.Worker
  ( FlowWorkerCfg(..), TaskProvider(..), TaskError(..), RunReport(..)
  , runTick, runWorker, dispatchOne, runCompletionTick, runCompletionWorker
  ) where

import Control.Concurrent
  ( forkIO, forkFinally, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay )
import Control.Exception
  ( SomeException, SomeAsyncException, bracket, fromException, throwIO, try )
import Control.Monad (forM, forever, unless)
import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import qualified Data.Text as T
import Data.UUID (UUID)
import System.Timeout (timeout)

import Hfsm.Flow.Types
  ( FlowErr(..), FlowStore(..), FlowTaskLease(..), TaskOutputs(..), TaskFailure(..) )
import Hfsm.Store.Postgres.Types (Runner)
import qualified Hfsm.Flow.Postgres as Pg

data FlowWorkerCfg = FlowWorkerCfg
  { workerId :: UUID
  , batchSize :: Int
  , leaseSeconds :: Int
  , pollDelayMs :: Int
  } deriving (Eq, Show)

data TaskError = TaskError
  { reason :: Text
  , retryable :: Bool
  } deriving (Eq, Show)

-- | Each registered executor has a stable name. Retries will use the same
-- effectKey and should produce identical content-addressed artifacts.
data TaskProvider = TaskProvider
  { executor :: Text
  , execute :: FlowTaskLease -> IO (Either TaskError [(Text, UUID)])
  }

data RunReport = RunReport
  { runId :: UUID
  , taskKey :: Text
  , attempt :: Int
  , outcome :: Text
  } deriving (Eq, Show)

runTick :: FlowWorkerCfg -> FlowStore IO -> [TaskProvider]
        -> IO (Either FlowErr [RunReport])
runTick cfg store providers = do
  claimed <- store.claim cfg.workerId cfg.batchSize cfg.leaseSeconds
  case claimed of
    Left err -> pure (Left err)
    Right tasks -> do
      slots <- forM tasks $ \task -> do
        slot <- newEmptyMVar
        _ <- forkFinally (dispatchOne cfg store providers task) (putMVar slot)
        pure slot
      completed <- mapM takeMVar slots
      sequence <$> traverse unwrap completed
 where
  unwrap (Right result) = pure result
  unwrap (Left failure) = throwIO failure

runWorker :: FlowWorkerCfg -> FlowStore IO -> [TaskProvider] -> IO ()
runWorker cfg store providers = forever $ do
  result <- runTick cfg store providers
  case result of
    Left err -> fail ("DF-01 worker failed: " <> show err)
    Right [] -> sleepMs cfg.pollDelayMs
    Right _ -> pure ()

dispatchOne :: FlowWorkerCfg -> FlowStore IO -> [TaskProvider]
            -> FlowTaskLease -> IO (Either FlowErr RunReport)
dispatchOne cfg store providers lease = do
  let provider = find (\p -> p.executor == lease.executor) providers
  decision <- case provider of
    Nothing -> pure (Left (TaskError "missing executor provider" False))
    Just p -> withHeartbeat cfg store lease $ do
      result <- try (timeout (microseconds lease.timeoutSeconds) (p.execute lease))
        :: IO (Either SomeException (Maybe (Either TaskError [(Text,UUID)])))
      case result of
        Right (Just output) -> pure output
        Right Nothing -> pure (Left (TaskError "task deadline exceeded" True))
        Left exception
          | isJust (fromException exception :: Maybe SomeAsyncException) ->
              throwIO exception
          | otherwise -> pure (Left (TaskError (T.pack (show exception)) True))
  case decision of
    Right bindings -> do
      finished <- store.finish TaskOutputs
        { runId=lease.runId,taskKey=lease.taskKey,leaseId=lease.leaseId
        , workerId=lease.workerId,attempt=lease.attempt,outputs=bindings }
      pure (outcomeReport "done" finished)
    Left problem -> do
      failed <- store.failTask TaskFailure
        { runId=lease.runId,taskKey=lease.taskKey,leaseId=lease.leaseId
        , workerId=lease.workerId,attempt=lease.attempt
        , reason=problem.reason,retryable=problem.retryable }
      pure (outcomeReport "failed-or-retry" failed)
 where
  outcomeReport label result = case result of
    Left e -> Left e
    Right False -> Left (FlowLeaseLost "task fenced out or run terminated")
    Right True -> Right RunReport
      {runId=lease.runId,taskKey=lease.taskKey,attempt=lease.attempt,outcome=label}

withHeartbeat :: FlowWorkerCfg -> FlowStore IO -> FlowTaskLease -> IO a -> IO a
withHeartbeat cfg store lease action =
  bracket (forkIO heartbeat) killThread (const action)
 where
  ttl = max 1 (min 3600 cfg.leaseSeconds)
  every = max 200000 (microseconds ttl `div` 3)
  heartbeat = do
    threadDelay every
    answer <- try (store.renew lease ttl)
      :: IO (Either SomeException (Either FlowErr UTCTime))
    case answer of
      Right (Right _) -> heartbeat
      _ -> pure () -- A lost lease can never commit due to SQL fencing.

microseconds :: Int -> Int
microseconds secs = fromInteger $ min (toInteger (maxBound :: Int))
  (max 1 (toInteger secs) * 1000000)

sleepMs :: Int -> IO ()
sleepMs ms = unless (ms <= 0) $ threadDelay
  (min 2147483 ms * 1000)

-- | Delivery is independently durable and idempotent in SQL. Each success
-- signal carries a deterministic producer dedupe key for CT-06 enqueue.
runCompletionTick :: Runner -> Int -> IO (Either FlowErr Int)
runCompletionTick runner limit = do
  pending <- Pg.pendingCompletions runner limit
  case pending of
    Left err -> pure (Left err)
    Right ids -> do
      results <- traverse (Pg.deliverCompletion runner) ids
      pure (length <$> sequence results)

runCompletionWorker :: Runner -> Int -> Int -> IO ()
runCompletionWorker runner batch pollMs = forever $ do
  result <- runCompletionTick runner batch
  case result of
    Left err -> fail ("DF-01 completion delivery failed: " <> show err)
    Right 0 -> sleepMs (max 20 pollMs)
    Right _ -> pure ()
