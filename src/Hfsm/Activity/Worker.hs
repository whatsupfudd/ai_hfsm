{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE ScopedTypeVariables #-}

-- | CT-04 outbox dispatcher. User code is *never* run in a SQL transaction.
-- Success is at-least-once unless the provider deduplicates effectKey.
module Hfsm.Activity.Worker
  ( runTick, runWorker, dispatchOne ) where

import Control.Concurrent (forkFinally, forkIO, killThread, newEmptyMVar, putMVar, takeMVar, threadDelay)
import Control.Exception
  ( SomeException, SomeAsyncException, bracket, fromException, throwIO, try )
import Control.Monad (forM, forever, unless)
import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (find)
import Data.Maybe (isJust)
import Data.Time.Clock (UTCTime)
import qualified Data.Text as T
import System.Timeout (timeout)

import Hfsm.Activity.Policy (chooseFailure)
import Hfsm.Activity.Types
  ( ActivityErr(..), ActivityLease(..), ActivityStore(..), ClaimRq(..)
  , RenewRq(..), FinishRq(..), FinishDecision(..), Provider(..)
  , ProviderFailure(..), WorkerCfg(..), DeliveryReport(..) )

-- | Executes every claimed row concurrently, so one slow provider cannot
-- cause later items in a batch to lose their claim before being started.
-- One tick is bounded by each activity timeout, not by a global timeout.
runTick :: WorkerCfg -> ActivityStore IO -> [Provider]
        -> IO (Either ActivityErr [DeliveryReport])
runTick cfg store providers = do
  let safe = [p.kind | p <- providers, p.supportsIdempotency]
      rq = ClaimRq
        { node=cfg.node, batchSize=cfg.batchSize
        , leaseSeconds=cfg.leaseSeconds, instanceFilter=cfg.instanceFilter
        , safeKinds=safe, supportedKinds=[p.kind | p <- providers] }
  claimed <- store.claim rq
  case claimed of
    Left err -> pure (Left err)
    Right [] -> pure (Right [])
    Right leases -> do
      slots <- forM leases $ \lease -> do
        slot <- newEmptyMVar
        _ <- forkFinally (dispatchOne cfg store providers lease) (putMVar slot)
        pure slot
      results <- mapM takeMVar slots
      let go [] = pure (Right [])
          go (Left exc : _) = throwIO exc
          go (Right (Left err) : _) = pure (Left err)
          go (Right (Right item) : rest) = do
            next <- go rest
            pure (fmap (item :) next)
      go results

runWorker :: WorkerCfg -> ActivityStore IO -> [Provider] -> IO ()
runWorker cfg store providers = forever $ do
  result <- runTick cfg store providers
  case result of
    Left err -> fail ("CT-04 dispatcher DB failure: " <> show err)
    Right [] -> sleepMs cfg.pollDelayMs
    Right _ -> pure ()

-- | The provider receives the SAME immutable effectKey on every attempt.
-- A lease expiry means the delivery outcome may be unknown, so a stale
-- worker is forbidden from overwriting the recovered worker's database row.
dispatchOne :: WorkerCfg -> ActivityStore IO -> [Provider]
            -> ActivityLease -> IO (Either ActivityErr DeliveryReport)
dispatchOne cfg store providers effect = do
  let detectedKind = case effect.payload of
        Object fields -> case KM.lookup (K.fromText "kind") fields of
          Just (String txt) -> Just txt
          _ -> Nothing
        _ -> Nothing
      provider = detectedKind >>= \k -> find (\p -> p.kind == k) providers
  decision <- case provider of
    Nothing -> pure (PermanentFailure "permanent" "No provider registered for effect kind")
    Just p
      | effect.attempt > 1 && effect.replaySafe && not p.supportsIdempotency ->
          pure (PermanentFailure "unknown" "Provider lost its idempotency guarantee between deployments")
      | otherwise -> withHeartbeat cfg store effect $ do
          result <- try (timeout (microseconds effect.timeoutSeconds)
                        (p.deliver effect))
                        :: IO (Either SomeException (Maybe (Either ProviderFailure Value)))
          case result of
            Right (Just (Right receipt)) -> pure (Delivered receipt)
            Right (Just (Left err)) -> pure (chooseFailure effect p.supportsIdempotency err)
            Right Nothing -> pure (chooseFailure effect p.supportsIdempotency
              ProviderFailure { message="provider deadline exceeded", retryable=True, ambiguous=True })
            Left exception
              | isJust (fromException exception :: Maybe SomeAsyncException) ->
                  throwIO exception
              | otherwise -> pure (chooseFailure effect p.supportsIdempotency
                  ProviderFailure
                    { message=T.pack (show exception), retryable=True, ambiguous=True })
  finished <- store.finish FinishRq
    { effectId=effect.effectId, leaseId=effect.leaseId, node=cfg.node
    , attempt=effect.attempt, decision=decision }
  pure (fmap (const DeliveryReport {effectKey=effect.effectKey,decision=decision}) finished)

withHeartbeat :: WorkerCfg -> ActivityStore IO -> ActivityLease -> IO a -> IO a
withHeartbeat cfg store lease action =
  bracket (forkIO heartbeat) killThread (const action)
 where
  ttl = max 1 (min 3600 cfg.leaseSeconds)
  every = max 200000 (microseconds ttl `div` 3)
  heartbeat = do
    threadDelay every
    rez <- try (store.renew RenewRq
      { effectId=lease.effectId, leaseId=lease.leaseId,
        node=cfg.node, leaseSeconds=ttl })
        :: IO (Either SomeException (Either ActivityErr UTCTime))
    case rez of
      Right (Right _) -> heartbeat
      _ -> pure () -- No longer authorized. The final fenced ACK will reject.

microseconds :: Int -> Int
microseconds seconds = fromInteger $ max 1000 $ min (toInteger (maxBound :: Int))
  (toInteger (max 1 seconds) * 1000000)

sleepMs :: Int -> IO ()
sleepMs delay = unless (delay <= 0) $ threadDelay
  (fromIntegral (min 2147483 delay) * 1000)
