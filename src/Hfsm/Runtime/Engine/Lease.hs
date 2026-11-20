-- | Optional heartbeat for IO-based long-running, pure computation or
-- orchestration before the fenced commit. It does NOT make external effects
-- exactly once; external side effects belong in a durable outbox worker.
module Hfsm.Runtime.Engine.Lease
  ( withLeaseHeartbeat
  ) where

import Control.Concurrent (forkIO, killThread, threadDelay)
import Control.Exception (SomeException, bracket, try)
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.UUID (UUID)

import Hfsm.Store.Class (Store(..), RenewRq(..), StoreErr)

-- | Keep the current fencing token alive while the supplied IO action runs.
-- This helper never transfers ownership: on renewal failure it stops and the
-- final commit still fails safely if the lease was lost. The owner must NEVER
-- dispatch irreversible external effects before a successful 'commitTurn'.
-- Run an action ONLY with the signal/lease returned by 'leaseSignal'.
withLeaseHeartbeat
  :: Store IO -> UUID -> UUID -> Int -> IO a -> IO a
withLeaseHeartbeat store signalUuid token leaseSeconds action =
  bracket (forkIO loop) killThread (const action)
 where
  ttl = max 1 (min 3600 leaseSeconds)
  delayMicros = max 200000 (ttl * 1000000 `div` 3)
  loop = do
    threadDelay delayMicros
    now <- getCurrentTime
    result <- try (store.renewLease RenewRq
      { signal = signalUuid
      , lease = token
      , extendSeconds = ttl
      , now = now
      }) :: IO (Either SomeException (Either StoreErr UTCTime))
    case result of
      Right (Right _) -> loop
      _ -> pure () -- A failed renewal cannot grant authority.
