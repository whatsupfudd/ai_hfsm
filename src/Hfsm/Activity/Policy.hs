-- | Pure, bounded retry policy. No randomness or process-local state is used.
-- The attempt counter is the number of times the provider was *claimed*;
-- maxAttempts includes the initial attempt.
module Hfsm.Activity.Policy
  ( retryDelaySeconds, chooseFailure ) where

import Data.Int (Int64)
import Data.Text (Text)
import Hfsm.Activity.Types
  ( ActivityLease(..), ProviderFailure(..), FinishDecision(..) )

-- | Exponential: base, 2*base, 4*base, ... capped before Int overflow.
-- First failure is attempt 1 and uses one base interval.
retryDelaySeconds :: Int -> Int -> Int -> Int
retryDelaySeconds base cap attemptNo =
  fromIntegral (go (max 0 (fromIntegral base :: Int64)) (max 0 (attemptNo - 1)))
 where
  ceilingDelay = max 0 (fromIntegral cap :: Int64)
  go value 0 = min ceilingDelay value
  go value n
    | value >= ceilingDelay = ceilingDelay
    | value > ceilingDelay `div` 2 = ceilingDelay
    | otherwise = go (value * 2) (n - 1)

-- | A retry is safe only when the provider has asserted stable-key dedupe,
-- and the *persisted* claim confirms this. An ambiguous failure of a
-- non-idempotent provider becomes a terminal outcome-unknown record.
chooseFailure :: ActivityLease -> Bool -> ProviderFailure -> FinishDecision
chooseFailure lease providerSafe err
  | err.retryable && providerSafe && lease.replaySafe && lease.attempt < lease.maxAttempts =
      RetryAfter err.message (retryDelaySeconds lease.retryBaseSeconds lease.retryCapSeconds lease.attempt)
  | err.ambiguous = PermanentFailure "unknown" err.message
  | err.retryable && lease.attempt >= lease.maxAttempts = PermanentFailure "exhausted" err.message
  | otherwise = PermanentFailure "permanent" err.message
