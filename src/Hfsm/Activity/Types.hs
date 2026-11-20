{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

-- | CT-04: durable effects are separate from HFSM state transitions.
-- The Store contract represents one fenced delivery attempt; an effect's
-- 'effectKey' NEVER changes when a delivery is retried or recovered.
module Hfsm.Activity.Types
  ( ActivityLease(..), ActivityErr(..)
  , ClaimRq(..), RenewRq(..), FinishRq(..), FinishDecision(..)
  , ActivityStore(..), Provider(..), ProviderFailure(..)
  , WorkerCfg(..), DeliveryReport(..)
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

-- | The database, not the worker clock, allocates attempt numbers and expiry.
data ActivityLease = ActivityLease
  { effectId         :: UUID
  , instanceId       :: UUID
  , stepId           :: UUID
  , effectKey        :: Text
  , payload          :: Value
  , attempt          :: Int
  , maxAttempts      :: Int
  , timeoutSeconds   :: Int
  , retryBaseSeconds :: Int
  , retryCapSeconds  :: Int
  , replaySafe       :: Bool
  , leaseId          :: UUID
  , leaseUntil       :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data ActivityErr
  = ActivityNotFound Text
  | ActivityLeaseLost Text
  | ActivityConflict Text
  | ActivityDecode Text
  | ActivityBackend Text
  deriving stock (Eq, Show)

data ClaimRq = ClaimRq
  { node           :: UUID
  , batchSize      :: Int
  , leaseSeconds   :: Int
  , instanceFilter :: Maybe UUID
  -- | Only providers that guarantee key-based deduplication belong here.
  , safeKinds      :: [Text]
  , supportedKinds :: [Text]
  }
  deriving stock (Eq, Show)

data RenewRq = RenewRq
  { effectId      :: UUID
  , leaseId       :: UUID
  , node          :: UUID
  , leaseSeconds  :: Int
  }
  deriving stock (Eq, Show)

data FinishDecision
  = Delivered Value
  | RetryAfter Text Int
  | PermanentFailure Text Text  -- ^ failure_class, explanation
  deriving stock (Eq, Show)

data FinishRq = FinishRq
  { effectId     :: UUID
  , leaseId      :: UUID
  , node         :: UUID
  , attempt      :: Int
  , decision     :: FinishDecision
  }
  deriving stock (Eq, Show)

-- | One operation = one database transaction, including attempt history.
data ActivityStore m = ActivityStore
  { claim  :: ClaimRq -> m (Either ActivityErr [ActivityLease])
  , renew  :: RenewRq -> m (Either ActivityErr UTCTime)
  , finish :: FinishRq -> m (Either ActivityErr ())
  }

-- | A provider receives the full leased effect (including the stable key,
-- instance identifier and payload). If replay is unsafe, uncertain delivery
-- is never retried automatically.
data Provider = Provider
  { kind                :: Text
  , supportsIdempotency :: Bool
  , deliver             :: ActivityLease -> IO (Either ProviderFailure Value)
  }

data ProviderFailure = ProviderFailure
  { message   :: Text
  , retryable :: Bool
  , ambiguous :: Bool
  }
  deriving stock (Eq, Show)

data WorkerCfg = WorkerCfg
  { node           :: UUID
  , batchSize      :: Int
  , leaseSeconds   :: Int
  , pollDelayMs    :: Int
  , instanceFilter :: Maybe UUID
  }
  deriving stock (Eq, Show)

data DeliveryReport = DeliveryReport
  { effectKey :: Text
  , decision  :: FinishDecision
  }
  deriving stock (Eq, Show)
