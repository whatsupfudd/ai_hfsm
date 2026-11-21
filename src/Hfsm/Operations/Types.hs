{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
module Hfsm.Operations.Types
  ( OpErr(..), WorkerRegistration(..), EnqueueOptions(..)
  , CancelRequest(..), CancelResult(..), DeadLetter(..)
  ) where

import Data.Aeson (Value, FromJSON, ToJSON)
import Data.Int (Int16, Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

-- | Provider authentication and operator authorization remain API/DB role
-- responsibilities: actor identifiers supplied here are NOT credentials.
data OpErr = OpBackend Text | OpDecode Text | OpRejected Text
  deriving stock (Eq, Show, Generic)

data WorkerRegistration = WorkerRegistration
  { workerId :: UUID
  , capabilities :: [Text]
  , heartbeatTtlSeconds :: Int
  , maxSignals :: Int
  , maxActivities :: Int
  }
  deriving stock (Eq, Show, Generic)

data EnqueueOptions = EnqueueOptions
  { priority :: Int16
  , correlationId :: Maybe UUID
  , causationId :: Maybe UUID
  , traceparent :: Maybe Text
  , expectedRelease :: Maybe Text
  }
  deriving stock (Eq, Show, Generic)

data CancelRequest = CancelRequest
  { instanceId :: UUID
  , expectedRevision :: Int64
  , operationId :: UUID
  , actor :: Text
  , reason :: Text
  }
  deriving stock (Eq, Show, Generic)

data CancelResult = Cancelled | AlreadyCancelled | CancelConflict
                  | CancelTerminal | CancelMissing
  deriving stock (Eq, Show, Generic)

data DeadLetter = DeadLetter
  { id :: Int64
  , tenantId :: UUID
  , instanceId :: UUID
  , sourceKind :: Text
  , sourceId :: UUID
  , reason :: Text
  , classification :: Text
  , payload :: Value
  , recordedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON,ToJSON)
