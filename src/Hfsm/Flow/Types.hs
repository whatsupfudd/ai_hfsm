{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
module Hfsm.Flow.Types
  ( FlowErr(..), ArtifactRef(..), PublishArtifact(..)
  , StartRun(..), FlowRun(..), FlowTaskLease(..), FlowInput(..)
  , WorkerResources(..), TaskOutputs(..), TaskFailure(..), FlowStore(..)
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)

data FlowErr
  = FlowBackend Text | FlowDecode Text | FlowInvalid Text | FlowLeaseLost Text
  deriving stock (Eq, Show)

-- | Object bytes live in an external durable CAS. A SQL record is a validated
-- manifest, not proof that the object exists or that a provider verified it.
data ArtifactRef = ArtifactRef
  { artifactId :: UUID
  , digest :: Text
  , sizeBytes :: Int64
  , storageUri :: Text
  , mediaType :: Text
  , metadata :: Value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data PublishArtifact = PublishArtifact
  { artifactId :: UUID
  , tenantId :: UUID
  , digest :: Text
  , sizeBytes :: Int64
  , storageUri :: Text
  , mediaType :: Text
  , metadata :: Value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | For an HFSM-originated flow sourceInstance, exact typed payloads and
-- release must be supplied by the application codec/registry. No implicit
-- JSON-to-application signal conversion is performed by the flow subsystem.
data StartRun = StartRun
  { runId :: UUID
  , definitionId :: UUID
  , sourceEffectKey :: Text
  , sourceInstance :: Maybe UUID
  , sourceRelease :: Maybe Text
  , runInput :: Value
  , successSignal :: Maybe Value
  , failureSignal :: Maybe Value
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FlowRun = FlowRun
  { runId :: UUID
  , tenantId :: UUID
  , definitionId :: UUID
  , status :: Text
  , sourceInstance :: Maybe UUID
  , startedAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FlowInput = FlowInput
  { port :: Text
  , artifact :: ArtifactRef
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data FlowTaskLease = FlowTaskLease
  { runId :: UUID
  , tenantId :: UUID
  , taskKey :: Text
  , executor :: Text
  , attempt :: Int
  , leaseId :: UUID
  , workerId :: UUID
  , leaseUntil :: UTCTime
  , timeoutSeconds :: Int
  , effectKey :: Text
  , correlationId :: Maybe UUID
  , traceparent :: Maybe Text
  , runInput :: Value
  , inputs :: [FlowInput]
  , outputs :: [Text]
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data WorkerResources = WorkerResources
  { workerId :: UUID
  , cpuUnits :: Int
  , memoryMb :: Int
  , maxTasks :: Int
  }
  deriving stock (Eq, Show, Generic)

data TaskOutputs = TaskOutputs
  { runId :: UUID
  , taskKey :: Text
  , leaseId :: UUID
  , workerId :: UUID
  , attempt :: Int
  , outputs :: [(Text, UUID)]
  }
  deriving stock (Eq, Show, Generic)

data TaskFailure = TaskFailure
  { runId :: UUID
  , taskKey :: Text
  , leaseId :: UUID
  , workerId :: UUID
  , attempt :: Int
  , reason :: Text
  , retryable :: Bool
  }
  deriving stock (Eq, Show, Generic)

-- | All functions return explicit errors; a claim never masks backend errors.
-- A full batch is an atomic admission decision. Output references are checked
-- again by SQL at completion, under the lease fence.
data FlowStore m = FlowStore
  { claim :: UUID -> Int -> Int -> m (Either FlowErr [FlowTaskLease])
  , renew :: FlowTaskLease -> Int -> m (Either FlowErr UTCTime)
  , finish :: TaskOutputs -> m (Either FlowErr Bool)
  , failTask :: TaskFailure -> m (Either FlowErr Bool)
  }
