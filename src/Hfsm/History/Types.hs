{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}

-- | Persisted planner inputs and immutable audit surfaces for CT-05.
module Hfsm.History.Types
  ( ReplayInput(..), asStepInput, HistoryTurn(..), MigrationEvidence(..)
  , HistoryErr(..), ReplayErr(..), ReplayResult(..)
  ) where

import Data.Aeson (FromJSON, ToJSON, Value)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Hfsm.Runtime.Engine.Step (StepInput(..))
import Hfsm.Runtime.Model (InstanceRw, SignalRw, SnapshotRw, StepRw)
import Hfsm.Runtime.Registry (MachineKey)
import Hfsm.Store.Class (ProjBatch)

-- | Exact pre-ACK values captured in the same transaction as the step INSERT.
-- NOT reconstructed from the mutable signal row after commit.
data ReplayInput = ReplayInput
  { machine :: MachineKey
  , instance_ :: InstanceRw
  , snapshot :: SnapshotRw
  , signal :: SignalRw
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

asStepInput :: ReplayInput -> StepInput
asStepInput x = StepInput
  { machine = x.machine, instance_ = x.instance_
  , snapshot = x.snapshot, signal = x.signal }

data MigrationEvidence = MigrationEvidence
  { operationId :: UUID
  , sourceKey :: MachineKey
  , targetKey :: MachineKey
  , mappingName :: Text
  , sourceSnapshotId :: UUID
  , targetSnapshotId :: UUID
  , oldCtx :: Value
  , newCtx :: Value
  , createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

-- | Outbox values are immutable payloads with deterministic effect keys;
-- mutable delivery status is deliberately NOT replay evidence.
-- Child status and breakpoint status may evolve after commit, so only their
-- immutable identity/configuration fields are recorded here.
data HistoryTurn = HistoryTurn
  { revision :: Int64
  , sourceSnapshotId :: UUID
  , step :: StepRw
  , snapshot :: SnapshotRw
  , input :: Maybe ReplayInput
  , outbox :: [Value]
  , children :: [Value]
  , breaks :: [Value]
  , proj :: ProjBatch
  , migration :: Maybe MigrationEvidence
  }
  deriving stock (Eq, Show, Generic)
  deriving anyclass (FromJSON, ToJSON)

data HistoryErr
  = HistoryBackend Text
  | HistoryDecode Text
  deriving stock (Eq, Show)

data ReplayErr
  = ReplayMissingInput Int64
  | ReplayUnknownRelease Int64 MachineKey
  | ReplayPlannerFailure Int64 Text
  | ReplayDivergence Int64 Text Value Value
  | ReplayInvalidHistory Int64 Text
  deriving stock (Eq, Show)

data ReplayResult
  = ReplayVerified Int64
  | ReplayMigrationVerified Int64
  deriving stock (Eq, Show)
