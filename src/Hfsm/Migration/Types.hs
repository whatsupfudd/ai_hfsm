{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | An explicit, version-to-version graph and context transformation.
module Hfsm.Migration.Types
  ( MigrationSpec(..), MigrationRequest(..), MigrationReceipt(..)
  , MigrationErr(..)
  ) where

import Data.Aeson (Value)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Hfsm.Core.Path (StatePath)
import Hfsm.Core.Ref (StateRef)
import Hfsm.Runtime.Model (InstanceStatus)
import Hfsm.Runtime.Registry (MachineKey)

-- | Source and target contexts can be DIFFERENT Haskell types.
-- Transformations must be deterministic and side-effect-free.
-- mapWait and mapChild may explicitly clear or adapt old graph references.
data MigrationSpec ctxOld ctxNew = MigrationSpec
  { mappingName     :: Text
  , mapControl      :: StateRef -> Either Text StateRef
  , mapContext      :: ctxOld -> Either Text ctxNew
  , mapWait         :: Maybe Value -> Either Text (Maybe Value)
  , mapChild        :: Value -> Either Text Value
  , mapStatus       :: InstanceStatus -> Either Text InstanceStatus
  }

-- | Fully planned migration; the transaction applies this request under CAS.
-- Caller creates stable UUIDs and timestamp BEFORE submission/retry.
data MigrationRequest = MigrationRequest
  { operationId     :: UUID
  , instanceId      :: UUID
  , stepId          :: UUID
  , snapshotId      :: UUID
  , expectedSnapshot :: UUID
  , expectedRevision :: Int64
  , sourceKey       :: MachineKey
  , targetKey       :: MachineKey
  , mappingName     :: Text
  , fromState       :: StateRef
  , toState         :: StateRef
  , toPath          :: StatePath
  , oldCtx          :: Value
  , newCtx          :: Value
  , newWait         :: Maybe Value
  , newChild        :: Value
  , nextStatus      :: InstanceStatus
  , createdAt       :: UTCTime
  }
  deriving stock (Eq, Show, Generic)

data MigrationReceipt = MigrationReceipt
  { operationId :: UUID
  , instanceId :: UUID
  , revision :: Int64
  , stepId :: UUID
  , snapshotId :: UUID
  }
  deriving stock (Eq, Show, Generic)

data MigrationErr
  = MigrationPlanError Text
  | MigrationConflict Text
  | MigrationStorageError Text
  deriving stock (Eq, Show)
