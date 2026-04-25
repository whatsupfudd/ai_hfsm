{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module Hfsm.Runtime.Model.Snapshot
  ( SnapshotRw(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Int (Int64)
import Data.Time (UTCTime)
import Data.UUID (UUID)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON, Value)

import Hfsm.Core.Digest (SpecDigest)
import Hfsm.Core.Path (StatePath)
import Hfsm.Core.Ref (StateRef)
import Hfsm.Core.Version (MachineVersion)


data SnapshotRw = SnapshotRw
  { uid :: Int64
  , uuid :: UUID
  , inst :: UUID
  , state :: StateRef
  , path :: StatePath
  , ctx :: Value
  , wait :: Maybe Value
  , child :: Value
  , version :: MachineVersion
  , digest :: SpecDigest
  , createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)