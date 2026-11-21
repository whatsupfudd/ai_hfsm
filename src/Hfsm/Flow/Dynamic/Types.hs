{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hfsm.Flow.Dynamic.Types
  ( PartitionBinding(..), ScatterSummary(..) ) where

import Data.Aeson (FromJSON, ToJSON)
import Data.Text (Text)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Hfsm.Flow.Types (ArtifactRef)

-- | Persisted immutable input to a dynamically generated partition task.
data PartitionBinding = PartitionBinding
  { groupKey :: Text
  , partitionKey :: Text
  , ordinal :: Int
  , artifact :: ArtifactRef
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)

-- | The final reduction's result is an ordinary DF-01 CAS artifact binding.
data ScatterSummary = ScatterSummary
  { groupId :: UUID
  , groupKey :: Text
  , sourceTask :: Text
  , rootTask :: Text
  , partitionCount :: Int
  , fanIn :: Int
  , planDigest :: Text
  , rootStatus :: Text
  , rootResult :: Maybe UUID
  } deriving stock (Eq, Show, Generic)
    deriving anyclass (FromJSON, ToJSON)
