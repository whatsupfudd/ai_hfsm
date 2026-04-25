{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hfsm.Runtime.Model.Child
  ( ChildStatus(..)
  , ChildRw(..)
  , renderChildStatus
  , parseChildStatus
  , isChildLive
  , isChildTerminal
  ) where

import Control.DeepSeq (NFData)
import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON(..), ToJSON(..), Value(String), withText)

import Hfsm.Core.Ref (JoinRef)


data ChildStatus
  = SpawnedCs
  | RunningCs
  | DoneCs
  | FailedCs
  | CancelledCs
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data ChildRw = ChildRw
  { uid :: Int64
  , uuid :: UUID
  , parent :: UUID
  , child :: UUID
  , key :: Maybe Text
  , status :: ChildStatus
  , join :: Maybe JoinRef
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

renderChildStatus :: ChildStatus -> Text
renderChildStatus childStatus =
  case childStatus of
    SpawnedCs -> "spawned"
    RunningCs -> "running"
    DoneCs -> "done"
    FailedCs -> "failed"
    CancelledCs -> "cancelled"

parseChildStatus :: Text -> Maybe ChildStatus
parseChildStatus raw =
  case T.toLower (T.strip raw) of
    "spawned" -> Just SpawnedCs
    "running" -> Just RunningCs
    "done" -> Just DoneCs
    "failed" -> Just FailedCs
    "cancelled" -> Just CancelledCs
    "canceled" -> Just CancelledCs
    _ -> Nothing

isChildLive :: ChildStatus -> Bool
isChildLive childStatus =
  case childStatus of
    SpawnedCs -> True
    RunningCs -> True
    DoneCs -> False
    FailedCs -> False
    CancelledCs -> False

isChildTerminal :: ChildStatus -> Bool
isChildTerminal = not . isChildLive

instance ToJSON ChildStatus where
  toJSON = String . renderChildStatus

instance FromJSON ChildStatus where
  parseJSON =
    withText "ChildStatus" $ \txt ->
      case parseChildStatus txt of
        Just childStatus -> pure childStatus
        Nothing -> fail ("invalid ChildStatus: " <> T.unpack txt)