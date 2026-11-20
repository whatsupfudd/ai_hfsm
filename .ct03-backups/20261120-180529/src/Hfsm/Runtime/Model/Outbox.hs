{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Hfsm.Runtime.Model.Outbox
  ( OutboxStatus(..)
  , OutboxRw(..)
  , outboxStatusText
  , parseOutboxStatus
  , isPendingOutboxStatus
  , isTerminalOutboxStatus
  , isSuccessfulOutboxStatus
  , renderOutboxStatus
  ) where

import Control.DeepSeq (NFData)

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON(..), ToJSON(..), Value, withText)


data OutboxStatus =
    ReadyOs
  | LeasedOs
  | DoneOs
  | FailedOs
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data OutboxRw = OutboxRw
  { uid :: Int64
  , uuid :: UUID
  , inst :: UUID
  , step :: UUID
  , status :: OutboxStatus
  , payload :: Value
  , lease :: Maybe UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

outboxStatusText :: OutboxStatus -> Text
outboxStatusText status =
  case status of
    ReadyOs -> "ready"
    LeasedOs -> "leased"
    DoneOs -> "done"
    FailedOs -> "failed"

parseOutboxStatus :: Text -> Maybe OutboxStatus
parseOutboxStatus raw =
  case T.toLower (T.strip raw) of
    "ready" -> Just ReadyOs
    "leased" -> Just LeasedOs
    "done" -> Just DoneOs
    "failed" -> Just FailedOs
    _ -> Nothing

isPendingOutboxStatus :: OutboxStatus -> Bool
isPendingOutboxStatus status =
  case status of
    ReadyOs -> True
    LeasedOs -> True
    DoneOs -> False
    FailedOs -> False

isTerminalOutboxStatus :: OutboxStatus -> Bool
isTerminalOutboxStatus status =
  case status of
    ReadyOs -> False
    LeasedOs -> False
    DoneOs -> True
    FailedOs -> True

isSuccessfulOutboxStatus :: OutboxStatus -> Bool
isSuccessfulOutboxStatus status =
  case status of
    DoneOs -> True
    ReadyOs -> False
    LeasedOs -> False
    FailedOs -> False

renderOutboxStatus :: OutboxStatus -> Text
renderOutboxStatus = outboxStatusText

instance ToJSON OutboxStatus where
  toJSON = toJSON . outboxStatusText

instance FromJSON OutboxStatus where
  parseJSON = withText "OutboxStatus" $ \txt ->
    case parseOutboxStatus txt of
      Just status -> pure status
      Nothing -> fail (T.unpack ("invalid outbox status: " <> txt))