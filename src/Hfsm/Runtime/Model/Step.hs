{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hfsm.Runtime.Model.Step
  ( StepKind(..)
  , StepRw(..)
  , parseStepKind
  , renderStepKind
  , stepHasSignal
  , stepChangesState
  , stepIsSynthetic
  ) where

import Control.DeepSeq (NFData)

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON(..), ToJSON(..), Value, withText)

import Hfsm.Core.Ref (CaseRef, RouteRef, StateRef)


data StepKind
  = EntrySk
  | SignalSk
  | ReplaySk
  | MigrateSk
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data StepRw = StepRw
  { uid :: Int64
  , uuid :: UUID
  , inst :: UUID
  , signal :: Maybe UUID
  , kind :: StepKind
  , from :: StateRef
  , to :: StateRef
  , caseRef :: Maybe CaseRef
  , route :: RouteRef
  , note :: Value
  , createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

parseStepKind :: Text -> Maybe StepKind
parseStepKind raw =
  case T.toLower (T.strip raw) of
    "entry" -> Just EntrySk
    "signal" -> Just SignalSk
    "replay" -> Just ReplaySk
    "migrate" -> Just MigrateSk
    _ -> Nothing

renderStepKind :: StepKind -> Text
renderStepKind kind =
  case kind of
    EntrySk -> "entry"
    SignalSk -> "signal"
    ReplaySk -> "replay"
    MigrateSk -> "migrate"

stepHasSignal :: StepRw -> Bool
stepHasSignal step = maybe False (const True) step.signal

stepChangesState :: StepRw -> Bool
stepChangesState step = step.from /= step.to

stepIsSynthetic :: StepRw -> Bool
stepIsSynthetic step =
  case step.kind of
    SignalSk -> False
    EntrySk -> True
    ReplaySk -> True
    MigrateSk -> True

instance ToJSON StepKind where
  toJSON = toJSON . renderStepKind

instance FromJSON StepKind where
  parseJSON = withText "StepKind" $ \txt ->
    case parseStepKind txt of
      Just kind -> pure kind
      Nothing -> fail ("invalid step kind: " <> T.unpack txt)