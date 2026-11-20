{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}

module Hfsm.Runtime.Model.Signal
  ( SignalStatus(..)
  , SignalRw(..)
  , signalStatusText
  , renderSignalStatus
  , pendingSignalStatuses
  , terminalSignalStatuses
  , isPendingSignalStatus
  , isTerminalSignalStatus
  , canLeaseSignalStatus
  , signalHasLease
  ) where

import Control.DeepSeq (NFData)

import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON, Value)


data SignalStatus =
    EnteredSs
  | LeasedSs
  | AppliedSs
  | RejectedSs
  | FailedSs
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data SignalRw = SignalRw
  { uid :: Int64
  , uuid :: UUID
  , inst :: UUID
  , status :: SignalStatus
  , payload :: Value
  , cause :: Maybe Value
  , lease :: Maybe UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

signalStatusText :: SignalStatus -> Text
signalStatusText status =
  case status of
    EnteredSs -> "entered"
    LeasedSs -> "leased"
    AppliedSs -> "applied"
    RejectedSs -> "rejected"
    FailedSs -> "failed"

renderSignalStatus :: SignalStatus -> Text
renderSignalStatus = signalStatusText

pendingSignalStatuses :: [SignalStatus]
pendingSignalStatuses = [EnteredSs, LeasedSs]

terminalSignalStatuses :: [SignalStatus]
terminalSignalStatuses = [AppliedSs, RejectedSs, FailedSs]

isPendingSignalStatus :: SignalStatus -> Bool
isPendingSignalStatus status =
  case status of
    EnteredSs -> True
    LeasedSs -> True
    AppliedSs -> False
    RejectedSs -> False
    FailedSs -> False

isTerminalSignalStatus :: SignalStatus -> Bool
isTerminalSignalStatus status =
  case status of
    EnteredSs -> False
    LeasedSs -> False
    AppliedSs -> True
    RejectedSs -> True
    FailedSs -> True

canLeaseSignalStatus :: SignalStatus -> Bool
canLeaseSignalStatus status =
  case status of
    EnteredSs -> True
    LeasedSs -> False
    AppliedSs -> False
    RejectedSs -> False
    FailedSs -> False

signalHasLease :: SignalRw -> Bool
signalHasLease signalRw =
  case signalRw.lease of
    Nothing -> False
    Just _ -> True