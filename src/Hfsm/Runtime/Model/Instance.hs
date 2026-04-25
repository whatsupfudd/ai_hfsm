{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hfsm.Runtime.Model.Instance
  ( InstanceStatus(..)
  , InstanceRw(..)
  , instanceStatusText
  , parseInstanceStatus
  , renderInstanceStatus
  , instanceIsActive
  , instanceIsTerminal
  , setInstanceStatus
  , setInstanceState
  , setInstanceSnap
  , touchInstance
  ) where

import Data.Int (Int64)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON(..), FromJSONKeyFunction(..), ToJSON(..), ToJSONKey(..), Value(String), withText)
import Data.Aeson.Types (FromJSONKey(..), toJSONKeyText)

import Hfsm.Core.Digest (SpecDigest)
import Hfsm.Core.Name (MachineName)
import Hfsm.Core.Path (StatePath)
import Hfsm.Core.Ref (StateRef)
import Hfsm.Core.Version (MachineVersion)

data InstanceStatus
  = RunningIs
  | WaitingIs
  | PausedIs
  | DoneIs
  | FailedIs
  | CancelledIs
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)

data InstanceRw = InstanceRw
  { uid :: Int64
  , uuid :: UUID
  , machine :: MachineName
  , version :: MachineVersion
  , digest :: SpecDigest
  , status :: InstanceStatus
  , state :: StateRef
  , path :: StatePath
  , snap :: UUID
  , parent :: Maybe UUID
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

instanceStatusText :: InstanceStatus -> Text
instanceStatusText status =
  case status of
    RunningIs -> "running"
    WaitingIs -> "waiting"
    PausedIs -> "paused"
    DoneIs -> "done"
    FailedIs -> "failed"
    CancelledIs -> "cancelled"

parseInstanceStatus :: Text -> Either Text InstanceStatus
parseInstanceStatus raw =
  case normalizeStatus raw of
    "running" -> Right RunningIs
    "runningis" -> Right RunningIs
    "waiting" -> Right WaitingIs
    "waitingis" -> Right WaitingIs
    "paused" -> Right PausedIs
    "pausedis" -> Right PausedIs
    "done" -> Right DoneIs
    "doneis" -> Right DoneIs
    "failed" -> Right FailedIs
    "failedis" -> Right FailedIs
    "cancelled" -> Right CancelledIs
    "cancelledis" -> Right CancelledIs
    "canceled" -> Right CancelledIs
    "canceledis" -> Right CancelledIs
    txt -> Left ("invalid instance status: " <> txt)

renderInstanceStatus :: InstanceStatus -> Text
renderInstanceStatus = instanceStatusText

instanceIsActive :: InstanceStatus -> Bool
instanceIsActive status =
  case status of
    RunningIs -> True
    WaitingIs -> True
    PausedIs -> True
    DoneIs -> False
    FailedIs -> False
    CancelledIs -> False

instanceIsTerminal :: InstanceStatus -> Bool
instanceIsTerminal status =
  case status of
    RunningIs -> False
    WaitingIs -> False
    PausedIs -> False
    DoneIs -> True
    FailedIs -> True
    CancelledIs -> True

setInstanceStatus :: InstanceStatus -> InstanceRw -> InstanceRw
setInstanceStatus status' row = row { status = status' }

setInstanceState :: StateRef -> StatePath -> InstanceRw -> InstanceRw
setInstanceState state' path' row = row { state = state', path = path' }

setInstanceSnap :: UUID -> InstanceRw -> InstanceRw
setInstanceSnap snap' row = row { snap = snap' }

touchInstance :: UTCTime -> InstanceRw -> InstanceRw
touchInstance now row = row { updatedAt = now }

instance ToJSON InstanceStatus where
  toJSON = String . instanceStatusText

instance FromJSON InstanceStatus where
  parseJSON = withText "InstanceStatus" $ \txt ->
    either (fail . T.unpack) pure (parseInstanceStatus txt)

instance ToJSONKey InstanceStatus where
  toJSONKey = toJSONKeyText instanceStatusText

instance FromJSONKey InstanceStatus where
  fromJSONKey = FromJSONKeyTextParser $ \txt ->
    either (fail . T.unpack) pure (parseInstanceStatus txt)

normalizeStatus :: Text -> Text
normalizeStatus = T.toLower . T.strip