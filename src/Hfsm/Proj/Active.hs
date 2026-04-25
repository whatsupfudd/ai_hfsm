{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module Hfsm.Proj.Active
  ( ActiveProj(..)
  , mkActiveProj
  , activeFromInstance
  , activeFromRows
  , setActiveWait
  , clearActiveWait
  , setActiveBlockedOn
  , clearActiveBlockedOn
  , touchActive
  , activeHasWait
  , activeIsRunning
  , activeIsWaiting
  , activeIsPaused
  , activeIsBlocked
  , activeIsActive
  , activeIsTerminal
  ) where

import Control.DeepSeq (NFData)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON, Value)

import Hfsm.Core.Name (MachineName)
import Hfsm.Core.Path (StatePath)
import Hfsm.Core.Ref (StateRef)
import Hfsm.Core.Version (MachineVersion)
import Hfsm.Runtime.Model.Instance
  ( InstanceRw
  , InstanceStatus(..)
  , instanceIsActive
  , instanceIsTerminal
  )
import Hfsm.Runtime.Model.Snapshot (SnapshotRw)
import Hfsm.Runtime.Model.Instance (InstanceRw(..))
import Hfsm.Runtime.Model (SnapshotRw(..))


data ActiveProj = ActiveProj
  { instance_ :: UUID
  , machine :: MachineName
  , version :: MachineVersion
  , status :: InstanceStatus
  , state :: StateRef
  , path :: StatePath
  , wait :: Maybe Value
  , blockedOn :: Maybe Text
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

mkActiveProj
  :: UUID
  -> MachineName
  -> MachineVersion
  -> InstanceStatus
  -> StateRef
  -> StatePath
  -> Maybe Value
  -> Maybe Text
  -> UTCTime
  -> ActiveProj
mkActiveProj instanceUuid machine version status state path wait blockedOn updatedAt =
  ActiveProj
    { instance_ = instanceUuid
    , machine = machine
    , version = version
    , status = status
    , state = state
    , path = path
    , wait = wait
    , blockedOn = normalizeMaybeText blockedOn
    , updatedAt = updatedAt
    }

activeFromInstance :: InstanceRw -> ActiveProj
activeFromInstance instRw =
  mkActiveProj
    instRw.uuid
    instRw.machine
    instRw.version
    instRw.status
    instRw.state
    instRw.path
    Nothing
    Nothing
    instRw.updatedAt

activeFromRows :: InstanceRw -> Maybe SnapshotRw -> Maybe Text -> ActiveProj
activeFromRows instRw snapRw blockedOn =
  let state' =
        case snapRw of
          Nothing -> instRw.state
          Just row -> row.state

      path' =
        case snapRw of
          Nothing -> instRw.path
          Just row -> row.path

      wait' =
        case snapRw of
          Nothing -> Nothing
          Just row -> row.wait
  in
  mkActiveProj
    instRw.uuid
    instRw.machine
    instRw.version
    instRw.status
    state'
    path'
    wait'
    blockedOn
    instRw.updatedAt

setActiveWait :: Maybe Value -> ActiveProj -> ActiveProj
setActiveWait wait active =
  active { wait = wait }

clearActiveWait :: ActiveProj -> ActiveProj
clearActiveWait active =
  active { wait = Nothing }

setActiveBlockedOn :: Maybe Text -> ActiveProj -> ActiveProj
setActiveBlockedOn blockedOn active =
  active { blockedOn = normalizeMaybeText blockedOn }

clearActiveBlockedOn :: ActiveProj -> ActiveProj
clearActiveBlockedOn active =
  active { blockedOn = Nothing }

touchActive :: UTCTime -> ActiveProj -> ActiveProj
touchActive now active =
  active { updatedAt = now }

activeHasWait :: ActiveProj -> Bool
activeHasWait active =
  isJust active.wait

activeIsRunning :: ActiveProj -> Bool
activeIsRunning active =
  active.status == RunningIs

activeIsWaiting :: ActiveProj -> Bool
activeIsWaiting active =
  active.status == WaitingIs

activeIsPaused :: ActiveProj -> Bool
activeIsPaused active =
  active.status == PausedIs

activeIsBlocked :: ActiveProj -> Bool
activeIsBlocked active =
  activeIsWaiting active || activeIsPaused active || isJust active.wait || isJust active.blockedOn

activeIsActive :: ActiveProj -> Bool
activeIsActive active =
  instanceIsActive active.status

activeIsTerminal :: ActiveProj -> Bool
activeIsTerminal active =
  instanceIsTerminal active.status

normalizeMaybeText :: Maybe Text -> Maybe Text
normalizeMaybeText raw =
  case raw of
    Nothing -> Nothing
    Just txt ->
      let txt' = T.strip txt
      in if T.null txt' then Nothing else Just txt'