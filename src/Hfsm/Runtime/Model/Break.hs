{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module Hfsm.Runtime.Model.Break
  ( BreakStatus(..)
  , BreakKind(..)
  , BreakRw(..)
  , breakIsArmed
  , breakIsHit
  , breakIsCleared
  , breakIsActive
  , breakAppliesToInst
  , breakAppliesToState
  , renderBreakStatus
  , renderBreakKind
  ) where

import Control.DeepSeq (NFData)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

import Hfsm.Core.Ref (StateRef)


data BreakStatus =
    ArmedBs
  | HitBs
  | ClearedBs
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data BreakKind =
    BeforeCaseBk
  | AfterCaseBk
  | BeforeRouteBk
  | AfterRouteBk
  | BeforeCommitBk
  | OnErrorBk
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data BreakRw = BreakRw
  { uid :: Int64
  , uuid :: UUID
  , inst :: Maybe UUID
  , kind :: BreakKind
  , state :: Maybe StateRef
  , status :: BreakStatus
  , note :: Maybe Text
  , createdAt :: UTCTime
  , updatedAt :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

breakIsArmed :: BreakRw -> Bool
breakIsArmed breakRw =
  breakRw.status == ArmedBs

breakIsHit :: BreakRw -> Bool
breakIsHit breakRw =
  breakRw.status == HitBs

breakIsCleared :: BreakRw -> Bool
breakIsCleared breakRw =
  breakRw.status == ClearedBs

breakIsActive :: BreakRw -> Bool
breakIsActive breakRw =
  case breakRw.status of
    ArmedBs -> True
    HitBs -> True
    ClearedBs -> False

breakAppliesToInst :: UUID -> BreakRw -> Bool
breakAppliesToInst instUuid breakRw =
  case breakRw.inst of
    Nothing -> True
    Just x -> x == instUuid

breakAppliesToState :: StateRef -> BreakRw -> Bool
breakAppliesToState stateRef breakRw =
  case breakRw.state of
    Nothing -> True
    Just x -> x == stateRef

renderBreakStatus :: BreakStatus -> Text
renderBreakStatus status =
  case status of
    ArmedBs -> "armed"
    HitBs -> "hit"
    ClearedBs -> "cleared"

renderBreakKind :: BreakKind -> Text
renderBreakKind kind =
  case kind of
    BeforeCaseBk -> "before-case"
    AfterCaseBk -> "after-case"
    BeforeRouteBk -> "before-route"
    AfterRouteBk -> "after-route"
    BeforeCommitBk -> "before-commit"
    OnErrorBk -> "on-error"