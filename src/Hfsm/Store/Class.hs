{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module Hfsm.Store.Class
  ( Store(..)
  , LeaseRq(..)
  , LeaseRez(..)
  , AckRq(..)
  , PollRq(..)
  , StepAppend(..)
  , SnapshotAppend(..)
  , OutboxAppend(..)
  , ChildAppend(..)
  , BreakAppend(..)
  , ProjBatch(..)
  , CommitRez(..)
  , StoreErr(..)
  ) where

import Control.DeepSeq (NFData)

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON, Value)

import Hfsm.Core.Digest (SpecDigest)
import Hfsm.Core.Path (StatePath)
import Hfsm.Core.Ref (CaseRef, JoinRef, RouteRef, StateRef)
import Hfsm.Core.Version (MachineVersion)

import Hfsm.Runtime.Model.Break (BreakKind, BreakStatus)
import Hfsm.Runtime.Model.Child (ChildStatus)
import Hfsm.Runtime.Model.Instance (InstanceRw)
import Hfsm.Runtime.Model.Signal (SignalRw, SignalStatus)
import Hfsm.Runtime.Model.Snapshot (SnapshotRw)
import Hfsm.Runtime.Model.Step (StepKind, StepRw)

data Store m = Store
  { leaseSignal :: LeaseRq -> m LeaseRez
  , loadInstance :: UUID -> m (Maybe InstanceRw)
  , loadSnapshot :: UUID -> m (Maybe SnapshotRw)
  , appendStep :: StepAppend -> m StepRw
  , appendSnapshot :: SnapshotAppend -> m SnapshotRw
  , appendOutbox :: [OutboxAppend] -> m ()
  , appendChild :: [ChildAppend] -> m ()
  , appendBreak :: [BreakAppend] -> m ()
  , ackSignal :: AckRq -> m ()
  , updateProj :: ProjBatch -> m ()
  , listSignals :: PollRq -> m [SignalRw]
  }

data LeaseRq = LeaseRq
  { node :: UUID
  , batchSize :: Int
  , leaseSeconds :: Int
  , now :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data LeaseRez
  = LeaseNoneLz
  | LeaseSomeLz [SignalRw]
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data AckRq = AckRq
  { signal :: UUID
  , lease :: UUID
  , status :: SignalStatus
  , note :: Maybe Text
  , now :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data PollRq = PollRq
  { status :: [SignalStatus]
  , limit :: Int
  , now :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data StepAppend = StepAppend
  { inst :: UUID
  , signal :: Maybe UUID
  , kind :: StepKind
  , from :: StateRef
  , to :: StateRef
  , caseRef :: Maybe CaseRef
  , route :: RouteRef
  , note :: Value
  , now :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data SnapshotAppend = SnapshotAppend
  { inst :: UUID
  , state :: StateRef
  , path :: StatePath
  , ctx :: Value
  , wait :: Maybe Value
  , child :: Value
  , version :: MachineVersion
  , digest :: SpecDigest
  , now :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data OutboxAppend = OutboxAppend
  { inst :: UUID
  , step :: UUID
  , payload :: Value
  , now :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data ChildAppend = ChildAppend
  { parent :: UUID
  , child :: UUID
  , key :: Maybe Text
  , status :: ChildStatus
  , join :: Maybe JoinRef
  , now :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data BreakAppend = BreakAppend
  { inst :: Maybe UUID
  , kind :: BreakKind
  , state :: Maybe StateRef
  , status :: BreakStatus
  , note :: Maybe Text
  , now :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data ProjBatch = ProjBatch
  { active :: [Value]
  , trace :: [Value]
  , queue :: [Value]
  , tree :: [Value]
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

instance Semigroup ProjBatch where
  a <> b =
    ProjBatch
      { active = a.active <> b.active
      , trace = a.trace <> b.trace
      , queue = a.queue <> b.queue
      , tree = a.tree <> b.tree
      }

instance Monoid ProjBatch where
  mempty =
    ProjBatch
      { active = []
      , trace = []
      , queue = []
      , tree = []
      }

data CommitRez = CommitRez
  { step :: StepRw
  , snapshot :: SnapshotRw
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data StoreErr
  = NotFoundEr Text
  | ConflictEr Text
  | LeaseLostEr Text
  | DecodeEr Text
  | BackendEr Text
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)