{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

-- | Semantic persistence boundary for one HFSM turn.
--
-- 'commitTurn' is the ONLY operation that persists a successful turn. Its
-- backend MUST atomically validate the expected snapshot and leased signal,
-- append step/snapshot/effects/children/breaks/projections, advance the
-- instance, and acknowledge the input. Errors commit no partial writes.
module Hfsm.Store.Class
  ( Store(..)
  , LeaseRq(..)
  , LeaseRez(..)
  , AckRq(..)
  , PollRq(..)
  , CommitRq(..)
  , CommitRez(..)
  , StoreErr(..)
  , OutboxAppend(..)
  , ChildAppend(..)
  , BreakAppend(..)
  , ProjBatch(..)
  -- To remove after deprecation of SnapshotAppend:
  , SnapshotAppend(..)
  , StepAppend(..)
  ) where

import Control.DeepSeq (NFData)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON, Value)

import Hfsm.Core.Ref (JoinRef, StateRef, CaseRef, RouteRef)
import Hfsm.Runtime.Model.Break (BreakKind, BreakStatus)
import Hfsm.Runtime.Model.Child (ChildStatus)
import Hfsm.Runtime.Model.Instance (InstanceRw, InstanceStatus)
import Hfsm.Runtime.Model.Signal (SignalRw, SignalStatus)
import Hfsm.Runtime.Model.Snapshot (SnapshotRw)
import Hfsm.Runtime.Model.Step (StepRw, StepKind)
-- To remove after deprecation of SnapshotAppend:
import Hfsm.Core.Version (MachineVersion)
import Hfsm.Core.Digest (SpecDigest)
import Hfsm.Core.Path (StatePath)

-- | The persistence contract, deliberately independent of PostgreSQL.
-- 'ackSignal' is reserved for rejected/failed/requeued turns which produce no step.
-- Do NOT acknowledge successful turns with 'ackSignal': 'commitTurn' owns that.
data Store m = Store
  { leaseSignal  :: LeaseRq -> m LeaseRez
  , loadInstance :: UUID -> m (Maybe InstanceRw)
  , loadSnapshot :: UUID -> m (Maybe SnapshotRw)
  , commitTurn   :: CommitRq -> m (Either StoreErr CommitRez)
  , ackSignal    :: AckRq -> m (Either StoreErr ())
  , listSignals  :: PollRq -> m [SignalRw]
  }

-- | Inputs are leased before planning, and the lease UUID acts as a fence.
data LeaseRq = LeaseRq
  { node :: UUID
  , batchSize :: Int
  , leaseSeconds :: Int
  , now :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data LeaseRez = LeaseNoneLz | LeaseSomeLz [SignalRw]
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

-- | Failure/rejection/requeue acknowledgement; must verify lease and status.
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

-- | All data necessary to persist a turn in ONE storage transaction.
-- Step/snapshot UUIDs are assigned by pure planning. Stores assign UIDs.
-- The (instanceId, expectedSnapshot) pair is a CAS revision guard until an
-- explicit monotonic revision is introduced in the following milestone.
-- Effects must be recorded with the committed step UUID (not UUID.nil).
data CommitRq = CommitRq
  { instanceId :: UUID
  , expectedSnapshot :: UUID
  , signalId :: UUID
  , leaseId :: UUID
  , step :: StepRw
  , snapshot :: SnapshotRw
  , nextStatus :: InstanceStatus
  , outbox :: [OutboxAppend]
  , child :: [ChildAppend]
  , breaks :: [BreakAppend]
  , proj :: ProjBatch
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
  a <> b = ProjBatch
    { active = a.active <> b.active
    , trace = a.trace <> b.trace
    , queue = a.queue <> b.queue
    , tree = a.tree <> b.tree
    }

instance Monoid ProjBatch where
  mempty = ProjBatch
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

-- To Deprecate:
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

