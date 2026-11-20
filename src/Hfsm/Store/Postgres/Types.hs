{-# LANGUAGE RankNTypes #-}

module Hfsm.Store.Postgres.Types
  ( Runner(..), runWithPool
  , NewInstance(..), EnqueueRq(..) ) where

import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)

import Data.Aeson (Value)

import qualified Hasql.Connection as Hc
import qualified Hasql.Session as Hs
import qualified Hasql.Pool as Hp

import Hfsm.Core.Path (StatePath)
import Hfsm.Core.Ref (StateRef)
import Hfsm.Runtime.Model.Instance (InstanceStatus)
import Hfsm.Runtime.Registry (MachineKey)

-- | The adapter is pool-agnostic. A caller can use Hc.use for a
-- single connection, or borrow a connection for each Session from its pool.
-- The same borrowed connection is held throughout each transaction Session.
newtype Runner = Runner
  { runSession :: forall a. Hs.Session a -> IO (Either Hp.UsageError a) }


runWithPool :: Hp.Pool -> Runner
runWithPool pool = Runner (Hp.use pool)


-- | Caller-provided IDs support child UUIDs reserved by the parent turn.
-- The initial snapshot/instance are inserted in the same SQL transaction.
data NewInstance = NewInstance
  { instanceId :: UUID
  , snapshotId :: UUID
  , machine :: MachineKey
  , status :: InstanceStatus
  , state :: StateRef
  , path :: StatePath
  , ctx :: Value
  , wait :: Maybe Value
  , child :: Value
  , parent :: Maybe UUID
  , createdAt :: UTCTime
  }
  deriving (Show)

-- | The producer's key is optional, but should be provided whenever producer
-- requests might be repeated after a network timeout.
data EnqueueRq = EnqueueRq
  { instanceId :: UUID
  , signalId :: UUID
  , payload :: Value
  , cause :: Maybe Value
  , dedupeKey :: Maybe Text
  , readyAt :: UTCTime
  }
  deriving (Eq, Show)
