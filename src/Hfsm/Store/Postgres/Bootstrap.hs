{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE NoFieldSelectors #-}

-- | Convenience constructors for inserting actual compiled HFSMs. No SQL
-- execution happens in this module; the caller chooses the Runner.
module Hfsm.Store.Postgres.Bootstrap
  ( initialLeaf
  , newInstanceFor
  , enqueueFor
  ) where

import Data.Aeson (object)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as U4
import Hfsm.Compile (CompiledMachine(..))
import Hfsm.Core.Codec (Codec(..), CodecSet(..))
import Hfsm.Core.Ref (RegionRef, StateRef, regionRefWord32, stateRefWord32)
import Hfsm.Graph.Def (MachineGraph(..), StateNd(..), RegionNd(..))
import Hfsm.Runtime.Model.Instance (InstanceStatus(..))
import Hfsm.Runtime.Registry (machineKeyOf)
import Hfsm.Spec.Def (MachineSpec(..))
import Hfsm.Store.Postgres.Types (NewInstance(..), EnqueueRq(..))

-- | Follow the unique initial region branch until an actual leaf is reached.
-- The visited counter detects cycles if the compiled graph is corrupt.
initialLeaf :: MachineGraph -> Either Text StateNd
initialLeaf g = visitRegion 0 g.root
 where
  budget = IM.size g.states + IM.size g.regions + 1
  visitRegion n rid
    | n > budget = Left "cycle in initial hierarchy"
    | otherwise = case IM.lookup (fromIntegral (regionRefWord32 rid)) g.regions of
        Nothing -> Left "missing initial region"
        Just r -> visitState (n + 1) r.initial
  visitState n sid
    | n > budget = Left "cycle in initial hierarchy"
    | otherwise = case IM.lookup (fromIntegral (stateRefWord32 sid)) g.states of
        Nothing -> Left "missing initial state"
        Just s -> case s.child of
          Nothing -> Right s
          Just rid -> visitRegion (n + 1) rid

-- | Construct (but do not persist) a revision-0 instance and snapshot.
-- A reserved child UUID can be supplied as instanceId after construction.
newInstanceFor :: CompiledMachine st sg hf ctx cmd child -> ctx
               -> Maybe UUID -> IO (Either Text NewInstance)
newInstanceFor compiled ctx parentId = do
  instId <- U4.nextRandom
  snapId <- U4.nextRandom
  now <- getCurrentTime
  pure $ do
    stateNode <- initialLeaf compiled.graph
    pure NewInstance
      { instanceId = instId
      , snapshotId = snapId
      , machine = machineKeyOf compiled
      , status = RunningIs
      , state = stateNode.ref
      , path = stateNode.path
      , ctx = compiled.spec.codecs.ctx.encode ctx
      , wait = Nothing
      , child = object []
      , parent = parentId
      , createdAt = now
      }

-- | Use the compiled machine's signal codec rather than an unrelated ToJSON.
enqueueFor :: CompiledMachine st sg hf ctx cmd child -> UUID -> sg
           -> Maybe Text -> IO EnqueueRq
enqueueFor compiled instId sg dedupe = do
  sigId <- U4.nextRandom
  now <- getCurrentTime
  pure EnqueueRq
    { instanceId = instId
    , signalId = sigId
    , payload = compiled.spec.codecs.signal.encode sg
    , cause = Nothing
    , dedupeKey = dedupe
    , readyAt = now
    }
