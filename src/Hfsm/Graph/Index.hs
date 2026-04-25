{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module Hfsm.Graph.Index
  ( GraphIx(..)
  , buildIx
  , initialState
  , descendantsOf
  , ancestorsOf
  ) where

import Control.DeepSeq (NFData)

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import GHC.Generics (Generic)

import Hfsm.Core.Path (StatePath, statePathVector)
import Hfsm.Core.Ref (CaseRef, RegionRef, RouteRef, StateRef, regionRefWord32, stateRefWord32)
import Hfsm.Graph.Def (MachineGraph(..), RegionNd(..), StateNd(..))


data GraphIx = GraphIx
  { stateByPath :: Map StatePath StateRef
  , parentState :: IntMap (Maybe StateRef)
  , childRegion :: IntMap (Maybe RegionRef)
  , caseByState :: IntMap (Vector CaseRef)
  , routeByState :: IntMap (Vector RouteRef)
  , descendants :: IntMap (Set StateRef)
  , ancestors :: IntMap (Vector StateRef)
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

buildIx :: MachineGraph -> GraphIx
buildIx graph =
  let stateNds = IM.elems graph.states
      ancestorsIx = buildAncestorsIx stateNds
  in GraphIx
       { stateByPath = buildStateByPathIx stateNds
       , parentState = buildParentStateIx ancestorsIx
       , childRegion = buildChildRegionIx stateNds
       , caseByState = buildCaseByStateIx stateNds
       , routeByState = buildRouteByStateIx stateNds
       , descendants = buildDescendantsIx stateNds ancestorsIx
       , ancestors = ancestorsIx
       }

initialState :: MachineGraph -> StateRef
initialState graph =
  case IM.lookup (regionRefKey graph.root) graph.regions of
    Just regionNd -> regionNd.initial
    Nothing -> error (T.unpack (missingRootRegionMsg graph.root))

descendantsOf :: GraphIx -> StateRef -> Set StateRef
descendantsOf ix stateRef =
  fromMaybe S.empty (IM.lookup (stateRefKey stateRef) ix.descendants)

ancestorsOf :: GraphIx -> StateRef -> Vector StateRef
ancestorsOf ix stateRef =
  fromMaybe V.empty (IM.lookup (stateRefKey stateRef) ix.ancestors)

buildStateByPathIx :: [StateNd] -> Map StatePath StateRef
buildStateByPathIx =
  foldl' step M.empty
  where
    step :: Map StatePath StateRef -> StateNd -> Map StatePath StateRef
    step acc stateNd =
      M.insert stateNd.path stateNd.ref acc

buildParentStateIx :: IntMap (Vector StateRef) -> IntMap (Maybe StateRef)
buildParentStateIx =
  IM.map vectorLast

buildChildRegionIx :: [StateNd] -> IntMap (Maybe RegionRef)
buildChildRegionIx =
  foldl' step IM.empty
  where
    step :: IntMap (Maybe RegionRef) -> StateNd -> IntMap (Maybe RegionRef)
    step acc stateNd =
      IM.insert (stateRefKey stateNd.ref) stateNd.child acc

buildCaseByStateIx :: [StateNd] -> IntMap (Vector CaseRef)
buildCaseByStateIx =
  foldl' step IM.empty
  where
    step :: IntMap (Vector CaseRef) -> StateNd -> IntMap (Vector CaseRef)
    step acc stateNd =
      IM.insert (stateRefKey stateNd.ref) stateNd.cases acc

buildRouteByStateIx :: [StateNd] -> IntMap (Vector RouteRef)
buildRouteByStateIx =
  foldl' step IM.empty
  where
    step :: IntMap (Vector RouteRef) -> StateNd -> IntMap (Vector RouteRef)
    step acc stateNd =
      IM.insert (stateRefKey stateNd.ref) stateNd.routes acc

buildAncestorsIx :: [StateNd] -> IntMap (Vector StateRef)
buildAncestorsIx =
  foldl' step IM.empty
  where
    step :: IntMap (Vector StateRef) -> StateNd -> IntMap (Vector StateRef)
    step acc stateNd =
      IM.insert (stateRefKey stateNd.ref) (stateAncestors stateNd.ref stateNd.path) acc

buildDescendantsIx :: [StateNd] -> IntMap (Vector StateRef) -> IntMap (Set StateRef)
buildDescendantsIx stateNds ancestorsIx =
  foldl' step emptyIx stateNds
  where
    emptyIx =
      foldl' (\acc stateNd -> IM.insert (stateRefKey stateNd.ref) S.empty acc) IM.empty stateNds

    step :: IntMap (Set StateRef) -> StateNd -> IntMap (Set StateRef)
    step acc stateNd =
      case IM.lookup (stateRefKey stateNd.ref) ancestorsIx of
        Nothing -> acc
        Just ancestorRefs ->
          V.foldl' (insertDescendant stateNd.ref) acc ancestorRefs

insertDescendant :: StateRef -> IntMap (Set StateRef) -> StateRef -> IntMap (Set StateRef)
insertDescendant stateRef acc ancestorRef
  | ancestorRef == stateRef = acc
  | otherwise = IM.insertWith S.union (stateRefKey ancestorRef) (S.singleton stateRef) acc

stateAncestors :: StateRef -> StatePath -> Vector StateRef
stateAncestors selfRef path =
  let refs = statePathVector path
  in case vectorLast refs of
       Just lastRef | lastRef == selfRef -> V.filter (/= selfRef) (V.init refs)
       _ -> V.filter (/= selfRef) refs

vectorLast :: Vector a -> Maybe a
vectorLast xs
  | V.null xs = Nothing
  | otherwise = Just (V.last xs)

regionRefKey :: RegionRef -> Int
regionRefKey = fromIntegral . regionRefWord32

stateRefKey :: StateRef -> Int
stateRefKey = fromIntegral . stateRefWord32

missingRootRegionMsg :: RegionRef -> Text
missingRootRegionMsg rootRef =
  "missing root region in machine graph index build: " <> tshow (regionRefWord32 rootRef)

tshow :: Show a => a -> Text
tshow = T.pack . show