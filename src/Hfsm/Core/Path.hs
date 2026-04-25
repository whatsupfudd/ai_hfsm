{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE GeneralizedNewtypeDeriving #-}

module Hfsm.Core.Path
  ( RegionPath
  , StatePath
  , InstancePath
  , regionPathVector
  , statePathVector
  , instancePathVector
  , regionPathToList
  , statePathToList
  , instancePathToList
  , emptyRegionPath
  , emptyStatePath
  , emptyInstancePath
  , singletonRegionPath
  , singletonStatePath
  , singletonInstancePath
  , appendRegionRef
  , appendStateRef
  , appendInstanceId
  , prependRegionRef
  , prependStateRef
  , prependInstanceId
  , regionPathLength
  , statePathLength
  , instancePathLength
  , regionPathLast
  , statePathLast
  , instancePathLast
  , regionPathInit
  , statePathInit
  , instancePathInit
  , renderRegionPath
  , renderStatePath
  , renderInstancePath
  ) where

import Control.DeepSeq (NFData)

import Data.Text (Text)
import qualified Data.Text as T
import Data.UUID (UUID)
import qualified Data.UUID as U
import Data.Vector (Vector)
import qualified Data.Vector as V

import Data.Aeson (FromJSON, ToJSON)


import Hfsm.Core.Ref
  ( RegionRef
  , StateRef
  , regionRefWord32
  , stateRefWord32
  )

newtype RegionPath = RegionPath (Vector RegionRef)
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (NFData, Semigroup, Monoid, ToJSON, FromJSON)

newtype StatePath = StatePath (Vector StateRef)
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (NFData, Semigroup, Monoid, ToJSON, FromJSON)

newtype InstancePath = InstancePath (Vector UUID)
  deriving stock (Eq, Ord, Show, Read)
  deriving newtype (NFData, Semigroup, Monoid, ToJSON, FromJSON)

regionPathVector :: RegionPath -> Vector RegionRef
regionPathVector (RegionPath xs) = xs

statePathVector :: StatePath -> Vector StateRef
statePathVector (StatePath xs) = xs

instancePathVector :: InstancePath -> Vector UUID
instancePathVector (InstancePath xs) = xs

regionPathToList :: RegionPath -> [RegionRef]
regionPathToList = V.toList . regionPathVector

statePathToList :: StatePath -> [StateRef]
statePathToList = V.toList . statePathVector

instancePathToList :: InstancePath -> [UUID]
instancePathToList = V.toList . instancePathVector

emptyRegionPath :: RegionPath
emptyRegionPath = RegionPath V.empty

emptyStatePath :: StatePath
emptyStatePath = StatePath V.empty

emptyInstancePath :: InstancePath
emptyInstancePath = InstancePath V.empty

singletonRegionPath :: RegionRef -> RegionPath
singletonRegionPath x = RegionPath (V.singleton x)

singletonStatePath :: StateRef -> StatePath
singletonStatePath x = StatePath (V.singleton x)

singletonInstancePath :: UUID -> InstancePath
singletonInstancePath x = InstancePath (V.singleton x)

appendRegionRef :: RegionRef -> RegionPath -> RegionPath
appendRegionRef x (RegionPath xs) = RegionPath (V.snoc xs x)

appendStateRef :: StateRef -> StatePath -> StatePath
appendStateRef x (StatePath xs) = StatePath (V.snoc xs x)

appendInstanceId :: UUID -> InstancePath -> InstancePath
appendInstanceId x (InstancePath xs) = InstancePath (V.snoc xs x)

prependRegionRef :: RegionRef -> RegionPath -> RegionPath
prependRegionRef x (RegionPath xs) = RegionPath (V.cons x xs)

prependStateRef :: StateRef -> StatePath -> StatePath
prependStateRef x (StatePath xs) = StatePath (V.cons x xs)

prependInstanceId :: UUID -> InstancePath -> InstancePath
prependInstanceId x (InstancePath xs) = InstancePath (V.cons x xs)

regionPathLength :: RegionPath -> Int
regionPathLength = V.length . regionPathVector

statePathLength :: StatePath -> Int
statePathLength = V.length . statePathVector

instancePathLength :: InstancePath -> Int
instancePathLength = V.length . instancePathVector

regionPathLast :: RegionPath -> Maybe RegionRef
regionPathLast (RegionPath xs) = xs V.!? (V.length xs - 1)

statePathLast :: StatePath -> Maybe StateRef
statePathLast (StatePath xs) = xs V.!? (V.length xs - 1)

instancePathLast :: InstancePath -> Maybe UUID
instancePathLast (InstancePath xs) = xs V.!? (V.length xs - 1)

regionPathInit :: RegionPath -> Maybe RegionPath
regionPathInit (RegionPath xs)
  | V.null xs = Nothing
  | otherwise = Just (RegionPath (V.init xs))

statePathInit :: StatePath -> Maybe StatePath
statePathInit (StatePath xs)
  | V.null xs = Nothing
  | otherwise = Just (StatePath (V.init xs))

instancePathInit :: InstancePath -> Maybe InstancePath
instancePathInit (InstancePath xs)
  | V.null xs = Nothing
  | otherwise = Just (InstancePath (V.init xs))

renderRegionPath :: RegionPath -> Text
renderRegionPath (RegionPath xs) =
  T.intercalate "/" $
    fmap (tshow . regionRefWord32) (V.toList xs)

renderStatePath :: StatePath -> Text
renderStatePath (StatePath xs) =
  T.intercalate "/" $
    fmap (tshow . stateRefWord32) (V.toList xs)

renderInstancePath :: InstancePath -> Text
renderInstancePath (InstancePath xs) =
  T.intercalate "/" $
    fmap U.toText (V.toList xs)

tshow :: Show a => a -> Text
tshow = T.pack . show