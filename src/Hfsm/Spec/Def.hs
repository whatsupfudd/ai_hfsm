{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module Hfsm.Spec.Def
  ( StateKind(..)
  , RegionSpec(..)
  , StateSpec(..)
  , MachineSpec(..)
  , parseStateKind
  , renderStateKind
  , mkRegionSpec
  , mkStateSpec
  , mkMachineSpec
  , setRegionName
  , clearRegionName
  , setRegionInitial
  , setRegionStates
  , addRegionState
  , setRegionMeta
  , mergeRegionMeta
  , setStateKind
  , setStateEntry
  , clearStateEntry
  , setStateCases
  , addStateCase
  , setStateRoutes
  , addStateRoute
  , setStateChild
  , clearStateChild
  , setStateMeta
  , mergeStateMeta
  , setMachineRoot
  , setMachineMeta
  , mergeMachineMeta
  , stateIsAtomic
  , stateIsComposite
  , stateIsTerminal
  , stateHasEntry
  , stateHasChild
  , stateCaseCount
  , stateRouteCount
  , regionStateCount
  , regionHasInitial
  , regionStateKeys
  , regionStatesDeep
  , findStateSpec
  , findStateSpecDeep
  , mapStateSpec
  , mapRegionSpec
  , mapMachineSpec
  , traverseStateSpec
  , traverseRegionSpec
  , traverseMachineSpec
  ) where

import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)
import qualified Data.Text as T

import GHC.Generics (Generic)

import Data.Aeson (FromJSON(..), ToJSON(..), Value(String), withText)

import Hfsm.Core.Codec (CodecSet)
import Hfsm.Core.Meta (MetaSpec, emptyMetaSpec, mergeMetaSpec)
import Hfsm.Core.Name (MachineName, RegionName, StateName)
import Hfsm.Core.Version (MachineVersion)
import Hfsm.Spec.Case (CaseSpec, EntryFn)
import Hfsm.Spec.Route (RouteSpec, mapRouteSpec, traverseRouteSpec)


data StateKind
  = AtomicSk
  | CompositeSk
  | TerminalSk
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)

data RegionSpec st sg hf ctx cmd child = RegionSpec
  { name :: Maybe RegionName
  , initial :: st
  , states :: [StateSpec st sg hf ctx cmd child]
  , meta :: MetaSpec
  }
  deriving stock (Generic)

data StateSpec st sg hf ctx cmd child = StateSpec
  { key :: st
  , name :: StateName
  , kind :: StateKind
  , entry :: Maybe (EntryFn ctx hf cmd)
  , cases :: [CaseSpec sg ctx hf cmd]
  , routes :: [RouteSpec st hf child]
  , child :: Maybe (RegionSpec st sg hf ctx cmd child)
  , meta :: MetaSpec
  }
  deriving stock (Generic)

data MachineSpec st sg hf ctx cmd child = MachineSpec
  { name :: MachineName
  , version :: MachineVersion
  , root :: RegionSpec st sg hf ctx cmd child
  , codecs :: CodecSet ctx sg cmd
  , meta :: MetaSpec
  }
  deriving stock (Generic)

instance ToJSON StateKind where
  toJSON = String . renderStateKind

instance FromJSON StateKind where
  parseJSON =
    withText "StateKind" $ \txt ->
      either (fail . T.unpack) pure (parseStateKind txt)

instance (Show st, Show hf, Show child) => Show (RegionSpec st sg hf ctx cmd child) where
  show regionSp =
    "RegionSpec {name = " <> show regionSp.name <>
    ", initial = " <> show regionSp.initial <>
    ", states = " <> show regionSp.states <>
    ", meta = " <> show regionSp.meta <> "}"

instance (Show st, Show hf, Show child) => Show (StateSpec st sg hf ctx cmd child) where
  show stateSp =
    "StateSpec {key = " <> show stateSp.key <>
    ", name = " <> show stateSp.name <>
    ", kind = " <> show stateSp.kind <>
    ", entry = " <> showEntry stateSp.entry <>
    ", cases = " <> show stateSp.cases <>
    ", routes = " <> show stateSp.routes <>
    ", child = " <> show stateSp.child <>
    ", meta = " <> show stateSp.meta <> "}"

instance (Show st, Show hf, Show child) => Show (MachineSpec st sg hf ctx cmd child) where
  show machineSp =
    "MachineSpec {name = " <> show machineSp.name <>
    ", version = " <> show machineSp.version <>
    ", root = " <> show machineSp.root <>
    ", codecs = <codec-set>" <>
    ", meta = " <> show machineSp.meta <> "}"

parseStateKind :: Text -> Either Text StateKind
parseStateKind raw =
  case normalizeStateKind raw of
    "atomic" -> Right AtomicSk
    "atomicsk" -> Right AtomicSk
    "composite" -> Right CompositeSk
    "compositesk" -> Right CompositeSk
    "terminal" -> Right TerminalSk
    "terminalsk" -> Right TerminalSk
    txt -> Left ("invalid state kind: " <> txt)

renderStateKind :: StateKind -> Text
renderStateKind kind =
  case kind of
    AtomicSk -> "atomic"
    CompositeSk -> "composite"
    TerminalSk -> "terminal"

mkRegionSpec :: st -> [StateSpec st sg hf ctx cmd child] -> RegionSpec st sg hf ctx cmd child
mkRegionSpec initial' states' =
  RegionSpec
    { name = Nothing
    , initial = initial'
    , states = states'
    , meta = emptyMetaSpec
    }

mkStateSpec :: st -> StateName -> StateKind -> StateSpec st sg hf ctx cmd child
mkStateSpec key' name' kind' =
  StateSpec
    { key = key'
    , name = name'
    , kind = kind'
    , entry = Nothing
    , cases = []
    , routes = []
    , child = Nothing
    , meta = emptyMetaSpec
    }

mkMachineSpec :: MachineName -> MachineVersion -> CodecSet ctx sg cmd -> RegionSpec st sg hf ctx cmd child -> MachineSpec st sg hf ctx cmd child
mkMachineSpec name' version' codecs' root' =
  MachineSpec
    { name = name'
    , version = version'
    , root = root'
    , codecs = codecs'
    , meta = emptyMetaSpec
    }

setRegionName :: Maybe RegionName -> RegionSpec st sg hf ctx cmd child -> RegionSpec st sg hf ctx cmd child
setRegionName name' regionSp =
  regionSp { name = name' }

clearRegionName :: RegionSpec st sg hf ctx cmd child -> RegionSpec st sg hf ctx cmd child
clearRegionName regionSp =
  regionSp { name = Nothing }

setRegionInitial :: st -> RegionSpec st sg hf ctx cmd child -> RegionSpec st sg hf ctx cmd child
setRegionInitial initial' regionSp =
  regionSp { initial = initial' }

setRegionStates :: [StateSpec st sg hf ctx cmd child] -> RegionSpec st sg hf ctx cmd child -> RegionSpec st sg hf ctx cmd child
setRegionStates states' regionSp =
  regionSp { states = states' }

addRegionState :: StateSpec st sg hf ctx cmd child -> RegionSpec st sg hf ctx cmd child -> RegionSpec st sg hf ctx cmd child
addRegionState stateSp regionSp =
  regionSp { states = regionSp.states <> [stateSp] }

setRegionMeta :: MetaSpec -> RegionSpec st sg hf ctx cmd child -> RegionSpec st sg hf ctx cmd child
setRegionMeta meta' regionSp =
  regionSp { meta = meta' }

mergeRegionMeta :: MetaSpec -> RegionSpec st sg hf ctx cmd child -> RegionSpec st sg hf ctx cmd child
mergeRegionMeta meta' regionSp =
  regionSp { meta = mergeMetaSpec regionSp.meta meta' }

setStateKind :: StateKind -> StateSpec st sg hf ctx cmd child -> StateSpec st sg hf ctx cmd child
setStateKind kind' stateSp =
  stateSp { kind = kind' }

setStateEntry :: EntryFn ctx hf cmd -> StateSpec st sg hf ctx cmd child -> StateSpec st sg hf ctx cmd child
setStateEntry entryFn stateSp =
  stateSp { entry = Just entryFn }

clearStateEntry :: StateSpec st sg hf ctx cmd child -> StateSpec st sg hf ctx cmd child
clearStateEntry stateSp =
  stateSp { entry = Nothing }

setStateCases :: [CaseSpec sg ctx hf cmd] -> StateSpec st sg hf ctx cmd child -> StateSpec st sg hf ctx cmd child
setStateCases cases' stateSp =
  stateSp { cases = cases' }

addStateCase :: CaseSpec sg ctx hf cmd -> StateSpec st sg hf ctx cmd child -> StateSpec st sg hf ctx cmd child
addStateCase caseSp stateSp =
  stateSp { cases = stateSp.cases <> [caseSp] }

setStateRoutes :: [RouteSpec st hf child] -> StateSpec st sg hf ctx cmd child -> StateSpec st sg hf ctx cmd child
setStateRoutes routes' stateSp =
  stateSp { routes = routes' }

addStateRoute :: RouteSpec st hf child -> StateSpec st sg hf ctx cmd child -> StateSpec st sg hf ctx cmd child
addStateRoute routeSp stateSp =
  stateSp { routes = stateSp.routes <> [routeSp] }

setStateChild :: RegionSpec st sg hf ctx cmd child -> StateSpec st sg hf ctx cmd child -> StateSpec st sg hf ctx cmd child
setStateChild childSp stateSp =
  stateSp { child = Just childSp }

clearStateChild :: StateSpec st sg hf ctx cmd child -> StateSpec st sg hf ctx cmd child
clearStateChild stateSp =
  stateSp { child = Nothing }

setStateMeta :: MetaSpec -> StateSpec st sg hf ctx cmd child -> StateSpec st sg hf ctx cmd child
setStateMeta meta' stateSp =
  stateSp { meta = meta' }

mergeStateMeta :: MetaSpec -> StateSpec st sg hf ctx cmd child -> StateSpec st sg hf ctx cmd child
mergeStateMeta meta' stateSp =
  stateSp { meta = mergeMetaSpec stateSp.meta meta' }

setMachineRoot :: RegionSpec st sg hf ctx cmd child -> MachineSpec st sg hf ctx cmd child -> MachineSpec st sg hf ctx cmd child
setMachineRoot root' machineSp =
  machineSp { root = root' }

setMachineMeta :: MetaSpec -> MachineSpec st sg hf ctx cmd child -> MachineSpec st sg hf ctx cmd child
setMachineMeta meta' machineSp =
  machineSp { meta = meta' }

mergeMachineMeta :: MetaSpec -> MachineSpec st sg hf ctx cmd child -> MachineSpec st sg hf ctx cmd child
mergeMachineMeta meta' machineSp =
  machineSp { meta = mergeMetaSpec machineSp.meta meta' }

stateIsAtomic :: StateSpec st sg hf ctx cmd child -> Bool
stateIsAtomic stateSp =
  stateSp.kind == AtomicSk

stateIsComposite :: StateSpec st sg hf ctx cmd child -> Bool
stateIsComposite stateSp =
  stateSp.kind == CompositeSk

stateIsTerminal :: StateSpec st sg hf ctx cmd child -> Bool
stateIsTerminal stateSp =
  stateSp.kind == TerminalSk

stateHasEntry :: StateSpec st sg hf ctx cmd child -> Bool
stateHasEntry stateSp =
  isJust stateSp.entry

stateHasChild :: StateSpec st sg hf ctx cmd child -> Bool
stateHasChild stateSp =
  isJust stateSp.child

stateCaseCount :: StateSpec st sg hf ctx cmd child -> Int
stateCaseCount stateSp =
  length stateSp.cases

stateRouteCount :: StateSpec st sg hf ctx cmd child -> Int
stateRouteCount stateSp =
  length stateSp.routes

regionStateCount :: RegionSpec st sg hf ctx cmd child -> Int
regionStateCount regionSp =
  length regionSp.states

regionHasInitial :: Eq st => RegionSpec st sg hf ctx cmd child -> Bool
regionHasInitial regionSp =
  any (\stateSp -> stateSp.key == regionSp.initial) regionSp.states

regionStateKeys :: RegionSpec st sg hf ctx cmd child -> [st]
regionStateKeys regionSp =
  fmap (.key) regionSp.states

regionStatesDeep :: RegionSpec st sg hf ctx cmd child -> [StateSpec st sg hf ctx cmd child]
regionStatesDeep regionSp =
  regionSp.states <> concatMap stateStatesDeep regionSp.states

findStateSpec :: Eq st => st -> RegionSpec st sg hf ctx cmd child -> Maybe (StateSpec st sg hf ctx cmd child)
findStateSpec key' regionSp =
  find (\stateSp -> stateSp.key == key') regionSp.states

findStateSpecDeep :: Eq st => st -> RegionSpec st sg hf ctx cmd child -> Maybe (StateSpec st sg hf ctx cmd child)
findStateSpecDeep key' regionSp =
  case findStateSpec key' regionSp of
    Just stateSp -> Just stateSp
    Nothing -> firstJust (fmap (findInChild key') regionSp.states)
  where
  findInChild :: Eq st => st -> StateSpec st sg hf ctx cmd child -> Maybe (StateSpec st sg hf ctx cmd child)
  findInChild aKey stateSp =
    case stateSp.child of
      Nothing -> Nothing
      Just childSp -> findStateSpecDeep aKey childSp


mapStateSpec :: (st1 -> st2) -> (child1 -> child2) -> StateSpec st1 sg hf ctx cmd child1 -> StateSpec st2 sg hf ctx cmd child2
mapStateSpec mapState mapChild stateSp =
  stateSp
    { key = mapState stateSp.key
    , routes = fmap (mapRouteSpec mapState mapChild) stateSp.routes
    , child = fmap (mapRegionSpec mapState mapChild) stateSp.child
    }

mapRegionSpec :: (st1 -> st2) -> (child1 -> child2) -> RegionSpec st1 sg hf ctx cmd child1 -> RegionSpec st2 sg hf ctx cmd child2
mapRegionSpec mapState mapChild regionSp =
  regionSp
    { initial = mapState regionSp.initial
    , states = fmap (mapStateSpec mapState mapChild) regionSp.states
    }

mapMachineSpec :: (st1 -> st2) -> (child1 -> child2) -> MachineSpec st1 sg hf ctx cmd child1 -> MachineSpec st2 sg hf ctx cmd child2
mapMachineSpec mapState mapChild machineSp =
  machineSp { root = mapRegionSpec mapState mapChild machineSp.root }

traverseStateSpec :: Monad f => (st1 -> f st2) -> (child1 -> f child2) -> StateSpec st1 sg hf ctx cmd child1 -> f (StateSpec st2 sg hf ctx cmd child2)
traverseStateSpec mapState mapChild stateSp = do
  key' <- mapState stateSp.key
  routes' <- traverse (traverseRouteSpec mapState mapChild) stateSp.routes
  child' <- traverse (traverseRegionSpec mapState mapChild) stateSp.child
  pure stateSp { key = key', routes = routes', child = child' }

traverseRegionSpec :: Monad f => (st1 -> f st2) -> (child1 -> f child2) -> RegionSpec st1 sg hf ctx cmd child1 -> f (RegionSpec st2 sg hf ctx cmd child2)
traverseRegionSpec mapState mapChild regionSp = do
  initial' <- mapState regionSp.initial
  states' <- traverse (traverseStateSpec mapState mapChild) regionSp.states
  pure regionSp { initial = initial', states = states' }

traverseMachineSpec :: Monad f => (st1 -> f st2) -> (child1 -> f child2) -> MachineSpec st1 sg hf ctx cmd child1 -> f (MachineSpec st2 sg hf ctx cmd child2)
traverseMachineSpec mapState mapChild machineSp = do
  root' <- traverseRegionSpec mapState mapChild machineSp.root
  pure machineSp { root = root' }

showEntry :: Maybe (EntryFn ctx hf cmd) -> String
showEntry entryMb =
  case entryMb of
    Nothing -> "Nothing"
    Just _ -> "Just <entry-fn>"

stateStatesDeep :: StateSpec st sg hf ctx cmd child -> [StateSpec st sg hf ctx cmd child]
stateStatesDeep stateSp =
  case stateSp.child of
    Nothing -> []
    Just childSp -> regionStatesDeep childSp

firstJust :: [Maybe a] -> Maybe a
firstJust maybes =
  case maybes of
    [] -> Nothing
    Nothing : rest -> firstJust rest
    Just x : _ -> Just x

normalizeStateKind :: Text -> Text
normalizeStateKind = T.toLower . T.strip