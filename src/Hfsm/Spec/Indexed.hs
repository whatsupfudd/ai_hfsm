{-# LANGUAGE GADTs #-}
{-# LANGUAGE PolyKinds #-}
{-# LANGUAGE RankNTypes #-}

module Hfsm.Spec.Indexed
  ( LowerHandoff(..)
  , EntryFnIx
  , CaseFnIx
  , CaseSpecIx(..)
  , RouteSpecIx(..)
  , StateSpecIx(..)
  , SomeStateSpecIx(..)
  , RegionSpecIx(..)
  , MachineSpecIx(..)

  , mkLowerHandoff

  , lowerReactRezIx
  , lowerEntryFnIx
  , lowerCaseFnIx
  , lowerCaseSpecIx
  , lowerRouteSpecIx
  , lowerSomeStateSpecIx
  , lowerStateSpecIx
  , lowerRegionSpecIx
  , lowerMachineSpecIx

  , mkCaseSpecIx
  , namedCaseSpecIx
  , setCaseNameIx
  , clearCaseNameIx
  , setCaseMetaIx
  , mergeCaseMetaIx
  , matchesCaseIx
  , findCaseSpecIx
  , runEntryFnIx
  , runCaseFnIx
  , runCaseSpecIx

  , mkRouteSpecIx

  , mkStateSpecIx
  , setStateEntryIx
  , clearStateEntryIx
  , setStateCasesIx
  , addStateCaseIx
  , setStateRoutesIx
  , addStateRouteIx
  , setStateChildIx
  , clearStateChildIx
  , setStateMetaIx
  , mergeStateMetaIx
  , stateIsAtomicIx
  , stateIsCompositeIx
  , stateIsTerminalIx
  , stateHasEntryIx
  , stateHasChildIx
  , stateCaseCountIx
  , stateRouteCountIx

  , mkSomeStateSpecIx
  , someStateKeyIx
  , someStateNameIx

  , mkRegionSpecIx
  , setRegionNameIx
  , clearRegionNameIx
  , setRegionInitialIx
  , setRegionStatesIx
  , addRegionStateIx
  , setRegionMetaIx
  , mergeRegionMetaIx
  , regionStateCountIx
  , regionHasInitialIx
  , regionStateKeysIx
  , regionStatesDeepIx
  , findStateSpecIx
  , findStateSpecDeepIx

  , mkMachineSpecIx
  , setMachineRootIx
  , setMachineMetaIx
  , mergeMachineMetaIx
  ) where

import Data.List (find)
import Data.Maybe (isJust)
import Data.Text (Text)

import Hfsm.Core.Codec (CodecSet)
import Hfsm.Core.Meta (MetaSpec, emptyMetaSpec, mergeMetaSpec)
import Hfsm.Core.Name
  ( CaseName
  , MachineName
  , RegionName
  , StateName
  , caseNameText
  )
import Hfsm.Core.Version (MachineVersion)
import Hfsm.Spec.Case
  ( CaseFn
  , CaseSpec(..)
  , EntryFn
  , ReactEnv
  , ReactErr
  , ReactRez
  , mapHandoff
  , rejectErr
  )
import Hfsm.Spec.Def
  ( MachineSpec(..)
  , RegionSpec(..)
  , StateKind(..)
  , StateSpec(..)
  )
import Hfsm.Spec.Route
  ( ControlPlan
  , RouteSpec(..)
  , normalizeControlPlan
  )

newtype LowerHandoff hk hf = LowerHandoff
  { lower :: forall phase. hk phase -> hf
  }

type EntryFnIx phase ctx hk cmd =
  ReactEnv -> ctx -> Either ReactErr (ReactRez ctx (hk phase) cmd)

type CaseFnIx phase sgk ctx hk cmd =
  ReactEnv -> sgk phase -> ctx -> Either ReactErr (ReactRez ctx (hk phase) cmd)

data CaseSpecIx phase sg sgk hk ctx cmd = CaseSpecIx
  { name :: Maybe CaseName
  , match :: sg -> Maybe (sgk phase)
  , react :: CaseFnIx phase sgk ctx hk cmd
  , meta :: MetaSpec
  }

data RouteSpecIx phase st hk child = RouteSpecIx
  { on :: hk phase
  , plan :: ControlPlan st child
  , meta :: MetaSpec
  }

data StateSpecIx phase st sg sgk hk ctx cmd child = StateSpecIx
  { key :: st
  , name :: StateName
  , kind :: StateKind
  , entry :: Maybe (EntryFnIx phase ctx hk cmd)
  , cases :: [CaseSpecIx phase sg sgk hk ctx cmd]
  , routes :: [RouteSpecIx phase st hk child]
  , child :: Maybe (RegionSpecIx st sg sgk hk ctx cmd child)
  , meta :: MetaSpec
  }

data SomeStateSpecIx st sg sgk hk ctx cmd child where
  SomeStateSpecIx :: StateSpecIx phase st sg sgk hk ctx cmd child -> SomeStateSpecIx st sg sgk hk ctx cmd child

data RegionSpecIx st sg sgk hk ctx cmd child = RegionSpecIx
  { name :: Maybe RegionName
  , initial :: st
  , states :: [SomeStateSpecIx st sg sgk hk ctx cmd child]
  , meta :: MetaSpec
  }

data MachineSpecIx st sg sgk hk ctx cmd child = MachineSpecIx
  { name :: MachineName
  , version :: MachineVersion
  , root :: RegionSpecIx st sg sgk hk ctx cmd child
  , codecs :: CodecSet ctx sg cmd
  , meta :: MetaSpec
  }

mkLowerHandoff :: (forall phase. hk phase -> hf) -> LowerHandoff hk hf
mkLowerHandoff lowerFn = LowerHandoff { lower = lowerFn }

lowerReactRezIx :: LowerHandoff hk hf -> ReactRez ctx (hk phase) cmd -> ReactRez ctx hf cmd
lowerReactRezIx lowerH = mapHandoff (lower lowerH)

lowerEntryFnIx :: LowerHandoff hk hf -> EntryFnIx phase ctx hk cmd -> EntryFn ctx hf cmd
lowerEntryFnIx lowerH entryFn env ctx =
  fmap (lowerReactRezIx lowerH) (entryFn env ctx)

lowerCaseFnIx :: LowerHandoff hk hf -> (sg -> Maybe (sgk phase)) -> CaseFnIx phase sgk ctx hk cmd -> CaseFn sg ctx hf cmd
lowerCaseFnIx lowerH matchFn caseFn env sg ctx =
  case matchFn sg of
    Nothing -> Left (caseMismatchErrIx Nothing)
    Just sgIx -> fmap (lowerReactRezIx lowerH) (caseFn env sgIx ctx)

lowerCaseSpecIx :: LowerHandoff hk hf -> CaseSpecIx phase sg sgk hk ctx cmd -> CaseSpec sg ctx hf cmd
lowerCaseSpecIx lowerH caseIx =
  CaseSpec
    { name = caseIx.name
    , match = \sg -> matchesCaseIx sg caseIx
    , react = \env sg ctx ->
        case caseIx.match sg of
          Nothing -> Left (caseMismatchErrIx caseIx.name)
          Just sgIx -> fmap (lowerReactRezIx lowerH) (caseIx.react env sgIx ctx)
    , meta = caseIx.meta
    }

lowerRouteSpecIx :: LowerHandoff hk hf -> RouteSpecIx phase st hk child -> RouteSpec st hf child
lowerRouteSpecIx lowerH routeIx =
  RouteSpec
    { on = lower lowerH routeIx.on
    , plan = normalizeControlPlan routeIx.plan
    , meta = routeIx.meta
    }

lowerSomeStateSpecIx :: LowerHandoff hk hf -> SomeStateSpecIx st sg sgk hk ctx cmd child -> StateSpec st sg hf ctx cmd child
lowerSomeStateSpecIx lowerH someStateIx =
  case someStateIx of
    SomeStateSpecIx stateIx -> lowerStateSpecIx lowerH stateIx

lowerStateSpecIx :: LowerHandoff hk hf -> StateSpecIx phase st sg sgk hk ctx cmd child -> StateSpec st sg hf ctx cmd child
lowerStateSpecIx lowerH stateIx =
  StateSpec
    { key = stateIx.key
    , name = stateIx.name
    , kind = stateIx.kind
    , entry = fmap (lowerEntryFnIx lowerH) stateIx.entry
    , cases = fmap (lowerCaseSpecIx lowerH) stateIx.cases
    , routes = fmap (lowerRouteSpecIx lowerH) stateIx.routes
    , child = fmap (lowerRegionSpecIx lowerH) stateIx.child
    , meta = stateIx.meta
    }

lowerRegionSpecIx :: LowerHandoff hk hf -> RegionSpecIx st sg sgk hk ctx cmd child -> RegionSpec st sg hf ctx cmd child
lowerRegionSpecIx lowerH regionIx =
  RegionSpec
    { name = regionIx.name
    , initial = regionIx.initial
    , states = fmap (lowerSomeStateSpecIx lowerH) regionIx.states
    , meta = regionIx.meta
    }

lowerMachineSpecIx :: LowerHandoff hk hf -> MachineSpecIx st sg sgk hk ctx cmd child -> MachineSpec st sg hf ctx cmd child
lowerMachineSpecIx lowerH machineIx =
  MachineSpec
    { name = machineIx.name
    , version = machineIx.version
    , root = lowerRegionSpecIx lowerH machineIx.root
    , codecs = machineIx.codecs
    , meta = machineIx.meta
    }

mkCaseSpecIx :: (sg -> Maybe (sgk phase)) -> CaseFnIx phase sgk ctx hk cmd -> CaseSpecIx phase sg sgk hk ctx cmd
mkCaseSpecIx matchFn reactFn =
  CaseSpecIx
    { name = Nothing
    , match = matchFn
    , react = reactFn
    , meta = emptyMetaSpec
    }

namedCaseSpecIx :: CaseName -> (sg -> Maybe (sgk phase)) -> CaseFnIx phase sgk ctx hk cmd -> CaseSpecIx phase sg sgk hk ctx cmd
namedCaseSpecIx caseName matchFn reactFn =
  CaseSpecIx
    { name = Just caseName
    , match = matchFn
    , react = reactFn
    , meta = emptyMetaSpec
    }

setCaseNameIx :: CaseName -> CaseSpecIx phase sg sgk hk ctx cmd -> CaseSpecIx phase sg sgk hk ctx cmd
setCaseNameIx caseName caseIx = caseIx { name = Just caseName }

clearCaseNameIx :: CaseSpecIx phase sg sgk hk ctx cmd -> CaseSpecIx phase sg sgk hk ctx cmd
clearCaseNameIx caseIx = caseIx { name = Nothing }

setCaseMetaIx :: MetaSpec -> CaseSpecIx phase sg sgk hk ctx cmd -> CaseSpecIx phase sg sgk hk ctx cmd
setCaseMetaIx metaSpec caseIx = caseIx { meta = metaSpec }

mergeCaseMetaIx :: MetaSpec -> CaseSpecIx phase sg sgk hk ctx cmd -> CaseSpecIx phase sg sgk hk ctx cmd
mergeCaseMetaIx metaSpec caseIx = caseIx { meta = mergeMetaSpec caseIx.meta metaSpec }

matchesCaseIx :: sg -> CaseSpecIx phase sg sgk hk ctx cmd -> Bool
matchesCaseIx sg caseIx = isJust (caseIx.match sg)

findCaseSpecIx :: sg -> [CaseSpecIx phase sg sgk hk ctx cmd] -> Maybe (CaseSpecIx phase sg sgk hk ctx cmd)
findCaseSpecIx sg = find (matchesCaseIx sg)

runEntryFnIx :: ReactEnv -> ctx -> EntryFnIx phase ctx hk cmd -> Either ReactErr (ReactRez ctx (hk phase) cmd)
runEntryFnIx env ctx entryFn = entryFn env ctx

runCaseFnIx :: ReactEnv -> sgk phase -> ctx -> CaseFnIx phase sgk ctx hk cmd -> Either ReactErr (ReactRez ctx (hk phase) cmd)
runCaseFnIx env sgIx ctx caseFn = caseFn env sgIx ctx

runCaseSpecIx :: ReactEnv -> sg -> ctx -> CaseSpecIx phase sg sgk hk ctx cmd -> Either ReactErr (ReactRez ctx (hk phase) cmd)
runCaseSpecIx env sg ctx caseIx =
  case caseIx.match sg of
    Nothing -> Left (caseMismatchErrIx caseIx.name)
    Just sgIx -> caseIx.react env sgIx ctx

mkRouteSpecIx :: hk phase -> ControlPlan st child -> RouteSpecIx phase st hk child
mkRouteSpecIx handoff plan' =
  RouteSpecIx
    { on = handoff
    , plan = normalizeControlPlan plan'
    , meta = emptyMetaSpec
    }

mkStateSpecIx :: st -> StateName -> StateKind -> StateSpecIx phase st sg sgk hk ctx cmd child
mkStateSpecIx key' stateName kind' =
  StateSpecIx
    { key = key'
    , name = stateName
    , kind = kind'
    , entry = Nothing
    , cases = []
    , routes = []
    , child = Nothing
    , meta = emptyMetaSpec
    }

setStateEntryIx :: EntryFnIx phase ctx hk cmd -> StateSpecIx phase st sg sgk hk ctx cmd child -> StateSpecIx phase st sg sgk hk ctx cmd child
setStateEntryIx entryFn stateIx = stateIx { entry = Just entryFn }

clearStateEntryIx :: StateSpecIx phase st sg sgk hk ctx cmd child -> StateSpecIx phase st sg sgk hk ctx cmd child
clearStateEntryIx stateIx = stateIx { entry = Nothing }

setStateCasesIx :: [CaseSpecIx phase sg sgk hk ctx cmd] -> StateSpecIx phase st sg sgk hk ctx cmd child -> StateSpecIx phase st sg sgk hk ctx cmd child
setStateCasesIx casesIx stateIx = stateIx { cases = casesIx }

addStateCaseIx :: CaseSpecIx phase sg sgk hk ctx cmd -> StateSpecIx phase st sg sgk hk ctx cmd child -> StateSpecIx phase st sg sgk hk ctx cmd child
addStateCaseIx caseIx stateIx = stateIx { cases = stateIx.cases <> [caseIx] }

setStateRoutesIx :: [RouteSpecIx phase st hk child] -> StateSpecIx phase st sg sgk hk ctx cmd child -> StateSpecIx phase st sg sgk hk ctx cmd child
setStateRoutesIx routesIx stateIx = stateIx { routes = fmap normalizeRouteSpecIx routesIx }

addStateRouteIx :: RouteSpecIx phase st hk child -> StateSpecIx phase st sg sgk hk ctx cmd child -> StateSpecIx phase st sg sgk hk ctx cmd child
addStateRouteIx routeIx stateIx = stateIx { routes = stateIx.routes <> [normalizeRouteSpecIx routeIx] }

setStateChildIx :: RegionSpecIx st sg sgk hk ctx cmd child -> StateSpecIx phase st sg sgk hk ctx cmd child -> StateSpecIx phase st sg sgk hk ctx cmd child
setStateChildIx childRegion stateIx = stateIx { child = Just childRegion }

clearStateChildIx :: StateSpecIx phase st sg sgk hk ctx cmd child -> StateSpecIx phase st sg sgk hk ctx cmd child
clearStateChildIx stateIx = stateIx { child = Nothing }

setStateMetaIx :: MetaSpec -> StateSpecIx phase st sg sgk hk ctx cmd child -> StateSpecIx phase st sg sgk hk ctx cmd child
setStateMetaIx metaSpec stateIx = stateIx { meta = metaSpec }

mergeStateMetaIx :: MetaSpec -> StateSpecIx phase st sg sgk hk ctx cmd child -> StateSpecIx phase st sg sgk hk ctx cmd child
mergeStateMetaIx metaSpec stateIx = stateIx { meta = mergeMetaSpec stateIx.meta metaSpec }

stateIsAtomicIx :: StateSpecIx phase st sg sgk hk ctx cmd child -> Bool
stateIsAtomicIx stateIx = stateIx.kind == AtomicSk

stateIsCompositeIx :: StateSpecIx phase st sg sgk hk ctx cmd child -> Bool
stateIsCompositeIx stateIx = stateIx.kind == CompositeSk

stateIsTerminalIx :: StateSpecIx phase st sg sgk hk ctx cmd child -> Bool
stateIsTerminalIx stateIx = stateIx.kind == TerminalSk

stateHasEntryIx :: StateSpecIx phase st sg sgk hk ctx cmd child -> Bool
stateHasEntryIx stateIx = isJust stateIx.entry

stateHasChildIx :: StateSpecIx phase st sg sgk hk ctx cmd child -> Bool
stateHasChildIx stateIx = isJust stateIx.child

stateCaseCountIx :: StateSpecIx phase st sg sgk hk ctx cmd child -> Int
stateCaseCountIx stateIx = length stateIx.cases

stateRouteCountIx :: StateSpecIx phase st sg sgk hk ctx cmd child -> Int
stateRouteCountIx stateIx = length stateIx.routes

mkSomeStateSpecIx :: StateSpecIx phase st sg sgk hk ctx cmd child -> SomeStateSpecIx st sg sgk hk ctx cmd child
mkSomeStateSpecIx = SomeStateSpecIx

someStateKeyIx :: SomeStateSpecIx st sg sgk hk ctx cmd child -> st
someStateKeyIx someStateIx =
  case someStateIx of
    SomeStateSpecIx stateIx -> stateIx.key

someStateNameIx :: SomeStateSpecIx st sg sgk hk ctx cmd child -> StateName
someStateNameIx someStateIx =
  case someStateIx of
    SomeStateSpecIx stateIx -> stateIx.name

mkRegionSpecIx :: st -> [SomeStateSpecIx st sg sgk hk ctx cmd child] -> RegionSpecIx st sg sgk hk ctx cmd child
mkRegionSpecIx initialState statesIx =
  RegionSpecIx
    { name = Nothing
    , initial = initialState
    , states = statesIx
    , meta = emptyMetaSpec
    }

setRegionNameIx :: Maybe RegionName -> RegionSpecIx st sg sgk hk ctx cmd child -> RegionSpecIx st sg sgk hk ctx cmd child
setRegionNameIx regionName regionIx = regionIx { name = regionName }

clearRegionNameIx :: RegionSpecIx st sg sgk hk ctx cmd child -> RegionSpecIx st sg sgk hk ctx cmd child
clearRegionNameIx regionIx = regionIx { name = Nothing }

setRegionInitialIx :: st -> RegionSpecIx st sg sgk hk ctx cmd child -> RegionSpecIx st sg sgk hk ctx cmd child
setRegionInitialIx initialState regionIx = regionIx { initial = initialState }

setRegionStatesIx :: [SomeStateSpecIx st sg sgk hk ctx cmd child] -> RegionSpecIx st sg sgk hk ctx cmd child -> RegionSpecIx st sg sgk hk ctx cmd child
setRegionStatesIx statesIx regionIx = regionIx { states = statesIx }

addRegionStateIx :: SomeStateSpecIx st sg sgk hk ctx cmd child -> RegionSpecIx st sg sgk hk ctx cmd child -> RegionSpecIx st sg sgk hk ctx cmd child
addRegionStateIx stateIx regionIx = regionIx { states = regionIx.states <> [stateIx] }

setRegionMetaIx :: MetaSpec -> RegionSpecIx st sg sgk hk ctx cmd child -> RegionSpecIx st sg sgk hk ctx cmd child
setRegionMetaIx metaSpec regionIx = regionIx { meta = metaSpec }

mergeRegionMetaIx :: MetaSpec -> RegionSpecIx st sg sgk hk ctx cmd child -> RegionSpecIx st sg sgk hk ctx cmd child
mergeRegionMetaIx metaSpec regionIx = regionIx { meta = mergeMetaSpec regionIx.meta metaSpec }

regionStateCountIx :: RegionSpecIx st sg sgk hk ctx cmd child -> Int
regionStateCountIx regionIx = length regionIx.states

regionHasInitialIx :: Eq st => RegionSpecIx st sg sgk hk ctx cmd child -> Bool
regionHasInitialIx regionIx = any ((== regionIx.initial) . someStateKeyIx) regionIx.states

regionStateKeysIx :: RegionSpecIx st sg sgk hk ctx cmd child -> [st]
regionStateKeysIx regionIx = fmap someStateKeyIx regionIx.states

regionStatesDeepIx :: RegionSpecIx st sg sgk hk ctx cmd child -> [SomeStateSpecIx st sg sgk hk ctx cmd child]
regionStatesDeepIx regionIx = concatMap flattenSomeStateIx regionIx.states

findStateSpecIx :: Eq st => st -> RegionSpecIx st sg sgk hk ctx cmd child -> Maybe (SomeStateSpecIx st sg sgk hk ctx cmd child)
findStateSpecIx key' regionIx = find (\someStateIx -> someStateKeyIx someStateIx == key') regionIx.states

findStateSpecDeepIx :: Eq st => st -> RegionSpecIx st sg sgk hk ctx cmd child -> Maybe (SomeStateSpecIx st sg sgk hk ctx cmd child)
findStateSpecDeepIx key' regionIx = find (\someStateIx -> someStateKeyIx someStateIx == key') (regionStatesDeepIx regionIx)

mkMachineSpecIx :: MachineName -> MachineVersion -> CodecSet ctx sg cmd -> RegionSpecIx st sg sgk hk ctx cmd child -> MachineSpecIx st sg sgk hk ctx cmd child
mkMachineSpecIx machineName machineVersion codecsSet rootRegion =
  MachineSpecIx
    { name = machineName
    , version = machineVersion
    , root = rootRegion
    , codecs = codecsSet
    , meta = emptyMetaSpec
    }

setMachineRootIx :: RegionSpecIx st sg sgk hk ctx cmd child -> MachineSpecIx st sg sgk hk ctx cmd child -> MachineSpecIx st sg sgk hk ctx cmd child
setMachineRootIx rootRegion machineIx = machineIx { root = rootRegion }

setMachineMetaIx :: MetaSpec -> MachineSpecIx st sg sgk hk ctx cmd child -> MachineSpecIx st sg sgk hk ctx cmd child
setMachineMetaIx metaSpec machineIx = machineIx { meta = metaSpec }

mergeMachineMetaIx :: MetaSpec -> MachineSpecIx st sg sgk hk ctx cmd child -> MachineSpecIx st sg sgk hk ctx cmd child
mergeMachineMetaIx metaSpec machineIx = machineIx { meta = mergeMetaSpec machineIx.meta metaSpec }

normalizeRouteSpecIx :: RouteSpecIx phase st hk child -> RouteSpecIx phase st hk child
normalizeRouteSpecIx routeIx = routeIx { plan = normalizeControlPlan routeIx.plan }

flattenSomeStateIx :: SomeStateSpecIx st sg sgk hk ctx cmd child -> [SomeStateSpecIx st sg sgk hk ctx cmd child]
flattenSomeStateIx someStateIx =
  case someStateIx of
    SomeStateSpecIx stateIx ->
      someStateIx :
      case stateIx.child of
        Nothing -> []
        Just childRegion -> regionStatesDeepIx childRegion

caseMismatchErrIx :: Maybe CaseName -> ReactErr
caseMismatchErrIx maybeCaseName = rejectErr (caseMismatchMsgIx maybeCaseName)

caseMismatchMsgIx :: Maybe CaseName -> Text
caseMismatchMsgIx maybeCaseName =
  case maybeCaseName of
    Nothing -> "indexed case mismatch"
    Just caseName -> "indexed case mismatch for case \"" <> caseNameText caseName <> "\""