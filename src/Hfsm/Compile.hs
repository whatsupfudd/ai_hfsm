{-# LANGUAGE BangPatterns #-}
{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE StrictData #-}
{-# LANGUAGE RankNTypes #-}

module Hfsm.Compile
  ( CompiledMachine(..)
  , CompileErr(..)
  , compile
  , compileMany
  ) where

import Control.DeepSeq (NFData)
import Control.Monad (foldM, forM, unless, when)
import Control.Monad.State.Strict (StateT, gets, modify', runStateT)
import Control.Monad.Trans.Class (lift)

import Data.Bifunctor (first)
import Data.Bits (xor)
import qualified Data.ByteString as BS
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (sortOn)
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import Data.Maybe (isNothing, isJust)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import Numeric (showHex)

import GHC.Generics (Generic)

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM

import Hfsm.Compile.Exec (ExecTable)
import qualified Hfsm.Compile.Exec as Exec
import Hfsm.Core.Digest
  ( GraphDigest
  , SpecDigest
  , digestLen
  , graphDigestText
  , mkGraphDigest
  , mkSpecDigest
  , renderDigestErr
  )
import Hfsm.Core.Meta (MetaSpec(..), emptyMetaSpec)
import Hfsm.Core.Name
  ( JoinName
  , RegionName
  , StateName
  , TimerName
  , caseNameText
  , joinNameText
  , machineNameText
  , regionNameText
  , stateNameText
  , timerNameText
  )
import Hfsm.Core.Path
  ( RegionPath
  , StatePath
  , appendRegionRef
  , appendStateRef
  , emptyRegionPath
  , emptyStatePath
  , regionPathToList
  , statePathToList
  )
import Hfsm.Core.Ref
  ( CaseRef
  , JoinRef
  , RegionRef
  , RouteRef
  , StateRef
  , TimerRef
  , caseRefFromInteger
  , caseRefWord32
  , joinRefFromInteger
  , joinRefWord32
  , regionRefFromInteger
  , regionRefWord32
  , renderRefErr
  , routeRefFromInteger
  , routeRefWord32
  , stateRefFromInteger
  , stateRefWord32
  , timerRefFromInteger
  , timerRefWord32
  )
import Hfsm.Core.Version (machineVersionText)
import Hfsm.Graph.Def
  ( BreakPlanG(..)
  , CaseNd(..)
  , JoinMode(..)
  , JoinNd(..)
  , MachineGraph(..)
  , RegionNd(..)
  , RouteEd(..)
  , RoutePlan(..)
  , RouteTarget(..)
  , SpawnPlanG(..)
  , StateNd(..)
  , TimerNd(..)
  , TimerPlanG(..)
  , WaitPlanG(..)
  )
import qualified Hfsm.Graph.Def as Graph
import Hfsm.Spec.Case (CaseSpec(..), EntryFn)
import qualified Hfsm.Spec.Case as Case
import Hfsm.Spec.Def
  ( MachineSpec(..)
  , RegionSpec(..)
  , StateKind(..)
  , StateSpec(..)
  )
import Hfsm.Spec.Route (
      ControlPlan (..), RouteSpec(..), JoinPlan(..), TimerPlan(..), TargetPlan(..), WaitPlan (..), SpawnPlan (..)
    , BreakPlan (..), RouteErr, renderRouteErr, validateRouteSpec, normalizeControlPlan
  )


data CompiledMachine st sg hf ctx cmd child = CompiledMachine
  { spec :: MachineSpec st sg hf ctx cmd child
  , graph :: MachineGraph
  , exec :: ExecTable sg ctx hf cmd
  , digest :: SpecDigest
  }

data CompileErr
  = DuplicateStateEr Text
  | DuplicateRegionEr Text
  | DuplicateCaseEr Text
  | DuplicateRouteEr Text
  | DuplicateJoinEr Text
  | DuplicateTimerEr Text
  | MissingInitialEr Text
  | MissingTargetEr Text
  | MissingRouteEr Text
  | InvalidCompositeEr StateRef Text
  | InvalidTerminalEr StateRef Text
  | LoweringEr Text
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data BuildSt st sg hf ctx cmd = BuildSt
  { nextRegion :: Integer
  , nextState :: Integer
  , nextCase :: Integer
  , nextRoute :: Integer
  , stateRefs :: Map st StateRef
  , exec :: ExecTable sg ctx hf cmd
  }

data LowerRegion st sg hf ctx cmd child = LowerRegion
  { ref :: RegionRef
  , parent :: Maybe StateRef
  , initial :: StateRef
  , states :: [LowerState st sg hf ctx cmd child]
  , path :: RegionPath
  , meta :: MetaSpec
  }

data LowerState st sg hf ctx cmd child = LowerState
  { ref :: StateRef
  , parent :: RegionRef
  , key :: st
  , name :: StateName
  , kind :: StateKind
  , entry :: Maybe (CaseRef, EntryFn ctx hf cmd)
  , cases :: [(CaseRef, CaseSpec sg ctx hf cmd)]
  , routes :: [(RouteRef, RouteSpec st hf child)]
  , child :: Maybe (LowerRegion st sg hf ctx cmd child)
  , path :: StatePath
  , meta :: MetaSpec
  }

data JoinAcc = JoinAcc
  { next :: Integer
  , defs :: Map JoinName JoinNd
  }

data TimerAcc = TimerAcc
  { next :: Integer
  , defs :: Map TimerName TimerNd
  }

compile :: (Ord st, Show st, Show hf, Show child, Show ctx, Show cmd) => MachineSpec st sg hf ctx cmd child -> Either CompileErr (CompiledMachine st sg hf ctx cmd child)
compile machineSpec = do
  blankDigest <- first (LoweringEr . renderDigestErr) (mkGraphDigest (T.replicate digestLen "0"))
  joinDefs <- collectJoinDefs machineSpec
  timerDefs <- collectTimerDefs machineSpec
  (rootRegion, buildSt) <- runStateT (lowerRegion Nothing emptyRegionPath emptyStatePath machineSpec.root) initBuildSt
  routeNds <- lowerRouteNds buildSt.stateRefs joinDefs.defs timerDefs.defs rootRegion

  let
    regionNds = lowerRegionNds rootRegion
    stateNds = lowerStateNds rootRegion
    caseNds = lowerCaseNds rootRegion
    graph0 =
      MachineGraph
        { machine = machineSpec.name
        , version = machineSpec.version
        , digest = blankDigest
        , root = rootRegion.ref
        , regions = intMapFromList (fromWord32 . regionRefWord32 . (.ref)) regionNds
        , states = intMapFromList (fromWord32 . stateRefWord32 . (.ref)) stateNds
        , cases = intMapFromList (fromWord32 . caseRefWord32 . (.ref)) caseNds
        , routes = intMapFromList (fromWord32 . routeRefWord32 . (.ref)) routeNds
        , joins = intMapFromList (fromWord32 . joinRefWord32 . (.ref)) (M.elems joinDefs.defs)
        , timers = intMapFromList (fromWord32 . timerRefWord32 . (.ref)) (M.elems timerDefs.defs)
        , meta = machineSpec.meta
        }

  (graph1, specDigest) <- finalizeDigests graph0
  pure CompiledMachine
    { spec = machineSpec
    , graph = graph1
    , exec = buildSt.exec
    , digest = specDigest
    }


compileMany :: (Ord st, Show st, Show hf, Show child, Show ctx, Show cmd) => [MachineSpec st sg hf ctx cmd child] -> Either CompileErr [CompiledMachine st sg hf ctx cmd child]
compileMany = traverse compile


initBuildSt :: BuildSt st sg hf ctx cmd
initBuildSt =
  BuildSt
    { nextRegion = 1
    , nextState = 1
    , nextCase = 1
    , nextRoute = 1
    , stateRefs = M.empty
    , exec = Exec.emptyExecTable
    }

collectJoinDefs :: MachineSpec st sg hf ctx cmd child -> Either CompileErr JoinAcc
collectJoinDefs machineSpec =
  foldM step initJoinAcc (routeSpecsDeep machineSpec.root)
  where
    initJoinAcc = JoinAcc { next = 1, defs = M.empty }

    step :: JoinAcc -> RouteSpec st sg hf -> Either CompileErr JoinAcc
    step acc routeSpec =
      case routeSpec.plan.join of
        JoinNoneJp -> Right acc
        JoinAllJp joinName -> ensureJoinDef joinName AllJm acc
        JoinAnyJp joinName -> ensureJoinDef joinName AnyJm acc
        JoinCountJp joinName count -> ensureJoinDef joinName (CountJm count) acc


collectTimerDefs :: MachineSpec st sg hf ctx cmd child -> Either CompileErr TimerAcc
collectTimerDefs machineSpec =
  foldM step initTimerAcc (routeSpecsDeep machineSpec.root)
  where
    initTimerAcc = TimerAcc { next = 1, defs = M.empty }

    step :: TimerAcc -> RouteSpec st sg hf -> Either CompileErr TimerAcc
    step acc routeSpec =
      foldM (flip ensureTimerDef) acc routeSpec.plan.timers

ensureJoinDef :: JoinName -> JoinMode -> JoinAcc -> Either CompileErr JoinAcc
ensureJoinDef joinName joinMode acc =
  case M.lookup joinName acc.defs of
    Nothing -> do
      joinRef <- first (LoweringEr . renderRefErr) (joinRefFromInteger acc.next)
      let joinNd =
            JoinNd
              { ref = joinRef
              , name = Just joinName
              , mode = joinMode
              , meta = emptyMetaSpec
              }
      pure (acc :: JoinAcc) { next = acc.next + 1, defs = M.insert joinName joinNd acc.defs }

    Just existing
      | existing.mode == joinMode ->
          Right acc
      | otherwise ->
          Left $
            DuplicateJoinEr $
              "join " <> quote (joinNameText joinName) <>
              " is declared with conflicting modes: " <>
              renderJoinMode existing.mode <> " vs " <> renderJoinMode joinMode

ensureTimerDef :: TimerPlan -> TimerAcc -> Either CompileErr TimerAcc
ensureTimerDef timerPlan acc =
  case M.lookup timerPlan.key acc.defs of
    Just _ ->
      Right acc

    Nothing -> do
      timerRef <- first (LoweringEr . renderRefErr) (timerRefFromInteger acc.next)
      let timerNd =
            TimerNd
              { ref = timerRef
              , name = Just timerPlan.key
              , meta = emptyMetaSpec
              }
      pure (acc :: TimerAcc) { next = acc.next + 1, defs = M.insert timerPlan.key timerNd acc.defs }

lowerRegion ::
  (Ord st, Show st, Show hf, Show child) =>
  Maybe StateRef ->
  RegionPath ->
  StatePath ->
  RegionSpec st sg hf ctx cmd child ->
  StateT (BuildSt st sg hf ctx cmd) (Either CompileErr) (LowerRegion st sg hf ctx cmd child)
lowerRegion parentRef regionPrefix statePrefix regionSpec = do
  regionRef <- freshRegionRef
  let regionPath = appendRegionRef regionRef regionPrefix
  stateNds <- traverse (lowerState regionRef regionPath statePrefix) regionSpec.states

  let directRefs = M.fromList [(stateNd.key, stateNd.ref) | stateNd <- stateNds]
  initialRef <-
    case M.lookup regionSpec.initial directRefs of
      Nothing ->
        lift $
          Left $
            MissingInitialEr $
              "region " <> quote (renderRegionLabel regionSpec.name parentRef) <>
              " has initial state key " <> quote (renderAuthored regionSpec.initial) <>
              " that is not one of its direct states"
      Just x ->
        pure x

  pure LowerRegion
    { ref = regionRef
    , parent = parentRef
    , initial = initialRef
    , states = stateNds
    , path = regionPath
    , meta = regionSpec.meta
    }

lowerState ::
  (Ord st, Show st, Show hf, Show child) =>
  RegionRef ->
  RegionPath ->
  StatePath ->
  StateSpec st sg hf ctx cmd child ->
  StateT (BuildSt st sg hf ctx cmd) (Either CompileErr) (LowerState st sg hf ctx cmd child)
lowerState regionRef regionPath statePrefix stateSpec = do
  stateRef <- freshStateRef
  registerStateKey stateSpec.key stateRef

  let statePath = appendStateRef stateRef statePrefix

  casePairs <- lowerCases stateRef stateSpec
  routePairs <- lowerRoutes stateSpec
  entryPair <- lowerEntry stateRef stateSpec.entry
  childRegion <- traverse (lowerRegion (Just stateRef) regionPath statePath) stateSpec.child

  checkStateShape stateRef stateSpec childRegion

  pure LowerState
    { ref = stateRef
    , parent = regionRef
    , key = stateSpec.key
    , name = stateSpec.name
    , kind = stateSpec.kind
    , entry = entryPair
    , cases = casePairs
    , routes = routePairs
    , child = childRegion
    , path = statePath
    , meta = stateSpec.meta
    }

lowerEntry :: StateRef -> Maybe (EntryFn ctx hf cmd) ->
              StateT (BuildSt st sg hf ctx cmd) (Either CompileErr) (Maybe (CaseRef, EntryFn ctx hf cmd))
lowerEntry st0 Nothing = pure Nothing
lowerEntry stateRef (Just entryFn) = do
  caseRef <- freshCaseRef
  modify' $ \st -> (st :: BuildSt st sg hf ctx cmd) { exec = Exec.insertEntryFn stateRef caseRef entryFn st.exec }
  pure (Just (caseRef, entryFn))


lowerCases :: StateRef -> StateSpec st sg hf ctx cmd child ->
              StateT (BuildSt st sg hf ctx cmd) (Either CompileErr) [(CaseRef, CaseSpec sg ctx hf cmd)]
lowerCases stateRef stateSpec = do
  ensureUniqueCaseNames stateSpec.name stateSpec.cases
  forM stateSpec.cases $ \caseSpec -> do
    caseRef <- freshCaseRef
    modify' $ \st -> (st :: BuildSt st sg hf ctx cmd) { exec = Exec.insertCaseSpec stateRef caseRef caseSpec st.exec }
    pure (caseRef, caseSpec)

lowerRoutes ::
  Show hf =>
  StateSpec st sg hf ctx cmd child ->
  StateT (BuildSt st sg hf ctx cmd) (Either CompileErr) [(RouteRef, RouteSpec st hf child)]
lowerRoutes stateSpec = do
  let routeSpecs = fmap normalizedRouteSpec stateSpec.routes
  ensureValidRoutes stateSpec.name routeSpecs
  ensureUniqueRouteHandoffs stateSpec.name routeSpecs
  forM routeSpecs $ \routeSpec -> do
    routeRef <- freshRouteRef
    pure (routeRef, routeSpec)

checkStateShape ::
  StateRef ->
  StateSpec st sg hf ctx cmd child ->
  Maybe (LowerRegion st sg hf ctx cmd child) ->
  StateT (BuildSt st sg hf ctx cmd) (Either CompileErr) ()
checkStateShape stateRef stateSpec childRegion =
  case stateSpec.kind of
    CompositeSk ->
      when (isNothing childRegion) $
        lift $
          Left $
            InvalidCompositeEr stateRef $
              "composite state " <> quote (stateNameText stateSpec.name) <> " must define a child region"

    AtomicSk ->
      when (isJust childRegion) $
        lift $
          Left $
            InvalidCompositeEr stateRef $
              "atomic state " <> quote (stateNameText stateSpec.name) <> " cannot define a child region"

    TerminalSk ->
      when terminalInvalid $
        lift $
          Left $
            InvalidTerminalEr stateRef $
              "terminal state " <> quote (stateNameText stateSpec.name) <>
              " cannot define entry logic, signal cases, routes, or a child region"
  where
    terminalInvalid =
      isJust stateSpec.entry ||
      not (null stateSpec.cases) ||
      not (null stateSpec.routes) ||
      isJust childRegion

registerStateKey ::
  (Ord st, Show st) =>
  st ->
  StateRef ->
  StateT (BuildSt st sg hf ctx cmd) (Either CompileErr) ()
registerStateKey stateKey stateRef = do
  seen <- gets (.stateRefs)
  case M.lookup stateKey seen of
    Nothing ->
      modify' $ \st -> st { stateRefs = M.insert stateKey stateRef st.stateRefs }

    Just existing ->
      lift $
        Left $
          DuplicateStateEr $
            "duplicate state key " <> quote (renderAuthored stateKey) <>
            " resolves both to " <> renderStateRef existing <> " and " <> renderStateRef stateRef

ensureUniqueCaseNames ::
  StateName ->
  [CaseSpec sg ctx hf cmd] ->
  StateT (BuildSt st sg hf ctx cmd) (Either CompileErr) ()
ensureUniqueCaseNames stateName caseSpecs =
  unless (null dupNames) $
    lift $
      Left $
        DuplicateCaseEr $
          "state " <> quote (stateNameText stateName) <>
          " has duplicate case names: " <> renderTextList dupNames
  where
    names = [caseNameText caseName | caseSpec <- caseSpecs, Just caseName <- [caseSpec.name]]
    dupNames = duplicatesOrd names

ensureValidRoutes ::
  Show hf =>
  StateName ->
  [RouteSpec st hf child] ->
  StateT (BuildSt st sg hf ctx cmd) (Either CompileErr) ()
ensureValidRoutes stateName routeSpecs =
  case firstRouteError routeSpecs of
    Nothing ->
      pure ()

    Just (handoffTxt, routeErrs) ->
      lift $
        Left $
          LoweringEr $
            "route for state " <> quote (stateNameText stateName) <>
            " and handoff " <> quote handoffTxt <>
            " is invalid: " <> T.intercalate "; " (fmap renderRouteErr routeErrs)

ensureUniqueRouteHandoffs ::
  Show hf =>
  StateName ->
  [RouteSpec st hf child] ->
  StateT (BuildSt st sg hf ctx cmd) (Either CompileErr) ()
ensureUniqueRouteHandoffs stateName routeSpecs =
  unless (null dupHandoffs) $
    lift $
      Left $
        DuplicateRouteEr $
          "state " <> quote (stateNameText stateName) <>
          " has duplicate route handoffs: " <> renderTextList dupHandoffs
  where
    handoffs = fmap (renderHandoff . (.on)) routeSpecs
    dupHandoffs = duplicatesOrd handoffs

freshRegionRef :: StateT (BuildSt st sg hf ctx cmd) (Either CompileErr) RegionRef
freshRegionRef = do
  n <- gets (.nextRegion)
  ref <- lift (first (LoweringEr . renderRefErr) (regionRefFromInteger n))
  modify' $ \st -> st { nextRegion = n + 1 }
  pure ref

freshStateRef :: StateT (BuildSt st sg hf ctx cmd) (Either CompileErr) StateRef
freshStateRef = do
  n <- gets (.nextState)
  ref <- lift (first (LoweringEr . renderRefErr) (stateRefFromInteger n))
  modify' $ \st -> st { nextState = n + 1 }
  pure ref

freshCaseRef :: StateT (BuildSt st sg hf ctx cmd) (Either CompileErr) CaseRef
freshCaseRef = do
  n <- gets (.nextCase)
  ref <- lift (first (LoweringEr . renderRefErr) (caseRefFromInteger n))
  modify' $ \st -> st { nextCase = n + 1 }
  pure ref

freshRouteRef :: StateT (BuildSt st sg hf ctx cmd) (Either CompileErr) RouteRef
freshRouteRef = do
  n <- gets (.nextRoute)
  ref <- lift (first (LoweringEr . renderRefErr) (routeRefFromInteger n))
  modify' $ \st -> st { nextRoute = n + 1 }
  pure ref

lowerRegionNds :: LowerRegion st sg hf ctx cmd child -> [RegionNd]
lowerRegionNds lowerRegion =
  lowerRegionNd lowerRegion : concatMap lowerStateRegionNds lowerRegion.states

lowerStateRegionNds :: LowerState st sg hf ctx cmd child -> [RegionNd]
lowerStateRegionNds lowerState =
  case lowerState.child of
    Nothing -> []
    Just childRegion -> lowerRegionNds childRegion

lowerRegionNd :: LowerRegion st sg hf ctx cmd child -> RegionNd
lowerRegionNd lowerRegion =
  RegionNd
    { ref = lowerRegion.ref
    , parent = lowerRegion.parent
    , initial = lowerRegion.initial
    , states = V.fromList (fmap (.ref) lowerRegion.states)
    , path = lowerRegion.path
    , meta = lowerRegion.meta
    }

lowerStateNds :: LowerRegion st sg hf ctx cmd child -> [StateNd]
lowerStateNds lowerRegion =
  concatMap collect lowerRegion.states
  where
    collect lowerState =
      lowerStateNd lowerState : maybe [] lowerStateNds lowerState.child

lowerStateNd :: LowerState st sg hf ctx cmd child -> StateNd
lowerStateNd lowerState =
  StateNd
    { ref = lowerState.ref
    , parent = lowerState.parent
    , name = lowerState.name
    , kind = lowerState.kind
    , entry = fmap fst lowerState.entry
    , cases = V.fromList (fmap fst lowerState.cases)
    , routes = V.fromList (fmap fst lowerState.routes)
    , child = fmap (.ref) lowerState.child
    , path = lowerState.path
    , meta = lowerState.meta
    }

lowerCaseNds :: LowerRegion st sg hf ctx cmd child -> [CaseNd]
lowerCaseNds lowerRegion =
  concatMap collect lowerRegion.states
  where
    collect lowerState =
      lowerStateCaseNds lowerState <> maybe [] lowerCaseNds lowerState.child

lowerStateCaseNds :: LowerState st sg hf ctx cmd child -> [CaseNd]
lowerStateCaseNds lowerState =
  entryCaseNd <> normalCaseNds
  where
    entryCaseNd =
      case lowerState.entry of
        Nothing ->
          []

        Just (caseRef, _) ->
          [ CaseNd
              { ref = caseRef
              , state = lowerState.ref
              , name = Nothing
              , meta = emptyMetaSpec
              }
          ]

    normalCaseNds =
      [ CaseNd
          { ref = caseRef
          , state = lowerState.ref
          , name = caseSpec.name
          , meta = caseSpec.meta
          }
      | (caseRef, caseSpec) <- lowerState.cases
      ]


lowerRouteNds ::
  (Ord st, Show st, Show hf, Show child, Show ctx, Show cmd) =>
  Map st StateRef ->
  Map JoinName JoinNd ->
  Map TimerName TimerNd ->
  LowerRegion st sg hf ctx cmd child ->
  Either CompileErr [RouteEd]
lowerRouteNds stateRefMap joinDefs timerDefs lowerRegion =
  fmap concat $
    traverse (lowerStateRouteNds stateRefMap joinDefs timerDefs) lowerRegion.states


lowerStateRouteNds ::
  (Ord st, Show st, Show hf, Show child, Show ctx, Show cmd) =>
  Map st StateRef ->
  Map JoinName JoinNd ->
  Map TimerName TimerNd ->
  LowerState st sg hf ctx cmd child ->
  Either CompileErr [RouteEd]
lowerStateRouteNds stateRefMap joinDefs timerDefs lowerState = do
  currentRoutes <- traverse (lowerRouteEd stateRefMap joinDefs timerDefs lowerState) lowerState.routes
  childRoutes <-
    case lowerState.child of
      Nothing -> Right []
      Just childRegion -> lowerRouteNds stateRefMap joinDefs timerDefs childRegion
  pure (currentRoutes <> childRoutes)

lowerRouteEd ::
  (Ord st, Show st, Show hf, Show child, Show ctx, Show cmd) =>
        Map StateRef StateRef -> Map JoinName JoinNd -> Map TimerName TimerNd -> LowerState st sg hf ctx cmd child ->
        (RouteRef, RouteSpec st hf child) -> Either CompileErr RouteEd
lowerRouteEd stateRefMap joinDefs timerDefs lowerState (routeRef, routeSpec) = do
  targetPlan <- lowerTarget stateRefMap lowerState routeSpec.plan.target
  waitPlan <- lowerWait timerDefs joinDefs lowerState routeSpec.plan.wait
  joinPlan <- lowerJoin joinDefs lowerState routeSpec.plan.join
  timerPlans <- traverse (lowerTimerPlan timerDefs lowerState) routeSpec.plan.timers

  let
    routeJoinRef = joinPlanRef joinPlan
    spawnPlans = fmap (lowerSpawnPlan routeJoinRef) routeSpec.plan.spawn
    breakPlans = lowerBreakPlans routeSpec.plan.break
    routePlan =
      RoutePlan
        { target = targetPlan
        , wait = waitPlan
        , spawn = spawnPlans
        , join = joinPlan
        , timers = timerPlans
        , break = breakPlans
        }

  pure RouteEd
    { ref = routeRef
    , state = lowerState.ref
    , caseRef = Nothing
    , handoff = renderHandoff routeSpec.on
    , plan = routePlan
    , meta = routeSpec.meta
    }

lowerTarget :: (Ord st, Show st, Eq st) => Map StateRef StateRef -> LowerState st sg hf ctx cmd child -> TargetPlan st ->
              Either CompileErr RouteTarget
lowerTarget stateRefMap lowerState targetPlan =
  case targetPlan of
    GotoTg stateKey ->
      case M.lookup (stateKey :: StateRef) stateRefMap of
        Just stateRef ->
          Right (GotoRt stateRef)

        Nothing ->
          Left $
            MissingTargetEr $
              "route from state " <> quote (stateNameText lowerState.name) <>
              " targets unknown state key " <> quote (renderAuthored stateKey)
    StayTg -> Right StayRt
    CompleteTg -> Right CompleteRt
    FailTg msg -> Right (FailRt msg)

lowerWait ::
  Map TimerName TimerNd ->
  Map JoinName JoinNd ->
  LowerState st sg hf ctx cmd child ->
  WaitPlan ->
  Either CompileErr WaitPlanG
lowerWait timerDefs joinDefs lowerState waitPlan =
  case waitPlan of
    WaitNoneWp ->
      Right WaitNoneWg

    WaitSignalWp ->
      Right WaitSignalWg

    WaitJoinWp joinName ->
      case M.lookup joinName joinDefs of
        Just joinNd ->
          Right (WaitJoinWg joinNd.ref)

        Nothing ->
          Left $
            MissingTargetEr $
              "route from state " <> quote (stateNameText lowerState.name) <>
              " references unknown join " <> quote (joinNameText joinName)

    WaitTimerWp timerName ->
      case M.lookup timerName timerDefs of
        Just timerNd ->
          Right (WaitTimerWg timerNd.ref)

        Nothing ->
          Left $
            MissingTargetEr $
              "route from state " <> quote (stateNameText lowerState.name) <>
              " references unknown timer " <> quote (timerNameText timerName)

lowerJoin ::
  Map JoinName JoinNd ->
  LowerState st sg hf ctx cmd child ->
  JoinPlan ->
  Either CompileErr Graph.JoinPlanG
lowerJoin joinDefs lowerState joinPlan =
  case joinPlan of
    JoinNoneJp ->
      Right Graph.JoinNoneJg

    JoinAllJp joinName ->
      maybe (Left (missingJoinErr lowerState.name joinName)) (Right . Graph.JoinAllJg . (.ref)) (M.lookup joinName joinDefs)

    JoinAnyJp joinName ->
      maybe (Left (missingJoinErr lowerState.name joinName)) (Right . Graph.JoinAnyJg . (.ref)) (M.lookup joinName joinDefs)

    JoinCountJp joinName count ->
      maybe (Left (missingJoinErr lowerState.name joinName)) (Right . (\joinNd -> Graph.JoinCountJg joinNd.ref count)) (M.lookup joinName joinDefs)

lowerTimerPlan ::
  Map TimerName TimerNd ->
  LowerState st sg hf ctx cmd child ->
  TimerPlan ->
  Either CompileErr TimerPlanG
lowerTimerPlan timerDefs lowerState timerPlan =
  case M.lookup timerPlan.key timerDefs of
    Just timerNd ->
      Right $
        TimerPlanG
          { ref = timerNd.ref
          , delay = timerPlan.delay
          , payload = timerPlan.payload
          }

    Nothing ->
      Left $
        MissingTargetEr $
          "route from state " <> quote (stateNameText lowerState.name) <>
          " references unknown timer " <> quote (timerNameText timerPlan.key)

lowerSpawnPlan :: Show child => Maybe JoinRef -> SpawnPlan child -> SpawnPlanG
lowerSpawnPlan joinRef spawnPlan =
  SpawnPlanG
    { child = renderChild spawnPlan.child
    , input = spawnPlan.input
    , key = normalizeMaybeText spawnPlan.key
    , join = joinRef
    }

lowerBreakPlans :: [BreakPlan] -> [BreakPlanG]
lowerBreakPlans =
  concatMap lowerBreakPlan

lowerBreakPlan :: BreakPlan -> [BreakPlanG]
lowerBreakPlan breakPlan =
  case breakPlan of
    BreakNoneBp -> []
    BreakBeforeRouteBp -> [BreakBeforeRouteBg]
    BreakAfterRouteBp -> [BreakAfterRouteBg]
    BreakBeforeCommitBp -> [BreakBeforeCommitBg]

joinPlanRef :: Graph.JoinPlanG -> Maybe JoinRef
joinPlanRef joinPlan =
  case joinPlan of
    Graph.JoinNoneJg -> Nothing
    Graph.JoinAllJg joinRef -> Just joinRef
    Graph.JoinAnyJg joinRef -> Just joinRef
    Graph.JoinCountJg joinRef _ -> Just joinRef

missingJoinErr :: StateName -> JoinName -> CompileErr
missingJoinErr stateName joinName =
  MissingTargetEr $
    "route from state " <> quote (stateNameText stateName) <>
    " references unknown join " <> quote (joinNameText joinName)

finalizeDigests :: MachineGraph -> Either CompileErr (MachineGraph, SpecDigest)
finalizeDigests graph0 = do
  let graphPayload = renderGraphPayload graph0
      graphHex = hash256Hex ("graph\n" <> graphPayload)
  graphDigest <- first (LoweringEr . renderDigestErr) (mkGraphDigest graphHex)
  let
    graph1 = (graph0 :: MachineGraph) { digest = graphDigest }
    specHex = hash256Hex ("spec\n" <> graphPayload <> "\n" <> graphDigestText graphDigest)
  specDigest <- first (LoweringEr . renderDigestErr) (mkSpecDigest specHex)
  pure (graph1, specDigest)

renderGraphPayload :: MachineGraph -> Text
renderGraphPayload graph =
  T.intercalate "\n" $
    [ renderStable ("machine" :: Text, machineNameText graph.machine)
    , renderStable ("version" :: Text, machineVersionText graph.version)
    , renderStable ("root" :: Text, regionRefWord32 graph.root)
    , renderStable ("meta" :: Text, renderMeta graph.meta)
    ]
    <> fmap renderRegionNdStable (IM.elems graph.regions)
    <> fmap renderStateNdStable (IM.elems graph.states)
    <> fmap renderCaseNdStable (IM.elems graph.cases)
    <> fmap renderRouteEdStable (IM.elems graph.routes)
    <> fmap renderJoinNdStable (IM.elems graph.joins)
    <> fmap renderTimerNdStable (IM.elems graph.timers)

renderRegionNdStable :: RegionNd -> Text
renderRegionNdStable regionNd =
  renderStable
    ( "region" :: Text
    , regionRefWord32 regionNd.ref
    , fmap stateRefWord32 regionNd.parent
    , stateRefWord32 regionNd.initial
    , fmap stateRefWord32 (V.toList regionNd.states)
    , fmap regionRefWord32 (regionPathToList regionNd.path)
    , renderMeta regionNd.meta
    )

renderStateNdStable :: StateNd -> Text
renderStateNdStable stateNd =
  renderStable
    ( "state" :: Text
    , stateRefWord32 stateNd.ref
    , regionRefWord32 stateNd.parent
    , stateNameText stateNd.name
    , show stateNd.kind
    , fmap caseRefWord32 stateNd.entry
    , fmap caseRefWord32 (V.toList stateNd.cases)
    , fmap routeRefWord32 (V.toList stateNd.routes)
    , fmap regionRefWord32 stateNd.child
    , fmap stateRefWord32 (statePathToList stateNd.path)
    , renderMeta stateNd.meta
    )

renderCaseNdStable :: CaseNd -> Text
renderCaseNdStable caseNd =
  renderStable
    ( "case" :: Text
    , caseRefWord32 caseNd.ref
    , stateRefWord32 caseNd.state
    , fmap caseNameText caseNd.name
    , renderMeta caseNd.meta
    )

renderRouteEdStable :: RouteEd -> Text
renderRouteEdStable routeEd =
  renderStable
    ( "route" :: Text
    , routeRefWord32 routeEd.ref
    , stateRefWord32 routeEd.state
    , fmap caseRefWord32 routeEd.caseRef
    , routeEd.handoff
    , renderRoutePlanStable routeEd.plan
    , renderMeta routeEd.meta
    )

renderJoinNdStable :: JoinNd -> Text
renderJoinNdStable joinNd =
  renderStable
    ( "join" :: Text
    , joinRefWord32 joinNd.ref
    , fmap joinNameText joinNd.name
    , renderJoinMode joinNd.mode
    , renderMeta joinNd.meta
    )

renderTimerNdStable :: TimerNd -> Text
renderTimerNdStable timerNd =
  renderStable
    ( "timer" :: Text
    , timerRefWord32 timerNd.ref
    , fmap timerNameText timerNd.name
    , renderMeta timerNd.meta
    )

renderRoutePlanStable :: RoutePlan -> Text
renderRoutePlanStable routePlan =
  renderStable
    ( renderRouteTarget routePlan.target
    , renderWaitPlan routePlan.wait
    , fmap renderSpawnPlan routePlan.spawn
    , renderGraphJoinPlan routePlan.join
    , fmap renderTimerPlan routePlan.timers
    , fmap renderBreakPlan routePlan.break
    )

renderRouteTarget :: RouteTarget -> Text
renderRouteTarget routeTarget =
  case routeTarget of
    StayRt -> renderStable ("stay" :: Text)
    GotoRt stateRef -> renderStable ("goto" :: Text, stateRefWord32 stateRef)
    CompleteRt -> renderStable ("complete" :: Text)
    FailRt msg -> renderStable ("fail" :: Text, msg)

renderWaitPlan :: WaitPlanG -> Text
renderWaitPlan waitPlan =
  case waitPlan of
    WaitNoneWg -> renderStable ("none" :: Text)
    WaitSignalWg -> renderStable ("signal" :: Text)
    WaitJoinWg joinRef -> renderStable ("join" :: Text, joinRefWord32 joinRef)
    WaitTimerWg timerRef -> renderStable ("timer" :: Text, timerRefWord32 timerRef)

renderSpawnPlan :: SpawnPlanG -> Text
renderSpawnPlan spawnPlan =
  renderStable
    ( spawnPlan.child
    , renderValueStable spawnPlan.input
    , spawnPlan.key
    , fmap joinRefWord32 spawnPlan.join
    )

renderGraphJoinPlan :: Graph.JoinPlanG -> Text
renderGraphJoinPlan joinPlan =
  case joinPlan of
    Graph.JoinNoneJg -> renderStable ("none" :: Text)
    Graph.JoinAllJg joinRef -> renderStable ("all" :: Text, joinRefWord32 joinRef)
    Graph.JoinAnyJg joinRef -> renderStable ("any" :: Text, joinRefWord32 joinRef)
    Graph.JoinCountJg joinRef count -> renderStable ("count" :: Text, joinRefWord32 joinRef, count)

renderTimerPlan :: TimerPlanG -> Text
renderTimerPlan timerPlan =
  renderStable
    ( timerRefWord32 timerPlan.ref
    , show timerPlan.delay
    , fmap renderValueStable timerPlan.payload
    )

renderBreakPlan :: BreakPlanG -> Text
renderBreakPlan breakPlan =
  case breakPlan of
    BreakBeforeCaseBg -> "before-case"
    BreakAfterCaseBg -> "after-case"
    BreakBeforeRouteBg -> "before-route"
    BreakAfterRouteBg -> "after-route"
    BreakBeforeCommitBg -> "before-commit"

renderJoinMode :: JoinMode -> Text
renderJoinMode joinMode =
  case joinMode of
    AllJm -> "all"
    AnyJm -> "any"
    CountJm count -> renderStable ("count" :: Text, count)

renderMeta :: MetaSpec -> Text
renderMeta meta =
  renderStable
    ( S.toAscList meta.tags
    , meta.note
    , meta.owner
    , meta.rank
    , fmap show meta.spans
    )

renderValueStable :: Value -> Text
renderValueStable value =
  case value of
    Null -> "null"
    Bool False -> "false"
    Bool True -> "true"
    Number sc -> T.pack (Sci.formatScientific Sci.Generic Nothing sc)
    String txt -> renderStable txt
    Array xs -> "[" <> T.intercalate "," (fmap renderValueStable (V.toList xs)) <> "]"
    Object obj ->
      let kvs = sortOn fst [(K.toText key, renderValueStable val) | (key, val) <- KM.toList obj]
      in "{" <> T.intercalate "," [renderStable key <> ":" <> val | (key, val) <- kvs] <> "}"

hash256Hex :: Text -> Text
hash256Hex txt =
  T.concat (fmap (padHex16 . hashWithSeed bytes) seeds)
  where
    bytes = TE.encodeUtf8 txt
    seeds =
      [ 0x243F6A8885A308D3
      , 0x13198A2E03707344
      , 0xA4093822299F31D0
      , 0x082EFA98EC4E6C89
      ]

hashWithSeed :: BS.ByteString -> Word64 -> Word64
hashWithSeed bytes seed =
  BS.foldl' step (seed `xor` fnvOffset) bytes
  where
    step !acc byte = (acc `xor` fromIntegral byte) * fnvPrime

fnvOffset :: Word64
fnvOffset = 0xCBF29CE484222325

fnvPrime :: Word64
fnvPrime = 0x100000001B3

padHex16 :: Word64 -> Text
padHex16 word64 =
  let raw = T.pack (showHex word64 "")
      width = 16 - T.length raw
  in T.replicate (max 0 width) "0" <> raw

routeSpecsDeep :: RegionSpec st sg hf ctx cmd child -> [RouteSpec st hf child]
routeSpecsDeep regionSpec =
  concatMap routeSpecsDeepState regionSpec.states

routeSpecsDeepState :: StateSpec st sg hf ctx cmd child -> [RouteSpec st hf child]
routeSpecsDeepState stateSpec =
  fmap normalizedRouteSpec stateSpec.routes <> maybe [] routeSpecsDeep stateSpec.child

normalizedRouteSpec :: RouteSpec st hf child -> RouteSpec st hf child
normalizedRouteSpec routeSpec =
  routeSpec { plan = normalizeControlPlan routeSpec.plan }


firstRouteError :: Show hf => [RouteSpec st hf child] -> Maybe (Text, [RouteErr])
firstRouteError [] = Nothing
firstRouteError (routeSpec : rest) =
  let
    errs = validateRouteSpec routeSpec
  in if null errs then firstRouteError rest else Just (renderHandoff routeSpec.on, errs)


duplicatesOrd :: Ord a => [a] -> [a]
duplicatesOrd xs = go S.empty S.empty xs
  where
    go _ dup [] = S.toAscList dup
    go seen dup (x : rest)
      | S.member x seen = go seen (S.insert x dup) rest
      | otherwise = go (S.insert x seen) dup rest

intMapFromList :: (a -> Int) -> [a] -> IntMap a
intMapFromList keyOf =
  IM.fromList . fmap (\x -> (keyOf x, x))

renderRegionLabel :: Maybe RegionName -> Maybe StateRef -> Text
renderRegionLabel maybeName parentRef =
  case maybeName of
    Just regionName -> regionNameText regionName
    Nothing ->
      case parentRef of
        Nothing -> "<root>"
        Just stateRef -> "<child-of-" <> renderStateRef stateRef <> ">"

renderStateRef :: StateRef -> Text
renderStateRef stateRef =
  T.pack (show (stateRefWord32 stateRef))

renderAuthored :: Show a => a -> Text
renderAuthored = T.pack . show

renderHandoff :: Show hf => hf -> Text
renderHandoff = T.strip . T.pack . show

renderChild :: Show child => child -> Text
renderChild = T.strip . T.pack . show

renderTextList :: [Text] -> Text
renderTextList =
  T.intercalate ", " . fmap quote

renderStable :: Show a => a -> Text
renderStable = T.pack . show

normalizeMaybeText :: Maybe Text -> Maybe Text
normalizeMaybeText Nothing = Nothing
normalizeMaybeText (Just txt) =
  let txt' = T.strip txt
  in if T.null txt' then Nothing else Just txt'

quote :: Text -> Text
quote txt = "\"" <> txt <> "\""

fromWord32 :: Word32 -> Int
fromWord32 = fromIntegral