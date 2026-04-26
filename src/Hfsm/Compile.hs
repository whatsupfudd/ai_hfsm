{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE RankNTypes #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Hfsm.Compile
  ( CompiledMachine(..)
  , CompileErr(..)
  , compile
  , compileMany
  ) where

import Control.Monad (forM, forM_, unless, when, void)
import Control.Monad.State.Strict (StateT, evalStateT, get, modify', runStateT)
import Control.Monad.Trans.Class (lift)

import Data.Aeson (Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import qualified Data.ByteString as BS
import Data.Bits (xor)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isJust)
import Data.Scientific (FPFormat(Generic), formatScientific)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Text.Encoding (encodeUtf8)
import qualified Data.Vector as V
import Data.Word (Word32, Word64)
import Numeric (showHex)

import qualified Hfsm.Compile.Exec as Exec
import Hfsm.Compile.Exec (ExecTable)
import Hfsm.Core.Digest
  ( GraphDigest
  , SpecDigest
  , digestLen
  , graphDigestText
  , mkGraphDigest
  , mkSpecDigest
  )
import Hfsm.Core.Meta (MetaSpec(..), emptyMetaSpec)
import Hfsm.Core.Name
  ( CaseName
  , JoinName
  , MachineName
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
  , renderRegionPath
  , renderStatePath
  , singletonRegionPath
  , singletonStatePath
  )
import Hfsm.Core.Ref
  ( CaseRef
  , JoinRef
  , RegionRef
  , RouteRef
  , StateRef
  , TimerRef
  , caseRefWord32
  , joinRefWord32
  , mkCaseRef
  , mkJoinRef
  , mkRegionRef
  , mkRouteRef
  , mkStateRef
  , mkTimerRef
  , regionRefWord32
  , routeRefWord32
  , stateRefWord32
  , timerRefWord32
  )
import Hfsm.Core.Version (MachineVersion, machineVersionText)
import qualified Hfsm.Graph.Def as G
import Hfsm.Spec.Case (CaseSpec(..))
import Hfsm.Spec.Def (MachineSpec(..), RegionSpec(..), StateKind(..), StateSpec(..))
import qualified Hfsm.Spec.Route as SR
import qualified Hfsm.Spec.Case as SC


data CompiledMachine st sg hf ctx cmd child = CompiledMachine
  { spec :: MachineSpec st sg hf ctx cmd child
  , graph :: G.MachineGraph
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
  deriving stock (Eq, Ord, Show, Read)

compile :: (Ord st, Show st, Show hf, Show child) =>
        MachineSpec st sg hf ctx cmd child -> Either CompileErr (CompiledMachine st sg hf ctx cmd child)
compile machineSpec = do
  layout <- buildLayout machineSpec
  defs <- collectDefs machineSpec
  lowered <- lowerMachine machineSpec layout defs
  let
    graph0 = G.MachineGraph {
        machine = machineSpec.name
      , version = machineSpec.version
      , digest = zeroGraphDigest
      , root = layout.root
      , regions = layout.regions
      , states = lowered.states
      , cases = lowered.cases
      , routes = lowered.routes
      , joins = defs.joins
      , timers = defs.timers
      , meta = machineSpec.meta
      }
    graphDigest = digestGraph graph0
    graph1 = graph0 { G.digest = graphDigest }
    specDigest = digestCompiled graph1
  pure
    CompiledMachine
      { spec = machineSpec
      , graph = graph1
      , exec = lowered.exec
      , digest = specDigest
      }

compileMany :: (Ord st, Show st, Show hf, Show child) => [MachineSpec st sg hf ctx cmd child] -> Either CompileErr [CompiledMachine st sg hf ctx cmd child]
compileMany = traverse compile

data Layout st = Layout
  { root :: RegionRef
  , regions :: IntMap G.RegionNd
  , states :: IntMap G.StateNd
  , stateRefs :: Map st StateRef
  }

data LayoutSt st = LayoutSt
  { nextRegion :: !Word32
  , nextState :: !Word32
  , regionNames :: !(Set RegionName)
  , regions :: !(IntMap G.RegionNd)
  , states :: !(IntMap G.StateNd)
  , stateRefs :: !(Map st StateRef)
  }

data Defs = Defs
  { joins :: IntMap G.JoinNd
  , timers :: IntMap G.TimerNd
  , joinRefs :: Map JoinName JoinRef
  , timerRefs :: Map TimerName TimerRef
  }

data DefsSt = DefsSt
  { nextJoin :: !Word32
  , nextTimer :: !Word32
  , joins :: !(IntMap G.JoinNd)
  , timers :: !(IntMap G.TimerNd)
  , joinRefs :: !(Map JoinName JoinRef)
  , timerRefs :: !(Map TimerName TimerRef)
  }

data Lowered sg ctx hf cmd = Lowered
  { states :: IntMap G.StateNd
  , cases :: IntMap G.CaseNd
  , routes :: IntMap G.RouteEd
  , exec :: ExecTable sg ctx hf cmd
  }

data LowerSt sg ctx hf cmd = LowerSt
  { nextCase :: !Word32
  , nextRoute :: !Word32
  , states :: !(IntMap G.StateNd)
  , cases :: !(IntMap G.CaseNd)
  , routes :: !(IntMap G.RouteEd)
  , exec :: !(ExecTable sg ctx hf cmd)
  }

buildLayout :: (Ord st, Show st) => MachineSpec st sg hf ctx cmd child -> Either CompileErr (Layout st)
buildLayout machineSpec = do
  (rootRef, st) <- runLayout (layoutRegion Nothing Nothing Nothing machineSpec.root) emptyLayoutSt
  pure
    Layout
      { root = rootRef
      , regions = st.regions
      , states = st.states
      , stateRefs = st.stateRefs
      }

emptyLayoutSt :: LayoutSt st
emptyLayoutSt =
  LayoutSt
    { nextRegion = 0
    , nextState = 0
    , regionNames = S.empty
    , regions = IM.empty
    , states = IM.empty
    , stateRefs = M.empty
    }

runLayout :: StateT (LayoutSt st) (Either CompileErr) a -> LayoutSt st -> Either CompileErr (a, LayoutSt st)
runLayout action st0 = do
  result <- evalStateT ((,) <$> action <*> get) st0
  pure result

layoutRegion :: forall st sg ctx hf cmd child. (Ord st, Show st) =>
  Maybe StateRef -> Maybe RegionPath -> Maybe StatePath -> RegionSpec st sg hf ctx cmd child
    -> StateT (LayoutSt st) (Either CompileErr) RegionRef
layoutRegion parentState mbParentRegionPath mbParentStatePath regionSpec = do
  registerRegionName regionSpec.name
  unless (regionInitialExists regionSpec) $
    lift $ Left $ MissingInitialEr $
      "region " <> renderRegionLabel regionSpec.name <> " initial state " <> quote (tshow regionSpec.initial) <> " is not a direct member of the region"
  regionRef <- freshRegionRef
  let
    regionPath = case mbParentRegionPath of
      Nothing -> singletonRegionPath regionRef
      Just parentPath -> appendRegionRef regionRef parentPath
  statePairs <- forM regionSpec.states $ \stateSpec -> do
    stateRef <- freshStateRef stateSpec.key
    let
      statePath = case mbParentStatePath of
        Nothing -> singletonStatePath stateRef
        Just parentPath -> appendStateRef stateRef parentPath
    validateStateShape stateRef stateSpec
    childRef <-
      case stateSpec.child of
        Nothing -> pure Nothing
        Just childRegion ->
          Just <$> layoutRegion (Just stateRef) (Just regionPath) (Just statePath) childRegion
    let stateNd =
          G.StateNd
            { ref = stateRef
            , parent = regionRef
            , name = stateSpec.name
            , kind = stateSpec.kind
            , entry = Nothing
            , cases = V.empty
            , routes = V.empty
            , child = childRef
            , path = statePath
            , meta = stateSpec.meta
            }
    modify' $ \st -> (st :: LayoutSt st) {
          states = IM.insert (stateRefKey stateRef) stateNd st.states
        }
    pure (stateSpec.key, stateRef)
  initialRef <-
    case lookupLocalStateRef regionSpec.initial statePairs of
      Nothing ->
        lift $
          Left $
            MissingInitialEr $
              "region " <> renderRegionLabel regionSpec.name <> " initial state " <> quote (tshow regionSpec.initial) <> " could not be resolved"
      Just ref -> pure ref
  let regionNd =
        G.RegionNd
          { ref = regionRef
          , parent = parentState
          , initial = initialRef
          , states = V.fromList (fmap snd statePairs)
          , path = regionPath
          , meta = regionSpec.meta
          }
  modify' $ \st -> (st :: LayoutSt st) {
       regions = IM.insert (regionRefKey regionRef) regionNd st.regions
      }
  pure regionRef

registerRegionName :: Maybe RegionName -> StateT (LayoutSt st) (Either CompileErr) ()
registerRegionName Nothing = pure ()
registerRegionName (Just regionName) = do
  st <- get
  when (S.member regionName st.regionNames) $
    lift $
      Left $
        DuplicateRegionEr $
          "duplicate region name " <> quote (regionNameText regionName)
  modify' $ \st0 ->
    st0
      { regionNames = S.insert regionName st0.regionNames
      }

freshRegionRef :: StateT (LayoutSt st) (Either CompileErr) RegionRef
freshRegionRef = do
  st <- get
  when (st.nextRegion == maxBound) $
    lift $
      Left $
        LoweringEr "region reference space exhausted"
  let ref = mkRegionRef st.nextRegion
  modify' $ \st0 -> st0 { nextRegion = st0.nextRegion + 1 }
  pure ref

freshStateRef :: (Ord st, Show st) => st -> StateT (LayoutSt st) (Either CompileErr) StateRef
freshStateRef key0 = do
  st <- get
  when (M.member key0 st.stateRefs) $
    lift $
      Left $
        DuplicateStateEr $
          "duplicate state key " <> quote (tshow key0)
  when (st.nextState == maxBound) $
    lift $
      Left $
        LoweringEr "state reference space exhausted"
  let ref = mkStateRef st.nextState
  modify' $ \st0 ->
    st0
      { nextState = st0.nextState + 1
      , stateRefs = M.insert key0 ref st0.stateRefs
      }
  pure ref

validateStateShape :: StateRef -> StateSpec st sg hf ctx cmd child -> StateT (LayoutSt st) (Either CompileErr) ()
validateStateShape stateRef stateSpec =
  case (stateSpec.kind, stateSpec.child) of
    (CompositeSk, Nothing) ->
      lift $
        Left $
          InvalidCompositeEr stateRef "composite state must define a child region"

    (AtomicSk, Just _) ->
      lift $
        Left $
          InvalidCompositeEr stateRef "atomic state cannot define a child region"

    (TerminalSk, Just _) ->
      lift $
        Left $
          InvalidTerminalEr stateRef "terminal state cannot define a child region"

    _ ->
      when (stateSpec.kind == TerminalSk && not (null stateSpec.routes)) $
        lift $
          Left $
            InvalidTerminalEr stateRef "terminal state cannot define structural routes"

regionInitialExists :: Eq st => RegionSpec st sg hf ctx cmd child -> Bool
regionInitialExists regionSpec = any (\stateSpec -> stateSpec.key == regionSpec.initial) regionSpec.states

lookupLocalStateRef :: Eq st => st -> [(st, StateRef)] -> Maybe StateRef
lookupLocalStateRef key0 = go
  where
    go [] = Nothing
    go ((key1, ref) : xs)
      | key0 == key1 = Just ref
      | otherwise = go xs

collectDefs :: MachineSpec st sg hf ctx cmd child -> Either CompileErr Defs
collectDefs machineSpec = do
  ((), st) <- runStateT (collectRegionDefs machineSpec.root) emptyDefsSt
  pure Defs { 
        joins = st.joins
      , timers = st.timers
      , joinRefs = st.joinRefs
      , timerRefs = st.timerRefs
      }

emptyDefsSt :: DefsSt
emptyDefsSt =
  DefsSt
    { nextJoin = 0
    , nextTimer = 0
    , joins = IM.empty
    , timers = IM.empty
    , joinRefs = M.empty
    , timerRefs = M.empty
    }

collectRegionDefs :: RegionSpec st sg hf ctx cmd child -> StateT DefsSt (Either CompileErr) ()
collectRegionDefs regionSpec =
  forM_ regionSpec.states collectStateDefs

collectStateDefs :: StateSpec st sg hf ctx cmd child -> StateT DefsSt (Either CompileErr) ()
collectStateDefs stateSpec = do
  forM_ stateSpec.routes collectRouteDefs
  forM_ stateSpec.child collectRegionDefs

collectRouteDefs :: SR.RouteSpec st hf child -> StateT DefsSt (Either CompileErr) ()
collectRouteDefs routeSpec = do
  collectJoinDef routeSpec.plan.join
  forM_ routeSpec.plan.timers collectTimerDef

collectJoinDef :: SR.JoinPlan -> StateT DefsSt (Either CompileErr) ()
collectJoinDef joinPlan =
  case joinPlan of
    SR.JoinNoneJp -> pure ()
    SR.JoinAllJp joinName -> ensureJoin joinName G.AllJm
    SR.JoinAnyJp joinName -> ensureJoin joinName G.AnyJm
    SR.JoinCountJp joinName count -> ensureJoin joinName (G.CountJm count)

collectTimerDef :: SR.TimerPlan -> StateT DefsSt (Either CompileErr) ()
collectTimerDef timerPlan = ensureTimer timerPlan.key

ensureJoin :: JoinName -> G.JoinMode -> StateT DefsSt (Either CompileErr) ()
ensureJoin joinName joinMode = do
  st <- get
  when (M.member joinName st.joinRefs) $
    lift $
      Left $
        DuplicateJoinEr $
          "duplicate join name " <> quote (joinNameText joinName)
  when (st.nextJoin == maxBound) $
    lift $
      Left $
        LoweringEr "join reference space exhausted"
  let ref = mkJoinRef st.nextJoin
      nd =
        G.JoinNd
          { ref = ref
          , name = Just joinName
          , mode = joinMode
          , meta = emptyMetaSpec
          }
  modify' $ \st0 ->
    st0
      { nextJoin = st0.nextJoin + 1
      , joinRefs = M.insert joinName ref st0.joinRefs
      , joins = IM.insert (joinRefKey ref) nd st0.joins
      }

ensureTimer :: TimerName -> StateT DefsSt (Either CompileErr) ()
ensureTimer timerName = do
  st <- get
  when (M.member timerName st.timerRefs) $
    lift $
      Left $
        DuplicateTimerEr $
          "duplicate timer name " <> quote (timerNameText timerName)
  when (st.nextTimer == maxBound) $
    lift $
      Left $
        LoweringEr "timer reference space exhausted"
  let ref = mkTimerRef st.nextTimer
      nd =
        G.TimerNd
          { ref = ref
          , name = Just timerName
          , meta = emptyMetaSpec
          }
  modify' $ \st0 ->
    st0
      { nextTimer = st0.nextTimer + 1
      , timerRefs = M.insert timerName ref st0.timerRefs
      , timers = IM.insert (timerRefKey ref) nd st0.timers
      }

lowerMachine :: forall st sg ctx hf cmd child. (Show st, Show hf, Show child, Ord st) =>
      MachineSpec st sg hf ctx cmd child -> Layout st -> Defs
      -> Either CompileErr (Lowered sg ctx hf cmd)
lowerMachine machineSpec layout defs = do
  ((), st) <- runStateT (lowerRegion machineSpec.root) (emptyLowerSt layout.states)
  pure Lowered {
        states = st.states
      , cases = st.cases
      , routes = st.routes
      , exec = st.exec
      }
  where
    lowerRegion :: RegionSpec st sg hf ctx cmd child -> StateT (LowerSt sg ctx hf cmd) (Either CompileErr) ()
    lowerRegion regionSpec = forM_ regionSpec.states lowerState

    lowerState :: StateSpec st sg hf ctx cmd child -> StateT (LowerSt sg ctx hf cmd) (Either CompileErr) ()
    lowerState stateSpec = do
      stateRef <- resolveStateRef layout stateSpec.key
      when (needsRoutes stateSpec && null stateSpec.routes) $
        lift $
          Left $
            MissingRouteEr $
              "state " <> renderStateLabel stateSpec.key stateSpec.name <> " defines handlers but no routes"
      void $ checkDuplicateCaseNames stateSpec
      checkDuplicateRoutes stateSpec
      entryRef <- lowerEntry stateRef stateSpec.entry
      caseRefs <- fmap V.fromList (forM stateSpec.cases (lowerCase stateRef))
      routeRefs <- fmap V.fromList (forM stateSpec.routes (lowerRoute stateRef))
      stateNd <- lookupStateNd stateRef
      let
        stateNd1 :: G.StateNd
        stateNd1 = stateNd { 
                G.entry = entryRef
              , G.cases = caseRefs
              , G.routes = routeRefs
              }
      putStateNd stateNd1
      forM_ stateSpec.child lowerRegion

    lowerEntry :: forall ctx hf cmd sg. StateRef -> Maybe (SC.EntryFn ctx hf cmd)
              -> StateT (LowerSt sg ctx hf cmd) (Either CompileErr) (Maybe CaseRef)
    lowerEntry _ Nothing = pure Nothing
    lowerEntry stateRef (Just entryFn) = do
      caseRef <- freshCaseRef
      let caseNd =
            G.CaseNd
              { ref = caseRef
              , state = stateRef
              , name = Nothing
              , meta = emptyMetaSpec
              }
      modify' $ \st -> (st :: LowerSt sg ctx hf cmd) {
           cases = IM.insert (caseRefKey caseRef) caseNd st.cases
          , exec = Exec.insertEntryFn stateRef caseRef entryFn st.exec
          }
      pure (Just caseRef)

    lowerCase :: forall sg ctx hf cmd. StateRef -> CaseSpec sg ctx hf cmd 
            -> StateT (LowerSt sg ctx hf cmd) (Either CompileErr) CaseRef
    lowerCase stateRef caseSpec = do
      caseRef <- freshCaseRef
      let caseNd =
            G.CaseNd
              { ref = caseRef
              , state = stateRef
              , name = caseSpec.name
              , meta = caseSpec.meta
              }
      modify' $ \st -> (st :: LowerSt sg ctx hf cmd) {
            cases = IM.insert (caseRefKey caseRef) caseNd st.cases
          , exec = Exec.insertCaseSpec stateRef caseRef caseSpec st.exec
        }
      pure caseRef


    lowerRoute :: StateRef -> SR.RouteSpec st hf child 
              -> StateT (LowerSt sg ctx hf cmd) (Either CompileErr) RouteRef
    lowerRoute stateRef routeSpec = do
      routeRef <- freshRouteRef
      planG <- lowerControlPlan routeSpec.plan
      let routeEd =
            G.RouteEd
              { ref = routeRef
              , state = stateRef
              , caseRef = Nothing
              , handoff = lowerHandoff routeSpec.on
              , plan = planG
              , meta = routeSpec.meta
              }
      modify' $ \st -> (st :: LowerSt sg ctx hf cmd) {
            routes = IM.insert (routeRefKey routeRef) routeEd st.routes
          }
      pure routeRef

    lowerControlPlan :: SR.ControlPlan st child
              -> StateT (LowerSt sg ctx hf cmd) (Either CompileErr) G.RoutePlan
    lowerControlPlan controlPlan = do
      targetG <- lowerTarget controlPlan.target
      waitG <- lowerWait controlPlan.wait
      joinG <- lowerJoin controlPlan.join
      let joinRefForSpawn = joinPlanRef joinG
      spawnG <- traverse (lowerSpawn joinRefForSpawn) controlPlan.spawn
      timersG <- traverse lowerTimerPlan controlPlan.timers
      let breaksG = lowerBreaks controlPlan.break
      pure
        G.RoutePlan
          { target = targetG
          , wait = waitG
          , spawn = spawnG
          , join = joinG
          , timers = timersG
          , break = breaksG
          }

    lowerTarget :: SR.TargetPlan st -> StateT (LowerSt sg ctx hf cmd) (Either CompileErr) G.RouteTarget
    lowerTarget = \case
      SR.StayTg -> pure G.StayRt
      SR.CompleteTg -> pure G.CompleteRt
      SR.FailTg msg -> pure (G.FailRt msg)
      SR.GotoTg targetKey -> G.GotoRt <$> resolveStateRef layout targetKey

    lowerWait :: SR.WaitPlan -> StateT (LowerSt sg ctx hf cmd) (Either CompileErr) G.WaitPlanG
    lowerWait = \case
      SR.WaitNoneWp -> pure G.WaitNoneWg
      SR.WaitSignalWp -> pure G.WaitSignalWg
      SR.WaitJoinWp joinName -> do
        joinRef <- resolveJoinRef defs joinName
        pure (G.WaitJoinWg joinRef)
      SR.WaitTimerWp timerName -> do
        timerRef <- resolveTimerRef defs timerName
        pure (G.WaitTimerWg timerRef)

    lowerJoin :: SR.JoinPlan -> StateT (LowerSt sg ctx hf cmd) (Either CompileErr) G.JoinPlanG
    lowerJoin = \case
      SR.JoinNoneJp -> pure G.JoinNoneJg
      SR.JoinAllJp joinName -> do
        joinRef <- resolveJoinRef defs joinName
        pure (G.JoinAllJg joinRef)
      SR.JoinAnyJp joinName -> do
        joinRef <- resolveJoinRef defs joinName
        pure (G.JoinAnyJg joinRef)
      SR.JoinCountJp joinName count -> do
        joinRef <- resolveJoinRef defs joinName
        pure (G.JoinCountJg joinRef count)

    lowerSpawn :: (Show child) =>
      Maybe JoinRef ->
      SR.SpawnPlan child ->
      StateT (LowerSt sg ctx hf cmd) (Either CompileErr) G.SpawnPlanG
    lowerSpawn joinRefMb spawnPlan =
      pure
        G.SpawnPlanG
          { child = T.pack (show spawnPlan.child)
          , input = spawnPlan.input
          , key = normalizeMaybeText spawnPlan.key
          , join = joinRefMb
          }

    lowerTimerPlan ::
      SR.TimerPlan ->
      StateT (LowerSt sg ctx hf cmd) (Either CompileErr) G.TimerPlanG
    lowerTimerPlan timerPlan = do
      timerRef <- resolveTimerRef defs timerPlan.key
      pure
        G.TimerPlanG
          { ref = timerRef
          , delay = timerPlan.delay
          , payload = timerPlan.payload
          }

    checkDuplicateCaseNames :: (Show st) => StateSpec st sg hf ctx cmd child -> StateT (LowerSt sg ctx hf cmd) (Either CompileErr) ()
    checkDuplicateCaseNames stateSpec =
      case firstDuplicate (fmap caseNameText (justCaseNames stateSpec.cases)) of
        Nothing -> pure ()
        Just dup ->
          lift $
            Left $
              DuplicateCaseEr $
                "state " <> renderStateLabel stateSpec.key stateSpec.name <> " has duplicate case name " <> quote dup

    checkDuplicateRoutes :: (Show st, Show hf) => StateSpec st sg hf ctx cmd child -> StateT (LowerSt sg ctx hf cmd) (Either CompileErr) ()
    checkDuplicateRoutes stateSpec =
      case firstDuplicate (fmap (lowerHandoff . (.on)) stateSpec.routes) of
        Nothing -> pure ()
        Just dup ->
          lift $
            Left $
              DuplicateRouteEr $
                "state " <> renderStateLabel stateSpec.key stateSpec.name <> " has duplicate route handoff " <> quote dup

    lookupStateNd :: StateRef -> StateT (LowerSt sg ctx hf cmd) (Either CompileErr) G.StateNd
    lookupStateNd stateRef = do
      st <- get
      case IM.lookup (stateRefKey stateRef) st.states of
        Just nd -> pure nd
        Nothing ->
          lift $
            Left $
              LoweringEr $
                "missing compiled state node for ref " <> renderStateRef stateRef

    putStateNd :: forall st sg ctx hf cmd. G.StateNd -> StateT (LowerSt sg ctx hf cmd) (Either CompileErr) ()
    putStateNd stateNd =
      modify' $ \st -> (st :: LowerSt sg ctx hf cmd) {
            states = IM.insert (stateRefKey stateNd.ref) stateNd st.states
          }

emptyLowerSt :: IntMap G.StateNd -> LowerSt sg ctx hf cmd
emptyLowerSt stateTable =
  LowerSt
    { nextCase = 0
    , nextRoute = 0
    , states = stateTable
    , cases = IM.empty
    , routes = IM.empty
    , exec = Exec.emptyExecTable
    }

freshCaseRef :: StateT (LowerSt sg ctx hf cmd) (Either CompileErr) CaseRef
freshCaseRef = do
  st <- get
  when (st.nextCase == maxBound) $
    lift $
      Left $
        LoweringEr "case reference space exhausted"
  let ref = mkCaseRef st.nextCase
  modify' $ \st0 -> st0 { nextCase = st0.nextCase + 1 }
  pure ref

freshRouteRef :: StateT (LowerSt sg ctx hf cmd) (Either CompileErr) RouteRef
freshRouteRef = do
  st <- get
  when (st.nextRoute == maxBound) $
    lift $
      Left $
        LoweringEr "route reference space exhausted"
  let ref = mkRouteRef st.nextRoute
  modify' $ \st0 -> st0 { nextRoute = st0.nextRoute + 1 }
  pure ref

resolveStateRef :: forall st s. (Show st, Ord st) => Layout st -> st -> StateT s (Either CompileErr) StateRef
resolveStateRef layout key0 =
  case M.lookup key0 layout.stateRefs of
    Just ref -> pure ref
    Nothing ->
      lift $
        Left $
          MissingTargetEr $
            "unknown state target " <> quote (tshow key0)

resolveJoinRef :: Defs -> JoinName -> StateT s (Either CompileErr) JoinRef
resolveJoinRef defs joinName =
  case M.lookup joinName defs.joinRefs of
    Just ref -> pure ref
    Nothing ->
      lift $
        Left $
          MissingTargetEr $
            "unknown join target " <> quote (joinNameText joinName)

resolveTimerRef :: Defs -> TimerName -> StateT s (Either CompileErr) TimerRef
resolveTimerRef defs timerName =
  case M.lookup timerName defs.timerRefs of
    Just ref -> pure ref
    Nothing ->
      lift $
        Left $
          MissingTargetEr $
            "unknown timer target " <> quote (timerNameText timerName)

needsRoutes :: StateSpec st sg hf ctx cmd child -> Bool
needsRoutes stateSpec = isJust stateSpec.entry || not (null stateSpec.cases)

justCaseNames :: [CaseSpec sg ctx hf cmd] -> [CaseName]
justCaseNames = foldr step []
  where
    step :: CaseSpec sg ctx hf cmd -> [CaseName] -> [CaseName]
    step caseSpec acc =
      case caseSpec.name of
        Nothing -> acc
        Just name0 -> name0 : acc

joinPlanRef :: G.JoinPlanG -> Maybe JoinRef
joinPlanRef joinPlan =
  case joinPlan of
    G.JoinNoneJg -> Nothing
    G.JoinAllJg ref -> Just ref
    G.JoinAnyJg ref -> Just ref
    G.JoinCountJg ref _ -> Just ref

lowerBreaks :: [SR.BreakPlan] -> [G.BreakPlanG]
lowerBreaks breakPlans = dedupBreaks (foldr step [] breakPlans)
  where
    step breakPlan acc =
      case breakPlan of
        SR.BreakNoneBp -> acc
        SR.BreakBeforeRouteBp -> G.BreakBeforeRouteBg : acc
        SR.BreakAfterRouteBp -> G.BreakAfterRouteBg : acc
        SR.BreakBeforeCommitBp -> G.BreakBeforeCommitBg : acc

dedupBreaks :: [G.BreakPlanG] -> [G.BreakPlanG]
dedupBreaks = go S.empty
  where
    go _ [] = []
    go seen (x : xs)
      | S.member x seen = go seen xs
      | otherwise = x : go (S.insert x seen) xs

lowerHandoff :: Show hf => hf -> Text
lowerHandoff = T.pack . show

normalizeMaybeText :: Maybe Text -> Maybe Text
normalizeMaybeText Nothing = Nothing
normalizeMaybeText (Just txt) =
  let txt1 = T.strip txt
  in if T.null txt1 then Nothing else Just txt1

firstDuplicate :: Ord a => [a] -> Maybe a
firstDuplicate = go S.empty
  where
    go _ [] = Nothing
    go seen (x : xs)
      | S.member x seen = Just x
      | otherwise = go (S.insert x seen) xs

digestGraph :: G.MachineGraph -> GraphDigest
digestGraph graph0 = unsafeGraphDigest (stableHashText (stableRenderGraphNoDigest graph0))

digestCompiled :: G.MachineGraph -> SpecDigest
digestCompiled graph0 = unsafeSpecDigest (stableHashText ("spec|" <> stableRenderGraph graph0))

unsafeGraphDigest :: Text -> GraphDigest
unsafeGraphDigest txt =
  case mkGraphDigest txt of
    Right digest0 -> digest0
    Left _ -> error "Hfsm.Compile: internal graph digest construction failed"

unsafeSpecDigest :: Text -> SpecDigest
unsafeSpecDigest txt =
  case mkSpecDigest txt of
    Right digest0 -> digest0
    Left _ -> error "Hfsm.Compile: internal spec digest construction failed"

zeroGraphDigest :: GraphDigest
zeroGraphDigest = unsafeGraphDigest (T.replicate digestLen "0")

stableHashText :: Text -> Text
stableHashText txt =
  let bytes = encodeUtf8 txt
      seeds =
        [ 0xcbf29ce484222325
        , 0xcbf29ce484222325 `xor` 0x9e3779b97f4a7c15
        , 0xcbf29ce484222325 `xor` 0xc2b2ae3d27d4eb4f
        , 0xcbf29ce484222325 `xor` 0x165667b19e3779f9
        ]
  in T.concat (fmap (word64Hex . fnv64 bytes) seeds)

fnv64 :: BS.ByteString -> Word64 -> Word64
fnv64 bytes seed0 = foldl' step seed0 (BS.unpack bytes)
  where
    prime = 1099511628211
    step acc w = (acc `xor` fromIntegral w) * prime

word64Hex :: Word64 -> Text
word64Hex w =
  let raw = T.pack (showHex w "")
      padLen = max 0 (16 - T.length raw)
  in T.replicate padLen "0" <> raw

stableRenderGraph :: G.MachineGraph -> Text
stableRenderGraph graph0 =
  T.intercalate
    "|"
    [ "machine=" <> machineNameText graph0.machine
    , "version=" <> machineVersionText graph0.version
    , "digest=" <> graphDigestText graph0.digest
    , "root=" <> renderRegionRef graph0.root
    , "regions=" <> renderRegionTable graph0.regions
    , "states=" <> renderStateTable graph0.states
    , "cases=" <> renderCaseTable graph0.cases
    , "routes=" <> renderRouteTable graph0.routes
    , "joins=" <> renderJoinTable graph0.joins
    , "timers=" <> renderTimerTable graph0.timers
    , "meta=" <> renderMeta graph0.meta
    ]

stableRenderGraphNoDigest :: G.MachineGraph -> Text
stableRenderGraphNoDigest graph0 =
  T.intercalate
    "|"
    [ "machine=" <> machineNameText graph0.machine
    , "version=" <> machineVersionText graph0.version
    , "root=" <> renderRegionRef graph0.root
    , "regions=" <> renderRegionTable graph0.regions
    , "states=" <> renderStateTable graph0.states
    , "cases=" <> renderCaseTable graph0.cases
    , "routes=" <> renderRouteTable graph0.routes
    , "joins=" <> renderJoinTable graph0.joins
    , "timers=" <> renderTimerTable graph0.timers
    , "meta=" <> renderMeta graph0.meta
    ]

renderRegionTable :: IntMap G.RegionNd -> Text
renderRegionTable = renderList renderRegionNd . IM.elems

renderStateTable :: IntMap G.StateNd -> Text
renderStateTable = renderList renderStateNd . IM.elems

renderCaseTable :: IntMap G.CaseNd -> Text
renderCaseTable = renderList renderCaseNd . IM.elems

renderRouteTable :: IntMap G.RouteEd -> Text
renderRouteTable = renderList renderRouteEd . IM.elems

renderJoinTable :: IntMap G.JoinNd -> Text
renderJoinTable = renderList renderJoinNd . IM.elems

renderTimerTable :: IntMap G.TimerNd -> Text
renderTimerTable = renderList renderTimerNd . IM.elems

renderRegionNd :: G.RegionNd -> Text
renderRegionNd regionNd =
  T.intercalate
    ","
    [ "ref=" <> renderRegionRef regionNd.ref
    , "parent=" <> maybe "-" renderStateRef regionNd.parent
    , "initial=" <> renderStateRef regionNd.initial
    , "states=" <> renderVector renderStateRef regionNd.states
    , "path=" <> renderRegionPath regionNd.path
    , "meta=" <> renderMeta regionNd.meta
    ]

renderStateNd :: G.StateNd -> Text
renderStateNd stateNd =
  T.intercalate
    ","
    [ "ref=" <> renderStateRef stateNd.ref
    , "parent=" <> renderRegionRef stateNd.parent
    , "name=" <> stateNameText stateNd.name
    , "kind=" <> T.pack (show stateNd.kind)
    , "entry=" <> maybe "-" renderCaseRef stateNd.entry
    , "cases=" <> renderVector renderCaseRef stateNd.cases
    , "routes=" <> renderVector renderRouteRef stateNd.routes
    , "child=" <> maybe "-" renderRegionRef stateNd.child
    , "path=" <> renderStatePath stateNd.path
    , "meta=" <> renderMeta stateNd.meta
    ]

renderCaseNd :: G.CaseNd -> Text
renderCaseNd caseNd =
  T.intercalate
    ","
    [ "ref=" <> renderCaseRef caseNd.ref
    , "state=" <> renderStateRef caseNd.state
    , "name=" <> maybe "-" caseNameText caseNd.name
    , "meta=" <> renderMeta caseNd.meta
    ]

renderRouteEd :: G.RouteEd -> Text
renderRouteEd routeEd =
  T.intercalate
    ","
    [ "ref=" <> renderRouteRef routeEd.ref
    , "state=" <> renderStateRef routeEd.state
    , "case=" <> maybe "-" renderCaseRef routeEd.caseRef
    , "handoff=" <> routeEd.handoff
    , "plan=" <> renderRoutePlan routeEd.plan
    , "meta=" <> renderMeta routeEd.meta
    ]

renderRoutePlan :: G.RoutePlan -> Text
renderRoutePlan routePlan =
  T.intercalate
    ","
    [ "target=" <> renderRouteTarget routePlan.target
    , "wait=" <> renderWaitPlan routePlan.wait
    , "spawn=" <> renderList renderSpawnPlan routePlan.spawn
    , "join=" <> renderJoinPlan routePlan.join
    , "timers=" <> renderList renderTimerPlan routePlan.timers
    , "break=" <> renderList renderBreakPlan routePlan.break
    ]

renderRouteTarget :: G.RouteTarget -> Text
renderRouteTarget routeTarget =
  case routeTarget of
    G.StayRt -> "stay"
    G.GotoRt stateRef -> "goto:" <> renderStateRef stateRef
    G.CompleteRt -> "complete"
    G.FailRt msg -> "fail:" <> msg

renderWaitPlan :: G.WaitPlanG -> Text
renderWaitPlan waitPlan =
  case waitPlan of
    G.WaitNoneWg -> "none"
    G.WaitSignalWg -> "signal"
    G.WaitJoinWg joinRef -> "join:" <> renderJoinRef joinRef
    G.WaitTimerWg timerRef -> "timer:" <> renderTimerRef timerRef

renderSpawnPlan :: G.SpawnPlanG -> Text
renderSpawnPlan spawnPlan =
  T.intercalate
    ","
    [ "child=" <> spawnPlan.child
    , "input=" <> stableRenderValue spawnPlan.input
    , "key=" <> maybe "-" id spawnPlan.key
    , "join=" <> maybe "-" renderJoinRef spawnPlan.join
    ]

renderJoinPlan :: G.JoinPlanG -> Text
renderJoinPlan joinPlan =
  case joinPlan of
    G.JoinNoneJg -> "none"
    G.JoinAllJg joinRef -> "all:" <> renderJoinRef joinRef
    G.JoinAnyJg joinRef -> "any:" <> renderJoinRef joinRef
    G.JoinCountJg joinRef count -> "count:" <> renderJoinRef joinRef <> ":" <> tshow count

renderJoinNd :: G.JoinNd -> Text
renderJoinNd joinNd =
  T.intercalate
    ","
    [ "ref=" <> renderJoinRef joinNd.ref
    , "name=" <> maybe "-" joinNameText joinNd.name
    , "mode=" <> renderJoinMode joinNd.mode
    , "meta=" <> renderMeta joinNd.meta
    ]

renderJoinMode :: G.JoinMode -> Text
renderJoinMode joinMode =
  case joinMode of
    G.AllJm -> "all"
    G.AnyJm -> "any"
    G.CountJm count -> "count:" <> tshow count

renderTimerPlan :: G.TimerPlanG -> Text
renderTimerPlan timerPlan =
  T.intercalate
    ","
    [ "ref=" <> renderTimerRef timerPlan.ref
    , "delay=" <> tshow timerPlan.delay
    , "payload=" <> maybe "-" stableRenderValue timerPlan.payload
    ]

renderTimerNd :: G.TimerNd -> Text
renderTimerNd timerNd =
  T.intercalate
    ","
    [ "ref=" <> renderTimerRef timerNd.ref
    , "name=" <> maybe "-" timerNameText timerNd.name
    , "meta=" <> renderMeta timerNd.meta
    ]

renderBreakPlan :: G.BreakPlanG -> Text
renderBreakPlan breakPlan =
  case breakPlan of
    G.BreakBeforeCaseBg -> "before-case"
    G.BreakAfterCaseBg -> "after-case"
    G.BreakBeforeRouteBg -> "before-route"
    G.BreakAfterRouteBg -> "after-route"
    G.BreakBeforeCommitBg -> "before-commit"

renderMeta :: MetaSpec -> Text
renderMeta metaSpec =
  T.intercalate
    ","
    [ "tags=" <> renderList id (S.toAscList metaSpec.tags)
    , "note=" <> maybe "-" id metaSpec.note
    , "owner=" <> maybe "-" id metaSpec.owner
    , "rank=" <> maybe "-" tshow metaSpec.rank
    , "spans=" <> renderList (T.pack . show) metaSpec.spans
    ]

stableRenderValue :: Value -> Text
stableRenderValue value =
  case value of
    Null -> "null"
    Bool False -> "false"
    Bool True -> "true"
    String txt -> T.pack (show txt)
    Number sc -> T.pack (formatScientific Generic Nothing sc)
    Array xs -> "[" <> T.intercalate "," (fmap stableRenderValue (V.toList xs)) <> "]"
    Object km ->
      let pairs =
            sortOn fst $
              fmap (\(k, v) -> (K.toText k, v)) (KM.toList km)
          renderPair (k, v) = T.pack (show k) <> ":" <> stableRenderValue v
      in "{" <> T.intercalate "," (fmap renderPair pairs) <> "}"

renderList :: (a -> Text) -> [a] -> Text
renderList renderOne xs = "[" <> T.intercalate ";" (fmap renderOne xs) <> "]"

renderVector :: (a -> Text) -> V.Vector a -> Text
renderVector renderOne = renderList renderOne . V.toList

renderStateLabel :: Show st => st -> StateName -> Text
renderStateLabel key0 stateName =
  quote (stateNameText stateName) <> " (" <> tshow key0 <> ")"

renderRegionLabel :: Maybe RegionName -> Text
renderRegionLabel Nothing = "<unnamed>"
renderRegionLabel (Just regionName) = quote (regionNameText regionName)

renderRegionRef :: RegionRef -> Text
renderRegionRef = tshow . regionRefWord32

renderStateRef :: StateRef -> Text
renderStateRef = tshow . stateRefWord32

renderCaseRef :: CaseRef -> Text
renderCaseRef = tshow . caseRefWord32

renderRouteRef :: RouteRef -> Text
renderRouteRef = tshow . routeRefWord32

renderJoinRef :: JoinRef -> Text
renderJoinRef = tshow . joinRefWord32

renderTimerRef :: TimerRef -> Text
renderTimerRef = tshow . timerRefWord32

regionRefKey :: RegionRef -> Int
regionRefKey = fromIntegral . regionRefWord32

stateRefKey :: StateRef -> Int
stateRefKey = fromIntegral . stateRefWord32

caseRefKey :: CaseRef -> Int
caseRefKey = fromIntegral . caseRefWord32

routeRefKey :: RouteRef -> Int
routeRefKey = fromIntegral . routeRefWord32

joinRefKey :: JoinRef -> Int
joinRefKey = fromIntegral . joinRefWord32

timerRefKey :: TimerRef -> Int
timerRefKey = fromIntegral . timerRefWord32

quote :: Text -> Text
quote txt = "'" <> txt <> "'"

tshow :: Show a => a -> Text
tshow = T.pack . show