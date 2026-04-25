{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE TupleSections #-}

module Hfsm.Validate.Version
  ( checkVersion
  ) where

import Data.Aeson (Value, encode)
import qualified Data.ByteString.Lazy as BL
import Data.Foldable (toList)
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (fromMaybe, mapMaybe)
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.Encoding as TE
import Data.Time (NominalDiffTime)

import Hfsm.Core.Name
  ( caseNameText
  , joinNameText
  , machineNameText
  , stateNameText
  , timerNameText
  )
import Hfsm.Core.Path (StatePath, renderStatePath, statePathToList)
import Hfsm.Core.Ref
  ( CaseRef
  , JoinRef
  , RouteRef
  , StateRef
  , TimerRef
  , caseRefWord32
  , joinRefWord32
  , routeRefWord32
  , stateRefWord32
  , timerRefWord32
  )
import Hfsm.Core.Version (MachineVersion, machineVersionText)
import Hfsm.Graph.Def
  ( BreakPlanG(..)
  , CaseNd(..)
  , JoinMode(..)
  , JoinNd(..)
  , JoinPlanG(..)
  , MachineGraph(..)
  , RouteEd(..)
  , RoutePlan(..)
  , RouteTarget(..)
  , SpawnPlanG(..)
  , StateNd(..)
  , TimerNd(..)
  , TimerPlanG(..)
  , WaitPlanG(..)
  )
import Hfsm.Validate.Error
  ( ValidateErr
  , atJoin
  , atMachine
  , atRoute
  , atState
  , errorErr
  , warnErr
  )

data VersionIx = VersionIx
  { statesByPath :: Map StatePathK [StateInfo]
  , statesByLeaf :: Map Text [StateInfo]
  , joinsByName :: Map Text [JoinInfo]
  , unnamedJoins :: [JoinInfo]
  , timersByName :: Map Text [TimerInfo]
  , unnamedTimers :: [TimerInfo]
  }

data StateInfo = StateInfo
  { ref :: StateRef
  , name :: Text
  , path :: StatePathK
  , rawPath :: StatePath
  , kind :: StateKindK
  , routes :: Map LocalRouteKey [RouteInfo]
  }

data JoinInfo = JoinInfo
  { ref :: JoinRef
  , name :: Maybe Text
  , mode :: JoinMode
  }

data TimerInfo = TimerInfo
  { ref :: TimerRef
  , name :: Maybe Text
  }

data RouteInfo = RouteInfo
  { ref :: RouteRef
  , key :: LocalRouteKey
  , target :: RouteTargetSig
  , wait :: WaitSig
  , join :: JoinSig
  , timers :: [TimerSig]
  }

newtype StatePathK = StatePathK [Text]
  deriving stock (Eq, Ord, Show, Read)

data LocalRouteKey = LocalRouteKey
  { caseName :: Maybe Text
  , handoff :: Text
  }
  deriving stock (Eq, Ord, Show, Read)

data RouteTargetSig
  = StayTs
  | GotoTs StatePathK
  | CompleteTs
  | FailTs Text
  deriving stock (Eq, Show, Read)

data WaitSig
  = WaitNoneWs
  | WaitSignalWs
  | WaitJoinWs (Maybe Text)
  | WaitTimerWs (Maybe Text)
  deriving stock (Eq, Show, Read)

data JoinSig
  = JoinNoneJs
  | JoinAllJs (Maybe Text)
  | JoinAnyJs (Maybe Text)
  | JoinCountJs (Maybe Text) Int
  deriving stock (Eq, Show, Read)

data TimerSig = TimerSig
  { name :: Maybe Text
  , delay :: NominalDiffTime
  , payload :: Maybe Value
  }
  deriving stock (Eq, Show, Read)

newtype StateKindK = StateKindK Text
  deriving stock (Eq, Ord, Show, Read)

checkVersion :: MachineGraph -> MachineGraph -> [ValidateErr]
checkVersion fromGraph toGraph
  | fromGraph.machine /= toGraph.machine = [machineMismatchErr fromGraph toGraph]
  | fromGraph.digest == toGraph.digest = []
  | otherwise =
      let fromIx = buildVersionIx fromGraph
          toIx = buildVersionIx toGraph
      in concat
           [ sameVersionDifferentGraphWarns fromGraph toGraph
           , duplicateStatePathErrs fromGraph fromIx
           , duplicateStatePathErrs toGraph toIx
           , duplicateRouteKeyErrs fromGraph fromIx
           , duplicateRouteKeyErrs toGraph toIx
           , unnamedJoinWarns fromGraph fromIx
           , unnamedJoinWarns toGraph toIx
           , duplicateJoinNameErrs fromGraph fromIx
           , duplicateJoinNameErrs toGraph toIx
           , unnamedTimerWarns fromGraph toIxEmptyGuard fromIx
           , unnamedTimerWarns toGraph toIxEmptyGuard toIx
           , duplicateTimerNameErrs fromGraph fromIx
           , duplicateTimerNameErrs toGraph toIx
           , stateContinuityErrs fromGraph toGraph fromIx toIx
           , joinContinuityErrs fromGraph toGraph fromIx toIx
           , timerContinuityErrs fromGraph toGraph fromIx toIx
           ]
  where
    toIxEmptyGuard = ()

buildVersionIx :: MachineGraph -> VersionIx
buildVersionIx graph =
  let stateInfos = fmap (mkStateInfo graph) (IM.elems graph.states)
      joinInfos = fmap mkJoinInfo (IM.elems graph.joins)
      timerInfos = fmap mkTimerInfo (IM.elems graph.timers)
  in VersionIx
       { statesByPath = groupedBy (.path) stateInfos
       , statesByLeaf = groupedBy (.name) stateInfos
       , joinsByName = groupedByMaybe (.name) joinInfos
       , unnamedJoins = filter (isNothingText . (.name)) joinInfos
       , timersByName = groupedByMaybe (.name) timerInfos
       , unnamedTimers = filter (isNothingText . (.name)) timerInfos
       }

mkStateInfo :: MachineGraph -> StateNd -> StateInfo
mkStateInfo graph stateNd =
  let routeInfos = fmap (mkRouteInfo graph) (resolveRoutes graph stateNd.routes)
  in StateInfo
       { ref = stateNd.ref
       , name = stateNameText stateNd.name
       , path = statePathKey graph stateNd.path
       , rawPath = stateNd.path
       , kind = StateKindK (T.pack (show stateNd.kind))
       , routes = groupedBy (.key) routeInfos
       }

mkJoinInfo :: JoinNd -> JoinInfo
mkJoinInfo joinNd =
  JoinInfo
    { ref = joinNd.ref
    , name = fmap joinNameText joinNd.name
    , mode = joinNd.mode
    }

mkTimerInfo :: TimerNd -> TimerInfo
mkTimerInfo timerNd =
  TimerInfo
    { ref = timerNd.ref
    , name = fmap timerNameText timerNd.name
    }

mkRouteInfo :: MachineGraph -> RouteEd -> RouteInfo
mkRouteInfo graph routeEd =
  RouteInfo
    { ref = routeEd.ref
    , key = LocalRouteKey
        { caseName = resolveRouteCaseName graph routeEd.caseRef
        , handoff = T.strip routeEd.handoff
        }
    , target = routeTargetSig graph routeEd.plan.target
    , wait = waitSig graph routeEd.plan.wait
    , join = joinSig graph routeEd.plan.join
    , timers = sortTimerSigs (fmap (timerSig graph) routeEd.plan.timers)
    }

machineMismatchErr :: MachineGraph -> MachineGraph -> ValidateErr
machineMismatchErr fromGraph toGraph =
  atMachine $
    errorErr
      "version.machine-mismatch"
      ("cannot compare machine " <> quote (machineNameText fromGraph.machine) <>
       " at version " <> quote (machineVersionText fromGraph.version) <>
       " with machine " <> quote (machineNameText toGraph.machine) <>
       " at version " <> quote (machineVersionText toGraph.version) <>
       "; version compatibility checks require identical machine names")
      Nothing

sameVersionDifferentGraphWarns :: MachineGraph -> MachineGraph -> [ValidateErr]
sameVersionDifferentGraphWarns fromGraph toGraph
  | fromGraph.version == toGraph.version && fromGraph.digest /= toGraph.digest =
      [ atMachine $
          warnErr
            "version.same-version-different-graph"
            ("machine " <> quote (machineNameText fromGraph.machine) <>
             " is being compared at the same version " <> quote (machineVersionText fromGraph.version) <>
             " but with different compiled graphs; live-instance compatibility should be reviewed explicitly")
            Nothing
      ]
  | otherwise = []

duplicateStatePathErrs :: MachineGraph -> VersionIx -> [ValidateErr]
duplicateStatePathErrs graph ix =
  fmap mkErr (duplicateEntries ix.statesByPath)
  where
    mkErr :: (StatePathK, [StateInfo]) -> ValidateErr
    mkErr (pathK, infos) =
      atMachine $
        errorErr
          "version.ambiguous-state-path"
          ("version " <> quote (machineVersionText graph.version) <>
           " contains multiple states with the same semantic path " <> quote (renderStatePathK pathK) <>
           " (refs: " <> renderStateRefs infos <> "); stable-key continuity requires unique semantic state paths")
          Nothing

duplicateRouteKeyErrs :: MachineGraph -> VersionIx -> [ValidateErr]
duplicateRouteKeyErrs graph ix =
  concatMap stateRouteErrs (uniqueStateInfos ix)
  where
    stateRouteErrs :: StateInfo -> [ValidateErr]
    stateRouteErrs stateInfo =
      fmap (mkErr stateInfo) (duplicateEntries stateInfo.routes)

    mkErr :: StateInfo -> (LocalRouteKey, [RouteInfo]) -> ValidateErr
    mkErr stateInfo (routeKey, infos) =
      atState stateInfo.ref $
        errorErr
          "version.ambiguous-route-key"
          ("state " <> quote (renderStatePathK stateInfo.path) <>
           " in version " <> quote (machineVersionText graph.version) <>
           " contains multiple routes with continuity key " <> quote (renderRouteKey routeKey) <>
           " (refs: " <> renderRouteRefs infos <> "); add case names or make handoffs unique to preserve route continuity across versions")
          Nothing

unnamedJoinWarns :: MachineGraph -> VersionIx -> [ValidateErr]
unnamedJoinWarns graph ix =
  case ix.unnamedJoins of
    [] -> []
    joinInfo : _ ->
      [ atJoin joinInfo.ref $
          warnErr
            "version.unnamed-join"
            ("version " <> quote (machineVersionText graph.version) <>
             " contains unnamed joins (refs: " <> renderJoinRefs ix.unnamedJoins <> "); join continuity checks are approximate without stable join names")
            Nothing
      ]

duplicateJoinNameErrs :: MachineGraph -> VersionIx -> [ValidateErr]
duplicateJoinNameErrs graph ix =
  fmap mkErr (duplicateEntries ix.joinsByName)
  where
    mkErr :: (Text, [JoinInfo]) -> ValidateErr
    mkErr (joinName, infos) =
      let firstRef = headRefJoin infos
      in atJoin firstRef $
           errorErr
             "version.duplicate-join-name"
             ("version " <> quote (machineVersionText graph.version) <>
              " contains multiple joins named " <> quote joinName <>
              " (refs: " <> renderJoinRefs infos <> "); stable join continuity requires unique join names")
             Nothing

unnamedTimerWarns :: MachineGraph -> () -> VersionIx -> [ValidateErr]
unnamedTimerWarns graph _ ix =
  case ix.unnamedTimers of
    [] -> []
    _ ->
      [ atMachine $
          warnErr
            "version.unnamed-timer"
            ("version " <> quote (machineVersionText graph.version) <>
             " contains unnamed timers (refs: " <> renderTimerRefs ix.unnamedTimers <> "); timer continuity checks are approximate without stable timer names")
            Nothing
      ]

duplicateTimerNameErrs :: MachineGraph -> VersionIx -> [ValidateErr]
duplicateTimerNameErrs graph ix =
  fmap mkErr (duplicateEntries ix.timersByName)
  where
    mkErr :: (Text, [TimerInfo]) -> ValidateErr
    mkErr (timerName, infos) =
      atMachine $
        errorErr
          "version.duplicate-timer-name"
          ("version " <> quote (machineVersionText graph.version) <>
           " contains multiple timers named " <> quote timerName <>
           " (refs: " <> renderTimerRefs infos <> "); stable timer continuity requires unique timer names")
          Nothing

stateContinuityErrs :: MachineGraph -> MachineGraph -> VersionIx -> VersionIx -> [ValidateErr]
stateContinuityErrs fromGraph toGraph fromIx toIx =
  concatMap checkState (uniqueStateInfos fromIx)
  where
    checkState :: StateInfo -> [ValidateErr]
    checkState fromState =
      case M.lookup fromState.path toIx.statesByPath of
        Just [toState] -> statePairErrs fromGraph toGraph fromState toState
        Just _ -> []
        Nothing -> [missingStateErr fromGraph toGraph fromIx toIx fromState]

statePairErrs :: MachineGraph -> MachineGraph -> StateInfo -> StateInfo -> [ValidateErr]
statePairErrs fromGraph toGraph fromState toState =
  kindErrs fromState toState <> routeContinuityErrs fromGraph toGraph fromState toState

kindErrs :: StateInfo -> StateInfo -> [ValidateErr]
kindErrs fromState toState
  | fromState.kind == toState.kind = []
  | otherwise =
      [ atState fromState.ref $
          errorErr
            "version.state-kind-changed"
            ("state " <> quote (renderStatePathK fromState.path) <>
             " changed kind from " <> quote (renderStateKindK fromState.kind) <>
             " to " <> quote (renderStateKindK toState.kind) <>
             "; changing the kind of a live state requires an explicit migration plan")
            Nothing
      ]

missingStateErr :: MachineGraph -> MachineGraph -> VersionIx -> VersionIx -> StateInfo -> ValidateErr
missingStateErr fromGraph toGraph _fromIx toIx fromState =
  case uniqueLeafMatch toIx fromState.name of
    Just toState ->
      atState fromState.ref $
        errorErr
          "version.state-moved"
          ("state " <> quote fromState.name <>
           " moved from " <> quote (renderStatePathK fromState.path) <>
           " in version " <> quote (machineVersionText fromGraph.version) <>
           " to " <> quote (renderStatePathK toState.path) <>
           " in version " <> quote (machineVersionText toGraph.version) <>
           "; moving a potentially active state requires an explicit migration plan")
          Nothing
    Nothing ->
      atState fromState.ref $
        errorErr
          "version.state-removed"
          ("state " <> quote (renderStatePathK fromState.path) <>
           " from version " <> quote (machineVersionText fromGraph.version) <>
           " is not present in version " <> quote (machineVersionText toGraph.version) <>
           "; live instances may still be active in that state and need migration")
          Nothing

routeContinuityErrs :: MachineGraph -> MachineGraph -> StateInfo -> StateInfo -> [ValidateErr]
routeContinuityErrs fromGraph toGraph fromState toState =
  concatMap checkRoute (uniqueRouteInfos fromState)
  where
    checkRoute :: RouteInfo -> [ValidateErr]
    checkRoute fromRoute =
      case M.lookup fromRoute.key toState.routes of
        Just [toRoute] -> routePairErrs fromGraph toGraph fromState toState fromRoute toRoute
        Just _ -> []
        Nothing ->
          [ atRoute fromRoute.ref $
              errorErr
                "version.route-removed"
                ("route " <> quote (renderRouteKey fromRoute.key) <>
                 " from state " <> quote (renderStatePathK fromState.path) <>
                 " is not present in version " <> quote (machineVersionText toGraph.version) <>
                 "; live instances may still emit that handoff and require migration")
                Nothing
          ]

routePairErrs :: MachineGraph -> MachineGraph -> StateInfo -> StateInfo -> RouteInfo -> RouteInfo -> [ValidateErr]
routePairErrs fromGraph toGraph fromState _toState fromRoute toRoute =
  concat
    [ targetErrs fromGraph toGraph fromState fromRoute toRoute
    , waitErrs toGraph fromState fromRoute toRoute
    , joinErrs toGraph fromState fromRoute toRoute
    , timerErrs toGraph fromState fromRoute toRoute
    ]

targetErrs :: MachineGraph -> MachineGraph -> StateInfo -> RouteInfo -> RouteInfo -> [ValidateErr]
targetErrs fromGraph toGraph fromState fromRoute toRoute =
  case (fromRoute.target, toRoute.target) of
    (StayTs, StayTs) -> []
    (CompleteTs, CompleteTs) -> []
    (GotoTs fromTarget, GotoTs toTarget)
      | fromTarget == toTarget -> []
      | otherwise ->
          [ atRoute fromRoute.ref $
              errorErr
                "version.route-target-changed"
                ("route " <> quote (renderRouteKey fromRoute.key) <>
                 " from state " <> quote (renderStatePathK fromState.path) <>
                 " changed target from " <> quote (renderStatePathK fromTarget) <>
                 " in version " <> quote (machineVersionText fromGraph.version) <>
                 " to " <> quote (renderStatePathK toTarget) <>
                 " in version " <> quote (machineVersionText toGraph.version) <>
                 "; redirecting an existing live route requires migration")
                Nothing
          ]
    (FailTs fromMsg, FailTs toMsg)
      | fromMsg == toMsg -> []
      | otherwise ->
          [ atRoute fromRoute.ref $
              warnErr
                "version.route-fail-changed"
                ("route " <> quote (renderRouteKey fromRoute.key) <>
                 " from state " <> quote (renderStatePathK fromState.path) <>
                 " changed failure text from " <> quote fromMsg <>
                 " to " <> quote toMsg <>
                 "; review whether any consumers depend on the failure payload")
                Nothing
          ]
    _ ->
      [ atRoute fromRoute.ref $
          errorErr
            "version.route-target-kind-changed"
            ("route " <> quote (renderRouteKey fromRoute.key) <>
             " from state " <> quote (renderStatePathK fromState.path) <>
             " changed target kind from " <> quote (renderRouteTargetSig fromRoute.target) <>
             " to " <> quote (renderRouteTargetSig toRoute.target) <>
             "; changing the control result of an existing live route requires migration")
            Nothing
      ]

waitErrs :: MachineGraph -> StateInfo -> RouteInfo -> RouteInfo -> [ValidateErr]
waitErrs toGraph fromState fromRoute toRoute
  | fromRoute.wait == toRoute.wait = []
  | otherwise =
      [ atRoute fromRoute.ref $
          errorErr
            "version.route-wait-changed"
            ("route " <> quote (renderRouteKey fromRoute.key) <>
             " from state " <> quote (renderStatePathK fromState.path) <>
             " changed wait behaviour in version " <> quote (machineVersionText toGraph.version) <>
             " from " <> quote (renderWaitSig fromRoute.wait) <>
             " to " <> quote (renderWaitSig toRoute.wait) <>
             "; wait-surface changes require migration for live instances")
            Nothing
      ]

joinErrs :: MachineGraph -> StateInfo -> RouteInfo -> RouteInfo -> [ValidateErr]
joinErrs toGraph fromState fromRoute toRoute
  | fromRoute.join == toRoute.join = []
  | otherwise =
      [ atRoute fromRoute.ref $
          errorErr
            "version.route-join-changed"
            ("route " <> quote (renderRouteKey fromRoute.key) <>
             " from state " <> quote (renderStatePathK fromState.path) <>
             " changed join behaviour in version " <> quote (machineVersionText toGraph.version) <>
             " from " <> quote (renderJoinSig fromRoute.join) <>
             " to " <> quote (renderJoinSig toRoute.join) <>
             "; join-surface changes require migration for live instances")
            Nothing
      ]

timerErrs :: MachineGraph -> StateInfo -> RouteInfo -> RouteInfo -> [ValidateErr]
timerErrs toGraph fromState fromRoute toRoute
  | fromRoute.timers == toRoute.timers = []
  | otherwise =
      [ atRoute fromRoute.ref $
          errorErr
            "version.route-timers-changed"
            ("route " <> quote (renderRouteKey fromRoute.key) <>
             " from state " <> quote (renderStatePathK fromState.path) <>
             " changed scheduled timers in version " <> quote (machineVersionText toGraph.version) <>
             " from " <> quote (renderTimerSigs fromRoute.timers) <>
             " to " <> quote (renderTimerSigs toRoute.timers) <>
             "; timer changes can affect live waits and require migration")
            Nothing
      ]

joinContinuityErrs :: MachineGraph -> MachineGraph -> VersionIx -> VersionIx -> [ValidateErr]
joinContinuityErrs fromGraph toGraph fromIx toIx =
  concatMap checkJoin (uniqueJoinInfos fromIx)
  where
    checkJoin :: JoinInfo -> [ValidateErr]
    checkJoin fromJoin =
      case fromJoin.name of
        Nothing -> []
        Just joinName ->
          case M.lookup joinName toIx.joinsByName of
            Just [toJoin]
              | fromJoin.mode == toJoin.mode -> []
              | otherwise ->
                  [ atJoin fromJoin.ref $
                      errorErr
                        "version.join-mode-changed"
                        ("join " <> quote joinName <>
                         " changed mode from " <> quote (renderJoinMode fromJoin.mode) <>
                         " in version " <> quote (machineVersionText fromGraph.version) <>
                         " to " <> quote (renderJoinMode toJoin.mode) <>
                         " in version " <> quote (machineVersionText toGraph.version) <>
                         "; changing join completion rules requires migration")
                        Nothing
                  ]
            Just _ -> []
            Nothing ->
              [ atJoin fromJoin.ref $
                  errorErr
                    "version.join-removed"
                    ("join " <> quote joinName <>
                     " from version " <> quote (machineVersionText fromGraph.version) <>
                     " is not present in version " <> quote (machineVersionText toGraph.version) <>
                     "; routes waiting on that join require migration")
                    Nothing
              ]

timerContinuityErrs :: MachineGraph -> MachineGraph -> VersionIx -> VersionIx -> [ValidateErr]
timerContinuityErrs fromGraph toGraph fromIx toIx =
  concatMap checkTimer (uniqueTimerInfos fromIx)
  where
    checkTimer :: TimerInfo -> [ValidateErr]
    checkTimer fromTimer =
      case fromTimer.name of
        Nothing -> []
        Just timerName ->
          case M.lookup timerName toIx.timersByName of
            Just [_] -> []
            Just _ -> []
            Nothing ->
              [ atMachine $
                  errorErr
                    "version.timer-removed"
                    ("timer " <> quote timerName <>
                     " from version " <> quote (machineVersionText fromGraph.version) <>
                     " is not present in version " <> quote (machineVersionText toGraph.version) <>
                     "; routes waiting on or scheduling that timer require migration")
                    Nothing
              ]

resolveRoutes :: Foldable f => MachineGraph -> f RouteRef -> [RouteEd]
resolveRoutes graph refs =
  mapMaybe (lookupRoute graph) (toList refs)

lookupRoute :: MachineGraph -> RouteRef -> Maybe RouteEd
lookupRoute graph routeRef =
  IM.lookup (routeRefKey routeRef) graph.routes

lookupState :: MachineGraph -> StateRef -> Maybe StateNd
lookupState graph stateRef =
  IM.lookup (stateRefKey stateRef) graph.states

lookupCase :: MachineGraph -> CaseRef -> Maybe CaseNd
lookupCase graph caseRef =
  IM.lookup (caseRefKey caseRef) graph.cases

lookupJoinName :: MachineGraph -> JoinRef -> Maybe Text
lookupJoinName graph joinRef =
  lookupJoin graph joinRef >>= fmap joinNameText . (.name)

lookupJoin :: MachineGraph -> JoinRef -> Maybe JoinNd
lookupJoin graph joinRef =
  IM.lookup (joinRefKey joinRef) graph.joins

lookupTimerName :: MachineGraph -> TimerRef -> Maybe Text
lookupTimerName graph timerRef =
  lookupTimer graph timerRef >>= fmap timerNameText . (.name)

lookupTimer :: MachineGraph -> TimerRef -> Maybe TimerNd
lookupTimer graph timerRef =
  IM.lookup (timerRefKey timerRef) graph.timers

resolveRouteCaseName :: MachineGraph -> Maybe CaseRef -> Maybe Text
resolveRouteCaseName graph mbCaseRef =
  case mbCaseRef of
    Nothing -> Nothing
    Just caseRef ->
      case lookupCase graph caseRef of
        Just caseNd -> fmap caseNameText caseNd.name
        Nothing -> Just ("#case-" <> tshow (caseRefWord32 caseRef))

routeTargetSig :: MachineGraph -> RouteTarget -> RouteTargetSig
routeTargetSig graph routeTarget =
  case routeTarget of
    StayTg -> StayTs
    GotoTg stateRef ->
      case lookupState graph stateRef of
        Just stateNd -> GotoTs (statePathKey graph stateNd.path)
        Nothing -> GotoTs (StatePathK ["#state-" <> tshow (stateRefWord32 stateRef)])
    CompleteTg -> CompleteTs
    FailTg msg -> FailTs msg

waitSig :: MachineGraph -> WaitPlanG -> WaitSig
waitSig graph waitPlan =
  case waitPlan of
    WaitNoneWg -> WaitNoneWs
    WaitSignalWg -> WaitSignalWs
    WaitJoinWg joinRef -> WaitJoinWs (lookupJoinName graph joinRef)
    WaitTimerWg timerRef -> WaitTimerWs (lookupTimerName graph timerRef)

joinSig :: MachineGraph -> JoinPlanG -> JoinSig
joinSig graph joinPlan =
  case joinPlan of
    JoinNoneJg -> JoinNoneJs
    JoinAllJg joinRef -> JoinAllJs (lookupJoinName graph joinRef)
    JoinAnyJg joinRef -> JoinAnyJs (lookupJoinName graph joinRef)
    JoinCountJg joinRef n -> JoinCountJs (lookupJoinName graph joinRef) n

timerSig :: MachineGraph -> TimerPlanG -> TimerSig
timerSig graph timerPlan =
  TimerSig
    { name = lookupTimerName graph timerPlan.ref
    , delay = timerPlan.delay
    , payload = timerPlan.payload
    }

sortTimerSigs :: [TimerSig] -> [TimerSig]
sortTimerSigs =
  sortOn (\timerInfo -> (timerInfo.name, timerInfo.delay, fmap renderValueStable timerInfo.payload))

statePathKey :: MachineGraph -> StatePath -> StatePathK
statePathKey graph path =
  StatePathK (fmap (resolveStatePathPart graph) (statePathToList path))

resolveStatePathPart :: MachineGraph -> StateRef -> Text
resolveStatePathPart graph stateRef =
  case lookupState graph stateRef of
    Just stateNd -> stateNameText stateNd.name
    Nothing -> "#state-" <> tshow (stateRefWord32 stateRef)

uniqueStateInfos :: VersionIx -> [StateInfo]
uniqueStateInfos ix =
  fmap snd (uniqueEntries ix.statesByPath)

uniqueJoinInfos :: VersionIx -> [JoinInfo]
uniqueJoinInfos ix =
  fmap snd (uniqueEntries ix.joinsByName)

uniqueTimerInfos :: VersionIx -> [TimerInfo]
uniqueTimerInfos ix =
  fmap snd (uniqueEntries ix.timersByName)

uniqueRouteInfos :: StateInfo -> [RouteInfo]
uniqueRouteInfos stateInfo =
  fmap snd (uniqueEntries stateInfo.routes)

uniqueLeafMatch :: VersionIx -> Text -> Maybe StateInfo
uniqueLeafMatch ix leafName =
  M.lookup leafName ix.statesByLeaf >>= onlyOne

groupedBy :: Ord k => (a -> k) -> [a] -> Map k [a]
groupedBy toKey =
  foldr (\x acc -> M.insertWith (<>) (toKey x) [x] acc) M.empty

groupedByMaybe :: Ord k => (a -> Maybe k) -> [a] -> Map k [a]
groupedByMaybe toKey =
  foldr step M.empty
  where
    step x acc =
      case toKey x of
        Nothing -> acc
        Just k -> M.insertWith (<>) k [x] acc

duplicateEntries :: Map k [a] -> [(k, [a])]
duplicateEntries =
  filter (\(_, xs) -> length xs > 1) . M.toList

uniqueEntries :: Map k [a] -> [(k, a)]
uniqueEntries =
  mapMaybe (\(k, xs) -> fmap (k,) (onlyOne xs)) . M.toList

onlyOne :: [a] -> Maybe a
onlyOne xs =
  case xs of
    [x] -> Just x
    _ -> Nothing

isNothingText :: Maybe Text -> Bool
isNothingText mbTxt =
  case mbTxt of
    Nothing -> True
    Just _ -> False

renderStatePathK :: StatePathK -> Text
renderStatePathK (StatePathK parts) =
  case parts of
    [] -> "<unknown>"
    _ -> T.intercalate "/" parts

renderRouteKey :: LocalRouteKey -> Text
renderRouteKey routeKey =
  case routeKey.caseName of
    Nothing -> "handoff " <> quote routeKey.handoff
    Just caseName -> "case " <> quote caseName <> ", handoff " <> quote routeKey.handoff

renderRouteTargetSig :: RouteTargetSig -> Text
renderRouteTargetSig targetSig =
  case targetSig of
    StayTs -> "stay"
    GotoTs pathK -> "goto " <> renderStatePathK pathK
    CompleteTs -> "complete"
    FailTs msg -> "fail " <> quote msg

renderWaitSig :: WaitSig -> Text
renderWaitSig waitInfo =
  case waitInfo of
    WaitNoneWs -> "no-wait"
    WaitSignalWs -> "wait-signal"
    WaitJoinWs mbJoin ->
      "wait-join " <> renderMaybeStableName mbJoin "<unnamed-join>"
    WaitTimerWs mbTimer ->
      "wait-timer " <> renderMaybeStableName mbTimer "<unnamed-timer>"

renderJoinSig :: JoinSig -> Text
renderJoinSig joinInfo =
  case joinInfo of
    JoinNoneJs -> "no-join"
    JoinAllJs mbJoin ->
      "join-all " <> renderMaybeStableName mbJoin "<unnamed-join>"
    JoinAnyJs mbJoin ->
      "join-any " <> renderMaybeStableName mbJoin "<unnamed-join>"
    JoinCountJs mbJoin n ->
      "join-count " <> renderMaybeStableName mbJoin "<unnamed-join>" <> " " <> tshow n

renderTimerSigs :: [TimerSig] -> Text
renderTimerSigs timerInfos =
  case timerInfos of
    [] -> "[]"
    _ -> "[" <> T.intercalate ", " (fmap renderTimerSig timerInfos) <> "]"

renderTimerSig :: TimerSig -> Text
renderTimerSig timerInfo =
  renderMaybeStableName timerInfo.name "<unnamed-timer>" <>
  "@" <> tshow timerInfo.delay <>
  maybe "" (\payload -> ":" <> renderValueStable payload) timerInfo.payload

renderJoinMode :: JoinMode -> Text
renderJoinMode joinMode =
  case joinMode of
    AllJm -> "all"
    AnyJm -> "any"
    CountJm n -> "count " <> tshow n

renderStateKindK :: StateKindK -> Text
renderStateKindK (StateKindK txt) = txt

renderStateRefs :: [StateInfo] -> Text
renderStateRefs infos =
  T.intercalate ", " (fmap (tshow . stateRefWord32 . (.ref)) infos)

renderRouteRefs :: [RouteInfo] -> Text
renderRouteRefs infos =
  T.intercalate ", " (fmap (tshow . routeRefWord32 . (.ref)) infos)

renderJoinRefs :: [JoinInfo] -> Text
renderJoinRefs infos =
  T.intercalate ", " (fmap (tshow . joinRefWord32 . (.ref)) infos)

renderTimerRefs :: [TimerInfo] -> Text
renderTimerRefs infos =
  T.intercalate ", " (fmap (tshow . timerRefWord32 . (.ref)) infos)

renderMaybeStableName :: Maybe Text -> Text -> Text
renderMaybeStableName mbName fallback =
  fromMaybe fallback mbName

renderValueStable :: Value -> Text
renderValueStable =
  TE.decodeUtf8 . BL.toStrict . encode

headRefJoin :: [JoinInfo] -> JoinRef
headRefJoin infos =
  case infos of
    x : _ -> x.ref
    [] -> error "Hfsm.Validate.Version.headRefJoin: empty join list"

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