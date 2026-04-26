{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hfsm.Migrate.Diff
  ( GraphDiff(..)
  , diffGraph
  ) where

import Control.DeepSeq (NFData)

import qualified Data.IntMap.Strict as IM
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, isNothing)
import qualified Data.Scientific as Sci
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Vector as V

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON, Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM

import Hfsm.Core.Name
  ( caseNameText
  , joinNameText
  , machineNameText
  , stateNameText
  , timerNameText
  )
import Hfsm.Core.Path (StatePath, statePathToList)
import Hfsm.Core.Ref
  ( CaseRef
  , JoinRef
  , RegionRef
  , RouteRef
  , StateRef
  , TimerRef
  , caseRefWord32
  , joinRefWord32
  , regionRefWord32
  , routeRefWord32
  , stateRefWord32
  , timerRefWord32
  )
import Hfsm.Core.Version (machineVersionText)
import Hfsm.Graph.Def
  ( BreakPlanG(..)
  , CaseNd(..)
  , JoinMode(..)
  , JoinNd(..)
  , JoinPlanG(..)
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

data GraphDiff
  = StateAddedGd StateRef
  | StateRemovedGd StateRef
  | StateMovedGd StateRef
  | RouteChangedGd RouteRef
  | JoinChangedGd JoinRef
  | TimerChangedGd TimerRef
  | IncompatibleGd Text
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data StateIx = StateIx
  { infos :: [StateInfo]
  , byPath :: M.Map StatePathK StateInfo
  , byLeaf :: M.Map Text [StateInfo]
  , diffs :: [GraphDiff]
  }

data StateInfo = StateInfo
  { ref :: StateRef
  , pathK :: StatePathK
  , leaf :: Text
  , kind :: Text
  , childInitial :: Maybe StateRef
  , routes :: M.Map LocalRouteKey RouteRaw
  }

data RouteRaw = RouteRaw
  { ref :: RouteRef
  , route :: RouteEd
  }

newtype StatePathK = StatePathK [Text]
  deriving stock (Eq, Ord, Show, Read)

data LocalRouteKey = LocalRouteKey
  { caseKey :: Text
  , handoff :: Text
  }
  deriving stock (Eq, Ord, Show, Read)

data RouteSig = RouteSig
  { target :: Text
  , wait :: Text
  , spawn :: [SpawnSig]
  , join :: Text
  , timers :: [TimerSig]
  , breaks :: [BreakPlanG]
  }
  deriving stock (Eq, Ord, Show, Read)

data SpawnSig = SpawnSig
  { child :: Text
  , input :: Text
  , key :: Maybe Text
  , join :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Read)

data TimerSig = TimerSig
  { timer :: Text
  , delay :: Rational
  , payload :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Read)

data Pairing = Pairing
  { pairs :: [(StateInfo, StateInfo)]
  , diffs :: [GraphDiff]
  }

diffGraph :: MachineGraph -> MachineGraph -> [GraphDiff]
diffGraph oldGraph newGraph
  | oldGraph.digest == newGraph.digest = []
  | oldGraph.machine /= newGraph.machine = [IncompatibleGd (machineMismatchMsg oldGraph newGraph)]
  | otherwise =
      let oldIx = buildStateIx oldGraph
          newIx = buildStateIx newGraph
          pairing = pairStates oldIx newIx
          (oldCanon, newCanon) = buildCanonMaps pairing.pairs
          rawDiffs =
            baseCompatibilityDiffs oldGraph newGraph
              <> oldIx.diffs
              <> newIx.diffs
              <> pairing.diffs
              <> diffRootInitial oldGraph newGraph oldCanon newCanon
              <> diffPairedStates oldGraph newGraph oldCanon newCanon pairing.pairs
              <> diffJoins oldGraph newGraph
              <> diffTimers oldGraph newGraph
      in stableDiffs rawDiffs

baseCompatibilityDiffs :: MachineGraph -> MachineGraph -> [GraphDiff]
baseCompatibilityDiffs oldGraph newGraph =
  sameVersionDifferentDigestDiffs oldGraph newGraph

sameVersionDifferentDigestDiffs :: MachineGraph -> MachineGraph -> [GraphDiff]
sameVersionDifferentDigestDiffs oldGraph newGraph
  | oldGraph.version == newGraph.version =
      [IncompatibleGd ("machine version " <> quote (machineVersionText oldGraph.version) <> " has different graph digests across compared graphs")]
  | otherwise = []


buildStateIx :: MachineGraph -> StateIx
buildStateIx graph =
  let
    builtInfos = fmap (mkStateInfo graph) (stateNodes graph)
    infos = fmap fst builtInfos
    stateDiffs = concatMap snd builtInfos
    pathGroups = groupedBy (.pathK) infos
    pathDiffs =
      [ IncompatibleGd (ambiguousStatePathMsg graph pathKey groupedInfos)
      | (pathKey, groupedInfos) <- M.toAscList pathGroups
      , length groupedInfos > 1
      ]
    byPath = M.mapMaybe onlyOne pathGroups
    byLeaf = groupedBy (.leaf) infos
  in
  StateIx
       { infos = infos
       , byPath = byPath
       , byLeaf = byLeaf
       , diffs = stateDiffs <> pathDiffs
       }


mkStateInfo :: MachineGraph -> StateNd -> (StateInfo, [GraphDiff])
mkStateInfo graph stateNd =
  let
    (routeMap, routeDiffs) = buildRouteMap graph stateNd
    info = StateInfo { 
          ref = stateNd.ref
        , pathK = statePathKey graph stateNd.path
        , leaf = stateNameText stateNd.name
        , kind = renderStateKind stateNd
        , childInitial = stateChildInitialRef graph stateNd
        , routes = routeMap
        }
  in
  (info, routeDiffs)


buildRouteMap :: MachineGraph -> StateNd -> (M.Map LocalRouteKey RouteRaw, [GraphDiff])
buildRouteMap graph stateNd =
  let
    routeRefs = V.toList stateNd.routes
    missingRouteDiffs =
      [ IncompatibleGd (missingRouteRefMsg graph stateNd routeRef)
      | routeRef <- routeRefs
      , isNothing $ lookupRouteNd graph routeRef
      ]
    raws = mapMaybe (mkRouteRaw graph stateNd) routeRefs
    routeGroups = groupedBy (.key) raws
    duplicateSlotDiffs =
      [ IncompatibleGd (duplicateRouteSlotMsg graph stateNd routeKey groupedRaws)
      | (routeKey, groupedRaws) <- M.toAscList routeGroups
      , length groupedRaws > 1
      ]
    routeMap = M.map (.raw) $ M.mapMaybe onlyOne routeGroups
  in
  (routeMap, missingRouteDiffs <> duplicateSlotDiffs)


mkRouteRaw :: MachineGraph -> StateNd -> RouteRef -> Maybe RouteWithKey
mkRouteRaw graph stateNd routeRef = do
  routeEd <- lookupRouteNd graph routeRef
  let routeKey = mkLocalRouteKey graph stateNd routeEd
  pure RouteWithKey { key = routeKey, raw = RouteRaw { ref = routeEd.ref, route = routeEd } }


data RouteWithKey = RouteWithKey
  { key :: LocalRouteKey
  , raw :: RouteRaw
  }


mkLocalRouteKey :: MachineGraph -> StateNd -> RouteEd -> LocalRouteKey
mkLocalRouteKey graph stateNd routeEd =
  LocalRouteKey
    { caseKey = routeCaseKey graph stateNd routeEd.caseRef
    , handoff = normalizeText routeEd.handoff
    }


routeCaseKey :: MachineGraph -> StateNd -> Maybe CaseRef -> Text
routeCaseKey graph stateNd maybeCaseRef =
  case maybeCaseRef of
    Nothing -> "<entry>"
    Just caseRef ->
      case lookupCaseNd graph caseRef of
        Just caseNd ->
          case caseNd.name of
            Just caseName -> "name:" <> caseNameText caseName
            Nothing ->
              case caseOrdinal stateNd caseRef of
                Just ix -> "index:" <> tshow ix
                Nothing -> "ref:" <> renderCaseRef caseRef
        Nothing -> "ref:" <> renderCaseRef caseRef


caseOrdinal :: StateNd -> CaseRef -> Maybe Int
caseOrdinal stateNd caseRef = V.findIndex (== caseRef) stateNd.cases


pairStates :: StateIx -> StateIx -> Pairing
pairStates oldIx newIx =
  let
    exactKeys = S.toAscList (M.keysSet oldIx.byPath `S.intersection` M.keysSet newIx.byPath)
    exactPairs = fmap (\pathKey -> (oldIx.byPath M.! pathKey, newIx.byPath M.! pathKey)) exactKeys

    exactOldRefs = S.fromList (fmap (\(oldInfo, _) -> oldInfo.ref) exactPairs)
    exactNewRefs = S.fromList (fmap (\(_, newInfo) -> newInfo.ref) exactPairs)

    remainingOld = filter (\info -> info.ref `S.notMember` exactOldRefs) oldIx.infos
    remainingNew = filter (\info -> info.ref `S.notMember` exactNewRefs) newIx.infos

    movedByLeaf = pairMovedByLeaf remainingOld remainingNew
    movedPairs = fmap fst movedByLeaf

    movedOldRefs = S.fromList (fmap (\((oldInfo, _), _) -> oldInfo.ref) movedByLeaf)
    movedNewRefs = S.fromList (fmap (\((_, newInfo), _) -> newInfo.ref) movedByLeaf)

    removedInfos = filter (\info -> info.ref `S.notMember` movedOldRefs) remainingOld
    addedInfos = filter (\info -> info.ref `S.notMember` movedNewRefs) remainingNew

    moveDiffs = fmap (\((_, newInfo), True) -> StateMovedGd newInfo.ref) (filter snd movedByLeaf)
    removeDiffs = fmap (StateRemovedGd . (.ref)) removedInfos
    addDiffs = fmap (StateAddedGd . (.ref)) addedInfos
  in
  Pairing { 
      pairs = exactPairs <> movedPairs
    , diffs = moveDiffs <> removeDiffs <> addDiffs
    }


pairMovedByLeaf :: [StateInfo] -> [StateInfo] -> [((StateInfo, StateInfo), Bool)]
pairMovedByLeaf oldInfos newInfos =
  let oldByLeaf = groupedBy (.leaf) oldInfos
      newByLeaf = groupedBy (.leaf) newInfos
      sharedLeaves = S.toAscList (M.keysSet oldByLeaf `S.intersection` M.keysSet newByLeaf)
  in concatMap (pairLeaf oldByLeaf newByLeaf) sharedLeaves


pairLeaf :: M.Map Text [StateInfo] -> M.Map Text [StateInfo] -> Text -> [((StateInfo, StateInfo), Bool)]
pairLeaf oldByLeaf newByLeaf leafKey =
  case (M.lookup leafKey oldByLeaf, M.lookup leafKey newByLeaf) of
    (Just [oldInfo], Just [newInfo])
      | oldInfo.kind == newInfo.kind ->
          let moved = oldInfo.pathK /= newInfo.pathK
          in [((oldInfo, newInfo), moved)]
    _ -> []


buildCanonMaps :: [(StateInfo, StateInfo)] -> (M.Map StateRef Text, M.Map StateRef Text)
buildCanonMaps pairs =
  L.foldl' step (M.empty, M.empty) pairs
  where
    step :: (M.Map StateRef Text, M.Map StateRef Text) -> (StateInfo, StateInfo) -> (M.Map StateRef Text, M.Map StateRef Text)
    step (oldMap, newMap) (oldInfo, newInfo) =
      let canon = "state:" <> renderStatePathK oldInfo.pathK
      in (M.insert oldInfo.ref canon oldMap, M.insert newInfo.ref canon newMap)

diffPairedStates :: MachineGraph -> MachineGraph -> M.Map StateRef Text -> M.Map StateRef Text -> [(StateInfo, StateInfo)] -> [GraphDiff]
diffPairedStates oldGraph newGraph oldCanon newCanon pairs =
  concatMap (diffStatePair oldGraph newGraph oldCanon newCanon) pairs

diffStatePair :: MachineGraph -> MachineGraph -> M.Map StateRef Text -> M.Map StateRef Text -> (StateInfo, StateInfo) -> [GraphDiff]
diffStatePair oldGraph newGraph oldCanon newCanon (oldInfo, newInfo) =
  kindDiffs oldInfo newInfo
    <> childInitialDiffs oldGraph newGraph oldCanon newCanon oldInfo newInfo
    <> routeDiffs oldGraph newGraph oldCanon newCanon oldInfo newInfo

kindDiffs :: StateInfo -> StateInfo -> [GraphDiff]
kindDiffs oldInfo newInfo
  | oldInfo.kind == newInfo.kind = []
  | otherwise =
      [ IncompatibleGd
          ( "state "
              <> quote (renderStatePathK oldInfo.pathK)
              <> " changed kind from "
              <> quote oldInfo.kind
              <> " to "
              <> quote newInfo.kind
          )
      ]

childInitialDiffs :: MachineGraph -> MachineGraph -> M.Map StateRef Text -> M.Map StateRef Text -> StateInfo -> StateInfo -> [GraphDiff]
childInitialDiffs oldGraph newGraph oldCanon newCanon oldInfo newInfo =
  let oldKey = fmap (stateCanonKey oldCanon oldGraph) oldInfo.childInitial
      newKey = fmap (stateCanonKey newCanon newGraph) newInfo.childInitial
  in if oldKey == newKey
       then []
       else
         [ IncompatibleGd
             ( "child region initial state changed for state "
                 <> quote (renderStatePathK oldInfo.pathK)
                 <> " from "
                 <> renderMaybeText oldKey
                 <> " to "
                 <> renderMaybeText newKey
             )
         ]

routeDiffs :: MachineGraph -> MachineGraph -> M.Map StateRef Text -> M.Map StateRef Text -> StateInfo -> StateInfo -> [GraphDiff]
routeDiffs oldGraph newGraph oldCanon newCanon oldInfo newInfo =
  let sharedKeys = S.toAscList (M.keysSet oldInfo.routes `S.union` M.keysSet newInfo.routes)
  in concatMap diffKey sharedKeys
  where
    diffKey :: LocalRouteKey -> [GraphDiff]
    diffKey routeKey =
      case (M.lookup routeKey oldInfo.routes, M.lookup routeKey newInfo.routes) of
        (Just oldRaw, Nothing) -> [RouteChangedGd oldRaw.ref]
        (Nothing, Just newRaw) -> [RouteChangedGd newRaw.ref]
        (Just oldRaw, Just newRaw) ->
          let oldSig = routeSig oldGraph oldCanon oldRaw.route
              newSig = routeSig newGraph newCanon newRaw.route
          in if oldSig == newSig then [] else [RouteChangedGd newRaw.ref]
        (Nothing, Nothing) -> []

routeSig :: MachineGraph -> M.Map StateRef Text -> RouteEd -> RouteSig
routeSig graph canon routeEd =
  RouteSig
    { target = renderRouteTargetSig graph canon routeEd.plan.target
    , wait = renderWaitSig graph routeEd.plan.wait
    , spawn = L.sort (fmap (spawnSig graph) routeEd.plan.spawn)
    , join = renderJoinSig graph routeEd.plan.join
    , timers = L.sort (fmap (timerSig graph) routeEd.plan.timers)
    , breaks = L.sort (L.nub routeEd.plan.break)
    }

spawnSig :: MachineGraph -> SpawnPlanG -> SpawnSig
spawnSig graph spawnPlan =
  SpawnSig
    { child = normalizeText spawnPlan.child
    , input = renderValueStable spawnPlan.input
    , key = normalizeMaybeText spawnPlan.key
    , join = fmap (stableJoinKey graph) spawnPlan.join
    }

timerSig :: MachineGraph -> TimerPlanG -> TimerSig
timerSig graph timerPlan =
  TimerSig
    { timer = stableTimerKey graph timerPlan.ref
    , delay = toRational timerPlan.delay
    , payload = fmap renderValueStable timerPlan.payload
    }

renderRouteTargetSig :: MachineGraph -> M.Map StateRef Text -> RouteTarget -> Text
renderRouteTargetSig graph canon routeTarget =
  case routeTarget of
    StayRt -> "stay"
    GotoRt stateRef -> "goto:" <> stateCanonKey canon graph stateRef
    CompleteRt -> "complete"
    FailRt msg -> "fail:" <> normalizeText msg

renderWaitSig :: MachineGraph -> WaitPlanG -> Text
renderWaitSig graph waitPlan =
  case waitPlan of
    WaitNoneWg -> "none"
    WaitSignalWg -> "signal"
    WaitJoinWg joinRef -> "join:" <> stableJoinKey graph joinRef
    WaitTimerWg timerRef -> "timer:" <> stableTimerKey graph timerRef

renderJoinSig :: MachineGraph -> JoinPlanG -> Text
renderJoinSig graph joinPlan =
  case joinPlan of
    JoinNoneJg -> "none"
    JoinAllJg joinRef -> "all:" <> stableJoinKey graph joinRef
    JoinAnyJg joinRef -> "any:" <> stableJoinKey graph joinRef
    JoinCountJg joinRef n -> "count:" <> stableJoinKey graph joinRef <> ":" <> tshow n

diffRootInitial :: MachineGraph -> MachineGraph -> M.Map StateRef Text -> M.Map StateRef Text -> [GraphDiff]
diffRootInitial oldGraph newGraph oldCanon newCanon =
  case (rootInitialRef oldGraph, rootInitialRef newGraph) of
    (Just oldInitial, Just newInitial) ->
      let oldKey = stateCanonKey oldCanon oldGraph oldInitial
          newKey = stateCanonKey newCanon newGraph newInitial
      in if oldKey == newKey
           then []
           else
             [ IncompatibleGd
                 ( "root region initial state changed from "
                     <> quote oldKey
                     <> " to "
                     <> quote newKey
                 )
             ]
    (Nothing, Nothing) -> []
    _ -> [IncompatibleGd "unable to resolve the root region initial state in one of the compared graphs"]

rootInitialRef :: MachineGraph -> Maybe StateRef
rootInitialRef graph = do
  rootRegion <- lookupRegionNd graph graph.root
  pure rootRegion.initial

diffJoins :: MachineGraph -> MachineGraph -> [GraphDiff]
diffJoins oldGraph newGraph =
  let (oldNamed, oldDiffs) = namedJoinMap oldGraph
      (newNamed, newDiffs) = namedJoinMap newGraph
      joinKeys = S.toAscList (M.keysSet oldNamed `S.union` M.keysSet newNamed)
      joinDiffs = concatMap (diffJoinKey oldNamed newNamed) joinKeys
  in oldDiffs <> newDiffs <> joinDiffs

diffJoinKey :: M.Map Text JoinNd -> M.Map Text JoinNd -> Text -> [GraphDiff]
diffJoinKey oldNamed newNamed joinKey =
  case (M.lookup joinKey oldNamed, M.lookup joinKey newNamed) of
    (Just oldJoin, Nothing) -> [JoinChangedGd oldJoin.ref]
    (Nothing, Just newJoin) -> [JoinChangedGd newJoin.ref]
    (Just oldJoin, Just newJoin)
      | oldJoin.mode == newJoin.mode -> []
      | otherwise -> [JoinChangedGd newJoin.ref]
    (Nothing, Nothing) -> []

namedJoinMap :: MachineGraph -> (M.Map Text JoinNd, [GraphDiff])
namedJoinMap graph =
  let joinNodes = IM.elems graph.joins
      unnamed = filter (\joinNd -> joinNd.name == Nothing) joinNodes
      unnamedDiffs =
        if null unnamed
          then []
          else
            [ IncompatibleGd
                ( "graph "
                    <> quote (machineNameText graph.machine)
                    <> " contains unnamed joins; stable migration diff requires named joins"
                )
            ]
      grouped = groupedByMaybe (\joinNd -> joinNameText <$> joinNd.name) joinNodes
      duplicateDiffs =
        [ IncompatibleGd
            ( "graph "
                <> quote (machineNameText graph.machine)
                <> " contains duplicate join name "
                <> quote joinName
                <> " on refs "
                <> renderJoinRefs groupedJoins
            )
        | (joinName, groupedJoins) <- M.toAscList grouped
        , length groupedJoins > 1
        ]
      named = M.mapMaybe onlyOne grouped
  in (named, unnamedDiffs <> duplicateDiffs)

diffTimers :: MachineGraph -> MachineGraph -> [GraphDiff]
diffTimers oldGraph newGraph =
  let (oldNamed, oldDiffs) = namedTimerMap oldGraph
      (newNamed, newDiffs) = namedTimerMap newGraph
      timerKeys = S.toAscList (M.keysSet oldNamed `S.union` M.keysSet newNamed)
      timerDiffs = concatMap (diffTimerKey oldNamed newNamed) timerKeys
  in oldDiffs <> newDiffs <> timerDiffs

diffTimerKey :: M.Map Text TimerNd -> M.Map Text TimerNd -> Text -> [GraphDiff]
diffTimerKey oldNamed newNamed timerKey =
  case (M.lookup timerKey oldNamed, M.lookup timerKey newNamed) of
    (Just oldTimer, Nothing) -> [TimerChangedGd oldTimer.ref]
    (Nothing, Just newTimer) -> [TimerChangedGd newTimer.ref]
    (Just _, Just _) -> []
    (Nothing, Nothing) -> []

namedTimerMap :: MachineGraph -> (M.Map Text TimerNd, [GraphDiff])
namedTimerMap graph =
  let timerNodes = IM.elems graph.timers
      unnamed = filter (\timerNd -> timerNd.name == Nothing) timerNodes
      unnamedDiffs =
        if null unnamed
          then []
          else
            [ IncompatibleGd
                ( "graph "
                    <> quote (machineNameText graph.machine)
                    <> " contains unnamed timers; stable migration diff requires named timers"
                )
            ]
      grouped = groupedByMaybe (\timerNd -> timerNameText <$> timerNd.name) timerNodes
      duplicateDiffs =
        [ IncompatibleGd
            ( "graph "
                <> quote (machineNameText graph.machine)
                <> " contains duplicate timer name "
                <> quote timerName
                <> " on refs "
                <> renderTimerRefs groupedTimers
            )
        | (timerName, groupedTimers) <- M.toAscList grouped
        , length groupedTimers > 1
        ]
      named = M.mapMaybe onlyOne grouped
  in (named, unnamedDiffs <> duplicateDiffs)

stateChildInitialRef :: MachineGraph -> StateNd -> Maybe StateRef
stateChildInitialRef graph stateNd = do
  childRegionRef <- stateNd.child
  childRegion <- lookupRegionNd graph childRegionRef
  pure childRegion.initial

stateCanonKey :: M.Map StateRef Text -> MachineGraph -> StateRef -> Text
stateCanonKey canon graph stateRef =
  M.findWithDefault fallback stateRef canon
  where
    fallback = "state:" <> renderStatePathK (statePathKeyFromRef graph stateRef)

statePathKeyFromRef :: MachineGraph -> StateRef -> StatePathK
statePathKeyFromRef graph stateRef =
  case lookupStateNd graph stateRef of
    Just stateNd -> statePathKey graph stateNd.path
    Nothing -> StatePathK ["#state:" <> renderStateRef stateRef]

statePathKey :: MachineGraph -> StatePath -> StatePathK
statePathKey graph statePath =
  StatePathK (fmap (statePathPart graph) (statePathToList statePath))

statePathPart :: MachineGraph -> StateRef -> Text
statePathPart graph stateRef =
  case lookupStateNd graph stateRef of
    Just stateNd -> stateNameText stateNd.name
    Nothing -> "#state:" <> renderStateRef stateRef

stableJoinKey :: MachineGraph -> JoinRef -> Text
stableJoinKey graph joinRef =
  case lookupJoinNd graph joinRef of
    Just joinNd ->
      case joinNd.name of
        Just joinName -> joinNameText joinName
        Nothing -> "#join:" <> renderJoinRef joinRef
    Nothing -> "#join:" <> renderJoinRef joinRef

stableTimerKey :: MachineGraph -> TimerRef -> Text
stableTimerKey graph timerRef =
  case lookupTimerNd graph timerRef of
    Just timerNd ->
      case timerNd.name of
        Just timerName -> timerNameText timerName
        Nothing -> "#timer:" <> renderTimerRef timerRef
    Nothing -> "#timer:" <> renderTimerRef timerRef

machineMismatchMsg :: MachineGraph -> MachineGraph -> Text
machineMismatchMsg oldGraph newGraph =
  "cannot diff different machine families: "
    <> quote (machineNameText oldGraph.machine)
    <> " vs "
    <> quote (machineNameText newGraph.machine)

ambiguousStatePathMsg :: MachineGraph -> StatePathK -> [StateInfo] -> Text
ambiguousStatePathMsg graph pathKey groupedInfos =
  "graph "
    <> quote (machineNameText graph.machine)
    <> " contains an ambiguous state path key "
    <> quote (renderStatePathK pathKey)
    <> " on refs "
    <> renderStateRefs groupedInfos

duplicateRouteSlotMsg :: MachineGraph -> StateNd -> LocalRouteKey -> [RouteWithKey] -> Text
duplicateRouteSlotMsg graph stateNd routeKey groupedRaws =
  "graph "
    <> quote (machineNameText graph.machine)
    <> " contains an ambiguous route slot "
    <> quote (renderLocalRouteKey routeKey)
    <> " in state "
    <> quote (renderStatePathK (statePathKey graph stateNd.path))
    <> " on refs "
    <> renderRouteRefs (fmap (\routeWithKey -> routeWithKey.raw.ref) groupedRaws)

missingRouteRefMsg :: MachineGraph -> StateNd -> RouteRef -> Text
missingRouteRefMsg graph stateNd routeRef =
  "state "
    <> quote (renderStatePathK (statePathKey graph stateNd.path))
    <> " in graph "
    <> quote (machineNameText graph.machine)
    <> " references missing route "
    <> quote (renderRouteRef routeRef)

renderLocalRouteKey :: LocalRouteKey -> Text
renderLocalRouteKey routeKey =
  routeKey.caseKey <> "/" <> routeKey.handoff

renderStatePathK :: StatePathK -> Text
renderStatePathK (StatePathK parts) = T.intercalate "/" parts

renderStateKind :: StateNd -> Text
renderStateKind stateNd = T.pack (show stateNd.kind)

renderMaybeText :: Maybe Text -> Text
renderMaybeText maybeTxt =
  case maybeTxt of
    Just txt -> quote txt
    Nothing -> "<none>"

renderStateRefs :: [StateInfo] -> Text
renderStateRefs infos =
  renderRefList renderStateRef (fmap (.ref) infos)

renderRouteRefs :: [RouteRef] -> Text
renderRouteRefs = renderRefList renderRouteRef

renderJoinRefs :: [JoinNd] -> Text
renderJoinRefs joinNds = renderRefList renderJoinRef (fmap (.ref) joinNds)

renderTimerRefs :: [TimerNd] -> Text
renderTimerRefs timerNds = renderRefList renderTimerRef (fmap (.ref) timerNds)

renderRefList :: (a -> Text) -> [a] -> Text
renderRefList renderRef =
  T.intercalate ", " . fmap renderRef

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

lookupRegionNd :: MachineGraph -> RegionRef -> Maybe RegionNd
lookupRegionNd graph regionRef = IM.lookup (word32Key (regionRefWord32 regionRef)) graph.regions

lookupStateNd :: MachineGraph -> StateRef -> Maybe StateNd
lookupStateNd graph stateRef = IM.lookup (word32Key (stateRefWord32 stateRef)) graph.states

lookupCaseNd :: MachineGraph -> CaseRef -> Maybe CaseNd
lookupCaseNd graph caseRef = IM.lookup (word32Key (caseRefWord32 caseRef)) graph.cases

lookupRouteNd :: MachineGraph -> RouteRef -> Maybe RouteEd
lookupRouteNd graph routeRef = IM.lookup (word32Key (routeRefWord32 routeRef)) graph.routes

lookupJoinNd :: MachineGraph -> JoinRef -> Maybe JoinNd
lookupJoinNd graph joinRef = IM.lookup (word32Key (joinRefWord32 joinRef)) graph.joins

lookupTimerNd :: MachineGraph -> TimerRef -> Maybe TimerNd
lookupTimerNd graph timerRef = IM.lookup (word32Key (timerRefWord32 timerRef)) graph.timers

stateNodes :: MachineGraph -> [StateNd]
stateNodes graph = IM.elems graph.states

groupedBy :: Ord k => (a -> k) -> [a] -> M.Map k [a]
groupedBy keyOf =
  L.foldl' step M.empty
  where
    step acc x = M.insertWith (<>) (keyOf x) [x] acc

groupedByMaybe :: Ord k => (a -> Maybe k) -> [a] -> M.Map k [a]
groupedByMaybe keyOf =
  L.foldl' step M.empty
  where
    step acc x =
      case keyOf x of
        Just key -> M.insertWith (<>) key [x] acc
        Nothing -> acc

onlyOne :: [a] -> Maybe a
onlyOne xs =
  case xs of
    [x] -> Just x
    _ -> Nothing

stableDiffs :: [GraphDiff] -> [GraphDiff]
stableDiffs =
  S.toAscList . S.fromList

normalizeText :: Text -> Text
normalizeText = T.strip

normalizeMaybeText :: Maybe Text -> Maybe Text
normalizeMaybeText maybeTxt =
  case fmap T.strip maybeTxt of
    Just txt | T.null txt -> Nothing
    other -> other

quote :: Text -> Text
quote txt = "'" <> txt <> "'"

word32Key :: Integral a => a -> Int
word32Key = fromIntegral

tshow :: Show a => a -> Text
tshow = T.pack . show

renderValueStable :: Value -> Text
renderValueStable value =
  case value of
    Null -> "null"
    Bool False -> "false"
    Bool True -> "true"
    Number sc -> T.pack (Sci.formatScientific Sci.Generic Nothing sc)
    String txt -> T.pack (show txt)
    Array xs ->
      "[" <> T.intercalate "," (fmap renderValueStable (V.toList xs)) <> "]"
    Object obj ->
      let pairs = sortObjectPairs obj
          renderPair (keyTxt, subValue) = T.pack (show keyTxt) <> ":" <> renderValueStable subValue
      in "{" <> T.intercalate "," (fmap renderPair pairs) <> "}"

sortObjectPairs :: KM.KeyMap Value -> [(Text, Value)]
sortObjectPairs obj =
  L.sortOn fst [(K.toText key, value) | (key, value) <- KM.toList obj]