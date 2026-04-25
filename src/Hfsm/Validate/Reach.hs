module Hfsm.Validate.Reach
  ( checkReach
  ) where

import Data.Graph (SCC(..))
import qualified Data.Graph as G
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl', sortOn)
import Data.Maybe (mapMaybe)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

import Hfsm.Core.Name (stateNameText)
import Hfsm.Core.Ref
  ( RegionRef (..)
  , RouteRef (..)
  , StateRef (..)
  , regionRefWord32
  , routeRefWord32
  , stateRefWord32
  )
import Hfsm.Graph.Def
  ( BreakPlanG(..)
  , JoinPlanG(..)
  , MachineGraph (..)
  , RegionNd (..)
  , RouteEd (..)
  , RouteTarget(..)
  , StateNd (..)
  , WaitPlanG(..)
  , RoutePlan(..)
  )
import Hfsm.Spec.Def (StateKind(..))
import Hfsm.Validate.Error
  ( ValidateErr
  , atRegion
  , atState
  , errorErr
  , warnErr
  )

checkReach :: MachineGraph -> [ValidateErr]
checkReach graph =
  case rootInitialState graph of
    Nothing -> []
    Just startRef ->
      let reachable = reachableStates graph startRef
          exitable = statesThatCanReachExit graph
          pureSuccs = pureStructuralSuccessorsByState graph reachable
      in concat
           [ checkUnreachableStates graph reachable
           , checkDeadCompositeRegions graph reachable
           , checkOrphanFinalStates graph
           , checkNoExitSurface graph reachable exitable
           , checkPureStructuralLivelock graph pureSuccs
           ]

checkUnreachableStates :: MachineGraph -> Set StateRef -> [ValidateErr]
checkUnreachableStates graph reachable =
  [ atState stNd.ref $
      warnErr "reach.unreachable-state"
        ("state " <> stateSummary graph stNd.ref <> " is unreachable from the machine root")
        Nothing
  | stNd <- IM.elems graph.states
  , S.notMember stNd.ref reachable
  ]

checkDeadCompositeRegions :: MachineGraph -> Set StateRef -> [ValidateErr]
checkDeadCompositeRegions graph reachable =
  mapMaybe checkRegion (IM.elems graph.regions)
  where
    checkRegion :: RegionNd -> Maybe ValidateErr
    checkRegion rgNd =
      case rgNd.parent of
        Nothing -> Nothing
        Just parentRef ->
          let parentReachable = S.member parentRef reachable
              memberRefs = V.toList rgNd.states
              anyReachable = any (`S.member` reachable) memberRefs
          in if parentReachable && not anyReachable
               then Just $
                 atRegion rgNd.ref $
                   errorErr "reach.dead-composite-region"
                     ("region " <> renderRegionRef rgNd.ref <> " has a reachable parent state "
                       <> renderStateRef parentRef <> " but no reachable member state")
                     Nothing
               else Nothing

checkOrphanFinalStates :: MachineGraph -> [ValidateErr]
checkOrphanFinalStates graph =
  let initialRefs = initialStateRefs graph
      incomingGotoRefs = incomingGotoTargetRefs graph
  in [ atState stNd.ref $
         warnErr "reach.orphan-final-state"
           ("terminal state " <> stateSummary graph stNd.ref <> " has no incoming structural edge")
           Nothing
     | stNd <- IM.elems graph.states
     , stNd.kind == TerminalSk
     , S.notMember stNd.ref initialRefs
     , S.notMember stNd.ref incomingGotoRefs
     ]

checkNoExitSurface :: MachineGraph -> Set StateRef -> Set StateRef -> [ValidateErr]
checkNoExitSurface graph reachable exitable =
  [ atState stNd.ref $
      warnErr "reach.no-exit-surface"
        ("no completion or failure surface is reachable from state " <> stateSummary graph stNd.ref)
        Nothing
  | stNd <- IM.elems graph.states
  , S.member stNd.ref reachable
  , S.notMember stNd.ref exitable
  ]

checkPureStructuralLivelock :: MachineGraph -> IntMap (Set StateRef) -> [ValidateErr]
checkPureStructuralLivelock graph pureSuccs =
  map mkErr problematicSccs
  where
    pureStateRefs = S.fromList (map intKeyToStateRef (IM.keys pureSuccs))

    nodes :: [(StateRef, Int, [Int])]
    nodes =
      [ (srcRef, stateRefKey srcRef, succKeysInPureSet srcRef succRefs)
      | (srcKey, succRefs) <- IM.toList pureSuccs
      , let srcRef = intKeyToStateRef srcKey
      ]

    succKeysInPureSet :: StateRef -> Set StateRef -> [Int]
    succKeysInPureSet _ succRefs =
      [ stateRefKey dstRef
      | dstRef <- S.toList succRefs
      , S.member dstRef pureStateRefs
      ]

    sccMembers :: SCC StateRef -> [StateRef]
    sccMembers scc =
      case scc of
        AcyclicSCC stRef -> [stRef]
        CyclicSCC stRefs -> stRefs

    hasCycle :: SCC StateRef -> Bool
    hasCycle scc =
      case scc of
        CyclicSCC stRefs -> not (null stRefs)
        AcyclicSCC stRef -> S.member stRef (lookupPureSuccs stRef)

    closedScc :: Set StateRef -> Bool
    closedScc memberRefs =
      all (\stRef -> lookupPureSuccs stRef `S.isSubsetOf` memberRefs) (S.toList memberRefs)

    lookupPureSuccs :: StateRef -> Set StateRef
    lookupPureSuccs stRef =
      IM.findWithDefault S.empty (stateRefKey stRef) pureSuccs

    problematicSccs :: [[StateRef]]
    problematicSccs =
      [ sortOn stateRefWord32 refs
      | scc <- G.stronglyConnComp nodes
      , hasCycle scc
      , let refs = sccMembers scc
      , let refSet = S.fromList refs
      , closedScc refSet
      ]

    mkErr :: [StateRef] -> ValidateErr
    mkErr refs =
      let pivotRef = head refs
          msg =
            "pure structural livelock cycle detected among states "
              <> T.intercalate ", " (map (stateSummary graph) refs)
      in atState pivotRef $
           errorErr "reach.pure-structural-livelock" msg Nothing

rootInitialState :: MachineGraph -> Maybe StateRef
rootInitialState graph = do
  rgNd <- lookupRegionNd graph graph.root
  if stateExists graph rgNd.initial then Just rgNd.initial else Nothing

reachableStates :: MachineGraph -> StateRef -> Set StateRef
reachableStates graph startRef
  | not (stateExists graph startRef) = S.empty
  | otherwise = go S.empty [startRef]
  where
    adj = forwardSuccessorsByState graph

    go :: Set StateRef -> [StateRef] -> Set StateRef
    go seen [] = seen
    go seen (stRef : rest)
      | S.member stRef seen = go seen rest
      | otherwise =
          let succs = S.toList (IM.findWithDefault S.empty (stateRefKey stRef) adj)
          in go (S.insert stRef seen) (succs <> rest)

statesThatCanReachExit :: MachineGraph -> Set StateRef
statesThatCanReachExit graph =
  backwardClosure reverseAdj exitSeeds
  where
    forwardAdj = forwardSuccessorsByState graph
    reverseAdj = reverseSuccessorsByState forwardAdj
    exitSeeds = S.fromList [stNd.ref | stNd <- IM.elems graph.states, isExitSurfaceState graph stNd]

backwardClosure :: IntMap (Set StateRef) -> Set StateRef -> Set StateRef
backwardClosure reverseAdj seeds = go S.empty (S.toList seeds)
  where
    go :: Set StateRef -> [StateRef] -> Set StateRef
    go seen [] = seen
    go seen (stRef : rest)
      | S.member stRef seen = go seen rest
      | otherwise =
          let preds = S.toList (IM.findWithDefault S.empty (stateRefKey stRef) reverseAdj)
          in go (S.insert stRef seen) (preds <> rest)

forwardSuccessorsByState :: MachineGraph -> IntMap (Set StateRef)
forwardSuccessorsByState graph =
  IM.fromList
    [ (stateRefKey stNd.ref, forwardSuccessors graph stNd)
    | stNd <- IM.elems graph.states
    ]

forwardSuccessors :: MachineGraph -> StateNd -> Set StateRef
forwardSuccessors graph stNd =
  childInitialTarget graph stNd `S.union` gotoTargets graph stNd

gotoTargets :: MachineGraph -> StateNd -> Set StateRef
gotoTargets graph stNd =
  S.fromList $
    mapMaybe routeGotoTarget (routesForState graph stNd)

routeGotoTarget :: RouteEd -> Maybe StateRef
routeGotoTarget rtEd =
  case rtEd.plan.target of
    GotoTg stRef -> Just stRef
    StayTg -> Nothing
    CompleteTg -> Nothing
    FailTg _ -> Nothing

reverseSuccessorsByState :: IntMap (Set StateRef) -> IntMap (Set StateRef)
reverseSuccessorsByState forwardAdj =
  foldl' addOutgoing IM.empty (IM.toList forwardAdj)
  where
    addOutgoing :: IntMap (Set StateRef) -> (Int, Set StateRef) -> IntMap (Set StateRef)
    addOutgoing acc (srcKey, dstRefs) =
      let srcRef = intKeyToStateRef srcKey
      in S.foldl' (\acc' dstRef -> IM.insertWith S.union (stateRefKey dstRef) (S.singleton srcRef) acc') acc dstRefs

pureStructuralSuccessorsByState :: MachineGraph -> Set StateRef -> IntMap (Set StateRef)
pureStructuralSuccessorsByState graph reachable =
  IM.fromList
    [ (stateRefKey stNd.ref, pureStructuralSuccessors graph stNd)
    | stNd <- IM.elems graph.states
    , isPureStructuralState graph reachable stNd
    ]

pureStructuralSuccessors :: MachineGraph -> StateNd -> Set StateRef
pureStructuralSuccessors graph stNd =
  childInitialTarget graph stNd `S.union` routeTargets
  where
    routeTargets =
      S.fromList $
        mapMaybe (pureStructuralRouteTarget stNd.ref) (routesForState graph stNd)

pureStructuralRouteTarget :: StateRef -> RouteEd -> Maybe StateRef
pureStructuralRouteTarget srcRef rtEd
  | not (isPureStructuralRoute rtEd) = Nothing
  | otherwise =
      case rtEd.plan.target of
        StayTg -> Just srcRef
        GotoTg dstRef -> Just dstRef
        CompleteTg -> Nothing
        FailTg _ -> Nothing

isPureStructuralState :: MachineGraph -> Set StateRef -> StateNd -> Bool
isPureStructuralState graph reachable stNd =
  S.member stNd.ref reachable
    && not (isExitSurfaceState graph stNd)
    && all isPureStructuralRoute (routesForState graph stNd)

isPureStructuralRoute :: RouteEd -> Bool
isPureStructuralRoute rtEd =
  rtEd.plan.wait == WaitNoneWg
    && null rtEd.plan.spawn
    && rtEd.plan.join == JoinNoneJg
    && null rtEd.plan.timers
    && case rtEd.plan.target of
         StayTg -> True
         GotoTg _ -> True
         CompleteTg -> False
         FailTg _ -> False

isExitSurfaceState :: MachineGraph -> StateNd -> Bool
isExitSurfaceState graph stNd =
  stNd.kind == TerminalSk || any routeIsDirectExit (routesForState graph stNd)

routeIsDirectExit :: RouteEd -> Bool
routeIsDirectExit rtEd =
  case rtEd.plan.target of
    CompleteTg -> True
    FailTg _ -> True
    StayTg -> False
    GotoTg _ -> False

childInitialTarget :: MachineGraph -> StateNd -> Set StateRef
childInitialTarget graph stNd =
  case stNd.child >>= lookupRegionNd graph of
    Nothing -> S.empty
    Just rgNd ->
      if stateExists graph rgNd.initial
        then S.singleton rgNd.initial
        else S.empty

initialStateRefs :: MachineGraph -> Set StateRef
initialStateRefs graph =
  S.fromList
    [ rgNd.initial
    | rgNd <- IM.elems graph.regions
    , stateExists graph rgNd.initial
    ]

incomingGotoTargetRefs :: MachineGraph -> Set StateRef
incomingGotoTargetRefs graph =
  S.fromList $
    mapMaybe routeGotoTarget (IM.elems graph.routes)

routesForState :: MachineGraph -> StateNd -> [RouteEd]
routesForState graph stNd =
  mapMaybe (lookupRouteEd graph) (V.toList stNd.routes)

lookupRegionNd :: MachineGraph -> RegionRef -> Maybe RegionNd
lookupRegionNd graph rgRef =
  IM.lookup (regionRefKey rgRef) graph.regions

lookupRouteEd :: MachineGraph -> RouteRef -> Maybe RouteEd
lookupRouteEd graph rtRef =
  IM.lookup (routeRefKey rtRef) graph.routes

lookupStateNd :: MachineGraph -> StateRef -> Maybe StateNd
lookupStateNd graph stRef =
  IM.lookup (stateRefKey stRef) graph.states

stateExists :: MachineGraph -> StateRef -> Bool
stateExists graph stRef =
  IM.member (stateRefKey stRef) graph.states

stateSummary :: MachineGraph -> StateRef -> Text
stateSummary graph stRef =
  case lookupStateNd graph stRef of
    Nothing -> renderStateRef stRef
    Just stNd -> quote (stateNameText stNd.name) <> " (" <> renderStateRef stRef <> ")"

renderRegionRef :: RegionRef -> Text
renderRegionRef = tshow . regionRefWord32

renderStateRef :: StateRef -> Text
renderStateRef = tshow . stateRefWord32

regionRefKey :: RegionRef -> Int
regionRefKey = fromIntegral . regionRefWord32

routeRefKey :: RouteRef -> Int
routeRefKey = fromIntegral . routeRefWord32

stateRefKey :: StateRef -> Int
stateRefKey = fromIntegral . stateRefWord32

intKeyToStateRef :: Int -> StateRef
intKeyToStateRef = toEnum
  {-
  where
    (>>>) :: (a -> b) -> (b -> c) -> a -> c
    f >>> g = g . f
  -}

quote :: Text -> Text
quote txt = "\"" <> txt <> "\""

tshow :: Show a => a -> Text
tshow = T.pack . show