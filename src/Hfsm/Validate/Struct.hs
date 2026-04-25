{-# LANGUAGE DerivingStrategies #-}
module Hfsm.Validate.Struct
  ( checkStruct
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (foldl')
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (isNothing)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V

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
import Hfsm.Graph.Def
  ( CaseNd (..)
  , JoinNd (..)
  , MachineGraph (..)
  , RegionNd (..)
  , RouteEd (..)
  , StateNd (..)
  , TimerNd (..)
  )
import Hfsm.Spec.Def (StateKind(..))
import Hfsm.Validate.Error
  ( ValidateErr
  , atCase
  , atJoin
  , atMachine
  , atRegion
  , atRoute
  , atState
  , errorErr
  )

checkStruct :: MachineGraph -> [ValidateErr]
checkStruct graph =
  let lu = mkLookups graph
  in concat
       [ checkRootRegion graph lu
       , checkRegionTable graph
       , checkStateTable graph
       , checkCaseTable graph
       , checkRouteTable graph
       , checkJoinTable graph
       , checkTimerTable graph
       , checkRegions lu
       , checkStates lu
       , checkCases lu
       , checkRoutes lu
       , checkContainmentCycles lu
       ]

data Lookups = Lookups
  { regionByRef :: Map RegionRef RegionNd
  , stateByRef :: Map StateRef StateNd
  , caseByRef :: Map CaseRef CaseNd
  , routeByRef :: Map RouteRef RouteEd
  , joinByRef :: Map JoinRef JoinNd
  , timerByRef :: Map TimerRef TimerNd
  }

data ContainNode
  = RegionCn RegionRef
  | StateCn StateRef
  deriving stock (Eq, Ord, Show, Read)

mkLookups :: MachineGraph -> Lookups
mkLookups graph =
  Lookups
    { regionByRef = refMap (\x -> x.ref) graph.regions
    , stateByRef = refMap (\x -> x.ref) graph.states
    , caseByRef = refMap (\x -> x.ref) graph.cases
    , routeByRef = refMap (\x -> x.ref) graph.routes
    , joinByRef = refMap (\x -> x.ref) graph.joins
    , timerByRef = refMap (\x -> x.ref) graph.timers
    }

refMap :: Ord ref => (a -> ref) -> IntMap a -> Map ref a
refMap getRef =
  M.fromListWith keepFirst . fmap (\x -> (getRef x, x)) . IM.elems
  where
    keepFirst :: a -> a -> a
    keepFirst _new old = old

checkRootRegion :: MachineGraph -> Lookups -> [ValidateErr]
checkRootRegion graph lu =
  let rootRef = graph.root
      rootRegionMb = lookupRegion lu rootRef
      parentlessRegionRefs = fmap (\x -> x.ref) (filter (isNothing . (.parent)) (M.elems lu.regionByRef))
      missingRootErrs =
        case rootRegionMb of
          Nothing ->
            [machineErr "struct.root.missing" ("machine graph root region " <> renderRegionRef rootRef <> " is not present in the region table")]
          Just _ ->
            []
      rootParentErrs =
        case rootRegionMb of
          Just rootRegion | rootRegion.parent /= Nothing ->
            [regionErr rootRef "struct.root.parent" "root region must have no parent state"]
          _ ->
            []
      parentlessErrs =
        case parentlessRegionRefs of
          [] ->
            [machineErr "struct.root.none" "machine graph has no region with parent = Nothing"]
          [_] ->
            []
          refs ->
            machineErr "struct.root.multiple"
              ("machine graph has multiple parentless regions: " <> renderRegionRefs refs)
              : [ regionErr ref "struct.root.extra" "region is parentless; exactly one root region is allowed"
                | ref <- refs
                ]
      rootMismatchErrs =
        case parentlessRegionRefs of
          [onlyRef] | onlyRef /= rootRef ->
            [machineErr "struct.root.mismatch" ("machine graph root is " <> renderRegionRef rootRef <> " but the unique parentless region is " <> renderRegionRef onlyRef)]
          _ ->
            []
  in missingRootErrs <> rootParentErrs <> parentlessErrs <> rootMismatchErrs

checkRegionTable :: MachineGraph -> [ValidateErr]
checkRegionTable graph =
  checkTableRefs
    "region"
    (\x -> x.ref)
    (fromIntegral . regionRefWord32)
    renderRegionRef
    (Just atRegion)
    graph.regions

checkStateTable :: MachineGraph -> [ValidateErr]
checkStateTable graph =
  checkTableRefs
    "state"
    (\x -> x.ref)
    (fromIntegral . stateRefWord32)
    renderStateRef
    (Just atState)
    graph.states

checkCaseTable :: MachineGraph -> [ValidateErr]
checkCaseTable graph =
  checkTableRefs
    "case"
    (\x -> x.ref)
    (fromIntegral . caseRefWord32)
    renderCaseRef
    (Just atCase)
    graph.cases

checkRouteTable :: MachineGraph -> [ValidateErr]
checkRouteTable graph =
  checkTableRefs
    "route"
    (\x -> x.ref)
    (fromIntegral . routeRefWord32)
    renderRouteRef
    (Just atRoute)
    graph.routes

checkJoinTable :: MachineGraph -> [ValidateErr]
checkJoinTable graph =
  checkTableRefs
    "join"
    (\x -> x.ref)
    (fromIntegral . joinRefWord32)
    renderJoinRef
    (Just atJoin)
    graph.joins

checkTimerTable :: MachineGraph -> [ValidateErr]
checkTimerTable graph =
  checkTableRefs
    "timer"
    (\x -> x.ref)
    (fromIntegral . timerRefWord32)
    renderTimerRef
    Nothing
    graph.timers

checkTableRefs ::
  Ord ref =>
  Text ->
  (a -> ref) ->
  (ref -> Int) ->
  (ref -> Text) ->
  Maybe (ref -> ValidateErr -> ValidateErr) ->
  IntMap a ->
  [ValidateErr]
checkTableRefs label getRef refKey renderRef mLocate table =
  let keyMismatchErrs =
        [ locateRef mLocate ref $
            errorErr
              ("struct." <> label <> ".key-mismatch")
              (label <> " table key " <> tshow key <> " does not match embedded ref " <> renderRef ref)
              Nothing
        | (key, node) <- IM.toList table
        , let ref = getRef node
        , key /= refKey ref
        ]
      duplicateRefErrs =
        [ locateRef mLocate ref $
            errorErr
              ("struct." <> label <> ".duplicate-ref")
              (label <> " ref " <> renderRef ref <> " appears more than once in its table")
              Nothing
        | ref <- duplicateRefs (fmap getRef (IM.elems table))
        ]
  in keyMismatchErrs <> duplicateRefErrs

checkRegions :: Lookups -> [ValidateErr]
checkRegions lu =
  concatMap (checkRegion lu) (M.elems lu.regionByRef)

checkRegion :: Lookups -> RegionNd -> [ValidateErr]
checkRegion lu regionNd =
  concat
    [ checkRegionParent lu regionNd
    , checkRegionInitial lu regionNd
    , checkRegionStates lu regionNd
    ]

checkRegionParent :: Lookups -> RegionNd -> [ValidateErr]
checkRegionParent lu regionNd =
  case regionNd.parent of
    Nothing ->
      []
    Just parentStateRef ->
      case lookupState lu parentStateRef of
        Nothing ->
          [regionErr regionNd.ref "struct.region.parent.missing-state" ("region parent state " <> renderStateRef parentStateRef <> " does not exist")]
        Just parentStateNd ->
          if parentStateNd.child == Just regionNd.ref
            then []
            else [regionErr regionNd.ref "struct.region.parent.child-mismatch" ("region parent state " <> renderStateRef parentStateRef <> " does not point back to this region via its child field")]

checkRegionInitial :: Lookups -> RegionNd -> [ValidateErr]
checkRegionInitial lu regionNd =
  let emptyRegionErrs =
        if V.null regionNd.states
          then [regionErr regionNd.ref "struct.region.empty" "region declares no member states"]
          else []
      initialMembershipErrs =
        if regionNd.initial `V.elem` regionNd.states
          then []
          else [regionErr regionNd.ref "struct.region.initial.outside" ("region initial state " <> renderStateRef regionNd.initial <> " is not listed in the region state vector")]
      initialExistenceErrs =
        case lookupState lu regionNd.initial of
          Nothing ->
            [regionErr regionNd.ref "struct.region.initial.missing" ("region initial state " <> renderStateRef regionNd.initial <> " does not exist")]
          Just initialStateNd ->
            if initialStateNd.parent == regionNd.ref
              then []
              else [regionErr regionNd.ref "struct.region.initial.parent-mismatch" ("region initial state " <> renderStateRef regionNd.initial <> " belongs to parent region " <> renderRegionRef initialStateNd.parent <> " instead of " <> renderRegionRef regionNd.ref)]
  in emptyRegionErrs <> initialMembershipErrs <> initialExistenceErrs

checkRegionStates :: Lookups -> RegionNd -> [ValidateErr]
checkRegionStates lu regionNd =
  let duplicateStateErrs =
        [ regionErr regionNd.ref "struct.region.state.duplicate" ("region contains duplicated state ref " <> renderStateRef stateRef)
        | stateRef <- duplicateRefs (V.toList regionNd.states)
        ]
      memberStateErrs =
        concatMap checkMemberState (V.toList regionNd.states)
      checkMemberState stateRef =
        case lookupState lu stateRef of
          Nothing ->
            [regionErr regionNd.ref "struct.region.state.missing" ("region references missing state " <> renderStateRef stateRef)]
          Just stateNd ->
            if stateNd.parent == regionNd.ref
              then []
              else [stateErr stateRef "struct.region.state.parent-mismatch" ("state is listed in region " <> renderRegionRef regionNd.ref <> " but its parent region is " <> renderRegionRef stateNd.parent)]
  in duplicateStateErrs <> memberStateErrs

checkStates :: Lookups -> [ValidateErr]
checkStates lu =
  concatMap (checkState lu) (M.elems lu.stateByRef)

checkState :: Lookups -> StateNd -> [ValidateErr]
checkState lu stateNd =
  concat
    [ checkStateParent lu stateNd
    , checkStateEntry lu stateNd
    , checkStateCases lu stateNd
    , checkStateRoutes lu stateNd
    , checkStateShape lu stateNd
    ]

checkStateParent :: Lookups -> StateNd -> [ValidateErr]
checkStateParent lu stateNd =
  case lookupRegion lu stateNd.parent of
    Nothing ->
      [stateErr stateNd.ref "struct.state.parent.missing-region" ("state parent region " <> renderRegionRef stateNd.parent <> " does not exist")]
    Just parentRegionNd ->
      if stateNd.ref `V.elem` parentRegionNd.states
        then []
        else [stateErr stateNd.ref "struct.state.parent.membership-missing" ("state belongs to region " <> renderRegionRef stateNd.parent <> " but that region does not list the state in its state vector")]

checkStateEntry :: Lookups -> StateNd -> [ValidateErr]
checkStateEntry lu stateNd =
  case stateNd.entry of
    Nothing ->
      []
    Just caseRef ->
      let existenceErrs =
            case lookupCase lu caseRef of
              Nothing ->
                [stateErr stateNd.ref "struct.state.entry.missing" ("state entry case " <> renderCaseRef caseRef <> " does not exist")]
              Just caseNd ->
                if caseNd.state == stateNd.ref
                  then []
                  else [stateErr stateNd.ref "struct.state.entry.owner-mismatch" ("state entry case " <> renderCaseRef caseRef <> " belongs to state " <> renderStateRef caseNd.state <> " instead of " <> renderStateRef stateNd.ref)]
          duplicateUseErrs =
            if caseRef `V.elem` stateNd.cases
              then [stateErr stateNd.ref "struct.state.entry.duplicated-in-cases" ("state entry case " <> renderCaseRef caseRef <> " also appears in the ordinary case vector")]
              else []
      in existenceErrs <> duplicateUseErrs

checkStateCases :: Lookups -> StateNd -> [ValidateErr]
checkStateCases lu stateNd =
  let duplicateCaseErrs =
        [ stateErr stateNd.ref "struct.state.case.duplicate" ("state contains duplicated case ref " <> renderCaseRef caseRef)
        | caseRef <- duplicateRefs (V.toList stateNd.cases)
        ]
      ownershipErrs =
        concatMap checkOwnedCase (V.toList stateNd.cases)
      checkOwnedCase caseRef =
        case lookupCase lu caseRef of
          Nothing ->
            [stateErr stateNd.ref "struct.state.case.missing" ("state references missing case " <> renderCaseRef caseRef)]
          Just caseNd ->
            if caseNd.state == stateNd.ref
              then []
              else [stateErr stateNd.ref "struct.state.case.owner-mismatch" ("state references case " <> renderCaseRef caseRef <> " but the case belongs to state " <> renderStateRef caseNd.state)]
  in duplicateCaseErrs <> ownershipErrs

checkStateRoutes :: Lookups -> StateNd -> [ValidateErr]
checkStateRoutes lu stateNd =
  let duplicateRouteErrs =
        [ stateErr stateNd.ref "struct.state.route.duplicate" ("state contains duplicated route ref " <> renderRouteRef routeRef)
        | routeRef <- duplicateRefs (V.toList stateNd.routes)
        ]
      ownershipErrs =
        concatMap checkOwnedRoute (V.toList stateNd.routes)
      checkOwnedRoute routeRef =
        case lookupRoute lu routeRef of
          Nothing ->
            [stateErr stateNd.ref "struct.state.route.missing" ("state references missing route " <> renderRouteRef routeRef)]
          Just routeEd ->
            if routeEd.state == stateNd.ref
              then []
              else [stateErr stateNd.ref "struct.state.route.owner-mismatch" ("state references route " <> renderRouteRef routeRef <> " but the route belongs to state " <> renderStateRef routeEd.state)]
  in duplicateRouteErrs <> ownershipErrs

checkStateShape :: Lookups -> StateNd -> [ValidateErr]
checkStateShape lu stateNd =
  let kindErrs =
        case (stateNd.kind, stateNd.child) of
          (AtomicSk, Just childRegionRef) ->
            [stateErr stateNd.ref "struct.state.kind.atomic-has-child" ("atomic state cannot own child region " <> renderRegionRef childRegionRef)]
          (CompositeSk, Nothing) ->
            [stateErr stateNd.ref "struct.state.kind.composite-no-child" "composite state must own a child region"]
          (TerminalSk, Just childRegionRef) ->
            [stateErr stateNd.ref "struct.state.kind.terminal-has-child" ("terminal state cannot own child region " <> renderRegionRef childRegionRef)]
          _ ->
            []
      childErrs =
        case stateNd.child of
          Nothing ->
            []
          Just childRegionRef ->
            case lookupRegion lu childRegionRef of
              Nothing ->
                [stateErr stateNd.ref "struct.state.child.missing" ("state child region " <> renderRegionRef childRegionRef <> " does not exist")]
              Just childRegionNd ->
                if childRegionNd.parent == Just stateNd.ref
                  then []
                  else [stateErr stateNd.ref "struct.state.child.parent-mismatch" ("state child region " <> renderRegionRef childRegionRef <> " points to a different parent state")]
      terminalRouteErrs =
        case stateNd.kind of
          TerminalSk | not (V.null stateNd.routes) ->
            [stateErr stateNd.ref "struct.state.kind.terminal-has-routes" ("terminal state has " <> tshow (V.length stateNd.routes) <> " outgoing route(s)")]
          _ ->
            []
  in kindErrs <> childErrs <> terminalRouteErrs

checkCases :: Lookups -> [ValidateErr]
checkCases lu =
  concatMap (checkCase lu) (M.elems lu.caseByRef)

checkCase :: Lookups -> CaseNd -> [ValidateErr]
checkCase lu caseNd =
  case lookupState lu caseNd.state of
    Nothing ->
      [caseErr caseNd.ref "struct.case.state.missing" ("case refers to missing owning state " <> renderStateRef caseNd.state)]
    Just stateNd ->
      if stateOwnsCaseRef stateNd caseNd.ref
        then []
        else [caseErr caseNd.ref "struct.case.unreferenced" ("case belongs to state " <> renderStateRef caseNd.state <> " but that state does not reference it via entry or cases")]

checkRoutes :: Lookups -> [ValidateErr]
checkRoutes lu =
  concatMap (checkRoute lu) (M.elems lu.routeByRef)

checkRoute :: Lookups -> RouteEd -> [ValidateErr]
checkRoute lu routeEd =
  let stateExistenceErrs =
        case lookupState lu routeEd.state of
          Nothing ->
            [routeErr routeEd.ref "struct.route.state.missing" ("route refers to missing owning state " <> renderStateRef routeEd.state)]
          Just stateNd ->
            if routeEd.ref `V.elem` stateNd.routes
              then []
              else [routeErr routeEd.ref "struct.route.unreferenced" ("route belongs to state " <> renderStateRef routeEd.state <> " but that state does not reference it in its route vector")]
      caseRefErrs =
        case routeEd.caseRef of
          Nothing ->
            []
          Just caseRef ->
            case lookupCase lu caseRef of
              Nothing ->
                [routeErr routeEd.ref "struct.route.case.missing" ("route refers to missing case " <> renderCaseRef caseRef)]
              Just caseNd ->
                let ownerErrs =
                      if caseNd.state == routeEd.state
                        then []
                        else [routeErr routeEd.ref "struct.route.case.owner-mismatch" ("route refers to case " <> renderCaseRef caseRef <> " owned by state " <> renderStateRef caseNd.state <> " instead of " <> renderStateRef routeEd.state)]
                    signalCaseErrs =
                      case lookupState lu routeEd.state of
                        Just stateNd | not (caseRef `V.elem` stateNd.cases) ->
                          [routeErr routeEd.ref "struct.route.case.not-signal-case" ("route case ref " <> renderCaseRef caseRef <> " is not present in the owning state's ordinary case vector")]
                        _ ->
                          []
                in ownerErrs <> signalCaseErrs
  in stateExistenceErrs <> caseRefErrs

checkContainmentCycles :: Lookups -> [ValidateErr]
checkContainmentCycles lu =
  snd $ foldl' step (S.empty, []) allNodes
  where
    allNodes = fmap RegionCn (M.keys lu.regionByRef) <> fmap StateCn (M.keys lu.stateByRef)

    step :: (Set ContainNode, [ValidateErr]) -> ContainNode -> (Set ContainNode, [ValidateErr])
    step (done, errs) start
      | start `S.member` done = (done, errs)
      | otherwise =
          let (seenNodes, cycleNodesMb) = walkContainment lu S.empty [] start
              done' = S.union done seenNodes
              errs' =
                case cycleNodesMb of
                  Nothing -> errs
                  Just cycleNodes -> errs <> [containmentCycleErr cycleNodes]
          in (done', errs')

walkContainment :: Lookups -> Set ContainNode -> [ContainNode] -> ContainNode -> (Set ContainNode, Maybe [ContainNode])
walkContainment lu seen path node
  | node `S.member` seen =
      (S.fromList (node : path), Just (containmentCycleNodes node path))
  | otherwise =
      case parentContainNode lu node of
        Nothing -> (S.fromList (node : path), Nothing)
        Just next -> walkContainment lu (S.insert node seen) (node : path) next

parentContainNode :: Lookups -> ContainNode -> Maybe ContainNode
parentContainNode lu node =
  case node of
    RegionCn regionRef -> do
      regionNd <- lookupRegion lu regionRef
      StateCn <$> regionNd.parent
    StateCn stateRef -> do
      stateNd <- lookupState lu stateRef
      pure (RegionCn stateNd.parent)

containmentCycleNodes :: ContainNode -> [ContainNode] -> [ContainNode]
containmentCycleNodes node path =
  node : takeWhile (/= node) path

containmentCycleErr :: [ContainNode] -> ValidateErr
containmentCycleErr cycleNodes =
  case cycleNodes of
    [] ->
      machineErr "struct.containment.cycle" "illegal containment cycle detected"
    firstNode : _ ->
      locateContainNode firstNode $
        errorErr
          "struct.containment.cycle"
          ("illegal containment cycle detected: " <> renderContainmentCycle cycleNodes)
          Nothing

lookupRegion :: Lookups -> RegionRef -> Maybe RegionNd
lookupRegion lu regionRef = M.lookup regionRef lu.regionByRef

lookupState :: Lookups -> StateRef -> Maybe StateNd
lookupState lu stateRef = M.lookup stateRef lu.stateByRef

lookupCase :: Lookups -> CaseRef -> Maybe CaseNd
lookupCase lu caseRef = M.lookup caseRef lu.caseByRef

lookupRoute :: Lookups -> RouteRef -> Maybe RouteEd
lookupRoute lu routeRef = M.lookup routeRef lu.routeByRef

stateOwnsCaseRef :: StateNd -> CaseRef -> Bool
stateOwnsCaseRef stateNd caseRef =
  stateNd.entry == Just caseRef || caseRef `V.elem` stateNd.cases

duplicateRefs :: Ord a => [a] -> [a]
duplicateRefs xs =
  M.keys (M.filter (> (1 :: Int)) counts)
  where
    counts = M.fromListWith (+) (fmap (\x -> (x, 1 :: Int)) xs)

locateRef :: Maybe (ref -> ValidateErr -> ValidateErr) -> ref -> ValidateErr -> ValidateErr
locateRef mLocate ref err =
  case mLocate of
    Nothing -> atMachine err
    Just locate -> locate ref err

locateContainNode :: ContainNode -> ValidateErr -> ValidateErr
locateContainNode node err =
  case node of
    RegionCn regionRef -> atRegion regionRef err
    StateCn stateRef -> atState stateRef err

machineErr :: Text -> Text -> ValidateErr
machineErr code msg = atMachine (errorErr code msg Nothing)

regionErr :: RegionRef -> Text -> Text -> ValidateErr
regionErr regionRef code msg = atRegion regionRef (errorErr code msg Nothing)

stateErr :: StateRef -> Text -> Text -> ValidateErr
stateErr stateRef code msg = atState stateRef (errorErr code msg Nothing)

caseErr :: CaseRef -> Text -> Text -> ValidateErr
caseErr caseRef code msg = atCase caseRef (errorErr code msg Nothing)

routeErr :: RouteRef -> Text -> Text -> ValidateErr
routeErr routeRef code msg = atRoute routeRef (errorErr code msg Nothing)

renderContainmentCycle :: [ContainNode] -> Text
renderContainmentCycle cycleNodes =
  case cycleNodes of
    [] -> "<empty>"
    _ -> T.intercalate " -> " (fmap renderContainNode (cycleNodes <> [head cycleNodes]))

renderContainNode :: ContainNode -> Text
renderContainNode node =
  case node of
    RegionCn regionRef -> "region " <> renderRegionRef regionRef
    StateCn stateRef -> "state " <> renderStateRef stateRef

renderRegionRefs :: [RegionRef] -> Text
renderRegionRefs refs =
  T.intercalate ", " (fmap renderRegionRef refs)

renderRegionRef :: RegionRef -> Text
renderRegionRef ref = "#" <> tshow (regionRefWord32 ref)

renderStateRef :: StateRef -> Text
renderStateRef ref = "#" <> tshow (stateRefWord32 ref)

renderCaseRef :: CaseRef -> Text
renderCaseRef ref = "#" <> tshow (caseRefWord32 ref)

renderRouteRef :: RouteRef -> Text
renderRouteRef ref = "#" <> tshow (routeRefWord32 ref)

renderJoinRef :: JoinRef -> Text
renderJoinRef ref = "#" <> tshow (joinRefWord32 ref)

renderTimerRef :: TimerRef -> Text
renderTimerRef ref = "#" <> tshow (timerRefWord32 ref)

tshow :: Show a => a -> Text
tshow = T.pack . show