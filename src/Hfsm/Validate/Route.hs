module Hfsm.Validate.Route
  ( checkRoute
  ) where

import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.List (sortOn)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe, maybeToList)
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Vector (Vector)
import qualified Data.Vector as V
import Data.Word (Word32)

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
  , atCase
  , atRoute
  , atState
  , errorErr
  )

checkRoute :: MachineGraph -> [ValidateErr]
checkRoute graph =
  concat
    [ checkStateEntryRefs graph
    , checkStateCaseRefs graph
    , checkStateRouteRefs graph
    , checkCaseOwnership graph
    , checkRouteStateRefs graph
    , checkRouteCaseRefs graph
    , checkDuplicateRouteSlots graph
    , checkRouteTargets graph
    , checkRoutePlanRefs graph
    , checkRoutePlanConsistency graph
    ]

checkStateEntryRefs :: MachineGraph -> [ValidateErr]
checkStateEntryRefs graph =
  concatMap checkState (stateNodes graph)
  where
    checkState :: StateNd -> [ValidateErr]
    checkState st =
      case st.entry of
        Nothing -> []
        Just caseRef ->
          let stRef = st.ref
          in case lookupCase graph caseRef of
               Nothing ->
                 [ stateErr stRef "state.entry.missing"
                     ("state " <> renderStateRef stRef <> " has missing entry case " <> renderCaseRef caseRef)
                 ]
               Just caseNd ->
                 concat
                   [ [ stateErr stRef "state.entry.mismatch"
                         ("state " <> renderStateRef stRef <> " uses entry case " <> renderCaseRef caseRef
                           <> ", but that case belongs to " <> renderStateRef caseNd.state)
                     | caseNd.state /= stRef
                     ]
                   , [ stateErr stRef "state.entry.ordinary-case"
                         ("state " <> renderStateRef stRef <> " uses " <> renderCaseRef caseRef
                           <> " both as entry and as an ordinary signal case")
                     | caseRef `V.elem` st.cases
                     ]
                   ]

checkStateCaseRefs :: MachineGraph -> [ValidateErr]
checkStateCaseRefs graph =
  concatMap checkState (stateNodes graph)
  where
    checkState :: StateNd -> [ValidateErr]
    checkState st =
      let stRef = st.ref
          caseRefs = V.toList st.cases
      in duplicateStateCaseErrs stRef caseRefs <> concatMap (checkCaseRef stRef) caseRefs

    checkCaseRef stRef caseRef =
      case lookupCase graph caseRef of
        Nothing ->
          [ stateErr stRef "state.case-ref.missing"
              ("state " <> renderStateRef stRef <> " lists missing case " <> renderCaseRef caseRef)
          ]
        Just caseNd ->
          [ stateErr stRef "state.case-ref.mismatch"
              ("state " <> renderStateRef stRef <> " lists case " <> renderCaseRef caseRef
                <> ", but that case belongs to " <> renderStateRef caseNd.state)
          | caseNd.state /= stRef
          ]

checkStateRouteRefs :: MachineGraph -> [ValidateErr]
checkStateRouteRefs graph =
  concatMap checkState (stateNodes graph)
  where
    checkState :: StateNd -> [ValidateErr]
    checkState st =
      let stRef = st.ref
          routeRefs = V.toList st.routes
      in duplicateStateRouteErrs stRef routeRefs <> concatMap (checkRouteRef stRef) routeRefs

    checkRouteRef stRef routeRef =
      case lookupRoute graph routeRef of
        Nothing ->
          [ stateErr stRef "state.route-ref.missing"
              ("state " <> renderStateRef stRef <> " lists missing route " <> renderRouteRef routeRef)
          ]
        Just route ->
          [ stateErr stRef "state.route-ref.mismatch"
              ("state " <> renderStateRef stRef <> " lists route " <> renderRouteRef routeRef
                <> ", but that route belongs to " <> renderStateRef route.state)
          | route.state /= stRef
          ]

checkCaseOwnership :: MachineGraph -> [ValidateErr]
checkCaseOwnership graph =
  concatMap checkCase (caseNodes graph)
  where
    referencedCases = referencedCaseRefs graph

    checkCase :: CaseNd -> [ValidateErr]
    checkCase caseNd =
      let caseRef = caseNd.ref
          stRef = caseNd.state
      in case lookupState graph stRef of
           Nothing ->
             [ caseErr caseRef "case.state.missing"
                 ("case " <> renderCaseRef caseRef <> " belongs to missing state " <> renderStateRef stRef)
             ]
           Just st ->
             [ caseErr caseRef "case.orphan"
                 ("case " <> renderCaseRef caseRef <> " belongs to " <> renderStateRef stRef
                   <> ", but that state does not reference it as entry or ordinary case")
             | S.notMember caseRef referencedCases || not (stateReferencesCase st caseRef)
             ]

checkRouteStateRefs :: MachineGraph -> [ValidateErr]
checkRouteStateRefs graph =
  concatMap checkRouteEd (routeNodes graph)
  where
    checkRouteEd :: RouteEd -> [ValidateErr]
    checkRouteEd route =
      let routeRef = route.ref
          stRef = route.state
      in case lookupState graph stRef of
           Nothing ->
             [ routeErr routeRef "route.state.missing"
                 ("route " <> renderRouteRef routeRef <> " belongs to missing state " <> renderStateRef stRef)
             ]
           Just st ->
             [ routeErr routeRef "route.state.unindexed"
                 ("route " <> renderRouteRef routeRef <> " belongs to " <> renderStateRef stRef
                   <> ", but that state does not list the route")
             | not (routeRef `V.elem` st.routes)
             ]

checkRouteCaseRefs :: MachineGraph -> [ValidateErr]
checkRouteCaseRefs graph =
  concatMap checkRouteEd (routeNodes graph)
  where
    checkRouteEd :: RouteEd -> [ValidateErr]
    checkRouteEd route =
      case route.caseRef of
        Nothing -> []
        Just caseRef ->
          let routeRef = route.ref
              stRef = route.state
          in case lookupCase graph caseRef of
               Nothing ->
                 [ routeErr routeRef "route.case.missing"
                     ("route " <> renderRouteRef routeRef <> " refers to missing case " <> renderCaseRef caseRef)
                 ]
               Just caseNd ->
                 concat
                   [ [ routeErr routeRef "route.case.mismatch"
                         ("route " <> renderRouteRef routeRef <> " refers to case " <> renderCaseRef caseRef
                           <> ", but that case belongs to " <> renderStateRef caseNd.state
                           <> " instead of " <> renderStateRef stRef)
                     | caseNd.state /= stRef
                     ]
                   , case lookupState graph stRef of
                       Nothing -> []
                       Just st ->
                         concat
                           [ [ routeErr routeRef "route.case.entry-origin"
                                 ("route " <> renderRouteRef routeRef <> " uses entry case "
                                   <> renderCaseRef caseRef
                                   <> " as an ordinary case slot; entry-origin routes must not carry an explicit caseRef")
                             | st.entry == Just caseRef
                             ]
                           , [ routeErr routeRef "route.case.unindexed"
                                 ("route " <> renderRouteRef routeRef <> " refers to case "
                                   <> renderCaseRef caseRef
                                   <> ", but state " <> renderStateRef stRef
                                   <> " does not list that case among its ordinary signal cases")
                             | not (caseRef `V.elem` st.cases)
                             ]
                           ]
                   ]

checkDuplicateRouteSlots :: MachineGraph -> [ValidateErr]
checkDuplicateRouteSlots graph =
  emptyHandoffErrs <> duplicateSlotErrs
  where
    routes = routeNodes graph

    emptyHandoffErrs =
      [ routeErr route.ref "route.handoff.empty"
          ("route " <> renderRouteRef route.ref <> " has an empty handoff key")
      | route <- routes
      , T.null (normalizeText route.handoff)
      ]

    groups = M.fromListWith (<>) [(routeSlotKey route, [route]) | route <- routes]
    duplicateGroups = filter (\xs -> length xs > 1) (M.elems groups)

    duplicateSlotErrs =
      concatMap mkDupErrs duplicateGroups

    mkDupErrs routesInSlot =
      let routeRefsTxt = renderRouteRefs (map routeRefOf routesInSlot)
          slotTxt = renderRouteSlot (head routesInSlot)
      in
      [ routeErr route.ref "route.duplicate"
          ("duplicate route slot " <> slotTxt <> "; conflicting routes: " <> routeRefsTxt)
      | route <- routesInSlot
      ]

checkRouteTargets :: MachineGraph -> [ValidateErr]
checkRouteTargets graph =
  concatMap checkRouteEd (routeNodes graph)
  where
    checkRouteEd :: RouteEd -> [ValidateErr]
    checkRouteEd route =
      case route.plan.target of
        GotoRt targetRef ->
          [ routeErr route.ref "route.target.missing"
              ("route " <> renderRouteRef route.ref <> " targets missing state " <> renderStateRef targetRef)
          | lookupState graph targetRef == Nothing
          ]
        StayRt -> []
        CompleteRt -> []
        FailRt _ -> []

checkRoutePlanRefs :: MachineGraph -> [ValidateErr]
checkRoutePlanRefs graph =
  concatMap checkRouteEd (routeNodes graph)
  where
    checkRouteEd route =
      concat
        [ checkWaitPlan graph route
        , checkJoinPlan graph route
        , checkSpawnPlans graph route
        , checkTimerPlans graph route
        ]

checkRoutePlanConsistency :: MachineGraph -> [ValidateErr]
checkRoutePlanConsistency graph =
  concatMap checkRouteEd (routeNodes graph)
  where
    checkRouteEd route =
      checkCompletionTargetBookkeeping route <> checkDuplicateBreaks route

checkWaitPlan :: MachineGraph -> RouteEd -> [ValidateErr]
checkWaitPlan graph route =
  case route.plan.wait of
    WaitNoneWg -> []
    WaitSignalWg -> []
    WaitJoinWg joinRef ->
      let routeRef = route.ref
          planJoinRef = joinPlanRef route.plan.join
      in
      concat
        [ [ routeErr routeRef "route.wait.join.missing"
              ("route " <> renderRouteRef routeRef <> " waits on missing join " <> renderJoinRef joinRef)
          | lookupJoin graph joinRef == Nothing
          ]
        , [ routeErr routeRef "route.wait.join-mismatch"
              ("route " <> renderRouteRef routeRef <> " waits on " <> renderJoinRef joinRef
                <> ", but its join plan uses " <> renderJoinRef otherJoinRef)
          | otherJoinRef <- maybeToList planJoinRef
          , otherJoinRef /= joinRef
          ]
        ]
    WaitTimerWg timerRef ->
      [ routeErr route.ref "route.wait.timer.missing"
          ("route " <> renderRouteRef route.ref <> " waits on missing timer " <> renderTimerRef timerRef)
      | lookupTimer graph timerRef == Nothing
      ]

checkJoinPlan :: MachineGraph -> RouteEd -> [ValidateErr]
checkJoinPlan graph route =
  case route.plan.join of
    JoinNoneJg -> []
    JoinAllJg joinRef ->
      joinExistsErrs graph route joinRef
        <> joinModeErrs graph route joinRef "all" (modeMatchesAll . (.mode))
    JoinAnyJg joinRef ->
      joinExistsErrs graph route joinRef
        <> joinModeErrs graph route joinRef "any" (modeMatchesAny . (.mode))
    JoinCountJg joinRef count ->
      let routeRef = route.ref
      in
      [ routeErr routeRef "route.join.count-nonpositive"
          ("route " <> renderRouteRef routeRef <> " uses non-positive join count " <> tshow count
            <> " for " <> renderJoinRef joinRef)
      | count <= 0
      ]
      <> joinExistsErrs graph route joinRef
      <> joinModeErrs graph route joinRef ("count(" <> tshow count <> ")") (\joinNd -> modeMatchesCount count joinNd.mode)

checkSpawnPlans :: MachineGraph -> RouteEd -> [ValidateErr]
checkSpawnPlans graph route =
  duplicateSpawnKeyErrs route <> concatMap checkSpawn route.plan.spawn
  where
    routeRef = route.ref
    routeJoin = joinPlanRef route.plan.join

    checkSpawn :: SpawnPlanG -> [ValidateErr]
    checkSpawn spawnPlan =
      let childKey = normalizeText spawnPlan.child
      in
      [ routeErr routeRef "route.spawn.child.empty"
          ("route " <> renderRouteRef routeRef <> " contains a spawn with an empty child machine key")
      | T.null childKey
      ]
      <> case spawnPlan.join of
           Nothing -> []
           Just joinRef ->
             concat
               [ [ routeErr routeRef "route.spawn.join.missing"
                     ("route " <> renderRouteRef routeRef <> " attaches spawned child "
                       <> quoteText childKey <> " to missing join " <> renderJoinRef joinRef)
                 | lookupJoin graph joinRef == Nothing
                 ]
               , [ routeErr routeRef "route.spawn.join-unexpected"
                     ("route " <> renderRouteRef routeRef <> " attaches spawned child "
                       <> quoteText childKey <> " to " <> renderJoinRef joinRef
                       <> ", but the route has no join plan")
                 | routeJoin == Nothing
                 ]
               , [ routeErr routeRef "route.spawn.join-mismatch"
                     ("route " <> renderRouteRef routeRef <> " attaches spawned child "
                       <> quoteText childKey <> " to " <> renderJoinRef joinRef
                       <> ", but the route join plan uses " <> renderJoinRef expectedJoinRef)
                 | expectedJoinRef <- maybeToList routeJoin
                 , expectedJoinRef /= joinRef
                 ]
               ]

checkTimerPlans :: MachineGraph -> RouteEd -> [ValidateErr]
checkTimerPlans graph route =
  duplicateTimerErrs route <> concatMap checkTimerPlan route.plan.timers
  where
    routeRef = route.ref

    checkTimerPlan :: TimerPlanG -> [ValidateErr]
    checkTimerPlan timerPlan =
      [ routeErr routeRef "route.timer.missing"
          ("route " <> renderRouteRef routeRef <> " schedules missing timer " <> renderTimerRef timerPlan.ref)
      | lookupTimer graph timerPlan.ref == Nothing
      ]

checkCompletionTargetBookkeeping :: RouteEd -> [ValidateErr]
checkCompletionTargetBookkeeping route =
  if isCompletionTarget route.plan.target
    then waitErrs <> spawnErrs <> joinErrs <> timerErrs
    else []
  where
    routeRef = route.ref
    targetTxt = renderCompletionTarget route.plan.target

    waitErrs =
      [ routeErr routeRef "route.terminal.wait"
          ("route " <> renderRouteRef routeRef <> " uses " <> targetTxt
            <> " but still declares wait bookkeeping")
      | route.plan.wait /= WaitNoneWg
      ]

    spawnErrs =
      [ routeErr routeRef "route.terminal.spawn"
          ("route " <> renderRouteRef routeRef <> " uses " <> targetTxt
            <> " but still spawns " <> tshow (length route.plan.spawn) <> " child machine(s)")
      | not (null route.plan.spawn)
      ]

    joinErrs =
      [ routeErr routeRef "route.terminal.join"
          ("route " <> renderRouteRef routeRef <> " uses " <> targetTxt
            <> " but still declares join bookkeeping")
      | route.plan.join /= JoinNoneJg
      ]

    timerErrs =
      [ routeErr routeRef "route.terminal.timer"
          ("route " <> renderRouteRef routeRef <> " uses " <> targetTxt
            <> " but still schedules " <> tshow (length route.plan.timers) <> " timer(s)")
      | not (null route.plan.timers)
      ]

checkDuplicateBreaks :: RouteEd -> [ValidateErr]
checkDuplicateBreaks route =
  [ routeErr route.ref "route.break.duplicate"
      ("route " <> renderRouteRef route.ref <> " lists breakpoint " <> renderBreakPlan breakPlan <> " more than once")
  | breakPlan <- duplicatesOrd route.plan.break
  ]

duplicateStateCaseErrs :: StateRef -> [CaseRef] -> [ValidateErr]
duplicateStateCaseErrs stRef caseRefs =
  [ stateErr stRef "state.case-ref.duplicate"
      ("state " <> renderStateRef stRef <> " lists case " <> renderCaseRef caseRef <> " more than once")
  | caseRef <- duplicatesOrd caseRefs
  ]

duplicateStateRouteErrs :: StateRef -> [RouteRef] -> [ValidateErr]
duplicateStateRouteErrs stRef routeRefs =
  [ stateErr stRef "state.route-ref.duplicate"
      ("state " <> renderStateRef stRef <> " lists route " <> renderRouteRef routeRef <> " more than once")
  | routeRef <- duplicatesOrd routeRefs
  ]

duplicateSpawnKeyErrs :: RouteEd -> [ValidateErr]
duplicateSpawnKeyErrs route =
  [ routeErr route.ref "route.spawn.key.duplicate"
      ("route " <> renderRouteRef route.ref <> " uses duplicate spawn key " <> quoteText keyTxt)
  | keyTxt <- duplicateSpawnKeys route
  ]

duplicateTimerErrs :: RouteEd -> [ValidateErr]
duplicateTimerErrs route =
  [ routeErr route.ref "route.timer.duplicate"
      ("route " <> renderRouteRef route.ref <> " schedules timer " <> renderTimerRef timerRef <> " more than once")
  | timerRef <- duplicateTimerRefs route
  ]

joinExistsErrs :: MachineGraph -> RouteEd -> JoinRef -> [ValidateErr]
joinExistsErrs graph route joinRef =
  [ routeErr route.ref "route.join.missing"
      ("route " <> renderRouteRef route.ref <> " refers to missing join " <> renderJoinRef joinRef)
  | lookupJoin graph joinRef == Nothing
  ]

joinModeErrs :: MachineGraph -> RouteEd -> JoinRef -> Text -> (JoinNd -> Bool) -> [ValidateErr]
joinModeErrs graph route joinRef expectedTxt matches =
  case lookupJoin graph joinRef of
    Nothing -> []
    Just joinNd ->
      [ routeErr route.ref "route.join.mode-mismatch"
          ("route " <> renderRouteRef route.ref <> " expects join " <> renderJoinRef joinRef
            <> " to use mode " <> expectedTxt <> ", but the graph defines " <> renderJoinMode joinNd.mode)
      | not (matches joinNd)
      ]

stateNodes :: MachineGraph -> [StateNd]
stateNodes graph = IM.elems graph.states

caseNodes :: MachineGraph -> [CaseNd]
caseNodes graph = IM.elems graph.cases

routeNodes :: MachineGraph -> [RouteEd]
routeNodes graph = IM.elems graph.routes

referencedCaseRefs :: MachineGraph -> Set CaseRef
referencedCaseRefs graph =
  S.fromList $
    concatMap refsForState (stateNodes graph)
  where
    refsForState :: StateNd -> [CaseRef]
    refsForState st = maybeToList st.entry <> V.toList st.cases

stateReferencesCase :: StateNd -> CaseRef -> Bool
stateReferencesCase st caseRef =
  st.entry == Just caseRef || caseRef `V.elem` st.cases

routeSlotKey :: RouteEd -> (StateRef, Maybe CaseRef, Text)
routeSlotKey route = (route.state, route.caseRef, normalizeText route.handoff)

renderRouteSlot :: RouteEd -> Text
renderRouteSlot route =
  renderStateRef route.state <> " / " <> renderCaseSlot route.caseRef <> " / " <> quoteText (normalizeText route.handoff)

renderCaseSlot :: Maybe CaseRef -> Text
renderCaseSlot maybeCaseRef =
  case maybeCaseRef of
    Nothing -> "entry/structural"
    Just caseRef -> renderCaseRef caseRef

duplicateSpawnKeys :: RouteEd -> [Text]
duplicateSpawnKeys route =
  duplicatesOrd $
    mapMaybe normalizeMaybeText [spawnPlan.key | spawnPlan <- route.plan.spawn]

duplicateTimerRefs :: RouteEd -> [TimerRef]
duplicateTimerRefs route =
  duplicatesOrd [timerPlan.ref | timerPlan <- route.plan.timers]

duplicatesOrd :: Ord a => [a] -> [a]
duplicatesOrd xs =
  M.keys $
    M.filter (> (1 :: Int)) $
      M.fromListWith (+) [(x, 1 :: Int) | x <- xs]

lookupState :: MachineGraph -> StateRef -> Maybe StateNd
lookupState graph stRef =
  IM.lookup (key32 (stateRefWord32 stRef)) graph.states

lookupCase :: MachineGraph -> CaseRef -> Maybe CaseNd
lookupCase graph caseRef =
  IM.lookup (key32 (caseRefWord32 caseRef)) graph.cases

lookupRoute :: MachineGraph -> RouteRef -> Maybe RouteEd
lookupRoute graph routeRef =
  IM.lookup (key32 (routeRefWord32 routeRef)) graph.routes

lookupJoin :: MachineGraph -> JoinRef -> Maybe JoinNd
lookupJoin graph joinRef =
  IM.lookup (key32 (joinRefWord32 joinRef)) graph.joins

lookupTimer :: MachineGraph -> TimerRef -> Maybe TimerNd
lookupTimer graph timerRef =
  IM.lookup (key32 (timerRefWord32 timerRef)) graph.timers

joinPlanRef :: JoinPlanG -> Maybe JoinRef
joinPlanRef joinPlan =
  case joinPlan of
    JoinNoneJg -> Nothing
    JoinAllJg joinRef -> Just joinRef
    JoinAnyJg joinRef -> Just joinRef
    JoinCountJg joinRef _ -> Just joinRef

modeMatchesAll :: JoinMode -> Bool
modeMatchesAll joinMode =
  case joinMode of
    AllJm -> True
    AnyJm -> False
    CountJm _ -> False

modeMatchesAny :: JoinMode -> Bool
modeMatchesAny joinMode =
  case joinMode of
    AllJm -> False
    AnyJm -> True
    CountJm _ -> False

modeMatchesCount :: Int -> JoinMode -> Bool
modeMatchesCount count joinMode =
  case joinMode of
    CountJm n -> n == count
    AllJm -> False
    AnyJm -> False

isCompletionTarget :: RouteTarget -> Bool
isCompletionTarget target =
  case target of
    CompleteRt -> True
    FailRt _ -> True
    StayRt -> False
    GotoRt _ -> False

renderCompletionTarget :: RouteTarget -> Text
renderCompletionTarget target =
  case target of
    CompleteRt -> "completion target"
    FailRt msg -> "failure target " <> quoteText msg
    StayRt -> "stay target"
    GotoRt stRef -> "goto target " <> renderStateRef stRef

renderJoinMode :: JoinMode -> Text
renderJoinMode joinMode =
  case joinMode of
    AllJm -> "all"
    AnyJm -> "any"
    CountJm n -> "count(" <> tshow n <> ")"

renderBreakPlan :: BreakPlanG -> Text
renderBreakPlan breakPlan =
  case breakPlan of
    BreakBeforeCaseBg -> "before-case"
    BreakAfterCaseBg -> "after-case"
    BreakBeforeRouteBg -> "before-route"
    BreakAfterRouteBg -> "after-route"
    BreakBeforeCommitBg -> "before-commit"

stateErr :: StateRef -> Text -> Text -> ValidateErr
stateErr stRef code msg =
  atState stRef (errorErr code msg Nothing)

caseErr :: CaseRef -> Text -> Text -> ValidateErr
caseErr caseRef code msg =
  atCase caseRef (errorErr code msg Nothing)

routeErr :: RouteRef -> Text -> Text -> ValidateErr
routeErr routeRef code msg =
  atRoute routeRef (errorErr code msg Nothing)

renderStateRef :: StateRef -> Text
renderStateRef stRef = "state#" <> tshow (stateRefWord32 stRef)

renderCaseRef :: CaseRef -> Text
renderCaseRef caseRef = "case#" <> tshow (caseRefWord32 caseRef)

renderRouteRef :: RouteRef -> Text
renderRouteRef routeRef = "route#" <> tshow (routeRefWord32 routeRef)

renderJoinRef :: JoinRef -> Text
renderJoinRef joinRef = "join#" <> tshow (joinRefWord32 joinRef)

renderTimerRef :: TimerRef -> Text
renderTimerRef timerRef = "timer#" <> tshow (timerRefWord32 timerRef)

renderRouteRefs :: [RouteRef] -> Text
renderRouteRefs routeRefs =
  T.intercalate ", " $
    fmap renderRouteRef $
      sortOn routeRefWord32 routeRefs

routeRefOf :: RouteEd -> RouteRef
routeRefOf route = route.ref

normalizeText :: Text -> Text
normalizeText = T.strip

normalizeMaybeText :: Maybe Text -> Maybe Text
normalizeMaybeText maybeTxt =
  case maybeTxt of
    Nothing -> Nothing
    Just txt ->
      let norm = normalizeText txt
      in if T.null norm then Nothing else Just norm

quoteText :: Text -> Text
quoteText txt = "\"" <> txt <> "\""

key32 :: Word32 -> Int
key32 = fromIntegral

tshow :: Show a => a -> Text
tshow = T.pack . show