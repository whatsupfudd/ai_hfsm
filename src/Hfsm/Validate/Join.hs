module Hfsm.Validate.Join
  ( checkJoin
  ) where

import Data.Foldable (foldl')
import Data.IntMap.Strict (IntMap)
import qualified Data.IntMap.Strict as IM
import Data.Maybe (catMaybes, isJust)
import Data.Text (Text)
import qualified Data.Text as T

import Hfsm.Core.Name (joinNameText)
import Hfsm.Core.Ref (JoinRef, RouteRef, joinRefWord32, routeRefWord32)
import Hfsm.Graph.Def
  ( JoinMode(..)
  , JoinNd(..)
  , JoinPlanG(..)
  , MachineGraph(..)
  , RouteEd(..)
  , SpawnPlanG(..)
  , WaitPlanG(..)
  )
import Hfsm.Validate.Error
  ( ValidateErr
  , ValidateLoc(..)
  , errorErr
  , warnErr
  )
import Hfsm.Graph.Def (RoutePlan(..))

checkJoin :: MachineGraph -> [ValidateErr]
checkJoin graph =
  let usageIx = buildJoinUsage graph
      routeErrs = concatMap (checkRouteJoin graph) (IM.elems graph.routes)
      joinErrs = concatMap (checkJoinDef usageIx) (IM.elems graph.joins)
  in routeErrs <> joinErrs

data JoinUsage = JoinUsage
  { spawnByRoute :: IntMap Int
  , waitRoutes :: [RouteRef]
  , joinRoutes :: [(RouteRef, JoinPlanG)]
  }

emptyJoinUsage :: JoinUsage
emptyJoinUsage =
  JoinUsage
    { spawnByRoute = IM.empty
    , waitRoutes = []
    , joinRoutes = []
    }

buildJoinUsage :: MachineGraph -> IntMap JoinUsage
buildJoinUsage graph =
  foldl' addRouteUsage IM.empty (IM.elems graph.routes)

addRouteUsage :: IntMap JoinUsage -> RouteEd -> IntMap JoinUsage
addRouteUsage acc route =
  let acc1 = foldl' (\m ref -> addSpawnUsage ref route.ref 1 m) acc (routeSpawnJoinRefs route)
      acc2 =
        case waitJoinRef route of
          Nothing -> acc1
          Just ref -> addWaitUsage ref route.ref acc1
      acc3 =
        case routeJoinPlanRef route of
          Nothing -> acc2
          Just ref -> addJoinPlanUsage ref route.ref route.plan.join acc2
  in acc3

addSpawnUsage :: JoinRef -> RouteRef -> Int -> IntMap JoinUsage -> IntMap JoinUsage
addSpawnUsage ref route n =
  IM.alter alterUsage (joinRefKey ref)
  where
    alterUsage Nothing =
      Just emptyJoinUsage { spawnByRoute = IM.singleton (routeRefKey route) n }
    alterUsage (Just usage) =
      Just usage { spawnByRoute = IM.insertWith (+) (routeRefKey route) n usage.spawnByRoute }

addWaitUsage :: JoinRef -> RouteRef -> IntMap JoinUsage -> IntMap JoinUsage
addWaitUsage ref route =
  IM.alter alterUsage (joinRefKey ref)
  where
    alterUsage Nothing =
      Just emptyJoinUsage { waitRoutes = [route] }
    alterUsage (Just usage) =
      Just usage { waitRoutes = usage.waitRoutes <> [route] }

addJoinPlanUsage :: JoinRef -> RouteRef -> JoinPlanG -> IntMap JoinUsage -> IntMap JoinUsage
addJoinPlanUsage ref route joinPlan =
  IM.alter alterUsage (joinRefKey ref)
  where
    alterUsage Nothing =
      Just emptyJoinUsage { joinRoutes = [(route, joinPlan)] }
    alterUsage (Just usage) =
      Just usage { joinRoutes = usage.joinRoutes <> [(route, joinPlan)] }

checkRouteJoin :: MachineGraph -> RouteEd -> [ValidateErr]
checkRouteJoin graph route =
  missingJoinRefErrs graph route
    <> routeJoinPlanErrs graph route

missingJoinRefErrs :: MachineGraph -> RouteEd -> [ValidateErr]
missingJoinRefErrs graph route =
  spawnMissingErrs graph route
    <> waitMissingErrs graph route
    <> joinPlanMissingErrs graph route

spawnMissingErrs :: MachineGraph -> RouteEd -> [ValidateErr]
spawnMissingErrs graph route =
  let refs = routeSpawnJoinRefs route
  in fmap (missingSpawnJoinErr route) (filter (not . hasJoinRef graph) refs)

waitMissingErrs :: MachineGraph -> RouteEd -> [ValidateErr]
waitMissingErrs graph route =
  case waitJoinRef route of
    Nothing -> []
    Just ref ->
      if hasJoinRef graph ref
        then []
        else [missingWaitJoinErr route ref]

joinPlanMissingErrs :: MachineGraph -> RouteEd -> [ValidateErr]
joinPlanMissingErrs graph route =
  case routeJoinPlanRef route of
    Nothing -> []
    Just ref ->
      if hasJoinRef graph ref
        then []
        else [missingRouteJoinErr route ref]

routeJoinPlanErrs :: MachineGraph -> RouteEd -> [ValidateErr]
routeJoinPlanErrs graph route =
  case route.plan.join of
    JoinNoneJg ->
      let joinedSpawnCount = length (routeSpawnJoinRefs route)
      in if joinedSpawnCount == 0
           then []
           else [spawnWithoutJoinPlanErr route joinedSpawnCount]

    joinPlan ->
      let ref = joinPlanRef joinPlan
          totalSpawnCount = routeSpawnCount route
          joinedSpawnCount = routeSpawnJoinCount ref route
          attachErrs =
            if totalSpawnCount == 0
              then [routeJoinNoSpawnErr route ref]
              else if joinedSpawnCount /= totalSpawnCount
                     then [routeJoinPartialAttachErr route ref joinedSpawnCount totalSpawnCount]
                     else []
          modeErrs =
            case lookupJoinNd graph ref of
              Nothing -> []
              Just joinNd ->
                if joinPlanMatchesMode joinPlan joinNd.mode
                  then []
                  else [routeJoinModeMismatchErr route joinNd joinPlan]
          countErrs =
            case joinPlan of
              JoinCountJg _ n
                | n <= 0 -> [routeJoinNonPositiveCountErr route ref n]
                | totalSpawnCount > 0 && joinedSpawnCount < n ->
                    [routeJoinUnsatisfiableCountErr route ref n joinedSpawnCount]
                | otherwise -> []
              _ -> []
      in attachErrs <> modeErrs <> countErrs

checkJoinDef :: IntMap JoinUsage -> JoinNd -> [ValidateErr]
checkJoinDef usageIx joinNd =
  let usage = lookupJoinUsage usageIx joinNd.ref
      spawnCount = joinUsageTotalSpawn usage
      hasWait = not (null usage.waitRoutes)
      hasJoinPlan = not (null usage.joinRoutes)
      hasConsumer = hasWait || hasJoinPlan
      hasAnyRef = spawnCount > 0 || hasConsumer
      unusedErrs =
        if hasAnyRef
          then []
          else [unusedJoinWarn joinNd]
      noChildWorkErrs =
        if spawnCount == 0 && hasConsumer
          then [joinHasNoChildWorkErr joinNd]
          else []
      orphanSpawnErrs =
        if spawnCount > 0 && not hasConsumer
          then [orphanJoinErr joinNd spawnCount]
          else []
      modeErrs = joinModeErrs joinNd usage
  in unusedErrs <> noChildWorkErrs <> orphanSpawnErrs <> modeErrs

joinModeErrs :: JoinNd -> JoinUsage -> [ValidateErr]
joinModeErrs joinNd usage =
  let spawnCount = joinUsageTotalSpawn usage
      waitOnly = not (null usage.waitRoutes) && null usage.joinRoutes
  in case joinNd.mode of
       CountJm n
         | n <= 0 ->
             [joinNonPositiveCountErr joinNd n]
         | waitOnly && spawnCount > 0 && spawnCount < n ->
             [joinUnsatisfiableCountErr joinNd n spawnCount]
         | otherwise ->
             []
       _ ->
         []

lookupJoinUsage :: IntMap JoinUsage -> JoinRef -> JoinUsage
lookupJoinUsage usageIx ref =
  IM.findWithDefault emptyJoinUsage (joinRefKey ref) usageIx

joinUsageTotalSpawn :: JoinUsage -> Int
joinUsageTotalSpawn usage =
  sum (IM.elems usage.spawnByRoute)

routeSpawnCount :: RouteEd -> Int
routeSpawnCount route =
  length route.plan.spawn

routeSpawnJoinRefs :: RouteEd -> [JoinRef]
routeSpawnJoinRefs route =
  catMaybes (fmap spawnJoinRef route.plan.spawn)

routeSpawnJoinCount :: JoinRef -> RouteEd -> Int
routeSpawnJoinCount ref route =
  length (filter (== ref) (routeSpawnJoinRefs route))

waitJoinRef :: RouteEd -> Maybe JoinRef
waitJoinRef route =
  case route.plan.wait of
    WaitJoinWg ref -> Just ref
    _ -> Nothing

routeJoinPlanRef :: RouteEd -> Maybe JoinRef
routeJoinPlanRef route =
  case route.plan.join of
    JoinNoneJg -> Nothing
    JoinAllJg ref -> Just ref
    JoinAnyJg ref -> Just ref
    JoinCountJg ref _ -> Just ref

joinPlanRef :: JoinPlanG -> JoinRef
joinPlanRef joinPlan =
  case joinPlan of
    JoinAllJg ref -> ref
    JoinAnyJg ref -> ref
    JoinCountJg ref _ -> ref
    JoinNoneJg -> error "joinPlanRef: JoinNoneJg has no join ref"

spawnJoinRef :: SpawnPlanG -> Maybe JoinRef
spawnJoinRef spawn =
  spawn.join

lookupJoinNd :: MachineGraph -> JoinRef -> Maybe JoinNd
lookupJoinNd graph ref =
  IM.lookup (joinRefKey ref) graph.joins

hasJoinRef :: MachineGraph -> JoinRef -> Bool
hasJoinRef graph ref =
  IM.member (joinRefKey ref) graph.joins

joinPlanMatchesMode :: JoinPlanG -> JoinMode -> Bool
joinPlanMatchesMode joinPlan mode =
  case (joinPlan, mode) of
    (JoinAllJg _, AllJm) -> True
    (JoinAnyJg _, AnyJm) -> True
    (JoinCountJg _ n1, CountJm n2) -> n1 == n2
    _ -> False

missingSpawnJoinErr :: RouteEd -> JoinRef -> ValidateErr
missingSpawnJoinErr route ref =
  errorErr
    "join.ref.missing.spawn"
    ("route " <> routeRefText route.ref <> " spawns child work attached to missing join ref " <> joinRefText ref)
    (Just (RouteVl route.ref))

missingWaitJoinErr :: RouteEd -> JoinRef -> ValidateErr
missingWaitJoinErr route ref =
  errorErr
    "join.ref.missing.wait"
    ("route " <> routeRefText route.ref <> " waits on missing join ref " <> joinRefText ref)
    (Just (RouteVl route.ref))

missingRouteJoinErr :: RouteEd -> JoinRef -> ValidateErr
missingRouteJoinErr route ref =
  errorErr
    "join.ref.missing.route"
    ("route " <> routeRefText route.ref <> " declares missing join ref " <> joinRefText ref <> " in its join plan")
    (Just (RouteVl route.ref))

spawnWithoutJoinPlanErr :: RouteEd -> Int -> ValidateErr
spawnWithoutJoinPlanErr route joinedSpawnCount =
  errorErr
    "join.route.spawn_without_plan"
    ("route " <> routeRefText route.ref <> " attaches " <> tshow joinedSpawnCount <> " spawned child(ren) to join refs but its route join plan is none")
    (Just (RouteVl route.ref))

routeJoinNoSpawnErr :: RouteEd -> JoinRef -> ValidateErr
routeJoinNoSpawnErr route ref =
  errorErr
    "join.route.no_spawn"
    ("route " <> routeRefText route.ref <> " declares join ref " <> joinRefText ref <> " in its join plan but spawns no child work")
    (Just (RouteVl route.ref))

routeJoinPartialAttachErr :: RouteEd -> JoinRef -> Int -> Int -> ValidateErr
routeJoinPartialAttachErr route ref joinedSpawnCount totalSpawnCount =
  errorErr
    "join.route.partial_attach"
    ("route " <> routeRefText route.ref <> " declares join ref " <> joinRefText ref <> " in its join plan but only "
      <> tshow joinedSpawnCount <> " of " <> tshow totalSpawnCount <> " spawned child(ren) are attached to that join")
    (Just (RouteVl route.ref))

routeJoinModeMismatchErr :: RouteEd -> JoinNd -> JoinPlanG -> ValidateErr
routeJoinModeMismatchErr route joinNd joinPlan =
  errorErr
    "join.route.mode_mismatch"
    ("route " <> routeRefText route.ref <> " uses join policy " <> renderJoinPlan joinPlan <> " for " <> joinLabel joinNd
      <> " but the join is declared as " <> renderJoinMode joinNd.mode)
    (Just (RouteVl route.ref))

routeJoinNonPositiveCountErr :: RouteEd -> JoinRef -> Int -> ValidateErr
routeJoinNonPositiveCountErr route ref n =
  errorErr
    "join.route.count.non_positive"
    ("route " <> routeRefText route.ref <> " declares count=" <> tshow n <> " for join ref " <> joinRefText ref <> ", but count joins must be positive")
    (Just (RouteVl route.ref))

routeJoinUnsatisfiableCountErr :: RouteEd -> JoinRef -> Int -> Int -> ValidateErr
routeJoinUnsatisfiableCountErr route ref required actual =
  errorErr
    "join.route.count.unsatisfiable"
    ("route " <> routeRefText route.ref <> " requires count=" <> tshow required <> " for join ref " <> joinRefText ref
      <> " but only " <> tshow actual <> " spawned child(ren) are attached to that join")
    (Just (RouteVl route.ref))

unusedJoinWarn :: JoinNd -> ValidateErr
unusedJoinWarn joinNd =
  warnErr
    "join.unused"
    (joinLabel joinNd <> " is declared but never referenced")
    (Just (JoinVl joinNd.ref))

joinHasNoChildWorkErr :: JoinNd -> ValidateErr
joinHasNoChildWorkErr joinNd =
  errorErr
    "join.no_child_work"
    (joinLabel joinNd <> " is referenced by wait or join plans but no spawned child work is ever attached to it")
    (Just (JoinVl joinNd.ref))

orphanJoinErr :: JoinNd -> Int -> ValidateErr
orphanJoinErr joinNd spawnCount =
  errorErr
    "join.orphan"
    (joinLabel joinNd <> " is attached to " <> tshow spawnCount <> " spawned child(ren) but no route ever waits on it or declares it in a join plan")
    (Just (JoinVl joinNd.ref))

joinNonPositiveCountErr :: JoinNd -> Int -> ValidateErr
joinNonPositiveCountErr joinNd n =
  errorErr
    "join.count.non_positive"
    (joinLabel joinNd <> " has count=" <> tshow n <> ", but count joins must be positive")
    (Just (JoinVl joinNd.ref))

joinUnsatisfiableCountErr :: JoinNd -> Int -> Int -> ValidateErr
joinUnsatisfiableCountErr joinNd required actual =
  errorErr
    "join.count.unsatisfiable"
    (joinLabel joinNd <> " requires count=" <> tshow required <> " but at most "
      <> tshow actual <> " spawned child(ren) are structurally attached to it")
    (Just (JoinVl joinNd.ref))

joinLabel :: JoinNd -> Text
joinLabel joinNd =
  case joinNd.name of
    Nothing -> "join ref " <> joinRefText joinNd.ref
    Just name -> "join " <> joinNameText name <> " (ref " <> joinRefText joinNd.ref <> ")"

renderJoinMode :: JoinMode -> Text
renderJoinMode mode =
  case mode of
    AllJm -> "all"
    AnyJm -> "any"
    CountJm n -> "count=" <> tshow n

renderJoinPlan :: JoinPlanG -> Text
renderJoinPlan joinPlan =
  case joinPlan of
    JoinNoneJg -> "none"
    JoinAllJg _ -> "all"
    JoinAnyJg _ -> "any"
    JoinCountJg _ n -> "count=" <> tshow n

joinRefKey :: JoinRef -> Int
joinRefKey = fromIntegral . joinRefWord32

routeRefKey :: RouteRef -> Int
routeRefKey = fromIntegral . routeRefWord32

joinRefText :: JoinRef -> Text
joinRefText = tshow . joinRefWord32

routeRefText :: RouteRef -> Text
routeRefText = tshow . routeRefWord32

tshow :: Show a => a -> Text
tshow = T.pack . show