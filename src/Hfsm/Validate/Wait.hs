module Hfsm.Validate.Wait
  ( checkWait
  ) where

import qualified Data.IntMap.Strict as IM
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Hfsm.Core.Ref
  ( JoinRef
  , RouteRef
  , TimerRef
  , joinRefWord32
  , routeRefWord32
  , timerRefWord32
  )
import Hfsm.Graph.Def
  ( JoinPlanG (..)
  , MachineGraph (..)
  , RouteEd (..)
  , RoutePlan (..)
  , RouteTarget (..)
  , TimerPlanG (..)
  , WaitPlanG (..)
  )
import Hfsm.Validate.Error
  ( ValidateErr
  , atRoute
  , errorErr
  , warnErr
  )

checkWait :: MachineGraph -> [ValidateErr]
checkWait graph =
  concatMap (checkRouteWait graph) (IM.elems graph.routes)

checkRouteWait :: MachineGraph -> RouteEd -> [ValidateErr]
checkRouteWait graph routeEd =
  checkWaitRefs graph routeEd
    <> checkTimerPlanRefs graph routeEd
    <> checkDuplicateTimerRefs routeEd
    <> checkWaitTarget routeEd
    <> checkWaitJoinConsistency routeEd
    <> checkWaitTimerConsistency graph routeEd

checkWaitRefs :: MachineGraph -> RouteEd -> [ValidateErr]
checkWaitRefs graph routeEd =
  case routeEd.plan.wait of
    WaitNoneWg -> []
    WaitSignalWg -> []
    WaitJoinWg joinRef ->
      if joinExists graph joinRef
        then []
        else
          [ routeError routeEd "wait.join.unknown" $
              routeDiagHeader routeEd <> " waits on unknown join ref " <> joinRefText joinRef
          ]
    WaitTimerWg timerRef ->
      if timerExists graph timerRef
        then []
        else
          [ routeError routeEd "wait.timer.unknown" $
              routeDiagHeader routeEd <> " waits on unknown timer ref " <> timerRefText timerRef
          ]

checkTimerPlanRefs :: MachineGraph -> RouteEd -> [ValidateErr]
checkTimerPlanRefs graph routeEd =
  concatMap checkTimerRef routeEd.plan.timers
  where
    checkTimerRef :: TimerPlanG -> [ValidateErr]
    checkTimerRef timerPlan =
      if timerExists graph timerPlan.ref
        then []
        else
          [ routeError routeEd "wait.timer.schedule.unknown" $
              routeDiagHeader routeEd <> " schedules unknown timer ref " <> timerRefText timerPlan.ref
          ]

checkDuplicateTimerRefs :: RouteEd -> [ValidateErr]
checkDuplicateTimerRefs routeEd =
  fmap mkErr (duplicateTimerRefs (fmap (.ref) routeEd.plan.timers))
  where
    mkErr :: TimerRef -> ValidateErr
    mkErr timerRef =
      routeError routeEd "wait.timer.schedule.duplicate" $
        routeDiagHeader routeEd <> " schedules timer ref " <> timerRefText timerRef <> " more than once"

checkWaitTarget :: RouteEd -> [ValidateErr]
checkWaitTarget routeEd
  | not (hasWait routeEd.plan.wait) = []
  | otherwise =
      case routeEd.plan.target of
        CompleteTg ->
          [ routeError routeEd "wait.target.complete" $
              routeDiagHeader routeEd <> " cannot both wait and complete"
          ]
        FailTg _ ->
          [ routeError routeEd "wait.target.fail" $
              routeDiagHeader routeEd <> " cannot both wait and fail"
          ]
        StayTg -> []
        GotoTg _ -> []

checkWaitJoinConsistency :: RouteEd -> [ValidateErr]
checkWaitJoinConsistency routeEd =
  case (routeEd.plan.wait, joinPlanRef routeEd.plan.join) of
    (WaitJoinWg waitJoinRef, Just planJoinRef)
      | waitJoinRef /= planJoinRef ->
          [ routeError routeEd "wait.join.mismatch" $
              routeDiagHeader routeEd
                <> " waits on join ref " <> joinRefText waitJoinRef
                <> " but its join plan uses join ref " <> joinRefText planJoinRef
          ]
    _ -> []

checkWaitTimerConsistency :: MachineGraph -> RouteEd -> [ValidateErr]
checkWaitTimerConsistency graph routeEd =
  case routeEd.plan.wait of
    WaitTimerWg waitTimerRef
      | not (timerExists graph waitTimerRef) -> []
      | null scheduledRefs -> []
      | waitTimerRef `elem` scheduledRefs -> []
      | otherwise ->
          [ routeWarn routeEd "wait.timer.mismatch" $
              routeDiagHeader routeEd
                <> " waits on timer ref " <> timerRefText waitTimerRef
                <> " but schedules "
                <> renderTimerRefList scheduledRefs
                <> "; if that timer is armed by an earlier step, this warning can be ignored"
          ]
      where
        scheduledRefs = fmap (.ref) routeEd.plan.timers
    _ -> []

hasWait :: WaitPlanG -> Bool
hasWait waitPlan =
  case waitPlan of
    WaitNoneWg -> False
    WaitSignalWg -> True
    WaitJoinWg _ -> True
    WaitTimerWg _ -> True

joinPlanRef :: JoinPlanG -> Maybe JoinRef
joinPlanRef joinPlan =
  case joinPlan of
    JoinNoneJg -> Nothing
    JoinAllJg joinRef -> Just joinRef
    JoinAnyJg joinRef -> Just joinRef
    JoinCountJg joinRef _ -> Just joinRef

joinExists :: MachineGraph -> JoinRef -> Bool
joinExists graph joinRef =
  IM.member (joinRefKey joinRef) graph.joins

timerExists :: MachineGraph -> TimerRef -> Bool
timerExists graph timerRef =
  IM.member (timerRefKey timerRef) graph.timers

duplicateTimerRefs :: [TimerRef] -> [TimerRef]
duplicateTimerRefs refs =
  go S.empty S.empty [] refs
  where
    go :: S.Set TimerRef -> S.Set TimerRef -> [TimerRef] -> [TimerRef] -> [TimerRef]
    go _ _ acc [] = reverse acc
    go seen dup acc (ref : rest)
      | ref `S.member` seen && not (ref `S.member` dup) =
          go seen (S.insert ref dup) (ref : acc) rest
      | ref `S.member` seen =
          go seen dup acc rest
      | otherwise =
          go (S.insert ref seen) dup acc rest

routeError :: RouteEd -> Text -> Text -> ValidateErr
routeError routeEd code msg =
  atRoute routeEd.ref (errorErr code msg Nothing)

routeWarn :: RouteEd -> Text -> Text -> ValidateErr
routeWarn routeEd code msg =
  atRoute routeEd.ref (warnErr code msg Nothing)

routeDiagHeader :: RouteEd -> Text
routeDiagHeader routeEd =
  "route " <> routeRefText routeEd.ref <> " on handoff " <> quote routeEd.handoff

routeRefText :: RouteRef -> Text
routeRefText = tshow . routeRefWord32

joinRefText :: JoinRef -> Text
joinRefText = tshow . joinRefWord32

timerRefText :: TimerRef -> Text
timerRefText = tshow . timerRefWord32

renderTimerRefList :: [TimerRef] -> Text
renderTimerRefList refs =
  "[" <> T.intercalate ", " (fmap timerRefText refs) <> "]"

joinRefKey :: JoinRef -> Int
joinRefKey = fromIntegral . joinRefWord32

timerRefKey :: TimerRef -> Int
timerRefKey = fromIntegral . timerRefWord32

quote :: Text -> Text
quote txt = "'" <> txt <> "'"

tshow :: Show a => a -> Text
tshow = T.pack . show