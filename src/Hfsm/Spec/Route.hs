{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}

module Hfsm.Spec.Route
  ( TargetPlan(..)
  , WaitPlan(..)
  , SpawnPlan(..)
  , JoinPlan(..)
  , TimerPlan(..)
  , BreakPlan(..)
  , ControlPlan(..)
  , RouteSpec(..)
  , RouteErr(..)
  , emptyControlPlan
  , stayPlan
  , gotoPlan
  , completePlan
  , failPlan
  , mkSpawnPlan
  , setSpawnKey
  , mkTimerPlan
  , setTimerPayload
  , noWait
  , waitSignal
  , waitJoin
  , waitTimer
  , clearSpawn
  , addSpawn
  , spawnChild
  , spawnChildWithKey
  , noJoin
  , joinAll
  , joinAny
  , joinCount
  , clearTimers
  , scheduleTimer
  , clearBreaks
  , addBreak
  , breakBeforeRoute
  , breakAfterRoute
  , breakBeforeCommit
  , mkRouteSpec
  , normalizeBreaks
  , normalizeControlPlan
  , mapTargetPlan
  , mapSpawnPlan
  , mapControlPlan
  , mapRouteSpec
  , traverseTargetPlan
  , traverseSpawnPlan
  , traverseControlPlan
  , traverseRouteSpec
  , validateControlPlan
  , validateRouteSpec
  , renderRouteErr
  ) where

import Control.DeepSeq (NFData)

import Data.List (foldl')
import Data.Set (Set)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (NominalDiffTime)

import GHC.Generics (Generic)

import Data.Aeson (Value)

import Hfsm.Core.Meta (MetaSpec, emptyMetaSpec)
import Hfsm.Core.Name (JoinName, TimerName, joinNameText, timerNameText)

data TargetPlan st
  = StayTg
  | GotoTg st
  | CompleteTg
  | FailTg Text
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data WaitPlan
  = WaitNoneWp
  | WaitSignalWp
  | WaitJoinWp JoinName
  | WaitTimerWp TimerName
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data SpawnPlan child = SpawnPlan
  { child :: child
  , input :: Value
  , key :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data JoinPlan
  = JoinNoneJp
  | JoinAllJp JoinName
  | JoinAnyJp JoinName
  | JoinCountJp JoinName Int
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data TimerPlan = TimerPlan
  { key :: TimerName
  , delay :: NominalDiffTime
  , payload :: Maybe Value
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data BreakPlan
  = BreakNoneBp
  | BreakBeforeRouteBp
  | BreakAfterRouteBp
  | BreakBeforeCommitBp
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData)

data ControlPlan st child = ControlPlan
  { target :: TargetPlan st
  , wait :: WaitPlan
  , spawn :: [SpawnPlan child]
  , join :: JoinPlan
  , timers :: [TimerPlan]
  , break :: [BreakPlan]
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data RouteSpec st hf child = RouteSpec
  { on :: hf
  , plan :: ControlPlan st child
  , meta :: MetaSpec
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

data RouteErr
  = NonPositiveJoinCountEr JoinName Int
  | NegativeTimerDelayEr TimerName NominalDiffTime
  | DuplicateTimerEr TimerName
  | TerminalTargetWaitEr
  | TerminalTargetSpawnEr Int
  | TerminalTargetJoinEr
  | TerminalTargetTimerEr Int
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData)

emptyControlPlan :: ControlPlan st child
emptyControlPlan =
  ControlPlan
    { target = StayTg
    , wait = WaitNoneWp
    , spawn = []
    , join = JoinNoneJp
    , timers = []
    , break = []
    }

stayPlan :: ControlPlan st child
stayPlan = emptyControlPlan

gotoPlan :: st -> ControlPlan st child
gotoPlan st =
  emptyControlPlan { target = GotoTg st }

completePlan :: ControlPlan st child
completePlan =
  emptyControlPlan { target = CompleteTg }

failPlan :: Text -> ControlPlan st child
failPlan msg =
  emptyControlPlan { target = FailTg msg }

mkSpawnPlan :: child -> Value -> SpawnPlan child
mkSpawnPlan child input =
  SpawnPlan
    { child = child
    , input = input
    , key = Nothing
    }

setSpawnKey :: Maybe Text -> SpawnPlan child -> SpawnPlan child
setSpawnKey rawKey spawnPl =
  spawnPl { key = normalizeMaybeText rawKey }

mkTimerPlan :: TimerName -> NominalDiffTime -> TimerPlan
mkTimerPlan key delay =
  TimerPlan
    { key = key
    , delay = delay
    , payload = Nothing
    }

setTimerPayload :: Maybe Value -> TimerPlan -> TimerPlan
setTimerPayload payload timerPl =
  timerPl { payload = payload }

noWait :: ControlPlan st child -> ControlPlan st child
noWait controlPl =
  controlPl { wait = WaitNoneWp }

waitSignal :: ControlPlan st child -> ControlPlan st child
waitSignal controlPl =
  controlPl { wait = WaitSignalWp }

waitJoin :: JoinName -> ControlPlan st child -> ControlPlan st child
waitJoin joinNm controlPl =
  controlPl { wait = WaitJoinWp joinNm }

waitTimer :: TimerName -> ControlPlan st child -> ControlPlan st child
waitTimer timerNm controlPl =
  controlPl { wait = WaitTimerWp timerNm }

clearSpawn :: ControlPlan st child -> ControlPlan st child
clearSpawn controlPl =
  controlPl { spawn = [] }

addSpawn :: SpawnPlan child -> ControlPlan st child -> ControlPlan st child
addSpawn spawnPl controlPl =
  controlPl { spawn = controlPl.spawn <> [normalizeSpawnPlan spawnPl] }

spawnChild :: child -> Value -> ControlPlan st child -> ControlPlan st child
spawnChild child input =
  addSpawn (mkSpawnPlan child input)

spawnChildWithKey :: child -> Value -> Text -> ControlPlan st child -> ControlPlan st child
spawnChildWithKey child input rawKey =
  addSpawn (setSpawnKey (Just rawKey) (mkSpawnPlan child input))

noJoin :: ControlPlan st child -> ControlPlan st child
noJoin controlPl =
  controlPl { join = JoinNoneJp }

joinAll :: JoinName -> ControlPlan st child -> ControlPlan st child
joinAll joinNm controlPl =
  controlPl { join = JoinAllJp joinNm }

joinAny :: JoinName -> ControlPlan st child -> ControlPlan st child
joinAny joinNm controlPl =
  controlPl { join = JoinAnyJp joinNm }

joinCount :: JoinName -> Int -> ControlPlan st child -> ControlPlan st child
joinCount joinNm count controlPl =
  controlPl { join = JoinCountJp joinNm count }

clearTimers :: ControlPlan st child -> ControlPlan st child
clearTimers controlPl =
  controlPl { timers = [] }

scheduleTimer :: TimerPlan -> ControlPlan st child -> ControlPlan st child
scheduleTimer timerPl controlPl =
  controlPl { timers = controlPl.timers <> [timerPl] }

clearBreaks :: ControlPlan st child -> ControlPlan st child
clearBreaks controlPl =
  controlPl { break = [] }

addBreak :: BreakPlan -> ControlPlan st child -> ControlPlan st child
addBreak breakPl controlPl =
  controlPl { break = normalizeBreaks (controlPl.break <> [breakPl]) }

breakBeforeRoute :: ControlPlan st child -> ControlPlan st child
breakBeforeRoute =
  addBreak BreakBeforeRouteBp

breakAfterRoute :: ControlPlan st child -> ControlPlan st child
breakAfterRoute =
  addBreak BreakAfterRouteBp

breakBeforeCommit :: ControlPlan st child -> ControlPlan st child
breakBeforeCommit =
  addBreak BreakBeforeCommitBp

mkRouteSpec :: hf -> ControlPlan st child -> RouteSpec st hf child
mkRouteSpec handoff controlPl =
  RouteSpec
    { on = handoff
    , plan = normalizeControlPlan controlPl
    , meta = emptyMetaSpec
    }

normalizeBreaks :: [BreakPlan] -> [BreakPlan]
normalizeBreaks = go S.empty []
  where
    go :: Set BreakPlan -> [BreakPlan] -> [BreakPlan] -> [BreakPlan]
    go seen acc breakPls =
      case breakPls of
        [] -> reverse acc
        BreakNoneBp : rest -> go seen acc rest
        breakPl : rest
          | S.member breakPl seen -> go seen acc rest
          | otherwise -> go (S.insert breakPl seen) (breakPl : acc) rest

normalizeControlPlan :: ControlPlan st child -> ControlPlan st child
normalizeControlPlan controlPl =
  controlPl
    { spawn = fmap normalizeSpawnPlan controlPl.spawn
    , break = normalizeBreaks controlPl.break
    }

mapTargetPlan :: (a -> b) -> TargetPlan a -> TargetPlan b
mapTargetPlan f targetPl =
  case targetPl of
    StayTg -> StayTg
    GotoTg st -> GotoTg (f st)
    CompleteTg -> CompleteTg
    FailTg msg -> FailTg msg

mapSpawnPlan :: (a -> b) -> SpawnPlan a -> SpawnPlan b
mapSpawnPlan f spawnPl =
  spawnPl { child = f spawnPl.child }

mapControlPlan :: (a -> b) -> (c -> d) -> ControlPlan a c -> ControlPlan b d
mapControlPlan mapState mapChild controlPl =
  controlPl
    { target = mapTargetPlan mapState controlPl.target
    , spawn = fmap (mapSpawnPlan mapChild) controlPl.spawn
    }

mapRouteSpec :: (a -> b) -> (c -> d) -> RouteSpec a hf c -> RouteSpec b hf d
mapRouteSpec mapState mapChild routeSp =
  routeSp { plan = mapControlPlan mapState mapChild routeSp.plan }

traverseTargetPlan :: Applicative f => (a -> f b) -> TargetPlan a -> f (TargetPlan b)
traverseTargetPlan f targetPl =
  case targetPl of
    StayTg -> pure StayTg
    GotoTg st -> GotoTg <$> f st
    CompleteTg -> pure CompleteTg
    FailTg msg -> pure (FailTg msg)

traverseSpawnPlan :: Applicative f => (a -> f b) -> SpawnPlan a -> f (SpawnPlan b)
traverseSpawnPlan f spawnPl =
  (\child -> spawnPl { child = child }) <$> f spawnPl.child

traverseControlPlan :: Applicative f => (a -> f b) -> (c -> f d) -> ControlPlan a c -> f (ControlPlan b d)
traverseControlPlan mapState mapChild controlPl =
  (\target spawn -> controlPl { target = target, spawn = spawn })
    <$> traverseTargetPlan mapState controlPl.target
    <*> traverse (traverseSpawnPlan mapChild) controlPl.spawn

traverseRouteSpec :: Applicative f => (a -> f b) -> (c -> f d) -> RouteSpec a hf c -> f (RouteSpec b hf d)
traverseRouteSpec mapState mapChild routeSp =
  (\plan -> routeSp { plan = plan }) <$> traverseControlPlan mapState mapChild routeSp.plan

validateControlPlan :: ControlPlan st child -> [RouteErr]
validateControlPlan controlPl =
  validateJoinPlan controlPl.join
    <> validateTimerPlans controlPl.timers
    <> validateTerminalTarget controlPl

validateRouteSpec :: RouteSpec st hf child -> [RouteErr]
validateRouteSpec routeSp =
  validateControlPlan routeSp.plan

renderRouteErr :: RouteErr -> Text
renderRouteErr err =
  case err of
    NonPositiveJoinCountEr joinNm count ->
      "join count must be positive for join " <> quote (joinNameText joinNm) <> "; actual=" <> tshow count

    NegativeTimerDelayEr timerNm delay ->
      "timer delay must be non-negative for timer " <> quote (timerNameText timerNm) <> "; actual=" <> tshow delay

    DuplicateTimerEr timerNm ->
      "duplicate timer key in control plan: " <> quote (timerNameText timerNm)

    TerminalTargetWaitEr ->
      "a completion or failure route cannot declare a wait policy"

    TerminalTargetSpawnEr count ->
      "a completion or failure route cannot spawn child machines; count=" <> tshow count

    TerminalTargetJoinEr ->
      "a completion or failure route cannot declare a join policy"

    TerminalTargetTimerEr count ->
      "a completion or failure route cannot schedule timers; count=" <> tshow count

normalizeSpawnPlan :: SpawnPlan child -> SpawnPlan child
normalizeSpawnPlan spawnPl =
  spawnPl { key = normalizeMaybeText spawnPl.key }

normalizeMaybeText :: Maybe Text -> Maybe Text
normalizeMaybeText raw =
  case raw of
    Nothing -> Nothing
    Just txt ->
      let txt' = T.strip txt
      in if T.null txt' then Nothing else Just txt'

validateJoinPlan :: JoinPlan -> [RouteErr]
validateJoinPlan joinPl =
  case joinPl of
    JoinNoneJp -> []
    JoinAllJp _ -> []
    JoinAnyJp _ -> []
    JoinCountJp joinNm count
      | count <= 0 -> [NonPositiveJoinCountEr joinNm count]
      | otherwise -> []

validateTimerPlans :: [TimerPlan] -> [RouteErr]
validateTimerPlans timerPls =
  negativeDelayErrs timerPls <> duplicateTimerErrs timerPls

negativeDelayErrs :: [TimerPlan] -> [RouteErr]
negativeDelayErrs timerPls =
  foldr step [] timerPls
  where
    step :: TimerPlan -> [RouteErr] -> [RouteErr]
    step timerPl acc
      | timerPl.delay < 0 = NegativeTimerDelayEr timerPl.key timerPl.delay : acc
      | otherwise = acc

duplicateTimerErrs :: [TimerPlan] -> [RouteErr]
duplicateTimerErrs timerPls =
  fmap DuplicateTimerEr (duplicateTimerKeys timerPls)

duplicateTimerKeys :: [TimerPlan] -> [TimerName]
duplicateTimerKeys timerPls =
  S.toList dups
  where
    (_, dups) = foldl' step (S.empty, S.empty) timerPls

    step :: (Set TimerName, Set TimerName) -> TimerPlan -> (Set TimerName, Set TimerName)
    step (seen, dups) timerPl
      | S.member timerPl.key seen = (seen, S.insert timerPl.key dups)
      | otherwise = (S.insert timerPl.key seen, dups)

validateTerminalTarget :: ControlPlan st child -> [RouteErr]
validateTerminalTarget controlPl
  | not (isTerminalTarget controlPl.target) = []
  | otherwise =
      waitErr <> spawnErr <> joinErr <> timerErr
  where
    waitErr =
      case controlPl.wait of
        WaitNoneWp -> []
        _ -> [TerminalTargetWaitEr]

    spawnErr =
      if null controlPl.spawn
        then []
        else [TerminalTargetSpawnEr (length controlPl.spawn)]

    joinErr =
      case controlPl.join of
        JoinNoneJp -> []
        _ -> [TerminalTargetJoinEr]

    timerErr =
      if null controlPl.timers
        then []
        else [TerminalTargetTimerEr (length controlPl.timers)]

isTerminalTarget :: TargetPlan st -> Bool
isTerminalTarget targetPl =
  case targetPl of
    CompleteTg -> True
    FailTg _ -> True
    StayTg -> False
    GotoTg _ -> False

quote :: Text -> Text
quote txt = "'" <> txt <> "'"

tshow :: Show a => a -> Text
tshow = T.pack . show