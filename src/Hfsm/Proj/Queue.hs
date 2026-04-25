{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module Hfsm.Proj.Queue
  ( QueueKind(..)
  , WaitKind(..)
  , QueueProj(..)
  , renderQueueKind
  , renderWaitKind
  , isSignalQueueKind
  , isOutboxQueueKind
  , isBreakQueueKind
  , isWaitQueueKind
  , isSignalQueueProj
  , isOutboxQueueProj
  , isBreakQueueProj
  , isWaitQueueProj
  , waitKindFromValue
  , signalQueueProj
  , outboxQueueProj
  , pausedBreakQueueProj
  , waitQueueProj
  , signalQueueProjs
  , outboxQueueProjs
  , pausedBreakQueueProjs
  , waitingQueueProjs
  , queueProjs
  , sortQueueProjs
  ) where

import Control.Applicative ((<|>))
import Control.DeepSeq (NFData)

import Data.Aeson (FromJSON, ToJSON, Value(..))
import qualified Data.Aeson.Key as K
import qualified Data.Aeson.KeyMap as KM
import Data.List (sortBy)
import Data.Map.Strict (Map)
import qualified Data.Map.Strict as M
import Data.Maybe (mapMaybe)
import Data.Ord (comparing)
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)

import GHC.Generics (Generic)

import Hfsm.Core.Path (StatePath)
import Hfsm.Core.Ref (StateRef)
import Hfsm.Runtime.Model.Break (BreakRw(..), breakIsHit, renderBreakStatus)
import Hfsm.Runtime.Model.Instance (InstanceRw(..), InstanceStatus(..), renderInstanceStatus)
import Hfsm.Runtime.Model.Outbox (OutboxRw(..), OutboxStatus(..), renderOutboxStatus)
import Hfsm.Runtime.Model.Signal (SignalRw(..), SignalStatus(..), renderSignalStatus)
import Hfsm.Runtime.Model.Snapshot (SnapshotRw(..))


data QueueKind
  = PendingSignalQk
  | LeasedSignalQk
  | ReadyOutboxQk
  | LeasedOutboxQk
  | PausedBreakQk
  | WaitSignalQk
  | WaitJoinQk
  | WaitTimerQk
  | WaitOtherQk
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data WaitKind
  = SignalWk
  | JoinWk
  | TimerWk
  | OtherWk
  deriving stock (Eq, Ord, Show, Read, Enum, Bounded, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data QueueProj = QueueProj
  { kind :: QueueKind
  , item :: UUID
  , inst :: Maybe UUID
  , state :: Maybe StateRef
  , path :: Maybe StatePath
  , status :: Text
  , lease :: Maybe UUID
  , wait :: Maybe Value
  , payload :: Maybe Value
  , cause :: Maybe Value
  , note :: Maybe Text
  , createdAt :: UTCTime
  , updatedAt :: Maybe UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

renderQueueKind :: QueueKind -> Text
renderQueueKind kind =
  case kind of
    PendingSignalQk -> "pending-signal"
    LeasedSignalQk -> "leased-signal"
    ReadyOutboxQk -> "ready-outbox"
    LeasedOutboxQk -> "leased-outbox"
    PausedBreakQk -> "paused-break"
    WaitSignalQk -> "wait-signal"
    WaitJoinQk -> "wait-join"
    WaitTimerQk -> "wait-timer"
    WaitOtherQk -> "wait-other"

renderWaitKind :: WaitKind -> Text
renderWaitKind kind =
  case kind of
    SignalWk -> "signal"
    JoinWk -> "join"
    TimerWk -> "timer"
    OtherWk -> "other"

isSignalQueueKind :: QueueKind -> Bool
isSignalQueueKind kind =
  case kind of
    PendingSignalQk -> True
    LeasedSignalQk -> True
    ReadyOutboxQk -> False
    LeasedOutboxQk -> False
    PausedBreakQk -> False
    WaitSignalQk -> False
    WaitJoinQk -> False
    WaitTimerQk -> False
    WaitOtherQk -> False

isOutboxQueueKind :: QueueKind -> Bool
isOutboxQueueKind kind =
  case kind of
    PendingSignalQk -> False
    LeasedSignalQk -> False
    ReadyOutboxQk -> True
    LeasedOutboxQk -> True
    PausedBreakQk -> False
    WaitSignalQk -> False
    WaitJoinQk -> False
    WaitTimerQk -> False
    WaitOtherQk -> False

isBreakQueueKind :: QueueKind -> Bool
isBreakQueueKind kind =
  case kind of
    PausedBreakQk -> True
    PendingSignalQk -> False
    LeasedSignalQk -> False
    ReadyOutboxQk -> False
    LeasedOutboxQk -> False
    WaitSignalQk -> False
    WaitJoinQk -> False
    WaitTimerQk -> False
    WaitOtherQk -> False

isWaitQueueKind :: QueueKind -> Bool
isWaitQueueKind kind =
  case kind of
    WaitSignalQk -> True
    WaitJoinQk -> True
    WaitTimerQk -> True
    WaitOtherQk -> True
    PendingSignalQk -> False
    LeasedSignalQk -> False
    ReadyOutboxQk -> False
    LeasedOutboxQk -> False
    PausedBreakQk -> False

isSignalQueueProj :: QueueProj -> Bool
isSignalQueueProj proj = isSignalQueueKind proj.kind

isOutboxQueueProj :: QueueProj -> Bool
isOutboxQueueProj proj = isOutboxQueueKind proj.kind

isBreakQueueProj :: QueueProj -> Bool
isBreakQueueProj proj = isBreakQueueKind proj.kind

isWaitQueueProj :: QueueProj -> Bool
isWaitQueueProj proj = isWaitQueueKind proj.kind

waitKindFromValue :: Value -> WaitKind
waitKindFromValue value =
  case value of
    String txt -> waitKindFromText txt
    Object obj ->
      case lookupObjectTag obj of
        Just txt ->
          let kind = waitKindFromText txt
          in if kind == OtherWk then inferWaitKindFromKeys obj else kind
        Nothing ->
          inferWaitKindFromKeys obj
    _ -> OtherWk

signalQueueProj :: Maybe InstanceRw -> SignalRw -> Maybe QueueProj
signalQueueProj instRw signalRw = do
  kind <- queueKindFromSignalStatus signalRw.status
  pure QueueProj
    { kind = kind
    , item = signalRw.uuid
    , inst = Just signalRw.inst
    , state = fmap (.state) instRw
    , path = fmap (.path) instRw
    , status = renderSignalStatus signalRw.status
    , lease = signalRw.lease
    , wait = Nothing
    , payload = Just signalRw.payload
    , cause = signalRw.cause
    , note = Nothing
    , createdAt = signalRw.createdAt
    , updatedAt = Just signalRw.updatedAt
    }

outboxQueueProj :: Maybe InstanceRw -> OutboxRw -> Maybe QueueProj
outboxQueueProj instRw outboxRw = do
  kind <- queueKindFromOutboxStatus outboxRw.status
  pure QueueProj
    { kind = kind
    , item = outboxRw.uuid
    , inst = Just outboxRw.inst
    , state = fmap (.state) instRw
    , path = fmap (.path) instRw
    , status = renderOutboxStatus outboxRw.status
    , lease = outboxRw.lease
    , wait = Nothing
    , payload = Just outboxRw.payload
    , cause = Nothing
    , note = Nothing
    , createdAt = outboxRw.createdAt
    , updatedAt = Just outboxRw.updatedAt
    }

pausedBreakQueueProj :: Maybe InstanceRw -> BreakRw -> Maybe QueueProj
pausedBreakQueueProj instRw breakRw
  | not (breakIsHit breakRw) = Nothing
  | otherwise =
      Just QueueProj
        { kind = PausedBreakQk
        , item = breakRw.uuid
        , inst = breakRw.inst
        , state = breakRw.state <|> fmap (.state) instRw
        , path = fmap (.path) instRw
        , status = renderBreakStatus breakRw.status
        , lease = Nothing
        , wait = Nothing
        , payload = Nothing
        , cause = Nothing
        , note = breakRw.note
        , createdAt = breakRw.createdAt
        , updatedAt = Just breakRw.updatedAt
        }

waitQueueProj :: InstanceRw -> Maybe SnapshotRw -> Maybe QueueProj
waitQueueProj instRw snapRw
  | instRw.status /= WaitingIs = Nothing
  | otherwise =
      let waitPayload = snapRw >>= \row -> row.wait
      in Just QueueProj
           { kind = queueKindFromWaitPayload waitPayload
           , item = instRw.uuid
           , inst = Just instRw.uuid
           , state = (snapRw >>= \row -> Just row.state) <|> Just instRw.state
           , path = (snapRw >>= \row -> Just row.path) <|> Just instRw.path
           , status = renderInstanceStatus instRw.status
           , lease = Nothing
           , wait = waitPayload
           , payload = Nothing
           , cause = Nothing
           , note = Nothing
           , createdAt = maybe instRw.updatedAt (.createdAt) snapRw
           , updatedAt = Just instRw.updatedAt
           }

signalQueueProjs :: [InstanceRw] -> [SignalRw] -> [QueueProj]
signalQueueProjs instRws signalRws =
  let instByUuid = indexInst instRws
  in mapMaybe (\signalRw -> signalQueueProj (M.lookup signalRw.inst instByUuid) signalRw) signalRws

outboxQueueProjs :: [InstanceRw] -> [OutboxRw] -> [QueueProj]
outboxQueueProjs instRws outboxRws =
  let instByUuid = indexInst instRws
  in mapMaybe (\outboxRw -> outboxQueueProj (M.lookup outboxRw.inst instByUuid) outboxRw) outboxRws

pausedBreakQueueProjs :: [InstanceRw] -> [BreakRw] -> [QueueProj]
pausedBreakQueueProjs instRws breakRws =
  let instByUuid = indexInst instRws
  in mapMaybe (\breakRw -> pausedBreakQueueProj (breakRw.inst >>= (`M.lookup` instByUuid)) breakRw) breakRws

waitingQueueProjs :: [InstanceRw] -> [SnapshotRw] -> [QueueProj]
waitingQueueProjs instRws snapRws =
  let snapByUuid = indexSnap snapRws
  in mapMaybe (\instRw -> waitQueueProj instRw (M.lookup instRw.snap snapByUuid)) instRws

queueProjs :: [InstanceRw] -> [SnapshotRw] -> [SignalRw] -> [OutboxRw] -> [BreakRw] -> [QueueProj]
queueProjs instRws snapRws signalRws outboxRws breakRws =
  sortQueueProjs $
    signalQueueProjs instRws signalRws
      <> outboxQueueProjs instRws outboxRws
      <> pausedBreakQueueProjs instRws breakRws
      <> waitingQueueProjs instRws snapRws

sortQueueProjs :: [QueueProj] -> [QueueProj]
sortQueueProjs =
  sortBy (comparing queueSortKey)

queueSortKey :: QueueProj -> (UTCTime, QueueKind, UUID)
queueSortKey proj = (proj.createdAt, proj.kind, proj.item)

queueKindFromSignalStatus :: SignalStatus -> Maybe QueueKind
queueKindFromSignalStatus status =
  case status of
    EnteredSs -> Just PendingSignalQk
    LeasedSs -> Just LeasedSignalQk
    AppliedSs -> Nothing
    RejectedSs -> Nothing
    FailedSs -> Nothing

queueKindFromOutboxStatus :: OutboxStatus -> Maybe QueueKind
queueKindFromOutboxStatus status =
  case status of
    ReadyOs -> Just ReadyOutboxQk
    LeasedOs -> Just LeasedOutboxQk
    DoneOs -> Nothing
    FailedOs -> Nothing

queueKindFromWaitPayload :: Maybe Value -> QueueKind
queueKindFromWaitPayload waitPayload =
  case waitPayload of
    Nothing -> WaitOtherQk
    Just value ->
      case waitKindFromValue value of
        SignalWk -> WaitSignalQk
        JoinWk -> WaitJoinQk
        TimerWk -> WaitTimerQk
        OtherWk -> WaitOtherQk

indexInst :: [InstanceRw] -> Map UUID InstanceRw
indexInst =
  M.fromList . fmap ( \row -> (row.uuid, row))

indexSnap :: [SnapshotRw] -> Map UUID SnapshotRw
indexSnap =
  M.fromList . fmap ( \row -> (row.uuid, row))

lookupObjectTag :: KM.KeyMap Value -> Maybe Text
lookupObjectTag obj =
  firstJust
    [ lookupObjectText "kind" obj
    , lookupObjectText "wait" obj
    , lookupObjectText "type" obj
    , lookupObjectText "tag" obj
    , lookupObjectText "mode" obj
    ]

lookupObjectText :: Text -> KM.KeyMap Value -> Maybe Text
lookupObjectText key obj =
  case KM.lookup (K.fromText key) obj of
    Just (String txt) -> Just txt
    _ -> Nothing

inferWaitKindFromKeys :: KM.KeyMap Value -> WaitKind
inferWaitKindFromKeys obj
  | hasAnyKey ["join", "joinRef", "joinKey", "joinName"] obj = JoinWk
  | hasAnyKey ["timer", "timerRef", "timerKey", "timerName", "deadline", "resumeAt", "wakeAt"] obj = TimerWk
  | hasAnyKey ["signal", "signalKey", "signalName", "event", "eventType", "input"] obj = SignalWk
  | otherwise = OtherWk

hasAnyKey :: [Text] -> KM.KeyMap Value -> Bool
hasAnyKey keys obj =
  any (\key -> KM.member (K.fromText key) obj) keys

waitKindFromText :: Text -> WaitKind
waitKindFromText raw =
  case normalizeAtom raw of
    "signal" -> SignalWk
    "wait-signal" -> SignalWk
    "await-signal" -> SignalWk
    "input" -> SignalWk
    "event" -> SignalWk
    "join" -> JoinWk
    "wait-join" -> JoinWk
    "await-join" -> JoinWk
    "all-join" -> JoinWk
    "any-join" -> JoinWk
    "count-join" -> JoinWk
    "timer" -> TimerWk
    "wait-timer" -> TimerWk
    "await-timer" -> TimerWk
    "delay" -> TimerWk
    "sleep" -> TimerWk
    "deadline" -> TimerWk
    _ -> OtherWk

normalizeAtom :: Text -> Text
normalizeAtom =
  T.map replaceSep . T.toLower . T.strip
  where
    replaceSep ch
      | ch == '_' = '-'
      | ch == ' ' = '-'
      | otherwise = ch

firstJust :: [Maybe a] -> Maybe a
firstJust maybes =
  case maybes of
    [] -> Nothing
    Nothing : rest -> firstJust rest
    Just x : _ -> Just x