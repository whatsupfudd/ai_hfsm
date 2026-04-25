{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE StrictData #-}

module Hfsm.Proj.Trace
  ( TraceProj(..)
  , mkTraceProj
  , traceProjFromStep
  , traceProjFromSteps
  , traceHasSignal
  , traceChangesState
  , traceIsSelfLoop
  , traceTouchesState
  , traceUsesRoute
  , traceForInst
  , traceForSignal
  , traceForState
  , traceForRoute
  , sortTraceProjAsc
  , sortTraceProjDesc
  ) where

import Control.DeepSeq (NFData)

import Data.List (sortOn)
import Data.Ord (Down(..))
import Data.Time (UTCTime)
import Data.UUID (UUID)

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON, Value)

import Hfsm.Core.Ref (RouteRef, StateRef)
import Hfsm.Runtime.Model.Step (StepRw(..))


data TraceProj = TraceProj
  { inst :: UUID
  , step :: UUID
  , signal :: Maybe UUID
  , from :: StateRef
  , to :: StateRef
  , route :: RouteRef
  , note :: Value
  , createdAt :: UTCTime
  }
  deriving stock (Eq, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

mkTraceProj :: UUID -> UUID -> Maybe UUID -> StateRef -> StateRef -> RouteRef -> Value -> UTCTime -> TraceProj
mkTraceProj instUuid stepUuid signalUuid fromRef toRef routeRef noteValue createdAt' =
  TraceProj
    { inst = instUuid
    , step = stepUuid
    , signal = signalUuid
    , from = fromRef
    , to = toRef
    , route = routeRef
    , note = noteValue
    , createdAt = createdAt'
    }

traceProjFromStep :: StepRw -> TraceProj
traceProjFromStep stepRw =
  TraceProj
    { inst = stepRw.inst
    , step = stepRw.uuid
    , signal = stepRw.signal
    , from = stepRw.from
    , to = stepRw.to
    , route = stepRw.route
    , note = stepRw.note
    , createdAt = stepRw.createdAt
    }

traceProjFromSteps :: [StepRw] -> [TraceProj]
traceProjFromSteps = fmap traceProjFromStep

traceHasSignal :: TraceProj -> Bool
traceHasSignal traceProj =
  case traceProj.signal of
    Nothing -> False
    Just _ -> True

traceChangesState :: TraceProj -> Bool
traceChangesState traceProj =
  traceProj.from /= traceProj.to

traceIsSelfLoop :: TraceProj -> Bool
traceIsSelfLoop = not . traceChangesState

traceTouchesState :: StateRef -> TraceProj -> Bool
traceTouchesState stateRef traceProj =
  traceProj.from == stateRef || traceProj.to == stateRef

traceUsesRoute :: RouteRef -> TraceProj -> Bool
traceUsesRoute routeRef traceProj =
  traceProj.route == routeRef

traceForInst :: UUID -> [TraceProj] -> [TraceProj]
traceForInst instUuid =
  filter ( \traceProj -> traceProj.inst == instUuid)

traceForSignal :: UUID -> [TraceProj] -> [TraceProj]
traceForSignal signalUuid =
  filter ( \traceProj -> traceProj.signal == Just signalUuid)

traceForState :: StateRef -> [TraceProj] -> [TraceProj]
traceForState stateRef =
  filter (traceTouchesState stateRef)

traceForRoute :: RouteRef -> [TraceProj] -> [TraceProj]
traceForRoute routeRef =
  filter (traceUsesRoute routeRef)

sortTraceProjAsc :: [TraceProj] -> [TraceProj]
sortTraceProjAsc =
  sortOn ( \traceProj -> (traceProj.createdAt, traceProj.inst, traceProj.step))

sortTraceProjDesc :: [TraceProj] -> [TraceProj]
sortTraceProjDesc =
  sortOn ( \traceProj -> (Down traceProj.createdAt, Down traceProj.inst, Down traceProj.step))