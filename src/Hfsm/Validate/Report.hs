{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hfsm.Validate.Report
  ( ValidateStats(..)
  , ValidateReport(..)
  , validate
  , validatePair
  , ok
  , errors
  , warnings
  ) where

import Control.DeepSeq (NFData)
import Data.Aeson (FromJSON, ToJSON)
import qualified Data.IntMap.Strict as IM
import GHC.Generics (Generic)

import Hfsm.Graph.Def (MachineGraph(..))
import Hfsm.Validate.Error (Severity(..), ValidateErr(..))
import qualified Hfsm.Validate.History as History
import qualified Hfsm.Validate.Join as Join
import qualified Hfsm.Validate.Reach as Reach
import qualified Hfsm.Validate.Route as Route
import qualified Hfsm.Validate.Struct as Struct
import qualified Hfsm.Validate.Version as Version
import qualified Hfsm.Validate.Wait as Wait

data ValidateStats = ValidateStats
  { regionCount :: Int
  , stateCount :: Int
  , caseCount :: Int
  , routeCount :: Int
  , joinCount :: Int
  , timerCount :: Int
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

data ValidateReport = ValidateReport
  { errs :: [ValidateErr]
  , stats :: ValidateStats
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

validate :: MachineGraph -> ValidateReport
validate graph =
  ValidateReport
    { errs = singleGraphErrs graph
    , stats = statsFromGraph graph
    }

validatePair :: MachineGraph -> MachineGraph -> ValidateReport
validatePair fromGraph toGraph =
  ValidateReport
    { errs = singleGraphErrs toGraph <> Version.checkVersion fromGraph toGraph
    , stats = statsFromGraph toGraph
    }

ok :: ValidateReport -> Bool
ok report =
  not (any isErrorSeverity report.errs)

errors :: ValidateReport -> [ValidateErr]
errors report =
  filter (\err -> err.sev == ErrorSv) report.errs

warnings :: ValidateReport -> [ValidateErr]
warnings report =
  filter (\err -> err.sev == WarnSv) report.errs

singleGraphErrs :: MachineGraph -> [ValidateErr]
singleGraphErrs graph =
  Struct.checkStruct graph
    <> Route.checkRoute graph
    <> Reach.checkReach graph
    <> Wait.checkWait graph
    <> Join.checkJoin graph
    <> History.checkHistory graph

statsFromGraph :: MachineGraph -> ValidateStats
statsFromGraph graph =
  ValidateStats
    { regionCount = IM.size graph.regions
    , stateCount = IM.size graph.states
    , caseCount = IM.size graph.cases
    , routeCount = IM.size graph.routes
    , joinCount = IM.size graph.joins
    , timerCount = IM.size graph.timers
    }

isErrorSeverity :: ValidateErr -> Bool
isErrorSeverity err =
  err.sev == ErrorSv