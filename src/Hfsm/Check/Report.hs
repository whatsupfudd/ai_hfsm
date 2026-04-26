{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hfsm.Check.Report
  ( CheckRez(..)
  , renderReport
  ) where

import Data.Text (Text)
import qualified Data.Text as T
import GHC.Generics (Generic)

import Hfsm.Core.Digest (SpecDigest, specDigestText)
import Hfsm.Core.Name (machineNameText)
import Hfsm.Core.Ref
  ( CaseRef
  , JoinRef
  , RegionRef
  , RouteRef
  , StateRef
  , caseRefWord32
  , joinRefWord32
  , regionRefWord32
  , routeRefWord32
  , stateRefWord32
  )
import Hfsm.Core.Version (machineVersionText)
import Hfsm.Graph.Def (MachineGraph(..))
import Hfsm.Validate.Error (Severity(..), ValidateErr(..), ValidateLoc(..))
import Hfsm.Validate.Report (ValidateReport(..), ValidateStats(..), ok)

data CheckRez = CheckRez
  { compiled :: Maybe SpecDigest
  , validate :: ValidateReport
  , graph :: Maybe MachineGraph
  }
  deriving stock (Eq, Ord, Show, Read, Generic)

renderReport :: CheckRez -> Text
renderReport rez =
  T.unlines $
    [ "hfsm check report"
    , "status: " <> renderStatus rez.validate
    , "compiled: " <> renderCompiled rez.compiled
    , "graph: " <> renderGraph rez.graph
    , "stats: " <> renderStats rez.validate.stats
    , "diagnostics: " <> renderDiagSummary rez.validate.errs
    ]
    <> renderDiagBlock rez.validate.errs

renderStatus :: ValidateReport -> Text
renderStatus report =
  if ok report then "ok" else "failed"

renderCompiled :: Maybe SpecDigest -> Text
renderCompiled maybeDigest =
  maybe "<none>" specDigestText maybeDigest

renderGraph :: Maybe MachineGraph -> Text
renderGraph maybeGraph =
  case maybeGraph of
    Nothing -> "<none>"
    Just graph0 -> machineNameText graph0.machine <> "@" <> machineVersionText graph0.version

renderStats :: ValidateStats -> Text
renderStats stats =
  T.intercalate ", "
    [ "regions=" <> tshow stats.regionCount
    , "states=" <> tshow stats.stateCount
    , "cases=" <> tshow stats.caseCount
    , "routes=" <> tshow stats.routeCount
    , "joins=" <> tshow stats.joinCount
    , "timers=" <> tshow stats.timerCount
    ]

renderDiagSummary :: [ValidateErr] -> Text
renderDiagSummary errs =
  T.intercalate ", "
    [ "errors=" <> tshow (countDiag ErrorSv errs)
    , "warnings=" <> tshow (countDiag WarnSv errs)
    , "info=" <> tshow (countDiag InfoSv errs)
    ]

renderDiagBlock :: [ValidateErr] -> [Text]
renderDiagBlock errs
  | null errs = []
  | otherwise = "" : "diagnostic-list:" : fmap renderDiag errs

renderDiag :: ValidateErr -> Text
renderDiag err =
  "- [" <> renderSeverity err.sev <> "] "
    <> err.code
    <> renderLocSuffix err.loc
    <> ": "
    <> err.msg

renderSeverity :: Severity -> Text
renderSeverity sev =
  case sev of
    InfoSv -> "info"
    WarnSv -> "warn"
    ErrorSv -> "error"

renderLocSuffix :: Maybe ValidateLoc -> Text
renderLocSuffix maybeLoc =
  case maybeLoc of
    Nothing -> ""
    Just loc -> " at " <> renderLoc loc

renderLoc :: ValidateLoc -> Text
renderLoc loc =
  case loc of
    MachineVl -> "machine"
    RegionVl ref -> "region#" <> renderRegionRef ref
    StateVl ref -> "state#" <> renderStateRef ref
    CaseVl ref -> "case#" <> renderCaseRef ref
    RouteVl ref -> "route#" <> renderRouteRef ref
    JoinVl ref -> "join#" <> renderJoinRef ref

countDiag :: Severity -> [ValidateErr] -> Int
countDiag want =
  length . filter (\err -> err.sev == want)

renderRegionRef :: RegionRef -> Text
renderRegionRef = tshow . regionRefWord32

renderStateRef :: StateRef -> Text
renderStateRef = tshow . stateRefWord32

renderCaseRef :: CaseRef -> Text
renderCaseRef = tshow . caseRefWord32

renderRouteRef :: RouteRef -> Text
renderRouteRef = tshow . routeRefWord32

renderJoinRef :: JoinRef -> Text
renderJoinRef = tshow . joinRefWord32

tshow :: Show a => a -> Text
tshow = T.pack . show