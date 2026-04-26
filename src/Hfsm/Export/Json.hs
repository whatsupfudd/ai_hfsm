module Hfsm.Export.Json
  ( exportGraphJson
  , exportValidateJson
  ) where

import Data.Aeson (Value(..), object, toJSON, (.=))
import qualified Data.IntMap.Strict as IntMap
import Data.Text (Text)
import qualified Data.Vector as V

import Hfsm.Core.Path (renderRegionPath, renderStatePath)
import Hfsm.Graph.Def
  ( BreakPlanG(..)
  , CaseNd(..)
  , JoinMode(..)
  , JoinNd(..)
  , JoinPlanG(..)
  , MachineGraph(..)
  , RegionNd(..)
  , RouteEd(..)
  , RoutePlan(..)
  , RouteTarget(..)
  , SpawnPlanG(..)
  , StateNd(..)
  , TimerNd(..)
  , TimerPlanG(..)
  , WaitPlanG(..)
  )
import Hfsm.Spec.Def (StateKind(..))
import Hfsm.Validate.Error
  ( Severity(..)
  , ValidateErr(..)
  , ValidateLoc(..)
  , isInfo
  )
import Hfsm.Validate.Report
  ( ValidateReport(..)
  , ValidateStats(..)
  , errors
  , ok
  , warnings
  )

exportGraphJson :: MachineGraph -> Value
exportGraphJson graph =
  object
    [ "schema" .= ("hfsm.graph.v1" :: Text)
    , "machine" .= graph.machine
    , "version" .= graph.version
    , "digest" .= graph.digest
    , "root" .= graph.root
    , "counts" .= exportGraphCounts graph
    , "meta" .= toJSON graph.meta
    , "regions" .= fmap exportRegionNd (IntMap.elems graph.regions)
    , "states" .= fmap exportStateNd (IntMap.elems graph.states)
    , "cases" .= fmap exportCaseNd (IntMap.elems graph.cases)
    , "routes" .= fmap exportRouteEd (IntMap.elems graph.routes)
    , "joins" .= fmap exportJoinNd (IntMap.elems graph.joins)
    , "timers" .= fmap exportTimerNd (IntMap.elems graph.timers)
    ]

exportValidateJson :: ValidateReport -> Value
exportValidateJson report =
  object
    [ "schema" .= ("hfsm.validate.v1" :: Text)
    , "ok" .= ok report
    , "stats" .= exportValidateStats report.stats
    , "counts" .= exportValidateCounts report
    , "diagnostics" .= fmap exportValidateErr report.errs
    , "errors" .= fmap exportValidateErr (errors report)
    , "warnings" .= fmap exportValidateErr (warnings report)
    , "infos" .= fmap exportValidateErr (filter isInfo report.errs)
    ]

exportGraphCounts :: MachineGraph -> Value
exportGraphCounts graph =
  object
    [ "regions" .= IntMap.size graph.regions
    , "states" .= IntMap.size graph.states
    , "cases" .= IntMap.size graph.cases
    , "routes" .= IntMap.size graph.routes
    , "joins" .= IntMap.size graph.joins
    , "timers" .= IntMap.size graph.timers
    ]

exportRegionNd :: RegionNd -> Value
exportRegionNd region =
  object
    [ "ref" .= region.ref
    , "parentState" .= region.parent
    , "initial" .= region.initial
    , "states" .= V.toList region.states
    , "path" .= region.path
    , "pathText" .= renderRegionPath region.path
    , "meta" .= toJSON region.meta
    ]

exportStateNd :: StateNd -> Value
exportStateNd stateNd =
  object
    [ "ref" .= stateNd.ref
    , "parentRegion" .= stateNd.parent
    , "name" .= stateNd.name
    , "kind" .= renderStateKind stateNd.kind
    , "entryCase" .= stateNd.entry
    , "cases" .= V.toList stateNd.cases
    , "routes" .= V.toList stateNd.routes
    , "childRegion" .= stateNd.child
    , "path" .= stateNd.path
    , "pathText" .= renderStatePath stateNd.path
    , "meta" .= toJSON stateNd.meta
    ]

exportCaseNd :: CaseNd -> Value
exportCaseNd caseNd =
  object
    [ "ref" .= caseNd.ref
    , "state" .= caseNd.state
    , "name" .= caseNd.name
    , "meta" .= toJSON caseNd.meta
    ]

exportRouteEd :: RouteEd -> Value
exportRouteEd routeEd =
  object
    [ "ref" .= routeEd.ref
    , "state" .= routeEd.state
    , "caseRef" .= routeEd.caseRef
    , "handoff" .= routeEd.handoff
    , "plan" .= exportRoutePlan routeEd.plan
    , "meta" .= toJSON routeEd.meta
    ]

exportRoutePlan :: RoutePlan -> Value
exportRoutePlan plan =
  object
    [ "target" .= exportRouteTarget plan.target
    , "wait" .= exportWaitPlanG plan.wait
    , "spawn" .= fmap exportSpawnPlanG plan.spawn
    , "join" .= exportJoinPlanG plan.join
    , "timers" .= fmap exportTimerPlanG plan.timers
    , "breaks" .= fmap exportBreakPlanG plan.break
    ]

exportRouteTarget :: RouteTarget -> Value
exportRouteTarget target =
  case target of
    StayRt -> object [ "kind" .= ("stay" :: Text) ]
    GotoRt stateRef -> object [ "kind" .= ("goto" :: Text), "state" .= stateRef ]
    CompleteRt -> object [ "kind" .= ("complete" :: Text) ]
    FailRt reason -> object [ "kind" .= ("fail" :: Text), "reason" .= reason ]


exportWaitPlanG :: WaitPlanG -> Value
exportWaitPlanG waitPlan =
  case waitPlan of
    WaitNoneWg -> object [ "kind" .= ("none" :: Text) ]
    WaitSignalWg -> object [ "kind" .= ("signal" :: Text) ]
    WaitJoinWg joinRef -> object [ "kind" .= ("join" :: Text), "join" .= joinRef ]
    WaitTimerWg timerRef -> object [ "kind" .= ("timer" :: Text), "timer" .= timerRef ]


exportSpawnPlanG :: SpawnPlanG -> Value
exportSpawnPlanG spawnPlan =
  object
    [ "child" .= spawnPlan.child
    , "input" .= spawnPlan.input
    , "key" .= spawnPlan.key
    , "join" .= spawnPlan.join
    ]


exportJoinPlanG :: JoinPlanG -> Value
exportJoinPlanG joinPlan =
  case joinPlan of
    JoinNoneJg ->
      object
        [ "kind" .= ("none" :: Text)
        ]

    JoinAllJg joinRef ->
      object
        [ "kind" .= ("all" :: Text)
        , "join" .= joinRef
        ]

    JoinAnyJg joinRef ->
      object
        [ "kind" .= ("any" :: Text)
        , "join" .= joinRef
        ]

    JoinCountJg joinRef n ->
      object
        [ "kind" .= ("count" :: Text)
        , "join" .= joinRef
        , "count" .= n
        ]

exportJoinNd :: JoinNd -> Value
exportJoinNd joinNd =
  object
    [ "ref" .= joinNd.ref
    , "name" .= joinNd.name
    , "mode" .= exportJoinMode joinNd.mode
    , "meta" .= toJSON joinNd.meta
    ]

exportJoinMode :: JoinMode -> Value
exportJoinMode mode =
  case mode of
    AllJm ->
      object
        [ "kind" .= ("all" :: Text)
        ]

    AnyJm ->
      object
        [ "kind" .= ("any" :: Text)
        ]

    CountJm n ->
      object
        [ "kind" .= ("count" :: Text)
        , "count" .= n
        ]

exportTimerPlanG :: TimerPlanG -> Value
exportTimerPlanG timerPlan =
  object
    [ "ref" .= timerPlan.ref
    , "delaySeconds" .= nominalDiffTimeSeconds timerPlan.delay
    , "payload" .= timerPlan.payload
    ]

exportTimerNd :: TimerNd -> Value
exportTimerNd timerNd =
  object
    [ "ref" .= timerNd.ref
    , "name" .= timerNd.name
    , "meta" .= toJSON timerNd.meta
    ]

exportBreakPlanG :: BreakPlanG -> Value
exportBreakPlanG breakPlan =
  String $
    case breakPlan of
      BreakBeforeCaseBg -> "beforeCase"
      BreakAfterCaseBg -> "afterCase"
      BreakBeforeRouteBg -> "beforeRoute"
      BreakAfterRouteBg -> "afterRoute"
      BreakBeforeCommitBg -> "beforeCommit"

exportValidateStats :: ValidateStats -> Value
exportValidateStats stats =
  object
    [ "regions" .= stats.regionCount
    , "states" .= stats.stateCount
    , "cases" .= stats.caseCount
    , "routes" .= stats.routeCount
    , "joins" .= stats.joinCount
    , "timers" .= stats.timerCount
    ]

exportValidateCounts :: ValidateReport -> Value
exportValidateCounts report =
  object
    [ "total" .= length report.errs
    , "errors" .= length (errors report)
    , "warnings" .= length (warnings report)
    , "infos" .= length (filter isInfo report.errs)
    ]

exportValidateErr :: ValidateErr -> Value
exportValidateErr err =
  object
    [ "code" .= err.code
    , "message" .= err.msg
    , "severity" .= renderSeverity err.sev
    , "location" .= exportValidateLocMaybe err.loc
    ]

exportValidateLocMaybe :: Maybe ValidateLoc -> Value
exportValidateLocMaybe mLoc =
  case mLoc of
    Nothing -> Null
    Just loc -> exportValidateLoc loc

exportValidateLoc :: ValidateLoc -> Value
exportValidateLoc loc =
  case loc of
    MachineVl ->
      object
        [ "kind" .= ("machine" :: Text)
        ]

    RegionVl regionRef ->
      object
        [ "kind" .= ("region" :: Text)
        , "ref" .= regionRef
        ]

    StateVl stateRef ->
      object
        [ "kind" .= ("state" :: Text)
        , "ref" .= stateRef
        ]

    CaseVl caseRef ->
      object
        [ "kind" .= ("case" :: Text)
        , "ref" .= caseRef
        ]

    RouteVl routeRef ->
      object
        [ "kind" .= ("route" :: Text)
        , "ref" .= routeRef
        ]

    JoinVl joinRef ->
      object
        [ "kind" .= ("join" :: Text)
        , "ref" .= joinRef
        ]

renderStateKind :: StateKind -> Text
renderStateKind kind =
  case kind of
    AtomicSk -> "atomic"
    CompositeSk -> "composite"
    TerminalSk -> "terminal"

renderSeverity :: Severity -> Text
renderSeverity sev =
  case sev of
    InfoSv -> "info"
    WarnSv -> "warn"
    ErrorSv -> "error"

nominalDiffTimeSeconds :: Real a => a -> Double
nominalDiffTimeSeconds = realToFrac