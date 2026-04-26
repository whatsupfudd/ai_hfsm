module Hfsm.Migrate.Check
  ( checkMigration
  ) where

import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T

import Hfsm.Core.Ref
  ( JoinRef
  , RouteRef
  , StateRef
  , TimerRef
  , joinRefWord32
  , routeRefWord32
  , stateRefWord32
  , timerRefWord32
  )
import Hfsm.Graph.Def (MachineGraph)
import Hfsm.Migrate.Diff (GraphDiff(..), diffGraph)
import Hfsm.Validate.Error
  ( ValidateErr(..)
  , atJoin
  , atMachine
  , atRoute
  , atState
  , errorErr
  , infoErr
  , warnErr
  )
import Hfsm.Validate.Report (ValidateReport(..))
import qualified Hfsm.Validate.Report as Report
import qualified Hfsm.Validate.Version as Version

checkMigration :: MachineGraph -> MachineGraph -> ValidateReport
checkMigration fromGraph toGraph =
  let
    fromReport = Report.validate fromGraph
    toReport = Report.validate toGraph
    sourceErrs = tagErrs "migrate-source" "source graph" fromReport.errs
    targetErrs = tagErrs "migrate-target" "target graph" toReport.errs
    compatErrs = fmap (tagErr "migrate-compat" "graph compatibility") (Version.checkVersion fromGraph toGraph)
    diffErrs = fmap graphDiffErr (diffGraph fromGraph toGraph)
    errs = stableErrs (sourceErrs <> targetErrs <> compatErrs <> diffErrs)
  in
  ValidateReport
    { errs = errs
    , stats = toReport.stats
    }

tagErrs :: Text -> Text -> [ValidateErr] -> [ValidateErr]
tagErrs prefix label = fmap (tagErr prefix label)

tagErr :: Text -> Text -> ValidateErr -> ValidateErr
tagErr prefix label err =
  err
    { code = scopedCode prefix err.code
    , msg = label <> ": " <> err.msg
    }

scopedCode :: Text -> Text -> Text
scopedCode prefix base
  | T.null prefix = base
  | T.null base = prefix
  | otherwise = prefix <> "-" <> base

graphDiffErr :: GraphDiff -> ValidateErr
graphDiffErr diff =
  case diff of
    StateAddedGd ref ->
      atState ref $
        infoErr "migrate-state-added" ("state addition detected: " <> renderStateRef ref) Nothing

    StateRemovedGd ref ->
      atState ref $
        warnErr "migrate-state-removed" ("state removal detected: " <> renderStateRef ref) Nothing

    StateMovedGd ref ->
      atState ref $
        warnErr "migrate-state-moved" ("state movement detected: " <> renderStateRef ref) Nothing

    RouteChangedGd ref ->
      atRoute ref $
        warnErr "migrate-route-changed" ("route change detected: " <> renderRouteRef ref) Nothing

    JoinChangedGd ref ->
      atJoin ref $
        warnErr "migrate-join-changed" ("join change detected: " <> renderJoinRef ref) Nothing

    TimerChangedGd ref ->
      atMachine $
        warnErr "migrate-timer-changed" ("timer change detected: " <> renderTimerRef ref) Nothing

    IncompatibleGd msg ->
      atMachine $
        errorErr "migrate-incompatible" msg Nothing

stableErrs :: [ValidateErr] -> [ValidateErr]
stableErrs = go S.empty
  where
    go _ [] = []
    go seen (err : rest)
      | S.member err seen = go seen rest
      | otherwise = err : go (S.insert err seen) rest

renderStateRef :: StateRef -> Text
renderStateRef ref = "state#" <> tshow (stateRefWord32 ref)

renderRouteRef :: RouteRef -> Text
renderRouteRef ref = "route#" <> tshow (routeRefWord32 ref)

renderJoinRef :: JoinRef -> Text
renderJoinRef ref = "join#" <> tshow (joinRefWord32 ref)

renderTimerRef :: TimerRef -> Text
renderTimerRef ref = "timer#" <> tshow (timerRefWord32 ref)

tshow :: Show a => a -> Text
tshow = T.pack . show