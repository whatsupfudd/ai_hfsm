{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TypeApplications #-}
{-# LANGUAGE DerivingStrategies #-}

module Demo.ApprovalHfsm
  ( Phase(..)
  , Signal(..)
  , Handoff(..)
  , ApprovalCtx(..)
  , Cmd(..)
  , ChildKey(..)
  , RunState(..)
  , approvalSpec
  , stepPure
  , demoPure
  ) where

import Control.Monad (foldM)
import qualified Data.List as List
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime(..), Day(..))

import GHC.Generics (Generic)

import Data.Aeson (FromJSON, ToJSON)

import Hfsm.Core
  ( CodecSet
  , MetaSpec
  , emptyMetaSpec
  , jsonCodec
  , mkCaseName
  , mkCodecSet
  , mkMachineName
  , mkMachineVersion
  , mkRegionName
  , mkStateName
  )

import Hfsm.Spec.Case
  ( CaseSpec(..)
  , ReactEnv(..)
  , ReactErr(..)
  , ReactRez(..)
  )

import Hfsm.Spec.Def
  ( MachineSpec(..)
  , RegionSpec(..)
  , StateKind(..)
  , StateSpec(..)
  )

import Hfsm.Spec.Route
  ( ControlPlan(..)
  , JoinPlan(..)
  , RouteSpec(..)
  , TargetPlan(..)
  , WaitPlan(..)
  )


data Phase =
    DraftPh
  | ReviewPh
  | ReworkPh
  | DonePh
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Signal =
    SubmitSg Text
  | ApproveSg Text
  | RejectSg Text Text
  | ResubmitSg Text
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Handoff =
    SubmittedHf
  | ApprovedHf
  | RejectedHf
  | ResubmittedHf
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ApprovalCtx = ApprovalCtx
  { title :: Text
  , author :: Text
  , revision :: Int
  , approvedBy :: Maybe Text
  , rejectedBy :: Maybe Text
  , rejectReason :: Maybe Text
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data Cmd =
    NotifyCm Text Text
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data ChildKey =
    NoChildCk
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (ToJSON, FromJSON)

data RunState = RunState
  { phase :: Phase
  , ctx :: ApprovalCtx
  , cmds :: [Cmd]
  }
  deriving stock (Eq, Ord, Show, Read)

approvalSpec :: Either Text (MachineSpec Phase Signal Handoff ApprovalCtx Cmd ChildKey)
approvalSpec = do
  machineName <- mapLeft showText $ mkMachineName "demo.approval"
  version <- mapLeft showText $ mkMachineVersion "1.0.0"
  rootName <- mapLeft showText $ mkRegionName "root"

  draftName <- mapLeft showText $ mkStateName "Draft"
  reviewName <- mapLeft showText $ mkStateName "Review"
  reworkName <- mapLeft showText $ mkStateName "Rework"
  doneName <- mapLeft showText $ mkStateName "Done"

  submitCaseName <- mapLeft showText $ mkCaseName "submit"
  approveCaseName <- mapLeft showText $ mkCaseName "approve"
  rejectCaseName <- mapLeft showText $ mkCaseName "reject"
  resubmitCaseName <- mapLeft showText $ mkCaseName "resubmit"

  let codecs =
        mkCodecSet
          (jsonCodec @ApprovalCtx)
          (jsonCodec @Signal)
          (jsonCodec @Cmd)

  let submitCase =
        CaseSpec
          { name = Just submitCaseName
          , match = \case
              SubmitSg _ -> True
              _ -> False
          , react = \_ signal ctx ->
              case signal of
                SubmitSg user ->
                  Right ReactRez
                    { next = ctx { author = user }
                    , handoff = SubmittedHf
                    , cmds = [NotifyCm "review-team" ("document submitted by " <> user)]
                    , note = ["submitted"]
                    }

                _ ->
                  Left (RejectREr "submit case received a non-submit signal")
          , meta = emptyMetaSpec
          }

  let approveCase =
        CaseSpec
          { name = Just approveCaseName
          , match = \case
              ApproveSg _ -> True
              _ -> False
          , react = \_ signal ctx ->
              case signal of
                ApproveSg user ->
                  Right ReactRez
                    { next = ctx { approvedBy = Just user }
                    , handoff = ApprovedHf
                    , cmds = [NotifyCm ctx.author ("approved by " <> user)]
                    , note = ["approved"]
                    }

                _ ->
                  Left (RejectREr "approve case received a non-approve signal")
          , meta = emptyMetaSpec
          }

  let rejectCase =
        CaseSpec
          { name = Just rejectCaseName
          , match = \case
              RejectSg _ _ -> True
              _ -> False
          , react = \_ signal ctx ->
              case signal of
                RejectSg user reason ->
                  Right ReactRez
                    { next = ctx { rejectedBy = Just user, rejectReason = Just reason }
                    , handoff = RejectedHf
                    , cmds = [NotifyCm ctx.author ("rejected by " <> user <> ": " <> reason)]
                    , note = ["rejected"]
                    }

                _ ->
                  Left (RejectREr "reject case received a non-reject signal")
          , meta = emptyMetaSpec
          }

  let resubmitCase =
        CaseSpec
          { name = Just resubmitCaseName
          , match = \case
              ResubmitSg _ -> True
              _ -> False
          , react = \_ signal ctx ->
              case signal of
                ResubmitSg user ->
                  Right ReactRez
                    { next = ctx { revision = ctx.revision + 1, rejectedBy = Nothing, rejectReason = Nothing }
                    , handoff = ResubmittedHf
                    , cmds = [NotifyCm "review-team" ("revision submitted by " <> user)]
                    , note = ["resubmitted"]
                    }

                _ ->
                  Left (RejectREr "resubmit case received a non-resubmit signal")
          , meta = emptyMetaSpec
          }

  let draftSt =
        StateSpec
          { key = DraftPh
          , name = draftName
          , kind = AtomicSk
          , entry = Nothing
          , cases = [submitCase]
          , routes =
              [ RouteSpec
                  { on = SubmittedHf
                  , plan = gotoPlan ReviewPh
                  , meta = emptyMetaSpec
                  }
              ]
          , child = Nothing
          , meta = emptyMetaSpec
          }

  let reviewSt =
        StateSpec
          { key = ReviewPh
          , name = reviewName
          , kind = AtomicSk
          , entry = Nothing
          , cases = [approveCase, rejectCase]
          , routes =
              [ RouteSpec
                  { on = ApprovedHf
                  , plan = gotoPlan DonePh
                  , meta = emptyMetaSpec
                  }
              , RouteSpec
                  { on = RejectedHf
                  , plan = gotoPlan ReworkPh
                  , meta = emptyMetaSpec
                  }
              ]
          , child = Nothing
          , meta = emptyMetaSpec
          }

  let reworkSt =
        StateSpec
          { key = ReworkPh
          , name = reworkName
          , kind = AtomicSk
          , entry = Nothing
          , cases = [resubmitCase]
          , routes =
              [ RouteSpec
                  { on = ResubmittedHf
                  , plan = gotoPlan ReviewPh
                  , meta = emptyMetaSpec
                  }
              ]
          , child = Nothing
          , meta = emptyMetaSpec
          }

  let doneSt =
        StateSpec
          { key = DonePh
          , name = doneName
          , kind = TerminalSk
          , entry = Nothing
          , cases = []
          , routes = []
          , child = Nothing
          , meta = emptyMetaSpec
          }

  let root =
        RegionSpec
          { name = Just rootName
          , initial = DraftPh
          , states = [draftSt, reviewSt, reworkSt, doneSt]
          , meta = emptyMetaSpec
          }

  Right MachineSpec
    { name = machineName
    , version = version
    , root = root
    , codecs = codecs
    , meta = emptyMetaSpec
    }

gotoPlan :: Phase -> ControlPlan Phase ChildKey
gotoPlan phase =
  ControlPlan
    { target = GotoTg phase
    , wait = WaitNoneWp
    , spawn = []
    , join = JoinNoneJp
    , timers = []
    , break = []
    }
  
-- The execution system:
stepPure :: MachineSpec Phase Signal Handoff ApprovalCtx Cmd ChildKey -> RunState -> Signal -> Either Text RunState
stepPure spec runState signal = do
  stateSpec <- findState runState.phase spec.root
  caseSpec <- findCase signal stateSpec
  rez <- mapLeft renderReactErr $ caseSpec.react emptyReactEnv signal runState.ctx
  routeSpec <- findRoute rez.handoff stateSpec
  nextPhase <- applyTarget runState.phase routeSpec.plan.target
  Right RunState
    { phase = nextPhase
    , ctx = rez.next
    , cmds = runState.cmds <> rez.cmds
    }

findState :: Phase -> RegionSpec Phase Signal Handoff ApprovalCtx Cmd ChildKey -> Either Text (StateSpec Phase Signal Handoff ApprovalCtx Cmd ChildKey)
findState phase region =
  case List.find (\st -> st.key == phase) region.states of
    Just st -> Right st
    Nothing -> Left ("state not found: " <> showText phase)

findCase :: Signal -> StateSpec Phase Signal Handoff ApprovalCtx Cmd ChildKey -> Either Text (CaseSpec Signal ApprovalCtx Handoff Cmd)
findCase signal stateSpec =
  case List.find (\caseSpec -> caseSpec.match signal) stateSpec.cases of
    Just caseSpec -> Right caseSpec
    Nothing -> Left ("no matching case in state " <> showText stateSpec.key <> " for signal " <> showText signal)

findRoute :: Handoff -> StateSpec Phase Signal Handoff ApprovalCtx Cmd ChildKey -> Either Text (RouteSpec Phase Handoff ChildKey)
findRoute handoff stateSpec =
  case List.find (\routeSpec -> routeSpec.on == handoff) stateSpec.routes of
    Just routeSpec -> Right routeSpec
    Nothing -> Left ("no route in state " <> showText stateSpec.key <> " for handoff " <> showText handoff)

applyTarget :: Phase -> TargetPlan Phase -> Either Text Phase
applyTarget current target =
  case target of
    StayTg ->
      Right current

    GotoTg phase ->
      Right phase

    CompleteTg ->
      Right DonePh

    FailTg msg ->
      Left ("machine failed: " <> msg)

emptyReactEnv :: ReactEnv
emptyReactEnv =
  ReactEnv
    { now = UTCTime (ModifiedJulianDay 0) 0 -- TODO: use a real time
    , attempt = 0
    , cause = Nothing
    , child = mempty
    , timer = mempty
    }

renderReactErr :: ReactErr -> Text
renderReactErr err =
  case err of
    DecodeREr msg -> "decode error: " <> msg
    RejectREr msg -> "reject error: " <> msg
    DomainEr msg -> "domain error: " <> msg
    InvariantEr msg -> "invariant error: " <> msg

mapLeft :: (a -> b) -> Either a x -> Either b x
mapLeft f rez =
  case rez of
    Left x -> Left (f x)
    Right x -> Right x

showText :: Show a => a -> Text
showText = T.pack . show

-- Just for a simple test:
demoPure :: IO ()
demoPure =
  case approvalSpec of
    Left err ->
      putStrLn ("invalid spec: " <> T.unpack err)

    Right spec -> do
      let initialCtx =
            ApprovalCtx
              { title = "Items custody note"
              , author = ""
              , revision = 0
              , approvedBy = Nothing
              , rejectedBy = Nothing
              , rejectReason = Nothing
              }

      let signals =
            [ SubmitSg "alice"
            , RejectSg "bob" "needs legal note"
            , ResubmitSg "alice"
            , ApproveSg "carol"
            ]

      case foldM (stepPure spec) RunState { phase = DraftPh, ctx = initialCtx, cmds = [] } signals of
        Left err ->
          putStrLn ("run failed: " <> T.unpack err)

        Right final ->
          print final
