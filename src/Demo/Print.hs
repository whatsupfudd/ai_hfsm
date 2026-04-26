{-# LANGUAGE LambdaCase #-}

module Demo.Print
  ( ppPhase
  , ppSignal
  , ppHandoff
  , ppApprovalCtx
  , ppCmd
  , ppRunState
  , ppRunStep
  , ppRunTrace
  , ppDemoPureTrace
  , ppRuntimeStore
  , printRunState
  , printRunTrace
  , printDemoPureTrace
  , printRuntimeStore
  ) where

import Control.Monad (foldM)

import Data.Text (Text)
import qualified Data.Text as T
import qualified Data.Text.IO as TIO

import Data.Aeson (Value)

import Hfsm.Spec.Def (MachineSpec(..))


import Demo.ApprovalHfsm
  ( ApprovalCtx(..)
  , Cmd(..)
  , Handoff(..)
  , Phase(..)
  , RunState(..)
  , Signal(..)
  , approvalSpec
  , stepPure, ChildKey
  )

import Hfsm.Render.Print
  ( ppSection
  , ppStoreView
  )

ppPhase :: Phase -> Text
ppPhase phase =
  case phase of
    DraftPh -> "Draft"
    ReviewPh -> "Review"
    ReworkPh -> "Rework"
    DonePh -> "Done"

ppSignal :: Signal -> Text
ppSignal signal =
  case signal of
    SubmitSg user ->
      "Submit by " <> user

    ApproveSg user ->
      "Approve by " <> user

    RejectSg user reason ->
      "Reject by " <> user <> " — " <> reason

    ResubmitSg user ->
      "Resubmit by " <> user

ppHandoff :: Handoff -> Text
ppHandoff handoff =
  case handoff of
    SubmittedHf -> "Submitted"
    ApprovedHf -> "Approved"
    RejectedHf -> "Rejected"
    ResubmittedHf -> "Resubmitted"

ppApprovalCtx :: ApprovalCtx -> Text
ppApprovalCtx ctx =
  T.intercalate "\n"
    [ "  title: " <> ctx.title
    , "  author: " <> blank ctx.author
    , "  revision: " <> tshow ctx.revision
    , "  approvedBy: " <> ppMaybe ctx.approvedBy
    , "  rejectedBy: " <> ppMaybe ctx.rejectedBy
    , "  rejectReason: " <> ppMaybe ctx.rejectReason
    ]

ppCmd :: Cmd -> Text
ppCmd cmd =
  case cmd of
    NotifyCm target msg ->
      "notify " <> target <> ": " <> msg

ppRunState :: RunState -> Text
ppRunState run =
  T.intercalate "\n"
    [ "phase: " <> ppPhase run.phase
    , "context:"
    , ppApprovalCtx run.ctx
    , "commands:"
    , ppCmdList run.cmds
    ]

ppRunStep :: Int -> Signal -> Either Text RunState -> Text
ppRunStep ix signal result =
  case result of
    Left err ->
      T.intercalate "\n"
        [ "Step " <> tshow ix <> " — " <> ppSignal signal
        , "  result: failed"
        , "  error: " <> err
        ]

    Right run ->
      T.intercalate "\n"
        [ "Step " <> tshow ix <> " — " <> ppSignal signal
        , "  result: ok"
        , "  phase: " <> ppPhase run.phase
        , "  context:"
        , indentBlock 4 (ppApprovalCtx run.ctx)
        , "  emitted commands:"
        , indentBlock 4 (ppCmdList run.cmds)
        ]

ppRunTrace :: RunState -> [Signal] -> Text
ppRunTrace initial signals =
  let
    trace =
      buildTrace initial signals
  in
  ppSection "Approval HFSM pure trace" $
    T.intercalate "\n\n"
      [ "Initial state\n" <> indentBlock 2 (ppRunState initial)
      , T.intercalate "\n\n" $
          fmap
            (\(ix, signal, result) -> ppRunStep ix signal result)
            trace
      , "Final state\n" <> indentBlock 2 (ppFinalFromTrace initial trace)
      ]

ppDemoPureTrace :: Text
ppDemoPureTrace =
  case approvalSpec of
    Left err ->
      ppSection "Approval HFSM pure trace" ("invalid spec: " <> err)

    Right spec ->
      ppRunTraceWithSpec spec demoInitialState demoSignals

ppRuntimeStore :: Value -> Text
ppRuntimeStore value =
  ppSection "Approval HFSM runtime store" $
    ppStoreView value

printRunState :: RunState -> IO ()
printRunState =
  TIO.putStrLn . ppRunState

printRunTrace :: RunState -> [Signal] -> IO ()
printRunTrace initial signals =
  TIO.putStrLn (ppRunTrace initial signals)

printDemoPureTrace :: IO ()
printDemoPureTrace =
  TIO.putStrLn ppDemoPureTrace

printRuntimeStore :: Value -> IO ()
printRuntimeStore =
  TIO.putStrLn . ppRuntimeStore

ppRunTraceWithSpec :: MachineSpec Phase Signal Handoff ApprovalCtx Cmd ChildKey -> RunState -> [Signal] -> Text
ppRunTraceWithSpec spec initial signals =
  let
    trace =
      buildTraceWithSpec spec initial signals
  in
  ppSection "Approval HFSM pure trace" $
    T.intercalate "\n\n"
      [ "Initial state\n" <> indentBlock 2 (ppRunState initial)
      , T.intercalate "\n\n" $
          fmap
            (\(ix, signal, result) -> ppRunStep ix signal result)
            trace
      , "Final state\n" <> indentBlock 2 (ppFinalFromTrace initial trace)
      ]

buildTrace :: RunState -> [Signal] -> [(Int, Signal, Either Text RunState)]
buildTrace initial signals =
  case approvalSpec of
    Left err ->
      [(0, SubmitSg "<spec>", Left err)]

    Right spec ->
      buildTraceWithSpec spec initial signals

buildTraceWithSpec :: MachineSpec Phase Signal Handoff ApprovalCtx Cmd ChildKey -> RunState -> [Signal] -> [(Int, Signal, Either Text RunState)]
buildTraceWithSpec spec initial signals =
  reverse trace
  where
    (_, trace) =
      foldl
        step
        (Right initial, [])
        (zip [1 :: Int ..] signals)

    step (Left err, acc) (ix, signal) =
      (Left err, (ix, signal, Left err) : acc)

    step (Right run, acc) (ix, signal) =
      let result = stepPure spec run signal
      in (result, (ix, signal, result) : acc)

ppFinalFromTrace :: RunState -> [(Int, Signal, Either Text RunState)] -> Text
ppFinalFromTrace initial trace =
  case lastResult trace of
    Nothing ->
      ppRunState initial

    Just (Left err) ->
      "failed: " <> err

    Just (Right run) ->
      ppRunState run

lastResult :: [(Int, Signal, Either Text RunState)] -> Maybe (Either Text RunState)
lastResult trace =
  case reverse trace of
    [] -> Nothing
    (_, _, result) : _ -> Just result

demoInitialState :: RunState
demoInitialState =
  RunState
    { phase = DraftPh
    , ctx =
        ApprovalCtx
          { title = "Gold custody note"
          , author = ""
          , revision = 0
          , approvedBy = Nothing
          , rejectedBy = Nothing
          , rejectReason = Nothing
          }
    , cmds = []
    }

demoSignals :: [Signal]
demoSignals =
  [ SubmitSg "alice"
  , RejectSg "bob" "needs legal note"
  , ResubmitSg "alice"
  , ApproveSg "carol"
  ]

ppCmdList :: [Cmd] -> Text
ppCmdList cmds =
  case cmds of
    [] ->
      "  <none>"

    _ ->
      T.intercalate "\n" $
        fmap
          (\(ix, cmd) -> "  " <> tshow ix <> ". " <> ppCmd cmd)
          (zip [1 :: Int ..] cmds)

ppMaybe :: Maybe Text -> Text
ppMaybe value =
  case value of
    Nothing -> "<none>"
    Just txt -> txt

blank :: Text -> Text
blank txt
  | T.null txt = "<blank>"
  | otherwise = txt

indentBlock :: Int -> Text -> Text
indentBlock n txt =
  T.intercalate "\n" $
    fmap
      (\line -> T.replicate n " " <> line)
      (T.lines txt)

tshow :: Show a => a -> Text
tshow =
  T.pack . show