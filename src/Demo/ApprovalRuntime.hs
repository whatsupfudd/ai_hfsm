{-# LANGUAGE OverloadedStrings #-}

module Demo.ApprovalRuntime where

import Control.Monad (unless)

import Demo.ApprovalHfsm
  ( ApprovalCtx(..)
  , Signal(..)
  , approvalSpec, Phase, Handoff, Cmd, ChildKey
  )

import Hfsm.Compile (compile, CompiledMachine (..))
import Hfsm.Runtime.Engine.Worker (WorkerCfg(..), runTick)
import Hfsm.Runtime.Registry (emptyRegistry, register)
import Hfsm.Validate.Report (ok, validate)
import Data.UUID (UUID)
import qualified Data.UUID.V4 as Uu

import qualified Data.Aeson as Ae

import Hfsm.Store.Class (Store)
import Demo.MemoryStore
  ( createInstance
  , enqueueSignal
  , loadInstanceView
  , newMemoryStore
  , storeOf
  )

import Hfsm.Render.Print (ppMachine)

testA :: IO ()
testA = do
  spec <-
    case approvalSpec of
      Left err -> fail ("invalid approval spec: " <> show err)
      Right x -> pure x

  compiled <-
    case compile spec of
      Left err -> fail ("compile failed: " <> show err)
      Right x -> pure x

  let report = validate compiled.graph
  unless (ok report) $
    fail ("validation failed: " <> show report)

  registry <-
    case register compiled emptyRegistry of
      Left err -> fail ("registry failed: " <> show err)
      Right x -> pure x

  store <- newMemoryStore

  inst <- createInstance store compiled initialCtx

  putStrLn $ "@[testA] machine: " <> show (ppMachine compiled)

  enqueueSignal store inst (SubmitSg "alice")
  enqueueSignal store inst (RejectSg "bob" "needs legal note")
  enqueueSignal store inst (ResubmitSg "alice")
  enqueueSignal store inst (ApproveSg "carol")

  demoNodeId <- Uu.nextRandom

  let
    cfg = WorkerCfg { batchSize = 10, pollDelayMs = 100, leaseSeconds = 30, node = demoNodeId }
    storeImpl = storeOf store

  runTick cfg storeImpl registry
  runTick cfg storeImpl registry
  runTick cfg storeImpl registry
  runTick cfg storeImpl registry

  final <- loadInstanceView store inst
  print $ Ae.encode final

initialCtx :: ApprovalCtx
initialCtx =
  ApprovalCtx
    { title = "Gold custody note"
    , author = ""
    , revision = 0
    , approvedBy = Nothing
    , rejectedBy = Nothing
    , rejectReason = Nothing
    }
