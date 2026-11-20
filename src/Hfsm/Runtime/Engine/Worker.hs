{-# LANGUAGE DeriveAnyClass #-}
{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE DerivingStrategies #-}

module Hfsm.Runtime.Engine.Worker ( WorkerCfg(..), runWorker, runTick ) where

import Control.Concurrent (threadDelay)
import Control.DeepSeq (NFData)
import Control.Monad (forever, unless, void, when)
import Control.Monad.IO.Class (MonadIO(liftIO))

import Data.Text (Text)
import qualified Data.Text as T
import Data.Time.Clock (UTCTime, getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as U

import GHC.Generics (Generic)
import Data.Aeson (FromJSON, ToJSON)

import Hfsm.Core.Digest (specDigestText)
import Hfsm.Core.Name (machineNameText)
import Hfsm.Core.Version (machineVersionText)
import Hfsm.Runtime.Engine.Step (EngineErr(..), StepInput(..), runStep)
import Hfsm.Runtime.Model (InstanceRw (..), SignalRw (..), SignalStatus(..), SnapshotRw (..))
import Hfsm.Runtime.Registry (MachineKey(..), Registry, resolve)
import Hfsm.Store.Class (AckRq(..), LeaseRq(..), LeaseRez(..), Store(..), StoreErr(..))

data WorkerCfg = WorkerCfg
  { batchSize :: Int
  , pollDelayMs :: Int
  , leaseSeconds :: Int
  , node :: UUID
  }
  deriving stock (Eq, Ord, Show, Read, Generic)
  deriving anyclass (NFData, ToJSON, FromJSON)

runWorker :: (MonadIO m, Show hf) => WorkerCfg -> Store m -> Registry st sg hf ctx cmd child -> m ()
runWorker rawCfg store registry = do
  let cfg = normalizeWorkerCfg rawCfg
  forever $ do
    processed <- runTickCount cfg store registry
    when (processed <= 0) $ sleepPollDelay cfg

runTick :: (MonadIO m, Show hf) => WorkerCfg -> Store m -> Registry st sg hf ctx cmd child -> m ()
runTick rawCfg store registry =
  void (runTickCount (normalizeWorkerCfg rawCfg) store registry)

runTickCount :: (MonadIO m, Show hf) => WorkerCfg -> Store m -> Registry st sg hf ctx cmd child -> m Int
runTickCount cfg store registry = do
  now <- liftIO getCurrentTime
  leaseRez <- store.leaseSignal (leaseRqOf cfg now)
  case leaseRez of
    LeaseNoneLz -> pure 0
    LeaseSomeLz sigs -> do
      mapM_ (runLeasedSignal cfg store registry) sigs
      pure (length sigs)

runLeasedSignal :: (MonadIO m, Show hf) => WorkerCfg -> Store m -> Registry st sg hf ctx cmd child -> SignalRw -> m ()
runLeasedSignal cfg store registry sigRw = do
  mInstRw <- store.loadInstance sigRw.inst
  case mInstRw of
    Nothing -> ackFailed cfg store sigRw (missingInstanceMsg sigRw)
    Just instRw -> do
      mSnapRw <- store.loadSnapshot instRw.snap
      case mSnapRw of
        Nothing -> ackFailed cfg store sigRw (missingSnapshotMsg instRw)
        Just snapRw -> do
          let key = machineKeyFromInstance instRw
          case resolve key registry of
            Nothing -> ackFailed cfg store sigRw (missingMachineMsg key)
            Just compiled -> do
              rez <- runStep store compiled (stepInputOf key instRw snapRw sigRw)
              case rez of
                -- An optimistic-CAS collision means a different turn moved
                -- the instance while we planned: release/requeue this signal.
                Left (StoreCommitEr (ConflictEr _)) -> ackRequeue cfg store sigRw
                -- A transient backend failure should not terminally fail work.
                Left (StoreCommitEr (BackendEr _)) -> ackRequeue cfg store sigRw
                -- A fenced-out worker must not touch a signal it no longer owns.
                Left (StoreCommitEr (LeaseLostEr _)) -> pure ()
                Left err -> ackFailed cfg store sigRw (stepFailedMsg err)
                -- Success ACK is an integral part of commitTurn; never ACK twice.
                Right _ -> pure ()

stepInputOf :: MachineKey -> InstanceRw -> SnapshotRw -> SignalRw -> StepInput
stepInputOf key instRw snapRw sigRw =
  StepInput { machine = key, instance_ = instRw, snapshot = snapRw, signal = sigRw }

leaseRqOf :: WorkerCfg -> UTCTime -> LeaseRq
leaseRqOf cfg now =
  LeaseRq
    { node = cfg.node
    , batchSize = cfg.batchSize
    , leaseSeconds = cfg.leaseSeconds
    , now = now
    }

machineKeyFromInstance :: InstanceRw -> MachineKey
machineKeyFromInstance instRw =
  MachineKey { name = instRw.machine, version = instRw.version, digest = instRw.digest }

ackFailed :: MonadIO m => WorkerCfg -> Store m -> SignalRw -> Text -> m ()
ackFailed cfg store sigRw msg =
  ackSignalWith cfg store sigRw FailedSs (Just msg)

ackRequeue :: MonadIO m => WorkerCfg -> Store m -> SignalRw -> m ()
ackRequeue cfg store sigRw =
  ackSignalWith cfg store sigRw EnteredSs Nothing

-- | Errors that produced no turn may be failed or retried using an explicit
-- lease-fenced semantic ACK. An applied signal is NEVER ACKed here.
ackSignalWith :: MonadIO m => WorkerCfg -> Store m -> SignalRw -> SignalStatus -> Maybe Text -> m ()
ackSignalWith cfg store sigRw statusVal noteVal = do
  now <- liftIO getCurrentTime
  void $ store.ackSignal
    AckRq
      { signal = sigRw.uuid
      , lease = signalLease cfg sigRw
      , status = statusVal
      , note = normalizeMaybeText noteVal
      , now = now
      }

signalLease :: WorkerCfg -> SignalRw -> UUID
signalLease cfg sigRw =
  case sigRw.lease of
    Just leaseId -> leaseId
    Nothing -> cfg.node

sleepPollDelay :: MonadIO m => WorkerCfg -> m ()
sleepPollDelay cfg = do
  let delayUs = pollDelayUs cfg
  unless (delayUs <= 0) $ liftIO (threadDelay delayUs)

pollDelayUs :: WorkerCfg -> Int
pollDelayUs cfg
  | cfg.pollDelayMs <= 0 = 0
  | cfg.pollDelayMs >= maxBound `div` 1000 = maxBound
  | otherwise = cfg.pollDelayMs * 1000

normalizeWorkerCfg :: WorkerCfg -> WorkerCfg
normalizeWorkerCfg cfg =
  cfg
    { batchSize = max 1 cfg.batchSize
    , pollDelayMs = max 0 cfg.pollDelayMs
    , leaseSeconds = max 1 cfg.leaseSeconds
    }

missingInstanceMsg :: SignalRw -> Text
missingInstanceMsg sigRw =
  "leased signal " <> renderUuid sigRw.uuid <>
  " references missing instance " <> renderUuid sigRw.inst

missingSnapshotMsg :: InstanceRw -> Text
missingSnapshotMsg instRw =
  "instance " <> renderUuid instRw.uuid <>
  " references missing snapshot " <> renderUuid instRw.snap

missingMachineMsg :: MachineKey -> Text
missingMachineMsg key =
  "no compiled machine registered for " <> renderMachineKey key

stepFailedMsg :: Show err => err -> Text
stepFailedMsg err = "step execution failed: " <> T.pack (show err)

renderMachineKey :: MachineKey -> Text
renderMachineKey key =
  machineNameText key.name <> "@" <> machineVersionText key.version <> "#" <> specDigestText key.digest

renderUuid :: UUID -> Text
renderUuid = U.toText

normalizeMaybeText :: Maybe Text -> Maybe Text
normalizeMaybeText mTxt =
  case fmap T.strip mTxt of
    Just txt | T.null txt -> Nothing
    other -> other
