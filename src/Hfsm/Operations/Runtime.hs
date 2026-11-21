{-# LANGUAGE NoFieldSelectors #-}
-- | Production process loops. A worker must register, heartbeat and use the
-- CT06 stores; the legacy Engine.runWorker does not enforce CT06 admission.
module Hfsm.Operations.Runtime
  ( withRegisteredWorker, runSignalWorker, runActivityWorker
  , runSignalWorkerTraced, runActivityWorkerTraced ) where

import Control.Concurrent (forkIO, killThread, myThreadId, threadDelay)
import Control.Exception (bracket, throwTo)
import Control.Monad (forever)
import qualified Data.Text as T
import Data.UUID (UUID)

import qualified Hfsm.Activity.Worker as Activity
import Hfsm.Activity.Types (Provider, WorkerCfg(..))
import qualified Hfsm.Runtime.Engine.Worker as Engine
import Hfsm.Runtime.Registry (Registry)
import Hfsm.Operations.Types (WorkerRegistration(..))
import Hfsm.Store.Postgres.Types (Runner)
import qualified Hfsm.Operations.Postgres as Pg
import Hfsm.Operations.Trace (SpanSink)
import qualified Hfsm.Operations.Traced as Traced

-- | The generation fence prevents a restarted process sharing a worker UUID
-- from silently keeping its predecessor's heartbeat registration alive.
withRegisteredWorker :: Runner -> WorkerRegistration -> IO a -> IO a
withRegisteredWorker runner reg action = bracket acquire release (\_ -> action)
 where
  acquire = do
    generation <- Pg.registerWorker runner reg >>= required "register worker"
    owner <- myThreadId
    watcher <- forkIO (heartbeat owner generation)
    pure (generation, watcher)
  release (generation,watcher) = do
    killThread watcher
    _ <- Pg.setWorkerState runner reg.workerId generation "stopped"
    pure ()
  heartbeat owner generation = forever $ do
    threadDelay (max 300000 (max 2 (min 3600 reg.heartbeatTtlSeconds) * 1000000 `div` 3))
    renewed <- Pg.heartbeatWorker runner reg.workerId generation
    case renewed of
      Right True -> pure ()
      other -> throwTo owner (userError ("CT06 heartbeat lost: " <> show other))
  required label result = either (fail . ((label <> ": ") <>) . show) pure result

runSignalWorker :: Show hf
                => Runner -> WorkerRegistration -> Engine.WorkerCfg
                -> Registry st sg hf ctx cmd child -> IO ()
runSignalWorker runner reg cfg registry = do
  if cfg.node /= reg.workerId then fail "CT06 signal config node /= registered worker" else pure ()
  withRegisteredWorker runner reg $ do
    let store = Pg.productionStore runner
    forever $ do
      Engine.runTick cfg store registry
      threadDelay (max 10000 (cfg.pollDelayMs * 1000))

runActivityWorker :: Runner -> WorkerRegistration -> WorkerCfg -> [Provider] -> IO ()
runActivityWorker runner reg cfg providers = do
  if cfg.node /= reg.workerId then fail "CT06 activity config node /= registered worker" else pure ()
  withRegisteredWorker runner reg $ do
    let store = Pg.productionActivityStore runner
    forever $ do
      result <- Activity.runTick cfg store providers
      case result of
        Left errorValue -> fail ("CT06 activity worker: " <> show errorValue)
        Right _ -> pure ()
      threadDelay (max 10000 (cfg.pollDelayMs * 1000))

-- | Optional OTLP instrumentation for normal production process loops.
-- Supplying an OTLP sink keeps span export outside the authoritative commit.
runSignalWorkerTraced :: Show hf
                => SpanSink -> Runner -> WorkerRegistration -> Engine.WorkerCfg
                -> Registry st sg hf ctx cmd child -> IO ()
runSignalWorkerTraced sink runner reg cfg registry = do
  if cfg.node /= reg.workerId then fail "CT06 signal config node /= registered worker" else pure ()
  withRegisteredWorker runner reg $ do
    let store = Traced.productionStoreTraced sink runner
    forever $ do
      Engine.runTick cfg store registry
      threadDelay (max 10000 (cfg.pollDelayMs * 1000))

runActivityWorkerTraced :: SpanSink -> Runner -> WorkerRegistration -> WorkerCfg -> [Provider] -> IO ()
runActivityWorkerTraced sink runner reg cfg providers = do
  if cfg.node /= reg.workerId then fail "CT06 activity config node /= registered worker" else pure ()
  withRegisteredWorker runner reg $ do
    let store = Traced.productionActivityStoreTraced sink runner
    forever $ do
      result <- Activity.runTick cfg store providers
      case result of
        Left errorValue -> fail ("CT06 activity worker: "<>show errorValue)
        Right _ -> pure ()
      threadDelay (max 10000 (cfg.pollDelayMs * 1000))
