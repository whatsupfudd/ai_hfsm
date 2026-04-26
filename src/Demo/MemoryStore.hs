{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
{-# LANGUAGE OverloadedRecordDot #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE ScopedTypeVariables #-}

module Demo.MemoryStore
  ( StoreImpl
  , storeOf
  , newMemoryStore
  , createInstance
  , enqueueSignal
  , loadInstanceView
  ) where

import Control.Concurrent.STM

import qualified Data.IntMap.Strict as IM
import Data.Int (Int64)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime)
import Data.UUID (UUID)
import qualified Data.UUID as U
import qualified Data.UUID.V4 as U4

import Data.Aeson (ToJSON, Value(..), object, toJSON, (.=))

import Hfsm.Compile (CompiledMachine (..))
import Hfsm.Core
  ( encodeWith
  , machineNameText
  , machineVersionText
  , regionRefWord32
  , routeRefWord32
  , caseRefWord32
  , stateRefWord32
  , specDigestText
  , renderStatePath
  , RegionRef (..)
  , StateRef (..)
  , joinRefWord32
  , Codec (..)
  , CodecSet (..)
  )

import Hfsm.Graph.Def
  ( RegionNd(..)
  , StateNd(..)
  , MachineGraph(..)
  )

import Hfsm.Runtime.Model
  ( BreakKind(..)
  , BreakRw(..)
  , BreakStatus(..)
  , ChildRw(..)
  , ChildStatus(..)
  , InstanceRw(..)
  , InstanceStatus(..)
  , OutboxRw(..)
  , OutboxStatus(..)
  , SignalRw(..)
  , SignalStatus(..)
  , SnapshotRw(..)
  , StepKind(..)
  , StepRw(..)
  )

import Hfsm.Store.Class
  ( AckRq(..)
  , BreakAppend(..)
  , ChildAppend(..)
  , CommitRez(..)
  , LeaseRez(..)
  , LeaseRq(..)
  , OutboxAppend(..)
  , PollRq(..)
  , ProjBatch(..)
  , SnapshotAppend(..)
  , StepAppend(..)
  , Store(..)
  )
import Hfsm.Spec.Def (MachineSpec(..), StateSpec(..))
import Hfsm.Spec.Route (RouteSpec(..))
import Hfsm.Spec.Case (CaseSpec(..))


data StoreImpl = StoreImpl
  { api :: Store IO
  , nextUid :: TVar Int64
  , instances :: TVar (Map UUID InstanceRw)
  , signals :: TVar (Map UUID SignalRw)
  , snapshots :: TVar (Map UUID SnapshotRw)
  , steps :: TVar (Map UUID StepRw)
  , outboxes :: TVar (Map UUID OutboxRw)
  , children :: TVar (Map UUID ChildRw)
  , breaks :: TVar (Map UUID BreakRw)
  , activeProj :: TVar [Value]
  , traceProj :: TVar [Value]
  , queueProj :: TVar [Value]
  , treeProj :: TVar [Value]
  }

storeOf :: StoreImpl -> Store IO
storeOf = (.api)

newMemoryStore :: IO StoreImpl
newMemoryStore = do
  nextUid <- newTVarIO 1
  instances <- newTVarIO M.empty
  signals <- newTVarIO M.empty
  snapshots <- newTVarIO M.empty
  steps <- newTVarIO M.empty
  outboxes <- newTVarIO M.empty
  children <- newTVarIO M.empty
  breaks <- newTVarIO M.empty
  activeProj <- newTVarIO []
  traceProj <- newTVarIO []
  queueProj <- newTVarIO []
  treeProj <- newTVarIO []

  let impl =
        StoreImpl
          { api = memoryApi impl
          , nextUid = nextUid
          , instances = instances
          , signals = signals
          , snapshots = snapshots
          , steps = steps
          , outboxes = outboxes
          , children = children
          , breaks = breaks
          , activeProj = activeProj
          , traceProj = traceProj
          , queueProj = queueProj
          , treeProj = treeProj
          }

  pure impl

memoryApi :: StoreImpl -> Store IO
memoryApi impl =
  Store
    { leaseSignal = memoryLeaseSignal impl
    , loadInstance = memoryLoadInstance impl
    , loadSnapshot = memoryLoadSnapshot impl
    , appendStep = memoryAppendStep impl
    , appendSnapshot = memoryAppendSnapshot impl
    , appendOutbox = memoryAppendOutbox impl
    , appendChild = memoryAppendChild impl
    , appendBreak = memoryAppendBreak impl
    , ackSignal = memoryAckSignal impl
    , updateProj = memoryUpdateProj impl
    , listSignals = memoryListSignals impl
    }

createInstance :: ToJSON ctx => StoreImpl -> CompiledMachine st sg hf ctx cmd child -> ctx -> IO UUID
createInstance impl compiled ctx = do
  now <- getCurrentTime
  instId <- U4.nextRandom
  snapId <- U4.nextRandom
  instUid <- freshUid impl
  snapUid <- freshUid impl

  stateNode <-
    case initialStateNode compiled of
      Left msg -> fail (T.unpack msg)
      Right x -> pure x

  let ctxValue = encodeWith compiled.spec.codecs.ctx ctx

  let snapRow =
        SnapshotRw
          { uid = snapUid
          , uuid = snapId
          , inst = instId
          , state = stateNode.ref
          , path = stateNode.path
          , ctx = ctxValue
          , wait = Nothing
          , child = object []
          , version = compiled.graph.version
          , digest = compiled.digest
          , createdAt = now
          }

  let instRow =
        InstanceRw
          { uid = instUid
          , uuid = instId
          , machine = compiled.graph.machine
          , version = compiled.graph.version
          , digest = compiled.digest
          , status = RunningIs
          , state = stateNode.ref
          , path = stateNode.path
          , snap = snapId
          , parent = Nothing
          , createdAt = now
          , updatedAt = now
          }

  atomically $ do
    modifyTVar' impl.snapshots (M.insert snapId snapRow)
    modifyTVar' impl.instances (M.insert instId instRow)

  pure instId

enqueueSignal :: ToJSON sg => StoreImpl -> UUID -> sg -> IO UUID
enqueueSignal impl instId signal = do
  now <- getCurrentTime
  uid <- freshUid impl
  signalId <- U4.nextRandom

  exists <- atomically $ do
    instances <- readTVar impl.instances
    pure (M.member instId instances)

  if not exists
    then fail ("instance not found: " <> T.unpack (U.toText instId))
    else do
      let row =
            SignalRw
              { uid = uid
              , uuid = signalId
              , inst = instId
              , status = EnteredSs
              , payload = toJSON signal
              , cause = Nothing
              , lease = Nothing
              , createdAt = now
              , updatedAt = now
              }

      atomically $
        modifyTVar' impl.signals (M.insert signalId row)

      pure signalId

loadInstanceView :: StoreImpl -> UUID -> IO Value
loadInstanceView impl instId = do
  instances <- readTVarIO impl.instances
  signals <- readTVarIO impl.signals
  snapshots <- readTVarIO impl.snapshots
  steps <- readTVarIO impl.steps
  outboxes <- readTVarIO impl.outboxes
  children <- readTVarIO impl.children
  breaks <- readTVarIO impl.breaks
  activeProj <- readTVarIO impl.activeProj
  traceProj <- readTVarIO impl.traceProj
  queueProj <- readTVarIO impl.queueProj
  treeProj <- readTVarIO impl.treeProj

  let mInst = M.lookup instId instances
  let mSnap = mInst >>= \inst -> M.lookup inst.snap snapshots

  let signalsForInst =
        L.sortOn (\(x :: SignalRw) -> x.uid) $
          filter (\(x :: SignalRw) -> x.inst == instId) $
            M.elems signals

  let stepsForInst =
        L.sortOn (\(x :: StepRw) -> x.uid) $
          filter (\(x :: StepRw) -> x.inst == instId) $
            M.elems steps

  let outboxesForInst =
        L.sortOn (\(x :: OutboxRw) -> x.uid) $
          filter (\(x :: OutboxRw) -> x.inst == instId) $
            M.elems outboxes

  let childrenForInst =
        L.sortOn (\(x :: ChildRw) -> x.uid) $
          filter (\(x :: ChildRw) -> x.parent == instId) $
            M.elems children

  let breaksForInst =
        L.sortOn (\(x :: BreakRw) -> x.uid) $
          filter (\(x :: BreakRw) -> x.inst == Just instId) $
            M.elems breaks

  pure $
    object
      [ "instance" .= maybe Null instanceValue mInst
      , "snapshot" .= maybe Null snapshotValue mSnap
      , "signals" .= fmap signalValue signalsForInst
      , "steps" .= fmap stepValue stepsForInst
      , "outbox" .= fmap outboxValue outboxesForInst
      , "children" .= fmap childValue childrenForInst
      , "breaks" .= fmap breakValue breaksForInst
      , "projections" .= object
          [ "active" .= activeProj
          , "trace" .= traceProj
          , "queue" .= queueProj
          , "tree" .= treeProj
          ]
      ]

memoryLeaseSignal :: StoreImpl -> LeaseRq -> IO LeaseRez
memoryLeaseSignal impl rq =
  atomically $ do
    signals <- readTVar impl.signals

    let candidates =
          take rq.batchSize $
            L.sortOn (\(x :: SignalRw) -> x.uid) $
              filter (\(x :: SignalRw) -> x.status == EnteredSs) $
                M.elems signals

    if null candidates
      then pure LeaseNoneLz
      else do
        let
          leased = fmap (\row -> (row :: SignalRw) {
                      status = LeasedSs
                    , lease = Just rq.node
                    , updatedAt = rq.now
                    }
              ) candidates

        writeTVar impl.signals $
          foldr (\row acc -> M.insert row.uuid row acc) signals leased

        pure (LeaseSomeLz leased)

memoryLoadInstance :: StoreImpl -> UUID -> IO (Maybe InstanceRw)
memoryLoadInstance impl instId =
  M.lookup instId <$> readTVarIO impl.instances

memoryLoadSnapshot :: StoreImpl -> UUID -> IO (Maybe SnapshotRw)
memoryLoadSnapshot impl snapshotId =
  M.lookup snapshotId <$> readTVarIO impl.snapshots

memoryAppendStep :: StoreImpl -> StepAppend -> IO StepRw
memoryAppendStep impl app = do
  uid <- freshUid impl
  stepId <- U4.nextRandom

  let row =
        StepRw
          { uid = uid
          , uuid = stepId
          , inst = app.inst
          , signal = app.signal
          , kind = app.kind
          , from = app.from
          , to = app.to
          , caseRef = app.caseRef
          , route = app.route
          , note = app.note
          , createdAt = app.now
          }

  atomically $
    modifyTVar' impl.steps (M.insert stepId row)

  pure row

memoryAppendSnapshot :: StoreImpl -> SnapshotAppend -> IO SnapshotRw
memoryAppendSnapshot impl app = do
  uid <- freshUid impl
  snapshotId <- U4.nextRandom

  let row =
        SnapshotRw
          { uid = uid
          , uuid = snapshotId
          , inst = app.inst
          , state = app.state
          , path = app.path
          , ctx = app.ctx
          , wait = app.wait
          , child = app.child
          , version = app.version
          , digest = app.digest
          , createdAt = app.now
          }

  atomically $ do
    modifyTVar' impl.snapshots (M.insert snapshotId row)
    modifyTVar' impl.instances $
      M.adjust
        (\inst ->
          inst
            { state = app.state
            , path = app.path
            , snap = snapshotId
            , updatedAt = app.now
            }
        )
        app.inst

  pure row

memoryAppendOutbox :: StoreImpl -> [OutboxAppend] -> IO ()
memoryAppendOutbox impl apps =
  mapM_ appendOne apps
  where
    appendOne :: OutboxAppend -> IO ()
    appendOne app = do
      uid <- freshUid impl
      outboxId <- U4.nextRandom

      let row =
            OutboxRw
              { uid = uid
              , uuid = outboxId
              , inst = app.inst
              , step = app.step
              , status = ReadyOs
              , payload = app.payload
              , lease = Nothing
              , createdAt = app.now
              , updatedAt = app.now
              }

      atomically $
        modifyTVar' impl.outboxes (M.insert outboxId row)

memoryAppendChild :: StoreImpl -> [ChildAppend] -> IO ()
memoryAppendChild impl apps =
  mapM_ appendOne apps
  where
    appendOne :: ChildAppend -> IO ()
    appendOne app = do
      uid <- freshUid impl
      rowId <- U4.nextRandom

      let row =
            ChildRw
              { uid = uid
              , uuid = rowId
              , parent = app.parent
              , child = app.child
              , key = app.key
              , status = app.status
              , join = app.join
              , createdAt = app.now
              , updatedAt = app.now
              }

      atomically $
        modifyTVar' impl.children (M.insert rowId row)

memoryAppendBreak :: StoreImpl -> [BreakAppend] -> IO ()
memoryAppendBreak impl apps =
  mapM_ appendOne apps
  where
    appendOne :: BreakAppend -> IO ()
    appendOne app = do
      uid <- freshUid impl
      breakId <- U4.nextRandom

      let row =
            BreakRw
              { uid = uid
              , uuid = breakId
              , inst = app.inst
              , kind = app.kind
              , state = app.state
              , status = app.status
              , note = app.note
              , createdAt = app.now
              , updatedAt = app.now
              }

      atomically $
        modifyTVar' impl.breaks (M.insert breakId row)

memoryAckSignal :: StoreImpl -> AckRq -> IO ()
memoryAckSignal impl rq =
  atomically $ do
    signals <- readTVar impl.signals

    case M.lookup rq.signal signals of
      Nothing ->
        pure ()

      Just row
        | row.lease == Just rq.lease ->
            writeTVar impl.signals $
              M.insert
                rq.signal
                (row :: SignalRw)
                  { status = rq.status
                  , updatedAt = rq.now
                  }
                signals

        | otherwise ->
            pure ()

memoryUpdateProj :: StoreImpl -> ProjBatch -> IO ()
memoryUpdateProj impl batch =
  atomically $ do
    modifyTVar' impl.activeProj (<> batch.active)
    modifyTVar' impl.traceProj (<> batch.trace)
    modifyTVar' impl.queueProj (<> batch.queue)
    modifyTVar' impl.treeProj (<> batch.tree)

memoryListSignals :: StoreImpl -> PollRq -> IO [SignalRw]
memoryListSignals impl rq =
  atomically $ do
    signals <- readTVar impl.signals
    let statusSet = S.fromList rq.status

    pure $
      take rq.limit $
        L.sortOn (\(x :: SignalRw) -> x.uid) $
          filter (\(x :: SignalRw) -> S.member x.status statusSet) $
            M.elems signals

freshUid :: StoreImpl -> IO Int64
freshUid impl =
  atomically $ do
    uid <- readTVar impl.nextUid
    writeTVar impl.nextUid (uid + 1)
    pure uid

initialStateNode :: CompiledMachine st sg hf ctx cmd child -> Either Text StateNd
initialStateNode compiled = do
  rootRegion <-
    case IM.lookup (regionKey compiled.graph.root) compiled.graph.regions of
      Nothing -> Left "root region not found in compiled graph"
      Just x -> Right x

  case IM.lookup (stateKey rootRegion.initial) compiled.graph.states of
    Nothing -> Left "root initial state not found in compiled graph"
    Just x -> Right x

regionKey :: RegionRef -> Int
regionKey = fromIntegral . regionRefWord32

stateKey :: StateRef -> Int
stateKey = fromIntegral . stateRefWord32

instanceValue :: InstanceRw -> Value
instanceValue row =
  object
    [ "uid" .= row.uid
    , "uuid" .= U.toText row.uuid
    , "machine" .= machineNameText row.machine
    , "version" .= machineVersionText row.version
    , "digest" .= specDigestText row.digest
    , "status" .= showText row.status
    , "state" .= stateRefWord32 row.state
    , "path" .= renderStatePath row.path
    , "snap" .= U.toText row.snap
    , "parent" .= fmap U.toText row.parent
    , "createdAt" .= row.createdAt
    , "updatedAt" .= row.updatedAt
    ]

signalValue :: SignalRw -> Value
signalValue row =
  object
    [ "uid" .= row.uid
    , "uuid" .= U.toText row.uuid
    , "inst" .= U.toText row.inst
    , "status" .= showText row.status
    , "payload" .= row.payload
    , "cause" .= row.cause
    , "lease" .= fmap U.toText row.lease
    , "createdAt" .= row.createdAt
    , "updatedAt" .= row.updatedAt
    ]

snapshotValue :: SnapshotRw -> Value
snapshotValue row =
  object
    [ "uid" .= row.uid
    , "uuid" .= U.toText row.uuid
    , "inst" .= U.toText row.inst
    , "state" .= stateRefWord32 row.state
    , "path" .= renderStatePath row.path
    , "ctx" .= row.ctx
    , "wait" .= row.wait
    , "child" .= row.child
    , "version" .= machineVersionText row.version
    , "digest" .= specDigestText row.digest
    , "createdAt" .= row.createdAt
    ]

stepValue :: StepRw -> Value
stepValue row =
  object
    [ "uid" .= row.uid
    , "uuid" .= U.toText row.uuid
    , "inst" .= U.toText row.inst
    , "signal" .= fmap U.toText row.signal
    , "kind" .= showText row.kind
    , "from" .= stateRefWord32 row.from
    , "to" .= stateRefWord32 row.to
    , "caseRef" .= fmap caseRefWord32 row.caseRef
    , "route" .= routeRefWord32 row.route
    , "note" .= row.note
    , "createdAt" .= row.createdAt
    ]

outboxValue :: OutboxRw -> Value
outboxValue row =
  object
    [ "uid" .= row.uid
    , "uuid" .= U.toText row.uuid
    , "inst" .= U.toText row.inst
    , "step" .= U.toText row.step
    , "status" .= showText row.status
    , "payload" .= row.payload
    , "lease" .= fmap U.toText row.lease
    , "createdAt" .= row.createdAt
    , "updatedAt" .= row.updatedAt
    ]

childValue :: ChildRw -> Value
childValue row =
  object
    [ "uid" .= row.uid
    , "uuid" .= U.toText row.uuid
    , "parent" .= U.toText row.parent
    , "child" .= U.toText row.child
    , "key" .= row.key
    , "status" .= showText row.status
    , "join" .= fmap joinRefWord32 row.join
    , "createdAt" .= row.createdAt
    , "updatedAt" .= row.updatedAt
    ]

breakValue :: BreakRw -> Value
breakValue row =
  object
    [ "uid" .= row.uid
    , "uuid" .= U.toText row.uuid
    , "inst" .= fmap U.toText row.inst
    , "kind" .= showText row.kind
    , "state" .= fmap stateRefWord32 row.state
    , "status" .= showText row.status
    , "note" .= row.note
    , "createdAt" .= row.createdAt
    , "updatedAt" .= row.updatedAt
    ]

showText :: Show a => a -> Text
showText = T.pack . show