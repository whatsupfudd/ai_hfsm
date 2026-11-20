{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE NoFieldSelectors #-}
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
import Control.Monad (replicateM)

import qualified Data.IntMap.Strict as IM
import Data.Int (Int64)
import qualified Data.List as L
import qualified Data.Map.Strict as M
import Data.Map.Strict (Map)
import qualified Data.Set as S
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (getCurrentTime, addUTCTime)
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

import Hfsm.Runtime.Model.Instance (instanceIsActive)
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
  , RenewRq(..)
  , CommitRq(..)
  , StoreErr(..)
  , BreakAppend(..)
  , ChildAppend(..)
  , CommitRez(..)
  , LeaseRez(..)
  , LeaseRq(..)
  , OutboxAppend(..)
  , PollRq(..)
  , ProjBatch(..)
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
    , commitTurn = memoryCommitTurn impl
    , ackSignal = memoryAckSignal impl
    , renewLease = memoryRenewLease impl
    , listSignals = memoryListSignals impl
    }

createInstance :: StoreImpl -> CompiledMachine st sg hf ctx cmd child -> ctx -> IO UUID
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
          , revision = 0
          , nextSignalOrdinal = 1
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
  inserted <- atomically $ do
    rows <- readTVar impl.instances
    case M.lookup instId rows of
      Nothing -> pure False
      Just inst -> do
        let row = SignalRw
              { uid = uid, uuid = signalId, inst = instId
              , ordinal = inst.nextSignalOrdinal
              , status = EnteredSs, payload = toJSON signal
              , cause = Nothing, lease = Nothing
              , leaseOwner = Nothing, leaseUntil = Nothing
              , attempts = 0, readyAt = now
              , createdAt = now, updatedAt = now }
        modifyTVar' impl.instances (M.insert instId
          (inst { nextSignalOrdinal = inst.nextSignalOrdinal + 1 } :: InstanceRw))
        modifyTVar' impl.signals (M.insert signalId row)
        pure True
  if inserted then pure signalId
    else fail ("instance not found: " <> T.unpack (U.toText instId))

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

-- | One lease per instance, selecting only the earliest pending ordinal.
-- An expired lease is reclaimed on the SAME signal with a fresh fencing UUID.
memoryLeaseSignal :: StoreImpl -> LeaseRq -> IO LeaseRez
memoryLeaseSignal impl rq = do
  now <- getCurrentTime
  let maxItems = max 0 rq.batchSize
      ttl = fromIntegral (max 1 rq.leaseSeconds)
  tokens <- replicateM maxItems U4.nextRandom
  atomically $ do
    allSignals <- readTVar impl.signals
    insts <- readTVar impl.instances
    let pending = filter (\s -> s.status == EnteredSs || s.status == LeasedSs)
                  (M.elems allSignals)
        oldest = M.elems $ M.fromListWith
          (\a b -> if a.ordinal <= b.ordinal then a else b)
          [(s.inst, s) | s <- pending]
        eligible s = case M.lookup s.inst insts of
          Nothing -> False
          Just inst -> instanceIsActive inst.status &&
            case s.status of
              EnteredSs -> s.readyAt <= now
              LeasedSs -> maybe False (<= now) s.leaseUntil
              _ -> False
        candidates = take maxItems $ L.sortOn (\s -> (s.readyAt,s.uid))
          (filter eligible oldest)
        leased = zipWith (\row token -> (row :: SignalRw)
          { status = LeasedSs, lease = Just token
          , leaseOwner = Just rq.node
          , leaseUntil = Just (addUTCTime ttl now)
          , attempts = row.attempts + 1
          , updatedAt = now }) candidates tokens
    if null leased
      then pure LeaseNoneLz
      else do
        writeTVar impl.signals $
          foldr (\row acc -> M.insert row.uuid row acc) allSignals leased
        pure (LeaseSomeLz leased)

memoryLoadInstance :: StoreImpl -> UUID -> IO (Maybe InstanceRw)
memoryLoadInstance impl instId =
  M.lookup instId <$> readTVarIO impl.instances

memoryLoadSnapshot :: StoreImpl -> UUID -> IO (Maybe SnapshotRw)
memoryLoadSnapshot impl snapshotId =
  M.lookup snapshotId <$> readTVarIO impl.snapshots

-- | Reference backend: the entire successful turn is one STM transaction.
-- UUID generation occurs before the transaction; no row is written until all
-- checks have passed. Unlike the previous append chain, crashes or conflicts
-- cannot expose partial commits.
memoryCommitTurn :: StoreImpl -> CommitRq -> IO (Either StoreErr CommitRez)
memoryCommitTurn impl rq = do
  outboxIds <- replicateM (length rq.outbox) U4.nextRandom
  childIds <- replicateM (length rq.child) U4.nextRandom
  breakIds <- replicateM (length rq.breaks) U4.nextRandom
  now <- getCurrentTime
  atomically $ do
    instances <- readTVar impl.instances
    signals <- readTVar impl.signals
    snapshots <- readTVar impl.snapshots
    steps <- readTVar impl.steps
    case (M.lookup rq.instanceId instances, M.lookup rq.signalId signals) of
      (Nothing, _) -> pure (Left (NotFoundEr "instance missing at commit"))
      (_, Nothing) -> pure (Left (NotFoundEr "signal missing at commit"))
      (Just instRow, Just sigRow)
        | instRow.revision /= rq.expectedRevision ->
            pure (Left (ConflictEr "instance revision changed since planning"))
        | instRow.revision == maxBound ->
            pure (Left (ConflictEr "instance revision exhausted"))
        | not (instanceIsActive instRow.status) ->
            pure (Left (ConflictEr "instance is terminal"))
        | instRow.snap /= rq.expectedSnapshot ->
            pure (Left (ConflictEr "instance snapshot changed since planning"))
        | M.notMember rq.expectedSnapshot snapshots ->
            pure (Left (NotFoundEr "expected snapshot not stored"))
        | sigRow.ordinal /= rq.signalOrdinal ->
            pure (Left (ConflictEr "signal ordinal changed since planning"))
        | sigRow.inst /= rq.instanceId ->
            pure (Left (ConflictEr "signal belongs to a different instance"))
        | sigRow.status /= LeasedSs || sigRow.lease /= Just rq.leaseId ->
            pure (Left (LeaseLostEr "signal lease no longer valid"))
        | maybe True (<= now) sigRow.leaseUntil ->
            pure (Left (LeaseLostEr "signal lease expired"))
        | any (\app -> app.inst /= rq.instanceId || app.step /= rq.step.uuid) rq.outbox ->
            pure (Left (ConflictEr "planned outbox is not bound to this turn"))
        | any (\app -> app.parent /= rq.instanceId) rq.child ->
            pure (Left (ConflictEr "child references a different parent"))
        | any (\app -> app.inst /= Just rq.instanceId) rq.breaks ->
            pure (Left (ConflictEr "breakpoint references a different instance"))
        | rq.step.inst /= rq.instanceId || rq.snapshot.inst /= rq.instanceId ->
            pure (Left (ConflictEr "planned step/snapshot has different instance"))
        | rq.step.signal /= Just rq.signalId ->
            pure (Left (ConflictEr "planned step has different signal"))
        | rq.step.from /= instRow.state ->
            pure (Left (ConflictEr "planned step has stale from-state"))
        | rq.snapshot.state /= rq.step.to ->
            pure (Left (ConflictEr "step target differs from snapshot state"))
        | rq.snapshot.version /= instRow.version || rq.snapshot.digest /= instRow.digest ->
            pure (Left (ConflictEr "snapshot machine version changed"))
        | rq.step.uuid == U.nil || rq.snapshot.uuid == U.nil ->
            pure (Left (ConflictEr "turn IDs must be planned and non-nil"))
        | M.member rq.step.uuid steps || M.member rq.snapshot.uuid snapshots ->
            pure (Left (ConflictEr "turn already committed"))
        | otherwise -> do
            startUid <- readTVar impl.nextUid
            let
              stepRw = rq.step { uid = startUid } :: StepRw
              snapshotRw = rq.snapshot { uid = startUid + 1 } :: SnapshotRw
              outboxStart = startUid + 2
              childStart = outboxStart + fromIntegral (length rq.outbox)
              breakStart = childStart + fromIntegral (length rq.child)
              endUid = breakStart + fromIntegral (length rq.breaks)
              outboxRows = zipWith3
                (\uid rowId app -> OutboxRw
                  { uid = uid, uuid = rowId, inst = app.inst
                  , step = stepRw.uuid
                  , effectKey = U.toText stepRw.uuid <> ":" <> T.pack (show (uid - outboxStart))
                  , status = ReadyOs, payload = app.payload
                  , lease = Nothing, createdAt = app.now, updatedAt = app.now })
                [outboxStart..] outboxIds rq.outbox
              childRows = zipWith3
                (\uid rowId app -> ChildRw
                  { uid = uid, uuid = rowId, parent = app.parent
                  , child = app.child, key = app.key, status = app.status
                  , join = app.join, createdAt = app.now, updatedAt = app.now })
                [childStart..] childIds rq.child
              breakRows = zipWith3
                (\uid rowId app -> BreakRw
                  { uid = uid, uuid = rowId, inst = app.inst
                  , kind = app.kind, state = app.state, status = app.status
                  , note = app.note, createdAt = app.now, updatedAt = app.now })
                [breakStart..] breakIds rq.breaks
              instNext = instRow
                { state = snapshotRw.state, path = snapshotRw.path
                , snap = snapshotRw.uuid, revision = instRow.revision + 1
                , status = rq.nextStatus
                , updatedAt = snapshotRw.createdAt }
              sigNext = (sigRow :: SignalRw)
                { status = AppliedSs, lease = Nothing, leaseOwner = Nothing
                , leaseUntil = Nothing, updatedAt = snapshotRw.createdAt }
            writeTVar impl.nextUid endUid
            writeTVar impl.instances (M.insert rq.instanceId instNext instances)
            writeTVar impl.signals (M.insert rq.signalId sigNext signals)
            writeTVar impl.steps (M.insert stepRw.uuid stepRw steps)
            writeTVar impl.snapshots (M.insert snapshotRw.uuid snapshotRw snapshots)
            modifyTVar' impl.outboxes (\xs -> foldr (\x -> M.insert x.uuid x) xs outboxRows)
            modifyTVar' impl.children (\xs -> foldr (\x -> M.insert x.uuid x) xs childRows)
            modifyTVar' impl.breaks (\xs -> foldr (\x -> M.insert x.uuid x) xs breakRows)
            modifyTVar' impl.activeProj (<> rq.proj.active)
            modifyTVar' impl.traceProj (<> rq.proj.trace)
            modifyTVar' impl.queueProj (<> rq.proj.queue)
            modifyTVar' impl.treeProj (<> rq.proj.tree)
            pure (Right CommitRez { step = stepRw, snapshot = snapshotRw })

-- | Errors have no successful turn to commit. Failure/rejection/requeue ACKs are
-- also fenced. A mismatched/stale lease never changes the signal row.
memoryAckSignal :: StoreImpl -> AckRq -> IO (Either StoreErr ())
memoryAckSignal impl rq = do
  now <- getCurrentTime
  atomically $ do
    rows <- readTVar impl.signals
    case M.lookup rq.signal rows of
      Nothing -> pure (Left (NotFoundEr "signal missing for failure ACK"))
      Just row
        | row.status /= LeasedSs || row.lease /= Just rq.lease ->
            pure (Left (LeaseLostEr "signal is not leased by this worker"))
        | maybe True (<= now) row.leaseUntil ->
            pure (Left (LeaseLostEr "signal lease expired before ACK"))
        | rq.status /= FailedSs && rq.status /= RejectedSs && rq.status /= EnteredSs ->
            pure (Left (ConflictEr "ACK outside commitTurn may only fail, reject or requeue"))
        | otherwise -> do
            writeTVar impl.signals $ M.insert rq.signal
              (row { status = rq.status, lease = Nothing, leaseOwner = Nothing
                   , leaseUntil = Nothing, readyAt = now, updatedAt = now } :: SignalRw) rows
            pure (Right ())

-- | Fenced extension; expires strictly at deadline and may never shorten TTL.
memoryRenewLease :: StoreImpl -> RenewRq -> IO (Either StoreErr UTCTime)
memoryRenewLease impl rq = do
  now <- getCurrentTime
  let wanted = addUTCTime (fromIntegral (max 1 (min 3600 rq.extendSeconds))) now
  atomically $ do
    rows <- readTVar impl.signals
    case M.lookup rq.signal rows of
      Nothing -> pure (Left (NotFoundEr "renewLease: missing signal"))
      Just row
        | row.status /= LeasedSs || row.lease /= Just rq.lease ->
            pure (Left (LeaseLostEr "renewLease: stale fencing token"))
        | maybe True (<= now) row.leaseUntil ->
            pure (Left (LeaseLostEr "renewLease: expired lease"))
        | otherwise -> do
            let until' = maybe wanted (max wanted) row.leaseUntil
            writeTVar impl.signals (M.insert rq.signal
              (row { leaseUntil = Just until' } :: SignalRw) rows)
            pure (Right until')

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
    , "revision" .= row.revision
    , "nextSignalOrdinal" .= row.nextSignalOrdinal
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
    , "ordinal" .= row.ordinal
    , "status" .= showText row.status
    , "payload" .= row.payload
    , "cause" .= row.cause
    , "lease" .= fmap U.toText row.lease
    , "leaseOwner" .= fmap U.toText row.leaseOwner
    , "leaseUntil" .= row.leaseUntil
    , "attempts" .= row.attempts
    , "readyAt" .= row.readyAt
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
    , "effectKey" .= row.effectKey
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