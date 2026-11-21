{-# LANGUAGE NoFieldSelectors #-}

-- | Pure, typed migration planning. No database writes, no external effects.
module Hfsm.Migration.Plan (planMigration) where

import Data.Bifunctor (first)
import qualified Data.IntMap.Strict as IM
import Data.Text (Text)
import qualified Data.Text as T
import Data.Time (UTCTime)
import Data.UUID (UUID)
import qualified Data.UUID as UUID

import Hfsm.Compile (CompiledMachine(..))
import Hfsm.Core.Codec (Codec(..), CodecSet(..))
import Hfsm.Core.Ref (stateRefWord32)
import Hfsm.Graph.Def (MachineGraph(..), StateNd(..))
import Hfsm.Runtime.Model (InstanceRw(..), SnapshotRw(..), InstanceStatus(..))
import Hfsm.Runtime.Registry (machineKeyOf, MachineKey(..))
import Hfsm.Spec.Def (MachineSpec(..), StateKind(..))
import Hfsm.Migration.Types (MigrationSpec(..), MigrationRequest(..), MigrationErr(..))

planMigration
  :: CompiledMachine st1 sg1 hf1 oldCtx cmd1 child1
  -> CompiledMachine st2 sg2 hf2 newCtx cmd2 child2
  -> MigrationSpec oldCtx newCtx
  -> UUID -> UUID -> UUID -> UTCTime
  -> InstanceRw -> SnapshotRw
  -> Either MigrationErr MigrationRequest
planMigration fromMachine toMachine spec operationId stepId snapId now inst snap = do
  let sourceKey = machineKeyOf fromMachine
      targetKey = machineKeyOf toMachine
      wrong msg = Left (MigrationPlanError msg)
  if sourceKey.name /= targetKey.name
    then wrong "migration must preserve machine name"
    else pure ()
  if sourceKey == targetKey then wrong "source and destination releases must differ" else pure ()
  if inst.machine /= sourceKey.name || inst.version /= sourceKey.version ||
     inst.digest /= sourceKey.digest || snap.version /= sourceKey.version ||
     snap.digest /= sourceKey.digest
    then wrong "source release does not match pinned instance/snapshot"
    else pure ()
  if snap.inst /= inst.uuid || snap.uuid /= inst.snap ||
     snap.state /= inst.state || snap.path /= inst.path
    then wrong "source snapshot/current instance pointer mismatch"
    else pure ()
  if inst.status `elem` [DoneIs, FailedIs, CancelledIs]
    then wrong "terminal instances cannot be migrated" else pure ()
  if operationId == UUID.nil || stepId == UUID.nil || snapId == UUID.nil ||
     stepId == snapId || snapId == inst.snap
    then wrong "migration UUIDs must be fresh, non-nil and distinct" else pure ()
  if T.null (T.strip spec.mappingName)
    then wrong "named migration transform is required" else pure ()
  if IM.notMember (fromIntegral (stateRefWord32 snap.state)) fromMachine.graph.states
    then wrong "source state absent from source graph" else pure ()
  old <- first (MigrationPlanError . T.pack . show) $ fromMachine.spec.codecs.ctx.decode snap.ctx
  new <- first MigrationPlanError (spec.mapContext old)
  destRef <- first MigrationPlanError (spec.mapControl snap.state)
  destState <- maybe (wrong "destination state absent from target graph") Right $
    IM.lookup (fromIntegral (stateRefWord32 destRef)) toMachine.graph.states
  waitVal <- first MigrationPlanError (spec.mapWait snap.wait)
  childVal <- first MigrationPlanError (spec.mapChild snap.child)
  statusVal <- first MigrationPlanError (spec.mapStatus inst.status)
  if statusVal == WaitingIs && waitVal == Nothing
    then wrong "WaitingIs requires an explicit new wait condition" else pure ()
  let rootTerminal = destState.kind == TerminalSk &&
                     destState.parent == toMachine.graph.root
  if rootTerminal && statusVal /= DoneIs
    then wrong "root terminal target must map lifecycle to DoneIs" else pure ()
  if statusVal == DoneIs && not rootTerminal
    then wrong "DoneIs migration must target a root terminal state" else pure ()
  pure MigrationRequest
    { operationId=operationId
    , instanceId=inst.uuid
    , stepId=stepId, snapshotId=snapId
    , expectedSnapshot=snap.uuid, expectedRevision=inst.revision
    , sourceKey=sourceKey, targetKey=targetKey
    , mappingName=spec.mappingName
    , fromState=snap.state, toState=destRef, toPath=destState.path
    , oldCtx=snap.ctx, newCtx=toMachine.spec.codecs.ctx.encode new
    , newWait=waitVal, newChild=childVal
    , nextStatus=statusVal, createdAt=now }
