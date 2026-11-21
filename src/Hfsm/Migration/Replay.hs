{-# LANGUAGE NoFieldSelectors #-}

-- | Optional replay of a named migration function against its immutable
-- before/after snapshots. No database writes or provider effects.
module Hfsm.Migration.Replay (verifyMigrationTransform) where

import Data.Bifunctor (first)
import qualified Data.IntMap.Strict as IM
import qualified Data.Text as T
import Hfsm.Compile (CompiledMachine(..))
import Hfsm.Core.Codec (Codec(..), CodecSet(..))
import Hfsm.Core.Ref (stateRefWord32)
import Hfsm.Graph.Def (MachineGraph(..), StateNd(..))
import Hfsm.History.Types (MigrationEvidence(..))
import Hfsm.Migration.Types (MigrationSpec(..), MigrationErr(..))
import Hfsm.Runtime.Model (SnapshotRw(..))
import Hfsm.Runtime.Registry (machineKeyOf)
import Hfsm.Spec.Def (MachineSpec(..))

verifyMigrationTransform
  :: CompiledMachine st1 sg1 hf1 oldCtx cmd1 child1
  -> CompiledMachine st2 sg2 hf2 newCtx cmd2 child2
  -> MigrationSpec oldCtx newCtx
  -> SnapshotRw -> SnapshotRw -> MigrationEvidence
  -> Either MigrationErr ()
verifyMigrationTransform source target spec before after evidence = do
  let failWith = Left . MigrationPlanError
  if evidence.sourceKey /= machineKeyOf source ||
     evidence.targetKey /= machineKeyOf target ||
     evidence.mappingName /= spec.mappingName
    then failWith "migration implementation/release identity differs"
    else pure ()
  if before.uuid /= evidence.sourceSnapshotId ||
     after.uuid /= evidence.targetSnapshotId ||
     before.ctx /= evidence.oldCtx ||
     after.ctx /= evidence.newCtx
    then failWith "immutable migration evidence/snapshot mismatch"
    else pure ()
  sourceCtx <- first (MigrationPlanError . T.pack . show) $
    source.spec.codecs.ctx.decode before.ctx
  targetCtx <- first MigrationPlanError (spec.mapContext sourceCtx)
  let producedCtx = target.spec.codecs.ctx.encode targetCtx
  if producedCtx /= after.ctx
    then failWith "replayed context transformation diverged" else pure ()
  targetState <- first MigrationPlanError (spec.mapControl before.state)
  state <- maybe (failWith "transformed state missing from target graph") Right $
    IM.lookup (fromIntegral (stateRefWord32 targetState)) target.graph.states
  if after.state /= targetState || after.path /= state.path
    then failWith "replayed graph/control migration diverged" else pure ()
  waitVal <- first MigrationPlanError (spec.mapWait before.wait)
  childVal <- first MigrationPlanError (spec.mapChild before.child)
  if waitVal /= after.wait || childVal /= after.child
    then failWith "replayed wait/child transformation diverged"
    else pure ()
