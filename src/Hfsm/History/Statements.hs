{-# LANGUAGE QuasiQuotes #-}

-- | CT-05 read-only history queries. Per-instance revision order is authoritative.
module Hfsm.History.Statements where

import Data.Aeson (Value)
import Data.Int (Int32, Int64)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Hasql.Statement (Statement)
import Hasql.TH (vectorStatement, maybeStatement)

-- | Paginated, immutable revision history, including migration steps.
-- Never paginate with global sequence UIDs (commit order is not UID order).
loadTurns :: Statement (UUID, Int64, Int32) (Vector Value)
loadTurns = [vectorStatement|
 SELECT jsonb_build_object(
   'revision', t.revision,
   'sourceSnapshotId', t.source_snapshot_id,
   'step', jsonb_build_object(
     'uid',t.uid,'uuid',t.id,'inst',t.instance_id,
     'signal',t.signal_id,'kind',t.kind,
     'from',t.from_state_ref,'to',t.to_state_ref,
     'caseRef',t.case_ref,'route',t.route_ref,
     'note',t.note,'createdAt',t.created_at),
   'snapshot', jsonb_build_object(
     'uid',s.uid,'uuid',s.id,'inst',s.instance_id,
     'state',s.state_ref,'path',s.state_path,
     'ctx',s.ctx,'wait',s.wait_value,'child',s.child_value,
     'version',s.machine_version,'digest',s.spec_digest,
     'createdAt',s.created_at),
   'input', ri.input,
   'outbox', (SELECT coalesce(jsonb_agg(
       jsonb_build_object('effectKey',o.effect_key,'payload',o.payload)
       ORDER BY o.ordinal),'[]'::jsonb)
     FROM hfsm.outbox o WHERE o.step_id=t.id),
   'children',(SELECT coalesce(jsonb_agg(
       jsonb_build_object('child',c.child_instance_id,
                          'key',c.child_key,'join',c.join_ref)
       ORDER BY c.ordinal),'[]'::jsonb)
     FROM hfsm.child_run c WHERE c.created_by_step_id=t.id),
   'breaks',(SELECT coalesce(jsonb_agg(
       jsonb_build_object('kind',b.kind,'state',b.state_ref,'note',b.note)
       ORDER BY b.ordinal),'[]'::jsonb)
     FROM hfsm.break_event b WHERE b.step_id=t.id),
   'proj', jsonb_build_object(
     'active', (SELECT coalesce(jsonb_agg(p.payload ORDER BY p.ordinal),'[]'::jsonb)
                  FROM hfsm.projection_event p WHERE p.step_id=t.id AND p.kind='active'),
     'trace', (SELECT coalesce(jsonb_agg(p.payload ORDER BY p.ordinal),'[]'::jsonb)
                  FROM hfsm.projection_event p WHERE p.step_id=t.id AND p.kind='trace'),
     'queue', (SELECT coalesce(jsonb_agg(p.payload ORDER BY p.ordinal),'[]'::jsonb)
                  FROM hfsm.projection_event p WHERE p.step_id=t.id AND p.kind='queue'),
     'tree', (SELECT coalesce(jsonb_agg(p.payload ORDER BY p.ordinal),'[]'::jsonb)
                  FROM hfsm.projection_event p WHERE p.step_id=t.id AND p.kind='tree')
   ),
   'migration',(SELECT jsonb_build_object(
      'operationId',m.operation_id,'sourceKey',m.source_key,
      'targetKey',m.target_key,'mappingName',m.mapping_name,
      'sourceSnapshotId',m.source_snapshot_id,
      'targetSnapshotId',m.target_snapshot_id,
      'oldCtx',m.old_ctx,'newCtx',m.new_ctx,'createdAt',m.created_at)
      FROM hfsm.migration_event m WHERE m.step_id=t.id)
 ) :: jsonb
 FROM hfsm.step t
 JOIN hfsm.snapshot s ON s.produced_by_step_id=t.id AND s.instance_id=t.instance_id
 LEFT JOIN hfsm.turn_input ri ON ri.step_id=t.id
 WHERE t.instance_id=$1 :: uuid AND t.revision >= $2 :: int8
 ORDER BY t.revision
 LIMIT $3 :: int4
|]

snapshotAt :: Statement (UUID, Int64) (Maybe Value)
snapshotAt = [maybeStatement|
 SELECT jsonb_build_object(
   'uid',s.uid,'uuid',s.id,'inst',s.instance_id,
   'state',s.state_ref,'path',s.state_path,
   'ctx',s.ctx,'wait',s.wait_value,'child',s.child_value,
   'version',s.machine_version,'digest',s.spec_digest,
   'createdAt',s.created_at) :: jsonb
 FROM hfsm.snapshot s
 WHERE s.instance_id=$1 :: uuid AND s.revision=$2 :: int8
|]
