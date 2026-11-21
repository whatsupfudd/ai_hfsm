{-# LANGUAGE QuasiQuotes #-}

-- | Hasql-TH boundary for CT-06. Every mutating operation is executed in its
-- own transaction; queue claim functions hold the global scheduler gate.
module Hfsm.Operations.Postgres.Statements where

import Data.Aeson (Value)
import Data.Int (Int16, Int32, Int64)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Hasql.Statement (Statement)
import Hasql.TH (singletonStatement, vectorStatement, resultlessStatement, maybeStatement)

registerWorker :: Statement (UUID, Vector Text, Int32, Int32, Int32) UUID
registerWorker = [singletonStatement|
 SELECT hfsm.ops_register_worker($1 :: uuid,$2 :: text[],$3 :: int4,$4 :: int4,$5 :: int4) :: uuid
|]

heartbeatWorker :: Statement (UUID, UUID) Bool
heartbeatWorker = [singletonStatement|
 SELECT hfsm.ops_heartbeat_worker($1 :: uuid,$2 :: uuid) :: bool
|]


setWorkerState :: Statement (UUID, UUID, Text) (Maybe Bool)
setWorkerState = [maybeStatement|
 UPDATE hfsm.worker_registry SET state=$3 :: text
 WHERE id=$1 :: uuid AND generation=$2 :: uuid
 RETURNING true :: bool
|]


claimSignals :: Statement (UUID, Int32, Int32, Maybe UUID) (Vector Value)
claimSignals = [vectorStatement|
 SELECT item :: jsonb FROM hfsm.ops_claim_signals($1 :: uuid,$2 :: int4,$3 :: int4,$4 :: uuid?) item
|]


claimActivities :: Statement (UUID, Int32, Int32, Maybe UUID, Vector Text, Vector Text) (Vector Value)
claimActivities = [vectorStatement|
 SELECT item :: jsonb FROM hfsm.ops_claim_activities(
   $1 :: uuid,$2 :: int4,$3 :: int4,$4 :: uuid?,$5 :: text[],$6 :: text[]) item
|]


enqueueManaged :: Statement
  (UUID, UUID, Value, Maybe Value, Maybe Text, Int16,
   Maybe UUID, Maybe UUID, Maybe Text, Maybe Text, UTCTime) UUID
enqueueManaged = [singletonStatement|
 SELECT hfsm.ops_enqueue_signal(
   $1 :: uuid,$2 :: uuid,$3 :: jsonb,$4 :: jsonb?,$5 :: text?,
   $6 :: int2,$7 :: uuid?,$8 :: uuid?,$9 :: text?,$10 :: text?,$11 :: timestamptz) :: uuid
|]


cancelInstance :: Statement (UUID, Int64, UUID, Text, Text) Text
cancelInstance = [singletonStatement|
 SELECT hfsm.ops_cancel_instance($1 :: uuid,$2 :: int8,$3 :: uuid,$4 :: text,$5 :: text) :: text
|]


createTenant :: Statement (UUID, Text, Int32, Int32, Int32, Int32, Int32) ()
createTenant = [resultlessStatement|
 INSERT INTO hfsm.tenant (id,tenant_key,max_active_instances,max_pending_signals,
   max_signal_inflight,max_activity_inflight,max_signal_attempts)
 VALUES($1 :: uuid,$2 :: text,$3 :: int4,$4 :: int4,$5 :: int4,$6 :: int4,$7 :: int4)
|]


createDefaultGroup :: Statement UUID ()
createDefaultGroup = [resultlessStatement|
 INSERT INTO hfsm.concurrency_group (tenant_id,group_key)
 VALUES($1 :: uuid,'default') ON CONFLICT DO NOTHING
|]


setLocalConfig :: Statement (Text,Text) Text
setLocalConfig = [singletonStatement|
 SELECT set_config($1 :: text,$2 :: text,true) :: text
|]


projectInstance :: Statement (Text,UUID,Int32) Int32
projectInstance = [singletonStatement|
 SELECT hfsm.ops_project_instance($1 :: text,$2 :: uuid,$3 :: int4) :: int4
|]


resetProjection :: Statement (Text,UUID) Int32
resetProjection = [singletonStatement|
 SELECT hfsm.ops_reset_projection($1 :: text,$2 :: uuid) :: int4
|]


readDeadLetters :: Statement (UUID,Int32) (Vector Value)
readDeadLetters = [vectorStatement|
 SELECT jsonb_build_object(
 'id',d.id,'tenantId',d.tenant_id,'instanceId',d.instance_id,
 'sourceKind',d.source_kind,'sourceId',d.source_id,'reason',d.reason,
 'classification',d.classification,'payload',d.payload,'recordedAt',d.recorded_at) :: jsonb
 FROM hfsm.dead_letter d WHERE d.tenant_id=$1 :: uuid
 ORDER BY d.recorded_at DESC,d.id DESC LIMIT $2 :: int4
|]


readActive :: Statement (Text,UUID) (Maybe Value)
readActive = [maybeStatement|
 SELECT payload :: jsonb FROM hfsm.readmodel_active
 WHERE consumer=$1 :: text AND instance_id=$2 :: uuid
|]

projectCandidates :: Statement (Text,Int32) (Vector UUID)
projectCandidates = [vectorStatement|
 SELECT i.id :: uuid FROM hfsm.instance i
 LEFT JOIN hfsm.projection_progress p ON p.consumer=$1 :: text AND p.instance_id=i.id
 LEFT JOIN hfsm.readmodel_active v ON v.consumer=$1 :: text AND v.instance_id=i.id
 WHERE i.revision > coalesce(p.last_revision,0)
    OR (i.status='cancelled' AND v.status IS DISTINCT FROM 'cancelled'
        AND i.revision>0)
 ORDER BY i.updated_at,i.uid LIMIT $2 :: int4
|]


readEffectTraceparent :: Statement UUID (Maybe Text)
readEffectTraceparent = [singletonStatement|
 SELECT traceparent :: text? FROM hfsm.outbox WHERE id=$1 :: uuid
|]


redriveSignal :: Statement (Int64,UUID,UUID,Text,Text,Maybe Text) UUID
redriveSignal = [singletonStatement|
 SELECT hfsm.ops_redrive_signal($1 :: int8,$2 :: uuid,$3 :: uuid,
   $4 :: text,$5 :: text,$6 :: text?) :: uuid
|]


setGroupLimits :: Statement (UUID,Text,Int32,Int32) Bool
setGroupLimits = [singletonStatement|
 SELECT hfsm.ops_set_group_limits($1 :: uuid,$2 :: text,$3 :: int4,$4 :: int4) :: bool
|]


readSignalTraceparent :: Statement UUID (Maybe Text)
readSignalTraceparent = [singletonStatement|
 SELECT traceparent :: text? FROM hfsm.signal WHERE id=$1 :: uuid
|]


readEffectCorrelation :: Statement UUID (Maybe UUID, Maybe Text)
readEffectCorrelation = [singletonStatement|
 SELECT correlation_id :: uuid?, traceparent :: text?
 FROM hfsm.outbox WHERE id=$1 :: uuid
|]


tenantMetrics :: Statement UUID Value
tenantMetrics = [singletonStatement|
 SELECT jsonb_build_object(
   'tenant',t.id,'enabled',t.enabled,
   'instances',(SELECT count(*)::int8 FROM hfsm.instance i WHERE i.tenant_id=t.id),
   'activeInstances',(SELECT count(*)::int8 FROM hfsm.instance i WHERE i.tenant_id=t.id
     AND i.status IN ('running','waiting','paused')),
   'pendingSignals',(SELECT count(*)::int8 FROM hfsm.signal s WHERE s.tenant_id=t.id
     AND s.status='entered'),
   'leasedSignals',(SELECT count(*)::int8 FROM hfsm.signal s WHERE s.tenant_id=t.id
     AND s.status='leased' AND s.lease_until>clock_timestamp()),
   'readyEffects',(SELECT count(*)::int8 FROM hfsm.outbox o WHERE o.tenant_id=t.id
     AND o.status='ready'),
   'leasedEffects',(SELECT count(*)::int8 FROM hfsm.outbox o WHERE o.tenant_id=t.id
     AND o.status='leased' AND o.lease_until>clock_timestamp()),
   'deadLetters',(SELECT count(*)::int8 FROM hfsm.dead_letter d WHERE d.tenant_id=t.id),
   'unknownEffects',(SELECT count(*)::int8 FROM hfsm.outbox o WHERE o.tenant_id=t.id
     AND o.failure_class='unknown')
 ) :: jsonb FROM hfsm.tenant t WHERE t.id=$1 :: uuid
|]
