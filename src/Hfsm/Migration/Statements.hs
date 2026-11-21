{-# LANGUAGE QuasiQuotes #-}

-- | All migration writes occur in one transaction under instance row lock.
module Hfsm.Migration.Statements where

import Data.Aeson (Value)
import Data.Int (Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement, singletonStatement, resultlessStatement)

lookupOperation :: Statement UUID
  (Maybe (UUID,Int64,UUID,UUID,Text,Text,Text))
lookupOperation = [maybeStatement|
 SELECT instance_id :: uuid, revision :: int8,
        step_id :: uuid, target_snapshot_id :: uuid,
        (target_key->>'version') :: text,
        (target_key->>'digest') :: text, mapping_name :: text
 FROM hfsm.migration_event WHERE operation_id=$1 :: uuid
|]

-- | A migration must not reinterpret an already queued signal or effect using
-- a new codec/graph. Caller must drain pending work before requesting it.
pendingWork :: Statement UUID (Int64,Int64,Int64)
pendingWork = [singletonStatement|
 SELECT
  (SELECT count(*)::int8 FROM hfsm.signal s
   WHERE s.instance_id=$1 :: uuid AND s.status IN ('entered','leased')) :: int8,
  (SELECT count(*)::int8 FROM hfsm.outbox o
   WHERE o.instance_id=$1 :: uuid AND o.status <> 'done') :: int8,
  (SELECT count(*)::int8 FROM hfsm.child_run c
   WHERE c.parent_instance_id=$1 :: uuid
     AND c.status IN ('spawned','running')) :: int8
|]

-- | Dedicated synthetic step, with no signal and reserved route_ref = 0.
-- Ordinary insertStep requires a non-null signal UUID, so cannot be reused.
insertMigrationStep :: Statement
 (UUID,UUID,Int64,UUID,Int64,Int64,Value,UTCTime) Int64
insertMigrationStep = [singletonStatement|
 INSERT INTO hfsm.step
   (id,instance_id,revision,signal_id,source_snapshot_id,kind,
    from_state_ref,to_state_ref,case_ref,route_ref,note,created_at)
 VALUES
   ($1 :: uuid,$2 :: uuid,$3 :: int8,NULL,$4 :: uuid,'migrate',
    $5 :: int8,$6 :: int8,NULL,0,$7 :: jsonb,$8 :: timestamptz)
 RETURNING uid :: int8
|]

-- | CAS includes current revision and snapshot, machine release identity,
-- absent admission lease and no eligible outstanding work.
advanceMigration :: Statement
 (UUID,UUID,Int64,Text,Text,Text,Text,UUID,Int64,Value,Text)
 (Maybe Int64)
advanceMigration = [maybeStatement|
 UPDATE hfsm.instance i SET
   machine_version=$6 :: text, spec_digest=$7 :: text,
   current_snapshot_id=$8 :: uuid,
   current_state_ref=$9 :: int8, current_path=$10 :: jsonb,
   status=$11 :: text,
   revision=i.revision+1,updated_at=clock_timestamp()
 WHERE i.id=$1 :: uuid AND i.current_snapshot_id=$2 :: uuid
   AND i.revision=$3 :: int8
   AND i.machine_version=$4 :: text AND i.spec_digest=$5 :: text
   AND i.in_flight_signal_id IS NULL
   AND i.status IN ('running','waiting','paused')
   AND NOT EXISTS (SELECT 1 FROM hfsm.signal s
     WHERE s.instance_id=i.id AND s.status IN ('entered','leased'))
   AND NOT EXISTS (SELECT 1 FROM hfsm.outbox o
     WHERE o.instance_id=i.id AND o.status <> 'done')
   AND NOT EXISTS (SELECT 1 FROM hfsm.child_run c
     WHERE c.parent_instance_id=i.id AND c.status IN ('spawned','running'))
 RETURNING revision :: int8
|]

insertEvent :: Statement
 (UUID,UUID,Int64,UUID,UUID,UUID,Value,Value,Text,Value,Value,UTCTime) ()
insertEvent = [resultlessStatement|
 INSERT INTO hfsm.migration_event
   (operation_id,instance_id,revision,step_id,
    source_snapshot_id,target_snapshot_id,source_key,target_key,
    mapping_name,old_ctx,new_ctx,created_at)
 VALUES
   ($1 :: uuid,$2 :: uuid,$3 :: int8,$4 :: uuid,
    $5 :: uuid,$6 :: uuid,$7 :: jsonb,$8 :: jsonb,
    $9 :: text,$10 :: jsonb,$11 :: jsonb,$12 :: timestamptz)
|]

insertProjections :: Statement (UUID,UUID,Int64,Value,Value) ()
insertProjections = [resultlessStatement|
 INSERT INTO hfsm.projection_event
   (instance_id,step_id,revision,kind,ordinal,payload)
 VALUES
   ($1 :: uuid,$2 :: uuid,$3 :: int8,'active',0,$4 :: jsonb),
   ($1 :: uuid,$2 :: uuid,$3 :: int8,'trace',0,$5 :: jsonb)
|]
