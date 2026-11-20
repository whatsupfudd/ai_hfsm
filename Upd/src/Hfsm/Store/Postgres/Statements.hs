{-# LANGUAGE QuasiQuotes #-}

-- | Prepared SQL for CT-02A.  All statements are compiled by Hasql.TH;
-- the Haskell adapter owns transaction sequencing, not stored procedures.
-- PostgreSQL 17+ with migrations/001_hfsm_runtime.sql installed.
module Hfsm.Store.Postgres.Statements where

import Data.Aeson (Value)
import Data.Int (Int32, Int64)
import Data.Text (Text)
import Data.Time (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement, singletonStatement, vectorStatement, resultlessStatement)

-- Read row projections deliberately retain the exact property names used by
-- the existing CT-01 runtime models.  This also allows initial adoption without
-- introducing a second set of row types into the engine.
loadInstance :: Statement UUID (Maybe Value)
loadInstance = [maybeStatement|
  SELECT jsonb_build_object(
    'uid', i.uid, 'uuid', i.id, 'machine', i.machine_name,
    'version', i.machine_version, 'digest', i.spec_digest,
    'status', i.status, 'state', i.current_state_ref,
    'path', i.current_path, 'snap', i.current_snapshot_id,
    'revision', i.revision, 'nextSignalOrdinal', i.next_signal_ordinal,
    'parent', i.parent_instance_id, 'createdAt', i.created_at,
    'updatedAt', i.updated_at
  ) :: jsonb
  FROM hfsm.instance AS i WHERE i.id = $1 :: uuid
|]

loadSnapshot :: Statement UUID (Maybe Value)
loadSnapshot = [maybeStatement|
  SELECT jsonb_build_object(
    'uid', s.uid, 'uuid', s.id, 'inst', s.instance_id,
    'state', s.state_ref, 'path', s.state_path,
    'ctx', s.ctx, 'wait', s.wait_value, 'child', s.child_value,
    'version', s.machine_version, 'digest', s.spec_digest,
    'createdAt', s.created_at
  ) :: jsonb
  FROM hfsm.snapshot AS s WHERE s.id = $1 :: uuid
|]

-- SignalStatus uses derived Aeson constructor names (e.g. LeasedSs), unlike
-- InstanceStatus which defines a custom lower-case JSON instance.
listSignals :: Statement (Vector Text, Int32) (Vector Value)
listSignals = [vectorStatement|
  SELECT jsonb_build_object(
    'uid', s.uid, 'uuid', s.id, 'inst', s.instance_id,
    'status', CASE s.status
       WHEN 'entered' THEN 'EnteredSs' WHEN 'leased' THEN 'LeasedSs'
       WHEN 'applied' THEN 'AppliedSs' WHEN 'rejected' THEN 'RejectedSs'
       ELSE 'FailedSs' END,
    'payload', s.payload, 'cause', s.cause, 'lease', s.lease_id,
    'ordinal', s.ordinal, 'leaseOwner', s.lease_owner,
    'leaseUntil', s.lease_until, 'attempts', s.attempts, 'readyAt', s.ready_at,
    'createdAt', s.created_at, 'updatedAt', s.updated_at
  ) :: jsonb
  FROM hfsm.signal s
  WHERE s.status = ANY($1 :: text[])
  ORDER BY s.uid LIMIT $2 :: int4
|]

-- A batch claim uses one DB transaction. All candidate *instance* rows are
-- locked before the corresponding signal rows are modified. The LATERAL query
-- always picks the oldest non-terminal signal; it never skips a live lease.
-- This statement sets a NEW gen_random_uuid fencing token on every reclaim.
claimSignals :: Statement (Int32, UUID, Int32) (Vector Value)
claimSignals = [vectorStatement|
  WITH candidates AS MATERIALIZED (
    SELECT i.id AS instance_id, oldest.id AS signal_id
    FROM hfsm.instance AS i
    JOIN LATERAL (
      SELECT s.id, s.status, s.ready_at, s.lease_until
      FROM hfsm.signal s
      WHERE s.instance_id = i.id AND s.status IN ('entered','leased')
      ORDER BY s.ordinal ASC LIMIT 1
    ) AS oldest ON true
    WHERE i.status IN ('running','waiting','paused')
      AND (i.in_flight_signal_id IS NULL OR i.in_flight_lease_until <= clock_timestamp())
      AND ((oldest.status = 'entered' AND oldest.ready_at <= clock_timestamp())
           OR (oldest.status = 'leased' AND oldest.lease_until <= clock_timestamp()))
    ORDER BY i.updated_at, i.uid
    LIMIT $1 :: int4
    FOR UPDATE OF i SKIP LOCKED
  ), leased AS (
    UPDATE hfsm.signal AS s SET
      status = 'leased', lease_id = gen_random_uuid(),
      lease_owner = $2 :: uuid,
      lease_until = clock_timestamp() + ($3 :: int4 * interval '1 second'),
      attempts = s.attempts + 1, updated_at = clock_timestamp()
    FROM candidates c WHERE s.id = c.signal_id
    RETURNING s.id, s.instance_id, s.lease_id, s.lease_owner, s.lease_until,
              s.ordinal, s.attempts, s.ready_at, s.uid, s.payload, s.cause,
              s.created_at, s.updated_at
  ), admitted AS (
    UPDATE hfsm.instance AS i SET
      in_flight_signal_id = l.id, in_flight_lease_id = l.lease_id,
      in_flight_lease_until = l.lease_until
    FROM leased AS l WHERE i.id = l.instance_id
    RETURNING l.id, l.instance_id, l.lease_id, l.lease_owner, l.lease_until,
              l.ordinal, l.attempts, l.ready_at, l.uid,
              l.payload, l.cause, l.created_at, l.updated_at
  )
  SELECT jsonb_build_object(
    'uid', a.uid, 'uuid', a.id, 'inst', a.instance_id,
    'status', 'LeasedSs', 'payload', a.payload, 'cause', a.cause,
    'lease', a.lease_id, 'ordinal', a.ordinal,
    'leaseOwner', a.lease_owner, 'leaseUntil', a.lease_until,
    'attempts', a.attempts, 'readyAt', a.ready_at,
    'createdAt', a.created_at, 'updatedAt', a.updated_at
  ) :: jsonb FROM admitted AS a
|]

-- Both row locks are acquired in this order throughout the adapter.
lockInstance :: Statement UUID (Maybe (UUID, Int64, Maybe UUID, Maybe UUID, Maybe UTCTime, Text, Text, Text, Int64, Value, Text))
lockInstance = [maybeStatement|
  SELECT current_snapshot_id :: uuid, revision :: int8,
         in_flight_signal_id :: uuid?, in_flight_lease_id :: uuid?,
         in_flight_lease_until :: timestamptz?,
         machine_name :: text, machine_version :: text, spec_digest :: text,
         current_state_ref :: int8, current_path :: jsonb, status :: text
  FROM hfsm.instance WHERE id = $1 :: uuid FOR UPDATE
|]

lockSignal :: Statement UUID (Maybe (UUID, Text, Maybe UUID, Maybe UTCTime, Int64))
lockSignal = [maybeStatement|
  SELECT instance_id :: uuid, status :: text, lease_id :: uuid?,
         lease_until :: timestamptz?, ordinal :: int8
  FROM hfsm.signal WHERE id = $1 :: uuid FOR UPDATE
|]

serverNow :: Statement () UTCTime
serverNow = [singletonStatement| SELECT clock_timestamp() :: timestamptz |]

insertStep :: Statement (UUID, UUID, Int64, UUID, UUID, Text, Int64, Int64, Maybe Int64, Int64, Value, UTCTime) Int64
insertStep = [singletonStatement|
  INSERT INTO hfsm.step
    (id, instance_id, revision, signal_id, source_snapshot_id,
     kind, from_state_ref, to_state_ref, case_ref, route_ref, note, created_at)
  VALUES
    ($1 :: uuid, $2 :: uuid, $3 :: int8, $4 :: uuid, $5 :: uuid,
     $6 :: text, $7 :: int8, $8 :: int8, $9 :: int8?, $10 :: int8,
     $11 :: jsonb, $12 :: timestamptz)
  RETURNING uid :: int8
|]

insertTurnSnapshot :: Statement (UUID, UUID, Int64, UUID, Int64, Value, Value, Maybe Value, Value, Text, Text, UTCTime) Int64
insertTurnSnapshot = [singletonStatement|
  INSERT INTO hfsm.snapshot
    (id, instance_id, revision, produced_by_step_id, state_ref,
     state_path, ctx, wait_value, child_value, machine_version,
     spec_digest, created_at)
  VALUES
    ($1 :: uuid, $2 :: uuid, $3 :: int8, $4 :: uuid, $5 :: int8,
     $6 :: jsonb, $7 :: jsonb, $8 :: jsonb?, $9 :: jsonb,
     $10 :: text, $11 :: text, $12 :: timestamptz)
  RETURNING uid :: int8
|]

-- Each side-record family is inserted in one prepared statement using a
-- JSONB array, preserving list ordinal and avoiding N round-trips per turn.
insertOutboxBatch :: Statement (UUID, UUID, UTCTime, Value) ()
insertOutboxBatch = [resultlessStatement|
  INSERT INTO hfsm.outbox
    (instance_id, step_id, ordinal, effect_key, status, payload,
     created_at, updated_at)
  SELECT $1 :: uuid, $2 :: uuid, (e.ordinal - 1)::integer,
         ($2 :: uuid)::text || ':' || (e.ordinal - 1)::text,
         'ready', e.item, $3 :: timestamptz, $3 :: timestamptz
  FROM jsonb_array_elements($4 :: jsonb) WITH ORDINALITY AS e(item, ordinal)
|]

insertChildBatch :: Statement (UUID, UUID, UTCTime, Value) ()
insertChildBatch = [resultlessStatement|
  INSERT INTO hfsm.child_run
    (parent_instance_id, child_instance_id, created_by_step_id, ordinal,
     child_key, status, join_ref, created_at, updated_at)
  SELECT $1 :: uuid, (e.item->>'child')::uuid, $2 :: uuid,
         (e.ordinal - 1)::integer, e.item->>'key',
         e.item->>'status', (e.item->>'join')::bigint,
         $3 :: timestamptz, $3 :: timestamptz
  FROM jsonb_array_elements($4 :: jsonb) WITH ORDINALITY AS e(item, ordinal)
|]

insertBreakBatch :: Statement (UUID, UUID, UTCTime, Value) ()
insertBreakBatch = [resultlessStatement|
  INSERT INTO hfsm.break_event
    (instance_id, step_id, ordinal, kind, state_ref, status,
     note, created_at, updated_at)
  SELECT $1 :: uuid, $2 :: uuid, (e.ordinal - 1)::integer,
         e.item->>'kind', (e.item->>'state')::bigint,
         e.item->>'status', e.item->>'note',
         $3 :: timestamptz, $3 :: timestamptz
  FROM jsonb_array_elements($4 :: jsonb) WITH ORDINALITY AS e(item, ordinal)
|]

insertProjectionBatch :: Statement (UUID, UUID, Int64, Value) ()
insertProjectionBatch = [resultlessStatement|
  INSERT INTO hfsm.projection_event
    (instance_id, step_id, revision, kind, ordinal, payload)
  SELECT $1 :: uuid, $2 :: uuid, $3 :: int8, e.item->>'kind',
         (e.item->>'ordinal')::integer, e.item->'payload'
  FROM jsonb_array_elements($4 :: jsonb) AS e(item)
|]

advanceInstance :: Statement (UUID, UUID, Int64, UUID, UUID, Int64, Value, Text, UUID) (Maybe Int64)
advanceInstance = [maybeStatement|
  UPDATE hfsm.instance SET
    current_snapshot_id = $4 :: uuid,
    current_state_ref = $6 :: int8, current_path = $7 :: jsonb,
    revision = revision + 1, status = $8 :: text,
    in_flight_signal_id = NULL, in_flight_lease_id = NULL,
    in_flight_lease_until = NULL, updated_at = clock_timestamp()
  WHERE id = $1 :: uuid AND current_snapshot_id = $2 :: uuid
    AND revision = $3 :: int8 AND in_flight_signal_id = $5 :: uuid
    AND in_flight_lease_id = $9 :: uuid
    AND in_flight_lease_until > clock_timestamp()
  RETURNING revision :: int8
|]

applySignal :: Statement (UUID, UUID) (Maybe UUID)
applySignal = [maybeStatement|
  UPDATE hfsm.signal SET
    status = 'applied', lease_id = NULL, lease_owner = NULL,
    lease_until = NULL, updated_at = clock_timestamp()
  WHERE id = $1 :: uuid AND status = 'leased'
    AND lease_id = $2 :: uuid AND lease_until > clock_timestamp()
  RETURNING id :: uuid
|]

releaseSignal :: Statement (UUID, UUID, Text, Maybe Text) (Maybe UUID)
releaseSignal = [maybeStatement|
  UPDATE hfsm.signal SET
    status = $3 :: text, lease_id = NULL, lease_owner = NULL,
    lease_until = NULL, last_error = $4 :: text?,
    ready_at = clock_timestamp(), updated_at = clock_timestamp()
  WHERE id = $1 :: uuid AND status = 'leased'
    AND lease_id = $2 :: uuid AND lease_until > clock_timestamp()
  RETURNING id :: uuid
|]

clearAdmission :: Statement (UUID, UUID, UUID) (Maybe UUID)
clearAdmission = [maybeStatement|
  UPDATE hfsm.instance SET
    in_flight_signal_id = NULL, in_flight_lease_id = NULL,
    in_flight_lease_until = NULL
  WHERE id = $1 :: uuid AND in_flight_signal_id = $2 :: uuid
    AND in_flight_lease_id = $3 :: uuid AND in_flight_lease_until > clock_timestamp()
  RETURNING id :: uuid
|]

registerRelease :: Statement (Text, Text, Text) ()
registerRelease = [resultlessStatement|
  INSERT INTO hfsm.machine_release (machine_name, machine_version, spec_digest)
  VALUES ($1 :: text, $2 :: text, $3 :: text)
  ON CONFLICT (machine_name, machine_version, spec_digest) DO NOTHING
|]

insertInstance :: Statement (UUID, Text, Text, Text, Text, Int64, Value, UUID, Maybe UUID, UTCTime) Int64
insertInstance = [singletonStatement|
  INSERT INTO hfsm.instance
    (id, machine_name, machine_version, spec_digest, status,
     current_state_ref, current_path, current_snapshot_id,
     parent_instance_id, revision, next_signal_ordinal, created_at, updated_at)
  VALUES
    ($1 :: uuid, $2 :: text, $3 :: text, $4 :: text, $5 :: text,
     $6 :: int8, $7 :: jsonb, $8 :: uuid,
     $9 :: uuid?, 0, 1, $10 :: timestamptz, $10 :: timestamptz)
  RETURNING uid :: int8
|]

insertInitialSnapshot :: Statement (UUID, UUID, Int64, Value, Value, Maybe Value, Value, Text, Text, UTCTime) Int64
insertInitialSnapshot = [singletonStatement|
  INSERT INTO hfsm.snapshot
    (id, instance_id, revision, produced_by_step_id, state_ref,
     state_path, ctx, wait_value, child_value, machine_version,
     spec_digest, created_at)
  VALUES ($1 :: uuid, $2 :: uuid, 0, NULL, $3 :: int8,
          $4 :: jsonb, $5 :: jsonb, $6 :: jsonb?, $7 :: jsonb,
          $8 :: text, $9 :: text, $10 :: timestamptz)
  RETURNING uid :: int8
|]

lookupDedupe :: Statement (UUID, Text) (Maybe UUID)
lookupDedupe = [maybeStatement|
  SELECT id :: uuid FROM hfsm.signal
  WHERE instance_id = $1 :: uuid AND dedupe_key = $2 :: text
|]

advanceSignalOrdinal :: Statement UUID (Maybe Int64)
advanceSignalOrdinal = [maybeStatement|
  UPDATE hfsm.instance SET next_signal_ordinal = next_signal_ordinal + 1
  WHERE id = $1 :: uuid RETURNING (next_signal_ordinal - 1) :: int8
|]

insertSignal :: Statement (UUID, UUID, Int64, Value, Maybe Value, Maybe Text, UTCTime) ()
insertSignal = [resultlessStatement|
  INSERT INTO hfsm.signal
    (id, instance_id, ordinal, status, payload, cause, dedupe_key, ready_at)
  VALUES ($1 :: uuid, $2 :: uuid, $3 :: int8, 'entered', $4 :: jsonb,
          $5 :: jsonb?, $6 :: text?, $7 :: timestamptz)
|]

-- Non-locking discovery is safe because a signal's instance_id is immutable.
-- ACK subsequently locks instance first and signal second.
lookupSignalParent :: Statement UUID (Maybe UUID)
lookupSignalParent = [maybeStatement|
  SELECT instance_id :: uuid FROM hfsm.signal WHERE id = $1 :: uuid
|]

-- CT-03: atomic lease renewal; caller holds instance+signal row locks.
-- Each update is fenced and rejects expiration at the database server.
renewSignalLease :: Statement (UUID, UUID, UTCTime) (Maybe UTCTime)
renewSignalLease = [maybeStatement|
  UPDATE hfsm.signal SET
    lease_until = GREATEST(lease_until, $3 :: timestamptz),
    updated_at = clock_timestamp()
  WHERE id = $1 :: uuid AND status = 'leased'
    AND lease_id = $2 :: uuid AND lease_until > clock_timestamp()
  RETURNING lease_until :: timestamptz
|]

renewInstanceLease :: Statement (UUID, UUID, UUID, UTCTime) (Maybe UTCTime)
renewInstanceLease = [maybeStatement|
  UPDATE hfsm.instance SET
    in_flight_lease_until = GREATEST(in_flight_lease_until, $4 :: timestamptz)
  WHERE id = $1 :: uuid AND in_flight_signal_id = $2 :: uuid
    AND in_flight_lease_id = $3 :: uuid
    AND in_flight_lease_until > clock_timestamp()
  RETURNING in_flight_lease_until :: timestamptz
|]
