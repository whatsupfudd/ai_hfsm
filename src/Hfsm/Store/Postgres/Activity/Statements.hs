{-# LANGUAGE QuasiQuotes #-}

-- | CT-04 prepared SQL, compiled via Hasql-TH.
-- Requires Support/002_hfsm_activity.sql.
module Hfsm.Store.Postgres.Activity.Statements where

import Data.Aeson (Value)
import Data.Int (Int32)
import Data.Text (Text)
import Data.Time.Clock (UTCTime)
import Data.UUID (UUID)
import Data.Vector (Vector)
import Hasql.Statement (Statement)
import Hasql.TH (maybeStatement, vectorStatement, resultlessStatement)

-- | Reap leases that cannot be safely retried (or exhausted the budget).
-- No provider invocation is performed during reclamation.
reapExhausted :: Statement (Maybe UUID) ()
reapExhausted = [resultlessStatement|
 WITH expired AS MATERIALIZED (
   SELECT o.id, o.lease_id, o.replay_safe
   FROM hfsm.outbox o
   WHERE o.status='leased' AND o.lease_until <= clock_timestamp()
     AND (NOT o.replay_safe OR o.attempts >= o.max_attempts)
     AND ($1 :: uuid? IS NULL OR o.instance_id = $1 :: uuid?)
   ORDER BY o.lease_until, o.uid
   LIMIT 200 FOR UPDATE OF o SKIP LOCKED
 ), closed AS (
   UPDATE hfsm.outbox o SET
     status='failed', lease_id=NULL, lease_owner=NULL, lease_until=NULL,
     completed_at=clock_timestamp(), updated_at=clock_timestamp(),
     failure_class='unknown',
     last_error='lease expired: external delivery outcome unknown (possibly exhausted)'
   FROM expired e WHERE o.id=e.id
   RETURNING o.id
 )
 UPDATE hfsm.activity_attempt a SET
   outcome='lease_expired', finished_at=clock_timestamp(),
   error_text='worker lease expired before recording outcome'
 FROM expired e JOIN closed c ON c.id=e.id
 WHERE a.outbox_id=e.id AND a.lease_id=e.lease_id
   AND a.outcome='leased'
|]

-- | Acquire fresh fenced claims. A provider's replay promise is frozen on
-- first claim; on recovery, ONLY replay_safe effects are reclaimed. Stale
-- attempts are annotated before the new attempt is logged in the same TX.
claimEffects :: Statement (Int32, UUID, Int32, Maybe UUID, Vector Text, Vector Text) (Vector Value)
claimEffects = [vectorStatement|
 WITH picked AS MATERIALIZED (
   SELECT o.id, o.lease_id
   FROM hfsm.outbox o
   WHERE o.attempts < o.max_attempts
     AND ($4 :: uuid? IS NULL OR o.instance_id = $4 :: uuid?)
     AND (o.payload->>'kind') = ANY($6 :: text[])
     AND (
       (o.status='ready' AND o.available_at <= clock_timestamp())
       OR (o.status='leased' AND o.lease_until <= clock_timestamp()
           AND o.replay_safe)
     )
   ORDER BY o.available_at, o.uid
   LIMIT $1 :: int4 FOR UPDATE OF o SKIP LOCKED
 ), abandoned AS (
   UPDATE hfsm.activity_attempt a SET
      outcome='lease_expired', finished_at=clock_timestamp(),
      error_text='reclaimed after worker lease expiry'
   FROM picked p
   WHERE a.outbox_id=p.id AND a.lease_id=p.lease_id
     AND a.outcome='leased'
   RETURNING a.outbox_id
 ), claimed AS (
   UPDATE hfsm.outbox o SET
     status='leased', lease_id=gen_random_uuid(), lease_owner=$2 :: uuid,
     lease_until=clock_timestamp() + ($3 :: int4 * interval '1 second'),
     attempts=o.attempts+1, updated_at=clock_timestamp(),
     replay_safe=CASE WHEN o.attempts=0
       THEN (o.payload->>'kind') = ANY($5 :: text[])
       ELSE o.replay_safe END
   FROM picked p WHERE o.id=p.id
   RETURNING o.id, o.instance_id, o.step_id, o.effect_key, o.payload,
     o.attempts, o.max_attempts, o.timeout_seconds,
     o.retry_base_seconds, o.retry_cap_seconds, o.replay_safe,
     o.lease_id, o.lease_owner, o.lease_until
 ), logged AS (
   INSERT INTO hfsm.activity_attempt
     (outbox_id,attempt_no,lease_id,worker_id,outcome)
   SELECT c.id,c.attempts,c.lease_id,c.lease_owner,'leased'
   FROM claimed c
   RETURNING outbox_id
 )
 SELECT jsonb_build_object(
   'effectId',c.id, 'instanceId',c.instance_id,
   'stepId',c.step_id, 'effectKey',c.effect_key,
   'payload',c.payload, 'attempt',c.attempts,
   'maxAttempts',c.max_attempts, 'timeoutSeconds',c.timeout_seconds,
   'retryBaseSeconds',c.retry_base_seconds,
   'retryCapSeconds',c.retry_cap_seconds,
   'replaySafe',c.replay_safe, 'leaseId',c.lease_id,
   'leaseUntil',c.lease_until
 ) :: jsonb
 FROM claimed c JOIN logged l ON l.outbox_id=c.id
|]

-- | A renewal never revives an expired token, even when a former worker
-- briefly reconnects. Use DB server time for expiry, not caller time.
renewEffect :: Statement (UUID, UUID, UUID, Int32) (Maybe UTCTime)
renewEffect = [maybeStatement|
 UPDATE hfsm.outbox SET
   lease_until=GREATEST(lease_until,
     clock_timestamp() + ($4 :: int4 * interval '1 second')),
   updated_at=clock_timestamp()
 WHERE id=$1 :: uuid AND lease_id=$2 :: uuid AND lease_owner=$3 :: uuid
   AND status='leased' AND lease_until > clock_timestamp()
 RETURNING lease_until :: timestamptz
|]

-- | Fenced CAS update. Its row count determines whether the worker still
-- owns this attempt. "ready" is a scheduled retry; "failed" is terminal.
-- All write branches clear lease fields and satisfy the schema CHECK.
finishEffect :: Statement
  (UUID, UUID, UUID, Int32, Text, Maybe Value, Maybe Text, Int32, Maybe Text)
  (Maybe UUID)
finishEffect = [maybeStatement|
 UPDATE hfsm.outbox SET
   status=$5 :: text,
   available_at=CASE WHEN $5 :: text='ready'
      THEN clock_timestamp()+($8 :: int4 * interval '1 second')
      ELSE available_at END,
   result=CASE WHEN $5 :: text='done' THEN $6 :: jsonb? ELSE result END,
   last_error=$7 :: text?, failure_class=$9 :: text?,
   completed_at=CASE WHEN $5 :: text IN ('done','failed')
      THEN clock_timestamp() ELSE NULL END,
   lease_id=NULL, lease_owner=NULL, lease_until=NULL,
   updated_at=clock_timestamp()
 WHERE id=$1 :: uuid AND lease_id=$2 :: uuid AND lease_owner=$3 :: uuid
   AND attempts=$4 :: int4 AND status='leased'
   AND lease_until > clock_timestamp()
   AND ($5 :: text <> 'ready' OR (replay_safe AND attempts < max_attempts))
 RETURNING id :: uuid
|]

finishAttempt :: Statement (UUID, UUID, Int32, Text, Maybe Text, Maybe Value) (Maybe UUID)
finishAttempt = [maybeStatement|
 UPDATE hfsm.activity_attempt SET
   outcome=$4 :: text, finished_at=clock_timestamp(),
   error_text=$5 :: text?, receipt=$6 :: jsonb?
 WHERE outbox_id=$1 :: uuid AND lease_id=$2 :: uuid
   AND attempt_no=$3 :: int4 AND outcome='leased'
 RETURNING outbox_id :: uuid
|]
