-- CT-04 forward-only migration. Apply ONCE after Support/001_hfsm_runtime.sql.
-- Keep transaction boundaries under the migration runner.
-- PostgreSQL 16+; no extension installation needed.

ALTER TABLE hfsm.outbox
  ADD COLUMN max_attempts integer NOT NULL DEFAULT 5 CHECK (max_attempts BETWEEN 1 AND 1000),
  ADD COLUMN timeout_seconds integer NOT NULL DEFAULT 60 CHECK (timeout_seconds BETWEEN 1 AND 86400),
  ADD COLUMN retry_base_seconds integer NOT NULL DEFAULT 2 CHECK (retry_base_seconds BETWEEN 0 AND 86400),
  ADD COLUMN retry_cap_seconds integer NOT NULL DEFAULT 300 CHECK (retry_cap_seconds BETWEEN 0 AND 86400),
  ADD COLUMN replay_safe boolean NOT NULL DEFAULT false,
  ADD COLUMN result jsonb,
  ADD COLUMN failure_class text,
  ADD CONSTRAINT outbox_retry_range_ck CHECK (retry_cap_seconds >= retry_base_seconds),
  ADD CONSTRAINT outbox_failure_class_ck CHECK
    (failure_class IS NULL OR failure_class IN ('permanent','unknown','exhausted','timeout'));

-- Keep attempt evidence even when a worker crashes after a provider accepted
-- the request. "lease_expired" does NOT mean the provider failed to act.
CREATE TABLE hfsm.activity_attempt (
  outbox_id     uuid NOT NULL REFERENCES hfsm.outbox(id) ON DELETE RESTRICT,
  attempt_no    integer NOT NULL CHECK (attempt_no >= 1),
  lease_id      uuid NOT NULL,
  worker_id     uuid NOT NULL,
  started_at    timestamptz NOT NULL DEFAULT clock_timestamp(),
  finished_at   timestamptz,
  outcome       text NOT NULL CHECK
    (outcome IN ('leased','succeeded','retry','failed','lease_expired')),
  error_text    text,
  receipt       jsonb,
  PRIMARY KEY (outbox_id, attempt_no),
  CONSTRAINT activity_attempt_lease_uq UNIQUE (outbox_id,lease_id),
  CONSTRAINT activity_attempt_finish_ck CHECK
    ((outcome = 'leased' AND finished_at IS NULL)
      OR (outcome <> 'leased' AND finished_at IS NOT NULL))
);

CREATE INDEX activity_attempt_pending_idx
  ON hfsm.activity_attempt (started_at)
  WHERE outcome = 'leased';
CREATE INDEX outbox_unknown_outcomes_idx
  ON hfsm.outbox (updated_at,uid)
  WHERE status='failed' AND failure_class='unknown';

COMMENT ON COLUMN hfsm.outbox.replay_safe IS
  'Recorded at initial claim from provider registry: safe to repeat calls with the identical effect_key';
COMMENT ON COLUMN hfsm.outbox.failure_class IS
  'unknown means an external effect may have happened; requires manual reconciliation';

-- Preserve timer delays that the HFSM planner serialized into an outbox
-- payload. The baseline INSERT does not set available_at explicitly.
-- Due time is measured from durable INSERT/COMMIT, not from a client clock.
CREATE FUNCTION hfsm.outbox_schedule_timer() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE
  delay_value numeric;
BEGIN
  IF NEW.payload->>'kind' = 'timer'
     AND jsonb_typeof(NEW.payload->'delaySeconds') = 'number' THEN
    delay_value := GREATEST(0, LEAST(31536000,
      (NEW.payload->>'delaySeconds')::numeric));
    NEW.available_at := GREATEST(
      NEW.available_at, clock_timestamp() + (delay_value * interval '1 second'));
  END IF;
  RETURN NEW;
END;
$$;

CREATE TRIGGER outbox_schedule_timer_insert_trg
BEFORE INSERT ON hfsm.outbox FOR EACH ROW
EXECUTE FUNCTION hfsm.outbox_schedule_timer();

-- Correct pre-migration, still-undelivered timer records. Already-leased or
-- previously-attempted effects are deliberately left untouched.
UPDATE hfsm.outbox
SET available_at=GREATEST(
      available_at, created_at +
      (GREATEST(0,LEAST(31536000,(payload->>'delaySeconds')::numeric)) * interval '1 second'))
WHERE status='ready' AND attempts=0 AND payload->>'kind'='timer'
  AND jsonb_typeof(payload->'delaySeconds')='number';
