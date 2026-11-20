-- HFSM CT-02 / baseline PostgreSQL runtime schema
-- Target: PostgreSQL 17+ (compatible PostgreSQL 16+ features are used).
-- Intended for a NEW schema, installed once through the migration runner.
-- The migration runner should own BEGIN/COMMIT and its version ledger.
-- No extensions are required on supported PostgreSQL versions.
-- This file is DDL, not a replacement for the transactional Store adapter.

CREATE SCHEMA IF NOT EXISTS hfsm;

-- Haskell's StateRef / RouteRef / CaseRef / JoinRef use Word32-compatible IDs.
CREATE DOMAIN hfsm.ref32 AS bigint
  CHECK (VALUE >= 0 AND VALUE <= 4294967295);

-- Hfsm.Core.Digest expects 64 lowercase hexadecimal characters.
CREATE DOMAIN hfsm.digest64 AS text
  CHECK (VALUE ~ '^[0-9a-f]{64}$');

-- Only the serializable graph/identity is stored. Haskell closures stay in the
-- executable Registry; this table does NOT permit arbitrary code deployment.
CREATE TABLE hfsm.machine_release (
  machine_name       text NOT NULL CHECK (length(machine_name) > 0),
  machine_version    text NOT NULL CHECK (length(machine_version) > 0),
  spec_digest        hfsm.digest64 NOT NULL,
  graph_digest       hfsm.digest64,
  graph_json         jsonb,
  registered_at      timestamptz NOT NULL DEFAULT clock_timestamp(),
  PRIMARY KEY (machine_name, machine_version, spec_digest),
  CONSTRAINT machine_release_graph_kind_ck
    CHECK (graph_json IS NULL OR jsonb_typeof(graph_json) = 'object')
);

-- One row is the authoritative CURRENT pointer and coordination gate.
-- The initial snapshot is revision 0. Every committed turn increments revision.
-- current_snapshot_id FK is added below (after snapshot exists).
CREATE TABLE hfsm.instance (
  id                      uuid PRIMARY KEY,
  uid                     bigint GENERATED ALWAYS AS IDENTITY UNIQUE,
  machine_name            text NOT NULL,
  machine_version         text NOT NULL,
  spec_digest             hfsm.digest64 NOT NULL,
  status                  text NOT NULL DEFAULT 'running'
                            CHECK (status IN ('running','waiting','paused',
                                             'done','failed','cancelled')),
  current_state_ref       hfsm.ref32 NOT NULL,
  current_path            jsonb NOT NULL CHECK (jsonb_typeof(current_path) = 'array'),
  current_snapshot_id     uuid NOT NULL,
  revision                bigint NOT NULL DEFAULT 0 CHECK (revision >= 0),
  next_signal_ordinal     bigint NOT NULL DEFAULT 1 CHECK (next_signal_ordinal >= 1),
  parent_instance_id      uuid REFERENCES hfsm.instance(id) ON DELETE RESTRICT,
  -- Per-instance admission slot: only one signal may be executing at a time.
  in_flight_signal_id     uuid,
  in_flight_lease_id      uuid,
  in_flight_lease_until   timestamptz,
  created_at              timestamptz NOT NULL DEFAULT clock_timestamp(),
  updated_at              timestamptz NOT NULL DEFAULT clock_timestamp(),
  CONSTRAINT instance_release_fk
    FOREIGN KEY (machine_name,machine_version,spec_digest)
    REFERENCES hfsm.machine_release(machine_name,machine_version,spec_digest)
    ON DELETE RESTRICT,
  CONSTRAINT instance_flight_all_or_none_ck CHECK
    ( (in_flight_signal_id IS NULL AND in_flight_lease_id IS NULL
                                  AND in_flight_lease_until IS NULL)
      OR
      (in_flight_signal_id IS NOT NULL AND in_flight_lease_id IS NOT NULL
                                  AND in_flight_lease_until IS NOT NULL) ),
  CONSTRAINT instance_id_snapshot_pointer_uq UNIQUE (id,current_snapshot_id)
);

-- Every enqueue must allocate ordinal under an instance-row lock, updating
-- instance.next_signal_ordinal in the same DB transaction as the INSERT.
-- The lease token must be NEW on every claim/reclaim, not merely the worker ID.
CREATE TABLE hfsm.signal (
  id                  uuid PRIMARY KEY,
  uid                 bigint GENERATED ALWAYS AS IDENTITY UNIQUE,
  instance_id         uuid NOT NULL REFERENCES hfsm.instance(id) ON DELETE RESTRICT,
  ordinal             bigint NOT NULL CHECK (ordinal >= 1),
  status              text NOT NULL DEFAULT 'entered'
                        CHECK (status IN ('entered','leased','applied',
                                         'rejected','failed')),
  payload             jsonb NOT NULL,
  cause               jsonb,
  -- Optional producer-supplied key to deduplicate retries of signal ENQUEUE.
  dedupe_key          text,
  ready_at            timestamptz NOT NULL DEFAULT clock_timestamp(),
  lease_id            uuid,
  lease_owner         uuid,
  lease_until         timestamptz,
  attempts            integer NOT NULL DEFAULT 0 CHECK (attempts >= 0),
  last_error          text,
  created_at          timestamptz NOT NULL DEFAULT clock_timestamp(),
  updated_at          timestamptz NOT NULL DEFAULT clock_timestamp(),
  CONSTRAINT signal_instance_ordinal_uq UNIQUE (instance_id, ordinal),
  CONSTRAINT signal_instance_id_uq UNIQUE (instance_id,id),
  CONSTRAINT signal_lease_state_ck CHECK
    ( (status = 'leased' AND lease_id IS NOT NULL AND lease_owner IS NOT NULL
                         AND lease_until IS NOT NULL)
      OR
      (status <> 'leased' AND lease_id IS NULL AND lease_owner IS NULL
                          AND lease_until IS NULL) )
);

-- An instance can never have two signals simultaneously leased, even if a
-- buggy scheduler attempts to bypass the in-flight slot. To recover an
-- expired lease, RECLAIM THE SAME SIGNAL (new token) before advancing FIFO.
CREATE UNIQUE INDEX signal_one_leased_per_instance_uq
  ON hfsm.signal (instance_id) WHERE status = 'leased';
CREATE UNIQUE INDEX signal_instance_dedupe_uq
  ON hfsm.signal (instance_id,dedupe_key) WHERE dedupe_key IS NOT NULL;
CREATE INDEX signal_ready_queue_idx
  ON hfsm.signal (ready_at,uid) WHERE status = 'entered';
CREATE INDEX signal_expired_lease_idx
  ON hfsm.signal (lease_until,uid) WHERE status = 'leased';
CREATE INDEX signal_instance_status_idx
  ON hfsm.signal (instance_id,status,ordinal);

-- One immutable history record per successfully committed turn.
-- The source snapshot FK is installed below to avoid a DDL dependency cycle.
CREATE TABLE hfsm.step (
  id                   uuid PRIMARY KEY, -- UUID assigned by CT-01 planner
  uid                  bigint GENERATED ALWAYS AS IDENTITY UNIQUE,
  instance_id          uuid NOT NULL REFERENCES hfsm.instance(id) ON DELETE RESTRICT,
  revision             bigint NOT NULL CHECK (revision >= 1),
  signal_id            uuid,
  source_snapshot_id   uuid NOT NULL,
  kind                 text NOT NULL
                         CHECK (kind IN ('entry','signal','replay','migrate')),
  from_state_ref       hfsm.ref32 NOT NULL,
  to_state_ref         hfsm.ref32 NOT NULL,
  case_ref             hfsm.ref32,
  route_ref            hfsm.ref32 NOT NULL,
  note                 jsonb NOT NULL,
  created_at           timestamptz NOT NULL,
  CONSTRAINT step_instance_revision_uq UNIQUE (instance_id,revision),
  CONSTRAINT step_instance_id_uq UNIQUE (instance_id,id),
  CONSTRAINT step_instance_revision_id_uq UNIQUE (instance_id,revision,id),
  CONSTRAINT step_signal_fk FOREIGN KEY (instance_id,signal_id)
    REFERENCES hfsm.signal(instance_id,id) ON DELETE RESTRICT
);
CREATE UNIQUE INDEX step_signal_once_uq
  ON hfsm.step (instance_id,signal_id) WHERE signal_id IS NOT NULL;
CREATE INDEX step_instance_history_idx
  ON hfsm.step (instance_id,revision DESC);
CREATE INDEX step_time_idx ON hfsm.step (created_at DESC,uid DESC);

-- Immutable state materializations. Context, wait, and child information use
-- precisely the JSON representations emitted by the Haskell codecs.
-- Revision 0 is the initial snapshot and has no producer step.
CREATE TABLE hfsm.snapshot (
  id                   uuid PRIMARY KEY, -- UUID assigned by CT-01 planner
  uid                  bigint GENERATED ALWAYS AS IDENTITY UNIQUE,
  instance_id          uuid NOT NULL REFERENCES hfsm.instance(id) ON DELETE RESTRICT,
  revision             bigint NOT NULL CHECK (revision >= 0),
  produced_by_step_id  uuid,
  state_ref            hfsm.ref32 NOT NULL,
  state_path           jsonb NOT NULL CHECK (jsonb_typeof(state_path) = 'array'),
  ctx                  jsonb NOT NULL,
  wait_value           jsonb,
  child_value          jsonb NOT NULL,
  machine_version      text NOT NULL,
  spec_digest          hfsm.digest64 NOT NULL,
  created_at           timestamptz NOT NULL,
  CONSTRAINT snapshot_instance_revision_uq UNIQUE (instance_id,revision),
  CONSTRAINT snapshot_instance_id_uq UNIQUE (instance_id,id),
  CONSTRAINT snapshot_initial_or_turn_ck CHECK
    ((revision = 0 AND produced_by_step_id IS NULL)
     OR (revision > 0 AND produced_by_step_id IS NOT NULL)),
  CONSTRAINT snapshot_step_revision_fk
    FOREIGN KEY (instance_id,revision,produced_by_step_id)
    REFERENCES hfsm.step(instance_id,revision,id) ON DELETE RESTRICT
);
CREATE INDEX snapshot_instance_time_idx
  ON hfsm.snapshot (instance_id,created_at DESC);

-- Composite keys guarantee that a current/expected snapshot and leased
-- signal always belong to the same instance as the corresponding record.
ALTER TABLE hfsm.instance
  ADD CONSTRAINT instance_current_snapshot_fk
  FOREIGN KEY (id,current_snapshot_id)
  REFERENCES hfsm.snapshot(instance_id,id)
  ON DELETE NO ACTION DEFERRABLE INITIALLY DEFERRED;
ALTER TABLE hfsm.instance
  ADD CONSTRAINT instance_inflight_signal_fk
  FOREIGN KEY (id,in_flight_signal_id)
  REFERENCES hfsm.signal(instance_id,id) ON DELETE RESTRICT;
ALTER TABLE hfsm.step
  ADD CONSTRAINT step_source_snapshot_fk
  FOREIGN KEY (instance_id,source_snapshot_id)
  REFERENCES hfsm.snapshot(instance_id,id) ON DELETE RESTRICT;

-- Outbox commands/timers/spawn requests are durable *intents*; delivery is
-- not part of commitTurn. (step_id,ordinal) is the stable deduplication key.
CREATE TABLE hfsm.outbox (
  id                uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  uid               bigint GENERATED ALWAYS AS IDENTITY UNIQUE,
  instance_id       uuid NOT NULL,
  step_id           uuid NOT NULL,
  ordinal           integer NOT NULL CHECK (ordinal >= 0),
  effect_key        text NOT NULL UNIQUE CHECK (length(effect_key) > 0),
  status            text NOT NULL DEFAULT 'ready'
                      CHECK (status IN ('ready','leased','done','failed')),
  payload           jsonb NOT NULL,
  available_at      timestamptz NOT NULL DEFAULT clock_timestamp(),
  lease_id          uuid,
  lease_owner       uuid,
  lease_until       timestamptz,
  attempts          integer NOT NULL DEFAULT 0 CHECK (attempts >= 0),
  last_error        text,
  completed_at      timestamptz,
  created_at        timestamptz NOT NULL,
  updated_at        timestamptz NOT NULL DEFAULT clock_timestamp(),
  CONSTRAINT outbox_step_ordinal_uq UNIQUE (step_id,ordinal),
  CONSTRAINT outbox_step_fk FOREIGN KEY (instance_id,step_id)
    REFERENCES hfsm.step(instance_id,id) ON DELETE RESTRICT,
  CONSTRAINT outbox_lease_state_ck CHECK
    ((status = 'leased' AND lease_id IS NOT NULL AND lease_owner IS NOT NULL
                         AND lease_until IS NOT NULL)
     OR (status <> 'leased' AND lease_id IS NULL AND lease_owner IS NULL
                             AND lease_until IS NULL))
);
CREATE INDEX outbox_ready_queue_idx
  ON hfsm.outbox (available_at,uid) WHERE status = 'ready';
CREATE INDEX outbox_expired_lease_idx
  ON hfsm.outbox (lease_until,uid) WHERE status = 'leased';
CREATE INDEX outbox_instance_idx
  ON hfsm.outbox (instance_id,uid);

-- The child UUID in CT-01 is RESERVED at parent turn commit, but the child
-- instance row may not exist until a later outbox activity executes.
-- Thus child_instance_id intentionally has NO FK to hfsm.instance(id).
CREATE TABLE hfsm.child_run (
  id                  uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  uid                 bigint GENERATED ALWAYS AS IDENTITY UNIQUE,
  parent_instance_id  uuid NOT NULL REFERENCES hfsm.instance(id) ON DELETE RESTRICT,
  child_instance_id   uuid NOT NULL UNIQUE,
  created_by_step_id  uuid NOT NULL,
  ordinal             integer NOT NULL CHECK (ordinal >= 0),
  child_key           text,
  status              text NOT NULL
                        CHECK (status IN ('spawned','running','done',
                                         'failed','cancelled')),
  join_ref            hfsm.ref32,
  created_at          timestamptz NOT NULL,
  updated_at          timestamptz NOT NULL,
  CONSTRAINT child_created_by_step_fk
    FOREIGN KEY (parent_instance_id,created_by_step_id)
    REFERENCES hfsm.step(instance_id,id) ON DELETE RESTRICT,
  CONSTRAINT child_created_ordinal_uq UNIQUE (created_by_step_id,ordinal),
  CONSTRAINT child_parent_uuid_uq UNIQUE (parent_instance_id,child_instance_id)
);
CREATE INDEX child_parent_status_idx
  ON hfsm.child_run (parent_instance_id,status,uid);
CREATE INDEX child_join_idx
  ON hfsm.child_run (parent_instance_id,join_ref,status);

-- CT-01's BreakAppend describes an execution breakpoint occurrence.
-- Global breakpoint definitions and operator controls are separate concerns.
CREATE TABLE hfsm.break_event (
  id                  uuid PRIMARY KEY DEFAULT gen_random_uuid(),
  uid                 bigint GENERATED ALWAYS AS IDENTITY UNIQUE,
  instance_id         uuid NOT NULL,
  step_id             uuid NOT NULL,
  ordinal             integer NOT NULL CHECK (ordinal >= 0),
  kind                text NOT NULL CHECK
                        (kind IN ('BeforeCaseBk','AfterCaseBk','BeforeRouteBk',
                                  'AfterRouteBk','BeforeCommitBk','OnErrorBk')),
  state_ref           hfsm.ref32,
  status              text NOT NULL CHECK (status IN ('ArmedBs','HitBs','ClearedBs')),
  note                text,
  created_at          timestamptz NOT NULL,
  updated_at          timestamptz NOT NULL,
  CONSTRAINT break_event_step_fk FOREIGN KEY (instance_id,step_id)
    REFERENCES hfsm.step(instance_id,id) ON DELETE RESTRICT,
  CONSTRAINT break_event_step_ordinal_uq UNIQUE (step_id,ordinal)
);
CREATE INDEX break_event_instance_idx
  ON hfsm.break_event (instance_id,uid DESC);

-- ProjBatch's active/trace/queue/tree values are recorded as an append-only
-- projection SOURCE in the same transaction. Materialized read models can
-- be rebuilt separately without risking the authoritative commit boundary.
CREATE TABLE hfsm.projection_event (
  uid                 bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
  instance_id         uuid NOT NULL,
  step_id             uuid NOT NULL,
  revision            bigint NOT NULL CHECK (revision >= 1),
  kind                text NOT NULL CHECK (kind IN ('active','trace','queue','tree')),
  ordinal             integer NOT NULL CHECK (ordinal >= 0),
  payload             jsonb NOT NULL,
  created_at          timestamptz NOT NULL DEFAULT clock_timestamp(),
  CONSTRAINT projection_event_step_fk
    FOREIGN KEY (instance_id,revision,step_id)
    REFERENCES hfsm.step(instance_id,revision,id) ON DELETE RESTRICT,
  CONSTRAINT projection_event_source_uq UNIQUE (step_id,kind,ordinal)
);
CREATE INDEX projection_event_inst_revision_idx
  ON hfsm.projection_event (instance_id,revision,kind,ordinal);
CREATE INDEX projection_event_kind_idx
  ON hfsm.projection_event (kind,uid);

-- Per-instance revision checkpoints are safe for building per-instance read
-- models. Do NOT use a global identity/high watermark as a commit-order cursor:
-- Postgres sequences are allocated before COMMIT and can commit out of order.
-- Checkpoints MUST be advanced in the same transaction as the read-model write.
CREATE TABLE hfsm.projection_progress (
  consumer             text NOT NULL CHECK (length(consumer) > 0),
  instance_id          uuid NOT NULL REFERENCES hfsm.instance(id) ON DELETE RESTRICT,
  last_revision        bigint NOT NULL DEFAULT 0 CHECK (last_revision >= 0),
  updated_at           timestamptz NOT NULL DEFAULT clock_timestamp(),
  PRIMARY KEY (consumer,instance_id)
);

-- Enforce append-only authoritative history and projection sources at SQL level.
-- The application database role should additionally lack DELETE/UPDATE
-- privileges on these tables. (No UPDATE allowed, including metadata edits.)
CREATE FUNCTION hfsm.reject_history_mutation()
RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
  RAISE EXCEPTION 'immutable HFSM history table %: % forbidden', TG_TABLE_NAME, TG_OP
    USING ERRCODE = '55000';
END;
$$;
CREATE TRIGGER step_immutable BEFORE UPDATE OR DELETE ON hfsm.step
  FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();
CREATE TRIGGER snapshot_immutable BEFORE UPDATE OR DELETE ON hfsm.snapshot
  FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();
CREATE TRIGGER projection_event_immutable BEFORE UPDATE OR DELETE ON hfsm.projection_event
  FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();

-- Administrative lookup indexes; no premature partitioning or JSON GIN.
CREATE INDEX instance_status_updated_idx
  ON hfsm.instance (status,updated_at,id);
CREATE INDEX instance_parent_idx
  ON hfsm.instance (parent_instance_id) WHERE parent_instance_id IS NOT NULL;
CREATE INDEX instance_flight_expiry_idx
  ON hfsm.instance (in_flight_lease_until)
  WHERE in_flight_signal_id IS NOT NULL;

COMMENT ON SCHEMA hfsm IS 'FUDD HFSM: authoritative transactional orchestration runtime';
COMMENT ON TABLE hfsm.instance IS 'Authoritative current control state and per-instance admission gate';
COMMENT ON TABLE hfsm.step IS 'Immutable committed turn history; one row per revision';
COMMENT ON TABLE hfsm.snapshot IS 'Immutable state snapshots, including initial revision 0';
COMMENT ON TABLE hfsm.signal IS 'Ordered durable inputs; leased by an expiring, fenced claim';
COMMENT ON TABLE hfsm.outbox IS 'Transactional external-effect intents; separate at-least-once dispatch';
COMMENT ON TABLE hfsm.projection_event IS 'Durable projection source; derived read models rebuild asynchronously';
