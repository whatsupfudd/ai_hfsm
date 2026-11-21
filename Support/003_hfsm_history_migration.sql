-- CT-05: immutable release identity, exact pre-commit replay inputs,
-- and explicit append-only graph/context migration audit.
-- Forward migration after 001_hfsm_runtime.sql and 002_hfsm_activity.sql.
-- Run once, in a transaction, on a staging database before production.

-- A user-visible version may have only one compiled digest. This deliberately
-- rejects dirty databases where the same version was reused for different code.
ALTER TABLE hfsm.machine_release
  ADD CONSTRAINT machine_release_name_version_uq UNIQUE (machine_name,machine_version);

-- Digest of deployed executable/closure implementation, supplied by CI. Graph
-- digests alone do NOT attest Haskell functions embedded in MachineSpec.
ALTER TABLE hfsm.machine_release
  ADD COLUMN implementation_id text CHECK
    (implementation_id IS NULL OR length(implementation_id) >= 12);

CREATE TRIGGER machine_release_immutable
  BEFORE UPDATE OR DELETE ON hfsm.machine_release
  FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();

-- An exact snapshot of the input seen by planStep, not a reconstruction from
-- hfsm.signal, whose updated_at, status and lease columns change on ACK.
-- Existing pre-CT05 steps intentionally have no row in this table.
CREATE TABLE hfsm.turn_input (
  step_id            uuid PRIMARY KEY,
  instance_id        uuid NOT NULL,
  revision           bigint NOT NULL CHECK (revision >= 1),
  source_snapshot_id uuid NOT NULL,
  input              jsonb NOT NULL CHECK (jsonb_typeof(input)='object'),
  captured_at        timestamptz NOT NULL DEFAULT clock_timestamp(),
  CONSTRAINT turn_input_step_fk FOREIGN KEY (instance_id,revision,step_id)
    REFERENCES hfsm.step(instance_id,revision,id) ON DELETE RESTRICT,
  CONSTRAINT turn_input_snapshot_fk FOREIGN KEY (instance_id,source_snapshot_id)
    REFERENCES hfsm.snapshot(instance_id,id) ON DELETE RESTRICT,
  CONSTRAINT turn_input_inst_revision_uq UNIQUE (instance_id,revision)
);
CREATE TRIGGER turn_input_immutable BEFORE UPDATE OR DELETE ON hfsm.turn_input
  FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();

-- This trigger executes inside commitTurn after step INSERT but before the
-- source signal is ACKed and before the current instance pointer advances.
-- Signal updated_at can be overwritten by CT-03 lease renewal. The pure
-- planner stamps the generated step with its original observed signal time,
-- so NEW.created_at is the authoritative replay clock.
-- No Haskell CT-03/CT-04 transactional APIs need to be changed.
CREATE FUNCTION hfsm.capture_turn_input() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE recorded jsonb;
BEGIN
  IF NEW.kind='migrate' THEN RETURN NEW; END IF;
  IF NEW.signal_id IS NULL THEN
    RAISE EXCEPTION 'CT05 cannot capture input for step % without signal', NEW.id;
  END IF;
  SELECT jsonb_build_object(
    'machine', jsonb_build_object(
      'name', i.machine_name, 'version', i.machine_version, 'digest', i.spec_digest),
    'instance_', jsonb_build_object(
      'uid', i.uid, 'uuid', i.id, 'machine', i.machine_name,
      'version', i.machine_version, 'digest', i.spec_digest,
      'status', i.status, 'state', i.current_state_ref,
      'path', i.current_path, 'snap', i.current_snapshot_id,
      'revision', i.revision, 'nextSignalOrdinal', i.next_signal_ordinal,
      'parent', i.parent_instance_id,
      'createdAt', i.created_at, 'updatedAt', i.updated_at),
    'snapshot', jsonb_build_object(
      'uid', sn.uid, 'uuid', sn.id, 'inst', sn.instance_id,
      'state', sn.state_ref, 'path', sn.state_path, 'ctx', sn.ctx,
      'wait', sn.wait_value, 'child', sn.child_value,
      'version', sn.machine_version, 'digest', sn.spec_digest,
      'createdAt', sn.created_at),
    'signal', jsonb_build_object(
      'uid', sg.uid, 'uuid', sg.id, 'inst', sg.instance_id,
      'ordinal', sg.ordinal, 'status', 'LeasedSs',
      'payload', sg.payload, 'cause', sg.cause, 'lease', sg.lease_id,
      'leaseOwner', sg.lease_owner, 'leaseUntil', sg.lease_until,
      'attempts', sg.attempts, 'readyAt', sg.ready_at,
      'createdAt', sg.created_at, 'updatedAt', NEW.created_at)
  ) INTO recorded
  FROM hfsm.instance i
  JOIN hfsm.signal sg ON sg.instance_id=i.id AND sg.id=NEW.signal_id
  JOIN hfsm.snapshot sn ON sn.instance_id=i.id AND sn.id=NEW.source_snapshot_id
  WHERE i.id=NEW.instance_id AND sg.status='leased'
    AND i.current_snapshot_id=NEW.source_snapshot_id
    AND i.revision=NEW.revision-1
    AND sg.lease_id=i.in_flight_lease_id
    AND i.in_flight_signal_id=sg.id;
  IF recorded IS NULL THEN
    RAISE EXCEPTION 'CT05 could not atomically capture input for step %', NEW.id;
  END IF;
  INSERT INTO hfsm.turn_input (step_id,instance_id,revision,source_snapshot_id,input)
    VALUES (NEW.id,NEW.instance_id,NEW.revision,NEW.source_snapshot_id,recorded);
  RETURN NEW;
END;
$$;
CREATE TRIGGER step_capture_input
  AFTER INSERT ON hfsm.step
  FOR EACH ROW EXECUTE FUNCTION hfsm.capture_turn_input();

-- A migration is an explicit, single-revision historical step. Its audit
-- stores both immutable release keys and the named transform applied.
CREATE TABLE hfsm.migration_event (
  operation_id       uuid PRIMARY KEY,
  instance_id        uuid NOT NULL,
  revision           bigint NOT NULL CHECK (revision >= 1),
  step_id            uuid NOT NULL UNIQUE,
  source_snapshot_id uuid NOT NULL,
  target_snapshot_id uuid NOT NULL,
  source_key         jsonb NOT NULL CHECK (jsonb_typeof(source_key)='object'),
  target_key         jsonb NOT NULL CHECK (jsonb_typeof(target_key)='object'),
  mapping_name       text NOT NULL CHECK (length(mapping_name)>0),
  old_ctx            jsonb NOT NULL,
  new_ctx            jsonb NOT NULL,
  created_at         timestamptz NOT NULL,
  CONSTRAINT migration_event_step_fk FOREIGN KEY (instance_id,revision,step_id)
    REFERENCES hfsm.step(instance_id,revision,id) ON DELETE RESTRICT,
  CONSTRAINT migration_event_source_fk FOREIGN KEY (instance_id,source_snapshot_id)
    REFERENCES hfsm.snapshot(instance_id,id) ON DELETE RESTRICT,
  CONSTRAINT migration_event_target_fk FOREIGN KEY (instance_id,target_snapshot_id)
    REFERENCES hfsm.snapshot(instance_id,id) ON DELETE RESTRICT,
  CONSTRAINT migration_event_inst_revision_uq UNIQUE (instance_id,revision)
);
CREATE INDEX migration_event_inst_idx
  ON hfsm.migration_event (instance_id,revision);

-- The old Pg.enqueue API has no producer release parameter. Once an instance
-- has migrated, reject unlabeled enqueues rather than silently decoding an
-- old-version payload with the new codec. The new enqueuePinned function sets
-- hfsm.enqueue_release transaction-locally after validating the instance row.
CREATE FUNCTION hfsm.enforce_migrated_enqueue_release() RETURNS trigger
LANGUAGE plpgsql AS $$
DECLARE expected_release text;
BEGIN
  IF EXISTS (SELECT 1 FROM hfsm.migration_event m
             WHERE m.instance_id=NEW.instance_id) THEN
    SELECT i.machine_name || '@' || i.machine_version || '#' || i.spec_digest
      INTO expected_release FROM hfsm.instance i WHERE i.id=NEW.instance_id;
    IF current_setting('hfsm.enqueue_release',true) IS DISTINCT FROM expected_release THEN
      RAISE EXCEPTION 'migrated HFSM % requires release-aware enqueue',NEW.instance_id
        USING ERRCODE='23514';
    END IF;
  END IF;
  RETURN NEW;
END;
$$;
CREATE TRIGGER signal_migrated_release_guard
  BEFORE INSERT ON hfsm.signal FOR EACH ROW
  EXECUTE FUNCTION hfsm.enforce_migrated_enqueue_release();
CREATE TRIGGER migration_event_immutable
  BEFORE UPDATE OR DELETE ON hfsm.migration_event
  FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();

-- No synthetic migration step may commit without its immutable event.
-- Deferred because the event is appended AFTER the step in the same TX.
CREATE FUNCTION hfsm.require_migration_event() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  IF NEW.kind='migrate' AND NOT EXISTS (
    SELECT 1 FROM hfsm.migration_event m
    WHERE m.step_id=NEW.id AND m.instance_id=NEW.instance_id
      AND m.revision=NEW.revision) THEN
    RAISE EXCEPTION 'migration step % missing append-only migration event', NEW.id
      USING ERRCODE='23514';
  END IF;
  RETURN NEW;
END;
$$;
CREATE CONSTRAINT TRIGGER step_migration_audit_fk
  AFTER INSERT ON hfsm.step DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION hfsm.require_migration_event();

-- Release pinning cannot be silently rewritten by an UPDATE. A release
-- change requires a matching migration event for the *same new revision*,
-- source and target keys, and both snapshot pointers. Verified at COMMIT.
CREATE FUNCTION hfsm.require_release_migration() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  IF (OLD.machine_name,OLD.machine_version,OLD.spec_digest) IS DISTINCT FROM
     (NEW.machine_name,NEW.machine_version,NEW.spec_digest) THEN
    IF NOT EXISTS (
      SELECT 1 FROM hfsm.migration_event m
      JOIN hfsm.step s ON s.id=m.step_id AND s.kind='migrate'
      WHERE m.instance_id=NEW.id AND m.revision=NEW.revision
        AND m.target_snapshot_id=NEW.current_snapshot_id
        AND m.source_snapshot_id=OLD.current_snapshot_id
        AND NEW.revision=OLD.revision+1
        AND m.source_key->>'name'=OLD.machine_name
        AND m.source_key->>'version'=OLD.machine_version
        AND m.source_key->>'digest'=OLD.spec_digest
        AND m.target_key->>'name'=NEW.machine_name
        AND m.target_key->>'version'=NEW.machine_version
        AND m.target_key->>'digest'=NEW.spec_digest
    ) THEN
      RAISE EXCEPTION 'release identity may change only through audited migration, instance %', NEW.id
        USING ERRCODE='23514';
    END IF;
  END IF;
  RETURN NEW;
END;
$$;
CREATE CONSTRAINT TRIGGER instance_release_migration_guard
  AFTER UPDATE ON hfsm.instance DEFERRABLE INITIALLY DEFERRED
  FOR EACH ROW EXECUTE FUNCTION hfsm.require_release_migration();

-- These records have mutable DELIVERY/LEASE fields but immutable historical
-- identities and intent payloads. Keep their stable fields append-only even
-- after CT-04 retries, so replay does not depend on mutable current views.
CREATE FUNCTION hfsm.guard_signal_intent() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  IF (OLD.id,OLD.instance_id,OLD.ordinal,OLD.payload,OLD.cause,
      OLD.dedupe_key,OLD.created_at)
     IS DISTINCT FROM
     (NEW.id,NEW.instance_id,NEW.ordinal,NEW.payload,NEW.cause,
      NEW.dedupe_key,NEW.created_at) THEN
    RAISE EXCEPTION 'HFSM signal identity/intent immutable' USING ERRCODE='23514';
  END IF;
  RETURN NEW;
END;
$$;
CREATE TRIGGER signal_intent_immutable BEFORE UPDATE ON hfsm.signal
  FOR EACH ROW EXECUTE FUNCTION hfsm.guard_signal_intent();

CREATE FUNCTION hfsm.guard_outbox_intent() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  IF (OLD.id,OLD.instance_id,OLD.step_id,OLD.ordinal,OLD.effect_key,
      OLD.payload,OLD.created_at)
     IS DISTINCT FROM
     (NEW.id,NEW.instance_id,NEW.step_id,NEW.ordinal,NEW.effect_key,
      NEW.payload,NEW.created_at) THEN
    RAISE EXCEPTION 'HFSM outbox effect identity/intent immutable' USING ERRCODE='23514';
  END IF;
  RETURN NEW;
END;
$$;
CREATE TRIGGER outbox_intent_immutable BEFORE UPDATE ON hfsm.outbox
  FOR EACH ROW EXECUTE FUNCTION hfsm.guard_outbox_intent();

CREATE FUNCTION hfsm.guard_child_identity() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  IF (OLD.id,OLD.parent_instance_id,OLD.child_instance_id,
      OLD.created_by_step_id,OLD.ordinal,OLD.child_key,OLD.join_ref,OLD.created_at)
     IS DISTINCT FROM
     (NEW.id,NEW.parent_instance_id,NEW.child_instance_id,
      NEW.created_by_step_id,NEW.ordinal,NEW.child_key,NEW.join_ref,NEW.created_at) THEN
    RAISE EXCEPTION 'HFSM child creation identity immutable' USING ERRCODE='23514';
  END IF;
  RETURN NEW;
END;
$$;
CREATE TRIGGER child_identity_immutable BEFORE UPDATE ON hfsm.child_run
  FOR EACH ROW EXECUTE FUNCTION hfsm.guard_child_identity();

CREATE FUNCTION hfsm.guard_break_identity() RETURNS trigger
LANGUAGE plpgsql AS $$
BEGIN
  IF (OLD.id,OLD.instance_id,OLD.step_id,OLD.ordinal,OLD.kind,
      OLD.state_ref,OLD.note,OLD.created_at)
     IS DISTINCT FROM
     (NEW.id,NEW.instance_id,NEW.step_id,NEW.ordinal,NEW.kind,
      NEW.state_ref,NEW.note,NEW.created_at) THEN
    RAISE EXCEPTION 'HFSM break occurrence identity immutable' USING ERRCODE='23514';
  END IF;
  RETURN NEW;
END;
$$;
CREATE TRIGGER break_identity_immutable BEFORE UPDATE ON hfsm.break_event
  FOR EACH ROW EXECUTE FUNCTION hfsm.guard_break_identity();

COMMENT ON TABLE hfsm.turn_input IS
  'Exact, immutable pre-ACK planner inputs. Null for legacy step revisions before CT-05.';
COMMENT ON TABLE hfsm.migration_event IS
  'Append-only evidence of explicit, revision-checked graph/context migrations.';
COMMENT ON COLUMN hfsm.machine_release.implementation_id IS
  'Deployment-generated fingerprint covering implementation/closure code; NULL for legacy registrations';
