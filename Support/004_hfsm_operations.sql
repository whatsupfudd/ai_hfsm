-- CT-06 operational admission and audit. Apply after 001,002,003.
-- PostgreSQL 16+. Migration runner owns BEGIN/COMMIT.
-- Default tenant preserves legacy data; production callers use explicit tenant IDs.
CREATE TABLE hfsm.tenant (
 id uuid PRIMARY KEY, tenant_key text NOT NULL UNIQUE CHECK (length(tenant_key)>0),
 enabled boolean NOT NULL DEFAULT true,
 max_active_instances integer NOT NULL DEFAULT 10000 CHECK (max_active_instances>0),
 max_pending_signals integer NOT NULL DEFAULT 100000 CHECK (max_pending_signals>0),
 max_signal_inflight integer NOT NULL DEFAULT 100 CHECK (max_signal_inflight>0),
 max_activity_inflight integer NOT NULL DEFAULT 100 CHECK (max_activity_inflight>0),
 max_signal_attempts integer NOT NULL DEFAULT 8 CHECK (max_signal_attempts>0),
 created_at timestamptz NOT NULL DEFAULT clock_timestamp()
);
INSERT INTO hfsm.tenant(id,tenant_key)
 VALUES ('00000000-0000-0000-0000-000000000001','legacy')
 ON CONFLICT (id) DO NOTHING;
CREATE TABLE hfsm.concurrency_group (
 tenant_id uuid NOT NULL REFERENCES hfsm.tenant(id),
 group_key text NOT NULL CHECK(length(group_key)>0),
 max_signals integer NOT NULL DEFAULT 100 CHECK(max_signals>0),
 max_activities integer NOT NULL DEFAULT 100 CHECK(max_activities>0),
 PRIMARY KEY(tenant_id,group_key)
);
INSERT INTO hfsm.concurrency_group(tenant_id,group_key)
 VALUES('00000000-0000-0000-0000-000000000001','default')
 ON CONFLICT DO NOTHING;

ALTER TABLE hfsm.instance
 ADD COLUMN tenant_id uuid NOT NULL DEFAULT '00000000-0000-0000-0000-000000000001' REFERENCES hfsm.tenant(id),
 ADD COLUMN concurrency_group text NOT NULL DEFAULT 'default',
 ADD COLUMN priority smallint NOT NULL DEFAULT 0 CHECK (priority BETWEEN -20 AND 20),
 ADD CONSTRAINT instance_tenant_group_fk FOREIGN KEY(tenant_id,concurrency_group)
   REFERENCES hfsm.concurrency_group(tenant_id,group_key),
 ADD CONSTRAINT instance_tenant_id_uq UNIQUE (tenant_id,id);
CREATE INDEX instance_tenant_status_idx ON hfsm.instance(tenant_id,status,updated_at);
ALTER TABLE hfsm.signal
 ADD COLUMN tenant_id uuid NOT NULL DEFAULT '00000000-0000-0000-0000-000000000001',
 ADD COLUMN priority smallint NOT NULL DEFAULT 0 CHECK(priority BETWEEN -20 AND 20),
 ADD COLUMN correlation_id uuid,
 ADD COLUMN causation_id uuid,
 ADD COLUMN traceparent text,
 ADD CONSTRAINT signal_tenant_instance_fk FOREIGN KEY(tenant_id,instance_id)
   REFERENCES hfsm.instance(tenant_id,id);
ALTER TABLE hfsm.outbox
 ADD COLUMN tenant_id uuid NOT NULL DEFAULT '00000000-0000-0000-0000-000000000001',
 ADD COLUMN correlation_id uuid,
 ADD COLUMN causation_id uuid,
 ADD COLUMN traceparent text,
 ADD CONSTRAINT outbox_tenant_instance_fk FOREIGN KEY(tenant_id,instance_id)
   REFERENCES hfsm.instance(tenant_id,id);
-- Backfill before new insert triggers. Existing rows are legacy tenant by design.
CREATE INDEX signal_tenant_queue_idx ON hfsm.signal(tenant_id,status,ready_at,priority DESC,ordinal);
CREATE INDEX outbox_tenant_queue_idx ON hfsm.outbox(tenant_id,status,available_at);

-- Global admission gate: deliberately serializes scheduling decisions.
-- This prioritizes correctness over dispatcher throughput. Replace with
-- sharded tenant/group admission locks only after adversarial load tests.
CREATE TABLE hfsm.admission_gate(id integer PRIMARY KEY CHECK(id=1));
INSERT INTO hfsm.admission_gate VALUES(1) ON CONFLICT DO NOTHING;

CREATE TABLE hfsm.worker_registry (
 id uuid PRIMARY KEY,
 capabilities text[] NOT NULL DEFAULT '{}' CHECK (array_position(capabilities,NULL) IS NULL),
 state text NOT NULL DEFAULT 'active' CHECK (state IN ('active','draining','stopped')),
 heartbeat_at timestamptz NOT NULL DEFAULT clock_timestamp(),
 ttl_seconds integer NOT NULL DEFAULT 30 CHECK (ttl_seconds BETWEEN 2 AND 3600),
 max_signals integer NOT NULL DEFAULT 8 CHECK(max_signals>0),
 max_activities integer NOT NULL DEFAULT 8 CHECK(max_activities>0),
 generation uuid NOT NULL DEFAULT gen_random_uuid(),
 started_at timestamptz NOT NULL DEFAULT clock_timestamp()
);
CREATE INDEX worker_registry_expire_idx ON hfsm.worker_registry(heartbeat_at);

CREATE TABLE hfsm.operator_action (
 operation_id uuid PRIMARY KEY,
 tenant_id uuid NOT NULL REFERENCES hfsm.tenant(id),
 instance_id uuid NOT NULL REFERENCES hfsm.instance(id),
 actor text NOT NULL CHECK(length(actor)>0),
 action text NOT NULL CHECK(action IN ('cancel','redrive','resolve_outbox')),
 reason text NOT NULL,
 observed_revision bigint NOT NULL,
 metadata jsonb NOT NULL DEFAULT '{}'::jsonb,
 created_at timestamptz NOT NULL DEFAULT clock_timestamp()
);
CREATE TRIGGER operator_action_immutable BEFORE UPDATE OR DELETE ON hfsm.operator_action
 FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();

CREATE TABLE hfsm.dead_letter (
 id bigint GENERATED ALWAYS AS IDENTITY PRIMARY KEY,
 tenant_id uuid NOT NULL REFERENCES hfsm.tenant(id),
 instance_id uuid NOT NULL REFERENCES hfsm.instance(id),
 source_kind text NOT NULL CHECK(source_kind IN ('signal','outbox')),
 source_id uuid NOT NULL,
 reason text NOT NULL,
 classification text NOT NULL DEFAULT 'permanent',
 payload jsonb NOT NULL,
 recorded_at timestamptz NOT NULL DEFAULT clock_timestamp(),
 UNIQUE(source_kind,source_id)
);
CREATE TRIGGER dead_letter_immutable BEFORE UPDATE OR DELETE ON hfsm.dead_letter
 FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();
CREATE INDEX dead_letter_tenant_idx ON hfsm.dead_letter(tenant_id,recorded_at DESC,id DESC);

-- Metadata passed with enqueue is immutable intention, not delivery-state data.
CREATE OR REPLACE FUNCTION hfsm.guard_signal_intent() RETURNS trigger
 LANGUAGE plpgsql AS $$ BEGIN
 IF (OLD.id,OLD.instance_id,OLD.ordinal,OLD.payload,OLD.cause,OLD.dedupe_key,
     OLD.created_at,OLD.tenant_id,OLD.priority,OLD.correlation_id,OLD.causation_id,OLD.traceparent)
 IS DISTINCT FROM
    (NEW.id,NEW.instance_id,NEW.ordinal,NEW.payload,NEW.cause,NEW.dedupe_key,
     NEW.created_at,NEW.tenant_id,NEW.priority,NEW.correlation_id,NEW.causation_id,NEW.traceparent)
 THEN RAISE EXCEPTION 'immutable HFSM signal intent' USING ERRCODE='23514'; END IF;
 RETURN NEW; END $$;
CREATE OR REPLACE FUNCTION hfsm.guard_outbox_intent() RETURNS trigger
 LANGUAGE plpgsql AS $$ BEGIN
 IF (OLD.id,OLD.instance_id,OLD.step_id,OLD.ordinal,OLD.effect_key,OLD.payload,
     OLD.created_at,OLD.tenant_id,OLD.correlation_id,OLD.causation_id,OLD.traceparent)
 IS DISTINCT FROM
    (NEW.id,NEW.instance_id,NEW.step_id,NEW.ordinal,NEW.effect_key,NEW.payload,
     NEW.created_at,NEW.tenant_id,NEW.correlation_id,NEW.causation_id,NEW.traceparent)
 THEN RAISE EXCEPTION 'immutable HFSM outbox intent' USING ERRCODE='23514'; END IF;
 RETURN NEW; END $$;

CREATE FUNCTION hfsm.ops_instance_insert() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE t hfsm.tenant%ROWTYPE; parent_t uuid;
BEGIN
 IF current_setting('hfsm.tenant_id',true) IS NOT NULL AND
    current_setting('hfsm.tenant_id',true) <> '' THEN
   NEW.tenant_id := current_setting('hfsm.tenant_id',true)::uuid;
 END IF;
 IF current_setting('hfsm.group_key',true) IS NOT NULL AND
    current_setting('hfsm.group_key',true) <> '' THEN
   NEW.concurrency_group := current_setting('hfsm.group_key',true);
 END IF;
 SELECT * INTO t FROM hfsm.tenant WHERE id=NEW.tenant_id FOR UPDATE;
 IF NOT FOUND OR NOT t.enabled THEN RAISE EXCEPTION 'tenant disabled or missing' USING ERRCODE='23514'; END IF;
 IF NEW.parent_instance_id IS NOT NULL THEN
   SELECT tenant_id INTO parent_t FROM hfsm.instance WHERE id=NEW.parent_instance_id;
   IF parent_t IS DISTINCT FROM NEW.tenant_id THEN
     RAISE EXCEPTION 'child instance cannot cross tenants' USING ERRCODE='23514';
   END IF;
 END IF;
 IF (SELECT count(*) FROM hfsm.instance i WHERE i.tenant_id=NEW.tenant_id
      AND i.status IN ('running','waiting','paused')) >= t.max_active_instances THEN
   RAISE EXCEPTION 'tenant active instance quota exceeded' USING ERRCODE='23514';
 END IF;
 RETURN NEW;
END $$;
CREATE TRIGGER ops_instance_insert BEFORE INSERT ON hfsm.instance
 FOR EACH ROW EXECUTE FUNCTION hfsm.ops_instance_insert();

CREATE FUNCTION hfsm.ops_signal_insert() RETURNS trigger LANGUAGE plpgsql AS $$
DECLARE t hfsm.tenant%ROWTYPE; instance_t uuid;
BEGIN
 SELECT i.tenant_id INTO instance_t FROM hfsm.instance i WHERE i.id=NEW.instance_id;
 IF instance_t IS NULL THEN RAISE EXCEPTION 'signal instance missing' USING ERRCODE='23503'; END IF;
 NEW.tenant_id:=instance_t;
 SELECT * INTO t FROM hfsm.tenant WHERE id=instance_t FOR UPDATE;
 IF NOT t.enabled THEN RAISE EXCEPTION 'tenant disabled' USING ERRCODE='23514'; END IF;
 IF (SELECT count(*) FROM hfsm.signal s WHERE s.tenant_id=instance_t
     AND s.status IN ('entered','leased')) >= t.max_pending_signals THEN
   RAISE EXCEPTION 'tenant pending signal quota exceeded' USING ERRCODE='23514';
 END IF;
 RETURN NEW;
END $$;
CREATE TRIGGER ops_signal_insert BEFORE INSERT ON hfsm.signal
 FOR EACH ROW EXECUTE FUNCTION hfsm.ops_signal_insert();

-- Preserve caller-supplied signal correlation from the turn which caused an effect.
CREATE FUNCTION hfsm.ops_outbox_insert() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 SELECT i.tenant_id,s.correlation_id,s.id,s.traceparent
 INTO NEW.tenant_id,NEW.correlation_id,NEW.causation_id,NEW.traceparent
 FROM hfsm.instance i
 JOIN hfsm.step st ON st.instance_id=i.id AND st.id=NEW.step_id
 LEFT JOIN hfsm.signal s ON s.id=st.signal_id
 WHERE i.id=NEW.instance_id;
 RETURN NEW;
END $$;
CREATE TRIGGER ops_outbox_insert BEFORE INSERT ON hfsm.outbox
 FOR EACH ROW EXECUTE FUNCTION hfsm.ops_outbox_insert();

-- Dead-letter creation is transactional with the failure status itself.
CREATE FUNCTION hfsm.ops_dead_letter() RETURNS trigger LANGUAGE plpgsql AS $$
BEGIN
 IF TG_TABLE_NAME='signal' AND NEW.status='failed' AND OLD.status IS DISTINCT FROM 'failed' THEN
   INSERT INTO hfsm.dead_letter(tenant_id,instance_id,source_kind,source_id,reason,payload)
   VALUES (NEW.tenant_id,NEW.instance_id,'signal',NEW.id,
           coalesce(NEW.last_error,'signal failed'),NEW.payload)
   ON CONFLICT(source_kind,source_id) DO NOTHING;
 ELSIF TG_TABLE_NAME='outbox' AND NEW.status='failed' AND OLD.status IS DISTINCT FROM 'failed' THEN
   INSERT INTO hfsm.dead_letter(tenant_id,instance_id,source_kind,source_id,reason,classification,payload)
   VALUES (NEW.tenant_id,NEW.instance_id,'outbox',NEW.id,
           coalesce(NEW.last_error,'effect failed'),coalesce(NEW.failure_class,'permanent'),NEW.payload)
   ON CONFLICT(source_kind,source_id) DO NOTHING;
 END IF;
 RETURN NEW;
END $$;
CREATE TRIGGER ops_signal_dead_letter AFTER UPDATE ON hfsm.signal
 FOR EACH ROW EXECUTE FUNCTION hfsm.ops_dead_letter();
CREATE TRIGGER ops_outbox_dead_letter AFTER UPDATE ON hfsm.outbox
 FOR EACH ROW EXECUTE FUNCTION hfsm.ops_dead_letter();

-- Production enqueue preserves CT-05 release pinning and CT-03 dedupe.
CREATE FUNCTION hfsm.ops_enqueue_signal(
 p_instance uuid,p_signal uuid,p_payload jsonb,p_cause jsonb,p_dedupe text,
 p_priority smallint,p_correlation uuid,p_causation uuid,p_traceparent text,p_release text,
 p_ready_at timestamptz
) RETURNS uuid LANGUAGE plpgsql AS $$
DECLARE i hfsm.instance%ROWTYPE; prev uuid; seqno bigint; expected text;
BEGIN
 SELECT * INTO i FROM hfsm.instance WHERE id=p_instance FOR UPDATE;
 IF NOT FOUND THEN RAISE EXCEPTION 'instance missing' USING ERRCODE='23503'; END IF;
 IF i.status IN ('done','failed','cancelled') THEN
   RAISE EXCEPTION 'cannot enqueue to terminal instance' USING ERRCODE='23514';
 END IF;
 expected:=i.machine_name||'@'||i.machine_version||'#'||i.spec_digest;
 IF p_release IS NOT NULL AND p_release<>expected THEN
   RAISE EXCEPTION 'producer release mismatch' USING ERRCODE='23514';
 END IF;
 IF EXISTS(SELECT 1 FROM hfsm.migration_event WHERE instance_id=i.id)
    AND p_release IS NULL THEN
   RAISE EXCEPTION 'migrated instance needs pinned release' USING ERRCODE='23514';
 END IF;
 IF p_dedupe IS NOT NULL THEN
   SELECT id INTO prev FROM hfsm.signal WHERE instance_id=p_instance AND dedupe_key=p_dedupe;
   IF FOUND THEN RETURN prev; END IF;
 END IF;
 IF p_release IS NOT NULL THEN PERFORM set_config('hfsm.enqueue_release',p_release,true); END IF;
 UPDATE hfsm.instance SET next_signal_ordinal=next_signal_ordinal+1 WHERE id=p_instance
   RETURNING next_signal_ordinal-1 INTO seqno;
 INSERT INTO hfsm.signal(id,instance_id,ordinal,status,payload,cause,dedupe_key,
   priority,correlation_id,causation_id,traceparent,ready_at)
 VALUES(p_signal,p_instance,seqno,'entered',p_payload,p_cause,p_dedupe,
   p_priority,coalesce(p_correlation,p_signal),p_causation,p_traceparent,p_ready_at);
 RETURN p_signal;
END $$;

-- Lifecycle registry: only a holder of the current generation can heartbeat.
CREATE FUNCTION hfsm.ops_register_worker(
 p_worker uuid,p_caps text[],p_ttl integer,p_sig integer,p_act integer
) RETURNS uuid LANGUAGE plpgsql AS $$ DECLARE g uuid;
BEGIN
 g:=gen_random_uuid();
 INSERT INTO hfsm.worker_registry(id,capabilities,ttl_seconds,max_signals,max_activities,
   state,heartbeat_at,generation,started_at)
 VALUES(p_worker,p_caps,p_ttl,p_sig,p_act,'active',clock_timestamp(),g,clock_timestamp())
 ON CONFLICT(id) DO UPDATE SET capabilities=excluded.capabilities,ttl_seconds=excluded.ttl_seconds,
  max_signals=excluded.max_signals,max_activities=excluded.max_activities,
  state='active',heartbeat_at=excluded.heartbeat_at,generation=excluded.generation,
  started_at=excluded.started_at
 RETURNING generation INTO g;
 RETURN g;
END $$;
CREATE FUNCTION hfsm.ops_heartbeat_worker(p_worker uuid,p_generation uuid)
 RETURNS boolean LANGUAGE plpgsql AS $$ DECLARE answer boolean;
BEGIN
 UPDATE hfsm.worker_registry SET heartbeat_at=clock_timestamp()
 WHERE id=p_worker AND generation=p_generation AND state IN ('active','draining')
 RETURNING true INTO answer;
 RETURN coalesce(answer,false);
END $$;

-- Reap exhausted *signals* under the same gate and instance->signal lock order.
CREATE FUNCTION hfsm.ops_reap_exhausted_signals() RETURNS integer LANGUAGE plpgsql AS $$
DECLARE r record; n integer:=0;
BEGIN
 FOR r IN
  SELECT i.id iid,s.id sid FROM hfsm.instance i
  JOIN hfsm.signal s ON s.id=i.in_flight_signal_id AND s.instance_id=i.id
  JOIN hfsm.tenant t ON t.id=i.tenant_id
  WHERE s.status='leased' AND s.lease_until<=clock_timestamp()
    AND s.attempts >= t.max_signal_attempts
  ORDER BY i.uid LIMIT 100 FOR UPDATE OF i SKIP LOCKED
 LOOP
  UPDATE hfsm.signal SET status='failed',lease_id=NULL,lease_owner=NULL,lease_until=NULL,
    last_error='retry budget exhausted',updated_at=clock_timestamp()
  WHERE id=r.sid AND status='leased' AND lease_until<=clock_timestamp();
  UPDATE hfsm.instance SET in_flight_signal_id=NULL,in_flight_lease_id=NULL,
    in_flight_lease_until=NULL WHERE id=r.iid AND in_flight_signal_id=r.sid;
  n:=n+1;
 END LOOP;
 RETURN n;
END $$;

-- Claim one at a time inside a transaction holding the shared admission gate.
-- The FIFO head is selected before priority ordering; no priority can leapfrog
-- an older input on the SAME instance.
CREATE FUNCTION hfsm.ops_claim_signal_one(p_node uuid,p_seconds integer,p_instance uuid)
 RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE w hfsm.worker_registry%ROWTYPE; target uuid; sid uuid; rec hfsm.signal%ROWTYPE;
BEGIN
 SELECT * INTO w FROM hfsm.worker_registry WHERE id=p_node;
 IF NOT FOUND OR w.state<>'active' OR
    w.heartbeat_at + (w.ttl_seconds*interval '1 second') <= clock_timestamp() OR
    NOT ('hfsm:signal'=ANY(w.capabilities) OR 'hfsm:*'=ANY(w.capabilities)) THEN
   RETURN NULL;
 END IF;
 IF (SELECT count(*) FROM hfsm.signal WHERE lease_owner=p_node
     AND status='leased' AND lease_until>clock_timestamp())>=w.max_signals THEN RETURN NULL; END IF;
 SELECT i.id,head.id INTO target,sid
 FROM hfsm.instance i
 JOIN hfsm.tenant t ON t.id=i.tenant_id AND t.enabled
 JOIN hfsm.concurrency_group g ON g.tenant_id=i.tenant_id AND g.group_key=i.concurrency_group
 JOIN LATERAL(
   SELECT s.id,s.status,s.lease_until,s.ready_at,s.priority
   FROM hfsm.signal s WHERE s.instance_id=i.id AND s.status IN ('entered','leased')
   ORDER BY s.ordinal LIMIT 1
 ) head ON true
 WHERE (p_instance IS NULL OR i.id=p_instance)
   AND i.status IN ('running','waiting','paused')
   AND (i.in_flight_signal_id IS NULL OR i.in_flight_lease_until<=clock_timestamp())
   AND ((head.status='entered' AND head.ready_at<=clock_timestamp())
        OR (head.status='leased' AND head.lease_until<=clock_timestamp()))
   AND (SELECT count(*) FROM hfsm.signal s WHERE s.tenant_id=i.tenant_id
         AND s.status='leased' AND s.lease_until>clock_timestamp()) < t.max_signal_inflight
   AND (SELECT count(*) FROM hfsm.signal s JOIN hfsm.instance i2 ON i2.id=s.instance_id
         WHERE i2.tenant_id=i.tenant_id AND i2.concurrency_group=i.concurrency_group
           AND s.status='leased' AND s.lease_until>clock_timestamp()) < g.max_signals
 -- Bounded age bonus ensures lower priorities eventually make progress.
 ORDER BY (i.priority+head.priority + LEAST(40,GREATEST(0,
   floor(EXTRACT(epoch FROM (clock_timestamp()-head.ready_at))/60)::integer))) DESC,
   i.updated_at, i.uid
 LIMIT 1 FOR UPDATE OF i SKIP LOCKED;
 IF NOT FOUND THEN RETURN NULL; END IF;
 UPDATE hfsm.signal SET status='leased',lease_id=gen_random_uuid(),lease_owner=p_node,
     lease_until=clock_timestamp()+(p_seconds*interval '1 second'),attempts=attempts+1,
     updated_at=clock_timestamp()
   WHERE id=sid RETURNING * INTO rec;
 UPDATE hfsm.instance SET in_flight_signal_id=sid,in_flight_lease_id=rec.lease_id,
   in_flight_lease_until=rec.lease_until WHERE id=target;
 RETURN jsonb_build_object('uid',rec.uid,'uuid',rec.id,'inst',rec.instance_id,
   'status','LeasedSs','payload',rec.payload,'cause',rec.cause,'lease',rec.lease_id,
   'ordinal',rec.ordinal,'leaseOwner',rec.lease_owner,'leaseUntil',rec.lease_until,
   'attempts',rec.attempts,'readyAt',rec.ready_at,
   'createdAt',rec.created_at,'updatedAt',rec.updated_at);
END $$;

CREATE FUNCTION hfsm.ops_claim_signals(p_node uuid,p_batch integer,p_seconds integer,p_instance uuid)
 RETURNS SETOF jsonb LANGUAGE plpgsql AS $$
DECLARE j jsonb; ix integer;
BEGIN
 PERFORM 1 FROM hfsm.admission_gate WHERE id=1 FOR UPDATE;
 PERFORM hfsm.ops_reap_exhausted_signals();
 FOR ix IN 1..LEAST(100,GREATEST(1,p_batch)) LOOP
   j:=hfsm.ops_claim_signal_one(p_node,LEAST(3600,GREATEST(1,p_seconds)),p_instance);
   EXIT WHEN j IS NULL;
   RETURN NEXT j;
 END LOOP;
END $$;


-- Unacknowledged effects of a cancelled instance must not remain READY forever.
-- A lease which has expired may have reached the provider: record UNKNOWN.
CREATE FUNCTION hfsm.ops_reap_cancelled_effects() RETURNS integer LANGUAGE plpgsql AS $$
DECLARE n integer:=0; found_count integer;
BEGIN
 WITH expired AS MATERIALIZED (
   SELECT o.id,o.lease_id FROM hfsm.outbox o
   JOIN hfsm.instance i ON i.id=o.instance_id
   WHERE i.status='cancelled' AND o.status='leased'
     AND o.lease_until<=clock_timestamp()
   ORDER BY o.lease_until,o.uid LIMIT 100 FOR UPDATE OF o SKIP LOCKED
 ), changed AS (
   UPDATE hfsm.outbox o SET status='failed',failure_class='unknown',
     last_error='cancelled instance: expired delivery outcome unknown',
     completed_at=clock_timestamp(),updated_at=clock_timestamp(),
     lease_id=NULL,lease_owner=NULL,lease_until=NULL
   FROM expired e WHERE o.id=e.id RETURNING o.id
 )
 UPDATE hfsm.activity_attempt a SET outcome='lease_expired',
   finished_at=clock_timestamp(),error_text='cancelled: expired external effect'
 FROM expired e JOIN changed c ON c.id=e.id
 WHERE a.outbox_id=e.id AND a.lease_id=e.lease_id AND a.outcome='leased';
 GET DIAGNOSTICS found_count=ROW_COUNT;
 n:=n+found_count;
 UPDATE hfsm.outbox o SET status='failed',failure_class='permanent',
    last_error='cancelled instance: retry suppressed',completed_at=clock_timestamp(),
    updated_at=clock_timestamp()
 FROM hfsm.instance i
 WHERE o.instance_id=i.id AND i.status='cancelled' AND o.status='ready';
 GET DIAGNOSTICS found_count=ROW_COUNT;
 RETURN n+found_count;
END $$;

-- Only failed *signals* can be redriven without external-provider reconciliation.
-- New input obtains a new ordinal; original history and DLQ stay immutable.
CREATE TABLE hfsm.redrive_event (
 operation_id uuid PRIMARY KEY,
 dead_letter_id bigint NOT NULL UNIQUE REFERENCES hfsm.dead_letter(id) ON DELETE RESTRICT,
 new_signal_id uuid NOT NULL UNIQUE REFERENCES hfsm.signal(id) ON DELETE RESTRICT,
 created_at timestamptz NOT NULL DEFAULT clock_timestamp()
);
CREATE TRIGGER redrive_event_immutable BEFORE UPDATE OR DELETE ON hfsm.redrive_event
 FOR EACH ROW EXECUTE FUNCTION hfsm.reject_history_mutation();
CREATE FUNCTION hfsm.ops_redrive_signal(p_dead_letter bigint,p_new_signal uuid,
 p_operation uuid,p_actor text,p_reason text,p_release text)
 RETURNS uuid LANGUAGE plpgsql AS $$
DECLARE d hfsm.dead_letter%ROWTYPE; prior uuid; original hfsm.signal%ROWTYPE;
  next_signal uuid; seen_deadletter bigint; seen_operation uuid;
BEGIN
 SELECT new_signal_id,dead_letter_id INTO prior,seen_deadletter FROM hfsm.redrive_event
 WHERE operation_id=p_operation;
 IF FOUND THEN
   IF seen_deadletter<>p_dead_letter THEN
     RAISE EXCEPTION 'operator redrive ID reused with another dead letter' USING ERRCODE='23514';
   END IF;
   RETURN prior;
 END IF;
 SELECT new_signal_id,operation_id INTO prior,seen_operation FROM hfsm.redrive_event
 WHERE dead_letter_id=p_dead_letter;
 IF FOUND THEN
   RAISE EXCEPTION 'dead letter already redriven by operation %',seen_operation USING ERRCODE='23514';
 END IF;
 SELECT * INTO d FROM hfsm.dead_letter WHERE id=p_dead_letter;
 IF NOT FOUND OR d.source_kind<>'signal' THEN
   RAISE EXCEPTION 'only failed signal dead letters can be redriven' USING ERRCODE='23514';
 END IF;
 SELECT * INTO original FROM hfsm.signal WHERE id=d.source_id;
 IF NOT FOUND OR original.status<>'failed' THEN
   RAISE EXCEPTION 'dead-letter source is not a failed signal' USING ERRCODE='23514';
 END IF;
 next_signal:=hfsm.ops_enqueue_signal(d.instance_id,p_new_signal,original.payload,original.cause,
    'hfsm/redrive/'||d.id::text,original.priority,original.correlation_id,
    original.id,original.traceparent,p_release,clock_timestamp());
 INSERT INTO hfsm.redrive_event(operation_id,dead_letter_id,new_signal_id)
 VALUES(p_operation,p_dead_letter,next_signal);
 INSERT INTO hfsm.operator_action(operation_id,tenant_id,instance_id,actor,action,reason,
    observed_revision,metadata)
 SELECT p_operation,d.tenant_id,d.instance_id,p_actor,'redrive',p_reason,i.revision,
   jsonb_build_object('deadLetterId',d.id,'newSignalId',next_signal)
 FROM hfsm.instance i WHERE i.id=d.instance_id;
 RETURN next_signal;
END $$;
REVOKE ALL ON FUNCTION hfsm.ops_redrive_signal(bigint,uuid,uuid,text,text,text) FROM PUBLIC;

-- CT-04 reaping still executes in the Haskell adapter. A recovered effect
-- keeps the original durable replay-safe flag and idempotency key.
CREATE FUNCTION hfsm.ops_claim_activity_one(
 p_node uuid,p_seconds integer,p_instance uuid,p_safe text[],p_supported text[]
) RETURNS jsonb LANGUAGE plpgsql AS $$
DECLARE w hfsm.worker_registry%ROWTYPE; eid uuid; o hfsm.outbox%ROWTYPE; kind text;
BEGIN
 SELECT * INTO w FROM hfsm.worker_registry WHERE id=p_node;
 IF NOT FOUND OR w.state<>'active' OR
    w.heartbeat_at+(w.ttl_seconds*interval '1 second')<=clock_timestamp() THEN RETURN NULL; END IF;
 IF (SELECT count(*) FROM hfsm.outbox WHERE lease_owner=p_node
      AND status='leased' AND lease_until>clock_timestamp()) >= w.max_activities THEN RETURN NULL; END IF;
 SELECT ob.id INTO eid FROM hfsm.outbox ob
 JOIN hfsm.instance i ON i.id=ob.instance_id
 JOIN hfsm.tenant t ON t.id=i.tenant_id AND t.enabled
 JOIN hfsm.concurrency_group g ON g.tenant_id=i.tenant_id AND g.group_key=i.concurrency_group
 WHERE (p_instance IS NULL OR i.id=p_instance)
   AND i.status<>'cancelled'
   AND ob.attempts<ob.max_attempts
   AND ob.payload->>'kind'=ANY(p_supported)
   AND (('activity:'||(ob.payload->>'kind'))=ANY(w.capabilities)
       OR 'activity:*'=ANY(w.capabilities) OR 'hfsm:*'=ANY(w.capabilities))
   AND ((ob.status='ready' AND ob.available_at<=clock_timestamp())
       OR (ob.status='leased' AND ob.lease_until<=clock_timestamp() AND ob.replay_safe))
   AND (SELECT count(*) FROM hfsm.outbox x WHERE x.tenant_id=i.tenant_id
       AND x.status='leased' AND x.lease_until>clock_timestamp()) < t.max_activity_inflight
   AND (SELECT count(*) FROM hfsm.outbox x JOIN hfsm.instance i2 ON i2.id=x.instance_id
       WHERE i2.tenant_id=i.tenant_id AND i2.concurrency_group=i.concurrency_group
        AND x.status='leased' AND x.lease_until>clock_timestamp()) < g.max_activities
 ORDER BY i.priority DESC,ob.available_at,ob.uid
 LIMIT 1 FOR UPDATE OF ob SKIP LOCKED;
 IF NOT FOUND THEN RETURN NULL; END IF;
 -- Preserve attempt history of a crashed/expired worker before reclaim.
 UPDATE hfsm.activity_attempt SET outcome='lease_expired',finished_at=clock_timestamp(),
   error_text='reclaimed after worker lease expiry'
 WHERE outbox_id=eid AND outcome='leased';
 UPDATE hfsm.outbox SET status='leased',lease_id=gen_random_uuid(),lease_owner=p_node,
    lease_until=clock_timestamp()+(p_seconds*interval '1 second'),
    attempts=attempts+1,updated_at=clock_timestamp(),
    replay_safe=CASE WHEN attempts=0 THEN payload->>'kind'=ANY(p_safe) ELSE replay_safe END
 WHERE id=eid RETURNING * INTO o;
 INSERT INTO hfsm.activity_attempt(outbox_id,attempt_no,lease_id,worker_id,outcome)
 VALUES(o.id,o.attempts,o.lease_id,p_node,'leased');
 RETURN jsonb_build_object('effectId',o.id,'instanceId',o.instance_id,
  'stepId',o.step_id,'effectKey',o.effect_key,'payload',o.payload,
  'attempt',o.attempts,'maxAttempts',o.max_attempts,'timeoutSeconds',o.timeout_seconds,
  'retryBaseSeconds',o.retry_base_seconds,'retryCapSeconds',o.retry_cap_seconds,
  'replaySafe',o.replay_safe,'leaseId',o.lease_id,'leaseUntil',o.lease_until);
END $$;
CREATE FUNCTION hfsm.ops_claim_activities(p_node uuid,p_batch integer,p_seconds integer,
 p_instance uuid,p_safe text[],p_supported text[])
 RETURNS SETOF jsonb LANGUAGE plpgsql AS $$
DECLARE j jsonb; ix integer;
BEGIN
 PERFORM 1 FROM hfsm.admission_gate WHERE id=1 FOR UPDATE;
 PERFORM hfsm.ops_reap_cancelled_effects();
 FOR ix IN 1..LEAST(100,GREATEST(1,p_batch)) LOOP
   j:=hfsm.ops_claim_activity_one(p_node,LEAST(3600,GREATEST(1,p_seconds)),p_instance,p_safe,p_supported);
   EXIT WHEN j IS NULL;
   RETURN NEXT j;
 END LOOP;
END $$;

-- Operators cannot revoke external side effects in flight; those may complete
-- later, so their outcome remains in the CT-04 activity log.
CREATE FUNCTION hfsm.ops_cancel_instance(p_id uuid,p_expected bigint,p_op uuid,p_actor text,p_reason text)
 RETURNS text LANGUAGE plpgsql AS $$
DECLARE i hfsm.instance%ROWTYPE;
BEGIN
 SELECT * INTO i FROM hfsm.instance WHERE id=p_id FOR UPDATE;
 IF NOT FOUND THEN RETURN 'not_found'; END IF;
 IF i.status='cancelled' THEN RETURN 'already_cancelled'; END IF;
 IF i.status IN ('done','failed') THEN RETURN 'terminal'; END IF;
 IF i.revision<>p_expected THEN RETURN 'conflict'; END IF;
 INSERT INTO hfsm.operator_action(operation_id,tenant_id,instance_id,actor,action,reason,observed_revision)
 VALUES(p_op,i.tenant_id,i.id,p_actor,'cancel',p_reason,i.revision);
 IF i.in_flight_signal_id IS NOT NULL THEN
  UPDATE hfsm.signal SET status='rejected',lease_id=NULL,lease_owner=NULL,lease_until=NULL,
    last_error='operator cancelled: '||p_reason,updated_at=clock_timestamp()
  WHERE id=i.in_flight_signal_id AND status='leased';
 END IF;
 UPDATE hfsm.signal SET status='rejected',last_error='operator cancelled: '||p_reason,
    updated_at=clock_timestamp()
  WHERE instance_id=i.id AND status='entered';
 UPDATE hfsm.instance SET status='cancelled',in_flight_signal_id=NULL,
    in_flight_lease_id=NULL,in_flight_lease_until=NULL,updated_at=clock_timestamp()
 WHERE id=i.id;
 -- Unclaimed effects can be prevented. Leased effects remain ambiguous.
 UPDATE hfsm.outbox SET status='failed',failure_class='permanent',
   last_error='operator cancelled before delivery',completed_at=clock_timestamp(),
   updated_at=clock_timestamp()
 WHERE instance_id=i.id AND status='ready';
 RETURN 'cancelled';
END $$;

-- Do not let arbitrary database users perform operator mutations.
REVOKE ALL ON FUNCTION hfsm.ops_cancel_instance(uuid,bigint,uuid,text,text) FROM PUBLIC;

-- Derived read models; deletable/rebuildable without mutating history.
CREATE TABLE hfsm.readmodel_event (
 consumer text NOT NULL,instance_id uuid NOT NULL,revision bigint NOT NULL,
 kind text NOT NULL,ordinal integer NOT NULL,payload jsonb NOT NULL,
 PRIMARY KEY(consumer,instance_id,revision,kind,ordinal)
);
CREATE INDEX readmodel_event_instance_idx ON hfsm.readmodel_event(consumer,instance_id,revision);
CREATE TABLE hfsm.readmodel_active (
 consumer text NOT NULL,instance_id uuid NOT NULL,
 revision bigint NOT NULL,status text NOT NULL, snapshot_id uuid NOT NULL,
 payload jsonb NOT NULL,updated_at timestamptz NOT NULL DEFAULT clock_timestamp(),
 PRIMARY KEY(consumer,instance_id)
);

CREATE FUNCTION hfsm.ops_project_instance(p_consumer text,p_instance uuid,p_limit integer)
 RETURNS integer LANGUAGE plpgsql AS $$
DECLARE fromrev bigint; nextrev bigint; s record; processed integer:=0; finalstatus text;
BEGIN
 INSERT INTO hfsm.projection_progress(consumer,instance_id,last_revision)
 VALUES(p_consumer,p_instance,0) ON CONFLICT DO NOTHING;
 SELECT last_revision INTO fromrev FROM hfsm.projection_progress
   WHERE consumer=p_consumer AND instance_id=p_instance FOR UPDATE;
 IF NOT FOUND THEN RETURN 0; END IF;
 FOR s IN
   SELECT st.revision,st.id step_id,st.note,st.created_at,
     sn.id snapshot_id,sn.state_ref,sn.state_path,sn.ctx,sn.wait_value,
     sn.machine_version,sn.spec_digest
   FROM hfsm.step st
   JOIN hfsm.snapshot sn ON sn.instance_id=st.instance_id AND sn.produced_by_step_id=st.id
   WHERE st.instance_id=p_instance AND st.revision>fromrev
   ORDER BY st.revision LIMIT LEAST(1000,GREATEST(1,p_limit))
 LOOP
   IF s.revision <> fromrev+1 THEN
     RAISE EXCEPTION 'projection revision gap instance % after %',p_instance,fromrev;
   END IF;
   INSERT INTO hfsm.readmodel_event(consumer,instance_id,revision,kind,ordinal,payload)
     SELECT p_consumer,p_instance,p.revision,p.kind,p.ordinal,p.payload
     FROM hfsm.projection_event p WHERE p.instance_id=p_instance AND p.revision=s.revision
     ON CONFLICT DO NOTHING;
   finalstatus:=coalesce(s.note->>'status','running');
   INSERT INTO hfsm.readmodel_active(consumer,instance_id,revision,status,snapshot_id,payload)
   VALUES(p_consumer,p_instance,s.revision,finalstatus,s.snapshot_id,
     jsonb_build_object('instance',p_instance,'revision',s.revision,'snapshot',s.snapshot_id,
       'state',s.state_ref,'path',s.state_path,'ctx',s.ctx,'wait',s.wait_value,
       'version',s.machine_version,'digest',s.spec_digest,'status',finalstatus,
       'step',s.step_id,'createdAt',s.created_at))
   ON CONFLICT(consumer,instance_id) DO UPDATE SET
     revision=excluded.revision,status=excluded.status,snapshot_id=excluded.snapshot_id,
     payload=excluded.payload,updated_at=clock_timestamp()
   WHERE hfsm.readmodel_active.revision<=excluded.revision;
   fromrev:=s.revision;
   processed:=processed+1;
 END LOOP;
 -- Cancellation updates operator state, not a transition revision. Overlay
 -- the authoritative current status when the projection caught up.
 UPDATE hfsm.readmodel_active rm SET status='cancelled',
   payload=jsonb_set(rm.payload,'{status}','"cancelled"'::jsonb),updated_at=clock_timestamp()
 FROM hfsm.instance i WHERE rm.consumer=p_consumer AND rm.instance_id=i.id AND i.id=p_instance
   AND i.status='cancelled' AND rm.revision=i.revision AND rm.status<>'cancelled';
 UPDATE hfsm.projection_progress SET last_revision=fromrev,updated_at=clock_timestamp()
 WHERE consumer=p_consumer AND instance_id=p_instance;
 RETURN processed;
END $$;
-- Admin-only rebuild: safe because all deletes affect derived data only.
CREATE FUNCTION hfsm.ops_reset_projection(p_consumer text,p_instance uuid)
 RETURNS integer LANGUAGE plpgsql AS $$
BEGIN
 DELETE FROM hfsm.readmodel_event WHERE consumer=p_consumer AND instance_id=p_instance;
 DELETE FROM hfsm.readmodel_active WHERE consumer=p_consumer AND instance_id=p_instance;
 DELETE FROM hfsm.projection_progress WHERE consumer=p_consumer AND instance_id=p_instance;
 RETURN 1;
END $$;
REVOKE ALL ON FUNCTION hfsm.ops_reset_projection(text,uuid) FROM PUBLIC;

COMMENT ON TABLE hfsm.admission_gate IS 'CT06 conservative globally serialized queue admission; optimize only after contention tests';
COMMENT ON TABLE hfsm.operator_action IS 'Immutable operator audit. Cancel is an operational terminal overlay, not a normal HFSM transition revision.';
COMMENT ON TABLE hfsm.dead_letter IS 'Append-only failed signal/outbox evidence; redrive creates a NEW signal with its own ordinal.';

-- Administration of per-group limits, serializable with admission decisions.
CREATE FUNCTION hfsm.ops_set_group_limits(p_tenant uuid,p_group text,p_signals integer,p_activities integer)
 RETURNS boolean LANGUAGE plpgsql AS $$
BEGIN
 PERFORM 1 FROM hfsm.admission_gate WHERE id=1 FOR UPDATE;
 INSERT INTO hfsm.concurrency_group(tenant_id,group_key,max_signals,max_activities)
 VALUES(p_tenant,p_group,p_signals,p_activities)
 ON CONFLICT(tenant_id,group_key) DO UPDATE SET
   max_signals=excluded.max_signals,max_activities=excluded.max_activities;
 RETURN true;
END $$;
REVOKE ALL ON FUNCTION hfsm.ops_set_group_limits(uuid,text,integer,integer) FROM PUBLIC;

-- Function execution privileges: deployment must explicitly GRANT these to
-- runtime/operator/projector roles. Table ownership alone is not tenant auth.
REVOKE ALL ON FUNCTION hfsm.ops_register_worker(uuid,text[],integer,integer,integer) FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.ops_heartbeat_worker(uuid,uuid) FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.ops_claim_signals(uuid,integer,integer,uuid) FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.ops_claim_signal_one(uuid,integer,uuid) FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.ops_claim_activities(uuid,integer,integer,uuid,text[],text[]) FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.ops_claim_activity_one(uuid,integer,uuid,text[],text[]) FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.ops_enqueue_signal(uuid,uuid,jsonb,jsonb,text,smallint,uuid,uuid,text,text,timestamptz) FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.ops_reap_exhausted_signals() FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.ops_reap_cancelled_effects() FROM PUBLIC;
REVOKE ALL ON FUNCTION hfsm.ops_project_instance(text,uuid,integer) FROM PUBLIC;
