-- Optional NON-DESTRUCTIVE smoke test, run AFTER 001_hfsm_runtime.sql.
-- It creates a release, initial instance, initial snapshot, leased signal,
-- successful turn + one outbox effect, then ROLLBACKs everything.
-- This does not validate a concurrent production Store implementation.

BEGIN;

INSERT INTO hfsm.machine_release (machine_name, machine_version, spec_digest)
VALUES ('ct02.smoke', '1', repeat('a',64));

INSERT INTO hfsm.instance
  (id, machine_name, machine_version, spec_digest,
   current_state_ref, current_path, current_snapshot_id)
VALUES
  ('00000000-0000-4000-8000-000000001001',
   'ct02.smoke', '1', repeat('a',64),
   1, '[1]'::jsonb, '00000000-0000-4000-8000-000000001002');

-- This succeeds because instance_current_snapshot_fk is deferred.
INSERT INTO hfsm.snapshot
  (id,instance_id,revision,produced_by_step_id,state_ref,state_path,
   ctx,wait_value,child_value,machine_version,spec_digest,created_at)
VALUES
  ('00000000-0000-4000-8000-000000001002',
   '00000000-0000-4000-8000-000000001001',0,NULL,1,'[1]'::jsonb,
   '{}'::jsonb,NULL,'{}'::jsonb,'1',repeat('a',64),clock_timestamp());

-- Under a real adapter the following admission step is performed by the
-- lease transaction, not manually by the caller.
INSERT INTO hfsm.signal
  (id,instance_id,ordinal,status,payload,lease_id,lease_owner,lease_until)
VALUES
  ('00000000-0000-4000-8000-000000001003',
   '00000000-0000-4000-8000-000000001001',1,'leased','{"type":"go"}'::jsonb,
   '00000000-0000-4000-8000-000000001004',
   '00000000-0000-4000-8000-000000001005',clock_timestamp()+interval '1 hour');

UPDATE hfsm.instance SET
  next_signal_ordinal=2,
  in_flight_signal_id='00000000-0000-4000-8000-000000001003',
  in_flight_lease_id='00000000-0000-4000-8000-000000001004',
  in_flight_lease_until=clock_timestamp()+interval '1 hour'
WHERE id='00000000-0000-4000-8000-000000001001';

-- One successful commitTurn (SQL shape only, without Haskell guards).
INSERT INTO hfsm.step
  (id,instance_id,revision,signal_id,source_snapshot_id,
   kind,from_state_ref,to_state_ref,route_ref,note,created_at)
VALUES
  ('00000000-0000-4000-8000-000000001006',
   '00000000-0000-4000-8000-000000001001',1,
   '00000000-0000-4000-8000-000000001003',
   '00000000-0000-4000-8000-000000001002',
   'signal',1,2,7,'{}'::jsonb,clock_timestamp());

INSERT INTO hfsm.snapshot
  (id,instance_id,revision,produced_by_step_id,state_ref,state_path,
   ctx,wait_value,child_value,machine_version,spec_digest,created_at)
VALUES
  ('00000000-0000-4000-8000-000000001007',
   '00000000-0000-4000-8000-000000001001',1,
   '00000000-0000-4000-8000-000000001006',
   2,'[2]'::jsonb,'{"ok":true}'::jsonb,NULL,
   '{}'::jsonb,'1',repeat('a',64),clock_timestamp());

INSERT INTO hfsm.outbox
  (instance_id,step_id,ordinal,effect_key,payload,created_at)
VALUES
  ('00000000-0000-4000-8000-000000001001',
   '00000000-0000-4000-8000-000000001006',0,
   '00000000-0000-4000-8000-000000001006:0',
   '{"kind":"cmd","payload":{"notice":"ready"}}'::jsonb,
   clock_timestamp());

UPDATE hfsm.instance SET
  current_state_ref=2,current_path='[2]'::jsonb,
  current_snapshot_id='00000000-0000-4000-8000-000000001007',
  revision=1,status='waiting',
  in_flight_signal_id=NULL,in_flight_lease_id=NULL,in_flight_lease_until=NULL
WHERE id='00000000-0000-4000-8000-000000001001';

UPDATE hfsm.signal SET
  status='applied',lease_id=NULL,lease_owner=NULL,lease_until=NULL
WHERE id='00000000-0000-4000-8000-000000001003';

DO $$
DECLARE n integer;
BEGIN
  SELECT count(*) INTO n FROM hfsm.step
  WHERE instance_id='00000000-0000-4000-8000-000000001001';
  IF n <> 1 THEN RAISE EXCEPTION 'CT02 smoke expected exactly one step'; END IF;
  SELECT revision INTO n FROM hfsm.instance
  WHERE id='00000000-0000-4000-8000-000000001001';
  IF n <> 1 THEN RAISE EXCEPTION 'CT02 smoke expected instance revision 1'; END IF;
END;
$$;

-- Force all deferred FK validation before rolling back.
SET CONSTRAINTS ALL IMMEDIATE;
ROLLBACK;
