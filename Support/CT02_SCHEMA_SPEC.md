# CT-02 — PostgreSQL durable HFSM runtime schema

Status: **proposed DDL baseline, for implementation/review**. Date: 2026-11-17.
Source of truth: `migrations/001_hfsm_runtime.sql`. This is **not** a completed
PostgreSQL Store implementation and has not been executed against a live
PostgreSQL server in the artifact-generation environment.

## 1. Source alignment

The CT-01 `Hfsm.Store.Class` defines:

```haskell
commitTurn :: CommitRq -> m (Either StoreErr CommitRez)
```

Its `CommitRq` includes `instanceId`, `expectedSnapshot`, `signalId`,
`leaseId`, `step`, `snapshot`, `nextStatus`, `outbox`, `child`, `breaks`, and
`proj`. The same interface exposes `leaseSignal`, `loadInstance`,
`loadSnapshot`, `ackSignal` for planning failures only, and `listSignals`.

The PostgreSQL schema records all of these. Additional fields are database
coordination fields; their exposure in Haskell should be a separate small
migration rather than retroactively changing the CT-01 semantic contract.

## 2. Entities and authority

| Table | Authority/role | CT-01 mapping |
|---|---|---|
| `machine_release` | immutable machine identity and optionally normalized graph; not executable closures | `MachineKey`, `CompiledMachine.graph` |
| `instance` | **one authoritative current pointer**, revision, machine identity, admission slot | `InstanceRw`, plus DB revision and lease metadata |
| `snapshot` | immutable state/context/wait/child materialization per revision | `SnapshotRw` |
| `signal` | durable inbound queue, FIFO ordinal, fenced lease, final disposition | `SignalRw` with additional DB-managed metadata |
| `step` | immutable successful turn record | `StepRw` plus DB revision/source snapshot |
| `outbox` | committed effect intents, independent delivery lifecycle | `OutboxAppend`, `OutboxRw` |
| `child_run` | reserved child UUID and parent relation | `ChildAppend`, `ChildRw` |
| `break_event` | breakpoint occurrences attached to a committed turn | `BreakAppend`, `BreakRw` |
| `projection_event` | append-only source for active/trace/queue/tree projection updates | `ProjBatch` entries |
| `projection_progress` | per-instance, per-consumer rebuild/checkpoint state | new asynchronous projection adapter |

Haskell's `StatePath` is JSON-encoded as an array; `ref32` corresponds to
unsigned 32-bit references. Digests use the existing 64-character lowercase
hexadecimal representation from `Hfsm.Core.Digest`.

## 3. Primary invariants

- **I1 / Atomic turn:** step, snapshot, outbox, child rows, breakpoint
  occurrences, projection events, instance update and signal success ACK all
  commit in the *same database transaction*.
- **I2 / No stale overwrite:** `instance.current_snapshot_id` MUST equal
  `CommitRq.expectedSnapshot` at the moment the locked turn is committed.
  Database `revision` is incremented exactly once per successful turn.
- **I3 / Lease ownership:** both `instance.in_flight_lease_id` and
  `signal.lease_id` MUST equal the requested token; the lease must not have
  expired according to **PostgreSQL server time**. `signal.status='leased'`.
- **I4 / Single admitted signal:** at most one `signal.status='leased'` per
  instance, enforced by `signal_one_leased_per_instance_uq`. The instance slot
  additionally stores the current signal and token for admission fencing.
- **I5 / Ordered ingress:** each signal has `(instance_id,ordinal)` unique.
  The enqueue transaction serializes ordinal assignment by locking its
  `instance` row; CT-02's scheduler admits the *oldest pending* ordinal.
  An expired lease must be reclaimed on the **same signal** rather than
  processing a later signal ahead of it.
- **I6 / At-most-one committed turn per input:** `step_signal_once_uq`.
  Synthetic future steps with a null signal are handled by revision uniqueness.
- **I7 / Snapshot lineage:** snapshot revision 0 is initial. New snapshot
  `(instance_id,revision,produced_by_step_id)` must reference a step for the
  *same revision and instance*. Each step points to its source snapshot.
- **I8 / No partial child or effect registration:** `child_run`, `outbox`
  and `break_event` records have a FK to the creating step; the parent
  `commitTurn` must create them together.
- **I9 / Append-only history:** UPDATE/DELETE triggers reject mutation of
  `step`, `snapshot`, and `projection_event`. Runtime roles should also lack
  update/delete privileges on history tables.
- **I10 / Replay-ready identity:** machine name/version/spec digest is
  immutable per instance; exact Haskell closures are registered by deployed
  code, not serialized in SQL. Replays must use the matching release.
- **I11 / No external exactly-once claim:** outbox delivery is separate,
  potentially at-least-once. Provider idempotency uses stable `effect_key`.

Cross-table checks involving the live lease, current state, outgoing source
snapshot and step content belong in the **transactional Store adapter**;
CHECK and FK constraints alone cannot prove arbitrary application invariants.

## 4. `commitTurn` transaction blueprint

The following is sequencing/pseudocode for the Hasql adapter, NOT a stored
procedure and NOT a directly executable transaction (field mapping omitted):

```sql
BEGIN ISOLATION LEVEL READ COMMITTED;
-- 1. SELECT instance ... WHERE id = :inst FOR UPDATE;
-- 2. Check current_snapshot_id == :expectedSnapshot; get revision R.
--    Check in_flight_signal_id == :signalId and lease ID == :leaseId.
-- 3. SELECT signal ... WHERE id = :signalId FOR UPDATE;
--    Check instance, status='leased', lease_id and lease_until > clock_timestamp().
-- 4. Load/validate source snapshot, machine identity, planned state lineage.
-- 5. INSERT step with revision R+1 and source_snapshot_id = :expectedSnapshot.
-- 6. INSERT snapshot with revision R+1, produced_by_step_id = :stepId.
-- 7. INSERT outbox, child_run, break_event and projection_event (all referring
--    to the new step). Assign ordinal to outbox/children/breaks by list index.
-- 8. UPDATE instance SET current_snapshot_id=:newSnapshot, revision=R+1,
--    status=:nextStatus, current_state_ref=..., current_path=...,
--    in_flight_signal_id=NULL, in_flight_lease_id=NULL,
--    in_flight_lease_until=NULL WHERE id=:inst.
-- 9. UPDATE signal SET status='applied', lease_id=NULL, lease_owner=NULL,
--    lease_until=NULL, updated_at=clock_timestamp() WHERE id=:signalId.
-- 10. Ensure exactly one instance row, exactly one signal row updated.
COMMIT;
```

**Lock order:** every claim/commit/ACK/reclaim path must lock `instance` first,
then `signal`, to avoid deadlocks. `READ COMMITTED` is sufficient here because
updates are serialized by the explicit instance row lock, and the CAS/lease
checks occur after acquiring it. Database uniqueness/FKs are further guards.
The transaction must roll back on **any** validation/INSERT/UPDATE error.
No external API or LLM activity may occur while this transaction is open.

**Error mapping:** no instance/snapshot -> `NotFoundEr`; stale expected
snapshot or duplicate step -> `ConflictEr`; wrong token/status/expired lease ->
`LeaseLostEr`; bad serializations -> `DecodeEr`; transient SQL failure ->
`BackendEr` (with retries at an upper level, never blindly retry external
side effects). SQLSTATE 23505 is not always retryable; classify by constraint.

## 5. Enqueue and leasing semantics

### Enqueue

1. `BEGIN`; lock `instance(id)` `FOR UPDATE`.
2. Allocate the current `next_signal_ordinal`, then increment it.
3. Insert a new `signal(status='entered', ordinal, payload, cause, ready_at)`.
4. If `dedupe_key` already exists, return the original signal instead of
   consuming another sequence number (check before increment under lock).
5. `COMMIT`.

This creates an order by enqueue-transaction serialization rather than a
potentially misleading global identity/creation timestamp.

### Lease

1. Select candidate **instances**, lock selected `instance` rows
   `FOR UPDATE SKIP LOCKED` in a short claim transaction.
2. For each claimed instance, find its lowest nonterminal input ordinal.
   If its oldest signal is leased and unexpired, do not admit another.
3. On a ready input or expired lease: set `signal.status='leased'`, assign a
   **fresh UUID token**, `lease_owner = LeaseRq.node`, `lease_until` based on
   server time, `attempts += 1`; mirror the same token/signal/expiry into the
   instance admission slot.
4. Return `SignalRw.lease = Just freshLeaseToken` (not the worker node ID).
5. Commit the claim immediately; calculate the pure step outside the DB
   transaction.

A scheduler implementation must avoid advancing past an older leased signal.
`signal_one_leased_per_instance_uq` provides a database backstop for this.

### Failure ACK / release

`ackSignal` is used only if no turn was committed. Lock instance then signal;
verify current slot and lease token, and **clear all lease metadata and the
instance slot atomically**. `rejected` and `failed` are terminal statuses.
Requeue sets `entered` with an appropriate retry/backoff `ready_at`.
Do not use `ackSignal` to mark a successfully committed input `applied`.

## 6. Materialized projections and a subtle PostgreSQL ordering issue

`ProjBatch` is included in CT-01's atomic commit. CT-02 records its entries in
`projection_event` in the commit transaction, making the projection source
durable. It does **not** need to update every read-model table in that same
transaction; that can produce costly coupling to the critical path.

Important: PostgreSQL identity/sequence values are allocated **before** a
transaction commits. Transaction B can commit `uid=102` before transaction A
commits `uid=101`. A consumer that persists `last_uid=102` and later selects
only `uid > 102` can silently miss A. Therefore, **never use a single global
identity high-water mark as a commit-order cursor**.

Instead, derive read-model progression from `instance.revision` and the
`projection_progress(consumer, instance_id, last_revision)` record. Apply the
complete revision update and advance the checkpoint in ONE transaction; retry
idempotently. For a global *low-latency* notification feed, add a separately
leased notification outbox/queue, not a global sequence checkpoint.

## 7. Additional implementation considerations

**Initialization.** `instance.current_snapshot_id` is mandatory, and initial
snapshot creation is cyclic with the instance FK. `instance_current_snapshot_fk`
is intentionally `DEFERRABLE INITIALLY DEFERRED`: initialize instance at
revision 0 and its matching `snapshot` in the SAME transaction. Run the
machine-release registration before initializing instances.

**Mapping.** Haskell `InstanceStatus`/`SignalStatus`/`OutboxStatus`/`ChildStatus`
need explicit codec-to-SQL text mappings. `BreakKind` and `BreakStatus` are
stored with names matching the current Haskell constructors. `StatePath` is
JSON array; context/wait/child and command payloads are `JSONB`.

**Type migration.** `instance.revision`, `signal.ordinal`, and lease expiry are
new DB fields. The minimal CT-01 Haskell types may be retained temporarily:
the adapter can populate additional columns and map `signal.lease` to the
fresh token. The *next* incremental Haskell migration should expose a real
revision and lease metadata in the model so application diagnostics are
accurate.

**Versioning.** `machine_release` records only identity and the normalized
serializable graph (optional until graph export is integrated). The full
Haskell compiled machine and closures MUST be deployed with the worker and
resolved by `(machine_name,machine_version,spec_digest)`.

**JSONB.** `JSONB` is suitable for structured context and audit semantics,
but it is not byte-preserving: object keys and numeric representations are
normalized. Large binary/source assets and cryptographically significant
original bytes belong in an artifact store with exact byte digests.

**Child lifecycle.** A child UUID is reserved when its parent commits but
may not yet be instantiated. Consequently `child_run.child_instance_id` has no
FK to `instance(id)` in CT-02. The spawn dispatcher must create/locate the
actual child idempotently and then advance status.

**Timer lifecycle.** Current HFSM emits timer intents in the outbox; no separate
`timer` table is required to implement the CT-01 `CommitRq`. A later due-time
scheduler may add a normalized `timer` table without changing `commitTurn`.

**Tenant isolation.** The first CT-02 schema is single trust-domain. Before
multi-tenant 4U2B1N deployment, add `tenant_id` consistently to every
identity/queue/read-model and enforce tenant-filtered access or RLS. Do not
claim this schema alone implements tenant security.

**Permissions.** Own schema and tables with a migrator role. Grant the
application role narrow SELECT/INSERT/UPDATE privileges, no DELETE of
history, and remove schema CREATE from application users. DDL itself is not
a substitute for deployment role/grant design.

**Scaling.** Start without partitions. Measure step/snapshot size and hot
queue size before introducing time/range partitioning. Partitioned unique
constraints and FK strategies need deliberate rework; blindly partitioning
by time conflicts with globally unique UUID and per-instance revisions.

**Schema migration management.** The DDL is intentionally one install-once
baseline migration. Use the project's migration runner to version/apply it.
Do not rerun this script against an already initialized schema.

## 8. Required CT-02 validation tests

1. Create machine release, instance and initial snapshot in one transaction.
2. Enqueue two signals; verify contiguous per-instance ordinal and idempotent
   producer dedupe.
3. Two claimers attempt same instance concurrently; no dual lease.
4. An unexpired lease prevents processing a later signal.
5. Reclaim expired lease with new token; stale token cannot commit or ACK.
6. Commit one successful turn; all side records appear atomically and signal
   is applied; instance revision increments from 0 to 1.
7. Inject failure after step insertion: rollback leaves **all** tables unchanged.
8. Attempt duplicate `(instance_id, signal_id)` commit: no second step/outbox.
9. Attempt stale `expectedSnapshot`: `ConflictEr`; no partial writes.
10. Attempt wrong `instance_id` for signal/snapshot: FK/check/adapter rejection.
11. External effect delivery may retry but stable effect key is unchanged.
12. Validate append-only triggers reject `UPDATE` / `DELETE` for step/snapshot.
13. Simulate projection-events committed out of global UID order; rebuild
    from per-instance revisions without skipping events.
14. Verify `step`, `snapshot`, `instance` codec round-trip from current Haskell.
15. Verify a transaction rollback cannot leave a stuck in-flight slot.

## 9. Acceptance criteria and next work

CT-02 is **not completed** when the schema is installed. It is completed only
when a real `Hfsm.Store.Postgres` adapter compiles, passes the above tests,
restarts without losing turns, correctly fences stale workers, and runs
with two or more cooperating workers. End-to-end external delivery guarantees
remain a later activity-dispatch milestone.

The planned future dataflow subsystem should reuse the principle of bounded,
fenced, transactional task commits but needs its own flow/run/operator/artifact
schema; do not prematurely mix flow tokens into `hfsm.signal`.
