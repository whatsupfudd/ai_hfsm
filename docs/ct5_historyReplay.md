# HFSM CT-05 — First-class history, strict replay, explicit migrations

**Base:** `whatsupfudd/ai_hfsm`, `main`, inspected 2026-11-20. **Delivery:** complete modules, no patch installer. **Validation:** static checks only; no GHC or PostgreSQL available in the artifact environment.

## Design

1. The existing `step`, `snapshot` and `projection_event` tables remain append-only. `snapshot` revision zero is the initial recovery checkpoint. Every turn commits a revision n+1. Pagination uses `(instance_id,revision)`, not global sequence order.
2. `Support/003_hfsm_history_migration.sql` adds an append-only `turn_input` table populated automatically by an `AFTER INSERT ON hfsm.step` trigger *within the existing atomic commitTurn transaction*. It captures the machine identity, current instance state, immutable source snapshot, and original leased signal payload/cause/attempt/clock. Because successful ACK mutates signal timestamps, the stored `signal.updatedAt` is taken from the **new immutable step.created_at**, which is what the pure `planStep` used, even if a heartbeat renewed the lease in between.
3. `Hfsm.History.Postgres` pages immutable history including step, produced snapshot, corresponding `turn_input`, outbox effect keys + payloads, child/break records' immutable fields, projection events and migration events. No IO during replay.
4. `Hfsm.History.Replay` invokes the original release's pure `planStep` and compares step ID, kind, handoff, route, full note, state/path/context/wait/child, deterministic outbox idempotency keys and payloads, children, breaks, projections. It fails closed if the original executable release or input envelope is missing. `verifyTimeline` checks source-snapshot continuity between adjacent records.
5. `Hfsm.History.Release` enforces one immutable `(machine,version,digest,graph_digest,implementation_id)` tuple per release. The `implementation_id` must identify the deployment artifact **including Haskell reaction functions and codecs**; the existing graph/spec digest alone cannot attest callback code. Use `registerPinnedRelease` BEFORE initializing any instances for a new release; do not call legacy `Pg.registerMachine` first for that release.
6. `Hfsm.Migration.Plan` takes **two separately compiled releases** and a typed `MigrationSpec oldCtx newCtx`: `oldCtx -> Either Text newCtx`, explicit state/control mapping, wait and child transformations, and lifecycle mapping. It validates source pinned identity, destination graph membership and exact snapshot pointer. No implicit conversion is attempted.
7. `Hfsm.Migration.Postgres` atomically inserts an explicit `MigrateSk` step, target-version snapshot, immutable migration audit and two projection events, and advances the pinned release under revision/snapshot CAS. It refuses pending signals, undelivered outbox effects, active children and active signal leases. A stable operation UUID makes identical request re-submission idempotent. This migration does **not** run the target machine as part of the migration transaction.
8. `Hfsm.Migration.PinnedEnqueue` tags enqueues with the producer's expected `(name,version,digest)` inside the same transaction as the row lock and insertion. The migration installs a DB trigger that rejects old unpinned `Pg.enqueue` after an instance has migrated. This prevents old-version producers silently enqueueing payloads for the new codec. `Hfsm.Migration.Replay` can independently rerun the *named migration function* against historical source/target snapshots, checking context, state/path, wait/child. The ordinary history replay verifies the persisted migration boundary without requiring the old migration implementation.

## Source tree

```text
src/Commands/Test.hs                       REPLACE (complete)
src/Hfsm/Runtime/Registry.hs              REPLACE (complete)
src/Demo/HistoryRegression.hs              NEW
src/Hfsm/History/Types.hs                  NEW
src/Hfsm/History/Statements.hs             NEW
src/Hfsm/History/Postgres.hs               NEW
src/Hfsm/History/Replay.hs                 NEW
src/Hfsm/History/Release.hs                NEW
src/Hfsm/Migration/Types.hs                NEW
src/Hfsm/Migration/Plan.hs                 NEW
src/Hfsm/Migration/Statements.hs           NEW
src/Hfsm/Migration/Postgres.hs             NEW
src/Hfsm/Migration/Replay.hs               NEW
src/Hfsm/Migration/PinnedEnqueue.hs          NEW
src/Hfsm/Activity/Provider/Enqueue.hs        REPLACE (complete)
Support/003_hfsm_history_migration.sql     NEW
```

**The CT-03 `commitTurn` and CT-04 dispatcher/storage modules are unchanged. The CT-04 internal enqueue provider is replaced with a backward-compatible version that also exposes `newReleaseAwareEnqueueProvider`.** This lowers risk to previously passing tests. The new SQL capture trigger must be installed **before** new replayable turns are committed.

## Database migration preflight

Run these queries *before* applying SQL 003:

```sql
-- Must return no rows, otherwise decide which release version to rename.
SELECT machine_name,machine_version,count(*),array_agg(spec_digest)
FROM hfsm.machine_release
GROUP BY machine_name,machine_version HAVING count(*)>1;

-- Check baseline and CT04 are installed.
SELECT to_regclass('hfsm.step'),to_regclass('hfsm.activity_attempt');
```

On an isolated/staging database, apply in a transaction:

```sh
psql -v ON_ERROR_STOP=1 -1 -f Support/003_hfsm_history_migration.sql
```

Do not reapply this forward-only migration. Never modify historical snapshots or steps to make old history appear replayable. For a legacy step with no `turn_input`, strict replay reports `ReplayMissingInput`; history browsing and audit still work.

## Copy-ready modules

From the extracted archive root:

```sh
cp -a src/. /path/to/ai_hfsm/src/
cp Support/003_hfsm_history_migration.sql /path/to/ai_hfsm/Support/
```

No source matching or in-place code transformations are needed.

## Test commands

```sh
stack build
stack exec -- thfsm test history
stack exec -- thfsm test historyDb --db
stack exec -- thfsm test approval --db
stack exec -- thfsm test commitTurn
stack exec -- thfsm test distributed
stack exec -- thfsm test distributedDb --db
stack exec -- thfsm test activity
stack exec -- thfsm test activityDb --db
```

Run live DB regressions on a **dedicated test database**. History tests use instance-scoped workers. The deliberately pending signal in the migration rejection test is acknowledged through the public fenced Store API before exiting.

## Known limits / conservative safety choices

- **No retroactive exact replay for pre-CT05 turns.** Old signals' original mutable `updated_at`/lease state has been lost; backfilling a guessed value would misrepresent provenance. Historical steps are still browsable.
- **The compiler digest covers the serializable machine graph, not arbitrary Haskell closures.** For strict production provenance, pin `implementation_id` to a content-addressed build/release containing handler implementation and codecs, retain the original executable, and run `verifyPinnedRelease` before replay. Legacy releases registered by `Pg.registerMachine` have NULL implementation_id and are flagged by the strict registration API; create a NEW version rather than overwriting one.
- **Enqueue release guard.** After first migration, use `enqueuePinned runner (machineKeyOf compiled) rq`, including internal activity providers that enqueue signals: use `newReleaseAwareEnqueueProvider` with a mapper that returns the intended MachineKey. Legacy `Pg.enqueue` then returns an error for migrated instances by design. The session-local GUC is an application coordination mechanism, not an authorization boundary.
- **Migration is deliberately quiescent.** Existing queued signals, incomplete/failed outbox activities and active child runs must be reconciled before migration. This prevents changing codecs/graph underneath in-flight work. It does not yet implement a multi-instance migration choreography.
- **Migration target must have the same machine name.** Rename/cross-machine migration is a separate operation requiring a deliberate policy.
- **No raw history UPDATE/DELETE.** Database triggers reject mutation. Roles should additionally be granted only minimal INSERT/SELECT privileges for immutable tables.
- **Only migration of active instances** (running/waiting/paused) is implemented. It is not an undo or resurrection mechanism for completed instances.
- **Read-only replay must not deliver external effects.** It reads stored outbox intent payloads, not provider receipts/status. Provider execution is controlled by CT-04.
- **No compile or live database execution was possible here.** Run GHC/Hasql-TH and SQL integration tests before deployment; these complete modules are implementation candidates.
