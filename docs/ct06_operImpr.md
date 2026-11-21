# ai_hfsm CT-06 — Operational runtime (2026-11-21)

Baseline: https://github.com/whatsupfudd/ai_hfsm, `main` tree SHA
`2720cd1d4396f98a0d994c48e2271b322a5cd480` at review time.

**Delivery format:** Complete copy-ready Haskell modules, one forward SQL
migration, test harness. There are NO source replacement scripts. The only
existing Haskell module replaced is `src/Commands/Test.hs` (starting from the
exact CT-05 GitHub revision). All `src/Hfsm/Operations/*` and `src/Demo/Operations*`
modules are new. No CT-03 `commitTurn`, CT-04 activity settlement, CT-05 replay,
or compiled-machine API changes are required.

## Install

1. Back up the target PostgreSQL DB and use staging first.
2. Apply `Support/004_hfsm_operations.sql` after 001, 002 and 003.
   It is a **one-time migration**. Do not reapply on the same database.
3. Copy the archive's `src/.` over the repository's `src/.`; only
   `Commands/Test.hs` will overwrite an existing module.
4. Build *after* schema migration: Hasql-TH validates SQL against a live schema
   when Template Haskell runs.
5. Run `stack exec -- thfsm test operations`, `... operationsDb --db`,
   `... chaosDb --db`, then CT-03/04/05 regressions in a dedicated test DB.
6. To run hard process-kill tests, `bash scripts/ct06_kill_matrix.sh` **only
   against a dedicated test database**. This deliberately SIGKILLs children.

`stack build` and live PostgreSQL execution were **not possible** in the
artifact-building environment. Static checks and archive CRCs are not a
substitute for compilation and actual test results.

## Operational APIs

### Tenant creation and admission

Use `Hfsm.Operations.Postgres.createTenant` to create a tenant with the caps
(max active instances, backlog, in-flight signals, in-flight activities, signal
attempts), then `createTenantInstance` instead of the legacy `Pg.initializeInstance`.
Call `setGroupLimits` to create/tune concurrency groups. Use
`enqueueWithPolicy` with `EnqueueOptions` for priority (-20..20), immutable
correlation/causation IDs, traceparent, the original requested `readyAt`
(including delayed scheduling), and optional CT-05 release pin.

`productionStore` and `productionActivityStore` wrap the **existing** CT-03/04
store implementations. They substitute only their lease-claim operations.
All commit/ack/renew methods retain the previous proven CAS/fence behavior.

Every worker MUST register a *unique per-process* UUID and capabilities with
`registerWorker`, then regularly call `heartbeatWorker`, or use
`Hfsm.Operations.Runtime.withRegisteredWorker`. For long-running workers use
`runSignalWorker` / `runActivityWorker`, or `*Traced` variants.

Capability strings: `hfsm:signal`, `activity:cmd`, `activity:timer`,
`activity:spawn`, `activity:<provider kind>`, plus `hfsm:*` and `activity:*`
wildcards. A worker with a stale heartbeat, incompatible capabilities, or
`draining` state cannot receive **new** work. Existing leases still require
ordinary CT-03/04 fencing, and their outcomes may remain ambiguous.

**Correctness tradeoff:** CT06 schedules through a *single PostgreSQL admission
gate row*. It safely serializes tenant/group capacity checks before claims.
This is intentionally conservative and can bottleneck busy deployments.
Tenant/group sharded locks and global fair scheduling should be a measured
follow-on after adversarial concurrency benchmarks. The queue is FIFO within
an instance. Across instances, priority includes a bounded aging bonus.

### Cancellation

`cancelInstance` checks expected revision under the instance lock and inserts
an append-only `hfsm.operator_action`. It rejects pending input, revokes
in-flight signal leases, prevents new activity claims and closes ready effects.
Already-leased external effects may have executed and **are not revoked**.
After expiry they are reconciled as outcome-unknown in the next activity
sweep. Cancellation is an audited **operational terminal overlay**, not a
synthetic transition step; the `instance.revision` is not incremented.
The projector merges this terminal overlay only when its projection has
caught up with the authoritative revision. Operator IDs are audit data,
NOT credentials. The API or DB role MUST enforce authorization.

### Dead letters and redrive

`hfsm.dead_letter` stores an immutable record for failed signals/effects.
`deadLetters` retrieves a tenant's records. `redriveSignal` permits one
explicit, idempotent operator-approved re-enqueue of a **failed signal**;
it allocates a new instance-local ordinal, links causation, and writes an
immutable redrive/operator audit. It does not alter historical rows. A failed
outbox effect must instead be reconciled with its provider—automatically
redriving effects with an unknown outcome could duplicate real-world action.

### Correlation and tracing

`ops_enqueue_signal` records `correlation_id`, `causation_id`, `traceparent`.
A before-insert outbox trigger propagates them from the originating signal.
Use `Hfsm.Operations.Provider.Enqueue.correlatedEnqueueProvider` to preserve
those fields across an internal outbox→signal handoff. Use
`Hfsm.Operations.Provider.HttpTraced.tracedHttpProvider` to send the stable
`Idempotency-Key` and persisted `traceparent` to HTTP providers.

`Hfsm.Operations.Trace` supports W3C traceparent parsing and OTLP/HTTP JSON
span emission. The `*Traced` worker constructors use it for claim, commit,
ack and activity completion spans. Collector failures do not change machine
semantics. OTLP export is an **optional operational configuration**, not
started implicitly. Database spans can be added via Hasql instrumentation
in production monitoring.

### Asynchronous projections

CT-05's `hfsm.projection_event` remains the append-only projection source.
CT06 `hfsm.readmodel_event` and `hfsm.readmodel_active` are derived tables.
`ops_project_instance` reads source steps/snapshots in *instance revision*
order, updates derived tables, and advances the existing
`hfsm.projection_progress` checkpoint **in the same transaction**. Concurrent
projector workers serialize on that checkpoint; no global sequence ID is
used as a commit-order cursor. `Hfsm.Operations.Projection` runs page-based
async projection and supports reset/rebuild from the source. Reset is an
**administrative** operation; pause competing projectors for that consumer.

These initial materializations cover the revisioned active state and the
existing projection-event stream. Full queue/tree materializations remain a
separate incremental view implementation, since their readiness/lease and
child statuses change outside successful HFSM turns.

## Security/deployment constraints

The migration revokes PUBLIC EXECUTE on CT06 mutation functions. Explicitly
GRANT the necessary function and table privileges to your runtime, operator
and projection **database roles**; if they are not the migration owner, they
will fail without those grants. Example (adapt to your role topology):

```sql
GRANT EXECUTE ON FUNCTION
  hfsm.ops_claim_signals(uuid,integer,integer,uuid)
  TO hfsm_runtime;
GRANT EXECUTE ON FUNCTION
  hfsm.ops_claim_signal_one(uuid,integer,uuid)
  TO hfsm_runtime;
```

The example assumes `hfsm_runtime` exists and has the required underlying
relation rights. CT06 supplies tenant identity/quotas **inside a trusted
service**; it does NOT implement application authentication, tenant-specific
row-level security for read APIs, operator RBAC, or customer-facing key
management. Do not expose these DB functions directly to untrusted tenants.
The legacy `Pg.postgresStore` and `Pg.enqueue` deliberately remain usable for
CT-03/04/05 tests but **bypass CT06 scheduling / worker capability policy**.
Enforce use of CT06 constructors in all production worker processes.

Also ensure `pgcrypto`/built-in `gen_random_uuid()` is available in your DB,
retain stable machine release binaries for CT-05 replay, and deploy at least
one activity worker for each provider kind.

## Test commands

```sh
stack build
stack exec -- thfsm test operations
stack exec -- thfsm test operationsDb --db
stack exec -- thfsm test chaosDb --db
# Separate-process test ONLY, never run in a valuable DB:
bash scripts/ct06_kill_matrix.sh
# Existing tests:
stack exec -- thfsm test approval --db
stack exec -- thfsm test commitTurn
stack exec -- thfsm test distributed
stack exec -- thfsm test distributedDb --db
stack exec -- thfsm test activity
stack exec -- thfsm test activityDb --db
stack exec -- thfsm test history
stack exec -- thfsm test historyDb --db
```

See `docs/CT06_ACCEPTANCE_MATRIX.md` for verified design properties, automated
cases, external chaos requirements, and known gaps.
