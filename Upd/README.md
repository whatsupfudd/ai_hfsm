# HFSM CT-03 — Complete Haskell modules (copy/replace delivery)

This ZIP contains **14 complete `.hs` source modules**, not patches or an in-place installer.
It supersedes the `scripts/install.py`-based archive issued earlier. Its directory
structure is relative to the root of `whatsupfudd/ai_hfsm`.

## Install

1. Back up or commit any local changes to the ai_hfsm repository.
2. Extract this ZIP into a temporary directory.
3. Copy the whole `src/` tree over the repository's existing `src/` tree,
   **merging directories and replacing only files with matching paths**.
   Example (from the extracted folder, on a POSIX shell):

       cp -a src/. /path/to/ai_hfsm/src/

4. Build and test:

       stack build
       stack exec -- thfsm test commitTurn
       stack exec -- thfsm test distributed
       stack exec -- thfsm test approval --db
       stack exec -- thfsm test distributedDb --db

   Use a **separate test database** for distributedDb: its worker claims use
   the global queue and may consume other instances' pending signals.

## Full modules

Existing modules replaced (11):

- src/Hfsm/Runtime/Model/Instance.hs
- src/Hfsm/Runtime/Model/Signal.hs
- src/Hfsm/Runtime/Model/Outbox.hs
- src/Hfsm/Store/Class.hs
- src/Hfsm/Runtime/Engine/Step.hs
- src/Demo/MemoryStore.hs
- src/Hfsm/Store/Postgres/Statements.hs
- src/Hfsm/Store/Postgres/Transaction.hs
- src/Hfsm/Store/Postgres.hs
- src/Demo/CommitTurnRegression.hs
- src/Commands/Test.hs

New modules added (3):

- src/Hfsm/Runtime/Engine/Lease.hs
- src/Demo/DistributedSafetyRegression.hs
- src/Demo/DistributedPostgresRegression.hs

## Implementation notes

- The `Hfsm.Runtime.Engine.Step` replacement is based on the current GitHub
  version, including the previously applied root-level TerminalSk -> DoneIs fix.
- `Hfsm.Store.Postgres` retains the **current Hasql.Pool-based Runner** and
  existing registration/initialization/enqueue APIs.
- `Hfsm.Store.Class` keeps the older, deprecated `StepAppend` and
  `SnapshotAppend` type exports for source compatibility.
- The CT-02 schema already includes revision, ordinal, lease, and effect-key
  columns, so this package does not include SQL DDL migrations.
- No scripts need to be run to transform code. The only code in this package
  is full Haskell source. No Python installer is included.
- There is an optional lease heartbeat for long-running IO work; it does NOT
  change exactly-once external effect semantics.
- The new row fields are **source-breaking** for clients constructing
  InstanceRw, SignalRw, OutboxRw or CommitRq directly.

## Validation limitations

Internal static contract checks against these complete files passed. This
runtime lacks GHC and a PostgreSQL server, so this bundle has NOT been
compiled here or run against a real database. Validate in your environment
before using distributed workers in production.

## SHA256

Run `sha256sum -c SHA256SUMS` from the extracted root to verify file contents.
