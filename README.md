# Hfsm: hierarchical finite state machine support for FUDD

`Hfsm` is the FUDD package for specifying, validating, compiling, executing, inspecting, and rendering hierarchical finite state machines.

It is designed for the kind of long-running orchestration work that appears throughout FUDD: batch-processing pipelines, document generation flows, human review chains, legal workflow tracking, animation scenarios, game-like persona-kernel extraction scenarios, Wapp runtime operations, and any computation that benefits from explicit states, typed handoffs, durable progress, and replayable execution.

The package is not merely a state-machine helper library. It is intended to become the common workflow substrate for the FUDD ecosystem.

At the highest level:

```text
Haskell HFSM specification
  -> compiled normalized graph
  -> validation report
  -> durable runtime execution
  -> observable projections
  -> pretty-printed / exported / inspected results
```

The current implementation direction has been validated with a working `Demo.ApprovalRuntime.main` test using an in-memory store and a formatted execution view.

---

# 1. Why this package exists

Across earlier FUDD work, HFSMs appeared repeatedly as the right structure for:

* long chains of computation;
* resumable orchestration;
* workflow state tracking;
* scenario execution;
* human-in-the-middle decision flows;
* batch-processing lifecycle management;
* document ingestion / transformation / publication;
* distributed execution with strong introspection.

Several implementation styles were explored: embedded Haskell DSLs, YAML specifications, direct Haskell records, multi-threaded runtimes, and distributed runtimes.

The conclusion was clear:

**FUDD needs a typed Haskell-first HFSM package where state bodies express the dynamic logic, transitions are explicit handoff routing, validation is strong, and execution is durable and inspectable.**

That is what `Hfsm` provides.

---

# 2. Central design decision: state-centered HFSM authoring

Textbook automata often put most logic on transitions:

```text
state -- event/guard/action --> state
```

That is not the preferred FUDD developer model.

In FUDD’s `Hfsm`, the **state** is the dynamic unit of logic. A state receives a signal, updates context, emits commands, and returns a handoff. The route table then maps that handoff to structural control movement.

The shape is:

```text
State + Signal + Context
  -> state handler logic
  -> Handoff + Updated Context + Commands
  -> route table
  -> next control state / wait / spawn / join / timer / break
```

This gives a much clearer separation:

```text
State handler:
  domain logic

Handoff:
  typed result of domain logic

Route:
  structural bookkeeping and control movement
```

A simplified example:

```haskell
approveCase =
  CaseSpec
    { name = Just approveCaseName
    , match = \case
        ApproveSg _ -> True
        _ -> False
    , react = \_ signal ctx ->
        case signal of
          ApproveSg user ->
            Right ReactRez
              { next = ctx { approvedBy = Just user }
              , handoff = ApprovedHf
              , cmds = [NotifyCm ctx.author ("approved by " <> user)]
              , note = ["approved"]
              }

          _ ->
            Left (RejectEr "approve case received a non-approve signal")
    , meta = emptyMetaSpec
    }
```

Then the route is just the structural consequence:

```haskell
RouteSpec
  { on = ApprovedHf
  , plan = gotoPlan DonePh
  , meta = emptyMetaSpec
  }
```

This is the most important usability point in the package: developers write workflow logic where they naturally expect to write it—inside the active state.

---

# 3. Haskell is the source of truth

`Hfsm` does not use YAML, JSON, SCXML, or stringly typed labels as the canonical machine definition format.

The canonical machine definition is ordinary Haskell.

That gives teams:

* full Haskell expressiveness;
* normal imports and abstraction;
* compiler-checked ADTs for phases, signals, handoffs, commands, and context;
* ordinary testing;
* no YAML drift;
* no weak string-based state identity;
* no hidden external language to keep synchronized.

The package still supports export surfaces later, such as Graphviz, Mermaid, JSON, or partial SCXML. But those are exports, not the source of truth.

---

# 4. The package architecture

The package is organized into layers.

```text
Hfsm.Core
  foundational names, refs, versions, digests, paths, metadata, codecs, errors

Hfsm.Spec
  authoring model: machine, region, state, cases, routes, handlers

Hfsm.Compile
  lowers Haskell specifications into pure normalized graphs and executable tables

Hfsm.Graph
  pure serializable graph model and graph indexes

Hfsm.Validate
  structural, routing, reachability, wait, join, history, and version checks

Hfsm.Runtime
  durable runtime model, registry, engine, replay, breakpoints, workers

Hfsm.Store
  persistence abstraction consumed by the runtime engine

Hfsm.Proj
  derived read models for admin/runtime visibility

Hfsm.Export
  graph/documentation/export surfaces

Hfsm.Migrate
  graph diffs, migration plans, and instance migration support

Hfsm.Render
  human-readable rendering and pretty-printing

Demo.*
  smoke-test and onboarding examples
```

The dependency discipline is deliberate:

```text
Spec -> Compile -> Graph -> Validate / Export / Runtime
Runtime -> Store -> Runtime.Model
Proj reads Runtime.Model
Check coordinates Compile + Validate + Export + Migrate
```

The package avoids cyclic semantic dependencies by keeping the core abstractions small and by separating authored Haskell functions from the normalized graph.

---

# 5. `Hfsm.Core`: stable foundation

`Hfsm.Core` provides the base vocabulary that every other layer uses.

The currently implemented core covers:

```text
Hfsm.Core.Name
Hfsm.Core.Ref
Hfsm.Core.Version
Hfsm.Core.Digest
Hfsm.Core.Path
Hfsm.Core.Meta
Hfsm.Core.Codec
Hfsm.Core.Error
Hfsm.Core
```

The style is intentionally strict and senior-developer oriented:

* camelCase constructor and helper naming;
* no snake_case labels;
* short record fields;
* explicit export lists;
* no redundant field prefixes;
* validated smart constructors where invariants matter;
* Aeson support where values cross persistence or JSON boundaries.

The `Name` layer defines typed human-facing names such as `MachineName`, `StateName`, `RegionName`, `CaseName`, `HandoffName`, `JoinName`, `TimerName`, and `BreakName`, together with validated constructors such as `mkMachineName` and `mkStateName`. 

The `Ref` layer defines compact internal structural references such as `StateRef`, `RegionRef`, `CaseRef`, `RouteRef`, `JoinRef`, `TimerRef`, and `BreakRef`, backed by checked `Word32` conversions. 

The `Version` layer defines `MachineVersion` and version policies such as `ExactVp`, `CompatibleVp`, and `MigratableVp`, which are needed for long-lived machine instances and future migration support. 

The broader core layer also includes:

```text
Digest:
  SpecDigest and GraphDigest

Path:
  RegionPath, StatePath, InstancePath

Meta:
  MetaSpec and SourceSpan

Codec:
  Codec and CodecSet for context, signal, and command encoding

Error:
  HfsmErr and common error rendering
```

The package rule is simple:

```text
Human identity uses names.
Structural identity uses refs.
Runtime compatibility uses versions and digests.
Persistent payloads use codecs.
Diagnostics use metadata and source spans.
```

---

# 6. `Hfsm.Spec`: authoring machines

The authoring layer describes machines in terms of:

```text
MachineSpec
  root region
  version
  codecs
  metadata

RegionSpec
  initial state
  states
  metadata

StateSpec
  key
  human name
  kind
  entry handler
  signal cases
  handoff routes
  optional child region
  metadata
```

The main state kinds are:

```haskell
data StateKind =
    AtomicSk
  | CompositeSk
  | TerminalSk
```

A machine is parameterized over the application’s own types:

```haskell
MachineSpec st sg hf ctx cmd child
```

where:

```text
st:
  authored state key type

sg:
  signal type

hf:
  handoff type

ctx:
  machine context type

cmd:
  emitted command type

child:
  child-machine key type
```

That means a FUDD team can define a machine using normal ADTs:

```haskell
data Phase =
    DraftPh
  | ReviewPh
  | ReworkPh
  | DonePh

data Signal =
    SubmitSg Text
  | ApproveSg Text
  | RejectSg Text Text
  | ResubmitSg Text

data Handoff =
    SubmittedHf
  | ApprovedHf
  | RejectedHf
  | ResubmittedHf
```

No string labels are needed for machine correctness.

---

# 7. `Hfsm.Spec.Case`: state logic

Cases define how a state reacts to incoming signals.

The conceptual shape is:

```haskell
data CaseSpec sg ctx hf cmd = CaseSpec
  { name :: Maybe CaseName
  , match :: sg -> Bool
  , react :: CaseFn sg ctx hf cmd
  , meta :: MetaSpec
  }
```

The reaction returns:

```haskell
data ReactRez ctx hf cmd = ReactRez
  { next :: ctx
  , handoff :: hf
  , cmds :: [cmd]
  , note :: [Text]
  }
```

This is a clean model for workflow computation:

```text
input signal + current context
  -> updated context
  -> handoff
  -> commands to execute later
  -> trace notes
```

State handlers do not perform external effects directly. They emit commands.

That is critical for:

* replay;
* deterministic tests;
* durable execution;
* debugging;
* outbox-based side-effect execution.

---

# 8. `Hfsm.Spec.Route`: structural movement

Routes map handoffs to control plans.

The conceptual shape is:

```haskell
data RouteSpec st hf child = RouteSpec
  { on :: hf
  , plan :: ControlPlan st child
  , meta :: MetaSpec
  }
```

A `ControlPlan` can express:

```text
target:
  stay, goto another state, complete, fail

wait:
  no wait, wait for signal, wait for join, wait for timer

spawn:
  child machines

join:
  all, any, count

timers:
  scheduled timer work

break:
  runtime breakpoint surfaces
```

For simple machines, routes are minimal:

```haskell
route ApprovedHf (gotoPlan DonePh)
```

For advanced workflows, they become the place where orchestration structure is expressed without mixing it with domain logic.

---

# 9. `Hfsm.Compile`: from authored Haskell to normalized graph

The compile layer is the bridge between rich Haskell authoring and deterministic runtime/external tooling.

It produces:

```haskell
data CompiledMachine st sg hf ctx cmd child = CompiledMachine
  { spec :: MachineSpec st sg hf ctx cmd child
  , graph :: MachineGraph
  , exec :: ExecTable sg ctx hf cmd
  , digest :: SpecDigest
  }
```

This separation matters:

```text
spec:
  original authored Haskell structure

graph:
  pure serializable normalized machine graph

exec:
  in-memory handler table containing Haskell closures

digest:
  stable compiled identity
```

The graph can be validated, exported, rendered, diffed, and stored.

The executable table is used only by the runtime engine.

This avoids the common mistake of trying to serialize Haskell functions or burying runtime behavior in an opaque source object.

---

# 10. `Hfsm.Graph`: normalized machine model

`Hfsm.Graph` is the pure graph representation.

Its purpose is to describe the HFSM structurally:

```text
MachineGraph
  regions
  states
  cases
  routes
  joins
  timers
  metadata
  digest
```

Graph nodes and edges include:

```text
RegionNd
StateNd
CaseNd
RouteEd
JoinNd
TimerNd
```

A route edge carries a normalized route plan:

```text
RoutePlan
  target
  wait
  spawn
  join
  timers
  breakpoints
```

The graph contains no Haskell closures.

This is the basis for:

* validation;
* Graphviz export;
* Mermaid export;
* admin displays;
* migration diffs;
* runtime state path resolution;
* future Wapp visualizations.

The graph layer is also where local Haskell authoring keys become stable internal refs.

---

# 11. `Hfsm.Validate`: correctness before execution

The validator is a first-class part of the package.

A machine should not be treated as operational merely because it compiles as Haskell. It must also validate as an HFSM.

The validation layer checks:

```text
Structure:
  root region legality
  valid initial states
  legal composite/terminal state shape
  parent/child consistency

Routes:
  valid targets
  route/handoff coherence
  missing or duplicate routes
  illegal terminal transitions

Reachability:
  unreachable states
  dead branches
  no-completion surfaces
  structural livelock candidates

Waits:
  wait references exist
  wait mode is coherent
  timers and joins are valid

Joins:
  child fan-out/fan-in consistency
  orphan joins
  impossible join surfaces

History:
  reserved for future re-entry/history validation

Version:
  compatibility between graph versions
  migration risks
```

The package-level validation result is:

```haskell
data ValidateReport = ValidateReport
  { errs :: [ValidateErr]
  , stats :: ValidateStats
  }
```

and:

```haskell
ok :: ValidateReport -> Bool
```

A typical runtime startup flow is:

```haskell
compiled <- compile spec
let report = validate compiled.graph
unless (ok report) $
  fail ("validation failed: " <> show report)
```

The validator gives FUDD teams confidence that workflows are structurally sane before they are launched into long-running execution.

---

# 12. `Hfsm.Runtime`: durable execution

The runtime exists because FUDD workflows are not just short function calls.

They can run across:

* minutes;
* hours;
* days;
* human reviews;
* external service calls;
* batch-processing windows;
* document-generation stages;
* child workflows;
* retries;
* migrations.

Therefore, execution needs durable bookkeeping.

The runtime model includes:

```text
InstanceRw:
  current machine instance

SignalRw:
  durable input queue

StepRw:
  append-only microstep log

SnapshotRw:
  persisted context and current control state

OutboxRw:
  emitted commands awaiting external execution

ChildRw:
  parent-child workflow relationships

BreakRw:
  breakpoint/pause surfaces
```

The engine execution shape is:

```text
lease signal
  -> load instance
  -> load snapshot
  -> decode signal and context
  -> find active state handler
  -> run pure handler
  -> resolve handoff route
  -> create step plan
  -> append step
  -> append snapshot
  -> append outbox commands
  -> append child/break records
  -> update projections
  -> acknowledge signal
```

This is what gives the package its operational strength:

* every input is visible;
* every state movement is logged;
* every context update is snapshotted;
* every emitted command is durable;
* every workflow can be inspected and replayed.

---

# 13. `Hfsm.Store.Class`: runtime persistence boundary

The runtime engine does not directly depend on Postgres, Hasql, files, or any specific persistence backend.

It depends on a `Store m` record:

```haskell
data Store m = Store
  { leaseSignal :: LeaseRq -> m LeaseRez
  , loadInstance :: UUID -> m (Maybe InstanceRw)
  , loadSnapshot :: UUID -> m (Maybe SnapshotRw)
  , appendStep :: StepAppend -> m StepRw
  , appendSnapshot :: SnapshotAppend -> m SnapshotRw
  , appendOutbox :: [OutboxAppend] -> m ()
  , appendChild :: [ChildAppend] -> m ()
  , appendBreak :: [BreakAppend] -> m ()
  , ackSignal :: AckRq -> m ()
  , updateProj :: ProjBatch -> m ()
  , listSignals :: PollRq -> m [SignalRw]
  }
```

This gives us:

```text
Postgres store:
  production runtime

Memory store:
  tests, demos, local development

Future S3/event-log store:
  specialized durable audit scenarios

Simulation store:
  model-checking and scenario testing
```

The current demo uses a fake in-memory `StoreImpl` that supports:

```haskell
newMemoryStore
createInstance
enqueueSignal
loadInstanceView
```

This has already been used successfully to test `Demo.ApprovalRuntime.main`.

---

# 14. `Hfsm.Proj`: observable runtime views

The runtime log is authoritative, but teams need readable operational views.

Projection modules provide derived views such as:

```text
Active:
  current state, path, wait status, machine version

Trace:
  chronological step movement

Queue:
  pending signals, outbox, waiting work

Tree:
  parent-child execution hierarchy
```

The projection layer is deliberately derived. It should be rebuildable from runtime records.

That makes the system robust:

```text
runtime rows are truth
projections are convenience
```

For Wapp-based admin tooling, these projections are the natural data model for dashboards and inspection pages.

---

# 15. `Hfsm.Render`: readable execution output

The package now includes a rendering surface for formatted textual inspection.

Two modules were introduced:

```text
Hfsm.Render.Print
Demo.Print
```

`Hfsm.Render.Print` provides generic value and store-view printing:

```haskell
ppValue
ppValueCompact
ppStoreView
printStoreView
```

`Demo.Print` provides approval-demo-specific rendering:

```haskell
ppPhase
ppSignal
ppApprovalCtx
ppRunState
ppRunTrace
ppRuntimeStore
printDemoPureTrace
printRuntimeStore
```

This matters because the package is not just about executing workflows. It is also about making workflow execution understandable.

The team has already reviewed the pretty-printed output and found it clear enough for presentation and debugging.

---

# 16. Demo: approval workflow

The first end-to-end demo is a simple approval workflow.

States:

```text
Draft
Review
Rework
Done
```

Signals:

```text
Submit
Approve
Reject
Resubmit
```

Handoffs:

```text
Submitted
Approved
Rejected
Resubmitted
```

Basic flow:

```text
Draft
  -- Submit --> Review

Review
  -- Approve --> Done
  -- Reject --> Rework

Rework
  -- Resubmit --> Review
```

The test signal sequence is:

```text
Submit by alice
Reject by bob, reason: needs legal note
Resubmit by alice
Approve by carol
```

Expected final state:

```text
Done
```

Expected final context:

```text
title: Items custody note
author: alice
revision: 1
approvedBy: carol
rejectedBy: <none>
rejectReason: <none>
```

Expected emitted commands:

```text
notify review-team: document submitted by alice
notify alice: rejected by bob: needs legal note
notify review-team: revision submitted by alice
notify alice: approved by carol
```

This demo is intentionally small, but it exercises the central mechanics:

* typed states;
* typed signals;
* typed handoffs;
* context mutation;
* command emission;
* route movement;
* durable runtime store;
* pretty-printed execution views.

---

# 17. The two execution modes

The package supports two useful execution modes.

## Pure smoke-test execution

The pure runner is a development aid. It runs a machine in memory without durable storage.

It is useful for:

```text
fast unit tests
authoring validation
teaching the state-centered model
checking handler behavior
comparing expected context transitions
```

Conceptually:

```haskell
foldM (stepPure spec) initialRunState signals
```

This mode is not the production runtime. It exists so teams can quickly test the semantic logic of their machines.

## Durable runtime execution

The durable runtime is the operational path.

It uses:

```text
CompiledMachine
ValidateReport
Registry
Store
WorkerCfg
runTick / worker loop
```

The shape is:

```haskell
spec <- approvalSpec
compiled <- compile spec

let report = validate compiled.graph
unless (ok report) $
  fail "validation failed"

registry <- register compiled emptyRegistry

store <- newMemoryStore
inst <- createInstance store compiled initialCtx

enqueueSignal store inst (SubmitSg "alice")
enqueueSignal store inst (RejectSg "bob" "needs legal note")
enqueueSignal store inst (ResubmitSg "alice")
enqueueSignal store inst (ApproveSg "carol")

runTick cfg (storeOf store) registry
runTick cfg (storeOf store) registry
runTick cfg (storeOf store) registry
runTick cfg (storeOf store) registry

final <- loadInstanceView store inst
printRuntimeStore final
```

The production version replaces the memory store with a Postgres-backed store while preserving the same runtime engine interface.

---

# 18. Why this is a strong fit for FUDD

`Hfsm` fits the FUDD ecosystem because it shares the same architectural instincts as the rest of the platform:

```text
typed structures over strings
server-side observability
durable state
explicit runtime records
versioned artifacts
copy-on-write / replay-friendly design
admin-readable projections
small composable modules
Haskell-first correctness
```

It also maps cleanly to the different FUDD product areas.

## Batcher / MonBatch

Batcher lifecycle states can be modeled as HFSM states:

```text
Entered
Prepared
Submitted
Polling
Fetched
Materialized
Completed
Failed
Cancelled
```

Signals can represent:

```text
batch submitted
provider accepted
poll completed
raw result fetched
answer materialized
provider failure
retry requested
```

Commands can represent:

```text
submit provider batch
poll provider
fetch batch result
write S3 raw artifact
materialize answers
notify dashboard
```

This would make batch execution fully inspectable and replay-friendly.

## HBDoc / document publication

Document transformation pipelines can use HFSMs for:

```text
ingest
parse
normalize
structure recovery
block edit
render
commit
publish
review
rollback
```

Because `Hfsm` supports durable steps and snapshots, these pipelines become observable and restartable.

---

# 19. What teams need to define for a new machine

To adopt `Hfsm`, a team defines six domain types:

```haskell
data Phase = ...
data Signal = ...
data Handoff = ...
data Ctx = ...
data Cmd = ...
data Child = ...
```

Then they define:

```haskell
MachineSpec Phase Signal Handoff Ctx Cmd Child
```

They provide codecs:

```haskell
mkCodecSet
  (jsonCodec @Ctx)
  (jsonCodec @Signal)
  (jsonCodec @Cmd)
```

Then they define states:

```haskell
StateSpec
  { key = SomePhase
  , name = someStateName
  , kind = AtomicSk
  , entry = Nothing
  , cases = [...]
  , routes = [...]
  , child = Nothing
  , meta = emptyMetaSpec
  }
```

Then they run the standard pipeline:

```text
compile
validate
register
create instance
enqueue signals
run worker tick
inspect store view
```

This makes machine adoption straightforward.

---

# 20. Current implementation status

The package has moved beyond design discussion.

Implemented or specified so far:

```text
Core modules:
  Name
  Ref
  Version
  Digest
  Path
  Meta
  Codec
  Error
  Core front-end

Spec contracts:
  Case
  Route
  Def
  Build / Indexed direction

Compile contracts:
  CompiledMachine
  ExecTable
  compile / compileMany

Graph contracts:
  MachineGraph
  RegionNd
  StateNd
  CaseNd
  RouteEd
  RoutePlan
  Join / Timer / Break graph records

Validate contracts:
  ValidateReport
  ValidateStats
  validation pass structure

Runtime contracts:
  Runtime.Model
  Store.Class
  Registry
  Engine.Step
  Engine.Worker
  Engine.Replay
  Engine.Break

Demo implementation:
  Approval HFSM
  pure runner
  in-memory store
  runtime execution
  formatted printing
```

---

# 21. How to read the package as a developer

A developer approaching the package should read it in this order.

## First: understand the vocabulary

```text
Hfsm.Core
```

This gives the types used everywhere else.

## Second: understand authoring

```text
Hfsm.Spec.Def
Hfsm.Spec.Case
Hfsm.Spec.Route
```

This explains how to write machines.

## Third: understand normalization

```text
Hfsm.Compile
Hfsm.Graph.Def
Hfsm.Graph.Index
```

This explains what a machine becomes after compilation.

## Fourth: understand safety

```text
Hfsm.Validate.Report
```

This explains what must be true before the machine is trusted.

## Fifth: understand execution

```text
Hfsm.Runtime.Model
Hfsm.Store.Class
Hfsm.Runtime.Engine.Step
Hfsm.Runtime.Engine.Worker
```

This explains how machines run durably.

## Sixth: understand inspection

```text
Hfsm.Render.Print
Demo.Print
```

This shows how execution can be presented.

## Seventh: run the demo

```text
Demo.ApprovalRuntime.main
```

This is currently the best onboarding example.

---

# 22. Recommended first machine for each team

Teams should not start by modeling their most complex workflow.

They should start with one small but real process.

Good first candidates:

```text
Batcher:
  one batch from entered -> submitted -> fetched -> completed

HBDoc:
  one document ingest from uploaded -> parsed -> normalized -> committed

```

The target for a first implementation should be:

```text
one machine
four to seven states
three to six signals
one context type
one command type
one demo signal sequence
one pretty-printed run
```

That is enough to learn the package and expose integration questions without overloading the first use case.

---

# 23. The operational value

The package gives teams a strong operational model.

Instead of a workflow being a long Haskell function with unclear partial progress, it becomes:

```text
a named machine
with a version
compiled to a graph
validated structurally
registered in a runtime
executed through durable signals
recorded as steps
snapshotted after transitions
emitting durable commands
visible through projections
printable for humans
replayable for debugging
migratable over time
```

This is the main promise of `Hfsm`.

It turns orchestration from opaque code into inspectable structure.

---

# 24. The conceptual contract

The package is built on a small number of invariants.

## Invariant 1: authored machines are typed

States, signals, handoffs, context, commands, and child keys should be ADTs, not strings.

## Invariant 2: state handlers are pure

Handlers update context, produce handoffs, and emit commands. They do not perform external effects.

## Invariant 3: routes are structural

Routes map handoffs to movement, waits, child machines, timers, joins, and breakpoints.

## Invariant 4: compiled graphs are pure

The graph contains structure, not closures.

## Invariant 5: runtime state is durable

Signals, steps, snapshots, outbox records, child links, and breakpoints are persisted.

## Invariant 6: validation precedes execution

A machine should compile and validate before it is registered and run.

## Invariant 7: introspection is not optional

Readable trace and store views are part of the package’s purpose.

---

# 25. Where the package goes next

The immediate next development targets should be:

```text
1. Harden Hfsm.Compile against larger nested machines.
2. Finish the validation passes with precise diagnostics.
3. Add Graphviz and Mermaid export.
4. Add a Postgres Store implementation.
5. Add projection rebuild functions.
6. Add replay verification.
7. Add migration diff support.
8. Build a Wapp admin page for machine instances.
9. Create more demos from real FUDD workflows.
```

The first production-grade store should probably be Postgres with:

```text
instance table
signal table
step table
snapshot table
outbox table
child table
break table
projection tables
```

using the usual FUDD database conventions:

```text
uid bigint primary key
uuid external identity
foreign keys with _fk suffix
camelCase or consistent SQL field naming
explicit status fields
append-only event/step records
```

---

# 26. Summary for development teams

`Hfsm` gives FUDD a shared way to define and run complex workflows.

It is:

```text
typed:
  machines are Haskell values with ADT states, signals, handoffs, and context

hierarchical:
  states can contain child regions and child workflows

state-centered:
  dynamic logic lives inside state handlers

validated:
  graph-level correctness checks catch bad workflow structures

durable:
  runtime execution is persisted through signals, steps, snapshots, and outbox records

observable:
  every execution can be inspected, rendered, and eventually visualized

extensible:
  storage, rendering, export, migration, and admin projections are separate layers

FUDD-native:
  it follows the same architecture as EasyWordy, HBDoc, Batcher, OpMaker, and the broader FUDD runtime ecosystem
```

The approval demo shows the complete model in miniature.

The next step for teams is to pick a small real workflow, encode it as an `Hfsm` machine, run it through the memory store, print the execution, and then move toward the durable Postgres-backed runtime once the behavior is correct.
