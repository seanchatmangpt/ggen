# ggen Building Blocks and Packs v26.7.31

**Document Type**: Product Requirements Document + Architecture Requirements Document  
**Status**: READY FOR GALL-CHECKPOINT IMPLEMENTATION  
**Date**: 2026-07-30  
**Repository Baseline**: `main@68952593c40214ac1a681073d65f3902a9cdfce4`  
**Scope**: `ggen bblock`, `ggen pack`, `ggen packs`, marketplace packs, pack registries, `.ggen/packs.lock`, local materialization, receipts, replay, and downstream actuation intents  
**Implementation Authority**: repository maintainers and bounded implementation agents  
**Standing Authority**: an external verifier over exact source, manifests, plans, execution evidence, and receipts

---

## 0. Canon Header

### 0.1 Governing laws

1. **Chatman Equation**: `A = μ(O*)`. Pack metadata, bblock declarations, source registries, project parameters, and policy become usable only after admission into bounded observation `O*`. Resolution and materialization are lawful manufacturing `μ`. The installed project state and its receipts are the artifact `A`.
2. **Atomic-pack truth**: bundles, profiles, bblocks, templates, and product names are composition aliases. The resolved atomic-pack closure is the authoritative capability set.
3. **Zero unreceipted actuation**: no lockfile mutation, cache installation, project materialization, migration, deletion, registry publication, infrastructure change, or downstream consequence may acquire standing without an intent/result receipt chain.
4. **Projection law**: ontology and admitted manifests are authority. Catalog JSON, TOML manifests, lockfiles, generated files, CLI JSON, diagrams, and documentation are projections or observations. A projection cannot silently become authority.
5. **BRCE-only DO boundary**: packs and bblocks may inspect, resolve, plan, validate, install into a local cache, manufacture project files, and emit bounded intents. Cloud, Kubernetes, Terraform, Pulumi, network, deployment, release, and other external actuation remain behind BRCE.
6. **Gall's Law**: the target grows from the working `ggen bblock`, fail-closed `ggen pack`, project-oriented `ggen packs`, marketplace registry, lockfile, policy, and receipt implementations. No checkpoint may replace all working surfaces before an equivalent shared engine is executable.
7. **Chesterton's Fence**: command aliases and divergent strictness are not removed until their callers, admission semantics, side effects, evidence, and failure behavior are named and covered by replayable compatibility tests.
8. **Conway's Law**: one semantic object has one owner. Pack identity, dependency resolution, ownership conflict detection, lockfile mutation, receipt issuance, and standing calculation must not each have multiple independent implementations.
9. **Little's Law**: resolution must reduce duplicate registry reads, repeated hashing, repeated parsing, and overlapping project mutations before adding concurrency.
10. **External witness rule**: a pack installer or bblock executor reports evidence. It cannot promote its own result to `ALIVE`.
11. **No evidence fabrication**: receipts, hashes, signatures, OCEL events, telemetry, filesystem effects, subprocess effects, and registry responses must derive from real execution or the operation must abstain.
12. **Generated-surface law**: generated catalogs, lock projections, documentation indexes, schemas, and examples are changed through their canonical source and generator, never by hand.

### 0.2 Standing vocabulary

- `UNKNOWN`: evidence is absent, stale, incomplete, or bound to a different source, plan, policy, parameter set, or project tree.
- `PARTIAL_ALIVE`: one or more bounded checkpoints executed successfully, but crown requirements are incomplete.
- `ALIVE`: every required conjunct executed and an external verifier admitted the exact aggregate receipt.
- `BLOCKED`: a required external capability, permission, registry, key, source, or tool is unavailable before lawful execution can complete.
- `BUILD_BROKEN`: admitted source or generated project output fails its declared build or verifier contract.
- `UNSUPPORTED`: the requested platform, pack class, source type, migration, projection, or actuation target is outside the admitted capability graph.

Typed refusal codes explain why admission or execution stopped. A refusal code is evidence; it is not a substitute standing state.

---

## 1. Working-Backwards Release Statement

### ggen v26.7.31 makes packs the immutable capability atoms and bblocks their receipted composition calculus

A user can select a building block such as a service surface, process-evidence subsystem, LSP verifier, CI control plane, deployment foundation, or complete product profile. ggen resolves the selection into a deterministic, duplicate-free closure of immutable atomic packs. The user can inspect the closure, conflicts, parameters, source provenance, ownership, generated paths, commands, and consequences before any project state changes.

Planning is pure. Enabling is a transactional local operation. The exact closure is pinned in `.ggen/packs.lock`. Every source object, parameter set, policy version, generated artifact, and ownership claim is bound into a BLAKE3 receipt chain. Re-running the same plan against the same admitted inputs yields the same semantic plan digest. Replaying the receipt either reproduces the same local result or produces a typed divergence report.

`ggen pack`, `ggen packs`, and `ggen bblock` remain available during migration, but they route through one resolver, one lockfile service, one receipt service, one ownership engine, and one standing verifier. Their distinct user intent remains visible:

- `pack`: inspect and acquire one registry-resolved immutable pack;
- `packs`: inspect and reconcile the complete project pack set;
- `bblock`: resolve and enable a named composition of packs.

No bblock directly deploys infrastructure. It may manufacture a downstream actuation intent only after local standing is admitted.

---

## 2. Executive Decision

The current system contains the correct primitives but not yet one closed product model.

The repository already proves several valuable behaviors:

- `ggen bblock` compiles an ontology-derived catalog, expands dependency-first groups, normalizes providers, refuses unsafe paths and cycles, writes deterministic plans, updates `.ggen/packs.lock`, and emits chained BLAKE3 receipts;
- `ggen pack` performs a fail-closed registry-backed installation and emits a provenance receipt over the real installed closure;
- `ggen packs` supports project bring-up by recording resolved or declared pack dependencies in the lockfile and emitting structured JSON;
- the marketplace defines atomic pack classes, dependencies, ownership, templates, queries, policies, validators, receipts, and consequence packs;
- the Fortune 5 deployment pack demonstrates that a bblock can be a generic compiler over ontology-owned composition data without provider-specific branches or direct actuation.

The defect is semantic duplication. Identity validation, strictness, digest algorithms, source resolution, lockfile mutation, installation state, receipt shape, removal behavior, and status vocabulary differ across command surfaces. The target architecture preserves the working fences while collapsing duplicated law into one pack kernel.

The first implementation checkpoint must be read-only. It will inventory and normalize existing manifests and command behavior, emit an equivalence report, and refuse ambiguous objects. It will not rewrite the lockfile or generated project files.

---

## 3. Product Model

### 3.1 Atomic pack

An atomic pack is the smallest independently versioned, content-addressed, policy-checkable capability contribution.

Every admitted atomic pack declares:

- stable pack identifier and semantic version;
- pack class;
- canonical content digest;
- source identity and immutable source revision;
- dependency constraints and optional capability predicates;
- owned files, directories, RDF namespaces, symbols, ports, commands, and generated surfaces;
- parameter schema and defaults, when defaults are permitted;
- templates, queries, transformations, validators, and executable verifiers;
- runtime and toolchain requirements;
- trust tier, signature policy, and publication provenance;
- migration and rollback consequences;
- evidence obligations and receipt schema version;
- license and redistribution constraints.

### 3.2 Atomic pack classes

The canonical classes are retained and made machine-enforced:

1. `surface-*` — enterprise-visible protocol or capability surfaces;
2. `contract-*` — external interface contracts independent of implementation;
3. `projection-*` — language, framework, or target projections;
4. `runtime-*` — execution model and deployment substrate;
5. `policy-*` — admission and governance rules;
6. `validator-*` — executable verification contributions;
7. `receipt-*` — receipt, signature, replay, and evidence formats;
8. `consequence-*` — upgrade, migration, rollback, and breaking-change law;
9. `core-*` — shared ontology, hooks, receipts, validation, policy, and versioning foundations.

A pack may have one primary class and additional capability tags. Class constraints are SHACL-enforced.

### 3.3 Bblock

A bblock is a versioned, parameterized composition graph that resolves to atomic packs. It contains no hidden implementation branch.

A bblock declares:

- stable identifier, version, title, purpose, and owner;
- member packs, selectors, and dependent bblocks;
- required and exclusive capability predicates;
- parameter schema and parameter-to-pack mapping;
- platform/provider variants as data;
- output ownership and directory intent;
- policy profile and trust floor;
- verifier profile;
- migration and removal consequences;
- downstream intent types it may manufacture;
- exclusions and unsupported combinations.

A bblock is not an archive of copied packs. It is a composition rule whose exact resolved closure is pinned by the project lockfile.

### 3.4 Bundle and profile

Bundles and profiles remain aliases over bblocks or atomic packs:

- a bundle names a convenient composition;
- a profile adds constraints, policy, trust, runtime, or organizational choices;
- neither can override atomic-pack identity, integrity, ownership, or verifier law.

### 3.5 Project pack set

The project pack set is the admitted closure recorded in `.ggen/packs.lock`. It includes direct requests, transitive dependencies, selected variants, source pins, digests, parameter digests, ownership claims, compiler version, policy digest, and receipt references.

---

## 4. Users and Required Outcomes

### Project author

The author selects a capability or bblock and receives an explainable plan before mutation. Conflicts and unsupported combinations are named with exact objects and paths.

### Pack author

The author publishes one immutable pack with explicit ownership, dependencies, verifier commands, evidence obligations, and migration consequences. Publication is refused when the pack is not reproducible or its ownership graph is ambiguous.

### Platform architect

The architect composes bblocks without copying implementation. Provider and platform variants are graph data, not conditionals embedded in the CLI.

### Implementation agent

The agent receives exact manifests, source revisions, commands, output boundaries, refusal cases, and machine-readable verification criteria. It cannot infer hidden defaults in strict profiles or modify generated surfaces directly.

### Security and compliance authority

The authority can prove which sources, signatures, policies, parameters, pack closures, generated artifacts, and execution results produced the project state.

### Operator

The operator receives a downstream actuation intent only after local pack standing is admitted. The intent remains inert until BRCE admits and executes it.

### Auditor

The auditor can replay resolution and local materialization from the lockfile and receipt chain, then compare the observed tree and evidence to the admitted result.

---

## 5. Product Requirements

### PR-001 — One shared pack kernel

`ggen pack`, `ggen packs`, and `ggen bblock` must route through one implementation for:

- identity parsing;
- manifest loading;
- source resolution;
- dependency solving;
- capability and ownership conflict detection;
- parameter admission;
- plan construction;
- lockfile reads and transactional writes;
- digest calculation;
- receipt issuance;
- replay;
- standing verification.

Command modules remain thin noun/verb adapters. No command may implement a parallel resolver or receipt format.

### PR-002 — Explicit admission modes

The system must name the difference currently expressed by singular and plural commands:

- `resolved-strict`: the pack must resolve to an immutable source and pass integrity, policy, and verifier admission before installation;
- `declared-bounded`: an unresolved dependency may enter a planning workspace only as `UNKNOWN`, with no materialization or `ALIVE` standing;
- `locked-replay`: every source and version comes from the existing lockfile;
- `offline-mirror`: resolution is confined to admitted local or mirrored sources.

A command cannot silently downgrade from strict resolution to declaration.

### PR-003 — Deterministic resolution

Given the same admitted manifests, registry index revisions, source pins, policy, parameters, platform facts, and compiler version, resolution must produce the same:

- atomic-pack closure;
- dependency order;
- selected variants;
- ownership graph;
- consequence set;
- plan digest.

Ordering is canonical. Duplicate packs collapse by exact identity. Incompatible version constraints, ownership collisions, cycles, missing capabilities, and ambiguous variants are typed refusals.

### PR-004 — Source and registry law

Supported source classes are:

- repository-local workspace;
- local cache;
- public ggen registry;
- private enterprise registry;
- mirrored or air-gapped registry;
- immutable Git source revision;
- immutable content-addressed archive.

Every resolved source must bind an immutable revision and content digest. Mutable branches, floating tags, unpinned remote archives, and source substitution after lock are refused in strict mode.

### PR-005 — Manifest authority

The canonical pack and bblock vocabulary is expressed in RDF and constrained by SHACL. Human-edited TOML may be an admitted carrier. Registry metadata, catalog JSON, CLI schemas, documentation, and diagrams are deterministic projections.

The implementation must provide round-trip equivalence checks between canonical graph facts and retained projections. Unknown fields are preserved or refused according to schema version; they are never silently discarded.

### PR-006 — Lockfile completeness

`.ggen/packs.lock` must bind:

- schema version;
- ggen/compiler version;
- root request set;
- exact atomic-pack closure;
- direct and transitive dependency edges;
- selected variants and profiles;
- admitted parameter digest and non-secret parameter values permitted for retention;
- source class, locator, immutable revision, and content digest;
- signature and trust evidence;
- ownership claims;
- policy and ontology digests;
- materialization plan digest;
- migration/consequence state;
- receipt chain head.

Secrets are referenced by capability or secret identifier and are never stored in the lockfile.

### PR-007 — Pure planning

`plan` performs no project mutation beyond an explicitly selected evidence output directory. It returns machine-readable:

- admitted inputs;
- resolved closure;
- dependency graph;
- ownership graph;
- parameter decisions;
- files and directories to create, replace, merge, preserve, or remove;
- commands and verifier suites to execute;
- consequence and migration steps;
- exclusions;
- predicted receipts and artifact classes;
- plan digest.

Plan generation may read real local state. It must not claim the predicted state exists.

### PR-008 — Transactional local materialization

`enable`, project `sync`, strict `install`, migration, and disable/remove are transactional local operations:

1. admit exact plan and current project-tree digest;
2. emit intent receipt;
3. materialize into a bounded staging area;
4. execute declared validators and real boundary checks;
5. calculate an observed diff;
6. atomically commit owned changes or restore the prior state;
7. update the lockfile;
8. emit result receipt;
9. invoke the external standing verifier.

Partial mutation without a typed recovery receipt is prohibited.

### PR-009 — Ownership and merge law

Every output path, namespace, symbol, and generated region has an ownership class:

- `exclusive` — exactly one pack may own it;
- `shared-merge` — contributions combine through a declared deterministic merge operator;
- `generated-region` — only the named generator may change the bounded region;
- `observe-only` — the pack may inspect but not mutate;
- `external` — owned outside ggen and excluded from mutation.

Undeclared writes, overlapping exclusive ownership, ambiguous merge order, edits outside generated regions, and deletion of unowned content are typed refusals.

### PR-010 — Enable, disable, remove, and rollback

Every materializing pack and bblock must declare inverse consequences or explicitly classify reversal as unsupported.

Disable/remove planning must distinguish:

- files wholly owned and safely removable;
- shared generated regions requiring recomputation;
- user-modified owned files requiring merge or refusal;
- dependent packs that prevent removal;
- irreversible external consequences, which remain outside local removal and require BRCE plans.

Rollback must use the prior admitted tree and receipt chain, not a best-effort deletion list.

### PR-011 — Upgrade and migration

An upgrade resolves old and new closures, computes a semantic graph diff, selects admitted consequence packs, and produces a migration plan. Breaking changes require explicit admission. The result binds pre-state, migration commands, post-state, verifier evidence, and rollback classification.

### PR-012 — Integrity, signatures, and trust

The canonical receipt and plan digest is BLAKE3. Source ecosystems may retain SHA-256 or another ecosystem digest as additional integrity evidence, but not as a replacement for the canonical receipt chain.

Strict admission verifies:

- content digest;
- immutable source revision;
- publisher signature or admitted local-authority signature;
- registry provenance;
- trust tier;
- policy profile;
- revocation status when available;
- dependency closure signatures.

Unsigned packs may be admitted only by an explicit policy profile that records the reduced trust standing.

### PR-013 — Receipt and replay contract

Every mutating operation emits linked intent and result receipts containing:

- operation and command schema;
- source tree and project pre-state digest;
- canonical manifest and ontology digests;
- lockfile pre-state and post-state digests;
- resolved closure and plan digest;
- parameter and policy digests;
- source and signature evidence;
- executed commands and exit status;
- artifact paths and content digests;
- verifier report digest;
- previous receipt digest;
- standing outcome and typed refusals.

Replay must support:

- `verify-only`: validate existing evidence without mutation;
- `re-resolve`: prove whether current sources still resolve to the locked closure;
- `re-materialize`: reproduce local outputs in a clean bounded directory;
- `compare`: emit a machine-readable divergence report.

### PR-014 — Stable command semantics

The target command surface is:

```text
ggen pack search|show|verify|install|remove|publish
ggen packs list|graph|plan|sync|verify|upgrade|replay|doctor
ggen bblock list|show|inspect|plan|enable|disable|diff|verify|replay
```

Existing verbs remain compatibility aliases until equivalence receipts prove migration safety. Every command emits versioned structured JSON. Human formatting is a projection of the same result object.

### PR-015 — Parameter law

Parameters are declared by schema with type, bounds, default policy, sensitivity, source, and affected pack facts. Strict profiles refuse undeclared values and hidden defaults. Secret values are resolved at execution boundaries, redacted from receipts, and represented by stable non-reversible evidence identifiers where policy permits.

### PR-016 — Policy and validator composition

Policy packs participate in admission before materialization. Validator packs contribute executable checks after staging and before commit. A pack cannot waive a repository or profile policy. Validator results include command, environment, evidence paths, duration, and exact subject digest.

### PR-017 — Bblock provider and platform variants

Provider, operating system, architecture, language, runtime, and framework variants are graph facts with explicit constraints. The generic resolver selects them from admitted observations. Provider-specific branches in the bblock CLI are prohibited.

### PR-018 — Downstream intent boundary

A pack or bblock may emit a typed downstream intent only when:

- local materialization is `ALIVE`;
- the exact lockfile and receipt chain head are bound;
- the intent schema is declared by the composition;
- required capabilities and policy are satisfied.

The emitted intent does not execute cloud, deployment, release, package publication, or network mutation. BRCE performs admission and actuation and emits its own execution receipt.

### PR-019 — Observability and OCEL

Resolution, admission, fetch, verification, staging, commit, rollback, replay, and downstream-intent manufacturing emit real process events. OCEL objects include pack, bblock, project, lockfile, plan, receipt, source, policy, artifact, and verifier report. Telemetry cannot establish standing without corroborating execution and state evidence.

### PR-020 — Machine-readable verifier report

Every verification run emits `ggen.verifier.report.v1` in JSON and, when enabled, an RDF/PROV-O projection. The report binds:

- exact subject digests;
- suite inventory;
- commands and boundaries crossed;
- evidence artifacts;
- passed, failed, blocked, unsupported, and skipped-with-law checks;
- refusal codes;
- benchmark measurements and declared budgets;
- replay comparison;
- aggregate standing.

Only the external verifier may set aggregate `ALIVE`.

---

## 6. Required Executable Verification Block

The bblocks/packs subsystem must expose distinct executable suites. A suite name is not evidence; every suite must cross its declared real boundary and publish artifacts into the verifier report.

### 6.1 Protocol and unit suite

Covers canonical serialization, identifier parsing, version constraints, graph closure, deterministic ordering, digest calculation, parameter admission, ownership algebra, and refusal-code mapping. Tests that exercise pure logic must also cross a real serialization, hashing, filesystem, or process boundary and verify the resulting artifact; isolated assertion-only tests do not establish standing.

### 6.2 Property and fuzz suite

Generates bounded valid and invalid manifest graphs, dependency graphs, ownership graphs, paths, versions, and parameter sets. It proves invariants including determinism, cycle refusal, path confinement, duplicate elimination, lockfile round-trip, no panic on hostile input, and receipt tamper detection. Fuzz corpora are retained as real inputs with hashes; telemetry and receipts are produced by actual runs.

### 6.3 stdio and HTTP integration suite

Exercises the real CLI stdio contract and any admitted registry/daemon HTTP boundary. It validates structured JSON schemas, exit behavior, streaming or body integrity, authentication refusal, immutable-source retrieval, cache writes, lockfile writes, and receipt production. Primary boundaries are never mocked.

### 6.4 Black-box CLI end-to-end suite

Runs the built `ggen` binary in clean temporary projects through:

- strict pack install;
- declared dependency planning;
- bblock inspect, plan, enable, verify, disable, and replay;
- conflict and unsupported cases;
- exact lockfile and project-tree comparisons;
- verifier report emission.

The suite validates causality: the command caused the observed files, state, process events, and receipt chain.

### 6.5 Security suite

Verifies path traversal refusal, archive escape refusal, symlink escape refusal, signature and digest tampering, malicious manifest fields, command injection resistance, secret redaction, source substitution, registry downgrade, ownership escape, untrusted generated-region edits, and policy weakening refusal.

### 6.6 Chaos suite

Injects real process interruption and environmental failure at bounded checkpoints: registry disconnect, truncated download, disk exhaustion, permission denial, lock contention, verifier termination, staging interruption, and receipt-write failure. It proves atomicity, recovery receipts, and absence of unclassified partial state.

### 6.7 Stress suite

Exercises large dependency closures, deep but bounded graphs, many bblocks, large lockfiles, concurrent read-only plans, serialized mutations, and repeated replay. It records memory, CPU, I/O, queue depth, and lock contention.

### 6.8 Benchmark suite

Measures cold and warm resolution, manifest parsing, graph closure, ownership analysis, hashing, lockfile read/write, planning, staging, verification, and replay. Budgets are versioned policy, not hardcoded folklore. Regressions require a machine-readable comparison and explicit admission.

### 6.9 Replay suite

Replays retained intent/result chains against clean bounded directories, proves deterministic semantic plan identity, verifies artifact digests, and reports environmental divergence separately from source or policy divergence.

### 6.10 Verifier-report suite

Validates `ggen.verifier.report.v1` against its schema and checks that every claimed capability has linked execution, evidence, and receipt objects. A missing suite, stale digest, fabricated artifact reference, or self-promoted standing is refused.

### 6.11 Verification ladder

Standing expands in this order:

```text
protocol/unit
→ property/fuzz
→ stdio+HTTP integration
→ black-box CLI E2E
→ security
→ chaos
→ stress
→ benchmark
→ replay
→ external verifier report
```

A lower checkpoint may be `PARTIAL_ALIVE`. The subsystem is `ALIVE` only after the required ladder for the change class is executed and externally admitted.

---

## 7. Target Architecture

### 7.1 Authority and execution layers

```text
Canonical pack + bblock ontology
        │
        ├── SHACL admission and policy packs
        │
        ├── admitted carriers
        │       ├── pack.toml / package.toml
        │       ├── bblock.toml
        │       └── project observations
        │
        ├── deterministic projections
        │       ├── registry index
        │       ├── catalog JSON
        │       ├── CLI schemas
        │       ├── docs and diagrams
        │       └── verifier schemas
        │
        ├── shared pack kernel
        │       ├── source adapter
        │       ├── resolver
        │       ├── capability/ownership solver
        │       ├── parameter admission
        │       ├── planner
        │       ├── transactional materializer
        │       ├── lockfile service
        │       ├── receipt/replay service
        │       └── verifier client
        │
        ├── noun/verb adapters
        │       ├── ggen pack
        │       ├── ggen packs
        │       └── ggen bblock
        │
        ├── external standing verifier
        │       └── verifier report + aggregate receipt
        │
        └── BRCE
                └── downstream admission, actuation, receipt, replay hook
```

### 7.2 Shared pack kernel boundaries

#### Manifest admission

Loads RDF authority and admitted carriers, applies schema versioning and SHACL, and produces normalized pack/bblock objects. It performs no source fetch or project mutation.

#### Source adapter

Resolves immutable objects from local, registry, mirror, Git, or archive sources. It writes only to a bounded cache staging area and returns content-addressed evidence.

#### Resolver

Computes dependency and capability closure. It is deterministic, side-effect-free after admitted source indexes are loaded, and emits a resolution graph plus typed refusals.

#### Ownership solver

Computes exclusive and shared ownership, merge operators, generated regions, and removal consequences against the observed project tree.

#### Planner

Combines closure, parameters, policy, ownership, current state, validators, and consequences into an immutable plan.

#### Transactional materializer

Applies an admitted plan to a staging tree, executes validators, calculates the observed diff, and atomically commits or rolls back.

#### Lockfile service

Owns parsing, validation, migration, canonical ordering, transactional persistence, and pre/post digests for `.ggen/packs.lock`.

#### Receipt and replay service

Owns BLAKE3 receipt schemas, chain linking, artifact hashing, replay modes, and divergence reports.

#### External standing verifier

Consumes execution evidence and emits the machine-readable verifier report. It is outside the executor that produced the evidence.

### 7.3 Storage layout

```text
.ggen/
├── packs.lock
├── cache/
│   └── blake3/<content-digest>/
├── plans/
│   ├── packs/<plan-digest>.json
│   └── bblocks/<plan-digest>.json
├── staging/<operation-id>/
├── receipts/
│   ├── intents/
│   ├── results/
│   └── chain.json
├── verifier/
│   └── <operation-id>.report.json
├── replay/
│   └── <operation-id>/
└── ocel/
    └── <operation-id>.json
```

The existing `.ggen/bblocks/` layout remains readable during migration. A checkpoint may project compatibility files there, but the canonical future layout is content- and operation-addressed rather than provider/group path-addressed.

---

## 8. Data Contracts

### 8.1 Canonical pack identity

```text
PackIdentity = namespace + name + semantic version + content digest
```

Human aliases cannot replace the content digest in a lock or receipt.

### 8.2 Resolution graph

The resolution graph contains nodes for requested bblocks, direct packs, transitive packs, variants, policies, validators, sources, and consequences. Edges are typed: `requires`, `selects`, `conflicts`, `provides`, `owns`, `validates`, `migrates`, and `manufacturesIntent`.

### 8.3 Plan identity

```text
plan_digest = BLAKE3(
  canonical manifests
  + admitted source index revisions
  + resolved closure
  + parameters
  + policy
  + current project-tree digest
  + ownership graph
  + consequence graph
  + compiler version
)
```

Two plans with different observed project trees are different plans even when the requested bblock is the same.

### 8.4 Receipt chain

```text
previous result receipt
→ operation intent receipt
→ staged artifact evidence
→ command and validator evidence
→ committed state evidence
→ operation result receipt
→ external verifier report
→ aggregate standing receipt
```

### 8.5 Refusal code families

- `PACK-ID-*` — malformed or ambiguous identity;
- `PACK-SOURCE-*` — missing, mutable, substituted, or untrusted source;
- `PACK-DEP-*` — cycle, unsatisfied constraint, or closure conflict;
- `PACK-OWN-*` — ownership collision or mutation outside authority;
- `PACK-PARAM-*` — missing, hidden, invalid, or secret-handling violation;
- `PACK-LOCK-*` — stale, malformed, non-canonical, or incomplete lockfile;
- `PACK-INTEGRITY-*` — digest, signature, provenance, or revocation failure;
- `PACK-POLICY-*` — policy admission failure or weakening attempt;
- `PACK-MATERIALIZE-*` — staging, validation, atomic commit, or rollback failure;
- `PACK-RECEIPT-*` — missing, stale, broken, or tampered receipt chain;
- `PACK-REPLAY-*` — source, environment, artifact, or policy divergence;
- `BBLOCK-*` — composition, variant, parameter, or downstream-intent failure;
- `VERIFIER-*` — missing suite, stale evidence, self-promotion, or report-schema failure.

---

## 9. Compatibility and Migration

### 9.1 Preserved fences

The migration preserves these observed behaviors until equivalence is proven:

- `ggen bblock providers|list|inspect|group|plan|enable|validate`;
- provider alias normalization retained as data;
- dependency-first deterministic bblock closure;
- local-only bblock actuation boundary;
- `.ggen/packs.lock` compatibility;
- strict registry installation through `ggen pack add`/`install`;
- project declaration workflow through `ggen packs install`;
- structured JSON outputs;
- existing receipt locations readable by replay and migration tooling.

### 9.2 Semantic normalization

The shared kernel classifies legacy operations:

| Existing surface | Preserved intent | Target semantic mode |
|---|---|---|
| `ggen pack add/install` | acquire one real registry pack | `resolved-strict` |
| `ggen packs install` resolved | add one project dependency | `resolved-strict` project reconciliation |
| `ggen packs install` unresolved | record planned dependency | `declared-bounded`, standing `UNKNOWN` |
| `ggen bblock plan` | write deterministic local plan evidence | pure plan with compatibility projection |
| `ggen bblock enable` | create local directories and lock entries | transactional bblock materialization |

### 9.3 Digest migration

Existing SHA-256 integrity fields remain readable as source-integrity evidence. New canonical plans and receipts use BLAKE3. Migration writes both when ecosystem compatibility requires SHA-256, with explicit algorithm labels and no digest reinterpretation.

### 9.4 Lockfile migration

Lockfile schema upgrades are explicit, reversible when possible, and receipted. The upgrader retains unknown extension fields or refuses the migration. It never silently truncates data.

### 9.5 Generated documentation and indexes

Documentation trees, indexes, catalogs, and ontology projections are regenerated by repository-owned commands after the canonical document or graph changes. Their generated outputs are not hand-edited in an implementation checkpoint.

---

## 10. Gall Checkpoints

### G0 — Fence inventory and machine-readable equivalence report

**State target**: `PARTIAL_ALIVE`

Deliver:

- inventory of all pack, packs, bblock, marketplace, lockfile, registry, policy, receipt, and generated surfaces;
- exact command behavior matrix;
- schema and digest matrix;
- ownership map;
- compatibility corpus from real repository packs and bblocks;
- `ggen.pack-equivalence.report.v1`.

No mutating behavior changes.

### G1 — Canonical schemas and read-only shared resolver

**State target**: `PARTIAL_ALIVE`

Deliver:

- RDF/SHACL vocabulary for pack and bblock objects;
- normalized internal model;
- read-only source-index adapters;
- deterministic dependency/capability/ownership resolver;
- CLI `inspect`, `graph`, and pure `plan` routed through the shared kernel;
- property/fuzz and replay evidence against retained catalogs.

Legacy mutators remain authoritative.

### G2 — Shared lockfile, digest, receipt, and verifier services

**State target**: `PARTIAL_ALIVE`

Deliver:

- canonical lockfile service with compatibility migration;
- BLAKE3 plan and receipt schemas;
- external verifier report;
- compatibility adapters for existing receipt paths;
- black-box proof that legacy and shared read-only results are equivalent or explicitly classified.

### G3 — Transactional strict pack installation

**State target**: `PARTIAL_ALIVE`

Route strict singular pack installation through staging, ownership checks, transactional lock update, real validators, receipts, replay, and external standing. Retain the legacy command spelling.

### G4 — Transactional project reconciliation and bblock enable

**State target**: `PARTIAL_ALIVE`

Route `ggen packs sync` and `ggen bblock enable` through the same materializer. Add disable/diff/replay. Prove rollback under chaos cases. Retire only duplicated implementations whose equivalence receipts pass.

### G5 — Registry trust, publication, upgrade, and consequence packs

**State target**: `PARTIAL_ALIVE`

Add immutable publication, signatures, trust tiers, revocation evidence, source mirrors, semantic upgrade planning, consequence execution, and rollback classification.

### G6 — Crown verifier and downstream BRCE intent integration

**State target**: `ALIVE`

Require the complete verification ladder, external aggregate standing, OCEL process evidence, replay, and a BRCE-compatible downstream intent. No external actuation is added to bblocks or packs.

---

## 11. Acceptance Criteria

The architecture is accepted only when all required criteria are executed against the exact implementation tree.

1. One shared kernel owns resolution, ownership, lockfile, digest, receipt, replay, and standing semantics.
2. Existing command surfaces either route through the shared kernel or have an explicit compatibility fence and retirement checkpoint.
3. The same admitted inputs produce the same semantic closure and plan digest across repeated runs.
4. Unknown, mutable, substituted, tampered, cyclic, conflicting, path-escaping, or policy-invalid inputs are refused with typed codes.
5. Strict operations never downgrade to unresolved declaration.
6. Planning does not mutate project state.
7. Materialization is staged, validated, atomic, and recoverable.
8. The lockfile binds the exact closure, sources, parameters, policy, ownership, plan, and receipt head.
9. BLAKE3 receipt chains verify and tampering is detected.
10. Disable/remove never deletes unowned content and refuses unresolved dependents or unsafe user modifications.
11. Replay reproduces the same semantic result or emits a bounded divergence report.
12. Provider/platform selection is data-driven with no provider-specific CLI branches.
13. No bblock or pack command directly performs external infrastructure or deployment actuation.
14. Every required suite emits real evidence and appears in `ggen.verifier.report.v1`.
15. Aggregate `ALIVE` is emitted only by the external verifier.
16. Generated catalogs, schemas, docs indexes, and projections pass drift verification.
17. Compatibility black-box tests prove preserved legacy behavior at each retirement boundary.
18. Security, chaos, stress, benchmark, and replay reports are retained and bound to the aggregate receipt.

---

## 12. Exclusions

This PRD/ARD does not authorize:

- direct cloud, Kubernetes, Terraform, Pulumi, deployment, release, or registry-side mutation from `ggen bblock`;
- hidden pack defaults in strict profiles;
- mutable source references in lockfiles;
- handwritten provider branches in the CLI;
- pack installation without ownership analysis;
- lockfile mutation without an intent/result receipt;
- self-declared standing by the command that performed the operation;
- generated-output edits that bypass canonical source and generator;
- fabricated telemetry, receipts, signatures, source responses, or process events;
- mocks of primary evidence boundaries;
- one-shot replacement of every existing pack surface;
- automatic destructive migration where rollback is unsupported or evidence is incomplete.

---

## 13. Falsifiers

The design is falsified by any observed case where:

1. two command surfaces resolve the same admitted request to different closures without a declared semantic mode difference;
2. a changed source, manifest, parameter, policy, project tree, or compiler version retains the same plan digest;
3. a pack reaches `ALIVE` while unresolved, unsigned under a signature-required profile, or bound to a mutable source;
4. plan changes project files or the lockfile;
5. enable/install leaves unclassified partial state after interruption;
6. removal deletes user or pack content outside declared ownership;
7. a provider or platform requires a new conditional branch in the generic bblock CLI;
8. a receipt verifies after any bound artifact or command result is altered;
9. replay claims equivalence while the semantic closure or artifact digests differ;
10. a passing status has no linked real execution evidence;
11. the executor promotes its own operation to aggregate `ALIVE`;
12. a bblock directly actuates infrastructure or external production state.

Any falsifier stops promotion and produces a typed report.

---

## 14. Operationalization

### 14.1 First implementation issue

Create the G0 fence inventory and equivalence reporter. The issue must name exact repository paths, commands, schemas, and expected output. It must not alter mutating behavior.

### 14.2 First executable commands

The first checkpoint should expose repository entry-point commands through `just`, including bounded equivalents of:

```text
just pack-fence-inventory
just pack-equivalence-report
just pack-protocol-unit
just pack-property-fuzz
just pack-cli-e2e
just pack-verifier-report
```

Exact task names may follow existing repository naming law, but there must be one stable entry point per suite and one aggregate verifier entry point.

### 14.3 Evidence retained per checkpoint

Each checkpoint retains:

- exact source SHA and tree digest;
- command inventory;
- environment/toolchain manifest;
- normalized manifest and schema digests;
- test and verifier artifacts;
- benchmark report when applicable;
- intent/result receipts for mutations;
- OCEL process evidence;
- machine-readable aggregate verifier report;
- explicit standing state.

### 14.4 Promotion rule

A checkpoint may merge as `PARTIAL_ALIVE` when its bounded acceptance criteria pass and it does not weaken the existing crown. The complete bblocks/packs architecture becomes `ALIVE` only at G6 after the full external verifier admits the exact implementation and replay evidence.

---

## 15. Final Architectural Invariant

```text
requested capability or bblock
→ admitted manifests, sources, parameters, policy, and project observation
→ deterministic atomic-pack closure
→ ownership and consequence proof
→ pure plan
→ intent receipt
→ transactional local materialization
→ real executable verification ladder
→ result receipt
→ replay comparison
→ external verifier report
→ ALIVE local standing
→ optional BRCE intent
→ separately admitted external actuation and receipt
```

**Packs are the capability atoms. Bblocks are the composition calculus. The lockfile is admitted project state. Receipts prove consequence. BRCE alone crosses the external DO boundary.**
