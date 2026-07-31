# ggen CI/CD Release-Law Refactor v26.7.31

**Document Type**: Product Requirements Document + Architecture Requirements Document  
**Status**: READY FOR GALL-CHECKPOINT IMPLEMENTATION  
**Date**: 2026-07-30  
**Repository Baseline**: `main@faa52dac474d456ae00105869770161d666ba31f`  
**Implementation Authority**: repository maintainers and bounded implementation agents  
**Promotion Authority**: external evidence witness; no workflow may self-promote its own standing

---

## 0. Canon Header

### Governing laws

1. **Chatman Equation**: admitted observation enters lawful manufacturing and produces an artifact with standing. CI status text alone is not standing.
2. **Zero unreceipted actuation**: no release, deployment, registry publication, branch mutation, or production consequence may occur without a receipt that binds the exact source tree, policy, executor, evidence, and result.
3. **BRCE-only DO boundary**: CI may observe, validate, manufacture evidence, and emit intents. Production actuation remains capability-bounded through BRCE.
4. **Gall's Law**: the replacement must grow from already working workflows and reusable actions. It must not replace the repository's CI/CD estate in one unverified rewrite.
5. **Little's Law**: reduce work in progress, duplicate queues, and repeated setup before adding runner capacity.
6. **Conway's Law**: one capability must have one workflow owner, one evidence contract, and one standing authority. Duplicate workflow ownership is an organizational defect expressed as YAML.
7. **Chesterton's Fence**: no existing workflow or job is deleted until its purpose, caller, evidence obligation, and standing effect are named and a shadow replacement proves equivalent or stronger behavior.
8. **External witness rule**: a capability lane reports evidence. A separate standing job verifies the evidence. The lane cannot declare itself promotable.
9. **Projection law**: the canonical graph is authority; `cicd.toml` is an admitted observation carrier; GitHub Actions YAML is a deterministic projection; execution receipts are observations of what actually happened.

### Standing vocabulary

- `UNKNOWN`: required execution evidence is absent, incomplete, stale, or bound to a different source tree.
- `REFUSED`: a required law, capability, command, evidence item, or provenance check failed.
- `ALIVE`: every required conjunct passed and the external standing verifier admitted the exact aggregate receipt.
- `PARTIAL_ALIVE`: useful diagnostic state for humans, but never sufficient for merge, release, or deployment promotion.

---

## 1. Working-Backwards Release Statement

### ggen v26.7.31 makes CI/CD one receipted manufacturing system

The ggen repository no longer treats every capability as an independent GitHub Actions island. A pull request now enters one CI control plane. That control plane admits the event, computes the impacted capability graph, calls reusable verification lanes, and produces one externally verified standing receipt.

Maintainers see stable check names instead of a changing wall of workflow statuses. Superseded commits cancel cleanly. Documentation-only changes do not compile the entire Rust workspace. Full-crown verification still runs before merge and on `main`. Bot-authored or workflow-authored commits no longer generate misleading zero-job `action_required` shells. Release workflows consume the exact commit's admitted standing instead of rerunning an unrelated subset of checks.

The repository's existing `github-actions-pack`, `setup-ggen` action, evidence emitter, reusable Rust inspection workflow, capability crowns, and release machinery remain the seed. The refactor removes duplicate orchestration, not proven capability logic.

---

## 2. Executive Decision

A CI/CD refactor is required.

The repository already contains the correct architectural seed: a GitHub Actions ontology, refusal gates, generated reusable inspection workflow, setup actions, evidence emission, and a documented shadow-retrofit sequence. However, the aggregate v26.7.30 merge exposed that the repository still executes as a collection of independently triggered workflows rather than as one admitted production process.

The target is **not fewer tests**. The target is fewer orchestration surfaces, stable branch-protection checks, deterministic impact planning, shared setup and cache law, explicit evidence contracts, and one external standing decision.

The first implementation checkpoint must not add a new Rust crate. Existing ggen pack generation, composite actions, reusable workflows, shell witnesses, and engine tests are sufficient to prove the architecture before any new runtime is justified.

---

## 3. As-Built Evidence

### 3.1 Existing production-process model

`packs/github-actions-pack/README.md` already defines four layers:

1. event;
2. workflow;
3. job and step;
4. production output.

It also distinguishes step execution, step success, job success, evidence admission, and standing acquisition. That distinction is retained as constitutional law.

The same pack already refuses excessive permissions, mutable third-party action references, unsafe `pull_request_target`, and undeclared secret use. These gates remain authoritative and are expanded rather than replaced.

### 3.2 Existing shadow retrofit

The repository already manufactures:

- `.github/workflows/reusable-rust-inspection.yml`;
- `.github/actions/emit-evidence/action.yml`;
- `.github/actions/setup-ggen/action.yml`;
- a documented caller example for shadow execution.

The pack explicitly prescribes running generated and handwritten inspection in shadow, comparing results, then retiring duplicate jobs. This PRD/ARD promotes that sequence to the repository-wide migration plan.

### 3.3 Aggregate-merge symptom

The final v26.7.30 consolidation required an exact-head one-use finalizer because no single persistent workflow owned the full merge, compatibility repair, normalization, targeted tests, complete engine verification, receipt publication, and self-removal sequence.

After the verified finalizer pushed the exact PR head, more than twenty workflow shells were associated with the same commit and reported `action_required` without executed jobs. These shells were not failing tests, but they were indistinguishable from failures at the status-summary surface. This is an observability and event-topology defect even when the underlying source is correct.

### 3.4 Structural diagnosis

The repository has four coupled problems:

1. **Trigger fan-out**: many top-level workflows independently subscribe to the same pull-request and push events.
2. **Repeated production setup**: toolchain provisioning, checkout, formatting, CLI build, cache setup, and evidence handling are duplicated.
3. **Fragmented standing**: capability workflows report local success, but no durable aggregate authority binds all required evidence to one exact commit.
4. **Recursive or synthetic event ambiguity**: commits produced by automation can create empty, blocked, skipped, or policy-gated status shells that are not classified separately from executed verification.

---

## 4. Product Goals

### 4.1 Primary goals

1. Provide one authoritative CI ingress for pull requests, merge queues, and `main` pushes.
2. Compute the required capability set from the changed paths and repository policy graph.
3. Reuse one Rust inspection implementation rather than duplicate command lists.
4. Preserve every proven capability crown while changing only how it is invoked and witnessed.
5. Produce one aggregate standing receipt bound to the exact commit SHA and tree.
6. Expose stable branch-protection check names independent of internal workflow decomposition.
7. Eliminate zero-job `action_required` ambiguity through explicit event-provenance classification and non-recursive orchestration.
8. Reduce queue depth and redundant compilation without weakening merge or release law.
9. Manufacture GitHub Actions YAML from the canonical graph and refuse handwritten drift.
10. Keep production actuation outside the verification workflows and behind BRCE.

### 4.2 Secondary goals

- make CI behavior locally explainable through `ggen` commands and generated plans;
- make every workflow's owner, input, output, permission ceiling, evidence obligation, and standing effect machine-readable;
- allow capability teams to evolve their verification profile without creating new top-level event subscriptions;
- retain a complete OCEL-compatible execution history for throughput and failure analysis;
- support merge queues without rerunning unrelated release or deployment workflows.

### 4.3 Non-goals

- replacing GitHub Actions as the current execution substrate;
- deleting all existing workflows in one PR;
- creating a generic agent framework;
- adding LLM calls to CI planning or standing decisions;
- allowing a successful job to self-declare release standing;
- moving production secrets into pull-request workflows;
- introducing a new CI orchestration crate before the generated pack and reusable-workflow path is exhausted;
- treating caches as evidence of correctness.

---

## 5. Users and Required Outcomes

### Maintainer

The maintainer sees at most four stable top-level checks and one final `ci/standing` decision. A failed standing check names the missing or refused capability and links to its bounded evidence.

### Capability owner

The capability owner defines a verification profile in the canonical graph. The profile declares paths, commands, prerequisites, artifacts, timeout, permission ceiling, and evidence schema. It does not create another repository-wide trigger.

### Release authority

The release authority receives a commit-bound standing receipt. Publication or deployment is refused when the receipt is absent, stale, incomplete, synthetic, or not admitted by BRCE.

### Implementation agent

The implementation agent receives a deterministic Gall checkpoint with exact files, commands, fixtures, acceptance tests, and refusal tests. The agent cannot weaken branch protection, skip negative fixtures, or promote its own output.

### Auditor

The auditor can reconstruct why a commit was admitted from the ontology version, projected YAML hash, workflow and action SHAs, toolchain and lock hashes, command results, artifacts, and aggregate receipt chain.

---

## 6. Required Product Behavior

### PR-001 — One event admission surface

A single top-level workflow, provisionally named `.github/workflows/ci-control-plane.yml`, must own:

- `pull_request`;
- `merge_group`;
- `push` to `main`;
- bounded `workflow_dispatch` diagnostics.

Other verification workflows must be callable through `workflow_call` or composite actions. Capability workflows must not independently subscribe to repository-wide pull-request or `main` push events after migration.

### PR-002 — Deterministic impact plan

The control plane must calculate a deterministic capability plan from:

- changed paths;
- dependency and ownership facts;
- universal admission requirements;
- event class;
- merge or release policy.

The plan must be serialized as an artifact and included in the aggregate receipt. An unknown path or missing owner is a typed refusal, not permission to skip verification.

### PR-003 — Stable checks

The public check surface must converge on these names:

- `ci/admission`;
- `ci/inspection`;
- `ci/capabilities`;
- `ci/standing`.

Internal job names may evolve. Branch protection must depend on `ci/standing`, and optionally `ci/admission` during migration, rather than on every capability workflow name.

### PR-004 — Shared Rust inspection

All Rust formatting, linting, build, test, and doctest jobs must call the generated reusable Rust inspection workflow or a successor generated from the same ontology. Command profiles may vary by scope, but command semantics must have one owner.

### PR-005 — Capability profiles

Each capability crown must declare a reusable profile containing:

- capability identifier;
- owned paths;
- prerequisites;
- exact commands;
- required fixtures;
- expected artifacts;
- timeout;
- matrix bounds;
- permission ceiling;
- standing contribution;
- refusal codes.

Profiles are graph data. YAML copies of profile logic are prohibited.

### PR-006 — External standing witness

Every lane emits evidence. A separate `standing` job runs with `if: always()` after all required lanes. It verifies:

- exact commit and tree identity;
- plan identity;
- required-lane completeness;
- command outcomes;
- artifact hashes;
- workflow and action provenance;
- policy version;
- absence of unclassified cancellation or skipped work.

Only the standing witness may emit `ALIVE`.

### PR-007 — Event provenance and synthetic-run classification

Every run must record whether its initiating event was human-authored, GitHub-authored, application-authored, workflow-authored, merge-queue-authored, or release-authored.

A workflow-generated commit must not recursively create another verification topology. The preferred behavior is:

1. verification workflows do not push source commits;
2. generated changes are proposed through a separate branch and PR;
3. release or maintenance mutation uses a dedicated capability-bounded identity;
4. the new commit receives a normal, independent control-plane run.

A zero-job or policy-blocked workflow shell must be classified as `CI-SHELL-001`, excluded from evidence, and surfaced separately from a test failure. The system must not silently treat it as success.

### PR-008 — Queue and concurrency law

For pull requests, all lanes share a concurrency group derived from the PR number and head SHA. A newer head cancels the older head's non-release work.

Merge-queue verification is exact-head and non-reusable across a changed merge-group tree.

Release and deployment work is never canceled by a newer source commit after actuation admission begins.

Matrix jobs must declare `max-parallel`. Increasing runners is not an acceptable substitute for duplicate-work removal.

### PR-009 — Cache law

Cache keys must bind at least:

- runner platform;
- pinned Rust toolchain;
- `Cargo.lock` hash;
- build profile;
- relevant feature or package set.

A cache hit or miss is recorded as performance evidence only. Cache state cannot establish correctness standing. Restored executable artifacts must be revalidated or rebuilt under the lane's law.

### PR-010 — Security and permissions

Every workflow and job must declare an explicit permission ceiling. Default verification is `contents: read`. Write permissions, OIDC, packages, deployments, or release permissions are confined to capability-specific jobs after standing admission.

All third-party actions must be pinned to immutable commit SHAs. Secrets are prohibited in untrusted pull-request execution. `pull_request_target` remains refused unless a separately reviewed law proves no untrusted checkout or evaluation path.

### PR-011 — Generated workflow authority

The canonical workflow graph must manufacture:

- top-level control-plane YAML;
- reusable workflow YAML;
- composite action metadata where applicable;
- branch-protection check-name inventory;
- capability/path map;
- documentation and diagrams;
- evidence schema fixtures.

A direct edit to generated YAML without the matching graph change is `CI-DRIFT-001` and must fail closed.

### PR-012 — Release admission

A release workflow must consume the exact commit's admitted standing receipt. It must not infer standing from branch name, tag presence, or a collection of green UI checks.

Release preparation may manufacture an intent. Registry publication, deployment, or external mutation requires BRCE admission and a resulting execution receipt.

### PR-013 — Local parity

The repository must expose a local command surface that produces the same plan and runs the same bounded profiles used in GitHub Actions. The preferred interface is generated through existing ggen CLI conventions:

- `ggen sync --locked` for projection;
- `ggen doctor` for configuration diagnostics;
- `ggen receipt verify` for evidence verification;
- a generated CI noun/verb surface for `plan`, `verify`, `standing`, and `drift`.

The CLI validates and projects. Domain tools perform their native checks. The external witness calculates standing.

---

## 7. Target Architecture

### 7.1 Authority layers

```text
Canonical CI/CD graph
        │
        ├── admitted project observation: cicd.toml
        │
        ├── ggen projection
        │       ├── ci-control-plane.yml
        │       ├── reusable inspection workflows
        │       ├── reusable capability workflows
        │       ├── composite actions
        │       └── generated documentation
        │
        ├── GitHub Actions execution
        │       ├── admission
        │       ├── inspection
        │       ├── capability lanes
        │       └── evidence artifacts
        │
        ├── external standing witness
        │       └── aggregate BLAKE3 receipt + OCEL events
        │
        └── BRCE
                └── admitted release/deployment actuation + execution receipt
```

### 7.2 Top-level workflow topology

#### `ci-control-plane.yml`

Responsibilities:

- admit event and source identity;
- calculate changed paths;
- call the impact planner;
- invoke universal inspection;
- invoke required capability profiles;
- invoke documentation verification when required;
- collect evidence;
- calculate final standing.

This workflow does not publish releases or modify source.

#### `release-admission.yml`

Responsibilities:

- react to admitted tag or release intent;
- retrieve the exact commit's standing receipt;
- verify freshness and provenance;
- manufacture a BRCE intent;
- record admitted or refused release standing.

#### Scheduled operations

Nightly, weekly, drift, benchmark, and long-horizon workflows remain separate event classes only when their event semantics are truly different. They call the same reusable verification profiles and evidence contracts. A schedule is not permission to duplicate commands.

### 7.3 Reusable workflow set

1. `reusable-rust-inspection.yml` — existing generated seed; fmt, lint, build, test, doctest, and aggregate inspection result.
2. `reusable-capability-verify.yml` — parameterized capability profile executor.
3. `reusable-docs-verify.yml` — book, generated docs, link, and drift verification.
4. `reusable-security-verify.yml` — permissions, action pinning, secret law, dependency and supply-chain evidence.
5. `reusable-standing.yml` — external aggregate evidence verifier.

The implementation may preserve additional specialized reusable workflows when the capability requires a materially different executor. It may not create another repository-wide trigger merely for naming convenience.

### 7.4 Composite actions

Retain and converge on:

- `setup-ggen`;
- `emit-evidence`;
- one pinned Rust/tooling setup action if the existing reusable workflow cannot fully own setup;
- one bounded artifact-hash action if shell duplication remains.

Composite actions perform setup or evidence mechanics. They do not decide standing.

### 7.5 Canonical graph model

The GitHub Actions pack must represent at minimum:

- `EventClass`;
- `WorkflowDefinition`;
- `CapabilityProfile`;
- `PathOwnershipRule`;
- `CommandObligation`;
- `EvidenceObligation`;
- `PermissionCeiling`;
- `ConcurrencyPolicy`;
- `CachePolicy`;
- `StandingConjunct`;
- `ActuationIntent`;
- `RefusalCode`;
- `LegacyWorkflowFence`.

Public vocabulary should carry generic provenance, catalog, policy, measurement, and event semantics where available. Custom `gha:` terms name repository-specific CI/CD standing concepts only when public ontologies do not already supply them.

### 7.6 `cicd.toml`

`cicd.toml` is the bounded project observation carrier. It may select profiles, declare repository-specific path groups, set non-semantic execution bounds, and identify release channels. It must not override constitutional laws such as immutable action pinning, external standing, BRCE-only actuation, or required evidence.

---

## 8. Evidence and Receipt Contract

Every capability lane must emit one structured evidence document through the existing evidence emitter contract or its generated successor.

### 8.1 Required fields

- repository identity;
- commit SHA;
- source tree hash;
- pull request or merge-group identity;
- event type and event actor class;
- control-plane workflow hash;
- reusable workflow hash;
- action hashes;
- canonical policy graph hash;
- impact-plan hash;
- toolchain identity;
- lockfile hash;
- command vector;
- start and completion timestamps;
- exit status;
- typed refusal code when not successful;
- artifact names and BLAKE3 hashes;
- cache observation;
- runner observation;
- capability identifier;
- claimed standing contribution.

### 8.2 Aggregate receipt

The standing witness emits an aggregate receipt containing:

- exact required capability set;
- evidence item hashes;
- missing, refused, skipped, canceled, or superseded items;
- conjunctive standing result;
- previous receipt link when part of a chain;
- OCEL-compatible event identifiers;
- strongest evidence-supported claim.

A weighted score cannot override a missing mandatory conjunct.

### 8.3 Receipt storage

Receipts are uploaded as immutable workflow artifacts and may be mirrored to the repository's evidence ledger through a separately admitted process. Generated build outputs are not committed merely to make CI visible.

### 8.4 Freshness

A receipt is stale when any of the following differs:

- commit or tree;
- policy graph;
- projected workflow;
- required capability plan;
- toolchain or lockfile when governed by policy;
- mandatory external evidence.

Stale evidence produces `UNKNOWN`, never `ALIVE`.

---

## 9. Failure Taxonomy

| Code | Meaning | Required response |
|---|---|---|
| `CI-ADM-001` | unsupported or malformed event provenance | refuse before execution |
| `CI-ADM-002` | source tree or merge-group identity mismatch | refuse and re-plan |
| `CI-PLAN-001` | changed path has no capability owner | refuse; add ownership fact |
| `CI-PLAN-002` | capability dependency cycle or unbounded fan-out | refuse; repair graph |
| `CI-PERM-001` | permission request exceeds declared ceiling | refuse before job start |
| `CI-ACTION-001` | mutable or unadmitted action reference | refuse projection or admission |
| `CI-EXEC-001` | required command failed | capability `REFUSED` |
| `CI-EVID-001` | required evidence missing | aggregate standing `UNKNOWN` |
| `CI-EVID-002` | evidence bound to another source or policy | aggregate standing `REFUSED` |
| `CI-SHELL-001` | zero-job, blocked, or policy-gated workflow shell | classify separately; never count as a test result |
| `CI-DRIFT-001` | generated workflow differs from canonical projection | refuse merge |
| `CI-STAND-001` | mandatory standing conjunct missing or refused | final standing `REFUSED` |
| `CI-RELEASE-001` | release lacks exact admitted standing receipt | refuse release intent |
| `CI-BRCE-001` | production actuation attempted outside BRCE | constitutional refusal |

Failure messages must name the violated law, exact evidence observed, and remediation. Generic `process completed with exit code 1` text is insufficient as the final user-facing diagnosis.

---

## 10. Throughput and Queue Requirements

### 10.1 Work-in-progress reduction

The control plane must reduce duplicate queued jobs before any runner expansion. The primary throughput metric is not raw parallelism. It is time from source-head publication to exact-head standing.

### 10.2 Cancellation

- superseded pull-request heads: cancel non-release work;
- merge-group heads: cancel only when GitHub invalidates the group;
- `main` verification: do not reuse a pull-request receipt when the merged tree differs;
- release actuation: non-cancelable after BRCE admission.

### 10.3 Build partitioning

Universal admission always runs. Capability-specific compilation and tests run from the impact graph. Full workspace and complete crowns run for:

- merge-group admission;
- `main`;
- release candidates;
- scheduled full-crown verification;
- explicit sabotage or architecture checkpoints.

### 10.4 Performance evidence

Record:

- queue wait;
- setup time;
- cache restore time;
- compile time;
- test time;
- evidence aggregation time;
- total standing latency;
- canceled superseded work;
- duplicate work avoided.

These measurements use QUDT-compatible units and do not alter correctness standing.

---

## 11. Security Architecture

1. Verification defaults to read-only repository access.
2. Every permission elevation is capability-local and justified by graph facts.
3. Third-party actions are pinned to full immutable SHAs.
4. PR execution receives no production secrets.
5. OIDC tokens are issued only after source and standing admission and only to the bounded release job.
6. Workflow-generated changes use a separate branch and normal PR, not a self-authorizing push to `main`.
7. Artifacts are hashed before aggregation.
8. Evidence from untrusted code is data, not authority.
9. The standing witness validates evidence schemas and provenance before interpretation.
10. Release and deployment intents cannot bypass BRCE through GitHub environment rules, ad hoc scripts, or manual dispatch.

---

## 12. Gall Checkpoint Roadmap

### G0 — Inventory and fence

**Purpose**: establish the exact current workflow graph before mutation.

**Artifacts**:

- refreshed as-built workflow inventory;
- workflow owner and event map;
- duplicate command map;
- branch-protection check inventory;
- queue and runtime baseline;
- `LegacyWorkflowFence` facts for every existing workflow.

**Acceptance**:

- every workflow has a named purpose, owner, trigger, permission ceiling, evidence output, and retirement condition;
- every unowned path or duplicate command is visible;
- no workflow is deleted.

**Refusal test**: inventory omits a workflow or assigns two owners to the same production output.

### G1 — Generated Rust inspection in shadow

**Purpose**: activate the existing reusable inspection workflow without changing branch protection.

**Artifacts**:

- generated caller in `.github/workflows/`;
- shared setup and evidence actions;
- equivalence report comparing handwritten and generated inspection.

**Acceptance**:

- both paths run on the same exact head;
- command vectors and results are equivalent or the stronger generated behavior is explicitly admitted;
- differing results name the first divergent command or evidence item.

**Refusal test**: the generated lane reports success while the handwritten lane fails the same required command.

### G2 — Event admission and impact plan

**Purpose**: introduce `ci-control-plane.yml` and deterministic path-to-capability planning.

**Artifacts**:

- control-plane workflow;
- generated impact-plan artifact;
- path ownership graph;
- stable `ci/admission` check.

**Acceptance**:

- docs-only, single-crate, cross-cutting, workflow, and unknown-path fixtures generate expected plans;
- superseded PR heads cancel;
- no production actuation exists.

**Refusal test**: an unknown changed path yields an empty plan or silently skips universal admission.

### G3 — External standing witness

**Purpose**: separate lane evidence from promotion authority.

**Artifacts**:

- reusable standing workflow;
- aggregate receipt schema;
- BLAKE3 evidence chain;
- stable `ci/standing` check;
- OCEL execution export.

**Acceptance**:

- missing, stale, malformed, canceled, and wrong-head evidence are refused;
- `ALIVE` requires every mandatory conjunct;
- the witness can run after failed dependencies through `if: always()`.

**Refusal test**: a capability lane edits its own evidence to claim `ALIVE` without the external witness.

### G4 — Capability-lane migration

**Purpose**: move existing repository-wide capability workflows behind `workflow_call` profiles.

**Migration order**:

1. architecture and autonomics;
2. GBB and combinatorial maximalism;
3. self-hosting and root dogfood;
4. Lean-to-Rust and RWR;
5. CertificationAssist and non-LLM self-play;
6. documentation and book verification;
7. remaining Rust and marketplace capabilities.

**Acceptance**:

- each migrated capability preserves its exact crown and sabotage tests;
- top-level trigger count declines after each migration;
- legacy workflow remains until shadow equivalence is receipted.

**Refusal test**: migration changes a capability's pass criteria merely to fit the shared executor.

### G5 — Generated authority and drift refusal

**Purpose**: make the ontology and ggen projection authoritative for CI configuration.

**Artifacts**:

- canonical CI/CD graph;
- generated control plane and reusable workflows;
- `cicd.toml` schema and admitted project instance;
- drift verifier;
- deterministic second-sync receipt.

**Acceptance**:

- `ggen sync --locked` produces the expected workflow set;
- a second sync is byte-identical;
- handwritten drift is refused;
- malformed graph and unsafe permission fixtures are refused before YAML publication.

**Refusal test**: direct YAML modification passes without a corresponding canonical graph change.

### G6 — Release and BRCE admission

**Purpose**: bind release and deployment to exact admitted standing.

**Artifacts**:

- release-admission workflow;
- standing receipt lookup and freshness verifier;
- BRCE intent and execution receipt adapters;
- negative fixtures for stale tag, wrong commit, absent receipt, and direct actuation.

**Acceptance**:

- no publication or deployment occurs without BRCE;
- release assets bind to the exact admitted source and build evidence;
- failed or partial publication is detected and receipted.

**Refusal test**: a manual dispatch with write permissions publishes from an `UNKNOWN` source tree.

### G7 — Retirement and branch-protection convergence

**Purpose**: delete proven duplication and freeze the stable public check contract.

**Artifacts**:

- retirement receipts for legacy workflows;
- updated branch-protection checks;
- final workflow ownership graph;
- queue and cost comparison;
- post-migration disaster-recovery procedure.

**Acceptance**:

- branch protection requires stable checks only;
- no retired workflow remains referenced by release, docs, CODEOWNERS, or operational procedures;
- rollback can restore the last admitted topology from canonical graph and receipt.

**Refusal test**: a legacy workflow is deleted before its replacement has an exact-head shadow equivalence receipt.

---

## 13. Test Strategy

### 13.1 Unit tests

- event classification;
- path ownership resolution;
- dependency closure;
- concurrency-group generation;
- permission derivation;
- cache-key derivation;
- evidence schema validation;
- standing conjunct calculation;
- refusal-code rendering.

### 13.2 Integration tests

Extend `crates/ggen-engine/tests/github_actions_pack_e2e.rs` to prove:

- canonical graph to YAML projection;
- immutable action references;
- explicit permissions;
- bounded matrices and timeouts;
- deterministic workflow ordering;
- generated reusable workflow calls;
- exact stable check names;
- drift refusal;
- byte-identical second sync.

### 13.3 End-to-end fixtures

Required fixture classes:

1. docs-only change;
2. one Rust crate;
3. cross-cutting engine change;
4. workflow or action change;
5. canonical graph change;
6. unknown path;
7. superseded PR head;
8. merge-group head;
9. workflow-authored commit;
10. zero-job or approval-blocked shell;
11. missing evidence artifact;
12. stale receipt;
13. wrong commit receipt;
14. mutable action reference;
15. excessive permission request;
16. failed capability lane;
17. canceled required lane;
18. release without BRCE;
19. partial publish;
20. sabotage attempt that forges success evidence.

### 13.4 Chicago TDD requirement

Tests must invoke the public workflow-generation and receipt-verification surfaces. They must not prove the architecture by asserting private implementation details alone.

### 13.5 No mocking of standing

Synthetic evidence may exercise parsers and refusals, but it cannot satisfy a production standing test. At least one real GitHub Actions execution must manufacture the accepted aggregate receipt for each promoted Gall checkpoint.

---

## 14. Success Metrics

### Structural

- one primary pull-request and `main` event ingress;
- no capability-wide duplicate event subscriptions after G4;
- 100% explicit workflow and job permission ceilings;
- 100% immutable third-party action references;
- 100% workflow ownership coverage;
- 100% generated-YAML drift coverage;
- one external aggregate standing authority.

### Throughput

- at least 50% reduction in top-level workflows associated with a typical Rust PR;
- at least 30% reduction in repeated toolchain/setup time across a typical PR;
- superseded heads canceled within one scheduler observation cycle;
- docs-only changes avoid full Rust workspace compilation;
- merge-group standing latency is measured and does not regress beyond the admitted baseline without an explicit exception receipt.

### Reliability

- zero unclassified `action_required` or zero-job shells counted as test failures or successes;
- zero releases from stale or absent standing receipts;
- zero branch-protection dependencies on ephemeral capability workflow names;
- every final refusal names the failed capability and missing evidence.

### Determinism

- byte-identical second projection;
- exact plan reproducibility for the same event and tree;
- exact receipt hash reproducibility from the same admitted evidence set;
- deterministic ordering of capability lanes and aggregate evidence.

---

## 15. Final Acceptance Criteria

The CI/CD refactor is `ALIVE` only when all of the following are true:

1. one control-plane workflow owns PR, merge-group, and `main` verification admission;
2. capability verification executes through reusable profiles rather than duplicate top-level triggers;
3. branch protection consumes stable checks and final aggregate standing;
4. the impact plan is deterministic, receipted, and fail-closed for unknown ownership;
5. all mandatory lanes emit exact-head evidence;
6. a separate witness verifies evidence and alone emits `ALIVE`;
7. zero-job and `action_required` shells are classified independently from executed test results;
8. superseded PR work is canceled while release actuation remains protected from cancellation;
9. workflow and action permissions are explicit and minimal;
10. all third-party actions are immutable;
11. YAML is generated from the canonical graph and drift is refused;
12. `ggen sync --locked` is byte-identical on the second run;
13. full-crown and sabotage tests pass on merge-group and `main` heads;
14. release and deployment require exact standing plus BRCE admission;
15. legacy workflows are deleted only after shadow equivalence receipts;
16. final queue, runtime, and duplicate-work metrics are published against the G0 baseline.

---

## 16. Implementation Order

The required next action is **G0 only**.

Do not begin by merging or deleting workflows. Refresh the as-built inventory against `main@faa52dac474d456ae00105869770161d666ba31f`, capture the current branch-protection contract, calculate duplicate command and trigger ownership, and manufacture the first inventory receipt.

The first implementation PR after this document should contain:

- updated GitHub Actions pack inventory facts;
- a generated workflow ownership report;
- a trigger and command duplication report;
- baseline queue/runtime observations;
- refusal fixtures for missing owner and duplicate production-output authority;
- no workflow deletion;
- no release behavior change;
- no new Rust crate.

That checkpoint creates the lawful fence from which the reusable-workflow migration can proceed.