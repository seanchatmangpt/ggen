# ggen Enterprise Architecture Machinery, Automation, and Autonomics

## Standing

This implementation establishes the first executable slice of **Enterprise
Architecture as Strategy with ggen**. It converts the book's architecture model
into a governed runtime kernel without collapsing planning into actuation.

## Machinery

`tools/ggen-architecture` provides six bounded mechanisms.

### Architecture registry

Every architecture object receives a stable identifier, type, owner, version,
lifecycle state, evidentiary standing, dependencies, replacement relations, and
extension attributes. Registered objects include capabilities, products,
services, repositories, components, ontologies, packs, projections,
requirements, policies, transition architectures, work packages, evidence, and
receipts.

### Lifecycle state machine

The lawful lifecycle is encoded directly:

```text
discovered -> identified -> qualified -> admitted -> active
active -> deprecated -> retired -> archived
```

Explicit early retirement and deprecated reactivation paths exist. Skipped or
backward transitions are typed refusals.

### Dependency and impact planning

The registry computes:

- dependency closure;
- dangling-dependency refusals;
- dependency-cycle refusals;
- transitive reverse impact;
- shortest discovered impact paths;
- dependency-safe revalidation order;
- dependency-safe lifecycle transition steps.

This makes a change to an ontology, pack, component, or capability calculable
before promotion.

### Capacity envelopes

Capacity is modeled as a workload vector rather than a file-count threshold:

```text
documents x quads x blank-nodes x rules x shapes x templates x projections
```

Each sample records end-to-end milliseconds, peak memory, and optional phase
timings. Policy classifies samples as healthy, warning, or refusal. The analyzer
records first warning, first refusal, and the first observed nonlinear adjacent-
slope knee. Prediction is limited to extrapolation from the final observed
segment; it never invents an unobserved breaking point.

### Architecture doctor

`ggen-architecture doctor` checks:

- registry key and embedded identity agreement;
- dependency closure and cycles;
- active use of retired dependencies;
- ownership of active assets;
- version identity for active ontologies and packs;
- successor coverage for deprecated assets;
- evidentiary standing;
- capacity observations and policy crossings;
- the constitutional zero-direct-actuation rule.

The report is deterministic and BLAKE3-receipted.

### Deterministic receipts

Intent, doctor, and autonomic-cycle receipts hash a schema identifier, receipt
kind, and ordered payload. Public models use ordered maps and sets, making
identical admitted inputs produce identical hashes.

## Automation

`architecture/run-autonomics.sh` and the Architecture Autonomics GitHub Actions
workflow perform the repeatable machinery:

1. validate registry closure;
2. generate the doctor report;
3. calculate the capacity envelope;
4. calculate ontology impact;
5. run a bounded synthetic autonomic cycle;
6. preserve JSON receipts and SHA-256 inventory as workflow artifacts.

The workflow runs on architecture-related pull requests, manually, and on a
weekly schedule. The scheduled run detects source-level governance regressions;
it is not a substitute for importing observed production stimuli.

## Autonomics

The controller implements a MAPE-K interpretation:

```text
Monitor -> Analyze -> Plan -> ArchitectureIntent
```

The Execute phase is intentionally absent. Stimuli may include:

- measured capacity samples;
- architecture asset changes;
- digest drift;
- dependency unavailability;
- lifecycle deadlines;
- standing changes.

Diagnoses may produce bounded intents such as:

- warn;
- reprofile;
- revalidate;
- block promotion;
- rebuild a projection;
- create a migration plan;
- recalculate a transition plan;
- submit an otherwise complete request to a broker.

Every intent declares:

- primary and affected assets;
- preconditions;
- required capabilities;
- expected evidence;
- deterministic payload;
- BLAKE3 identity.

## Constitutional fence

The autonomic policy field `direct_actuation_allowed` must remain `false`.
Setting it to `true` causes both a doctor refusal and a controller-level typed
refusal. No filesystem, process, network, deployment, or external API actuation
surface exists in the crate.

The lawful continuation is:

```text
stimulus
-> diagnosis
-> architecture intent
-> independent admission
-> BRCE execution grant
-> actuator
-> observed result
-> evidence admission
-> receipt
```

## Current canonical state

`architecture/ggen-enterprise.json` registers the first ecosystem graph:

- lawful manufacturing;
- enterprise architecture ontology;
- federated architecture registry;
- ggen architecture kernel;
- Graphlaw;
- star-toml;
- ggen engine;
- MFW/POWL planning;
- Gall standing;
- BRCE actuation;
- wasm4pm evidence;
- mfact/procint assurance;
- cargo-cicd release governance;
- the ggen platform product.

This graph is deliberately a first admitted baseline, not a claim that every
cross-repository integration is complete. The platform product therefore has
`PARTIAL_ALIVE` standing.

## Gall checkpoints

### EA-MACH-001 — Registry and lifecycle

- positive witness: closed canonical architecture graph;
- negative fixture: dependency cycle and skipped lifecycle transition;
- receipt: registry validation JSON.

### EA-MACH-002 — Impact and transition planning

- positive witness: ontology change reaches all transitive dependents in
  dependency-safe order;
- negative fixture: missing dependency refusal;
- receipt: ontology impact JSON.

### EA-MACH-003 — Capacity governance

- positive witness: warning, refusal, and knee detection tests;
- negative fixture: hard-cap crossing;
- receipt: capacity envelope JSON.

### EA-AUTO-001 — Architecture doctor

- positive witness: canonical state diagnosis;
- negative fixture: direct-actuation policy refusal;
- receipt: doctor JSON with BLAKE3 hash.

### EA-AUTO-002 — Autonomic intent generation

- positive witness: asset change, capacity warning, and drift produce bounded
  intents;
- negative fixture: direct actuation is unreachable;
- receipt: autonomic cycle JSON with `actuation_performed=false`.

## Next architecture transitions

1. Project the JSON registry from the enterprise architecture ontology rather
   than maintaining both surfaces independently.
2. Admit real ontology stress receipts, including environment fingerprints and
   peak-memory evidence.
3. Integrate `architecture doctor`, `impact`, and `cycle` into the generated
   clap-noun-verb CLI surface.
4. Define the BRCE-side `ArchitectureIntent` admission contract without adding
   an execution dependency to this kernel.
5. Import repository, pack, ontology, Gall, and receipt identities from their
   authoritative registries.
6. Project ArchiMate views, migration work packages, dashboards, and policy
   gates from the canonical graph.
7. Add OCEL events for every autonomic cycle and downstream broker decision.

The implementation is therefore not a generic agent framework. It is a
bounded architecture control kernel whose outputs are inspectable, deterministic,
and incapable of bypassing the actuation constitution.
