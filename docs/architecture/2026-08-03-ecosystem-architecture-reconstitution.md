# Ecosystem Architecture Reconstitution

**Date:** 2026-08-03  
**Repository:** `seanchatmangpt/ggen`  
**Admitted base:** `a9fce3c1db64d3e6dff72f61e5dabf4d0af45e73`  
**Change class:** architecture documentation only  
**Code impact:** none

## 1. Decision

Adopt a capability-first ecosystem architecture in which `ggen` is the semantic authority and deterministic manufacturing kernel, `ggen-legacy` is the executable corpus and independent verification court, domain repositories are governed capability realizations, Ferroplan is the planning and operator-experience plane, wasm4pm is the process-evidence plane, and BRCE is the exclusive consequential actuation boundary.

The architecture is governed by:

```text
A = μ(O*)
R = receipt(A)
```

`O*` is admitted, aligned, grounded, and bounded observation. `μ` is lawful manufacture. `R` binds subject identity, authority, consequence, replay, and standing.

This decision does not assert that the entire cross-repository ecosystem is `ALIVE`. It defines the target composition and the evidence needed to promote bounded subjects from `UNKNOWN` or `PARTIAL_ALIVE` to `ALIVE`.

## 2. Observed architectural phase change

The reviewed work converges on a common execution law:

```text
exact subject
→ bounded authority
→ executable witness
→ linked receipt
→ deterministic replay
→ adversarial refusal
→ scoped standing
```

Repository success is no longer the primary unit of architecture. The primary unit is a canonical capability with one or more repository realizations, each linked to authority, witnesses, falsifiers, receipts, replay, and a bounded standing claim.

## 3. Architecture principles

### 3.1 Capability identity outranks repository identity

A repository is an ownership and realization coordinate. A capability is the stable architectural identity.

```text
Capability
→ requirement
→ authority
→ realization
→ witness or falsifier
→ receipt
→ replay
→ standing
```

This permits multiple implementations without conflating them and supports explicit substitution, migration, compatibility, and retirement laws.

### 3.2 SELECT, CONSTRUCT, and DO remain separate

- **SELECT** chooses an admitted subject, route, or candidate.
- **CONSTRUCT** creates reversible graphs, plans, projections, intents, and artifacts.
- **DO** changes machine or external state through an authorized boundary.

Raw input, model output, proof text, planner output, semantic derivation, and hooks have no ambient actuation authority. Hooks manufacture intents. BRCE alone performs consequential actuation where a BRCE boundary exists.

### 3.3 Receipts bind operation identity, not merely output hashes

The ecosystem receipt edge should bind:

```text
subject
+ authority
+ plan
+ resolved arguments
+ grant
+ result
+ observed consequence
+ parent topology
→ receipt edge
```

A result hash without operation, authority, and consequence identity is insufficient for cross-repository standing.

### 3.4 Local capsules are first-class architecture

The preferred executable boundary is:

```text
Source Capsule
× Validation Pack
× Execution Mode
× Toolchain Capsule
→ Receipt DAG
```

Hosted CI supplements this proof. It does not replace exact-subject local execution when local execution is available.

### 3.5 Standing is multidimensional

Each realization may have independent standings for source authority, local runtime, exact-head runtime, external dependencies, device execution, production execution, and aggregate composition.

Use only:

- `UNKNOWN`
- `PARTIAL_ALIVE`
- `ALIVE`
- `BLOCKED`
- `BUILD_BROKEN`
- `UNSUPPORTED`
- typed `REFUSED_*`

Do not average these dimensions into a misleading repository-wide status.

## 4. Target architecture

The target architecture has nine planes.

1. **Constitutional authority** — equations, ownership, claim ceilings, standing vocabulary, and BRCE law.
2. **Ecosystem capability graph** — canonical capability identities, ABB requirements, SBB realizations, dependencies, and substitution rules.
3. **Observation and admission** — exact source identity, RDF, SHACL, provenance, contradiction handling, and admitted `O*`.
4. **Construction and planning** — CMD, Ferroplan, linear, temporal, FOND, probabilistic, and persistent-mind planning.
5. **Manufacture** — graph/query/template/ggen projections and deterministic second manufacture.
6. **Brokered runtime** — BRCE grants and filesystem, process, network, service, or device consequences.
7. **Evidence and process intelligence** — receipts, OCEL, wasm4pm, replay, conformance, checkpoints, and resumability.
8. **Independent verification** — ggen-legacy, Truthforge, clean-room replay, mutation portfolios, and refusal verification.
9. **Product and experience projections** — CLI, MCP, A2A, LSP, browser, web, television, AtomVM, and domain products.

## 5. Stable ecosystem interfaces

Four interfaces are repeated often enough to become shared contracts.

### Doctor

Observes exact subjects and produces bounded findings. Doctor has no actuation authority.

```text
DoctorObservation
→ DoctorFinding
→ standing candidate or RepairIntent
```

### Wizard

Constructs bounded, reversible plans from admitted requirements and findings.

```text
WizardRequest
→ WizardPlan
→ BRCEIntent
```

### Telco

Carries typed identities, envelopes, grants, records, and correlation across repository boundaries.

```text
TelcoEnvelope
= subject + authority + payload + correlation + receipt parent
```

### Truthforge

Independently evaluates evidence, mutation controls, replay, and claim ceilings.

```text
TruthforgeVerdict
= admitted evidence + falsifier outcomes + standing
```

These interfaces should share ontology and wire identity while allowing repository-native implementations.

## 6. Cross-repository conflicts

### 6.1 Competing capability catalogs

Capability catalogs exist in ggen Vision 2030, ggen SBB, ggen-legacy, CNS, Ferroplan, DTeam, KNHK, MCPP, GitVan, ByteStar, and other domain repositories. Their structures overlap, but identity and realization rules are not canonical.

**Disposition:** `PARTIAL_ALIVE` until one canonical capability registry and realization protocol are manufactured and replayed.

### 6.2 Repeated Doctor, Wizard, Telco, and Truthforge implementations

The doctrine is converging, but schemas and authority ceilings differ.

**Disposition:** `PARTIAL_ALIVE`. Extract shared contracts, not shared implementation bodies.

### 6.3 Toolchain transport

Rust toolchain absence or transport failure repeatedly prevents exact-subject execution.

**Disposition:** `BLOCKED` for a universal offline Rust Toolchain Capsule.

### 6.4 Local ALIVE does not imply ecosystem ALIVE

Several bounded products have strong local crowns, but no complete receipt DAG proves composition through one capability graph, planning plane, BRCE consequence, process evidence rail, and independent verifier.

**Disposition:** aggregate cross-repository crown remains `UNKNOWN`.

### 6.5 Generated-surface ownership drift

Some products preserve graph → query → template → generated projection. Others duplicate catalogs or hand-author projections that should be generated.

**Disposition:** `PARTIAL_ALIVE`; introduce explicit generated-surface ownership and second-manufacture replay.

## 7. Required ggen building blocks

### Constitution

- `ExactSubject`
- `AuthorityAssertion`
- `ClaimCeiling`
- `StandingClaim`
- `TypedRefusal`
- `ExecutionGrant`
- `ReceiptEdge`
- `ReplayIdentity`

### Capability architecture

- `CanonicalCapability`
- `RepositoryRealization`
- `CapabilityPassport`
- `ABBRequirement`
- `SBBRealization`
- `CapabilityDependency`
- `CapabilityDisposition`
- `ExternalBoundary`

### Operator experience

- `DoctorObservation`
- `DoctorFinding`
- `RepairIntent`
- `WizardRequest`
- `WizardPlan`
- `TelcoEnvelope`
- `TruthforgeVerdict`
- `QoLAtomicBatch`

### Planning and evidence

- `LinearPlan`
- `TemporalPlan`
- `FondPolicy`
- `ProbabilisticPolicy`
- `PersistentMind`
- `Checkpoint`
- `ParetoFrontier`
- `BoundedCapabilityLattice`
- `ObjectCentricEvent`
- `IndependentVerifierReport`
- `MutationPortfolio`
- `ToolchainCapsule`

### Manufacture and actuation

- `OntologyQueryTemplateRule`
- `GeneratedSurfaceOwner`
- `WritePlan`
- `ProjectionReceipt`
- `SecondManufactureReplay`
- `PackCompatibilityContract`
- `BRCEIntent`
- `BRCEGrant`
- `ActuationResult`
- `ObservedConsequence`

## 8. Required packs

- `ecosystem-capability-authority-pack`
- `capability-passport-pack`
- `doctor-pack`
- `wizard-pack`
- `telco-envelope-pack`
- `truthforge-verifier-pack`
- `brce-actuation-pack`
- `persistent-mind-pack`
- `planner-algebra-pack`
- `receipt-dag-pack`
- `ocel-process-evidence-pack`
- `toolchain-capsule-pack`
- `polyglot-local-crown-pack`
- `browser-runtime-proof-pack`
- `physical-device-evidence-pack`
- `generated-surface-ownership-pack`
- `cross-repository-reference-product-pack`

## 9. Required templates

```text
canonical-capability.ttl.tera
repository-realization.ttl.tera
capability-passport.json.tera
standing-ledger.json.tera
doctor-report.json.tera
repair-intent.json.tera
wizard-plan.json.tera
telco-envelope.json.tera
truthforge-report.json.tera
brce-intent.json.tera
brce-grant.json.tera
result-receipt.json.tera
consequence-receipt.json.tera
receipt-dag.json.tera
replay-report.json.tera
toolchain-capsule-manifest.json.tera
planner-carrier.json.tera
product-receiver.json.tera
daily-architecture-reconstitution.md.tera
```

## 10. Architecture acceptance criteria

The first ecosystem crown is admitted only when one bounded reference product demonstrates:

```text
canonical capability identity
→ admitted requirement
→ plan construction
→ deterministic manufacture
→ BRCE-authorized consequence
→ receipt DAG
→ OCEL or equivalent process evidence
→ independent replay
→ mutation refusal
→ scoped standing
```

The crown must also prove:

- no self-certification cycle;
- no direct actuation outside BRCE;
- exact source and toolchain identity;
- explicit external boundaries;
- byte-stable or semantically stable replay as specified;
- typed treatment of blocked and unsupported edges;
- no promotion from checkpoint to aggregate standing.

## 11. Explicit exclusions

This architecture document does not:

- change runtime code, manifests, workflows, schemas, generated artifacts, or release configuration;
- declare the complete ecosystem `ALIVE`;
- declare production, physical-device, or external-service standing;
- merge duplicate implementations before equivalence and substitution laws exist;
- make ggen-legacy an actuation authority;
- grant Doctor, Wizard, Telco, Truthforge, hooks, planners, or generated artifacts ambient execution authority.

## 12. Consequence

The immediate architecture objective is not more catalog breadth. It is one independently replayable cross-repository reference product whose receipt DAG proves the complete law from canonical capability identity through consequence and independent standing.