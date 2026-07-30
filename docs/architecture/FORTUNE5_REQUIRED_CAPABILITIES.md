# Fortune 5 Required Capabilities

This document records the implemented boundary between the canonical Fortune 5 assessor, the pure runtime kernel, ggen packs, Gall, BRCE, deployment building blocks, and downstream workflow/process execution.

## Constitutional split

| Layer | Authority |
|---|---|
| `tools/ggen-architecture` | Canonical conjunctive Level-5 assessor, architecture registry, capacity/doctor machinery, MAPE-K planning, exact crown, CLI, and receipts |
| `ggen-fortune5-kernel` | Pure deterministic runtime policy for lifecycle, dependencies, SLOs, capacity, regions, identity, KMS, observability, bounded intents, and BLAKE3 receipts |
| `fortune5-architecture-pack` | RDF vocabulary, named SPARQL refusals, enterprise capability census, control closure, WCP01–WCP43 evidence requirements, and review projections |
| `fortune5-deployment-blocks-pack` | RDF authority for AWS, Azure, and GCP deployment groups consumed by `ggen bblock <verb>` |
| Gall | Dependency-closed implementation work, isolated agent execution, evidence collection, replay, and crown governance |
| BRCE | Sole admitted conversion of bounded intent into actuation |
| POWL/wasm4pm | Executable process semantics and evidence for declared workflow patterns |
| KNHK adapter | Receipted graph-to-workflow-engine interoperability boundary |

No architecture or building-block layer invokes cloud infrastructure directly.

## Exact Level-5 profile

The canonical profile is conjunctive:

```text
21 dimensions
99 machine-visible controls
63 proof obligations
= 21 Design
+ 21 Operation
+ 21 Falsifier
```

A dimension is `ALIVE` only when all three proof kinds are independently admitted. Evidence packages require stable identity, artifacts, digest, standing, and distinct producer, approver, and verifier authorities.

Synthetic evidence may prove the machinery and structural closure. It can never make `promotion_ready` true.

## Exact crown above 21/99/63

The `fortune5 crown` command adds four independent crown surfaces.

### Six release truths

1. Deterministic execution
2. Performance guarantees
3. Cryptographic receipts
4. Infinity Generation
5. Fortune 5 integration
6. Dark Matter/Energy elimination

### Five SLA governors

1. SLO Tracking, including the R1/W1/C1 latency classes
2. Promotion Gates
3. Multi-Region
4. SPIFFE/SPIRE
5. KMS Integration

### Six operational ingress controls

- `max_run_len`, bounded by the Chatman Constant at 8;
- `budget_cap`, defaulting to 2,000,000,000;
- `rate_limit`, represented canonically as 50,000 parts per million;
- `chronology`;
- `conservation`, with a 1,000-parts-per-million tolerance;
- `legality`, including hard exclusion regions.

### Crown invariants

- exact taxonomy/profile cardinality;
- all six truths independently alive;
- all five SLA governors independently alive;
- all six ingress controls valid;
- receipt replay independently verified;
- no direct architecture actuation;
- synthetic structure is never promoted.

The crown emits its own deterministic receipt bound to the underlying 21/99/63 assessment receipt.

## Enterprise architecture capabilities

### Architecture and dependency law

- accountable assets and enterprise capabilities;
- legal lifecycle transitions and evidence-bearing promotion gates;
- acyclic dependency closure and deterministic topological ordering;
- transitive impact analysis across assets, capabilities, and regions;
- canonical BLAKE3 snapshot and output digests;
- predecessor-linked receipt ledgers.

### Reliability and scale

- measurable SLI/SLO definitions;
- minimum-sample refusal and error-budget standing;
- capacity envelopes with minimum, maximum, current, unit capacity, observed load, and reserve ratio;
- scale-out and admission-control intent manufacture;
- multi-region replication, read/write quorum, RPO, RTO, and jurisdiction constraints;
- regional-loss failover intent manufacture;
- deterministic Hot, Warm, and Cold path classification.

### Security and governance

- SPIFFE trust domains and workload identity allowlists;
- mTLS and workload-attestation requirements;
- bounded SVID lifetime and renewal intent manufacture;
- KMS provider, key alias, rotation, HSM backing, envelope encryption, decrypt audit, and dual-control break glass;
- evidence-bearing promotion gates requiring approvals, SLO standing, replay, and security controls;
- direct-actuation refusal.

### Observability and autonomics

- logs, metrics, traces, profiles, correlation IDs, redaction, retention, and OTLP routing;
- stale-telemetry intent manufacture;
- MAPE-K policy evaluation;
- bounded intents addressed to BRCE only;
- no network, filesystem, process, cloud, or Kubernetes actuation in the kernel.

### Cold-path operation

- supervised distributed execution contracts;
- restart strategy and intensity windows;
- child specification census;
- distribution and telemetry requirements;
- full SPARQL routing for complex or large graph execution.

### Process and improvement closure

- the revised 43 control-flow workflow-pattern census;
- separate implementation, positive witness, negative falsifier, and receipt-verifier references for every pattern;
- complete DFLSS Define, Measure, Analyze, Design, and Verify evidence contract;
- receipted KNHK graph-input/receipt-output adapter contract.

## Globally available deployment building blocks

`ggen bblock <verb>` is a generic compiler over an ontology-derived retained catalog. It supports:

```bash
ggen bblock providers
ggen bblock list
ggen bblock inspect <group> <provider>
ggen bblock group <group> <provider>
ggen bblock plan <group> <provider>
ggen bblock enable <group> <provider>
ggen bblock validate
```

Providers are exactly:

- `aws`;
- `azure`;
- `gcp`, with the compatibility alias `gpc`.

The catalog provides 14 atomic groups and four composite groups:

```text
fortune5-foundation
fortune5-platform
fortune5-control-plane
fortune5-complete
```

Atomic coverage includes global networking, workload identity, KMS, observability, managed containers, serverless compute, transactional data, object storage, event fabric, artifact registry, policy/governance, resilience/DR, edge delivery, and evidence ledgers.

Resolution is transitive, dependency-first, deterministic, and duplicate-free. `plan` and `enable` create a fixed local control surface:

```text
.ggen/bblocks/
├── groups/<group>.json
├── plans/<provider>/<group>.json
└── receipts/<provider>/...
```

`enable` creates ontology-declared `infrastructure/` directories and adds catalog-bound pack identities to `.ggen/packs.lock`. It never invokes Terraform, Pulumi, Kubernetes, a cloud SDK, or a provider API.

## What this does not falsely claim

The architecture pack does not prove a downstream workflow engine merely by naming an evidence path. It proves that the required evidence universe is complete and structurally admitted. Actual POWL/wasm4pm executability, negative fixtures, and receipt verification remain downstream proof obligations and must be bound into the Gall crown.

SPIFFE, KMS, cloud-region, OTLP, KNHK, and provider building-block values are typed contracts in this layer. Live credentials and external endpoint execution belong to separately admitted adapters behind BRCE.

## Crown equation

```text
Fortune 5 standing
= 21 dimensions
= 99 controls
= 63 Design/Operation/Falsifier proofs
= six release truths alive
= five SLA governors alive
= six operational controls valid
= every critical architecture control declared
= every promotion gate closed
= WCP01–WCP43 evidence complete
= DFLSS evidence complete
= KNHK adapter receipted
= deployment catalog RDF/JSON equivalent
= autonomic intents broker-only
= deterministic replay green
= synthetic promotion impossible
```
