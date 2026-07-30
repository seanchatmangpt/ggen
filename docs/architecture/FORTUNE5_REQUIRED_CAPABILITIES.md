# Fortune 5 Required Capabilities

This document records the implemented boundary between `ggen-architecture`, `fortune5-architecture-pack`, Gall, BRCE, and downstream workflow/process execution.

## Constitutional split

| Layer | Authority |
|---|---|
| `ggen-architecture` | Pure deterministic evaluation of architecture assets, dependencies, lifecycle, SLOs, capacity, regions, security controls, promotion, impact, autonomic intents, and receipts |
| `fortune5-architecture-pack` | RDF vocabulary, named SPARQL refusals, enterprise capability census, control closure, and generated review projections |
| Gall | Dependency-closed implementation work, agent execution, evidence collection, replay, and crown governance |
| BRCE | Sole admitted conversion of bounded intent into actuation |
| POWL/wasm4pm | Executable process semantics and evidence for declared workflow patterns |
| KNHK adapter | Receipted graph-to-workflow-engine interoperability boundary |

No architecture layer performs direct deployment or infrastructure mutation.

## Implemented capabilities

### Enterprise architecture kernel

- accountable assets and enterprise capabilities;
- lifecycle transition law;
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

## What this does not falsely claim

The architecture pack does not prove a downstream workflow engine merely by naming an evidence path. It proves that the required evidence universe is complete and structurally admitted. Actual POWL/wasm4pm executability, negative fixtures, and receipt verification remain downstream proof obligations and must be bound into the Gall crown.

Similarly, SPIFFE, KMS, cloud-region, OTLP, and KNHK values are typed contracts in this layer. Live provider credentials and external endpoint execution belong to separately admitted adapters behind BRCE.

## Crown equation

```text
Fortune 5 standing
= architecture model valid
= every critical control declared
= every promotion gate closed
= WCP01–WCP43 evidence complete
= DFLSS evidence complete
= KNHK adapter receipted
= autonomic intents broker-only
= deterministic replay green
```
