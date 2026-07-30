# fortune5-architecture-pack

`fortune5-architecture-pack` is the graph-native constitutional layer for Fortune 5 enterprise architecture. A consumer declares accountable business capabilities, architecture assets, dependencies, regions, lifecycle, reliability, security, observability, supervision, workflow-pattern evidence, improvement law, and workflow-engine interoperability as RDF. ggen validates the model and manufactures reviewable catalog, dependency, control, and evidence projections.

## Architecture boundary

The pack does not deploy infrastructure and does not embed a second workflow engine. It defines what must be true before downstream systems receive standing.

```text
enterprise ontology
→ named SPARQL refusals
→ deterministic projections
→ ggen receipt
→ bounded architecture intents
→ BRCE
→ downstream execution
```

Autonomic policy must declare `f5:broker "BRCE"` and `f5:directActuation false`. Shell commands, HTTP endpoints, Kubernetes mutations, and cloud API calls are forbidden inside admitted architecture law.

## Required Fortune 5 controls

Every Tier 0 or Tier 1 asset must declare:

- measurable SLOs and sample floors;
- an explicit capacity envelope and reserve ratio;
- multi-region replication with RPO, RTO, and quorum policy;
- jurisdiction constraints for data residency;
- SPIFFE/SPIRE workload identity, mTLS, attestation, and bounded SVID TTL;
- KMS-backed envelope encryption, rotation, decrypt audit, and critical-tier HSM dual control;
- logs, metrics, traces, correlation IDs, redaction, retention, and OTLP routing;
- supervised distributed execution with restart intensity and telemetry policy;
- an evidence-bearing lifecycle promotion gate.

## Workflow capability census

The pack declares `WCP01` through `WCP43`, the revised Workflow Patterns Initiative control-flow catalogue. Every pattern must have four separate evidence references:

```text
implementation evidence
positive witness
negative falsifier
receipt verifier
```

The evidence references are contracts, not self-certifying claims. The downstream engine—such as POWL/wasm4pm—must make those references executable and receipted.

## Enterprise improvement and interoperability

A complete program also requires:

- a DFLSS program with Define, Measure, Analyze, Design, and Verify evidence;
- a receipted KNHK adapter contract with graph input and receipt output classes;
- deterministic Hot/Warm/Cold path policy;
- broker-only MAPE-K intent generation.

## Generated artifacts

| Artifact | Purpose |
|---|---|
| `docs/FORTUNE5_ARCHITECTURE_CATALOG.md` | Accountable assets and capabilities |
| `docs/FORTUNE5_ARCHITECTURE_DAG.dot` | Dependency and impact graph |
| `docs/FORTUNE5_CONTROL_MATRIX.md` | Critical control coverage and autonomic authority |
| `docs/FORTUNE5_WORKFLOW_PATTERN_COVERAGE.md` | WCP01–WCP43 evidence census |

## Named refusals

The gates reject incomplete base contracts, missing Tier 0/1 controls, weak identity or KMS policy, unsafe replication and capacity, promotion bypasses, direct autonomic actuation, incomplete workflow-pattern evidence, missing supervision, incomplete DFLSS, and unreceipted KNHK integration.

## Example

`examples/fortune5-architecture` is a complete Tier 0 consumer fixture. The dedicated GitHub Actions workflow builds the real ggen compiler, manufactures the fixture twice, verifies receipts, proves byte idempotence, independently parses Turtle and SPARQL, and executes named sabotage cases.
