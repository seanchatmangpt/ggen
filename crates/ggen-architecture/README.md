# ggen-architecture

`ggen-architecture` is the pure, deterministic enterprise-architecture kernel for Fortune 5 operation.

It treats architecture as executable law rather than a diagram repository. The kernel admits architecture assets, capabilities, dependencies, regions, SLOs, capacity envelopes, promotion gates, replication policy, workload identity, KMS controls, and observability policy. From those facts it derives dependency closure, topological order, impact reports, promotion decisions, capacity standing, deterministic path selection, bounded autonomic intents, and cryptographic receipts.

## Hard boundary

The crate is IO-free and contains no deployment executor, shell runner, network client, cloud SDK, Kubernetes client, or infrastructure mutation surface.

Autonomics implement the MAPE-K decision boundary only:

```text
observations
→ evaluate admitted architecture law
→ manufacture bounded ArchitectureIntent
→ address intent to BRCE
```

Only BRCE or another admitted broker may convert an intent into actuation. The crate never performs direct actuation.

## Fortune 5 capability closure

The kernel implements:

- accountable architecture assets and enterprise capabilities;
- legal lifecycle transitions and evidence-bearing promotion gates;
- acyclic dependency closure and deterministic topological ordering;
- transitive impact analysis across assets, capabilities, and regions;
- SLI/SLO standing, minimum-sample refusal, and error-budget escalation;
- capacity envelopes, reserve policy, scale-out intent, and admission control;
- deterministic Hot/Warm/Cold path selection:
  - Hot: at most 8 triples, no joins, simple predicates;
  - Warm: at most 1,000 triples, at most 4 bounded joins, simple predicates;
  - Cold: larger or complex graph requiring full SPARQL execution;
- multi-region topology, RPO/RTO, data residency, and quorum safety;
- SPIFFE trust domains, SPIFFE ID allowlists, mTLS, attestation, and bounded SVID TTL;
- KMS provider, key aliases, rotation, HSM backing, envelope encryption, decrypt audit, and dual-control break glass;
- logs, metrics, traces, correlation IDs, redaction, retention, and OTLP routing;
- deterministic broker-only MAPE-K intents for SLO, capacity, region, key, identity, and telemetry conditions;
- canonical BLAKE3 snapshot digests and predecessor-linked receipt ledgers.

## Standing rule

A Tier 0 or Tier 1 asset is invalid unless it has all of the following:

```text
capacity envelope
multi-region replication
SPIFFE/SPIRE identity policy
KMS policy
comprehensive observability
at least one measurable SLO
```

No policy is satisfied by prose. The model must carry the typed facts, and `ArchitectureModel::validate` must return zero violations.

## Verification

```bash
cargo test -p ggen-architecture
cargo test -p ggen-architecture --doc
```

The unit suite includes positive witnesses and negative falsifiers for lifecycle skips, dependency cycles, missing critical controls, promotion bypasses, capacity exhaustion, regional quorum loss, SLO breach, key rotation, identity renewal, receipt determinism, and receipt tampering.
