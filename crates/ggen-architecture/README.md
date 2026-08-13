# ggen-architecture

`ggen-architecture` is the pure, deterministic enterprise-architecture kernel for Fortune 5 operation.

It treats architecture as executable law rather than a diagram repository. The canonical graph spans enterprise scope, motivation, strategy, business, information, application, technology, implementation/migration, governance, and evidence. The existing Fortune 5 profile then applies operational controls for assets, regions, SLOs, capacity, identity, KMS, observability, bounded intents, and receipts.

## Hard boundary

The crate is IO-free and contains no deployment executor, shell runner, network client, cloud SDK, Kubernetes client, or infrastructure mutation surface.

Architecture analysis and autonomics stop at construction:

```text
observations / admitted graph facts
→ validate architecture law
→ derive views, impact, governance, and transition order
→ manufacture bounded intent or static architecture receipt
→ BRCE boundary
```

Only BRCE or another admitted broker may convert an intent into actuation. The crate never performs direct actuation. Static graph completeness never upgrades represented workloads to `ALIVE`; execution standing remains an independent evidence dimension.

## Canonical enterprise graph

`enterprise.rs` adds the governed enterprise layer above the operational profile:

- enterprise and architecture-boundary identity;
- principles, requirements, standards, capabilities, value streams, products, and services;
- organizations, actors, roles, processes, owners, and stewards;
- information concepts, datasets, and ontologies;
- applications, components, interfaces, technology, platforms, environments, repositories, packs, queries, templates, and projections;
- plans, dependency-closed work packages, transition architectures, and migrations;
- decisions, claims, evidence, receipts, metrics, risks, controls, and exceptions;
- 24 typed directional relationship kinds for realization, implementation, dependency, deployment, evidence, governance, mitigation, migration, and impact;
- deterministic requirement-to-evidence traceability;
- bidirectional transitive architecture impact closure;
- capability-to-realization portfolio matrices;
- disposable Motivation, Strategy, Business, Information, Application, Technology, Implementation/Migration, Governance, Evidence, and Full viewpoints;
- deterministic transition ordering with cycle and unknown-dependency refusals;
- static governance findings for missing ownership, realization, satisfaction, mitigation, observability, and evidence;
- BLAKE3 model identity and predecessor-linked static architecture receipts.

Public ontology terms are semantic alignments, not authority. The kernel exposes explicit identifiers for PROV-O, W3C ORG, SKOS, DCAT, ODRL, and SOSA and never dereferences them or grants execution rights because a fact carries one of those types.

The sibling `.specify/specs/togaf/` pack projects the same enterprise semantics into RDF/TOGAF/ArchiMate space. Generated diagrams and documents remain projections; the canonical graph is the editing authority.

## Enterprise CLI

The `ggen-architecture-cli` workspace member exposes a separate analysis-only binary:

```bash
cargo run --manifest-path crates/ggen-architecture/Cargo.toml \
  -p ggen-architecture-cli --bin ggen-ea -- \
  validate --model crates/ggen-architecture/examples/enterprise-model.json

cargo run --manifest-path crates/ggen-architecture/Cargo.toml \
  -p ggen-architecture-cli --bin ggen-ea -- \
  trace --model crates/ggen-architecture/examples/enterprise-model.json \
  --subject req:residency --max-depth 8

cargo run --manifest-path crates/ggen-architecture/Cargo.toml \
  -p ggen-architecture-cli --bin ggen-ea -- \
  transition-order --model crates/ggen-architecture/examples/enterprise-model.json \
  --transition transition:modernize-data
```

Available commands are `validate`, `governance`, `impact`, `trace`, `portfolio`, `view`, `transition-order`, and `receipt`. They perform deterministic local SELECT/CONSTRUCT-style analysis only.

## Fortune 5 operational capability closure

The operational profile implements:

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

A Tier 0 or Tier 1 operational asset is invalid unless it has all of the following:

```text
capacity envelope
multi-region replication
SPIFFE/SPIRE identity policy
KMS policy
comprehensive observability
at least one measurable SLO
```

No policy is satisfied by prose. The model must carry typed facts and validation must return zero structural violations. Separately, `ALIVE` requires observed execution of the exact admitted subject; neither SHACL conformance nor static enterprise governance closure is sufficient.

## Verification

```bash
cargo fmt --manifest-path crates/ggen-architecture/Cargo.toml --all -- --check
cargo clippy --manifest-path crates/ggen-architecture/Cargo.toml --workspace --all-targets -- -D warnings
cargo test --manifest-path crates/ggen-architecture/Cargo.toml --workspace --all-targets
cargo test --manifest-path crates/ggen-architecture/Cargo.toml --workspace --doc
```

The suite includes positive witnesses and negative falsifiers for enterprise traceability, structural admission, governance closure, impact propagation, transition cycles, viewpoint projection, receipt determinism/tampering, and the existing operational lifecycle, capacity, regional, SLO, key, identity, and autonomic boundaries. `tools/ggen-architecture/tests/enterprise_cli.rs` executes the real `ggen-ea` process against `examples/enterprise-model.json`.
