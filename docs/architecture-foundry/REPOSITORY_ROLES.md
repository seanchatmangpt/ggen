# Repository Authority and Ownership Boundaries

## 1. `ggen` — Repository Manufacturing Kernel

`ggen` owns generalized manufacturing law and reusable execution machinery. It must remain smaller, more stable, and less enterprise-specific than the corpus it processes.

### `ggen` owns

- admitted-observation interfaces;
- ontology loading, validation, and query execution;
- SHACL, SPARQL, graph transformation, and semantic closure;
- planning models and plan verification;
- bblock and pack resolution;
- repository projection and controlled filesystem actuation;
- receipt issuance and verification;
- deterministic replay;
- equivalence-runner infrastructure;
- external subsystem verification;
- standing and admission computation;
- generic adapters for CLI, config, filesystem, RDF, templates, receipts, LSP, diagnostics, packs, workflows, and typed refusals;
- governance that prevents generated projections from becoming authority.

### `ggen` does not permanently own

- historical enterprise-specific implementations;
- superseded repository layouts;
- organization-specific architecture decisions;
- retired CLI contracts;
- obsolete configuration families;
- customer-specific solution packs;
- historical compatibility layers whose value is evidentiary or referential rather than kernel-level;
- large collections of reference architectures that can evolve independently of the kernel.

A component remains in `ggen` only when at least one of these predicates is proven:

1. it is required to manufacture arbitrary repositories;
2. it implements a generic verifier or adapter family;
3. it is part of the minimal actuation, receipt, or replay path;
4. removing it would prevent the kernel from processing multiple independent architecture corpora.

## 2. `ggen-legacy` — Enterprise Architecture Foundry Corpus

`ggen-legacy` owns observed and generalized architecture knowledge derived from mature systems.

### `ggen-legacy` owns

- historical source snapshots or lawful references to immutable source revisions;
- capability inventories with provenance;
- architecture decision evidence;
- superseded and alternative implementation families;
- compatibility and migration adapters;
- explicit refusals and negative examples;
- reusable enterprise architecture primitives;
- architecture bblocks and solution packs;
- reference implementations that serve as positive witnesses;
- negative fixtures that prove failure and refusal behavior;
- economics, operations, governance, security, data, integration, and organizational constraints;
- cross-version equivalence portfolios;
- cross-repository lineage receipts;
- reusable Fortune-scale solution architecture patterns.

### Required corpus separation

`ggen-legacy` must distinguish five layers:

```text
observed source
  -> admitted evidence
  -> generalized primitive
  -> composed solution pack
  -> executable reference witness
```

These layers must never collapse into one directory of copied code.

Recommended top-level form:

```text
ggen-legacy/
├── corpus/
│   ├── repositories/
│   ├── histories/
│   ├── observations/
│   ├── capabilities/
│   └── decisions/
├── ontology/
├── primitives/
│   ├── governance/
│   ├── system/
│   ├── engine/
│   ├── graph/
│   ├── projection/
│   ├── evidence/
│   ├── products/
│   ├── verification/
│   ├── economics/
│   └── legacy/
├── bblocks/
├── packs/
├── reference-implementations/
├── migrations/
├── equivalence/
├── verifiers/
├── negative-fixtures/
├── receipts/
└── standing/
```

## 3. Manufactured customer repositories

Customer repositories are projections of admitted customer observations and selected foundry primitives. They are not forks of `ggen-legacy` and must not inherit the entire corpus.

A customer repository receives only:

- primitives selected by the admitted plan;
- customer-specific configuration and authority;
- generated implementation surfaces;
- customer-specific verifiers and evidence requirements;
- lineage receipts referencing the exact foundry inputs;
- a replayable manufacturing manifest.

## 4. Cross-repository authority chain

Every extraction from `ggen` to `ggen-legacy` must produce a cross-repository lineage record containing:

```text
source_repository
source_commit
source_path
source_digest
capability_ids
disposition
corpus_destination
corpus_commit
corpus_digest
replacement_owner
migration_evidence
equivalence_report
receipt_digest
```

Every manufacture from `ggen-legacy` into a new repository must produce:

```text
customer_observation_digest
selected_primitive_ids
selected_pack_ids
foundry_source_commit
kernel_source_commit
manufacturing_plan_digest
generated_tree_digest
verifier_report_digest
replay_report_digest
solution_standing
```

## 5. Disposition law

Every historical component receives exactly one final disposition:

- `PRESERVED` — behavior and implementation remain valid as an active reference witness;
- `SUBSUMED` — capability is represented by a broader primitive or pack;
- `REPLACED` — a new implementation owns the contract and equivalence is proven;
- `ARCHIVED` — retained as historical evidence but excluded from active composition;
- `REFUSED` — intentionally excluded with a typed reason, migration consequence, and negative evidence.

`UNKNOWN` is permitted only during observation. It blocks corpus admission and sunset admission.

## 6. Change propagation

- Changes to generalized manufacturing law begin in `ggen`.
- Changes to corpus content, primitives, or solution packs begin in `ggen-legacy`.
- Changes to a manufactured customer instance begin in customer authority and are replayed through `ggen`.
- A customer-specific improvement enters `ggen-legacy` only after de-identification, evidence review, generalization, and independent admission.
- A corpus primitive enters `ggen` only when it is proven to be kernel-generic across multiple independent corpora.

This prevents both repositories from becoming undifferentiated monoliths.

## Migrated v26.8.1 corpus

The versioned reconstruction corpus is owned by `seanchatmangpt/ggen-legacy`; see `config/ggen-legacy-corpus.toml` for the exact admitted coordinate. The generalized manufacturing kernel remains in this repository.
