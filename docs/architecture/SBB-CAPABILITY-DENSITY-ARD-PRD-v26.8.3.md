# SBB Capability Density v26.8.3

**Document type:** Product Requirements Document + Architecture Requirements Document  
**Status:** IMPLEMENTED CHECKPOINT — external standing pending repository CI and independent verification  
**Date:** 2026-08-02  
**Repository baseline:** `main@8351af4c5bbbf60bd99ab8417752a1762c6ea4e3`  
**Product surface:** `ggen sbb schema|inspect|validate|distribution|receipt|replay`  
**Crown target:** one Solution Building Block may admit at least 1,000 unique commit-equivalent capability units and distribute their effects across every declared ontology, textual form, audience, language, jurisdiction, organization profile, and runtime without duplicating the canonical maintenance lineage.

---

## 0. Canon

### 0.1 Governing laws

1. **Chatman Equation:** `A = μ(O*)`. Git history, files, tests, manifests, and claims are observations. Only bounded, digest-checked evidence enters `O*`. Density evaluation and projection are manufacturing `μ`. The report and receipt chain are artifacts `A`.
2. **Combinatorial Maximalism:** maximize reversible construction across admitted ontology and textual-form combinations; execute the minimum irreversible consequence.
3. **One commit, at most one density unit:** multiple labels, files, generated outputs, or capabilities claimed from the same commit cannot inflate commit-equivalent density.
4. **No weighted self-claims:** authors cannot assign arbitrary numerical weights. One admitted unique commit contributes exactly one commit-equivalent unit.
5. **Complete manufacturing chain:** every counted delta binds `Ontology → SHACL → SPARQL → Typestate → Template → Artifact → Runtime Surface → Walkthrough → Receipt → Replay`.
6. **Claim standing chain:** every counted delta additionally binds a positive witness, negative fixture, adversarial falsifier, and executable verifier.
7. **Observed evidence only:** a capability counts only when its commit exists in the admitted Git repository and every repository-relative evidence object is readable at that exact commit and matches its BLAKE3 digest.
8. **External witness rule:** `ggen sbb` reports evidence and a claim ceiling. It cannot promote its own result to `ALIVE`; its maximum self-reported standing is `PARTIAL_ALIVE`.
9. **No evidence fabrication:** absent Git objects, malformed digests, digest mismatches, duplicate commits, duplicate identities, and empty distribution axes are violations.
10. **Zero unreceipted mutation:** the only write operation in this checkpoint is `ggen sbb receipt`, which writes a deterministic report and chained intent/result receipts using atomic file replacement. It performs no deployment or external actuation.
11. **Projection law:** the RDF ontology is semantic authority. SHACL, SPARQL, JSON Schema, CLI JSON, reports, diagrams, and documentation are constrained projections.
12. **SBB distribution law:** delivered capability instances are a derived metric, never additional canonical capabilities.

### 0.2 Vocabulary

- **Capability delta:** one reusable capability increment bound to one unique Git commit and a complete evidence chain.
- **Commit-equivalent unit:** one unique commit whose capability delta is Git-observed, digest-valid, structurally complete, and non-duplicated.
- **Canonical maintenance units:** the number of admitted commit-equivalent units maintained in the SBB lineage.
- **Distribution context:** one member of the Cartesian product of all declared distribution axes.
- **Delivered capability instance:** one commit-equivalent unit projected into one distribution context.
- **Distribution multiplier:** the number of distribution contexts served by one canonical lineage.
- **Capability density:** reusable, verified capability effects divided by canonical maintenance surface. Projected instances are not represented as independent capabilities.

---

## 1. Working-backwards release statement

### ggen v26.8.3 makes a 1,000-commit SBB an auditable distribution unit rather than a marketing claim

A service provider can publish an SBB capability manifest with a target of 1,000 commit-equivalent units. Each unit must name a unique Git commit, capability IRI, family, ontology modules, textual forms, and all required evidence bindings. `ggen sbb inspect` evaluates every binding. `validate` determines whether the declared threshold is met. `distribution` calculates the lawful projection surface. `receipt` persists a deterministic report and chained receipts. `replay` recomputes the result against the exact manifest and Git-object evidence and returns `REPLAY_MATCH` or `REPLAY_DIVERGED`.

The feature does not count raw commit volume. It counts only unique commits with a complete observed proof chain. A 1,000-commit target therefore means 1,000 independently attributable capability deltas, not 1,000 formatting changes, generated-file churn, merge commits, or duplicated claims.

---

## 2. Product model

### 2.1 SBB identity and repository boundary

Every manifest declares:

- an admitted local Git repository root used for commit-object observation;
- stable SBB identifier;
- semantic version;
- architecture-contract IRI;
- positive minimum commit-equivalent target.

The standard high-density service-provider profile sets:

```json
{
  "minimum_commit_equivalent_units": 1000
}
```

This is a threshold, not evidence that the threshold has been met.

### 2.2 Capability delta

A delta declares:

- stable delta identifier;
- unique Git commit SHA;
- capability IRI;
- capability family;
- human summary;
- ontology modules used;
- textual forms projected;
- ten-stage manufacturing chain;
- positive witness;
- negative fixture;
- adversarial falsifier;
- verifier.

A commit claimed by multiple deltas invalidates every colliding claim. This makes density resistant to relabeling attacks.

### 2.3 Evidence binding

An evidence binding contains:

- a safe repository-relative path;
- a BLAKE3 content digest.

Evidence counts only when `git show <commit>:<path>` returns the bytes and their digest matches. Current working-tree bytes do not overwrite historical evidence. Remote URIs, absolute paths, parent traversal, and missing Git objects are outside this checkpoint and are refused.

### 2.4 Distribution axes

Every SBB declares non-empty, duplicate-free sets of:

- ontology modules;
- textual forms;
- audiences;
- languages;
- jurisdictions;
- organization profiles;
- runtimes.

The distribution context count is their Cartesian product. The calculation uses checked `u128` arithmetic and refuses overflow.

### 2.5 Density report

The report includes:

- manifest and report digests;
- declared and observed delta counts;
- observed unique commit-equivalent units;
- duplicate-commit collisions;
- target status;
- external-admission eligibility;
- axis cardinalities;
- distribution contexts;
- delivered capability instances;
- per-delta violations;
- claim ceiling.

### 2.6 Receipt and replay

`ggen sbb receipt` writes:

1. `density-intent.json`;
2. `density-report.json`;
3. `density-result.json`.

The result receipt points to the intent receipt digest. Subsequent runs chain from the prior result digest. `ggen sbb replay` verifies both receipt digests, the intent/result link, manifest digest, report digest, and a freshly recomputed report.

---

## 3. Service-provider economics

For `U` admitted commit-equivalent units and axis cardinalities `a₁…aₙ`:

```text
contexts = Π aᵢ
delivered_capability_instances = U × contexts
canonical_maintenance_units = U
distribution_multiplier = contexts
```

A provider therefore maintains one canonical 1,000-unit lineage while distributing its effects across every admitted SBB context. The metric does not claim that projections are new intellectual capabilities. It measures how much verified capability is delivered without reproducing the canonical engineering history for each customer.

Manual delivery repeatedly reconstructs capability and evidence. Agent delivery repeatedly generates candidate capability and review burden. SBB distribution amortizes architecture, ontology, verification, migration law, and receipts across customers while preserving explicit overlays and environmental bindings.

---

## 4. Functional requirements

### FR-001 — Machine-readable schema

`ggen sbb schema` returns canonical schema identifiers, the ten required chain stages, required claim witnesses, density-unit definition, and external-witness rule.

### FR-002 — Pure inspection

`ggen sbb inspect <manifest>` reads the manifest and Git-object evidence without writing state. It returns the complete density report.

### FR-003 — Threshold validation

`ggen sbb validate <manifest>` reports target attainment and external-admission eligibility. It must never report `ALIVE`.

### FR-004 — Distribution analysis

`ggen sbb distribution <manifest>` reports canonical maintenance units, axis cardinalities, distribution contexts, multiplier, and delivered capability instances.

### FR-005 — Duplicate resistance

Duplicate delta IDs, capability IRIs, and commit claims are violations. Duplicate commit claims contribute zero observed units.

### FR-006 — Digest enforcement

All evidence digests use `blake3:<64 hex>` or `blake3-<64 hex>`. A missing Git object or digest mismatch prevents the delta from counting.

### FR-007 — Repository-object boundary

Only safe repository-relative evidence paths are countable. Remote URIs, absolute paths, parent traversal, missing Git objects, and uncommitted working-tree substitutions are refused.

### FR-008 — Receipted report emission

`ggen sbb receipt <manifest> <output>` uses atomic writes and emits chained intent/result receipts over the exact report and manifest digests.

### FR-009 — Replay

`ggen sbb replay <manifest> <output>` returns only `REPLAY_MATCH` or `REPLAY_DIVERGED` with the relevant digests.

### FR-010 — 1,000-unit profile

The implementation must support a manifest target of at least 1,000 units without changing its algorithm or schema. Target attainment is computed from unique fully observed commits.

---

## 5. Non-functional requirements

- **Determinism:** identical manifest bytes and Git commit-object evidence bytes produce the same report digest.
- **Fail closed:** malformed schema, identity, commit, IRI, digest, axis, evidence, or arithmetic results in violations or command refusal.
- **Bounded memory:** evaluation is linear in delta count plus evidence count; uniqueness indexes use ordered sets and maps.
- **Stable JSON:** report and receipt schemas are versioned.
- **Thin CLI:** noun/verb functions delegate to manifest loading, evidence evaluation, density calculation, receipt issuance, and replay helpers.
- **No network:** the evaluator performs no remote fetch and no external actuation.
- **No self-certification:** the claim ceiling is fixed at `PARTIAL_ALIVE`.

---

## 6. Refusal conditions

| Code | Condition |
|---|---|
| `SBB-DENSITY-001` | unsupported manifest schema |
| `SBB-DENSITY-002` | incomplete SBB identity or zero target |
| `SBB-DENSITY-003` | invalid or duplicate distribution axis |
| `SBB-DENSITY-004` | invalid or unobservable Git commit identity |
| `SBB-DENSITY-005` | duplicate delta ID or capability IRI |
| `SBB-DENSITY-006` | duplicate commit claim |
| `SBB-DENSITY-007` | missing evidence or malformed digest |
| `SBB-DENSITY-008` | Git-object evidence digest mismatch |
| `SBB-DENSITY-009` | incomplete ontology or textual-form binding |
| `SBB-DENSITY-010` | distribution arithmetic overflow |
| `SBB-DENSITY-011` | prior receipt digest invalid |
| `SBB-DENSITY-012` | replay divergence |

The initial CLI returns human-readable violations; stable machine refusal codes are the next compatibility checkpoint.

---

## 7. Implementation map

| Semantic layer | Repository artifact |
|---|---|
| Ontology | `ontology/sbb-capability-density.ttl` |
| SHACL | `ontology/sbb-capability-density.shacl.ttl` |
| SPARQL | `queries/sbb-capability-density.rq` |
| JSON carrier | `packs/sbb-capability-density-pack/schema/sbb-capability-manifest.schema.json` |
| Typestate/runtime evaluator | `crates/ggen-cli/src/cmds/sbb.rs` |
| CLI noun registration | `crates/ggen-cli/src/cmds/mod.rs` |
| Architecture contract | this document |
| Tests | unit tests in `sbb.rs` |

---

## 8. Acceptance criteria

1. One fully observed unique commit whose evidence is read from the Git object database produces exactly one commit-equivalent unit.
2. Two deltas claiming the same commit produce zero units and one collision.
3. Uncommitted working-tree mutation does not rewrite historical commit evidence.
4. A valid report can be emitted with intent/result receipts and replayed to `REPLAY_MATCH`.
5. The evaluator never reports `ALIVE`.
6. A manifest with `minimum_commit_equivalent_units = 1000` remains below target until 1,000 unique commits have complete observed chains.
7. Distribution counts are derived from unique non-empty axis values and checked for overflow.
8. Inspection and validation perform no writes.

---

## 9. Standing declaration

This change implements the repository-native capability-density model, CLI evaluator, ontology, shape constraints, query, JSON carrier schema, receipts, replay, and unit tests. Because this document cannot execute repository CI or serve as an independent verifier, the implementation remains **PARTIAL_ALIVE** until exact-head checks and an external witness admit the aggregate result.
