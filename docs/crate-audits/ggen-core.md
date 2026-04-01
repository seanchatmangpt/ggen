# ggen-core — Crate Audit

**Path:** `crates/ggen-core/`
**Lines:** ~55 modules, largest crate in workspace
**Role:** Graph-aware code generation engine, pipeline orchestration, quality gates, RDF processing

---

## STUBS (functions that return empty/error instead of working)

### SHACL Validation — Entire subsystem stubbed (T014)

| File:Line | Function | Returns | Should Do |
|-----------|----------|---------|-----------|
| `validation/shacl.rs:132` | `ShapeLoader::load()` | Empty `ShaclShapeSet` | SPARQL-based shape loading from SHACL TTL. 628-line implementation exists in git history. |
| `validation/validator.rs:113` | `SparqlValidator::validate()` | Empty violations `Vec` | Run SPARQL queries against graph, collect constraint violations. Variable declared but never populated. |
| `validation/tests.rs:3` | 22 test functions | All commented out | Cardinality, enumeration, datatype, pattern, string length constraint tests. 715 lines in git history. |

**Cascade:** `v6/passes/normalization.rs:227` calls `ShapeLoader::load()` then passes dummy empty graph to validator. All SHACL enforcement is a no-op.

### Schema Parsing

| File:Line | Function | Returns | Should Do |
|-----------|----------|---------|-----------|
| `schema/parser.rs:281` | `from_json_schema()` | `Err("JSON Schema parsing not yet implemented")` | Parse JSON Schema into `Schema` struct |
| `schema/generators.rs:601` | `PythonGenerator::generate()` | Comment string | Generate Python dataclasses from schema |
| `schema/generators.rs:466` | Test `#[ignore]` | — | Blocked on `from_json_schema` |

### Sigma Runtime Invariants (4 of 7 are mock)

| File:Line | Invariant | Body |
|-----------|-----------|------|
| `ontology/validators.rs:162` | `TypeSoundness` | "Would validate types in RDF triples. For now: mock implementation." |
| `ontology/validators.rs:165` | `GuardSoundness` | "Would validate guard definitions are satisfiable." |
| `ontology/validators.rs:168` | `ProjectionDeterminism` | "Would verify projections are deterministic." |
| `ontology/validators.rs:171` | `SLOPreservation` | "Would check operator latencies." |

### Delta Proposer

| File:Line | Function | Returns |
|-----------|----------|---------|
| `ontology/delta_proposer.rs:345` | Build prompt | Returns mock proposals instead of calling LLM. Comment: "Build prompt (currently unused in mock implementation)" |

### Packs

| File:Line | Function | Returns |
|-----------|----------|---------|
| `packs/install.rs:33` | `install_pack()` | Bails: "install_pack is a stub. Use ggen_marketplace::install::Installer instead." |

### Lifecycle — Disabled Modules

| File:Line | Module | Status |
|-----------|--------|--------|
| `lifecycle/mod.rs:42` | `template_phase` | Commented out: "needs ggen_template crate which doesn't exist yet" |
| `lifecycle/mod.rs:45` | `dag` | `#[allow(dead_code)]` — "DAG resolution - use direct hooks for now" |
| `lifecycle/mod.rs:47` | `dx` | `#[allow(dead_code)]` — "Developer experience helpers" |
| `lifecycle/model.rs:183` | `on_error`/`on_success` | Commented out: "Future: Error handling hooks (80/20)" |

---

## DEAD CODE (structs/modules with zero consumers)

| File:Line | Item | Status |
|-----------|------|--------|
| `config/hive_coordinator.rs` | `HiveCoordinator`, `HiveState`, `ConsensusTopic`, `PackageConflict`, `CompatibilityMatrix` | Entire module `#[allow(dead_code)]`. Planned distributed dep resolution, never wired. |
| `ontology/sigma_runtime.rs:380` | `overlays: BTreeMap<String, SigmaOverlay>` | `#[allow(dead_code)]` — "Reserved for overlay functionality (not yet implemented)" |
| `v6/pipeline.rs:34` | `V6Pipeline` struct | `#[allow(dead_code)]` — earlier iteration, separate from `StagedPipeline` |
| `v6/pipeline.rs:204` | `PipelineStage` | `#[allow(dead_code)]` |
| `v6/pipeline.rs:310` | `V6PipelineConfig` | `#[allow(dead_code)]` |
| `pipeline.rs:384` | `Plan` struct | Dead code, no consumers |
| `pack_resolver.rs:170` | `stage_pack_templates()` | `#[allow(dead_code)]` — never called |
| `prevention/state_machine.rs:289` | `validate_template()` | Only checks empty content, should validate Tera syntax |
| `codegen/execution_proof.rs:116` | Verification logic | Comment describes what should happen; no code executes |

---

## ARCHITECTURE ISSUES

### Two Parallel Pipelines

`GenerationPipeline` (`codegen/pipeline.rs:811`) is the production pipeline used by `ggen sync`.
`StagedPipeline` (`v6/pipeline.rs:329`) is the constitutional pipeline with receipt provenance — never called from CLI.

**Decision needed:** Which is canonical? If v6, wire it into sync. If GenerationPipeline, document it as canonical and deprioritize v6.

### PackResolver Location

Documented as `v6/resolver.rs` but actually lives at `src/pack_resolver.rs` (top-level module, not inside v6/).

### Build Warnings

`ggen-receipt/src/chain.rs:5` — unused import `SigningKey`. Fix: remove or use in test.

---

## FIX / DELETE / REFACTOR

| Action | Item | Priority |
|--------|------|----------|
| **FIX** | T014 SHACL: restore from git history, resolve Graph::query() API | P0 |
| **FIX** | Sigma invariants: implement real validation logic | P2 |
| **FIX** | Delta proposer: wire to actual LLM service | P2 |
| **FIX** | Schema parser: implement JSON Schema parsing | P1 |
| **FIX** | Build warning: unused `SigningKey` import in ggen-receipt | P3 |
| **DELETE** | `config/hive_coordinator.rs` — entire module dead | P3 |
| **DELETE** | `v6/pipeline.rs` dead structs: `V6Pipeline`, `PipelineStage`, `V6PipelineConfig` | P3 |
| **DELETE** | `pipeline.rs` dead `Plan` struct | P3 |
| **DELETE** | `packs/install.rs` — redirect exists, remove stub | P3 |
| **REFACTOR** | Decide canonical pipeline (Generation vs Staged) | P0 |
| **REFACTOR** | Move PackResolver into v6/ or update all docs | P3 |
