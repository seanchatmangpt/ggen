# Tasks: N3/CONSTRUCT Semantic Code Generator

**Branch**: `008-n3-code-gen` | **Date**: 2024-12-14
**Input**: Design documents from `/specs/008-n3-code-gen/`
**Prerequisites**: plan.md ✓, spec.md ✓, research.md ✓, data-model.md ✓, contracts/ ✓

**Strategic Decision**: Named CONSTRUCT queries as canonical rule language (NOT full N3 rule execution).

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US7)
- Exact file paths included for LLM execution

## Path Conventions

```
crates/ggen-core/src/          # Core library (manifest, codegen, graph extensions)
crates/ggen-core/tests/        # Integration tests
crates/ggen-cli/src/commands/  # CLI commands (generate, validate)
```

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Module scaffolding and dependency setup

- [X] T001 [P] Create `crates/ggen-core/src/manifest/mod.rs` - Module declaration with `parser`, `types`, `validation` submodules
- [X] T002 [P] Create `crates/ggen-core/src/codegen/pipeline.rs` - Empty module with `GenerationPipeline` struct stub
- [X] T003 [P] Create `crates/ggen-core/src/codegen/code_graph.rs` - Empty module with `CodeGraphBuilder` struct stub
- [X] T004 [P] Create `crates/ggen-core/src/codegen/audit.rs` - Empty module with `AuditTrailBuilder` struct stub
- [X] T005 [P] Create `crates/ggen-core/src/graph/construct.rs` - Empty module with `ConstructExecutor` struct stub
- [X] T006 Update `crates/ggen-core/src/lib.rs` - Add `pub mod manifest;` and update `codegen` module exports
- [X] T007 Update `crates/ggen-core/Cargo.toml` - Verify `toml = "0.9"` dependency exists (should be present)
- [X] T008 Run `cargo make check` - Verify compilation with new module stubs

**Checkpoint**: Module structure compiles, ready for type definitions

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core types and parsers that ALL user stories depend on

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

### Manifest Types (T009-T014)

- [X] T009 [P] Implement `GgenManifest` in `crates/ggen-core/src/manifest/types.rs` - Root config struct with `project`, `ontology`, `inference`, `generation`, `validation` fields per data-model.md
- [X] T010 [P] Implement `ProjectConfig` in `crates/ggen-core/src/manifest/types.rs` - `name`, `version`, `description` fields
- [X] T011 [P] Implement `OntologyConfig` in `crates/ggen-core/src/manifest/types.rs` - `source`, `imports`, `base_iri`, `prefixes` (BTreeMap)
- [X] T012 [P] Implement `InferenceConfig` and `InferenceRule` in `crates/ggen-core/src/manifest/types.rs` - `rules` Vec, `name`, `construct`, `order`, `when` fields
- [X] T013 [P] Implement `GenerationConfig` and `GenerationRule` in `crates/ggen-core/src/manifest/types.rs` - Including `QuerySource`, `TemplateSource` enums
- [X] T014 [P] Implement `ValidationConfig` and `ValidationRule` in `crates/ggen-core/src/manifest/types.rs` - `shacl`, `validate_syntax`, `no_unsafe`, custom rules

### Manifest Parser (T015-T018)

- [X] T015 Implement `ManifestParser::parse(path: &Path) -> Result<GgenManifest, GgenError>` in `crates/ggen-core/src/manifest/parser.rs` - TOML parsing with serde
- [X] T016 Implement `ManifestParser::validate(&self) -> Result<(), GgenError>` in `crates/ggen-core/src/manifest/validation.rs` - Schema validation (required fields, path existence)
- [X] T017 Add unit tests in `crates/ggen-core/tests/manifest/parser_tests.rs` - Valid manifest parsing, missing fields error, invalid paths error (inline in parser.rs)
- [X] T018 Add unit tests in `crates/ggen-core/tests/manifest/validation_tests.rs` - Schema validation edge cases (inline in validation.rs)

### CONSTRUCT Executor (T019-T023)

- [X] T019 Implement `ConstructExecutor::new(graph: &Graph) -> Self` in `crates/ggen-core/src/graph/construct.rs` - Initialize with graph reference
- [X] T020 Implement `ConstructExecutor::execute(query: &str) -> Result<Vec<String>, GgenError>` in `crates/ggen-core/src/graph/construct.rs` - Execute CONSTRUCT, return triples as strings (leverage existing `QueryResults::Graph` handling from core.rs:200-206)
- [X] T021 Implement `ConstructExecutor::execute_and_materialize(query: &str) -> Result<usize, GgenError>` in `crates/ggen-core/src/graph/construct.rs` - Execute CONSTRUCT and insert results back into graph, return triple count
- [X] T022 Update `crates/ggen-core/src/graph/mod.rs` - Export `construct` module and `ConstructExecutor`
- [X] T023 Add unit tests in `crates/ggen-core/tests/construct/query_tests.rs` - CONSTRUCT execution, triple parsing, materialization (inline in construct.rs)

**Checkpoint**: Foundation ready - user story implementation can now begin

---

## Phase 3: US1 - Domain Model to Rust Code (Priority: P1) 🎯 MVP

**Goal**: Developer creates ontology.ttl → runs `ggen generate` → receives Rust structs with derives

**Independent Test**: Create `domain.ttl` with 3 entities, generate, verify `pub struct User { ... }` with correct derive macros

### Code Graph Types (T024-T029)

- [X] T024 [P] [US1] Implement `CodeStruct` in `crates/ggen-core/src/codegen/code_graph.rs` - `iri`, `name`, `visibility`, `derives`, `fields`, `docstring`, `attributes`, `source_iri` per data-model.md
- [X] T025 [P] [US1] Implement `CodeField` in `crates/ggen-core/src/codegen/code_graph.rs` - `iri`, `name`, `field_type`, `visibility`, `docstring`, `attributes`, `default`
- [X] T026 [P] [US1] Implement `CodeModule` in `crates/ggen-core/src/codegen/code_graph.rs` - `iri`, `name`, `visibility`, `imports`, `items`, `attributes`
- [X] T027 [P] [US1] Implement `CodeItem` enum in `crates/ggen-core/src/codegen/code_graph.rs` - `Struct(CodeStruct)`, `Trait(CodeTrait)`, `Impl(CodeImpl)`, `Enum(CodeEnum)` variants
- [X] T028 [P] [US1] Implement `CodeImport` in `crates/ggen-core/src/codegen/code_graph.rs` - `path`, `alias` fields
- [X] T029 [US1] Implement `CodeGraphBuilder::from_sparql_results(results: &[BTreeMap<String, String>]) -> Result<Vec<CodeStruct>, GgenError>` - Parse SPARQL SELECT results into CodeStruct objects

### Code Graph to Template Integration (T030-T034)

- [X] T030 [US1] Implement `CodeGraphBuilder::to_tera_context(&self) -> tera::Context` in `crates/ggen-core/src/codegen/code_graph.rs` - Convert code graph entities to Tera-compatible context
- [X] T031 [US1] Create `crates/ggen-core/src/rdf/templates/struct.tera` - Default Tera template for Rust struct generation with derives, fields, docstrings (+ impl.tera, enum.tera, trait.tera, module.tera)
- [X] T032 [US1] Implement `GenerationPipeline::render_template(template: &TemplateSource, context: &Context) -> Result<String, GgenError>` in `crates/ggen-core/src/codegen/pipeline.rs` - Tera rendering with existing template.rs infrastructure (stub)
- [X] T033 [US1] Implement output file path expansion: `GenerationPipeline::expand_output_path(pattern: &str, context: &BTreeMap<String, String>) -> PathBuf` - Replace `{{name}}` etc.
- [ ] T034 [US1] Add integration test in `crates/ggen-core/tests/codegen/pipeline_tests.rs` - End-to-end: load ontology → SPARQL SELECT → render template → verify Rust output

### US1 Acceptance Scenario Tests (T035-T037)

- [ ] T035 [US1] Add test: Given `:User a rdfs:Class ; :codegen-as "struct"` → output contains `pub struct User { ... }` - in `crates/ggen-core/tests/codegen/us1_acceptance.rs`
- [ ] T036 [US1] Add test: Given struct with `Uuid` field → `Serialize, Deserialize` derives added (via inference rule) - in `crates/ggen-core/tests/codegen/us1_acceptance.rs`
- [ ] T037 [US1] Add test: Given `:auditable true` → `created_at`, `updated_at` fields added (via inference rule) - in `crates/ggen-core/tests/codegen/us1_acceptance.rs`

**Checkpoint**: US1 complete - ontology → Rust struct generation works end-to-end

---

## Phase 4: US2 - Relationship-Driven Impl Generation (Priority: P1)

**Goal**: Developer defines `:User :has_many :Order` → impl blocks with accessor methods generated

**Independent Test**: Define relationship → verify `get_orders(&self) -> Vec<Order>` method in impl block

### Impl and Method Types (T038-T042)

- [ ] T038 [P] [US2] Implement `CodeTrait` in `crates/ggen-core/src/codegen/code_graph.rs` - `iri`, `name`, `visibility`, `bounds`, `methods`, `is_async`, `docstring`
- [ ] T039 [P] [US2] Implement `CodeMethod` in `crates/ggen-core/src/codegen/code_graph.rs` - `iri`, `name`, `visibility`, `is_async`, `self_param`, `params`, `return_type`, `body`, `docstring`
- [ ] T040 [P] [US2] Implement `CodeParam` in `crates/ggen-core/src/codegen/code_graph.rs` - `name`, `param_type`
- [ ] T041 [P] [US2] Implement `CodeImpl` in `crates/ggen-core/src/codegen/code_graph.rs` - `iri`, `for_type`, `trait_name`, `generics`, `methods`
- [ ] T042 [US2] Create CONSTRUCT query template for relationship → impl in `crates/ggen-core/src/rdf/queries/relationship_impl.sparql`

### Relationship Processing (T043-T046)

- [ ] T043 [US2] Implement `CodeGraphBuilder::build_impl_from_relationship(source: &str, rel_type: &str, target: &str) -> CodeImpl` - Build impl block with getter method
- [ ] T044 [US2] Create `crates/ggen-core/src/rdf/templates/impl.tera` - Default Tera template for impl block generation
- [ ] T045 [US2] Add test: Given `:Entity1 :has_many :Entity2` → `impl Entity1 { pub fn get_entity2s(&self) -> Vec<Entity2> }` - in `crates/ggen-core/tests/codegen/us2_acceptance.rs`
- [ ] T046 [US2] Add test: Given `:Entity :soft_delete true` → `deleted_at: Option<DateTime<Utc>>` field inferred - in `crates/ggen-core/tests/codegen/us2_acceptance.rs`

**Checkpoint**: US2 complete - relationships generate accessor methods

---

## Phase 5: US3 - ggen.toml Manifest-Driven Generation (Priority: P1)

**Goal**: Developer creates `ggen.toml` → `ggen generate` executes entire pipeline from manifest

**Independent Test**: Create minimal ggen.toml → run generate → verify output files exist

### Generation Pipeline (T047-T053)

- [ ] T047 [US3] Implement `GenerationPipeline::new(manifest: GgenManifest) -> Self` in `crates/ggen-core/src/codegen/pipeline.rs` - Initialize with parsed manifest
- [ ] T048 [US3] Implement `GenerationPipeline::load_ontology(&mut self) -> Result<(), GgenError>` - Load source.ttl and imports into graph
- [ ] T049 [US3] Implement `GenerationPipeline::execute_inference_rules(&mut self) -> Result<Vec<ExecutedRule>, GgenError>` - Execute CONSTRUCT rules in order, materialize results
- [ ] T050 [US3] Implement `GenerationPipeline::execute_generation_rules(&mut self) -> Result<Vec<GeneratedFile>, GgenError>` - Execute SELECT queries, render templates, write files
- [ ] T051 [US3] Implement `GenerationPipeline::run(&mut self) -> Result<PipelineState, GgenError>` - Full pipeline: load → inference → generation → validation
- [ ] T052 [US3] Implement `PipelineState` struct in `crates/ggen-core/src/codegen/pipeline.rs` - Track manifest, graphs, executed rules, generated files, validation results per data-model.md
- [ ] T053 [US3] Add integration test: Given ggen.toml with `[ontology]` and `[[generation.rules]]` → files generated - in `crates/ggen-core/tests/codegen/us3_acceptance.rs`

### CLI Commands (T054-T058)

- [ ] T054 [US3] Create `crates/ggen-cli/src/commands/generate.rs` - `ggen generate` command using clap
- [ ] T055 [US3] Implement CLI arguments per cli-contract.md: `[MANIFEST]`, `--output-dir`, `--dry-run`, `--force`, `--audit`, `--verbose`, `--quiet`, `--timeout`
- [ ] T056 [US3] Implement `run_generate(args: GenerateArgs) -> Result<(), GgenError>` - Load manifest, create pipeline, execute, report results
- [ ] T057 [US3] Update `crates/ggen-cli/src/cmds/mod.rs` - Register `generate` subcommand
- [ ] T058 [US3] Add CLI test: `ggen generate --dry-run ./ggen.toml` shows preview without writing - in `crates/ggen-cli/tests/generate_tests.rs`

**Checkpoint**: US3 complete - full pipeline from manifest works via CLI

---

## Phase 6: US4 - Inference Rules (Priority: P2)

**Goal**: Developer writes inference rules in ggen.toml that apply before CONSTRUCT execution

**Independent Test**: Define rule for auto-deriving `PartialEq` on structs with >5 fields → verify derive added

### Inference Rule Execution (T059-T063)

- [ ] T059 [US4] Implement `InferenceExecutor::new(graph: &mut Graph) -> Self` in `crates/ggen-core/src/codegen/pipeline.rs` - Initialize with mutable graph reference
- [ ] T060 [US4] Implement `InferenceExecutor::execute_rule(rule: &InferenceRule) -> Result<ExecutedRule, GgenError>` - Execute CONSTRUCT, materialize, track metrics
- [ ] T061 [US4] Implement `InferenceExecutor::check_when_condition(when: &str) -> Result<bool, GgenError>` - Execute SPARQL ASK for conditional rules
- [ ] T062 [US4] Add inference rule timeout handling with `max_reasoning_timeout_ms` config
- [ ] T063 [US4] Add test: Given custom inference rule in `[[inference.rules]]` → rule applied before generation - in `crates/ggen-core/tests/codegen/us4_acceptance.rs`

### Built-in Inference Rules (T064-T067)

- [ ] T064 [P] [US4] Create `crates/ggen-core/src/rdf/rules/auditable_fields.sparql` - CONSTRUCT rule: `:auditable true` → add `created_at`, `updated_at` fields
- [ ] T065 [P] [US4] Create `crates/ggen-core/src/rdf/rules/uuid_derives.sparql` - CONSTRUCT rule: `Uuid` field → add `Serialize`, `Deserialize` derives
- [ ] T066 [P] [US4] Create `crates/ggen-core/src/rdf/rules/soft_delete.sparql` - CONSTRUCT rule: `:soft_delete true` → add `deleted_at` field
- [ ] T067 [US4] Implement `GenerationPipeline::load_builtin_rules() -> Vec<InferenceRule>` - Load embedded rules as defaults

**Checkpoint**: US4 complete - inference rules execute and enrich graph

---

## Phase 7: US5 - CONSTRUCT Query Composition (Priority: P2)

**Goal**: Developer chains multiple CONSTRUCT queries for progressive code graph building

**Independent Test**: Define 3 CONSTRUCT queries (structs, impls, tests) → verify all execute in order

### Sequential Materialization (T068-T072)

- [ ] T068 [US5] Implement `ConstructExecutor::execute_chain(queries: &[InferenceRule]) -> Result<Vec<ExecutedRule>, GgenError>` - Execute queries in order, materialize after each
- [ ] T069 [US5] Add deterministic IRI generation: `ConstructExecutor::generate_iri(base: &str, salt: Option<&str>) -> String` - Hash-based for reproducibility
- [ ] T070 [US5] Add ORDER BY validation in `ManifestParser::validate()` - Warn if CONSTRUCT queries lack ORDER BY
- [ ] T071 [US5] Add test: Given CONSTRUCT with `BIND(IRI(CONCAT(...)))` → new IRIs generated without collision - in `crates/ggen-core/tests/construct/materialization_tests.rs`
- [ ] T072 [US5] Add test: Given 3 chained CONSTRUCT queries → each can query results of previous - in `crates/ggen-core/tests/construct/materialization_tests.rs`

**Checkpoint**: US5 complete - CONSTRUCT chaining works with materialization

---

## Phase 8: US6 - Poka-Yoke Safety Validation (Priority: P2)

**Goal**: Validation CONSTRUCT queries check code graph before writing files

**Independent Test**: Create ontology with missing `:fieldType` → verify exit code 1 with clear error

### Validation Pipeline (T073-T079)

- [ ] T073 [US6] Implement `ValidationExecutor::new(graph: &Graph) -> Self` in `crates/ggen-core/src/codegen/pipeline.rs`
- [ ] T074 [US6] Implement `ValidationExecutor::validate_code_graph() -> Result<Vec<ValidationResult>, GgenError>` - Run built-in SPARQL ASK validations
- [ ] T075 [US6] Implement `ValidationExecutor::validate_custom_rules(rules: &[ValidationRule]) -> Result<Vec<ValidationResult>, GgenError>` - Run user-defined ASK queries
- [ ] T076 [US6] Implement `ValidationExecutor::validate_syntax(code: &str) -> Result<(), GgenError>` - Parse generated Rust with syn crate
- [ ] T077 [US6] Implement `ValidationExecutor::check_no_unsafe(code: &str) -> Result<(), GgenError>` - Scan for `unsafe` blocks if `no_unsafe = true`
- [ ] T078 [US6] Create `crates/ggen-cli/src/commands/validate.rs` - `ggen validate` command per cli-contract.md
- [ ] T079 [US6] Add tests per acceptance scenarios: missing fieldType → exit 1, SHACL violation → fail, `unsafe` detected → exit 4 - in `crates/ggen-core/tests/codegen/us6_acceptance.rs`

### Error Messages (T080-T082)

- [ ] T080 [P] [US6] Implement structured error format per cli-contract.md: `error[ECODE]: Brief description\n  --> file:line:column`
- [ ] T081 [P] [US6] Add error code mapping: E0001=Validation, E0002=SPARQL, E0003=Template, E0004=Output, E0005=Timeout
- [ ] T082 [US6] Add test: Error messages identify exact IRI/property for validation failures (SC-010) - in `crates/ggen-core/tests/codegen/us6_acceptance.rs`

**Checkpoint**: US6 complete - validation prevents invalid code generation

---

## Phase 9: US7 - Audit Trail for Agent Verification (Priority: P3)

**Goal**: `audit.json` created with input hashes, pipeline steps, output verification

**Independent Test**: Run generation twice → both `audit.json` files identical except timestamps

### Audit Trail Generation (T083-T089)

- [ ] T083 [US7] Implement `AuditTrailBuilder::new() -> Self` in `crates/ggen-core/src/codegen/audit.rs`
- [ ] T084 [US7] Implement `AuditTrailBuilder::record_inputs(manifest: &Path, ontologies: &[Path], templates: &[Path]) -> AuditInputs` - SHA256 hash each input file
- [ ] T085 [US7] Implement `AuditTrailBuilder::record_step(step_type: &str, name: &str, duration: Duration, triples: Option<usize>, status: &str) -> AuditStep`
- [ ] T086 [US7] Implement `AuditTrailBuilder::record_output(path: &Path, content: &str, source_rule: &str) -> AuditOutput` - SHA256 hash content
- [ ] T087 [US7] Implement `AuditTrailBuilder::build() -> AuditTrail` - Assemble complete audit record
- [ ] T088 [US7] Implement `AuditTrail::write_to(path: &Path) -> Result<(), GgenError>` - Write JSON with sorted keys (BTreeMap) for determinism
- [ ] T089 [US7] Add tests: Same inputs → identical audit.json (except timestamp), content hashes match files - in `crates/ggen-core/tests/codegen/audit_tests.rs`

### Determinism Verification (T090-T092)

- [ ] T090 [P] [US7] Verify all internal maps use BTreeMap (audit crate/ggen-core grep check)
- [ ] T091 [P] [US7] Add `--verify-determinism` flag to `ggen generate` - Run twice, compare output hashes
- [ ] T092 [US7] Add test: Generation on different runs produces byte-identical output (SC-002) - in `crates/ggen-core/tests/codegen/audit_tests.rs`

**Checkpoint**: US7 complete - audit trail enables reproducibility verification

---

## Phase 10: Polish & Cross-Cutting Concerns

**Purpose**: Documentation, performance, integration

### Documentation (T093-T095)

- [ ] T093 [P] Update `crates/ggen-core/src/rdf/code_ontology.ttl` - Add any missing code: vocabulary terms discovered during implementation
- [ ] T094 [P] Create example project in `examples/008-semantic-gen/` - Working ggen.toml + ontology + templates demonstrating US1-US7
- [ ] T095 Run quickstart.md validation - Verify all steps work end-to-end

### Performance Validation (T096-T098)

- [ ] T096 Add benchmark: 50 entities generates in <5s (SC-001) - in `crates/ggen-core/benches/generation_bench.rs`
- [ ] T097 Add benchmark: 500 entities generates in <30s (SC-009) - in `crates/ggen-core/benches/generation_bench.rs`
- [ ] T098 Profile and optimize hot paths if benchmarks fail

### Final Integration (T099-T101)

- [ ] T099 Run `cargo make test` - All tests pass
- [ ] T100 Run `cargo make lint` - No clippy warnings
- [ ] T101 Run `cargo make check` - Compilation clean

---

## Dependencies & Execution Order

### Phase Dependencies

```
Phase 1 (Setup) ──────────────────┐
                                  ▼
Phase 2 (Foundational) ──────────► BLOCKS ALL USER STORIES
                                  │
          ┌───────────────────────┼───────────────────────┐
          ▼                       ▼                       ▼
Phase 3 (US1-P1)           Phase 4 (US2-P1)        Phase 5 (US3-P1)
Domain→Struct              Relationships            Manifest Pipeline
          │                       │                       │
          └───────────────────────┼───────────────────────┘
                                  ▼
          ┌───────────────────────┼───────────────────────┐
          ▼                       ▼                       ▼
Phase 6 (US4-P2)           Phase 7 (US5-P2)        Phase 8 (US6-P2)
Inference Rules            CONSTRUCT Compose        Validation
          │                       │                       │
          └───────────────────────┼───────────────────────┘
                                  ▼
                          Phase 9 (US7-P3)
                          Audit Trail
                                  │
                                  ▼
                          Phase 10 (Polish)
```

### Critical Path

1. **T009-T014** (Manifest Types) → **T015-T016** (Parser) → **T019-T021** (CONSTRUCT Executor)
2. **T024-T029** (Code Graph Types) → **T030-T034** (Template Integration)
3. **T047-T052** (Pipeline) → **T054-T057** (CLI)

### Parallel Opportunities

- **Phase 1**: All T001-T005 can run in parallel (different files)
- **Phase 2**: T009-T014 can run in parallel (type definitions in same file but independent)
- **Phase 3-5**: Can start in parallel once Phase 2 complete (US1, US2, US3 are independent at P1)
- **Phase 6-8**: Can start in parallel once Phase 5 complete (US4, US5, US6 are independent at P2)

---

## Implementation Strategy

### MVP First (US1 + US3 Only)

1. Complete Phase 1: Setup
2. Complete Phase 2: Foundational
3. Complete Phase 3: US1 (Domain → Struct)
4. Complete Phase 5: US3 (Manifest → CLI)
5. **STOP and VALIDATE**: `ggen generate` produces Rust structs from ontology
6. Deploy MVP

### Full Feature Set

1. MVP + Phase 4: US2 (Relationships)
2. Add Phase 6: US4 (Inference Rules)
3. Add Phase 7: US5 (CONSTRUCT Composition)
4. Add Phase 8: US6 (Validation)
5. Add Phase 9: US7 (Audit Trail)
6. Complete Phase 10: Polish

---

## Notes

- All tasks include exact file paths for LLM execution
- [P] tasks can run in parallel
- [US#] maps task to user story for traceability
- BTreeMap used everywhere for determinism (not HashMap)
- Named CONSTRUCT queries are the canonical rule language (NOT N3 rules)
- Leverage existing infrastructure: Graph, template.rs, code_ontology.ttl, GgenError
