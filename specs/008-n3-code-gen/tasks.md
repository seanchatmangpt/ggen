# Tasks: ggen v5 - Unified Sync Command

**Branch**: `008-n3-code-gen` | **Date**: 2024-12-14
**Input**: Design documents from `/specs/008-n3-code-gen/`
**Prerequisites**: plan.md ✓, spec.md ✓, research.md ✓, data-model.md ✓, contracts/ ✓

**Strategic Decision**: ggen v5 has ONE command: `ggen sync`. All other commands are removed.

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: Which user story this task belongs to (US1-US7)
- Exact file paths included for LLM execution

## Path Conventions

```
crates/ggen-core/src/          # Core library (manifest, codegen, graph extensions)
crates/ggen-core/tests/        # Integration tests
crates/ggen-cli/src/cmds/      # CLI commands (sync ONLY)
```

---

## Phase 1: Core Sync Command (THE ONLY COMMAND)

**Purpose**: Create `ggen sync` as the single command for ggen v5

### Create Sync CLI Module (T001-T004)

- [ ] T001 [P] Create `crates/ggen-cli/src/cmds/sync.rs` - `#[verb("sync", "ggen")]` with all CLI options per cli-contract.md
- [ ] T002 [P] Implement `SyncOutput` struct in `sync.rs` - Serializable output for JSON format
- [ ] T003 Register sync in `crates/ggen-cli/src/cmds/mod.rs` - Add `pub mod sync;`
- [ ] T004 Implement CLI argument parsing in `sync.rs` - `--manifest`, `--output-dir`, `--dry-run`, `--verbose`, `--watch`, `--rule`, `--force`, `--audit`, `--validate-only`, `--format`, `--timeout`

### Implement SyncOptions (T005-T006)

- [ ] T005 [P] Add `SyncOptions` type to `crates/ggen-core/src/codegen/mod.rs` - Configuration struct for pipeline per data-model.md
- [ ] T006 [P] Add `OutputFormat` enum to `crates/ggen-core/src/codegen/mod.rs` - Text, Json variants

**Checkpoint**: `ggen sync --help` works, shows all options

---

## Phase 2: Pipeline Implementation

**Purpose**: Complete the sync pipeline: ontology → CONSTRUCT → SELECT → Template → Code

### CONSTRUCT Execution (T007-T010)

- [ ] T007 [US1] Implement `GenerationPipeline::execute_inference_rule()` in `crates/ggen-core/src/codegen/pipeline.rs` - Execute single CONSTRUCT rule, materialize triples
- [ ] T008 [US1] Implement `GenerationPipeline::execute_inference_rules()` in `crates/ggen-core/src/codegen/pipeline.rs` - Execute all rules in order
- [ ] T009 [US1] Add timeout handling for inference rules - Use `max_reasoning_timeout_ms` from config
- [ ] T010 [US1] Add test: CONSTRUCT query adds triples to graph - in `crates/ggen-core/tests/codegen/pipeline_tests.rs`

### SELECT + Template Generation (T011-T015)

- [ ] T011 [US1] Implement `GenerationPipeline::execute_generation_rule()` in `crates/ggen-core/src/codegen/pipeline.rs` - Execute SELECT, render template, return content
- [ ] T012 [US1] Integrate `Template::process_graph()` - Pass SPARQL row bindings as template context
- [ ] T013 [US1] Implement output file path expansion - Replace `{{name}}` etc. with SPARQL result values
- [ ] T014 [US1] Add test: SELECT query results populate template context - in `crates/ggen-core/tests/codegen/pipeline_tests.rs`
- [ ] T015 [US1] Add test: Output path expansion with variables - in `crates/ggen-core/tests/codegen/pipeline_tests.rs`

### File Writing (T016-T019)

- [ ] T016 [US3] Implement file writing with `GenerationMode` - Handle Create/Overwrite/Merge modes
- [ ] T017 [US3] Respect `--force` flag - Overwrite protected files when enabled
- [ ] T018 [US3] Create parent directories automatically - `std::fs::create_dir_all()`
- [ ] T019 [US3] Add test: File writing creates correct directory structure - in `crates/ggen-core/tests/codegen/pipeline_tests.rs`

### Full Pipeline Run (T020-T023)

- [ ] T020 [US3] Implement `GenerationPipeline::run()` - Full pipeline: load → inference → generation → validation
- [ ] T021 [US3] Return `PipelineState` with all execution details
- [ ] T022 [US3] Add test: End-to-end sync with sample ggen.toml - in `crates/ggen-core/tests/codegen/sync_integration.rs`
- [ ] T023 Run `cargo make check` - Verify Phase 2 compiles

**Checkpoint**: `ggen sync` generates code from ggen.toml

---

## Phase 3: Sync Features

**Purpose**: Implement all sync command flags and features

### Dry Run Mode (T024-T026)

- [ ] T024 [US3] Implement `--dry-run` in pipeline - Preview files without writing
- [ ] T025 [US3] Show "would create", "would overwrite" status per file
- [ ] T026 [US3] Add test: Dry run doesn't write files - in `crates/ggen-cli/tests/sync_tests.rs`

### Validate Only Mode (T027-T029)

- [ ] T027 [US6] Implement `--validate-only` - Validate manifest and ontology without generating
- [ ] T028 [US6] Check manifest schema, ontology syntax, SPARQL queries, templates
- [ ] T029 [US6] Add test: Validate-only exits without generating - in `crates/ggen-cli/tests/sync_tests.rs`

### Verbose Output (T030-T032)

- [ ] T030 [US3] Implement `--verbose` output - Show detailed progress
- [ ] T031 [US3] Log: manifest load, ontology triples, inference rules, generation rules, file writes
- [ ] T032 [US3] Add test: Verbose output includes expected sections - in `crates/ggen-cli/tests/sync_tests.rs`

### JSON Output Format (T033-T035)

- [ ] T033 [US3] Implement `--format json` - Output SyncOutput as JSON
- [ ] T034 [US3] Include: status, files_synced, duration_ms, files array, rules counts
- [ ] T035 [US3] Add test: JSON output is valid and parseable - in `crates/ggen-cli/tests/sync_tests.rs`

### Watch Mode (T036-T038)

- [ ] T036 [US3] Implement `--watch` - Watch for file changes and regenerate
- [ ] T037 [US3] Watch: ggen.toml, ontology files, template files
- [ ] T038 [US3] Add test: Watch triggers on file change - in `crates/ggen-cli/tests/sync_tests.rs`

### Rule Selection (T039-T041)

- [ ] T039 [US3] Implement `--rule <NAME>` - Run specific rule(s) only
- [ ] T040 [US3] Support multiple `--rule` flags for multiple rules
- [ ] T041 [US3] Add test: Rule selection runs only specified rules - in `crates/ggen-cli/tests/sync_tests.rs`

### Audit Trail (T042-T046)

- [ ] T042 [US7] Implement `--audit` - Generate audit.json
- [ ] T043 [US7] Record input hashes: manifest, ontology, templates, queries
- [ ] T044 [US7] Record pipeline steps: step_type, name, duration_ms, triples_added, status
- [ ] T045 [US7] Record output hashes: path, content_hash, size_bytes, source_rule
- [ ] T046 [US7] Add test: Audit trail is deterministic (same inputs = same hashes) - in `crates/ggen-core/tests/codegen/audit_tests.rs`

**Checkpoint**: All sync features work

---

## Phase 4: Remove ALL Old Commands (Fresh Start)

**Purpose**: Delete all legacy commands, leaving only `ggen sync`

### Delete Command Modules (T047-T057)

- [ ] T047 [P] Delete `crates/ggen-cli/src/cmds/generate.rs` - Replaced by sync
- [ ] T048 [P] Delete `crates/ggen-cli/src/cmds/template.rs` - Replaced by sync
- [ ] T049 [P] Delete `crates/ggen-cli/src/cmds/project.rs` - Add back in v5.1+
- [ ] T050 [P] Delete `crates/ggen-cli/src/cmds/graph.rs` - Add back in v5.1+
- [ ] T051 [P] Delete `crates/ggen-cli/src/cmds/ontology.rs` - Add back in v5.1+
- [ ] T052 [P] Delete `crates/ggen-cli/src/cmds/marketplace.rs` - Add back in v5.1+
- [ ] T053 [P] Delete `crates/ggen-cli/src/cmds/ai.rs` - Add back in v5.1+
- [ ] T054 [P] Delete `crates/ggen-cli/src/cmds/test.rs` - Add back in v5.1+ (if feature enabled)
- [ ] T055 [P] Delete `crates/ggen-cli/src/cmds/utils.rs` - Add back in v5.1+
- [ ] T056 [P] Delete `crates/ggen-cli/src/cmds/ci.rs` - Add back in v5.1+
- [ ] T057 [P] Delete `crates/ggen-cli/src/cmds/workflow.rs` - Add back in v5.1+

### Rewrite Command Router (T058-T060)

- [ ] T058 Rewrite `crates/ggen-cli/src/cmds/mod.rs` - Only register `sync` and `helpers` modules
- [ ] T059 Remove all dead module declarations from `mod.rs`
- [ ] T060 Update binary entry point `crates/ggen-cli/src/main.rs` if needed

### Verify Clean CLI (T061-T063)

- [ ] T061 Run `cargo make check` - Ensure clean compilation
- [ ] T062 Run `ggen --help` - Verify only `sync` command shown
- [ ] T063 Run `ggen generate` - Verify returns "unknown command" error

**Checkpoint**: `ggen sync` is THE ONLY COMMAND

---

## Phase 5: Validation & Poka-Yoke (US6)

**Purpose**: Semantic validation before code generation

### Validation Pipeline (T064-T069)

- [ ] T064 [US6] Implement `ValidationExecutor::validate_code_graph()` - Run built-in SPARQL ASK validations
- [ ] T065 [US6] Implement `ValidationExecutor::validate_custom_rules()` - Run user-defined ASK queries from ggen.toml
- [ ] T066 [US6] Implement `ValidationExecutor::validate_syntax()` - Parse generated Rust with syn crate
- [ ] T067 [US6] Implement `ValidationExecutor::check_no_unsafe()` - Scan for `unsafe` blocks if `no_unsafe = true`
- [ ] T068 [US6] Add test: Missing fieldType fails validation - in `crates/ggen-core/tests/codegen/validation_tests.rs`
- [ ] T069 [US6] Add test: Unsafe code detected when no_unsafe=true - in `crates/ggen-core/tests/codegen/validation_tests.rs`

### Error Messages (T070-T072)

- [ ] T070 [P] [US6] Implement structured error format per cli-contract.md - `error[ECODE]: description`
- [ ] T071 [P] [US6] Add error code mapping: E0001=Manifest, E0002=Ontology, E0003=SPARQL, E0004=Template, E0005=IO, E0006=Timeout
- [ ] T072 [US6] Add test: Error messages identify exact IRI/property for validation failures - in `crates/ggen-core/tests/codegen/validation_tests.rs`

**Checkpoint**: Validation prevents invalid code generation

---

## Phase 6: User Story Acceptance Tests

**Purpose**: Verify all user stories work end-to-end

### US1: Domain Model to Rust Code (T073-T075)

- [ ] T073 [US1] Add test: Given `:User a rdfs:Class` → output contains `pub struct User { ... }` - in `crates/ggen-core/tests/codegen/us1_acceptance.rs`
- [ ] T074 [US1] Add test: Given struct with `Uuid` field → `Serialize, Deserialize` derives added - in `crates/ggen-core/tests/codegen/us1_acceptance.rs`
- [ ] T075 [US1] Add test: Given `:auditable true` → `created_at`, `updated_at` fields added - in `crates/ggen-core/tests/codegen/us1_acceptance.rs`

### US2: Relationship-Driven Impl Generation (T076-T078)

- [ ] T076 [US2] Add test: Given `:Entity1 :has_many :Entity2` → `get_entity2s(&self)` method generated - in `crates/ggen-core/tests/codegen/us2_acceptance.rs`
- [ ] T077 [US2] Add test: Given `:Entity :soft_delete true` → `deleted_at` field inferred - in `crates/ggen-core/tests/codegen/us2_acceptance.rs`
- [ ] T078 [US2] Add test: Relationship impl has correct return type - in `crates/ggen-core/tests/codegen/us2_acceptance.rs`

### US3: Manifest-Driven Sync (T079-T081)

- [ ] T079 [US3] Add test: Given ggen.toml with `[ontology]` and `[[generation.rules]]` → files generated - in `crates/ggen-core/tests/codegen/us3_acceptance.rs`
- [ ] T080 [US3] Add test: `ggen sync --dry-run` shows preview without writing - in `crates/ggen-cli/tests/us3_acceptance.rs`
- [ ] T081 [US3] Add test: Output directory override works - in `crates/ggen-cli/tests/us3_acceptance.rs`

### US4: Inference Rules (T082-T084)

- [ ] T082 [US4] Add test: Custom inference rule in `[[inference.rules]]` executes - in `crates/ggen-core/tests/codegen/us4_acceptance.rs`
- [ ] T083 [US4] Add test: Inference rules execute in order - in `crates/ggen-core/tests/codegen/us4_acceptance.rs`
- [ ] T084 [US4] Add test: `when` condition skips rule when false - in `crates/ggen-core/tests/codegen/us4_acceptance.rs`

### US5: CONSTRUCT Composition (T085-T087)

- [ ] T085 [US5] Add test: Multiple CONSTRUCT queries enrich graph progressively - in `crates/ggen-core/tests/codegen/us5_acceptance.rs`
- [ ] T086 [US5] Add test: Later CONSTRUCT can query earlier CONSTRUCT results - in `crates/ggen-core/tests/codegen/us5_acceptance.rs`
- [ ] T087 [US5] Add test: BIND(IRI(CONCAT(...))) generates unique IRIs - in `crates/ggen-core/tests/codegen/us5_acceptance.rs`

### US6: Poka-Yoke Validation (T088-T090)

- [ ] T088 [US6] Add test: Field without type fails validation with exit code 1 - in `crates/ggen-cli/tests/us6_acceptance.rs`
- [ ] T089 [US6] Add test: Generated code with `unsafe` fails when `no_unsafe = true` - in `crates/ggen-cli/tests/us6_acceptance.rs`
- [ ] T090 [US6] Add test: SHACL validation failure produces clear error - in `crates/ggen-cli/tests/us6_acceptance.rs`

### US7: Audit Trail (T091-T093)

- [ ] T091 [US7] Add test: `ggen sync --audit` creates audit.json - in `crates/ggen-cli/tests/us7_acceptance.rs`
- [ ] T092 [US7] Add test: Same inputs produce identical audit.json (except timestamp) - in `crates/ggen-cli/tests/us7_acceptance.rs`
- [ ] T093 [US7] Add test: Audit trail content_hash matches generated file hash - in `crates/ggen-cli/tests/us7_acceptance.rs`

**Checkpoint**: All user stories have passing acceptance tests

---

## Phase 7: Polish & Performance

**Purpose**: Documentation, performance validation, final cleanup

### Documentation (T094-T096)

- [ ] T094 [P] Update `crates/ggen-core/src/rdf/code_ontology.ttl` - Add any missing code: vocabulary terms
- [ ] T095 [P] Create example project in `examples/008-sync-demo/` - Working ggen.toml + ontology + templates
- [ ] T096 Run quickstart.md validation - Verify all steps work end-to-end

### Performance Validation (T097-T099)

- [ ] T097 Add benchmark: 50 entities sync in <5s (SC-001) - in `crates/ggen-core/benches/sync_bench.rs`
- [ ] T098 Add benchmark: 500 entities sync in <30s (SC-008) - in `crates/ggen-core/benches/sync_bench.rs`
- [ ] T099 Profile and optimize if benchmarks fail

### Determinism Verification (T100-T101)

- [ ] T100 Add test: Sync on different runs produces byte-identical output (SC-002) - in `crates/ggen-core/tests/codegen/determinism_tests.rs`
- [ ] T101 Verify all maps use BTreeMap (grep check for HashMap in sync-related code)

### Final Integration (T102-T104)

- [ ] T102 Run `cargo make test` - All tests pass
- [ ] T103 Run `cargo make lint` - No clippy warnings
- [ ] T104 Run `cargo make check` - Compilation clean

**Checkpoint**: ggen v5 release ready

---

## Dependencies & Execution Order

### Phase Dependencies

```
Phase 1 (Core Sync) ────────┐
                            ▼
Phase 2 (Pipeline) ─────────► BLOCKS ALL FEATURES
                            │
          ┌─────────────────┼─────────────────┐
          ▼                 ▼                 ▼
Phase 3 (Features)   Phase 4 (Remove)  Phase 5 (Validation)
          │                 │                 │
          └─────────────────┼─────────────────┘
                            ▼
                    Phase 6 (Acceptance)
                            │
                            ▼
                    Phase 7 (Polish)
```

### Critical Path

1. **T001-T004** (Sync CLI) → **T005-T006** (SyncOptions)
2. **T007-T023** (Pipeline) - Core sync functionality
3. **T047-T063** (Remove Commands) - Fresh start
4. **T073-T093** (Acceptance Tests) - User story validation

### Parallel Opportunities

- **Phase 1**: T001, T002 can run in parallel (different struct definitions)
- **Phase 4**: T047-T057 can ALL run in parallel (independent file deletions)
- **Phase 6**: All US* acceptance tests can run in parallel (independent test files)

---

## Implementation Strategy

### MVP First (Phases 1-2)

1. Complete Phase 1: Core Sync Command
2. Complete Phase 2: Pipeline Implementation
3. **STOP and VALIDATE**: `ggen sync` generates code from ggen.toml
4. Deploy MVP

### Full Feature Set

1. MVP + Phase 3: Sync Features
2. Add Phase 4: Remove ALL Old Commands (BREAKING CHANGE)
3. Add Phase 5: Validation & Poka-Yoke
4. Add Phase 6: Acceptance Tests
5. Complete Phase 7: Polish & Performance

---

## Success Criteria

| Criterion | Measure | Target |
|-----------|---------|--------|
| SC-001 | Sync time for 10-50 entities | <5s |
| SC-002 | Output determinism | 100% byte-identical |
| SC-003 | Annotation reduction via inference | 60% |
| SC-004 | Generated code syntax errors | 0 |
| SC-005 | Audit trail enables verification | Pass |
| SC-008 | Sync time for 500+ entities | <30s |
| SC-010 | `ggen sync` is the ONLY command | TRUE |

---

## Notes

- All tasks include exact file paths for LLM execution
- [P] tasks can run in parallel
- [US#] maps task to user story for traceability
- BTreeMap used everywhere for determinism (not HashMap)
- `ggen sync` is THE ONLY COMMAND for ggen v5
- All other commands removed; can be added back incrementally in v5.1+
