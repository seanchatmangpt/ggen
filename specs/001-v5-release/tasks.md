# Task Breakdown: ggen v5.0.0 Release

**Feature Branch**: `001-v5-release`
**Date**: 2025-12-17
**Status**: Ready for Implementation

## Overview

This task breakdown organizes work by user story priority (P1 → P2 → P3) with explicit dependencies and parallel execution opportunities. Tasks follow Chicago TDD methodology: write failing test (RED) → implement (GREEN) → refactor.

**Total Estimated Tasks**: 47 tasks
**Implementation Time**: 2-3 weeks (not months - 85% already complete)

---

## Task Format

```
- [ ] [TASK-NNN] [Priority] [User Story Reference] Description
  **Files**: Specific file paths to create/modify
  **Dependencies**: Prerequisite task IDs
  **Acceptance**: How to verify completion
  **Time**: Estimated effort
```

---

## Phase 1: Setup & Foundation (Blocking Prerequisites)

**Goal**: Establish test infrastructure and synthetic data generators before implementing user stories.

### Setup Tasks

- [ ] [TASK-001] [P0] [Foundation] Create ggen-testdata crate for synthetic data generators
  **Files**: `crates/ggen-testdata/Cargo.toml`, `crates/ggen-testdata/src/lib.rs`
  **Dependencies**: None (blocking prerequisite)
  **Acceptance**: Crate compiles with `cargo make check`, exports OntologyGenerator/TemplateGenerator/WorkspaceGenerator
  **Time**: 30 minutes

- [ ] [TASK-002] [P0] [Foundation] Implement OntologyGenerator for test data (10-50K triples)
  **Files**: `crates/ggen-testdata/src/generators/ontology.rs`, `crates/ggen-testdata/tests/ontology_generator_test.rs`
  **Dependencies**: TASK-001
  **Acceptance**: Can generate ontologies with configurable triple counts (10, 100, 1K, 10K, 50K), deterministic output
  **Time**: 2 hours

- [ ] [TASK-003] [P0] [Foundation] Implement TemplateGenerator for test templates (simple/medium/complex)
  **Files**: `crates/ggen-testdata/src/generators/template.rs`, `crates/ggen-testdata/tests/template_generator_test.rs`
  **Dependencies**: TASK-001
  **Acceptance**: Generates Tera templates with 3 complexity levels, includes SPARQL query references
  **Time**: 1.5 hours

- [ ] [TASK-004] [P0] [Foundation] Implement WorkspaceGenerator for multi-crate test workspaces
  **Files**: `crates/ggen-testdata/src/generators/workspace.rs`, `crates/ggen-testdata/tests/workspace_generator_test.rs`
  **Dependencies**: TASK-001
  **Acceptance**: Generates workspaces with 1-10 crates, dependency relationships, ggen.toml per crate
  **Time**: 2 hours

---

## Phase 2: User Story 1 - Unified Sync Command (P1)

**Goal**: Complete integration tests and workspace sync for the already-implemented core sync command.

### Integration Testing

- [ ] [TASK-005] [P1] [Story 1] Create integration test: Basic full sync (1K triples)
  **Files**: `crates/ggen-core/tests/sync_pipeline_test.rs` (create), `crates/ggen-core/tests/fixtures/basic_ontology.ttl`
  **Dependencies**: TASK-002, TASK-003
  **Acceptance**: Test RED (fails), verifies SyncExecutor::execute_full_sync loads ontology → executes inference → renders template → writes file
  **Time**: 1 hour

- [ ] [TASK-006] [P1] [Story 1] Implement: Fix failing basic sync test (GREEN)
  **Files**: `crates/ggen-core/src/codegen/executor.rs` (review/fix if needed)
  **Dependencies**: TASK-005
  **Acceptance**: Test GREEN (passes), generated file exists with expected content hash
  **Time**: 30 minutes

- [ ] [TASK-007] [P1] [Story 1] Create integration test: Multi-rule SPARQL pipeline
  **Files**: `crates/ggen-core/tests/sync_pipeline_test.rs` (add test), `crates/ggen-core/tests/fixtures/multi_rule.ttl`
  **Dependencies**: TASK-006
  **Acceptance**: Test RED, verifies multiple CONSTRUCT rules execute in order, multiple SELECT queries provide template context
  **Time**: 1 hour

- [ ] [TASK-008] [P1] [Story 1] Implement: Fix multi-rule pipeline test (GREEN)
  **Files**: `crates/ggen-core/src/codegen/pipeline.rs` (review CONSTRUCT ordering)
  **Dependencies**: TASK-007
  **Acceptance**: Test GREEN, all CONSTRUCT queries materialize triples correctly, SELECT results merged
  **Time**: 45 minutes

- [ ] [TASK-009] [P1] [Story 1] Create integration test: Verify mode (no file writes)
  **Files**: `crates/ggen-core/tests/sync_pipeline_test.rs` (add test)
  **Dependencies**: TASK-008
  **Acceptance**: Test RED, verifies SyncMode::Verify validates consistency without modifying files
  **Time**: 45 minutes

- [ ] [TASK-010] [P1] [Story 1] Implement: Fix verify mode test (GREEN)
  **Files**: `crates/ggen-core/src/codegen/executor.rs` (verify mode logic)
  **Dependencies**: TASK-009
  **Acceptance**: Test GREEN, no files written, consistency report generated
  **Time**: 1 hour

### Workspace Sync

- [ ] [TASK-011] [P1] [Story 1] Create integration test: Workspace-wide sync (sequential)
  **Files**: `tests/sync_end_to_end_test.rs` (create), `tests/fixtures/multi_crate_workspace/` (via WorkspaceGenerator)
  **Dependencies**: TASK-004, TASK-010
  **Acceptance**: Test RED, verifies workspace discovery parses Cargo.toml, syncs all crates with ggen.toml
  **Time**: 1.5 hours

- [ ] [TASK-012] [P1] [Story 1] Implement: Workspace discovery (Cargo.toml parsing)
  **Files**: `crates/ggen-core/src/workspace/discovery.rs` (create), `crates/ggen-core/src/workspace/mod.rs`
  **Dependencies**: TASK-011
  **Acceptance**: Test GREEN, parses workspace.members, expands glob patterns, returns Vec<PathBuf> of crate paths
  **Time**: 2 hours

- [ ] [TASK-013] [P1] [Story 1] Implement: Sequential workspace sync
  **Files**: `crates/ggen-core/src/codegen/executor.rs` (add workspace sync logic)
  **Dependencies**: TASK-012
  **Acceptance**: Test GREEN, syncs all crates in order, aggregates SyncOutput from all members
  **Time**: 1.5 hours

### CLI Integration

- [ ] [TASK-014] [P1] [Story 1] Create CLI integration test: ggen sync --mode full
  **Files**: `crates/ggen-cli/tests/sync_cli_test.rs` (create)
  **Dependencies**: TASK-013
  **Acceptance**: Test RED, uses assert_cmd to verify CLI exits 0, generates expected files
  **Time**: 1 hour

- [ ] [TASK-015] [P1] [Story 1] Implement: Fix CLI integration test (GREEN)
  **Files**: `crates/ggen-cli/src/cmds/sync.rs` (review, should already work)
  **Dependencies**: TASK-014
  **Acceptance**: Test GREEN, CLI invocation matches integration test expectations
  **Time**: 30 minutes

- [ ] [TASK-016] [P1] [Story 1] Create CLI integration test: ggen sync --dry-run
  **Files**: `crates/ggen-cli/tests/sync_cli_test.rs` (add test)
  **Dependencies**: TASK-015
  **Acceptance**: Test RED, verifies --dry-run previews changes without writing files
  **Time**: 45 minutes

- [ ] [TASK-017] [P1] [Story 1] Implement: Dry-run mode
  **Files**: `crates/ggen-core/src/codegen/executor.rs` (add dry_run branch), `crates/ggen-core/src/codegen/pipeline.rs`
  **Dependencies**: TASK-016
  **Acceptance**: Test GREEN, dry_run=true skips file writes, reports what would change
  **Time**: 1 hour

---

## Phase 3: User Story 2 - SPARQL Pipeline (P1)

**Goal**: Verify and enhance the already-implemented SPARQL CONSTRUCT/SELECT pipeline.

### Pipeline Testing

- [ ] [TASK-018] [P1] [Story 2] Create test: CONSTRUCT query materializes triples
  **Files**: `crates/ggen-core/tests/sparql_pipeline_test.rs` (create)
  **Dependencies**: TASK-002
  **Acceptance**: Test RED, verifies InferenceExecutor executes CONSTRUCT, adds triples to Oxigraph Store
  **Time**: 1 hour

- [ ] [TASK-019] [P1] [Story 2] Implement: Fix CONSTRUCT materialization test (GREEN)
  **Files**: `crates/ggen-domain/src/packs/sparql_executor.rs` (review existing implementation)
  **Dependencies**: TASK-018
  **Acceptance**: Test GREEN, CONSTRUCT query results inserted into graph
  **Time**: 30 minutes

- [ ] [TASK-020] [P1] [Story 2] Create test: SELECT query extracts JSON context
  **Files**: `crates/ggen-core/tests/sparql_pipeline_test.rs` (add test)
  **Dependencies**: TASK-019
  **Acceptance**: Test RED, verifies SELECT execution converts QuerySolution to serde_json::Value
  **Time**: 1 hour

- [ ] [TASK-021] [P1] [Story 2] Implement: Fix SELECT extraction test (GREEN)
  **Files**: `crates/ggen-core/src/codegen/pipeline.rs` (extract_data stage)
  **Dependencies**: TASK-020
  **Acceptance**: Test GREEN, SELECT results available as JSON for templates
  **Time**: 1 hour

- [ ] [TASK-022] [P1] [Story 2] Create test: Template rendering with SPARQL variables
  **Files**: `crates/ggen-core/tests/template_pipeline_test.rs` (create)
  **Dependencies**: TASK-003, TASK-021
  **Acceptance**: Test RED, verifies Tera renders template using SELECT query results
  **Time**: 1 hour

- [ ] [TASK-023] [P1] [Story 2] Implement: Fix template rendering test (GREEN)
  **Files**: `crates/ggen-core/src/templates/generator.rs` (review Tera integration)
  **Dependencies**: TASK-022
  **Acceptance**: Test GREEN, template variables populated from SPARQL, output correct
  **Time**: 45 minutes

### Error Handling

- [ ] [TASK-024] [P1] [Story 2] Create test: Invalid SPARQL syntax error
  **Files**: `crates/ggen-core/tests/error_handling_test.rs` (create)
  **Dependencies**: TASK-023
  **Acceptance**: Test RED, verifies malformed SPARQL query returns SparqlError with line number
  **Time**: 1 hour

- [ ] [TASK-025] [P1] [Story 2] Implement: Enhanced SPARQL error context
  **Files**: `crates/ggen-core/src/error.rs` (add query context to SparqlError), `crates/ggen-core/src/codegen/pipeline.rs`
  **Dependencies**: TASK-024
  **Acceptance**: Test GREEN, SPARQL errors include query snippet, line/column numbers
  **Time**: 1.5 hours

- [ ] [TASK-026] [P1] [Story 2] Create test: Circular CONSTRUCT dependency detection
  **Files**: `crates/ggen-core/tests/error_handling_test.rs` (add test)
  **Dependencies**: TASK-025
  **Acceptance**: Test RED, verifies cycle detection in CONSTRUCT query execution order
  **Time**: 1 hour

- [ ] [TASK-027] [P1] [Story 2] Implement: Dependency cycle detection
  **Files**: `crates/ggen-core/src/codegen/pipeline.rs` (add topological sort with cycle check)
  **Dependencies**: TASK-026
  **Acceptance**: Test GREEN, detects cycles, reports dependency chain
  **Time**: 2 hours

---

## Phase 4: Performance Benchmarks (P0)

**Goal**: Validate all success criteria with comprehensive benchmark suite.

### Baseline Benchmarks

- [ ] [TASK-028] [P0] [SC-002] Create benchmark: Baseline scaling (10-50K triples)
  **Files**: `benches/sync_scaling.rs` (create), `benches/support/mod.rs`
  **Dependencies**: TASK-002, TASK-003
  **Acceptance**: Criterion benchmark suite with scenarios: 10, 100, 1K, 10K, 50K triples
  **Time**: 2 hours

- [ ] [TASK-029] [P0] [SC-002] Validate SLO: Full sync 1K triples <5s
  **Files**: `scripts/validate_slos.sh` (create)
  **Dependencies**: TASK-028
  **Acceptance**: Script runs baseline_scaling/1000 benchmark, asserts mean <5s, exits 0 if GREEN, 1 if RED
  **Time**: 1 hour

- [ ] [TASK-030] [P0] [SC-010] Validate SLO: 50K triples completes without timeout
  **Files**: `scripts/validate_slos.sh` (extend)
  **Dependencies**: TASK-029
  **Acceptance**: baseline_scaling/50000 completes successfully, no timeout errors
  **Time**: 30 minutes

### Verify Mode Benchmarks

- [ ] [TASK-031] [P0] [SC-007] Create benchmark: Verify mode performance
  **Files**: `benches/verify_mode.rs` (create)
  **Dependencies**: TASK-010, TASK-028
  **Acceptance**: Criterion benchmark for SyncMode::Verify with 1K triples
  **Time**: 1 hour

- [ ] [TASK-032] [P0] [SC-007] Validate SLO: Verify 1K triples <2s
  **Files**: `scripts/validate_slos.sh` (extend)
  **Dependencies**: TASK-031
  **Acceptance**: verify_mode/verify_consistent_state mean <2s, GREEN signal
  **Time**: 30 minutes

### Workspace Benchmarks

- [ ] [TASK-033] [P0] [SC-006] Create benchmark: Workspace scaling (1-10 crates)
  **Files**: `benches/workspace_scaling.rs` (create)
  **Dependencies**: TASK-004, TASK-013
  **Acceptance**: Benchmarks with 1, 5, 10 crate workspaces
  **Time**: 1.5 hours

### Andon Signal Integration

- [ ] [TASK-034] [P0] [Constitution] Implement Andon signals in benchmark validation
  **Files**: `scripts/validate_slos.sh` (add RED/YELLOW/GREEN logic)
  **Dependencies**: TASK-030, TASK-032
  **Acceptance**: Script outputs 🔴 RED (>SLO), 🟡 YELLOW (90-100% SLO), 🟢 GREEN (<90% SLO) for each benchmark
  **Time**: 1 hour

- [ ] [TASK-035] [P0] [Constitution] Create benchmark comparison script
  **Files**: `scripts/compare_benchmarks.sh` (create)
  **Dependencies**: TASK-034
  **Acceptance**: Script compares two criterion JSON outputs, detects regressions, exits 1 if RED
  **Time**: 1.5 hours

---

## Phase 5: User Story 3 - Migration Guide (P2)

**Goal**: Document v4 to v5 migration path with command equivalents.

### Documentation

- [ ] [TASK-036] [P2] [Story 3] Create migration guide structure
  **Files**: `docs/v4-to-v5-migration.md` (create)
  **Dependencies**: TASK-017 (CLI tests validate equivalents)
  **Acceptance**: Guide has sections: Overview, Command Mapping, Config Migration, Breaking Changes, FAQ
  **Time**: 1 hour

- [ ] [TASK-037] [P2] [Story 3] Document v4 → v5 command equivalents (47 verbs)
  **Files**: `docs/v4-to-v5-migration.md` (fill command mapping table)
  **Dependencies**: TASK-036, use docs/v4-to-v5-sync-analysis.md as input
  **Acceptance**: Table maps all 47 v4 verbs to v5 sync equivalents or "Removed" status
  **Time**: 2 hours

- [ ] [TASK-038] [P2] [Story 3] Document ggen.toml v4 → v5 migration
  **Files**: `docs/v4-to-v5-migration.md` (add config section)
  **Dependencies**: TASK-037
  **Acceptance**: Documents schema changes, provides before/after examples, explains backward compatibility
  **Time**: 1.5 hours

- [ ] [TASK-039] [P2] [Story 3] Document CI/CD pipeline migration
  **Files**: `docs/v4-to-v5-migration.md` (add CI section)
  **Dependencies**: TASK-038
  **Acceptance**: Provides GitHub Actions workflow examples for v4 vs v5
  **Time**: 1 hour

---

## Phase 6: User Story 4 - Error Handling (P2)

**Goal**: Comprehensive error diagnostics with actionable messages.

### Enhanced Error Messages

- [ ] [TASK-040] [P2] [Story 4] Create test: RDF syntax error with line numbers
  **Files**: `crates/ggen-core/tests/error_diagnostics_test.rs` (create)
  **Dependencies**: TASK-025
  **Acceptance**: Test RED, verifies OntologyLoadError includes exact line/column for malformed Turtle
  **Time**: 1 hour

- [ ] [TASK-041] [P2] [Story 4] Implement: RDF parse error enhancement
  **Files**: `crates/ggen-core/src/rdf/loader.rs` (enhance error context from Oxigraph)
  **Dependencies**: TASK-040
  **Acceptance**: Test GREEN, errors show line number, character position, suggested fix
  **Time**: 2 hours

- [ ] [TASK-042] [P2] [Story 4] Create test: Template undefined variable error
  **Files**: `crates/ggen-core/tests/error_diagnostics_test.rs` (add test)
  **Dependencies**: TASK-041
  **Acceptance**: Test RED, verifies TemplateError lists all missing variables, suggests SPARQL query
  **Time**: 1 hour

- [ ] [TASK-043] [P2] [Story 4] Implement: Template variable diagnostics
  **Files**: `crates/ggen-core/src/templates/generator.rs` (catch Tera rendering errors, enhance context)
  **Dependencies**: TASK-042
  **Acceptance**: Test GREEN, errors list required vs provided variables
  **Time**: 1.5 hours

---

## Phase 7: User Story 5 - Incremental Sync (P3)

**Goal**: Deferred to v5.1.0 - only design documentation in v5.0.0.

### Design Documentation

- [ ] [TASK-044] [P3] [Story 5] Document incremental sync design
  **Files**: `docs/incremental-sync-design.md` (create)
  **Dependencies**: None (design only)
  **Acceptance**: Documents hybrid change detection (content hash + timestamp), region preservation (`// GGEN_MANUAL`)
  **Time**: 2 hours

- [ ] [TASK-045] [P3] [Story 5] Create placeholder for .ggen/sync-state.json schema
  **Files**: `docs/incremental-sync-design.md` (add schema section)
  **Dependencies**: TASK-044
  **Acceptance**: Defines ArtifactMetadata structure (path, content_hash, source_hash, dependencies, last_generated)
  **Time**: 1 hour

---

## Phase 8: Polish & Cross-Cutting

**Goal**: Final validation and release preparation.

### Final Validation

- [ ] [TASK-046] [P0] [Release] Run full CI pipeline validation
  **Files**: `.github/workflows/ci.yml` (ensure runs all benchmarks)
  **Dependencies**: TASK-035, TASK-043
  **Acceptance**: `cargo make ci` passes, all 1,168+ tests GREEN, benchmarks meet SLOs
  **Time**: 30 minutes

- [ ] [TASK-047] [P0] [Release] Update CHANGELOG.md and README.md for v5.0.0
  **Files**: `CHANGELOG.md`, `README.md`
  **Dependencies**: TASK-039, TASK-046
  **Acceptance**: Changelog documents breaking changes, new features. README shows v5 sync examples.
  **Time**: 1 hour

---

## Dependency Graph

### Critical Path (Blocking Dependencies)

```
Phase 1: Setup
  TASK-001 → TASK-002, TASK-003, TASK-004

Phase 2: User Story 1 (P1)
  TASK-002, TASK-003 → TASK-005 → TASK-006 → TASK-007 → TASK-008 → TASK-009 → TASK-010
  TASK-004, TASK-010 → TASK-011 → TASK-012 → TASK-013 → TASK-014 → TASK-015 → TASK-016 → TASK-017

Phase 3: User Story 2 (P1)
  TASK-002 → TASK-018 → TASK-019 → TASK-020 → TASK-021
  TASK-003, TASK-021 → TASK-022 → TASK-023 → TASK-024 → TASK-025 → TASK-026 → TASK-027

Phase 4: Benchmarks (P0)
  TASK-002, TASK-003 → TASK-028 → TASK-029 → TASK-030
  TASK-010, TASK-028 → TASK-031 → TASK-032
  TASK-004, TASK-013 → TASK-033
  TASK-030, TASK-032 → TASK-034 → TASK-035

Phase 5: Migration (P2)
  TASK-017 → TASK-036 → TASK-037 → TASK-038 → TASK-039

Phase 6: Error Handling (P2)
  TASK-025 → TASK-040 → TASK-041 → TASK-042 → TASK-043

Phase 7: Incremental Design (P3)
  None → TASK-044 → TASK-045

Phase 8: Release
  TASK-035, TASK-043 → TASK-046 → TASK-047
```

### Parallel Execution Opportunities

**After TASK-017 (Phase 2 complete), can parallelize:**
- Phase 3 (TASK-018 to TASK-027) - SPARQL pipeline testing
- Phase 4 (TASK-028 to TASK-035) - Benchmark suite
- Phase 5 (TASK-036 to TASK-039) - Migration guide

**After TASK-004 (WorkspaceGenerator ready):**
- TASK-011 to TASK-013 (workspace sync) can run in parallel with other Phase 2 tasks

**After TASK-028 (baseline benchmarks created):**
- TASK-031 (verify benchmarks) can run in parallel with TASK-029/TASK-030

---

## Task Summary

### By Priority

- **P0 (Blocking)**: 14 tasks (TASK-001 to TASK-004, TASK-028 to TASK-035, TASK-046, TASK-047)
- **P1 (Critical)**: 23 tasks (TASK-005 to TASK-027)
- **P2 (Important)**: 8 tasks (TASK-036 to TASK-043)
- **P3 (Future)**: 2 tasks (TASK-044, TASK-045)

### By User Story

- **Story 1 (Unified Sync)**: 13 tasks (TASK-005 to TASK-017)
- **Story 2 (SPARQL Pipeline)**: 10 tasks (TASK-018 to TASK-027)
- **Story 3 (Migration)**: 4 tasks (TASK-036 to TASK-039)
- **Story 4 (Error Handling)**: 4 tasks (TASK-040 to TASK-043)
- **Story 5 (Incremental)**: 2 tasks (TASK-044, TASK-045)
- **Foundation**: 4 tasks (TASK-001 to TASK-004)
- **Benchmarks**: 8 tasks (TASK-028 to TASK-035)
- **Release**: 2 tasks (TASK-046, TASK-047)

### By Phase

- **Phase 1 (Setup)**: 4 tasks (5-6 hours)
- **Phase 2 (Story 1)**: 13 tasks (14 hours)
- **Phase 3 (Story 2)**: 10 tasks (11 hours)
- **Phase 4 (Benchmarks)**: 8 tasks (9 hours)
- **Phase 5 (Migration)**: 4 tasks (5.5 hours)
- **Phase 6 (Error Handling)**: 4 tasks (5.5 hours)
- **Phase 7 (Incremental)**: 2 tasks (3 hours)
- **Phase 8 (Release)**: 2 tasks (1.5 hours)

**Total Estimated Time**: ~55 hours (7 working days at 8 hours/day)

---

## Implementation Strategy

### Week 1: Foundation + User Story 1 (P1)
- **Day 1-2**: Phase 1 (setup) + Phase 2 (integration tests, workspace sync)
- **Day 3**: Phase 2 continued (CLI integration, dry-run mode)
- **Day 4-5**: Phase 3 (SPARQL pipeline testing, error handling)

### Week 2: Benchmarks + Documentation (P0/P2)
- **Day 1-2**: Phase 4 (comprehensive benchmark suite, Andon signals)
- **Day 3**: Phase 5 (migration guide)
- **Day 4**: Phase 6 (enhanced error diagnostics)
- **Day 5**: Phase 8 (final validation, release prep)

### Week 3: Buffer + Polish
- **Day 1-2**: Address any RED signals, fix regressions
- **Day 3**: Phase 7 (incremental sync design documentation)
- **Day 4-5**: Final QA, tag v5.0.0

---

## Success Metrics

**Definition of Done**:
- [ ] All P0 tasks complete (14 tasks)
- [ ] All P1 tasks complete (23 tasks)
- [ ] All benchmarks meet SLOs (SC-002, SC-003, SC-007, SC-010)
- [ ] Migration guide published
- [ ] CI pipeline GREEN (1,168+ tests passing)
- [ ] Zero Andon RED signals
- [ ] CHANGELOG.md and README.md updated

**Performance Targets**:
- ✅ SC-002: Full sync 1K triples <5s
- ✅ SC-003: Incremental sync 1 triple <3s (deferred to v5.1)
- ✅ SC-007: Verify mode <2s
- ✅ SC-010: 50K triples completes

---

**Task Breakdown Version**: 1.0.0
**Generated**: 2025-12-17
**Status**: Ready for Implementation
