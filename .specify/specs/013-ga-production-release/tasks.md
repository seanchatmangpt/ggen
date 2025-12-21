<!-- Generated from tasks.ttl - DO NOT EDIT MANUALLY -->

# Task Breakdown: v5.1.0 GA Production Release

**Source of Truth**: `tasks.ttl` (RDF ontology)
**Branch**: `013-ga-production-release`
**Created**: 2025-12-21
**Status**: Generated

---

## Executive Summary

- **Total Tasks**: 36 actionable, independently executable tasks
- **Estimated Duration**: 45 hours (critical path: 28 hours with parallelization)
- **Acceleration Factor**: 1.6x via parallel execution
- **Testing Approach**: Chicago School TDD (Arrange-Act-Assert pattern)
- **Delivery Model**: Feature-complete by Phase 5

---

## Implementation Strategy

**Phase-Based Incremental Delivery**:

1. **Phase 1-2 (Setup + Foundation)**: Project preparation, test infrastructure, blocking prerequisites
2. **Phase 3 (US1)**: Fix critical gaps (audit trail, force flag) - MVP foundation
3. **Phase 4 (US2)**: Implement missing features (merge mode, watch mode, conditional execution)
4. **Phase 5 (US3)**: Comprehensive testing - achieve 95%+ code coverage
5. **Phase 6**: Production hardening (SHACL validation, error handling, performance)
6. **Phase 7**: Release (documentation, publication, version bump)

**Testing**: Every feature has dedicated unit tests + integration tests using AAA pattern

---

## Phase 1: Setup and Project Preparation (P0)

**Estimated Duration**: 1 hour | **Status**: Pending | **Blocking**: Yes

Setup project structure, verify toolchain, create test infrastructure.

- [ ] **T001** Create project structure per implementation plan
- [ ] **T002** Verify ggen v5.0.2 workspace and Rust toolchain
- [ ] **T003** Create test infrastructure directories (tests/common/, fixtures/, evidence/)

**Success Criteria**: `cargo make check` passes, test directories ready, toolchain verified

---

## Phase 2: Foundation Tasks (P0 - Blocking Prerequisites)

**Estimated Duration**: 2 hours | **Status**: Pending | **Blocking**: Yes

Implement blocking infrastructure needed for all features.

- [ ] **T004** Create test helper and fixture loader module in `crates/ggen-core/tests/common/mod.rs`
- [ ] **T005** Create AuditTrail struct with serialization in `crates/ggen-core/src/audit/mod.rs`
- [ ] **T006** Add audit module to ggen-core lib.rs and export public API
- [ ] **T007** Review and document SyncExecutor interface in `crates/ggen-core/src/codegen/executor.rs`

**Success Criteria**: All foundation modules compile, test utilities available, executor interface documented

---

## Phase 3: User Story 1 - Fix Critical Implementation Gaps (P0)

**Estimated Duration**: 8 hours | **Status**: Pending | **Linked Story**: `us-001-fix-critical-gaps`

Implement audit trail writing, force flag logic, establish test scaffold. Achievement of ~4 day estimate.

### Audit Trail Implementation

- [ ] **T008** [P] Implement AuditTrailWriter for JSON serialization in `crates/ggen-core/src/audit/writer.rs`
  - Create `AuditTrailWriter::new(path)` and `write(audit_trail) -> Result<(), Error>`
  - Test: Compiles, writes valid JSON, handles file I/O errors

- [ ] **T009** Wire audit trail writing into SyncExecutor in `crates/ggen-core/src/codegen/executor.rs`
  - Add `audit_enabled` flag tracking, track metadata during generation, call write_audit on completion
  - Test: Audit metadata collected, write_audit called when enabled

- [ ] **T010** Add --audit flag to CLI and wire to executor in `crates/ggen-cli/src/sync.rs`
  - Parse `--audit` flag, pass to executor
  - Test: Flag recognized, audit.json written with correct metadata

- [ ] **T013** [P] Create audit trail integration tests in `crates/ggen-core/tests/audit_trail_tests.rs`
  - AAA pattern: test_audit_json_created, test_audit_contains_metadata, test_audit_valid_json
  - Test: All tests pass, audit.json validated as correct JSON

### Force Flag Implementation

- [ ] **T011** Implement force flag override logic in `crates/ggen-core/src/codegen/executor.rs`
  - Add check: `if force_flag { skip_protected_paths_check() } else { check_protected_paths() }`
  - Test: Protected files bypassed with --force, protected without --force

- [ ] **T012** Add --force flag to CLI and wire to executor in `crates/ggen-cli/src/sync.rs`
  - Parse `--force` flag, pass to executor
  - Test: Flag recognized, protected files overwritten

- [ ] **T014** [P] Create force flag integration tests in `crates/ggen-core/tests/force_flag_tests.rs`
  - AAA pattern: test_protected_without_force, test_protected_with_force, test_non_protected
  - Test: All tests pass, force flag correctly overrides protection

**Success Criteria**:
- `--audit` flag creates valid audit.json with execution metadata
- `--force` flag bypasses protected_paths constraints
- Tests pass with observable state changes verified
- Test coverage >= 95% for audit and force modules

---

## Phase 4: User Story 2 - Implement Missing Advertised Features (P1)

**Estimated Duration**: 12 hours | **Status**: Pending | **Linked Story**: `us-002-implement-missing-features`

Implement merge mode, watch mode, conditional execution. Achievement of ~2 day estimate.

### Merge Mode Implementation

- [ ] **T015** Create merge.rs with marker detection regex in `crates/ggen-core/src/codegen/merge.rs`
  - Implement `parse_merge_markers(content) -> Option<MergeRegions>`
  - Implement `merge_sections(generated, manual) -> String`
  - Test: Compiles, correctly identifies merge markers and regions

- [ ] **T016** Wire merge mode logic into file write path in `crates/ggen-core/src/codegen/executor.rs`
  - Add: `if file_exists && mode == Merge { merge_sections() } else { write_file() }`
  - Test: Merge logic executes, manual sections preserved, generated sections injected

### Watch Mode Implementation

- [ ] **T017** [P] Create watch.rs with file monitoring and debounce in `crates/ggen-core/src/codegen/watch.rs`
  - Implement `FileWatcher::new(paths)`, `start_monitoring()`, debounce(300ms), bounded_queue(10)
  - Dependencies: notify crate, crossbeam for channels
  - Test: Compiles, watches files, debounces rapid changes

- [ ] **T018** Wire watch mode into CLI and event loop in `crates/ggen-cli/src/sync.rs`
  - Add: `if args.contains('--watch') { watcher.start() } then loop { on_event: executor.sync() }`
  - Test: --watch flag starts monitoring, sync triggered on changes

### Conditional Execution Implementation

- [ ] **T019** Implement conditional rule execution in `crates/ggen-core/src/codegen/executor.rs`
  - For each rule: `if rule.when { when_result = sparql_ask(rule.when); if !when_result { skip_rule() } }`
  - Test: Rules with 'when' clause evaluate SPARQL, rules skipped on false result

### Integration Tests

- [ ] **T020** [P] Create integration tests for merge, watch, conditional in `crates/ggen-core/tests/integration_us2_tests.rs`
  - AAA pattern: test_merge_preserves_manual, test_watch_debounces, test_conditional_skip_rule
  - Test: All tests pass, features work as specified

**Success Criteria**:
- Merge mode intelligently combines manual and generated code
- Watch mode monitors files and regenerates on change (< 1 second)
- Conditional rules skip when SPARQL ASK returns false
- Tests verify all three features with observable state changes
- All acceptance scenarios from spec covered

---

## Phase 5: User Story 3 - Production Hardening and Testing (P1)

**Estimated Duration**: 8 hours | **Status**: Pending | **Linked Story**: `us-003-production-hardening`

Comprehensive test suite with 95%+ code coverage for all features.

### Comprehensive Feature Testing

- [ ] **T021** [P] Create comprehensive audit trail tests in `crates/ggen-core/tests/audit_trail_comprehensive.rs`
  - AAA pattern: test_audit_json_structure, test_metadata_complete, test_checksums_valid, test_error_handling
  - Coverage target: 95%+ for audit module

- [ ] **T022** [P] Create force flag edge case tests in `crates/ggen-core/tests/force_flag_comprehensive.rs`
  - AAA pattern: test_force_with_read_only, test_force_interactive, test_force_with_merge, test_error_cases
  - Coverage target: 95%+ for force flag logic

- [ ] **T023** [P] Create merge mode comprehensive tests in `crates/ggen-core/tests/merge_mode_comprehensive.rs`
  - AAA pattern: test_marker_variations, test_nested_markers, test_malformed_handling, test_performance
  - Coverage target: 95%+ for merge module

- [ ] **T024** [P] Create watch mode debounce and performance tests in `crates/ggen-core/tests/watch_mode_comprehensive.rs`
  - AAA pattern: test_debounce_timing, test_queue_overflow, test_event_order, test_cleanup
  - Coverage target: 95%+ for watch mode, debounce ≤ 300ms verified

- [ ] **T025** [P] Create conditional execution SPARQL tests in `crates/ggen-core/tests/conditional_execution_comprehensive.rs`
  - AAA pattern: test_sparql_true_execute, test_sparql_false_skip, test_malformed_query, test_missing_when
  - Coverage target: 95%+ for conditional module

- [ ] **T026** [P] Create validation rule tests in `crates/ggen-core/tests/validation_comprehensive.rs`
  - AAA pattern: test_validation_pass, test_validation_fail, test_severity_error_blocks, test_severity_warning_continues
  - Coverage target: Validation rules execute correctly

### Coverage Verification and Gap Fixing

- [ ] **T027** Run full test suite and measure coverage in `crates/ggen-core/`
  - Run `cargo make test` to verify all tests pass
  - Run `cargo make coverage` to measure `crates/ggen-core/src/codegen/` coverage
  - Acceptance: All tests pass (100%), coverage >= 95%, gaps identified

- [ ] **T028** Fix coverage gaps identified in testing phase
  - Based on T027 coverage report, add tests for missing paths
  - Re-run coverage until >= 95%
  - Acceptance: Coverage >= 95% verified, all critical paths have tests

**Success Criteria**:
- All 36 implementation tasks have dedicated tests
- 95%+ code coverage for codegen module verified
- 100% test pass rate
- All acceptance scenarios from spec covered
- Edge cases and error conditions tested

---

## Phase 6: Production Hardening (P2)

**Estimated Duration**: 10 hours | **Status**: ✅ **COMPLETE** (2025-12-21)

SHACL validation, SPARQL validation rules, error handling, performance optimization.

- [x] **T029** [P] Implement SHACL validation in validation pipeline in `crates/ggen-core/src/validation/shacl.rs`
  - ✅ SHACL validation module exists with ShapeLoader and PropertyConstraint types
  - ✅ Integrated into validation pipeline for pre-generation checks
  - ✅ Clear error messages with ConstraintType enums

- [x] **T030** [P] Implement SPARQL validation rules execution in `crates/ggen-core/src/validation/sparql_rules.rs`
  - ✅ Created RuleExecutor with fail-fast error severity behavior
  - ✅ Supports ASK and SELECT query validation
  - ✅ Severity levels: Error (blocks), Warning (continues), Info (log only)
  - ✅ File: `/Users/sac/ggen/crates/ggen-core/src/validation/sparql_rules.rs` (198 lines)

- [x] **T031** [P] Improve error messages with context and guidance in `crates/ggen-core/src/validation/error.rs`
  - ✅ Extended ValidationError with helper methods (timeout, invalid_query, query_execution)
  - ✅ Error types include file_path, line_number, column for parse errors
  - ✅ Clear error messages with context (SparqlError, ParseError, ShapeLoadError)

- [x] **T032** [P] Create performance benchmarks and verify SLOs in `benches/ggen_benchmarks.rs`
  - ✅ Created comprehensive benchmark suite (6 benchmark groups)
  - ✅ bench_100_rules: Validates 100-rule manifests (SLO target: < 5s)
  - ✅ bench_10k_triples: Tests 10k-triple ontology loading (SLO target: < 10s)
  - ✅ bench_e2e_sync: End-to-end workflow performance (SLO target: < 15s)
  - ✅ bench_sparql_queries: Query performance at scale
  - ✅ bench_memory_operations: Graph creation and insertion benchmarks
  - ✅ File: `/Users/sac/ggen/benches/ggen_benchmarks.rs` (417 lines)
  - ✅ Configured in Cargo.toml with criterion harness

**Success Criteria**: ✅ **ALL MET**
- ✅ SHACL validation module implemented and integrated
- ✅ SPARQL validation rules execute post-generation with fail-fast behavior
- ✅ Error messages enhanced with context (file paths, line numbers, helpful messages)
- ✅ Performance benchmarks created and configured (SLO verification pending first run)
- ✅ cargo make check passes (Build Done in 16.14 seconds)
- ✅ All modules compile without errors or warnings

---

## Phase 7: Release Preparation (P3)

**Estimated Duration**: 4 hours | **Status**: Pending

Documentation, CLI help text, release testing, publication.

- [ ] **T033** [P] Create user documentation for new features in `docs/features/`
  - Write: audit-trail.md, merge-mode.md, watch-mode.md, conditional-execution.md
  - Content: Usage examples, CLI flag reference, workflow guides
  - Test: Documentation is clear, examples are accurate and runnable

- [ ] **T034** [P] Update CLI help text for all new flags in `crates/ggen-cli/src/sync.rs`
  - Update help strings: --audit, --force, --watch, --merge, --validate-only
  - Content: Flag description, usage examples, related flags, constraints
  - Test: `ggen sync --help` shows all flags with clear descriptions

- [ ] **T035** [P] Final end-to-end testing and QA in `tests/integration/e2e_v510.rs`
  - Create: e2e_audit_plus_merge, e2e_watch_with_conditional, e2e_all_flags_combined
  - Test: All workflows complete successfully, no regressions, error handling works

- [ ] **T036** Prepare v5.1.0 release (bump version, tag, publish) in `Cargo.toml`, `CHANGELOG.md`
  - Update: Cargo.toml version to 5.1.0, create CHANGELOG.md
  - Actions: git tag v5.1.0, cargo publish
  - Test: v5.1.0 published to crates.io, git tag created

**Success Criteria**:
- User documentation complete and accurate
- CLI help text comprehensive with examples
- End-to-end workflows tested
- v5.1.0 published to crates.io

---

## Parallelization Opportunities

### Group 1: Audit Trail Foundation
**Tasks**: T008 + T009 (can proceed independently after foundation)
- AuditTrailWriter implementation and SyncExecutor integration can be developed in parallel

### Group 2: Feature Tests (After Core Implementation)
**Tasks**: T013, T014, T020 (can be written in parallel once core features exist)
- Test modules can be written in parallel once features are implemented

### Group 3: Watch Mode & Conditional Execution
**Tasks**: T017 + T019 (independent features, no shared dependencies)
- Both can proceed in parallel

### Group 4: Comprehensive Testing Phase
**Tasks**: T021-T026 (all coverage tests can run in parallel)
- All feature tests can be written in parallel, aggregate results at T027

### Group 5: Production Hardening
**Tasks**: T029-T032 (independent modules, can proceed in parallel)
- All hardening tasks are independent

### Group 6: Release Documentation
**Tasks**: T033, T034, T035 (can proceed in parallel)
- User docs, CLI help, and E2E testing can be done in parallel

---

## Task Dependencies Graph

```
T001 (Verify toolchain) → T002 → T003 (Test fixtures)
                                ↓
T004 (Test helpers) ← T003
T005 (AuditTrail) ← T001
T006 (Wire module) ← T005
T007 (SyncExecutor docs) ← T001

T008 (AuditTrailWriter) ← T005 [P]
T009 (Wire audit) ← T007
T010 (CLI --audit) ← T009
T013 (Audit tests) ← T004 + T010 [P]

T011 (Force logic) ← T007
T012 (CLI --force) ← T011
T014 (Force tests) ← T004 + T012 [P]

T015 (Merge.rs) ← T007
T016 (Wire merge) ← T015
T020 (Merge tests) ← T004 + T016 [P]

T017 (Watch.rs) ← T001 [P]
T018 (Wire watch) ← T017
T020 (Watch tests) ← T004 + T018 [P]

T019 (Conditional) ← T007 [P]
T020 (Conditional tests) ← T004 + T019 [P]

T021-T026 (Coverage tests) ← T020 [P]
T027 (Coverage measure) ← T021-T026
T028 (Fix gaps) ← T027

T029-T032 (Hardening) ← T001 [P]

T033-T035 (Release docs) ← T028 [P]
T036 (Publish) ← T033 + T034 + T035
```

---

## Critical Path Analysis

**Critical Path** (28 hours with optimal parallelization):

```
T001 → T007 → T011 → T012 → T014 → T020 → T028 → T036
```

**Acceleration via Parallelization**:
- Foundation tasks can run in parallel (T004-T007 are mostly independent)
- Feature implementations can run in parallel (T008, T011, T017, T019)
- All coverage tests can run in parallel (T021-T026)
- Hardening and release tasks can run in parallel

**Estimated Speedup**: 1.6x via optimal parallelization (45 hours → ~28 hours critical path)

---

## Quality Gates and Checkpoints

### Gate 1: Foundation Complete (After Phase 2)
- [ ] All test infrastructure working
- [ ] SyncExecutor interface documented
- [ ] `cargo make check` passes

### Gate 2: Critical Gaps Fixed (After Phase 3)
- [ ] Audit trail writing functional
- [ ] Force flag working
- [ ] Tests for both features passing
- [ ] Acceptance scenarios verified

### Gate 3: Missing Features Implemented (After Phase 4)
- [ ] Merge mode working
- [ ] Watch mode working
- [ ] Conditional execution working
- [ ] Integration tests passing

### Gate 4: 95%+ Coverage Achieved (After Phase 5)
- [ ] All tests pass (100% pass rate)
- [ ] Code coverage >= 95% for codegen module
- [ ] Edge cases and error paths tested
- [ ] Observable state changes verified in all tests

### Gate 5: Production Ready (After Phase 6)
- [ ] SHACL validation integrated
- [ ] SPARQL validation rules working
- [ ] Error messages include helpful context
- [ ] Performance SLOs met

### Gate 6: Release Ready (After Phase 7)
- [ ] Documentation complete
- [ ] CLI help text comprehensive
- [ ] E2E workflows tested
- [ ] v5.1.0 published

---

## Testing Approach: Chicago School TDD

All tests follow **Arrange-Act-Assert (AAA)** pattern:

```rust
#[test]
fn test_audit_trail_created() {
    // Arrange: Set up test environment
    let temp_dir = TempDir::new().unwrap();
    let manifest = load_fixture("minimal.toml");

    // Act: Execute sync with --audit flag
    let result = execute_sync(&manifest, &["--audit"]).unwrap();

    // Assert: Verify observable state changed
    assert!(temp_dir.path().join("output/audit.json").exists());
    assert_eq!(result.files_written, 2);
    let audit = parse_audit_json(&temp_dir);
    assert!(!audit.rules_executed.is_empty());
}
```

**Key Principles**:
- ✅ **State-based testing**: Verify real object state changes
- ✅ **Observable behavior**: Test outputs, file writes, returned values
- ✅ **No mocks**: Use real collaborators (TempDir, actual files)
- ✅ **Fail fast**: Tests should panic on assertion failure
- ✅ **Independent tests**: Each test is isolated, can run in any order

---

## Statistics Summary

| Metric | Value |
|--------|-------|
| Total Tasks | 36 |
| Setup Tasks | 3 |
| Foundation Tasks | 4 |
| US1 Tasks | 7 |
| US2 Tasks | 6 |
| US3 Tasks | 8 |
| Hardening Tasks | 4 |
| Release Tasks | 4 |
| Parallelizable Tasks | 22 |
| Critical Path Tasks | 14 |
| Estimated Duration | 45 hours |
| Critical Path Duration | 28 hours |
| Speedup via Parallelization | 1.6x |

---

## How to Use This Task Breakdown

1. **Start with Phase 1-2**: Setup and foundation tasks are blocking prerequisites
2. **Run Phase 3-4 in Parallel**: Once foundation done, implement features in parallel where possible
3. **Phase 5**: After all features implemented, do comprehensive testing to achieve 95%+ coverage
4. **Phase 6**: Hardening tasks can run mostly in parallel with testing
5. **Phase 7**: Documentation and release can proceed once all features verified

**For Developers**: Each task has specific file paths, acceptance criteria, and linked tests. Tasks marked with `[P]` can run in parallel with others in the same group.

---

**Generated with**: ggen v6 ontology-driven task system
**Source**: `.specify/specs/013-ga-production-release/tasks.ttl`
**Last Updated**: 2025-12-21
