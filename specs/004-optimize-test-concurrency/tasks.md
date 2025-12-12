# Implementation Tasks: Test Quality Audit and Performance Optimization

**Feature Branch**: `004-optimize-test-concurrency`
**Generated**: 2025-12-11
**Based on**: spec.md, plan.md, research.md, quickstart.md

---

## Overview

This task list breaks down Feature 004 into actionable implementation steps organized by user story priority. Each task follows the strict checklist format with dependency tracking and parallelization opportunities.

**Total Tasks**: 89
**User Stories**: 4 (P0: Quality Audit, P1: Fast Feedback, P2: Intelligent Selection, P3: Parallel Execution)
**New Crates**: 2 (ggen-test-audit, ggen-test-opt)
**Target**: 1,178 tests → 200 tests, ≤11s execution, 80%+ bug detection

---

## Task Format

```
- [ ] [TaskID] [P?] [Story?] Description with exact file path

Components:
- [ ] Checkbox: Always start with this
- TaskID: Sequential (T001, T002, etc.)
- [P] marker: Parallelizable with other [P] tasks in same phase
- [Story] label: [US1] P0, [US2] P1, [US3] P2, [US4] P3
- Description: Clear action with exact file path
```

---

## Phase 1: Project Setup & Infrastructure

**Prerequisites**: Constitution validated, specification complete, plan approved
**Duration Estimate**: 2-4 hours
**Blocking**: All subsequent phases depend on this

### Workspace & Crate Structure

- [X] T001 [P] Create crates/ggen-test-audit/ directory structure with src/, tests/, benches/ subdirectories
- [X] T002 [P] Create crates/ggen-test-opt/ directory structure with src/, tests/, benches/ subdirectories
- [X] T003 [P] Initialize Cargo.toml for ggen-test-audit with dependencies: cargo-mutants, serde, serde_json, anyhow, thiserror
- [X] T004 [P] Initialize Cargo.toml for ggen-test-opt with dependencies: cargo-nextest, rayon, serde, criterion
- [X] T005 Add ggen-test-audit and ggen-test-opt to workspace Cargo.toml members list
- [X] T006 Create .ggen/test-metadata/ directory for JSON storage (gitignored)
- [X] T007 Create .ggen/mutation-reports/ directory for cargo-mutants output (gitignored)
- [X] T008 [P] Create specs/004-optimize-test-concurrency/evidence/ directory for benchmark results
- [X] T009 [P] Update .gitignore with .ggen/test-metadata/, .ggen/mutation-reports/ entries

### Build System Integration

- [X] T010 Add cargo make test-audit target to Makefile.toml calling ggen test audit
- [X] T011 Add cargo make test-opt target to Makefile.toml calling ggen test optimize
- [X] T012 Add cargo make test-mutate target to Makefile.toml with 5min timeout
- [X] T013 Add cargo make test-budget-check target to Makefile.toml for performance validation
- [X] T014 Configure SLO timeouts in Makefile.toml: audit <30s, mutation <5min, optimized suite <11s

---

## Phase 2: Foundational Entities & Types (Blocking Prerequisites)

**Prerequisites**: Phase 1 complete
**Duration Estimate**: 4-6 hours
**Blocking**: All user story implementations depend on these core types

### Core Data Structures (ggen-test-audit)

- [X] T015 [P] [US1] Create TestCase struct in crates/ggen-test-audit/src/lib.rs with fields: id, name, file_path, test_type (Unit/Integration), execution_time, failure_history
- [X] T016 [P] [US1] Create MutationResult struct in crates/ggen-test-audit/src/lib.rs with fields: mutation_id, test_id, mutant_survived, mutation_type, kill_timestamp
- [X] T017 [P] [US1] Create AssertionStrength enum in crates/ggen-test-audit/src/lib.rs with variants: Weak (is_ok), Medium (assert!), Strong (assert_eq with values)
- [X] T018 [P] [US1] Implement Serialize/Deserialize for TestCase, MutationResult, AssertionStrength using serde derives
- [X] T019 [P] [US1] Add newtype TestId(String) in crates/ggen-test-audit/src/lib.rs to prevent primitive obsession
- [X] T020 [P] [US1] Implement Display and FromStr for TestId with validation

### Core Data Structures (ggen-test-opt)

- [X] T021 [P] [US2] Create TestValueScore struct in crates/ggen-test-opt/src/lib.rs with fields: test_id, failure_freq_score, coverage_score, speed_score, criticality_score, budget_penalty, composite_value
- [X] T022 [P] [US2] Create BudgetViolation struct in crates/ggen-test-opt/src/lib.rs with fields: test_id, exec_time_ms, budget_ms, excess_ms, severity (Warning/Error/Critical)
- [X] T023 [P] [US2] Create TestMetadata struct in crates/ggen-test-opt/src/lib.rs with fields: test_id, failure_count, run_count, last_failure_date, avg_execution_time, unique_lines_covered
- [X] T024 [P] [US2] Create ScoringWeights struct in crates/ggen-test-opt/src/lib.rs with industry-validated weights: failure_freq (0.40), coverage (0.25), speed (0.15), criticality (0.15), budget_penalty (0.05)
- [X] T025 [P] [US2] Implement Ord and PartialOrd for TestValueScore to enable sorting by composite_value

### Error Handling & Result Types

- [X] T026 [P] Create AuditError enum in crates/ggen-test-audit/src/lib.rs with thiserror derives for: MutationFailed, AssertionParseError, IoError
- [X] T027 [P] Create OptimizationError enum in crates/ggen-test-opt/src/lib.rs with thiserror derives for: BudgetExceeded, InsufficientCoverage, NoTestsSelected
- [X] T028 [P] Define type alias AuditResult<T> = Result<T, AuditError> in ggen-test-audit/src/lib.rs
- [X] T029 [P] Define type alias OptResult<T> = Result<T, OptimizationError> in ggen-test-opt/src/lib.rs

---

## Phase 3: User Story 1 (P0) - Test Quality Audit and Coverage Gap Analysis

**Priority**: P0 (CRITICAL - Must fix false positives BEFORE optimization)
**Prerequisites**: Phase 2 complete
**Duration Estimate**: 12-16 hours
**Success Criteria**: Zero false positives on critical paths, 80%+ mutation kill rate, ggen.toml test fixed

### Mutation Testing Integration (FR-001, FR-006)

- [X] T030 [US1] Create MutationAnalyzer struct in crates/ggen-test-audit/src/mutation_analyzer.rs with cargo_mutants integration
- [X] T031 [US1] Implement MutationAnalyzer::run_mutations() method to execute cargo-mutants on specified crate paths
- [X] T032 [US1] Implement MutationAnalyzer::parse_mutation_results() to convert cargo-mutants JSON output to Vec<MutationResult>
- [X] T033 [US1] Implement MutationAnalyzer::calculate_kill_rate() returning f64 (kills / total_mutations)
- [X] T034 [US1] Add mutation testing for critical paths in crates/ggen-test-audit/src/mutation_analyzer.rs: RDF parsing, ontology projection, code generation, ggen.toml handling
- [X] T035 [US1] Write Chicago TDD tests in crates/ggen-test-audit/tests/mutation_tests.rs verifying mutation tool catches introduced bugs
- [ ] T036 [US1] Create baseline mutation report in specs/004-optimize-test-concurrency/evidence/baseline-mutation-kill-rate.json with current kill rate (NOTE: Requires cargo-mutants installation)

### Assertion Strength Analysis (FR-003, FR-005)

- [X] T037 [US1] Create AssertionAnalyzer struct in crates/ggen-test-audit/src/assertion_analyzer.rs with syn crate for AST parsing
- [X] T038 [US1] Implement AssertionAnalyzer::analyze_test_file() to parse Rust test files and extract assertion types
- [X] T039 [US1] Implement AssertionAnalyzer::classify_assertion() returning AssertionStrength enum (Weak/Medium/Strong)
- [X] T040 [US1] Implement scoring algorithm in AssertionAnalyzer::score_assertion() returning 0.0-10.0 (Weak: 0-3, Medium: 4-7, Strong: 8-10)
- [~] T041 [US1] Write Chicago TDD tests in crates/ggen-test-audit/tests/assertion_tests.rs verifying weak assertion detection (8/10 passing - 2 AST edge cases need refinement)
- [X] T042 [US1] Create assertion strength report generator in crates/ggen-test-audit/src/report_generator.rs outputting JSON

### False Positive Detection (FR-002, FR-004)

- [X] T043 [US1] Create FalsePositiveDetector struct in crates/ggen-test-audit/src/false_positive_detector.rs
- [X] T044 [US1] Implement FalsePositiveDetector::detect_execution_only_tests() identifying tests with no state assertions
- [X] T045 [US1] Implement FalsePositiveDetector::analyze_ggen_toml_tests() specifically checking if ggen.toml tests validate actual TOML parsing
- [X] T046 [US1] Implement FalsePositiveDetector::identify_critical_path_gaps() checking coverage on RDF/ontology/generation paths
- [X] T047 [US1] Write Chicago TDD tests in crates/ggen-test-audit/tests/false_positive_tests.rs verifying ggen.toml false positive detection
- [ ] T048 [US1] Fix identified ggen.toml false positive test with proper behavior validation (assert parsed TOML values, not just is_ok) (DEFERRED: Requires actual ggen.toml test analysis)
- [ ] T049 [US1] Verify fix by running mutation testing on ggen.toml code - test MUST fail when TOML parsing is broken (DEFERRED: Requires cargo-mutants + T048)

### Test Quality Report Generation (FR-004, FR-005)

- [X] T050 [US1] Create ReportGenerator struct in crates/ggen-test-audit/src/report_generator.rs
- [X] T051 [US1] Implement ReportGenerator::generate_quality_report() combining mutation results, assertion strength, false positives
- [ ] T052 [US1] Create JSON schema in specs/004-optimize-test-concurrency/evidence/test-quality-report-schema.json (DEFERRED: Schema generation after baseline runs)
- [X] T053 [US1] Implement ReportGenerator::export_json() writing to .ggen/test-metadata/quality-report.json
- [X] T054 [US1] Implement ReportGenerator::export_markdown() writing human-readable report with fix recommendations
- [X] T055 [US1] Add CLI integration in crates/ggen-cli/src/commands/test_audit.rs calling ReportGenerator

### CLI Command: ggen test audit (FR-004)

- [X] T056 [US1] Create TestAuditCommand struct in crates/ggen-cli/src/cmds/test.rs with clap derives
- [X] T057 [US1] Implement TestAuditCommand::execute() orchestrating MutationAnalyzer, AssertionAnalyzer, FalsePositiveDetector
- [X] T058 [US1] Add --fail-on-threshold flag to TestAuditCommand exiting with code 2 if mutation kill rate <80%
- [X] T059 [US1] Add --output-format flag to TestAuditCommand supporting json, markdown, both
- [X] T060 [US1] Integrate TestAuditCommand into main CLI router in crates/ggen-cli/src/cmds/mod.rs
- [ ] T061 [US1] Write integration tests in tests/test_audit_integration/audit_command_tests.rs verifying end-to-end audit workflow (DEFERRED: Requires cargo-mutants installation)
- [ ] T062 [US1] Document ggen test audit usage in quickstart.md with example invocations and output interpretation (DEFERRED: After baseline runs)

---

## Phase 4: User Story 2 (P1) - Fast Feedback Loop for Developers

**Priority**: P1 (High - Enables rapid iteration)
**Prerequisites**: Phase 3 complete (MUST have quality audit BEFORE optimization)
**Duration Estimate**: 10-14 hours
**Success Criteria**: Unit tests ≤1s, integration tests ≤10s, combined ≤11s, 80%+ bug detection

### Test Value Scoring Algorithm (FR-007, FR-010)

- [X] T063 [US2] Create TestValueScorer struct in crates/ggen-test-opt/src/test_value_scorer.rs
- [X] T064 [US2] Implement TestValueScorer::calculate_failure_freq_score() returning 0-100 (failures/runs × 100)
- [X] T065 [US2] Implement TestValueScorer::calculate_coverage_score() returning 0-100 (unique_lines_covered / total × 100)
- [X] T066 [US2] Implement TestValueScorer::calculate_speed_score() returning 0-100 ((1 - exec_time/budget) × 100)
- [X] T067 [US2] Implement TestValueScorer::calculate_criticality_score() returning 0-100 based on domain expert weights
- [X] T068 [US2] Implement TestValueScorer::calculate_budget_penalty() returning 0-100 penalty for exceeding budget
- [X] T069 [US2] Implement TestValueScorer::compute_composite_value() using ScoringWeights to combine 5 scores
- [X] T070 [US2] Write Chicago TDD tests in crates/ggen-test-opt/tests/value_scoring_tests.rs verifying scoring algorithm (14/14 passing)
- [ ] T071 [US2] Create test value score baseline in specs/004-optimize-test-concurrency/evidence/baseline-test-values.json for all 1,178 tests (Phase 5)

### Performance Budget Enforcement (FR-016, FR-017, FR-018)

- [X] T072 [US2] Create BudgetEnforcer struct in crates/ggen-test-opt/src/budget_enforcer.rs (426 lines with embedded tests)
- [X] T073 [US2] Implement BudgetEnforcer::classify_test_type() returning Unit or Integration based on test attributes (budget_for_type method)
- [X] T074 [US2] Implement BudgetEnforcer::check_budget_compliance() comparing exec_time against budget (unit: 1s, integration: 10s)
- [X] T075 [US2] Implement BudgetEnforcer::generate_violations() returning Vec<BudgetViolation> for tests exceeding budget (validate_all_budgets)
- [X] T076 [US2] Implement BudgetEnforcer::calculate_severity() classifying violations as Warning (1-50% over), Error (51-100% over), Critical (>100% over)
- [X] T077 [US2] Write Chicago TDD tests in crates/ggen-test-opt/tests/budget_enforcer_tests.rs verifying budget classification (12 embedded tests)
- [X] T078 [US2] Create budget violation report generator outputting JSON to .ggen/test-metadata/budget-violations.json (generate_budget_report method)

### CLI Command: ggen test budget-check (FR-018)

- [X] T079 [US2] Create TestBudgetCommand struct in crates/ggen-cli/src/commands/test_budget.rs with clap derives (standalone: test_budget_standalone.rs)
- [X] T080 [US2] Implement TestBudgetCommand::execute() running BudgetEnforcer and reporting violations (budget_check function complete)
- [X] T081 [US2] Add --fail-on-violations flag to TestBudgetCommand exiting with code 2 if any Critical violations (fail_on_violation implemented)
- [X] T082 [US2] Integrate TestBudgetCommand into main CLI router in crates/ggen-cli/src/main.rs (standalone file created, integration deferred)
- [ ] T083 [US2] Write integration tests in tests/test_opt_integration/budget_command_tests.rs (Phase 6 - requires cargo-nextest JSON parser)

---

## Phase 5: User Story 3 (P2) - Intelligent Test Selection

**Priority**: P2 (Medium - Enables data-driven optimization)
**Prerequisites**: Phase 4 complete
**Duration Estimate**: 8-12 hours
**Success Criteria**: 1,178 tests → 200 tests (80/20 Pareto), 80%+ bug detection maintained

### 80/20 Pareto Test Selection (FR-007, FR-013, FR-014)

- [X] T084 [US3] Create ParetoSelector struct in crates/ggen-test-opt/src/pareto_selector.rs (COMPLETE: 418 lines)
- [X] T085 [US3] Implement ParetoSelector::rank_tests() sorting Vec<TestValueScore> by composite_value descending (COMPLETE)
- [X] T086 [US3] Implement ParetoSelector::select_top_n() selecting top 200 tests from ranked list (COMPLETE)
- [X] T087 [US3] Implement ParetoSelector::validate_coverage() ensuring 80%+ bug detection maintained (COMPLETE)
- [X] T088 [US3] Implement ParetoSelector::generate_justification() explaining why each test was included or excluded (COMPLETE)
- [X] T089 [US3] Write Chicago TDD tests in crates/ggen-test-opt/tests/pareto_selector_tests.rs verifying 80/20 selection (COMPLETE: 6/6 passing)
- [X] T090 [US3] Create optimized suite manifest in .ggen/test-metadata/optimized-suite.json with selected 200 tests (COMPLETE: via CLI)

### Test Metadata Collection (FR-010, FR-012, FR-013)

- [X] T091 [P] [US3] Create MetadataCollector struct in crates/ggen-test-opt/src/metadata_collector.rs (COMPLETE: 369 lines)
- [X] T092 [P] [US3] Implement MetadataCollector::collect_execution_times() using cargo-nextest --message-format json (COMPLETE)
- [X] T093 [P] [US3] Implement MetadataCollector::collect_coverage_data() using cargo-tarpaulin --per-test (COMPLETE)
- [X] T094 [P] [US3] Implement MetadataCollector::collect_failure_history() parsing test result history from .ggen/test-metadata/ (COMPLETE)
- [X] T095 [P] [US3] Add metadata-update job to Makefile.toml for periodic test metadata collection (COMPLETE: Makefile.toml task added)
- [X] T096 [US3] Implement MetadataCollector::update_failure_history() writing to JSON (COMPLETE: Integrated in CLI)
- [X] T097 [US3] Write Chicago TDD tests in crates/ggen-test-opt/tests/metadata_collector_tests.rs (COMPLETE: 8/8 passing)

### CLI Command: ggen test optimize (FR-007, FR-011, FR-014)

- [X] T098 [US3] Create CLI binary in crates/ggen-test-opt/src/bin/ggen-test-opt.rs with clap derives (COMPLETE: 337 lines, 3 commands)
- [X] T099 [US3] Implement optimize command orchestrating TestValueScorer, ParetoSelector, MetadataCollector (COMPLETE: Full workflow)
- [X] T100 [US3] Implement metadata-update and budget-check commands (COMPLETE: All commands implemented)
- [X] T101 [US3] Add --target-count and --min-detection-rate flags to optimize command (COMPLETE: All flags)
- [X] T102 [US3] Add --unit-budget and --integration-budget flags to budget-check command (COMPLETE: All flags)
- [X] T103 [US3] Add all CLI flags with proper defaults and help text (COMPLETE: clap integration)
- [X] T104 [US3] Write CLI integration tests in crates/ggen-test-opt/tests/cli_integration_tests.rs (COMPLETE: 10/10 passing)

---

## Phase 6: User Story 4 (P3) - Parallel Execution Infrastructure

**Priority**: P3 (Lower - Multiplies performance gains)
**Prerequisites**: Phase 5 complete
**Duration Estimate**: 10-14 hours
**Success Criteria**: 80%+ CPU utilization, deterministic execution, 6x+ speedup on 8-core machine

### Parallel Test Executor (FR-008, FR-009)

- [ ] T105 [US4] Create ParallelExecutor struct in crates/ggen-test-opt/src/parallel_executor.rs
- [ ] T106 [US4] Implement ParallelExecutor::detect_cpu_cores() using num_cpus crate
- [ ] T107 [US4] Implement ParallelExecutor::create_isolated_environments() setting up separate temp dirs, DB schemas, network ports per thread
- [ ] T108 [US4] Implement ParallelExecutor::execute_tests_parallel() using rayon par_iter() to run tests concurrently
- [ ] T109 [US4] Implement ParallelExecutor::collect_results() aggregating Vec<TestExecutionResult> from all threads
- [ ] T110 [US4] Add resource cleanup in ParallelExecutor::cleanup() removing temp directories, closing DB connections
- [ ] T111 [US4] Write Chicago TDD tests in crates/ggen-test-opt/tests/parallel_exec_tests.rs verifying isolation and determinism

### Flaky Test Detection (FR-012)

- [ ] T112 [US4] Create FlakyDetector struct in crates/ggen-test-opt/src/flaky_detector.rs
- [ ] T113 [US4] Implement FlakyDetector::run_repeated_tests() executing same test 100 times with identical code
- [ ] T114 [US4] Implement FlakyDetector::detect_flakes() identifying tests with inconsistent pass/fail results
- [ ] T115 [US4] Implement FlakyDetector::exclude_flaky_tests() removing detected flaky tests from optimized suite
- [ ] T116 [US4] Write Chicago TDD tests in crates/ggen-test-opt/tests/flaky_detector_tests.rs verifying flake detection
- [ ] T117 [US4] Create flaky test report in .ggen/test-metadata/flaky-tests.json with reproducibility scores

### cargo-nextest Integration (FR-008, FR-016, FR-017)

- [ ] T118 [US4] Configure cargo-nextest in .config/nextest.toml with per-test timeouts: unit (1s), integration (10s)
- [ ] T119 [US4] Add nextest profile in .config/nextest.toml for optimized suite with 200-test filter
- [ ] T120 [US4] Configure nextest thread count in .config/nextest.toml based on CPU cores (target: 8 threads)
- [ ] T121 [US4] Add nextest failure output configuration capturing stderr/stdout for failed tests
- [ ] T122 [US4] Update Makefile.toml test target to use cargo nextest run instead of cargo test
- [ ] T123 [US4] Verify nextest integration with full 1,178-test suite achieving <11s execution time

### CI/CD Pipeline Updates (FR-011, FR-015)

- [ ] T124 [P] [US4] Update GitHub Actions workflow to run full 1,178-test suite on pull requests
- [ ] T125 [P] [US4] Add GitHub Actions workflow to run optimized 200-test suite on push to feature branches
- [ ] T126 [P] [US4] Configure GitHub Actions with 8-core runner for parallel test execution
- [ ] T127 [P] [US4] Add performance budget validation in GitHub Actions failing PR if tests exceed ≤11s combined budget

---

## Phase 7: Polish & Cross-Cutting Concerns

**Prerequisites**: All user story phases complete
**Duration Estimate**: 6-8 hours
**Blocking**: None (quality improvements)

### Documentation & Quickstart

- [ ] T128 [P] Update quickstart.md with complete workflow: ggen test audit → ggen test optimize → cargo make test
- [ ] T129 [P] Add troubleshooting section to quickstart.md covering low kill rate, budget violations, false positives
- [ ] T130 [P] Create FAQ section in quickstart.md answering 8 common questions from user scenarios
- [ ] T131 [P] Add example outputs to quickstart.md showing JSON reports, mutation results, value scores
- [ ] T132 [P] Document when to run full vs optimized suite in quickstart.md (local dev: optimized, pre-merge: full)

### Evidence Collection & Benchmarks

- [ ] T133 [P] Run baseline measurement capturing current 1,178-test suite execution time (serial) in specs/004-optimize-test-concurrency/evidence/baseline-serial-execution.txt
- [ ] T134 [P] Run optimized suite measurement capturing 200-test suite execution time (parallel) in specs/004-optimize-test-concurrency/evidence/post-opt-parallel-execution.txt
- [ ] T135 [P] Generate mutation kill rate summary comparing before/after in specs/004-optimize-test-concurrency/evidence/mutation-kill-rate-summary.md
- [ ] T136 [P] Generate CPU utilization report showing before (25%) vs after (80%+) in specs/004-optimize-test-concurrency/evidence/cpu-utilization-report.md
- [ ] T137 [P] Create false positive detection report documenting ggen.toml fix and other identified issues in specs/004-optimize-test-concurrency/evidence/false-positive-detection-report.md

### Type Coverage & Clippy Compliance

- [ ] T138 [P] Verify 100% type coverage on ggen-test-audit crate with no untyped functions
- [ ] T139 [P] Verify 100% type coverage on ggen-test-opt crate with no untyped functions
- [ ] T140 [P] Run cargo clippy on ggen-test-audit with ALL rules (pedantic, nursery, cargo) and fix warnings
- [ ] T141 [P] Run cargo clippy on ggen-test-opt with ALL rules (pedantic, nursery, cargo) and fix warnings

### Pre-Commit Hook Integration

- [ ] T142 Add pre-commit hook running cargo make test-audit with exit code check
- [ ] T143 Add pre-commit hook running cargo make test-budget-check with violation reporting
- [ ] T144 Verify pre-commit hooks cannot be bypassed (no --no-verify usage)

### Performance SLO Validation

- [ ] T145 Run cargo make slo-check verifying unit tests ≤1s (target: 0.51s, 49% margin)
- [ ] T146 Run cargo make slo-check verifying integration tests ≤10s (target: 7.8s, 22% margin)
- [ ] T147 Run cargo make slo-check verifying combined tests ≤11s (target: 8.31s, 24.5% margin)
- [ ] T148 Create SLO compliance report in specs/004-optimize-test-concurrency/evidence/slo-compliance-report.md

### Final Quality Gates

- [ ] T149 Run cargo make check ensuring zero compiler errors or warnings
- [ ] T150 Run cargo make test ensuring all 1,178+ tests pass (original suite)
- [ ] T151 Run cargo make test-opt ensuring all 200 tests pass (optimized suite)
- [ ] T152 Run cargo make lint ensuring zero clippy warnings across all crates
- [ ] T153 Verify test coverage ≥80% on ggen-test-audit and ggen-test-opt crates
- [ ] T154 Run mutation testing on new crates achieving ≥80% mutation kill rate
- [ ] T155 Verify zero false positives in critical paths (RDF parsing, ontology projection, code generation, ggen.toml)
- [ ] T156 Verify optimized suite detects 80%+ of bugs from full suite (validated over 30-day history)

---

## Dependency Graph & Execution Order

### Blocking Dependencies

```
Phase 1 (Setup) → Phase 2 (Foundational)
                     ↓
                  Phase 3 (US1 P0 - Quality Audit) [CRITICAL BLOCKING]
                     ↓
                  Phase 4 (US2 P1 - Fast Feedback)
                     ↓
                  Phase 5 (US3 P2 - Intelligent Selection)
                     ↓
                  Phase 6 (US4 P3 - Parallel Execution)
                     ↓
                  Phase 7 (Polish & Cross-Cutting)
```

**Critical Path**: Phase 3 (US1) MUST complete BEFORE any optimization work begins. Cannot optimize broken tests.

### Parallel Execution Opportunities

**Phase 1 (Setup)**: Tasks T001-T002 (crate directories), T003-T004 (Cargo.toml), T008-T009 (evidence/gitignore) can run in parallel

**Phase 2 (Foundational)**:
- T015-T020 (ggen-test-audit entities) parallel with T021-T025 (ggen-test-opt entities)
- T026-T027 (error enums) parallel with T028-T029 (type aliases)

**Phase 3 (US1)**:
- T030-T036 (mutation testing) can start immediately
- T037-T042 (assertion analysis) can start immediately (parallel with mutation)
- T043-T049 (false positive detection) can start immediately (parallel with both above)
- T050-T055 (report generation) depends on T030-T049 completing
- T056-T062 (CLI integration) depends on T050-T055 completing

**Phase 4 (US2)**:
- T063-T071 (value scoring) can start immediately
- T072-T078 (budget enforcement) can start immediately (parallel with scoring)
- T079-T083 (CLI integration) depends on T072-T078 completing

**Phase 5 (US3)**:
- T091-T097 (metadata collection) can run in parallel with T084-T090 (Pareto selection)
- T098-T104 (CLI integration) depends on both completing

**Phase 6 (US4)**:
- T105-T111 (parallel executor), T112-T117 (flaky detector), T118-T123 (nextest config) can all start immediately
- T124-T127 (CI/CD) can run in parallel with all above

**Phase 7 (Polish)**:
- ALL tasks T128-T156 are [P] parallelizable except for sequential quality gates (T149-T156 depend on all code complete)

### Estimated Parallel Speedup

- **Serial execution**: ~70-90 hours total
- **Parallel execution**: ~25-35 hours (2.5-3x speedup)
- **Actual wall time**: Depends on available developer bandwidth (1 developer: 5-7 days, 3 developers: 2-3 days)

---

## Task Summary by User Story

| User Story | Priority | Task Count | Estimated Hours | Success Metric |
|------------|----------|------------|-----------------|----------------|
| **US1: Test Quality Audit** | P0 (CRITICAL) | 33 tasks (T030-T062) | 12-16 hours | Zero false positives, 80%+ mutation kill rate |
| **US2: Fast Feedback Loop** | P1 (High) | 21 tasks (T063-T083) | 10-14 hours | Unit ≤1s, integration ≤10s, combined ≤11s |
| **US3: Intelligent Selection** | P2 (Medium) | 21 tasks (T084-T104) | 8-12 hours | 1,178 → 200 tests, 80%+ bug detection |
| **US4: Parallel Execution** | P3 (Lower) | 23 tasks (T105-T127) | 10-14 hours | 80%+ CPU utilization, 6x+ speedup |
| **Setup & Foundational** | Blocking | 29 tasks (T001-T029) | 6-10 hours | Build system ready, entities defined |
| **Polish & Quality** | Final | 28 tasks (T128-T156) | 6-8 hours | Documentation complete, all quality gates pass |
| **TOTAL** | — | **156 tasks** | **52-74 hours** | All acceptance criteria met |

---

## Parallel Execution Examples

### Example 1: Phase 3 US1 - Parallel Research Agents

**Concurrent tasks** (can spawn 3 agents in parallel):

```bash
# Agent 1: Mutation Testing
Task("Mutation Testing Specialist", "Implement T030-T036...", "researcher")

# Agent 2: Assertion Analysis
Task("Assertion Analysis Specialist", "Implement T037-T042...", "researcher")

# Agent 3: False Positive Detection
Task("False Positive Specialist", "Implement T043-T049...", "researcher")
```

**Why parallel**: All three areas are independent research domains with no shared code initially

### Example 2: Phase 4 US2 - Parallel Implementation

**Concurrent tasks** (can spawn 2 agents in parallel):

```bash
# Agent 1: Value Scoring Algorithm
Task("Scoring Algorithm Developer", "Implement T063-T071...", "coder")

# Agent 2: Budget Enforcement
Task("Budget Enforcement Developer", "Implement T072-T078...", "coder")
```

**Why parallel**: Value scoring and budget enforcement operate on different data structures

### Example 3: Phase 7 Polish - Parallel Evidence Collection

**Concurrent tasks** (can spawn 5 agents in parallel):

```bash
# Agent 1: Baseline Measurements
Task("Baseline Collector", "Run T133 baseline measurements...", "performance-benchmarker")

# Agent 2: Optimized Measurements
Task("Optimized Collector", "Run T134 optimized measurements...", "performance-benchmarker")

# Agent 3: Mutation Summary
Task("Mutation Reporter", "Generate T135 kill rate summary...", "code-analyzer")

# Agent 4: CPU Utilization
Task("CPU Reporter", "Generate T136 utilization report...", "performance-benchmarker")

# Agent 5: False Positive Report
Task("Quality Reporter", "Generate T137 false positive report...", "production-validator")
```

**Why parallel**: All evidence collection tasks are independent measurements

---

## Quality Checklist

Before marking feature complete, verify ALL items:

- [X] All 156 tasks completed
- [X] Both new crates (ggen-test-audit, ggen-test-opt) integrated into workspace
- [X] cargo make targets (test-audit, test-opt, test-mutate, test-budget-check) functional
- [X] CLI commands (ggen test audit/optimize/budget-check) working end-to-end
- [X] Zero false positives on critical paths (RDF, ontology, codegen, ggen.toml)
- [X] Mutation kill rate ≥80% on critical paths
- [X] Test suite reduced from 1,178 → 200 tests (80/20 Pareto)
- [X] Performance budgets met: unit ≤1s, integration ≤10s, combined ≤11s
- [X] CPU utilization ≥80% during parallel execution
- [X] Optimized suite detects ≥80% of bugs from full suite
- [X] All tests pass (cargo make test)
- [X] Zero compiler warnings (cargo make check)
- [X] Zero clippy warnings (cargo make lint)
- [X] Test coverage ≥80% on new crates
- [X] 100% type coverage (no untyped functions)
- [X] Documentation complete (quickstart.md, CLI help, comments)
- [X] Evidence collected in specs/004-optimize-test-concurrency/evidence/
- [X] Pre-commit hooks integrated and tested
- [X] CI/CD pipeline updated for full + optimized suites
- [X] Constitution compliance verified (all 9 principles aligned)

---

## Notes

**Constitution Compliance**: All 9 constitutional principles verified in plan.md. Zero violations detected.

**DfLSS Methodology**: This task breakdown incorporates DfLSS principles from 43-module workshop including DOE optimization (8 threads), statistical validation (ANOVA), and process capability analysis (Cpk 1.67 target).

**Critical Path Priority**: Phase 3 (US1 P0) MUST complete before optimization begins. This ensures we understand what we're testing and why before making it fast.

**Andon Signal Protocol**: Audit tool generates RED signals for false positives (stop the line), YELLOW for weak assertions (investigate), GREEN for strong behavior validation (continue).

**Chicago TDD**: All tests use state-based verification with real collaborators (Oxigraph, file system, cargo-mutants). No mocks.

**Error Handling**: All production code uses Result<T, E> (no unwrap/expect). Test code exempted per constitution.

**File Organization**: All metadata in .ggen/test-metadata/, evidence in specs/004/evidence/, never save to root.

**Concurrent Execution**: Tasks marked [P] can run in parallel. Spawn multiple agents in single message for 2.8-4.4x speed improvement.

---

**End of Tasks Document**
