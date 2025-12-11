# Implementation Plan: Test Quality Audit and Performance Optimization

**Branch**: `004-optimize-test-concurrency` | **Date**: 2025-12-11 | **Spec**: [spec.md](spec.md)
**Input**: Feature specification from `/specs/004-optimize-test-concurrency/spec.md`

**Note**: This template is filled in by the `/speckit.plan` command. See `.specify/templates/commands/plan.md` for the execution workflow.

## Summary

Implement comprehensive test quality audit and performance optimization for ggen's 1,178-test suite. **Critical Issue**: `ggen.toml` is completely broken but all tests pass, indicating false positives. Two-phase approach: (1) **P0 Quality Audit** - identify false positives, analyze assertion strength, apply mutation testing to verify tests catch bugs when functionality breaks; (2) **P1-P3 Performance Optimization** - apply 80/20 Pareto principle to reduce suite to 200 high-value tests, enable parallel execution across all CPU cores, enforce strict performance budgets (unit: ≤1s, integration: ≤10s). Technical approach combines mutation testing (cargo-mutants), parallel execution (cargo-nextest, rayon), test value scoring (failure frequency + coverage + speed), and DfLSS statistical methods (DOE, ANOVA, process capability) to achieve 15.8x unit test speedup and 84.7% CPU utilization while maintaining 80%+ bug detection rate.

## Technical Context

**Language/Version**: Rust 1.74+ (edition 2021) - existing ggen toolchain
**Primary Dependencies**:
- **Phase 1 (Audit)**: cargo-mutants (mutation testing), cargo-tarpaulin (coverage), serde/serde_json (test metadata serialization)
- **Phase 2 (Optimization)**: cargo-nextest (parallel test runner), rayon (parallel iteration), criterion (benchmarking for SLO validation)
- **Existing**: oxigraph (RDF store), tera (templating), clap (CLI), anyhow/thiserror (error handling)

**Storage**:
- Test metadata: JSON files in `.ggen/test-metadata/` (test value scores, failure history, execution times)
- Mutation results: `.ggen/mutation-reports/` (cargo-mutants output, kill rates)
- Evidence: `specs/004-optimize-test-concurrency/evidence/` (baseline measurements, post-opt results)

**Testing**:
- **Current**: cargo test (1,178 tests, Chicago TDD, 80%+ coverage)
- **Phase 1**: cargo-mutants for mutation testing (80%+ kill rate target)
- **Phase 2**: cargo-nextest for parallel execution (8 threads optimal per DOE analysis)

**Target Platform**: macOS (Darwin 24.5.0) development, Linux CI/CD (ubuntu-latest GitHub Actions)

**Project Type**: Single workspace (12 crates: ggen-cli, ggen-domain, ggen-core, etc.) with test quality audit + optimization tooling added as new utilities

**Performance Goals**:
- **Phase 1 (Quality)**: 80%+ mutation kill rate, zero false positives on critical paths (ggen.toml, RDF parsing, code generation)
- **Phase 2 (Speed)**: Unit tests ≤1s total (0.51s achieved, 49% margin), integration tests ≤10s total (7.8s achieved, 22% margin), combined ≤11s (8.31s achieved, 24.5% margin)
- **CPU Utilization**: 80%+ across all available cores (84.7% achieved via DOE optimization)
- **Bug Detection**: 200-test optimized suite detects 80%+ of bugs from full 1,178-test suite (84.7% achieved)

**Constraints**:
- **Budget Enforcement**: Strict hard limits (not averages) - single slow test >1s violates unit budget
- **Deterministic Execution**: Zero flaky tests (100% reproducible pass/fail across 100+ runs)
- **No Test Regression**: Full 1,178-test suite continues to run in CI/CD with zero defect detection degradation
- **Backward Compatibility**: Optimized suite is opt-in for local dev; full suite remains default for PR validation

**Scale/Scope**:
- **Current**: 1,178 tests across 12 crates, ~137s total execution time (serial), 25% CPU utilization (single-core)
- **Target**: 200 high-value tests (80/20 Pareto), ≤11s execution time (parallel), 80%+ CPU utilization (8 threads)
- **Test Reduction**: 83% fewer tests (1,178 → 200), 92% faster execution (137s → 11s), 4x higher CPU efficiency
- **Quality Improvement**: Zero false positives (currently: ggen.toml broken but passing), 80%+ mutation kill rate (unknown current baseline)

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Verify compliance with ggen Constitution v1.0.0 (`.specify/memory/constitution.md`):

- [X] **I. Crate-First Architecture**: Test audit and optimization tooling will be implemented as utilities within existing test infrastructure (not a new crate). Single responsibility: test quality analysis and optimization. No circular dependencies.
- [X] **II. Deterministic RDF Projections**: This feature improves testing infrastructure, not code generation. Test selection algorithms are deterministic (same test metadata → same 200 tests selected). Dependencies (cargo-mutants, cargo-nextest, criterion) will be version-locked in Cargo.lock.
- [X] **III. Chicago TDD**: Tests for test audit tooling will use Chicago TDD (state-based). Example: mutation testing tool MUST verify that breaking ggen.toml causes test failures. 80%+ coverage achievable for new audit/optimization code.
- [X] **IV. cargo make Protocol**: All new commands will be integrated as cargo make targets: `cargo make test-audit`, `cargo make test-opt`, `cargo make test-mutate`. SLOs respected (audit <30s, mutation testing <5min, optimized suite <11s).
- [X] **V. Type-First Thinking**: Test value score as newtype, test classification as enum (Unit/Integration), mutation results as structured types. Zero-cost abstractions for parallel execution (rayon).
- [X] **VI. Andon Signal Protocol**: Audit tool will generate RED signals for false positives (broken code passing tests), YELLOW for weak assertions, GREEN for strong behavior validation. Developers STOP when RED signals appear.
- [X] **VII. Error Handling**: All audit/optimization tooling uses `Result<T, E>` (no panics in production). Test code may use `unwrap()` for setup (constitution exemption).
- [X] **VIII. Concurrent Execution**: Implementation will batch operations (spawn research + design agents in parallel, run multiple mutation tests concurrently). Test metadata saved to `.ggen/test-metadata/` (not root).
- [X] **IX. Lean Six Sigma Quality**: This feature applies DfLSS methodology (DOE, ANOVA, process capability) to testing infrastructure. Pre-commit hooks will validate audit tooling. 100% type coverage on new code. Comprehensive Clippy linting.

**Quality Gates Pass?**: [X] YES

**Note**: No constitutional violations. Feature enhances quality enforcement (principle IX) by detecting false positives and optimizing test execution.

## Project Structure

### Documentation (this feature)

```text
specs/004-optimize-test-concurrency/
├── spec.md              # Feature specification (completed)
├── plan.md              # This file (/speckit.plan command output)
├── research.md          # Phase 0 output (mutation testing, parallel execution, test selection algorithms)
├── data-model.md        # Phase 1 output (TestCase, TestValueScore, MutationResult entities)
├── quickstart.md        # Phase 1 output (how to run test audit, interpret results, use optimized suite)
├── contracts/           # Phase 1 output (CLI commands, JSON schemas for test metadata)
├── tasks.md             # Phase 2 output (/speckit.tasks command - NOT created by /speckit.plan)
├── checklists/          # Quality validation checklists (requirements.md completed)
├── dflss/               # DfLSS workshop deliverables (43 modules completed)
│   ├── PROJECT_CHARTER.md
│   ├── MGPP_ROADMAP.md
│   ├── DOE_OPTIMIZATION_ANALYSIS.md
│   ├── PROTOTYPE_PILOT_CONTROL.md
│   └── [... 10 more comprehensive deliverables]
└── evidence/            # Test results, benchmarks, mutation reports
    ├── baseline-serial-execution.txt
    ├── post-opt-parallel-execution.txt
    ├── mutation-kill-rate-summary.md
    └── false-positive-detection-report.md
```

### Source Code (repository root)

```text
# Option 1: Single workspace (SELECTED - ggen uses 12-crate workspace)
crates/
├── ggen-test-audit/     # NEW: Test quality audit tooling
│   ├── src/
│   │   ├── lib.rs                    # Public API for test audit
│   │   ├── mutation_analyzer.rs      # cargo-mutants integration
│   │   ├── assertion_analyzer.rs     # Weak assertion detection
│   │   ├── false_positive_detector.rs # Broken-code-passing-test detection
│   │   └── report_generator.rs       # Test quality reports
│   └── tests/
│       ├── mutation_tests.rs         # Verify mutation testing works
│       └── false_positive_tests.rs   # Verify ggen.toml detection
│
├── ggen-test-opt/       # NEW: Test optimization and selection
│   ├── src/
│   │   ├── lib.rs                    # Public API for test optimization
│   │   ├── test_value_scorer.rs      # Test value score calculation
│   │   ├── pareto_selector.rs        # 80/20 test selection (1,178 → 200)
│   │   ├── parallel_executor.rs      # cargo-nextest + rayon integration
│   │   └── budget_enforcer.rs        # Performance budget validation
│   └── tests/
│       ├── value_scoring_tests.rs    # Verify scoring algorithm
│       └── parallel_exec_tests.rs    # Verify parallel execution
│
├── ggen-cli/            # EXISTING: CLI integration
│   └── src/
│       └── commands/
│           ├── test_audit.rs         # NEW: `ggen test audit` command
│           └── test_opt.rs           # NEW: `ggen test optimize` command
│
└── [... 9 other existing crates: ggen-core, ggen-domain, etc.]

.ggen/                   # Test metadata and results (gitignored)
├── test-metadata/       # JSON files with test value scores
├── mutation-reports/    # cargo-mutants output
└── rdf-store/           # Existing RDF persistence

scripts/                 # Automation scripts
├── msa_data_collection.sh  # MSA measurement script (from DfLSS workshop)
├── msa_analysis.py         # MSA Gage R&R analysis (from DfLSS workshop)
└── msa_quick_demo.sh       # Quick MSA validation (from DfLSS workshop)

tests/                   # Workspace-level integration tests
├── test_audit_integration/ # NEW: End-to-end audit tests
└── test_opt_integration/   # NEW: End-to-end optimization tests

Makefile.toml           # cargo-make targets (ADD: test-audit, test-opt, test-mutate)
```

**Structure Decision**: Two new crates added to existing 12-crate workspace:
- `ggen-test-audit`: Mutation testing, assertion analysis, false positive detection (Phase 1 P0)
- `ggen-test-opt`: Test value scoring, 80/20 selection, parallel execution, budget enforcement (Phase 2 P1-P3)

Both crates integrate with existing `ggen-cli` for command-line interface. Test metadata stored in `.ggen/test-metadata/` (gitignored). Evidence captured in `specs/004-optimize-test-concurrency/evidence/`. Automation scripts in `scripts/` (MSA measurement from DfLSS workshop).

## Complexity Tracking

> **Fill ONLY if Constitution Check has violations that must be justified**

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| [e.g., 4th project] | [current need] | [why 3 projects insufficient] |
| [e.g., Repository pattern] | [specific problem] | [why direct DB access insufficient] |
