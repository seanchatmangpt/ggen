# Implementation Plan: End-to-End Testing with Testcontainers

**Branch**: `011-e2e-testcontainers` | **Date**: 2025-12-16 | **Spec**: [spec.md](./spec.md)
**Input**: Feature specification from `/specs/011-e2e-testcontainers/spec.md`

## Summary

Implement cross-platform E2E testing for `ggen sync` using testcontainers-rs for Linux container testing and native execution for macOS. Tests verify byte-for-byte identical output across platforms, validate Homebrew installation path, and ensure all example projects (thesis-gen) work correctly with golden file comparison.

## Technical Context

**Language/Version**: Rust 1.75+ (edition 2021) - existing ggen v4.0.0 workspace
**Primary Dependencies**: testcontainers 0.25 (already in dev-deps), testcontainers-modules 0.13, assert_cmd 2.0, clnrm 0.1.0
**Storage**: File system (golden files, test fixtures, generated output)
**Testing**: cargo make test with #[ignore] for E2E tests requiring Docker
**Target Platform**: linux-x86_64, darwin-x86_64, darwin-arm64
**Project Type**: New crate `ggen-e2e` in workspace + GitHub Actions workflows
**Performance Goals**: <10 min full E2E suite, <2 min per test case
**Constraints**: Docker required for Linux testing, GitHub Actions macOS runners for darwin testing
**Scale/Scope**: 5+ test scenarios, 3 platforms, 2+ example projects validated

## Constitution Check

*GATE: Must pass before Phase 0 research. Re-check after Phase 1 design.*

Verify compliance with ggen Constitution v1.0.0 (`.specify/memory/constitution.md`):

- [x] **I. Crate-First Architecture**: New `ggen-e2e` crate with clear scope (E2E testing only). Self-contained, independently runnable.
- [x] **II. Deterministic RDF Projections**: Tests VERIFY determinism by comparing outputs across platforms. Same ontology → same output is the core assertion.
- [x] **III. Chicago TDD**: State-based testing - verify generated files match golden files. Real Docker containers, real file systems.
- [x] **IV. cargo make Protocol**: New targets `cargo make test-e2e`, `cargo make test-e2e-linux`, `cargo make test-e2e-macos` with timeouts.
- [x] **V. Type-First Thinking**: Platform enum, TestResult struct, GoldenFile abstraction with type-safe APIs.
- [x] **VI. Andon Signal Protocol**: Container startup failures = RED, output mismatches = RED, container cleanup failures = YELLOW.
- [x] **VII. Error Handling**: All container operations use `Result<T, E>`. No `unwrap()` in production test harness code.
- [x] **VIII. Concurrent Execution**: Tests run in parallel where container isolation allows. Batched container operations.
- [x] **IX. Lean Six Sigma Quality**: Golden file comparison with diff output, container log capture on failure, retry with backoff.

**Quality Gates Pass?**: [x] YES / [ ] NO

## Project Structure

### Documentation (this feature)

```text
specs/011-e2e-testcontainers/
├── plan.md              # This file
├── research.md          # Phase 0: testcontainers patterns, CI/CD best practices
├── data-model.md        # Phase 1: Test entities and relationships
├── quickstart.md        # Phase 1: How to run E2E tests
├── contracts/           # Phase 1: Test fixture schemas
│   └── test-fixtures.yaml
└── tasks.md             # Phase 2 output (/speckit.tasks)
```

### Source Code (repository root)

```text
crates/ggen-e2e/
├── Cargo.toml                    # Crate manifest
├── src/
│   ├── lib.rs                    # Public API
│   ├── container.rs              # Testcontainer lifecycle management
│   ├── platform.rs               # Platform detection and abstraction
│   ├── golden.rs                 # Golden file comparison utilities
│   ├── fixtures.rs               # Test fixture management
│   └── runner.rs                 # Test execution orchestration
└── tests/
    ├── e2e_linux.rs              # Linux container tests (#[ignore])
    ├── e2e_macos.rs              # macOS native tests (#[ignore])
    ├── e2e_cross_platform.rs     # Cross-platform comparison tests
    ├── e2e_homebrew.rs           # Homebrew installation tests (#[ignore])
    └── e2e_examples.rs           # Example project validation

tests/e2e/
├── golden/                       # Golden output files
│   ├── thesis-gen/               # Expected thesis-gen output
│   └── README.md                 # Golden file maintenance guide
└── fixtures/
    ├── minimal-project/          # Minimal ggen project for quick tests
    └── thesis-gen-sample/        # Subset of thesis-gen for E2E

.github/workflows/
├── e2e-linux.yml                 # Linux E2E tests on PR
├── e2e-macos.yml                 # macOS E2E tests on PR
└── e2e-cross-platform.yml        # Cross-platform comparison on release
```

**Structure Decision**: New `crates/ggen-e2e/` crate following crate-first architecture. E2E tests isolated from unit tests. Golden files in `tests/e2e/golden/` for version control. GitHub Actions workflows for CI integration.

## Complexity Tracking

> No violations - feature follows constitution principles.

| Violation | Why Needed | Simpler Alternative Rejected Because |
|-----------|------------|-------------------------------------|
| None | N/A | N/A |

## Phase Completion Status

### Phase 0: Research ✅ COMPLETE

- [x] Testcontainers-rs patterns for Rust CLI testing
- [x] Golden file testing best practices
- [x] GitHub Actions matrix strategy for cross-platform
- [x] Homebrew tap testing patterns
- [x] Container image selection (Ubuntu LTS vs Alpine)

**Output**: `research.md`

### Phase 1: Design & Contracts ✅ COMPLETE

- [x] Entity definitions (Platform, TestFixture, GoldenFile, TestRunner, TestResult, etc.)
- [x] Test fixture schema (YAML)
- [x] CI workflow design
- [x] Quickstart guide

**Outputs**:
- `data-model.md` - 8 entities with Rust type definitions
- `contracts/test-fixtures.yaml` - Fixture, container, platform, CI schemas
- `quickstart.md` - User guide for running E2E tests

### Phase 2: Task Generation 🔜 PENDING

Run `/speckit.tasks` to generate actionable task breakdown.

## Constitution Re-Check (Post-Design)

*Re-verified after Phase 1 design completion:*

- [x] **I. Crate-First**: New `ggen-e2e` crate with clear module boundaries ✓
- [x] **II. Deterministic**: CrossPlatformComparison entity enforces byte-for-byte match ✓
- [x] **III. Chicago TDD**: State-based testing via golden files, real containers ✓
- [x] **IV. cargo make**: New targets defined (test-e2e, test-e2e-linux, test-e2e-macos) ✓
- [x] **V. Type-First**: Platform/Os/Arch enums, Result<T,E> throughout ✓
- [x] **VI. Andon Signals**: TestStatus enum with clear Pass/Fail/TimedOut states ✓
- [x] **VII. Error Handling**: Comprehensive error types (E2EError, PlatformError, etc.) ✓
- [x] **VIII. Concurrent**: Parallel test execution via container isolation ✓
- [x] **IX. Quality**: Golden file checksums, container log capture, retry logic ✓

**Quality Gates Pass?**: [x] YES - Ready for `/speckit.tasks`
