# Tasks: End-to-End Testing with Testcontainers

**Input**: Design documents from `/specs/011-e2e-testcontainers/`
**Prerequisites**: plan.md ✓, spec.md ✓, research.md ✓, data-model.md ✓, contracts/ ✓

**Tests**: Tests are integral to this feature (it's a testing framework). Each user story includes test implementation as core deliverable.

**Organization**: Tasks grouped by user story from spec.md. 5 user stories: US1 (P1), US2 (P1), US3 (P2), US4 (P2), US5 (P3).

## Format: `[ID] [P?] [Story] Description`

- **[P]**: Can run in parallel (different files, no dependencies)
- **[Story]**: US1-US5 maps to spec.md user stories
- Paths based on plan.md: `crates/ggen-e2e/src/`, `tests/e2e/`

## Summary

| Phase | User Story | Tasks | Priority |
|-------|------------|-------|----------|
| 1 | Setup | 6 | - |
| 2 | Foundational | 8 | - |
| 3 | Cross-Platform Validation | 10 | P1 |
| 4 | Dependency Isolation | 8 | P1 |
| 5 | Homebrew Verification | 6 | P2 |
| 6 | Sample Project Suite | 8 | P2 |
| 7 | CI Pipeline Integration | 8 | P3 |
| 8 | Polish | 6 | - |
| **Total** | | **60** | |

---

## Phase 1: Setup (Shared Infrastructure)

**Purpose**: Create ggen-e2e crate and basic project structure

- [ ] T001 Create crates/ggen-e2e/ directory structure per plan.md
- [ ] T002 Create Cargo.toml for ggen-e2e crate in crates/ggen-e2e/Cargo.toml
- [ ] T003 Add ggen-e2e to workspace members in root Cargo.toml
- [ ] T004 [P] Create crates/ggen-e2e/src/lib.rs with module declarations
- [ ] T005 [P] Create tests/e2e/ directory structure (golden/, fixtures/)
- [ ] T006 [P] Create tests/e2e/golden/README.md with golden file maintenance guide

---

## Phase 2: Foundational (Blocking Prerequisites)

**Purpose**: Core types and error handling that ALL user stories depend on

**⚠️ CRITICAL**: No user story work can begin until this phase is complete

- [ ] T007 [P] Implement error types (E2EError, PlatformError, FixtureError, GoldenError, ContainerError, RunnerError) in crates/ggen-e2e/src/error.rs
- [ ] T008 [P] Implement Platform, Os, Arch types with detection in crates/ggen-e2e/src/platform.rs
- [ ] T009 [P] Implement GoldenFile and GoldenMismatch types in crates/ggen-e2e/src/golden.rs
- [ ] T010 [P] Implement TestFixture type in crates/ggen-e2e/src/fixture.rs
- [ ] T011 [P] Implement ContainerConfig and VolumeMount types in crates/ggen-e2e/src/container.rs
- [ ] T012 Implement TestResult and TestStatus types in crates/ggen-e2e/src/result.rs
- [ ] T013 Implement TestRunner and TestExecution types in crates/ggen-e2e/src/runner.rs
- [ ] T014 Add cargo make targets (test-e2e, test-e2e-linux, test-e2e-macos) in Makefile.toml

**Checkpoint**: Foundation ready - all core types implemented. User story implementation can begin.

---

## Phase 3: User Story 1 - Cross-Platform ggen sync Validation (Priority: P1) 🎯 MVP

**Goal**: Verify `ggen sync` works correctly on both macOS and Linux with identical output

**Independent Test**: Run `cargo make test-e2e` to execute tests on current platform, verify golden file comparison works

**Acceptance Criteria (from spec.md)**:
1. Linux container executes `ggen sync` and produces expected output
2. macOS native execution produces byte-for-byte identical output
3. Results identical regardless of installation method

### Implementation for User Story 1

- [ ] T015 [US1] Implement GgenExecutor trait in crates/ggen-e2e/src/runner.rs
- [ ] T016 [P] [US1] Implement NativeExecutor (macOS) in crates/ggen-e2e/src/runner.rs
- [ ] T017 [P] [US1] Implement ContainerExecutor (Linux) using testcontainers in crates/ggen-e2e/src/runner.rs
- [ ] T018 [US1] Implement TestRunner.run() orchestration in crates/ggen-e2e/src/runner.rs
- [ ] T019 [US1] Implement golden file comparison logic (normalize line endings) in crates/ggen-e2e/src/golden.rs
- [ ] T020 [P] [US1] Create minimal test fixture in tests/e2e/fixtures/minimal-project/
- [ ] T021 [P] [US1] Generate golden files for minimal fixture in tests/e2e/golden/minimal/
- [ ] T022 [US1] Implement CrossPlatformComparison entity in crates/ggen-e2e/src/comparison.rs
- [ ] T023 [US1] Create e2e_linux.rs test file in crates/ggen-e2e/tests/e2e_linux.rs
- [ ] T024 [US1] Create e2e_macos.rs test file in crates/ggen-e2e/tests/e2e_macos.rs

**Checkpoint**: US1 complete - cross-platform sync validation working. Can run `cargo make test-e2e` on any platform.

---

## Phase 4: User Story 2 - Dependency Isolation via Testcontainers (Priority: P1)

**Goal**: Tests run in isolated containers with automatic dependency management and cleanup

**Independent Test**: Run E2E tests on machine with only Docker installed, verify containers start, run, and clean up

**Acceptance Criteria (from spec.md)**:
1. CI runner with only Docker can run full E2E suite
2. No state leaks between test runs (containers fully cleaned up)
3. Container logs captured on failure for debugging

### Implementation for User Story 2

- [ ] T025 [US2] Implement container startup with retry and backoff in crates/ggen-e2e/src/container.rs
- [ ] T026 [US2] Implement container cleanup (Drop trait) with log capture in crates/ggen-e2e/src/container.rs
- [ ] T027 [P] [US2] Implement Docker availability check in crates/ggen-e2e/src/platform.rs
- [ ] T028 [US2] Implement volume mounting for fixtures in crates/ggen-e2e/src/container.rs
- [ ] T029 [US2] Implement ggen binary injection into containers in crates/ggen-e2e/src/container.rs
- [ ] T030 [US2] Implement test timeout handling (configurable, default 5 min) in crates/ggen-e2e/src/runner.rs
- [ ] T031 [US2] Create e2e_cross_platform.rs test comparing outputs in crates/ggen-e2e/tests/e2e_cross_platform.rs
- [ ] T032 [US2] Add container log capture and display on test failure in crates/ggen-e2e/src/result.rs

**Checkpoint**: US2 complete - testcontainers fully working with cleanup. Docker-only environments supported.

---

## Phase 5: User Story 3 - Homebrew Installation Verification (Priority: P2)

**Goal**: Verify ggen installed via Homebrew works correctly

**Independent Test**: On macOS, run `brew install seanchatmangpt/tap/ggen`, then `ggen sync` on sample project

**Acceptance Criteria (from spec.md)**:
1. `brew install ggen` succeeds and ggen available in PATH
2. Homebrew-installed ggen produces output identical to cargo-installed
3. `brew upgrade ggen` works correctly

### Implementation for User Story 3

- [ ] T033 [P] [US3] Create Homebrew test helper module in crates/ggen-e2e/src/homebrew.rs
- [ ] T034 [US3] Implement brew install/uninstall wrapper in crates/ggen-e2e/src/homebrew.rs
- [ ] T035 [US3] Implement brew version verification in crates/ggen-e2e/src/homebrew.rs
- [ ] T036 [US3] Create e2e_homebrew.rs test file (#[ignore] for CI) in crates/ggen-e2e/tests/e2e_homebrew.rs
- [ ] T037 [US3] Implement smoke test comparing homebrew ggen vs cargo ggen in crates/ggen-e2e/tests/e2e_homebrew.rs
- [ ] T038 [US3] Add homebrew-specific cargo make target in Makefile.toml (test-e2e-homebrew)

**Checkpoint**: US3 complete - Homebrew installation path verified. macOS users can trust brew distribution.

---

## Phase 6: User Story 4 - Sample Project Validation Suite (Priority: P2)

**Goal**: All example projects in repository work correctly with golden file validation

**Independent Test**: Run `cargo make test-e2e-examples` to validate all examples against golden files

**Acceptance Criteria (from spec.md)**:
1. thesis-gen example produces LaTeX matching golden output
2. Any example with ggen.toml completes without errors
3. Golden files can be updated when examples change

### Implementation for User Story 4

- [ ] T039 [P] [US4] Create thesis-gen-sample fixture (subset of thesis-gen) in tests/e2e/fixtures/thesis-gen-sample/
- [ ] T040 [P] [US4] Generate golden files for thesis-gen in tests/e2e/golden/thesis-gen/
- [ ] T041 [US4] Implement UPDATE_GOLDEN env var support in crates/ggen-e2e/src/golden.rs
- [ ] T042 [US4] Implement fixture discovery (scan tests/e2e/fixtures/) in crates/ggen-e2e/src/fixture.rs
- [ ] T043 [US4] Create e2e_examples.rs test iterating all fixtures in crates/ggen-e2e/tests/e2e_examples.rs
- [ ] T044 [US4] Implement checksum validation for golden files in crates/ggen-e2e/src/golden.rs
- [ ] T045 [P] [US4] Add .gitattributes for LF line endings in tests/e2e/ in tests/e2e/.gitattributes
- [ ] T046 [US4] Add cargo make target test-e2e-examples in Makefile.toml

**Checkpoint**: US4 complete - all example projects validated. Regressions in examples will be caught.

---

## Phase 7: User Story 5 - CI Pipeline Integration (Priority: P3)

**Goal**: E2E tests integrated into GitHub Actions, blocking PRs on failure

**Independent Test**: Create PR with intentional failure, verify CI blocks merge

**Acceptance Criteria (from spec.md)**:
1. PRs trigger E2E tests on Linux and macOS runners
2. Failed E2E tests block PR merge with clear failure logs
3. Passing E2E shows green checkmark

### Implementation for User Story 5

- [ ] T047 [P] [US5] Create .github/workflows/e2e-linux.yml for Linux E2E tests
- [ ] T048 [P] [US5] Create .github/workflows/e2e-macos.yml for macOS E2E tests
- [ ] T049 [P] [US5] Create .github/workflows/e2e-cross-platform.yml for release comparison
- [ ] T050 [US5] Configure workflow triggers (PR, push to master, release) in workflows
- [ ] T051 [US5] Add artifact upload for test logs on failure in workflows
- [ ] T052 [US5] Configure matrix strategy for platform coverage in workflows
- [ ] T053 [US5] Add status check requirement to branch protection (document in README)
- [ ] T054 [US5] Implement JUnit XML output for CI reporting in crates/ggen-e2e/src/result.rs

**Checkpoint**: US5 complete - CI fully integrated. PRs cannot merge without E2E pass.

---

## Phase 8: Polish & Cross-Cutting Concerns

**Purpose**: Final improvements affecting multiple user stories

- [ ] T055 [P] Add comprehensive rustdoc comments to all public APIs in crates/ggen-e2e/src/
- [ ] T056 [P] Update quickstart.md with actual commands and outputs in specs/011-e2e-testcontainers/quickstart.md
- [ ] T057 Run `cargo make test` to verify ggen-e2e integrates with existing test suite
- [ ] T058 Run `cargo make clippy` and fix any lints in ggen-e2e crate
- [ ] T059 [P] Add README.md for ggen-e2e crate in crates/ggen-e2e/README.md
- [ ] T060 Validate quickstart.md instructions work end-to-end

---

## Dependencies & Execution Order

### Phase Dependencies

```
Phase 1: Setup ──────────────────────────────────────────────────────────┐
                                                                         │
Phase 2: Foundational ◀──────────────────────────────────────────────────┘
         ⚠️ BLOCKS ALL USER STORIES                                      │
                                                                         │
         ┌───────────────────────────────────────────────────────────────┘
         │
         ▼
┌────────────────┐  ┌────────────────┐  ┌────────────────┐
│ Phase 3: US1   │  │ Phase 4: US2   │  │ Phase 5: US3   │
│ Cross-Platform │  │ Testcontainers │  │ Homebrew       │
│ (P1) 🎯 MVP    │  │ (P1)           │  │ (P2)           │
└───────┬────────┘  └───────┬────────┘  └───────┬────────┘
        │                   │                   │
        │                   │                   │
        ▼                   ▼                   ▼
┌────────────────┐  ┌────────────────┐
│ Phase 6: US4   │  │ Phase 7: US5   │
│ Sample Suite   │  │ CI Integration │
│ (P2)           │  │ (P3)           │
└───────┬────────┘  └───────┬────────┘
        │                   │
        └─────────┬─────────┘
                  │
                  ▼
         Phase 8: Polish
```

### User Story Dependencies

- **US1 (P1)**: Depends on Foundational only. MVP - can ship after this.
- **US2 (P1)**: Depends on Foundational + partial US1 (ContainerExecutor). Often co-developed.
- **US3 (P2)**: Depends on Foundational only. Independent of US1/US2.
- **US4 (P2)**: Depends on US1 (golden file comparison). Extends fixture coverage.
- **US5 (P3)**: Depends on US1 + US2 (tests must exist). CI wires up existing tests.

### Within Each User Story

- Core types before implementations
- Implementations before test files
- Test files before integration
- Verify tests pass before marking story complete

### Parallel Opportunities

**Phase 1 (all parallel)**:
```bash
T004 [P] lib.rs
T005 [P] tests/e2e/ structure
T006 [P] golden/README.md
```

**Phase 2 (error types and core types parallel)**:
```bash
T007 [P] error.rs
T008 [P] platform.rs
T009 [P] golden.rs
T010 [P] fixture.rs
T011 [P] container.rs
```

**Phase 3 US1 (executors parallel, then fixtures parallel)**:
```bash
T016 [P] NativeExecutor
T017 [P] ContainerExecutor
---
T020 [P] minimal fixture
T021 [P] golden files
```

**Phase 7 US5 (all workflows parallel)**:
```bash
T047 [P] e2e-linux.yml
T048 [P] e2e-macos.yml
T049 [P] e2e-cross-platform.yml
```

---

## Implementation Strategy

### MVP First (User Story 1 Only)

1. Complete Phase 1: Setup (T001-T006)
2. Complete Phase 2: Foundational (T007-T014)
3. Complete Phase 3: User Story 1 (T015-T024)
4. **STOP and VALIDATE**: Run `cargo make test-e2e` locally
5. Deploy/demo if ready - basic E2E testing working

### Incremental Delivery

1. Setup + Foundational → Foundation ready (14 tasks)
2. Add US1 → Cross-platform working → **MVP!** (24 tasks total)
3. Add US2 → Testcontainers isolation → Robust CI (32 tasks total)
4. Add US3 → Homebrew verified (38 tasks total)
5. Add US4 → Example coverage (46 tasks total)
6. Add US5 → CI integration → **Full feature** (54 tasks total)
7. Polish → Production ready (60 tasks total)

### Suggested MVP Scope

**Minimum Viable Product = Phase 1 + Phase 2 + Phase 3 (US1)**

This delivers:
- ✅ ggen-e2e crate with core types
- ✅ Platform detection (Linux/macOS)
- ✅ Golden file comparison
- ✅ NativeExecutor for macOS
- ✅ ContainerExecutor for Linux
- ✅ Basic E2E tests

**Value**: Developers can run `cargo make test-e2e` to validate ggen sync works.

---

## Notes

- Tests use #[ignore] attribute for Docker-dependent tests to allow `cargo test` to pass without Docker
- Container cleanup is critical - use Drop trait to ensure no orphaned containers
- Golden files stored with LF line endings (.gitattributes enforces this)
- UPDATE_GOLDEN=1 env var updates golden files for intentional changes
- Homebrew tests (#[ignore]) only run manually or in release CI
