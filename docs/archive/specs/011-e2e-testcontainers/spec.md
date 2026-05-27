# Feature Specification: End-to-End Testing with Testcontainers

**Feature Branch**: `011-e2e-testcontainers`
**Created**: 2025-12-16
**Status**: Draft
**Input**: User description: "end to end testing. use testcontainers and brew to ensure that ggen sync works on osx and linux no matter what"

## User Scenarios & Testing *(mandatory)*

### User Story 1 - Cross-Platform ggen sync Validation (Priority: P1)

As a developer, I want automated end-to-end tests that verify `ggen sync` works correctly on both macOS and Linux, so I can confidently release ggen knowing it will work for all users regardless of their operating system.

**Why this priority**: This is the core value proposition - ensuring ggen's primary command (`sync`) works reliably across all supported platforms. Without this, users on different platforms may experience silent failures or inconsistent behavior.

**Independent Test**: Run the E2E test suite which spins up platform-specific containers, executes `ggen sync` with sample ontologies, and validates the generated output matches expected results.

**Acceptance Scenarios**:

1. **Given** a fresh Linux container with ggen installed, **When** I run `ggen sync` on a sample thesis-gen project, **Then** all expected output files are generated with correct content
2. **Given** a fresh macOS environment (native or via runner), **When** I run `ggen sync` on the same sample project, **Then** the output is byte-for-byte identical to the Linux output
3. **Given** ggen is installed via different methods (cargo install, pre-built binary), **When** I run `ggen sync`, **Then** the results are identical regardless of installation method

---

### User Story 2 - Dependency Isolation via Testcontainers (Priority: P1)

As a CI/CD pipeline maintainer, I want E2E tests to run in isolated containers with all dependencies managed automatically, so tests are reproducible and don't fail due to environmental differences between CI runners.

**Why this priority**: Flaky tests due to environmental differences waste developer time and erode confidence in the test suite. Testcontainers ensure a clean, reproducible environment every time.

**Independent Test**: Run the testcontainer-based test suite on a machine with no ggen dependencies pre-installed and verify it successfully sets up the environment, runs tests, and tears down cleanly.

**Acceptance Scenarios**:

1. **Given** a CI runner with only Docker installed, **When** the E2E test suite runs, **Then** all required dependencies (Rust toolchain, ggen binary, sample projects) are provisioned automatically inside containers
2. **Given** multiple test runs on the same machine, **When** each run completes, **Then** no state leaks between runs (containers are fully cleaned up)
3. **Given** a test failure occurs, **When** the test suite exits, **Then** containers are still cleaned up and logs are captured for debugging

---

### User Story 3 - Homebrew Installation Verification (Priority: P2)

As a macOS user, I want E2E tests that verify ggen can be installed via Homebrew and works correctly after installation, so I can trust the Homebrew distribution channel.

**Why this priority**: Homebrew is the primary distribution method for macOS users. Verifying this installation path works end-to-end prevents broken releases from reaching users.

**Independent Test**: Run a test that installs ggen via `brew install` (from a tap or formula), then executes `ggen sync` and validates output.

**Acceptance Scenarios**:

1. **Given** a clean macOS environment with Homebrew installed, **When** I run `brew install ggen` (or from tap), **Then** ggen is installed and available in PATH
2. **Given** ggen is installed via Homebrew, **When** I run `ggen sync` on a sample project, **Then** the output is identical to cargo-installed ggen
3. **Given** a new ggen version is released, **When** I run `brew upgrade ggen`, **Then** the upgrade completes successfully and the new version works correctly

---

### User Story 4 - Sample Project Validation Suite (Priority: P2)

As a ggen maintainer, I want E2E tests to validate all example projects in the repository work correctly, so I catch regressions that affect real-world use cases.

**Why this priority**: Example projects serve as both documentation and regression tests. If examples break, users lose trust and documentation becomes misleading.

**Independent Test**: Run `ggen sync` on each example project (thesis-gen, etc.) and validate outputs against golden files.

**Acceptance Scenarios**:

1. **Given** the thesis-gen example project, **When** I run `ggen sync`, **Then** all LaTeX files are generated matching the golden output
2. **Given** any example project with a `ggen.toml`, **When** I run `ggen sync`, **Then** the command completes without errors and generates expected files
3. **Given** an example project is modified, **When** golden files are updated, **Then** subsequent test runs pass with the new expected output

---

### User Story 5 - CI Pipeline Integration (Priority: P3)

As a repository maintainer, I want E2E tests integrated into the GitHub Actions CI pipeline, so every PR is validated for cross-platform compatibility before merge.

**Why this priority**: Automated CI integration ensures no PR can be merged without passing cross-platform E2E tests, preventing regressions from reaching the main branch.

**Independent Test**: Create a PR that intentionally breaks Linux compatibility and verify the CI fails with a clear error message.

**Acceptance Scenarios**:

1. **Given** a PR is opened, **When** CI runs, **Then** E2E tests execute on both Linux and macOS runners
2. **Given** E2E tests fail on one platform, **When** CI completes, **Then** the PR is blocked from merge with clear failure logs
3. **Given** E2E tests pass on all platforms, **When** CI completes, **Then** the PR shows a green checkmark for E2E validation

---

### Edge Cases

- What happens when Docker is not available on the CI runner? (Test should fail gracefully with clear error message)
- How does the test handle network failures when pulling container images? (Retry with exponential backoff, fail after 3 attempts)
- What happens when a container takes longer than expected to start? (Configurable timeout with sensible default of 2 minutes)
- How does the test handle disk space constraints? (Pre-check available space, fail early with helpful message)
- What happens when ggen sync produces warnings but not errors? (Capture warnings in test output, optionally fail on warnings via flag)

## Requirements *(mandatory)*

### Functional Requirements

- **FR-001**: System MUST run `ggen sync` inside Linux containers (Ubuntu-based) and validate output
- **FR-002**: System MUST run `ggen sync` on macOS (native runner or environment) and validate output
- **FR-003**: System MUST compare outputs between platforms to ensure deterministic generation
- **FR-004**: System MUST use testcontainers-rs (or equivalent) for container lifecycle management
- **FR-005**: System MUST automatically provision all ggen dependencies inside test containers
- **FR-006**: System MUST clean up all containers and resources after test completion (success or failure)
- **FR-007**: System MUST capture and expose container logs when tests fail
- **FR-008**: System MUST support running tests locally with Docker and in CI environments
- **FR-009**: System MUST validate Homebrew installation path works on macOS
- **FR-010**: System MUST test all example projects in the repository (thesis-gen, etc.)
- **FR-011**: System MUST produce deterministic test results (same inputs = same outputs)
- **FR-012**: System MUST integrate with existing `cargo make test` workflow
- **FR-013**: System MUST support parallel test execution where container isolation allows
- **FR-014**: System MUST timeout tests that exceed maximum allowed duration (configurable, default 5 minutes per test)

### Key Entities

- **TestContainer**: Isolated Docker container running a specific OS with ggen installed
- **TestProject**: A sample ggen project (ontology + templates + ggen.toml) used for validation
- **GoldenOutput**: Expected output files for comparison against actual generated output
- **TestResult**: Outcome of a test run including pass/fail status, logs, and timing
- **Platform**: Target OS/architecture combination (linux-x86_64, darwin-x86_64, darwin-arm64)

## Success Criteria *(mandatory)*

### Measurable Outcomes

- **SC-001**: E2E tests complete successfully on both Linux and macOS with 100% pass rate before release
- **SC-002**: Test suite completes in under 10 minutes for the full cross-platform validation
- **SC-003**: Zero platform-specific bugs reach production after E2E tests are implemented (measured over 6 months)
- **SC-004**: Container cleanup is 100% reliable - no orphaned containers after test runs
- **SC-005**: Test failures provide actionable diagnostic information (logs, diffs, container state) within 30 seconds of failure
- **SC-006**: New contributors can run E2E tests locally with only Docker as a prerequisite
- **SC-007**: CI pipeline blocks PRs with E2E failures 100% of the time (no false negatives)
- **SC-008**: Generated output is byte-for-byte identical across platforms for the same input

## Assumptions

- Docker is available on all CI runners and developer machines for testcontainer execution
- macOS testing uses GitHub Actions macOS runners (native execution, not containerized)
- Homebrew formula/tap for ggen exists or will be created as part of this feature
- Example projects (thesis-gen) have stable, expected outputs that can serve as golden files
- Test containers use publicly available base images (Ubuntu LTS, etc.)
- Network access is available for pulling container images during test setup

## Out of Scope

- Windows platform testing (can be added in future iteration)
- Performance benchmarking (separate from functional E2E validation)
- Load testing or stress testing
- Testing ggen subcommands other than `sync` (focus on primary use case first)
- Creating the Homebrew formula itself (only testing the installation path)
