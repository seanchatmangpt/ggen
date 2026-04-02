# ggen v2.0 Test Strategy: Async/Sync Wrapper Architecture

## Executive Summary

This document defines the London TDD test strategy for ggen v2.0's async/sync wrapper architecture.

**Architecture Pattern:**
```
cmds (clap) → domain (async) → pure business logic
                ↓
            runtime::execute (sync wrapper for legacy code)
```

## Test Distribution (London TDD)

- **20% Integration Tests**: CLI → domain → result (end-to-end flow)
- **60% Component Tests**: Domain logic with mocked boundaries
- **20% Unit Tests**: Pure functions (runtime bridge, utilities)

## Performance Targets

- **Individual tests**: <100ms
- **Full suite**: <1s total execution time
- **Test parallelization**: Maximize with `cargo test --jobs=N`

## Architecture Layers

### Layer 1: Entry Point (`src/main.rs`)
- **Responsibility**: Tokio runtime creation, error handling
- **Test Strategy**: Integration tests only (binary execution)
- **Key Path**: `#[tokio::main] async fn main()`

### Layer 2: CLI Matching (`lib.rs::cli_match`)
- **Responsibility**: Clap parsing, config merging, OTEL initialization
- **Test Strategy**: Component tests with mocked subcommands
- **Key Paths**:
  - OTEL enabled/disabled branches
  - Config loading (from file, from args)
  - Telemetry initialization/shutdown

### Layer 3: Commands (`cmds/`)
- **Responsibility**: Clap integration (noun-verb structure)
- **Test Strategy**: Component tests with mocked domain
- **Key Pattern**: `async fn run(args: &Args) -> Result<()>`
- **Examples**: `cmds/doctor.rs`, `cmds/ai/`, `cmds/template/`

### Layer 4: Domain (`cli/src/domain/`)
- **Responsibility**: Pure async business logic
- **Test Strategy**: Component tests with extensive mocking
- **Key Pattern**: `pub async fn run_*(args...) -> Result<Output>`
- **Examples**: `domain/utils/doctor.rs`, `domain/ai/analyze.rs`

### Layer 5: Runtime Bridge (`runtime.rs`)
- **Responsibility**: Async→sync wrapper for legacy code
- **Test Strategy**: Unit tests (isolated function testing)
- **Key Function**: `pub fn execute<F: Future>(future: F) -> Result<()>`

## Test Categories

### 1. Unit Tests (20% - Fast, Isolated)

**Target**: Pure functions without external dependencies

```rust
// tests/london_tdd/v2_architecture/unit/runtime_bridge_test.rs
#[test]
fn test_runtime_execute_success()
#[test]
fn test_runtime_execute_failure()
#[test]
fn test_runtime_execute_tokio_panic()
#[test]
fn test_runtime_execute_performance() // <10ms
```

**Coverage**:
- `runtime::execute()` - All branches
- Utility functions (path parsing, validation)
- Error transformation logic

### 2. Component Tests (60% - Mocked Boundaries)

**Target**: Domain logic with mocked I/O, filesystem, network

```rust
// tests/london_tdd/v2_architecture/component/doctor_domain_test.rs
#[tokio::test]
async fn test_doctor_all_checks_passing()
#[tokio::test]
async fn test_doctor_missing_rust_toolchain()
#[tokio::test]
async fn test_doctor_platform_specific_instructions()
#[tokio::test]
async fn test_doctor_verbose_mode()
#[tokio::test]
async fn test_doctor_specific_check()
```

**Mocking Strategy**:
- **Mock Trait**: `SystemCommandExecutor` for command execution
- **Mock FS**: Virtual filesystem for file operations
- **Mock Network**: Canned responses for HTTP requests
- **Mock Time**: Controlled time for timeout testing

**Coverage**:
- All domain functions (`domain/**/*.rs`)
- All branches (success, failure, edge cases)
- All error paths

### 3. Integration Tests (20% - End-to-End)

**Target**: Full CLI → domain → output flow

```rust
// tests/london_tdd/v2_architecture/integration/cli_e2e_test.rs
#[tokio::test]
async fn test_doctor_command_e2e()
#[tokio::test]
async fn test_ai_analyze_command_e2e()
#[tokio::test]
async fn test_template_new_command_e2e()
#[tokio::test]
async fn test_otel_integration_e2e()
```

**Test Approach**:
- Use `run_for_node()` for programmatic CLI execution
- Capture stdout/stderr
- Verify exit codes
- Check side effects (files created, logs generated)

## Testing the Async/Sync Boundary

### Pattern 1: cmds → domain (No Wrapper Needed)

```rust
// cmds/doctor.rs
pub async fn run(args: &DoctorArgs) -> Result<()> {
    crate::domain::utils::doctor::run_doctor(
        args.verbose,
        args.check.as_deref(),
        args.env,
    ).await
}
```

**Test Strategy**:
- Component test with mocked domain
- Verify argument transformation
- Verify error propagation

### Pattern 2: Sync wrapper → runtime → domain (Legacy)

```rust
// commands/utils/doctor.rs (deprecated)
pub fn run_sync(verbose: bool) -> Result<()> {
    crate::runtime::execute(async move {
        crate::domain::utils::doctor::run_doctor(verbose, None, false).await
    })
}
```

**Test Strategy**:
- Unit test for runtime::execute
- Component test for sync wrapper
- Verify Tokio runtime creation/cleanup

## Mock Implementation Patterns

### Mock 1: System Command Executor

```rust
use mockall::automock;

#[automock]
pub trait SystemCommandExecutor: Send + Sync {
    fn execute(&self, cmd: &str, args: Vec<&str>) -> Result<String>;
}
```

### Mock 2: Filesystem Operations

```rust
#[automock]
pub trait FileSystem: Send + Sync {
    async fn read(&self, path: &Path) -> Result<String>;
    async fn write(&self, path: &Path, content: &str) -> Result<()>;
    async fn exists(&self, path: &Path) -> bool;
}
```

### Mock 3: HTTP Client

```rust
#[automock]
pub trait HttpClient: Send + Sync {
    async fn get(&self, url: &str) -> Result<HttpResponse>;
    async fn post(&self, url: &str, body: &str) -> Result<HttpResponse>;
}
```

## Performance Testing

### Micro-Benchmarks (Individual Tests)

```rust
#[test]
fn test_runtime_execute_performance() {
    let start = std::time::Instant::now();

    let result = runtime::execute(async {
        Ok(())
    });

    let elapsed = start.elapsed();
    assert!(elapsed.as_millis() < 10, "Runtime bridge took {:?}", elapsed);
    assert!(result.is_ok());
}
```

### Suite-Level Performance

```bash
# Run with timing
cargo test --release -- --test-threads=8 --nocapture | grep "test result"

# Expected output:
# test result: ok. 45 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out; finished in 0.87s
```

## Test Organization

```
tests/
├── london_tdd/
│   └── v2_architecture/
│       ├── unit/
│       │   ├── mod.rs
│       │   ├── runtime_bridge_test.rs        (10 tests, <50ms total)
│       │   └── error_handling_test.rs         (8 tests, <30ms total)
│       ├── component/
│       │   ├── mod.rs
│       │   ├── doctor_domain_test.rs          (15 tests, <300ms total)
│       │   ├── ai_analyze_test.rs             (12 tests, <250ms total)
│       │   └── template_new_test.rs           (10 tests, <200ms total)
│       └── integration/
│           ├── mod.rs
│           ├── cli_e2e_test.rs                (8 tests, <350ms total)
│           └── otel_integration_test.rs       (5 tests, <150ms total)
```

## Test Execution Plan

### Phase 1: Unit Tests (Immediate Feedback)
```bash
cargo test london_tdd::v2_architecture::unit --lib
# Expected: ~18 tests, <100ms
```

### Phase 2: Component Tests (Domain Logic)
```bash
cargo test london_tdd::v2_architecture::component --lib
# Expected: ~37 tests, <750ms
```

### Phase 3: Integration Tests (Full Flow)
```bash
cargo test london_tdd::v2_architecture::integration
# Expected: ~13 tests, <500ms
```

### Full Suite
```bash
cargo test london_tdd::v2_architecture
# Expected: ~68 tests, <1.35s (target: <1s after optimization)
```

## Quality Metrics

### Coverage Targets
- **Unit tests**: 100% of runtime bridge
- **Component tests**: 90% of domain logic
- **Integration tests**: 80% of CLI surface area

### Reliability Targets
- **Flaky test rate**: 0%
- **Test stability**: 100% pass rate across 10 consecutive runs
- **Performance variance**: <5% between runs

### Maintainability Targets
- **Mock setup**: <5 lines per test
- **Test clarity**: Each test verifies ONE behavior
- **Test naming**: `test_<function>_<scenario>_<expected_outcome>`

## Edge Cases & Error Paths

### Runtime Bridge
- [x] Successful async execution
- [x] Async function returning error
- [x] Tokio runtime creation failure
- [x] Panic during async execution
- [x] Timeout scenarios

### Domain Logic
- [x] All checks passing (happy path)
- [x] Missing dependencies (partial failure)
- [x] Platform-specific logic (macOS, Linux, Windows)
- [x] Verbose vs. normal output
- [x] Specific check execution

### CLI Integration
- [x] Valid arguments
- [x] Invalid arguments (clap errors)
- [x] OTEL enabled/disabled
- [x] Config file present/absent
- [x] Signal handling (CTRL+C)

## Continuous Testing

### Pre-commit Hook
```bash
#!/bin/bash
# .git/hooks/pre-commit
cargo test london_tdd::v2_architecture --quiet
if [ $? -ne 0 ]; then
    echo "❌ Tests failed. Commit aborted."
    exit 1
fi
```

### CI Pipeline
```yaml
- name: Unit Tests
  run: cargo test london_tdd::v2_architecture::unit --lib

- name: Component Tests
  run: cargo test london_tdd::v2_architecture::component --lib

- name: Integration Tests
  run: cargo test london_tdd::v2_architecture::integration

- name: Performance Validation
  run: |
    TIME=$(cargo test london_tdd::v2_architecture --release 2>&1 | grep "finished in" | awk '{print $4}')
    if (( $(echo "$TIME > 1.0" | bc -l) )); then
      echo "❌ Test suite too slow: ${TIME}s (target: <1s)"
      exit 1
    fi
```

## Success Criteria

✅ All 68 tests passing (100% success rate)
✅ Full suite execution <1s
✅ No flaky tests (10 consecutive runs)
✅ All edge cases covered
✅ Mocks properly isolated
✅ Performance targets met (<100ms per test)
