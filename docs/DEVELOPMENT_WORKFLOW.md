<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [Development Workflow](#development-workflow)
  - [Overview](#overview)
  - [Development Commands](#development-commands)
    - [Required: Use `cargo make` Commands](#required-use-cargo-make-commands)
    - [Timeout SLAs](#timeout-slas)
  - [Testing Strategy](#testing-strategy)
    - [Chicago TDD (Test-Driven Development)](#chicago-tdd-test-driven-development)
      - [Test Macros](#test-macros)
      - [Testing Principles](#testing-principles)
      - [Test Execution](#test-execution)
  - [Error Handling Standards](#error-handling-standards)
    - [Error Types](#error-types)
    - [Error Construction](#error-construction)
    - [Error Handling Patterns](#error-handling-patterns)
  - [Andon Signals (Quality Assurance)](#andon-signals-quality-assurance)
    - [Signal Types](#signal-types)
    - [Andon Workflow](#andon-workflow)
    - [Verification Before Completion](#verification-before-completion)
  - [Code Quality Standards](#code-quality-standards)
    - [Prohibited Patterns](#prohibited-patterns)
    - [Required Patterns](#required-patterns)
  - [Continuous Improvement (Kaizen)](#continuous-improvement-kaizen)
    - [Kaizen Workflow](#kaizen-workflow)
    - [Improvement Criteria](#improvement-criteria)
  - [Documentation Standards](#documentation-standards)
    - [Module Documentation](#module-documentation)
    - [Public API Documentation](#public-api-documentation)
    - [Documentation Waste Prevention (Muda Elimination)](#documentation-waste-prevention-muda-elimination)
  - [Git Workflow](#git-workflow)
    - [Pre-Commit Hooks](#pre-commit-hooks)
    - [Pre-Push Hooks](#pre-push-hooks)
  - [Performance SLOs](#performance-slos)
  - [Resources](#resources)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# Development Workflow

This document describes the development workflow for ggen, including testing practices, error handling standards, and quality assurance processes.

## Overview

The ggen development workflow follows Lean Six Sigma principles with a focus on:
- **Continuous Improvement (Kaizen)**: Small, incremental improvements
- **Quality Assurance (Andon Signals)**: Visual problem indicators with "stop the line" workflow
- **Deterministic Testing**: Chicago TDD with behavior verification
- **Error Prevention**: Type-safe error handling throughout

## Development Commands

### Required: Use `cargo make` Commands

**CRITICAL**: All development workflows MUST use `cargo make` commands. **NEVER use direct `cargo` commands**.

```bash
# Quick feedback (5s timeout)
cargo make check          # Compile check
cargo make test-unit       # Unit tests only
cargo make lint            # Clippy linting

# Full validation (longer timeouts)
cargo make test            # All tests (10s unit + 30s integration)
cargo make pre-commit      # Format + lint + test-unit
cargo make ci              # Full CI pipeline

# Specialized commands
cargo make timeout-check   # Verify timeout command exists
cargo make test-timings    # Identify slow tests
cargo make completions     # Generate shell completions
```

### Timeout SLAs

All CLI commands have timeout wrappers to prevent indefinite hangs:

- **Quick checks**: `timeout 5s` (cargo check, cargo fmt, cargo clippy)
- **Compilation**: `timeout 10s` (cargo build debug)
- **Release builds**: `timeout 30s` (cargo build --release)
- **Unit tests**: `timeout 10s` (cargo test --lib)
- **Integration tests**: `timeout 30s` (cargo test --test)
- **Pre-push hooks**: `timeout 30s` (cargo make check-pre-push) - Longer timeout for lock contention

## Testing Strategy

### Chicago TDD (Test-Driven Development)

ggen uses Chicago School TDD with state-based testing and behavior verification.

#### Test Macros

Use `chicago_tdd_tools` macros instead of standard `#[test]` attributes:

```rust
use chicago_tdd_tools::prelude::*;  // prelude::* imports all common macros and types

// Synchronous test
test!(test_name, {
    // Arrange
    let input = "test";

    // Act
    let result = function_under_test(input);

    // Assert - verify observable outputs
    assert_eq!(result, expected_value);
});

// Async test
async_test!(test_async_function, {
    // Arrange
    let input = "test";

    // Act
    let result = async_function(input).await;

    // Assert
    assert_eq!(result, expected_value);
});

// Test with Result handling
test!(test_with_result, {
    // Arrange
    let result: Result<u32, String> = Ok(42);

    // Act & Assert
    assert_ok!(&result, "Result should be Ok");
    if let Ok(value) = result {
        assert_eq!(value, 42);
    }
});
```

**Note**: Assertion macros like `assert_ok!` and `assert_err!` are exported with `#[macro_export]`, so they're available at crate root automatically. You can use them directly or with fully qualified paths like `chicago_tdd_tools::assert_ok!`.

#### Testing Principles

1. **State-Based Testing**: Verify outputs and state changes, not implementation details
2. **Real Collaborators**: Use real objects, minimize mocks
3. **Behavior Verification**: Verify what code does (observable outputs/state changes)
4. **AAA Pattern**: Arrange-Act-Assert required
5. **No Meaningless Tests**: Tests must verify observable outputs, not just `assert_ok!()`

#### Test Execution

```bash
# Single-threaded for determinism
cargo make test-single-threaded

# Fixed seed for reproducibility
cargo make deterministic

# Identify slow tests
cargo make test-timings
```

### Test Organization

#### Test Structure

```
ggen/
├── crates/
│   ├── ggen-ai/
│   │   ├── src/
│   │   ├── tests/              # Integration tests
│   │   │   ├── integration_ai.rs
│   │   │   └── llm_integration.rs
│   │   └── benches/            # Performance tests
│   │       └── agent_performance.rs
│   │
│   └── ggen-cli/
│       └── tests/
│           ├── conventions/    # Test by feature area
│           ├── watch_tests.rs
│           └── integration_e2e.rs
│
├── ontologies/                 # Test fixtures
│   └── test-fixtures/
│
└── tests/                      # Top-level E2E tests
    └── scenarios/
```

#### Test Naming Conventions

```rust
// Unit test for a specific function
#[test]
fn test_observation_schema_validation() { }

// Integration test for feature area
#[test]
fn test_kernel_decision_with_multiple_constraints() { }

// End-to-end test for user workflow
#[test]
#[ignore = "E2E test - requires setup"]
fn test_complete_code_generation_workflow() { }

// Async test
#[tokio::test]
async fn test_watch_service_detects_file_changes() { }

// Property-based test
#[quickcheck]
fn prop_observation_remains_valid(obs: Observation) -> bool { }
```

### Running Tests

#### All Tests

```bash
# Run all tests
cargo test --all

# Run with output
cargo test --all -- --nocapture

# Run with specific number of threads
cargo test --all -- --test-threads=1
```

#### Specific Crate

```bash
# Test single crate
cargo test -p ggen-core

# Test multiple crates
cargo test -p ggen-core -p ggen-domain
```

#### Specific Test

```bash
# By function name
cargo test test_observation_creation

# By module path
cargo test ggen_domain::mape_k::

# With pattern matching
cargo test kernel
```

#### Test Filters

```bash
# Run ignored tests only
cargo test --all -- --ignored

# Run only unit tests (skip integration tests)
cargo test --lib

# Run only integration tests
cargo test --test '*'

# Run only doc tests
cargo test --doc
```

#### Watch Mode for Development

```bash
# Watch and re-test on file changes
cargo watch -x "test --lib"

# Watch specific crate
cargo watch -w crates/ggen-core -x "test -p ggen-core"
```

### Writing Tests

#### Unit Test Example

```rust
#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_invariant_check_passes_for_valid_data() {
        // ARRANGE
        let invariant = Invariant::new(
            InvariantId::new(),
            "test_invariant",
            "Value must be positive",
            vec!["value".to_string()],
        );

        let data = HashMap::from([
            ("value".to_string(), "42".to_string()),
        ]);

        // ACT
        let result = invariant.check(&data);

        // ASSERT
        assert!(result.is_ok());
    }

    #[test]
    fn test_invariant_check_fails_for_invalid_data() {
        // ARRANGE
        let invariant = Invariant::new(
            InvariantId::new(),
            "test_invariant",
            "Value must be positive",
            vec!["value".to_string()],
        );

        let data = HashMap::from([
            ("value".to_string(), "-42".to_string()),
        ]);

        // ACT
        let result = invariant.check(&data);

        // ASSERT
        assert!(result.is_err());
    }
}
```

#### Integration Test Example

```rust
// tests/integration_workflow.rs

use ggen::GeneratorConfig;

#[test]
fn test_complete_generation_workflow() {
    // Setup
    let config = GeneratorConfig::default();
    let ontology_path = "test-fixtures/simple.ttl";
    let template_path = "templates/basic.jinja2";

    // Execute
    let result = generate_with_config(ontology_path, template_path, config);

    // Verify
    assert!(result.is_ok());
    let output = result.unwrap();
    assert!(!output.generated_files.is_empty());
}
```

#### Async Test Example

```rust
#[tokio::test]
async fn test_async_kernel_decision() {
    // ARRANGE
    let kernel = create_test_kernel().await;
    let observation = create_test_observation();

    // ACT
    let decision = kernel.decide(&observation).await;

    // ASSERT
    assert!(decision.is_ok());
}
```

#### Property-based Test Example

```rust
use quickcheck::{quickcheck, TestResult};

quickcheck! {
    fn prop_observation_serialization_roundtrip(obs: Observation) -> bool {
        // Serialize and deserialize
        let json = serde_json::to_string(&obs).unwrap();
        let restored: Observation = serde_json::from_str(&json).unwrap();

        // Should be equal
        obs == restored
    }
}
```

### Test Fixtures

#### Creating Test Data

```rust
// Helper function for common test setup
fn create_test_observation() -> Observation {
    Observation::new(
        ObservationType::FileChange,
        HashMap::from([
            ("path".to_string(), "test.rs".to_string()),
            ("change_type".to_string(), "modified".to_string()),
        ]),
    )
}

// Reusable fixture
struct TestFixture {
    ontology_path: String,
    template_path: String,
    output_dir: TempDir,
}

impl TestFixture {
    fn new() -> Self {
        Self {
            ontology_path: "test-fixtures/default.ttl".to_string(),
            template_path: "templates/default.jinja2".to_string(),
            output_dir: TempDir::new().unwrap(),
        }
    }
}
```

#### Test Fixture Ontologies

Location: `ontologies/test-fixtures/`

```
test-fixtures/
├── minimal.ttl           # Smallest valid ontology
├── complete.ttl          # Full feature ontology
├── invalid.ttl           # Intentionally invalid
├── large.ttl             # Performance test data
└── specialized/
    ├── rust-microservice.ttl
    ├── academic-paper.ttl
    └── api-gateway.ttl
```

### Coverage Analysis

#### Generate Coverage Report

```bash
# Install tarpaulin
cargo install cargo-tarpaulin

# Generate coverage
cargo tarpaulin --all --out Html --output-dir coverage/

# View in browser
open coverage/index.html
```

#### Coverage Targets

Current targets:
- **ggen-core**: 85%+ coverage
- **ggen-domain**: 80%+ coverage
- **ggen-ai**: 75%+ coverage
- **ggen-cli**: 70%+ coverage

#### Checking Coverage

```bash
# Check coverage on specific crate
cargo tarpaulin -p ggen-core --timeout 300

# With verbose output
cargo tarpaulin -p ggen-core -v --timeout 300
```

### Benchmarking

#### Criterion Benchmarks

```bash
# Run benchmarks
cargo bench --all

# Specific benchmark
cargo bench -- kernel_decision

# With verbose output
cargo bench --all -- --verbose
```

#### Benchmark Example

```rust
use criterion::{black_box, criterion_group, criterion_main, Criterion};

fn benchmark_observation_parsing(c: &mut Criterion) {
    c.bench_function("parse_small_observation", |b| {
        b.iter(|| {
            let obs = black_box(create_test_observation());
            serde_json::to_string(&obs)
        })
    });
}

criterion_group!(benches, benchmark_observation_parsing);
criterion_main!(benches);
```

### Common Testing Issues

#### Issue: Test Hangs

```rust
// Solution: Use timeout
#[tokio::test]
#[timeout::secs(5)]
async fn test_with_timeout() { }
```

#### Issue: Flaky Tests

```rust
// Problem: Race condition
#[tokio::test]
async fn test_concurrent_access() {
    // BAD: Race condition
    // let result = handle_a.await;
    // assert_eq!(result, expected);

    // GOOD: Use synchronization
    let (tx, rx) = tokio::sync::mpsc::channel(1);
    let result = tokio::select! {
        _ = tokio::time::sleep(Duration::from_secs(1)) => panic!("timeout"),
        msg = rx.recv() => msg,
    };
    assert_eq!(result, Some(expected));
}
```

#### Issue: Test Isolation

```rust
// Problem: Tests interfere with each other
#[test]
fn test_a() {
    GLOBAL_STATE.set(true);
    // ...
}

// Solution: Use separate threads or fixtures
#[test]
fn test_with_isolation() {
    let _guard = GLOBAL_STATE.lock();
    GLOBAL_STATE.set(true);
    // Test runs with exclusive lock
}
```

### Test Data Management

#### Managing Large Test Files

```bash
# Store large test files outside repo (optional)
# Use ggen download-test-fixtures command

# Or use git LFS
git lfs install
git lfs track "test-fixtures/large/*.ttl"
```

#### Generating Test Data

```bash
# Generate synthetic ontology for testing
ggen generate-test-fixture \
    --type ontology \
    --size large \
    --output test-fixtures/generated/
```

### Documentation Tests

#### Doc Test Example

```rust
/// Parse an RDF ontology file
///
/// # Examples
///
/// ```ignore
/// use ggen::ontology::Ontology;
///
/// let ontology = Ontology::from_file("ontology.ttl")?;
/// assert!(!ontology.is_empty());
/// # Ok::<_, Box<dyn std::error::Error>>(())
/// ```
pub fn from_file(path: &str) -> Result<Ontology> {
    // Implementation
}
```

Run documentation tests:

```bash
cargo test --doc
```

### Test Best Practices

#### ✓ Do

- **Use descriptive test names**: Name tests by what they test, not how
- **Follow AAA pattern**: Arrange, Act, Assert
- **Test one thing**: Each test should verify one behavior
- **Use fixtures**: Reuse setup code in tests
- **Test error cases**: Don't just test the happy path
- **Keep tests fast**: Aim for millisecond execution
- **Isolate dependencies**: Mock external services

#### ✗ Don't

- **Use `panic!` for assertions**: Use `assert!`, `assert_eq!`, etc.
- **Test implementation details**: Test public APIs only
- **Create test interdependencies**: Tests should be independent
- **Ignore flaky tests**: Fix root cause, don't ignore
- **Mix units and integration**: Keep layers separate
- **Test library dependencies**: Assume they work correctly

### Fuzzing

#### Fuzzing with cargo-fuzz

```bash
cargo install cargo-fuzz
cargo fuzz list

# Run fuzzer
cargo fuzz run ontology_parser

# With specific corpus
cargo fuzz run ontology_parser corpus/
```

#### Fuzzing Example

```rust
// fuzz/fuzz_targets/ontology_parser.rs

#![no_main]
use libfuzzer_sys::fuzz_target;
use ggen::ontology::Ontology;

fuzz_target!(|data: &[u8]| {
    // Don't crash on any input
    if let Ok(s) = std::str::from_utf8(data) {
        let _ = Ontology::parse(s);
    }
});
```

## Error Handling Standards

### Error Types

**Libraries** (`ggen-core`, `ggen-utils`, `ggen-domain`):
- Use `ggen_utils::error::Result<T>` for all fallible operations
- Use `ggen_utils::error::Error` for error construction
- **NEVER** use `anyhow::Result` or `anyhow::anyhow!()` in library code

**Binaries** (`ggen-cli`):
- May use `anyhow::Result` for CLI error handling
- Use `ggen_utils::error::Result` when calling library functions

### Error Construction

```rust
use ggen_utils::error::{Error, Result};

// Simple error
return Err(Error::new("Operation failed"));

// Error with context
return Err(Error::with_context("Failed to read file", &path.to_string_lossy()));

// Error with source
.map_err(|e| Error::with_source("Failed to process", Box::new(e)))?;

// Error with formatted message
return Err(Error::new(&format!("Invalid value: {}", value)));
```

### Error Handling Patterns

**DO**:
- Use `Result<T, E>` for all fallible operations
- Provide clear, actionable error messages
- Include context (paths, values, etc.) in error messages
- Chain errors with `Error::with_source()` when appropriate

**DON'T**:
- Use `unwrap()` or `expect()` in production code
- Use `anyhow` in library code
- Swallow errors silently
- Use `panic!()` except in unreachable code

## Andon Signals (Quality Assurance)

Andon signals are visual problem indicators that trigger a "stop the line" workflow.

### Signal Types

1. **CRITICAL (Red)** - Must stop immediately:
   - Compiler errors (`error[E...]`)
   - Test failures (`test ... FAILED`)

2. **HIGH (Yellow)** - Should stop:
   - Compiler warnings (`warning:`)
   - Linting errors (clippy warnings/errors)

3. **MEDIUM (Yellow)** - Investigate:
   - Performance regressions
   - Code quality warnings

### Andon Workflow

1. **Monitor**: Run `cargo make check`, `cargo make test`, `cargo make lint`
2. **Stop**: When signal appears, immediately stop current work
3. **Investigate**: Use root cause analysis (5 Whys) to understand why signal appeared
4. **Fix**: Address root cause, not just symptom
5. **Verify**: Re-run checks to confirm signal cleared

### Verification Before Completion

Before marking any task as complete:

```bash
# Verify timeout command exists
cargo make timeout-check

# Check for compiler errors (CRITICAL)
cargo make check

# Run tests (CRITICAL)
cargo make test

# Check for linting errors (HIGH)
cargo make lint

# Verify performance SLOs
cargo make slo-check
```

**All signals must be cleared before work continues.**

## Code Quality Standards

### Prohibited Patterns

- **NEVER** use direct `cargo` commands - always use `cargo make`
- **NEVER** use `unwrap()` or `expect()` in production code
- **NEVER** use `anyhow` in library code
- **NEVER** ignore Andon signals
- **NEVER** proceed with signals present
- **NEVER** suppress or hide signals without fixing root cause

### Required Patterns

- Real implementations - No placeholders or stubs
- Error handling - `Result<T, E>` for all fallible operations
- Feature gating - `#[cfg(feature = "...")]` for optional dependencies
- Test verification - All code must be testable and tested
- Behavior verification - Tests must verify observable outputs/state changes

## Continuous Improvement (Kaizen)

### Kaizen Workflow

1. **Identify**: Find small, focused improvement opportunities
2. **Plan**: Design minimal change that improves code
3. **Do**: Implement the improvement
4. **Check**: Verify improvement achieved its goal
5. **Act**: Standardize the improvement if successful

### Improvement Criteria

- **Small**: Can be done in minutes, not hours
- **Focused**: Addresses one specific thing
- **Safe**: Low risk of breaking things
- **Value**: Adds clarity, performance, or maintainability

## Documentation Standards

### Module Documentation

All modules must have comprehensive `//!` documentation:

```rust
//! Module description
//!
//! This module provides [purpose]. It includes:
//!
//! - **Feature 1**: Description
//! - **Feature 2**: Description
//!
//! ## Examples
//!
//! ```rust,no_run
//! use crate::module::Type;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let instance = Type::new()?;
//! # Ok(())
//! # }
//! ```
```

### Public API Documentation

All public APIs must have:
- Description of what the function/type does
- `# Examples` section with runnable doctests when possible
- Error documentation for `Result`-returning functions
- Parameter and return value documentation

### Documentation Waste Prevention (Muda Elimination)

Documentation work-in-progress files should be actively managed to prevent waste accumulation:

**Work-in-Progress Documentation**:
- Place intermediate analysis and reports in `docs/wip/`
- Use `_CURRENT` suffix for active tracking documents (e.g., `KAIZEN_IMPROVEMENT_CURRENT.md`)
- Remove outdated versions immediately when creating new ones

**When to Archive**:
- ✅ Work is complete and documented in code (improvements applied)
- ✅ Document is superseded by a newer version
- ✅ Information is consolidated into permanent documentation

**When NOT to Keep**:
- ❌ Historical analysis reports for completed work
- ❌ Intermediate work summaries that are no longer referenced
- ❌ Gemba walks and FMEA analyses after action items are implemented
- ❌ Duplicate files (keep only the `_CURRENT` version)

**Archival Schedule**:
- Monthly review: Check `docs/wip/` for outdated documents
- Remove files immediately after work is completed and verified
- Keep documentation clean to reduce cognitive load
- Prevent WIP directory from becoming permanent dumping ground

## Git Workflow

### Pre-Commit Hooks

Pre-commit hooks run:
- Code formatting (`cargo fmt --check`)
- Linting (`cargo clippy`)
- Quick compilation check

### Pre-Push Hooks

Pre-push hooks run:
- Full test suite
- Comprehensive linting
- Performance SLO checks

**All hooks must pass before pushing.**

## Performance SLOs

- **First build**: ≤ 15s
- **Incremental build**: ≤ 2s
- **RDF processing**: ≤ 5s for 1k+ triples
- **Generation memory**: ≤ 100MB
- **CLI scaffolding**: ≤ 3s end-to-end

Verify SLOs with:
```bash
cargo make slo-check
```

## Resources

- [Rust Standards](.cursor/rules/rust-standards.mdc)
- [Build System Practices](.cursor/rules/build-system-practices.mdc)
- [Determinism Standards](.cursor/rules/determinism-standards.mdc)
- [Contributing Guide](../CONTRIBUTING.md)

