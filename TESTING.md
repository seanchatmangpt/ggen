# Testing Guide for ggen

This guide covers ggen's testing strategy, test organization, and how to write effective tests.

## Testing Philosophy

ggen follows a multi-layered testing approach:

1. **Unit Tests**: Test individual functions and modules in isolation
2. **Integration Tests**: Test subsystem interactions
3. **End-to-End Tests**: Test complete workflows
4. **Property-based Tests**: Verify invariants hold for all inputs
5. **Performance Tests**: Ensure optimization targets are met

## Test Organization

### Test Structure

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

### Test Naming Conventions

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

## Running Tests

### All Tests

```bash
# Run all tests
cargo test --all

# Run with output
cargo test --all -- --nocapture

# Run with specific number of threads
cargo test --all -- --test-threads=1
```

### Specific Crate

```bash
# Test single crate
cargo test -p ggen-core

# Test multiple crates
cargo test -p ggen-core -p ggen-domain
```

### Specific Test

```bash
# By function name
cargo test test_observation_creation

# By module path
cargo test ggen_domain::mape_k::

# With pattern matching
cargo test kernel
```

### Test Filters

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

### Watch Mode for Development

```bash
# Watch and re-test on file changes
cargo watch -x "test --lib"

# Watch specific crate
cargo watch -w crates/ggen-core -x "test -p ggen-core"
```

## Writing Tests

### Unit Test Example

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

### Integration Test Example

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

### Async Test Example

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

### Property-based Test Example

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

## Test Fixtures

### Creating Test Data

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

### Test Fixture Ontologies

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

## Coverage Analysis

### Generate Coverage Report

```bash
# Install tarpaulin
cargo install cargo-tarpaulin

# Generate coverage
cargo tarpaulin --all --out Html --output-dir coverage/

# View in browser
open coverage/index.html
```

### Coverage Targets

Current targets:
- **ggen-core**: 85%+ coverage
- **ggen-domain**: 80%+ coverage
- **ggen-ai**: 75%+ coverage
- **ggen-cli**: 70%+ coverage

### Checking Coverage

```bash
# Check coverage on specific crate
cargo tarpaulin -p ggen-core --timeout 300

# With verbose output
cargo tarpaulin -p ggen-core -v --timeout 300
```

## Continuous Integration Tests

All tests run automatically on:
- Pull requests
- Commits to main/develop
- Nightly builds

### GitHub Actions Workflow

Tests run on:
- **Linux** (Ubuntu 22.04): primary platform
- **macOS**: supported platform
- **Windows**: experimental

Rust versions tested:
- MSRV (1.70.0)
- Stable
- Beta
- Nightly

## Benchmarking

### Criterion Benchmarks

```bash
# Run benchmarks
cargo bench --all

# Specific benchmark
cargo bench -- kernel_decision

# With verbose output
cargo bench --all -- --verbose
```

### Benchmark Example

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

## Common Testing Issues

### Issue: Test Hangs

```rust
// Solution: Use timeout
#[tokio::test]
#[timeout::secs(5)]
async fn test_with_timeout() { }
```

### Issue: Flaky Tests

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

### Issue: Test Isolation

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

## Test Data Management

### Managing Large Test Files

```bash
# Store large test files outside repo (optional)
# Use ggen download-test-fixtures command

# Or use git LFS
git lfs install
git lfs track "test-fixtures/large/*.ttl"
```

### Generating Test Data

```bash
# Generate synthetic ontology for testing
ggen generate-test-fixture \
    --type ontology \
    --size large \
    --output test-fixtures/generated/
```

## Documentation Tests

### Doc Test Example

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

## Test Best Practices

### ✓ Do

- **Use descriptive test names**: Name tests by what they test, not how
- **Follow AAA pattern**: Arrange, Act, Assert
- **Test one thing**: Each test should verify one behavior
- **Use fixtures**: Reuse setup code in tests
- **Test error cases**: Don't just test the happy path
- **Keep tests fast**: Aim for millisecond execution
- **Isolate dependencies**: Mock external services

### ✗ Don't

- **Use `panic!` for assertions**: Use `assert!`, `assert_eq!`, etc.
- **Test implementation details**: Test public APIs only
- **Create test interdependencies**: Tests should be independent
- **Ignore flaky tests**: Fix root cause, don't ignore
- **Mix units and integration**: Keep layers separate
- **Test library dependencies**: Assume they work correctly

## Fuzzing

### Fuzzing with cargo-fuzz

```bash
cargo install cargo-fuzz
cargo fuzz list

# Run fuzzer
cargo fuzz run ontology_parser

# With specific corpus
cargo fuzz run ontology_parser corpus/
```

### Fuzzing Example

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

## Test Reporting

### GitHub Integration

Tests are automatically reported in:
- PR checks
- Commit status
- Workflow runs
- Release notes

## Getting Help

For testing questions:
- Check [Testing Discussion](https://github.com/seanchatmangpt/ggen/discussions?discussions_q=label%3Atesting)
- Review [CONTRIBUTING.md](CONTRIBUTING.md)
- Open an [Issue](https://github.com/seanchatmangpt/ggen/issues) with `[TEST]` prefix
