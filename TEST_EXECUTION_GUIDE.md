# ggen-core Test Execution Guide

**Reference**: Complete commands for testing ggen-core
**Date**: 2026-01-25
**Methodology**: Chicago TDD with Poka-Yoke enforcement

---

## Quick Reference Commands

### Andon Signals (Stop-the-Line Validation)

#### RED Signal (CRITICAL - STOP)

```bash
# 1. Compiler errors - STOP if found
timeout 30s cargo make check
# Expected: ✓ PASS (no error[E...] in output)

# 2. Test failures - STOP if found
timeout 300s cargo test --lib --all
# Expected: ✓ test result: ok (all tests pass)
```

#### YELLOW Signal (HIGH - INVESTIGATE)

```bash
# 3. Compiler warnings - INVESTIGATE
timeout 60s cargo clippy --all -- -D warnings
# Expected: ✓ PASS (no warnings)

# 4. Performance degradation - CHECK
timeout 15s cargo make slo-check
# Expected: ✓ Build ≤15s, Incremental ≤2s, RDF ≤5s
```

---

## Test Suite Commands

### Full Unit Test Suite

```bash
# Run all library tests (standard)
timeout 300s cargo test --lib --all

# Run with detailed output
timeout 300s cargo test --lib --all -- --nocapture

# Run with logging
RUST_LOG=debug timeout 300s cargo test --lib --all

# Single-threaded (determinism verification)
cargo test --lib -- --test-threads=1

# Show test names without running
cargo test --lib -- --list
```

### Module-Specific Tests

```bash
# Graph module tests
cargo test --lib graph::

# Security tests
cargo test --lib security::

# Validation tests (SHACL)
cargo test --lib validation::

# Config tests
cargo test --lib config::

# Lifecycle tests
cargo test --lib lifecycle::

# E2E tests
cargo test --lib e2e_tests::

# Poka-Yoke tests
cargo test --lib poka_yoke::
```

### Specific Test Functions

```bash
# Run single test
cargo test --lib test_persistent_store_creation

# Run tests matching pattern
cargo test --lib graph_store

# Run specific test with output
cargo test --lib test_command_whitelist -- --nocapture
```

### Integration Tests

```bash
# All integration tests
timeout 300s cargo test --test '*'

# Specific integration test
cargo test --test marketplace_tests_main

# E2E tests
cargo test --test swarm_e2e_tests

# Watch mode tests
cargo test --test watch_mode_tests
```

### Benchmark Execution

```bash
# Run all benchmarks
cargo bench --benches

# Specific benchmark
cargo bench --bench template_benchmarks

# With detailed output
cargo bench --benches -- --verbose

# Generate HTML reports
cargo bench --benches
open target/criterion/report/index.html
```

---

## Coverage Measurement

### Tarpaulin (Line Coverage)

```bash
# Generate HTML coverage report
cargo tarpaulin --out Html --output-dir coverage

# View report
open coverage/index.html

# With specific features
cargo tarpaulin --out Html --output-dir coverage --features otel
```

### LLVM Coverage (Detailed)

```bash
# Install llvm-cov if needed
cargo install cargo-llvm-cov

# Generate coverage
cargo llvm-cov --html --output-dir coverage

# View report
open coverage/index.html

# Show coverage in terminal
cargo llvm-cov --output-format summary
```

### Mutation Testing

```bash
# Install cargo-mutants if needed
cargo install cargo-mutants

# Run mutation tests
cargo mutants --timeout 300

# Generate report
cargo mutants --timeout 300 --generate-report
```

---

## Chicago TDD Validation

### Verify AAA Pattern

```bash
# Search for tests using AAA pattern
grep -r "// Arrange" crates/ggen-core/src --include="*.rs" | wc -l
# Should find many tests

# Find tests missing comments
grep -r "#\[test\]" crates/ggen-core/src --include="*.rs" -A 3 | \
  grep -v "// Arrange\|// Act\|// Assert"
```

### Check for Meaningless Tests

```bash
# Find assert_ok! usage (should be minimal)
grep -r "assert_ok!" crates/ggen-core/src --include="*.rs" | wc -l

# Find assert!(true) anti-pattern
grep -r "assert!(true)" crates/ggen-core/src --include="*.rs"

# Find tests with no assertions
grep -r "#\[test\]" crates/ggen-core/src --include="*.rs" -A 20 | \
  grep -v "assert"
```

---

## Async Test Validation

### Run Async Tests

```bash
# All async tests
cargo test --lib tokio::test

# Async tests with Tokio multi-threaded runtime
cargo test --lib --features tokio/macros

# Single-threaded async (determinism)
cargo test --lib -- --test-threads=1

# With timeout verification
timeout 60s cargo test --lib tokio::test
```

### Verify Tokio Configuration

```bash
# Check tokio version
cargo tree | grep tokio

# Check tokio features
cargo tree -p tokio --features

# Expected: full runtime features enabled
```

---

## Determinism Verification

### Run with Fixed RNG Seed

```bash
# Set RNG seed for reproducibility
export RNG_SEED=42

# Run tests with fixed seed
RNG_SEED=42 cargo test --lib -- --test-threads=1

# Run multiple times, verify identical output
for i in {1..3}; do
  RNG_SEED=42 cargo test --lib > output_$i.txt
  echo "Run $i complete"
done

# Compare outputs
diff output_1.txt output_2.txt  # Should be identical
diff output_2.txt output_3.txt  # Should be identical
```

### Verify Code Generation Determinism

```bash
# Run generation twice
ggen sync --input spec1.ttl --output output1
ggen sync --input spec1.ttl --output output2

# Compare outputs
diff -r output1 output2
# Should show no differences
```

---

## Security Testing

### Vulnerability Scanning

```bash
# Check for known vulnerabilities
cargo audit

# Fix vulnerable dependencies
cargo audit fix

# Check for security patterns
cargo clippy --all -- -D clippy::all -D unsafe_code
```

### Manual Security Tests

```bash
# Test command whitelist
cargo test --lib test_command_whitelist -- --nocapture

# Test path validation
cargo test --lib test_path_validation -- --nocapture

# Test error sanitization
cargo test --lib test_error_sanitization -- --nocapture

# Run all security tests
cargo test --lib security:: -- --nocapture
```

---

## Performance Testing

### Build Performance

```bash
# Measure first build
time cargo build

# Measure incremental (touch one file)
touch crates/ggen-core/src/lib.rs
time cargo build

# Clean and measure fresh build
cargo clean
time cargo build
```

### RDF Processing Performance

```bash
# Benchmark RDF parsing
cargo bench --bench clnrm_benchmarks

# Benchmark template rendering
cargo bench --bench template_benchmarks

# Benchmark overall generation
cargo bench --bench performance_benchmark
```

### Memory Profiling

```bash
# Using cargo-valgrind (if available)
cargo valgrind --lib test_memory_intensive

# Using heaptrack
heaptrack cargo test --lib

# Using perf
perf record -g cargo test --lib
perf report
```

---

## Troubleshooting

### Test Hangs/Timeouts

```bash
# Run with shorter timeout to identify hanging tests
timeout 30s cargo test --lib -- --test-threads=1

# Run tests in isolation
cargo test --lib test_specific_name -- --nocapture --test-threads=1

# Check for infinite loops or deadlocks
cargo build --lib && lldb target/debug/test_binary
```

### Memory Leaks

```bash
# Run under memory sanitizer
RUSTFLAGS="-Z sanitizer=memsan" cargo test --lib

# Or use valgrind
valgrind --leak-check=full cargo test --lib
```

### Flaky Tests

```bash
# Run test many times to detect flakiness
for i in {1..100}; do
  cargo test --lib test_name || echo "FAILED on iteration $i"
done

# Run with verbose output
cargo test --lib test_name -- --nocapture --test-threads=1
```

---

## CI/CD Integration

### GitHub Actions Commands

```yaml
# In .github/workflows/test.yml
- name: Run tests
  run: timeout 300s cargo test --lib --all

- name: Check coverage
  run: |
    cargo install cargo-tarpaulin
    cargo tarpaulin --out Html --output-dir coverage

- name: Upload coverage
  uses: codecov/codecov-action@v3
  with:
    files: ./coverage.xml
```

### Pre-Commit Hook

```bash
#!/bin/bash
# Save in .git/hooks/pre-commit

timeout 30s cargo make check || exit 1
timeout 300s cargo test --lib || exit 1
timeout 60s cargo clippy --all -- -D warnings || exit 1

echo "✓ All pre-commit checks passed"
```

---

## Test Development Workflow

### Add New Test

```rust
#[test]
fn test_my_feature() {
    // Arrange - Setup test data
    let test_data = create_test_data();

    // Act - Execute code under test
    let result = my_feature(test_data);

    // Assert - Verify observable behavior
    assert!(result.is_success());
    assert_eq!(result.output, expected_output);
}
```

### Add Async Test

```rust
#[tokio::test]
async fn test_async_feature() {
    // Arrange
    let async_resource = create_async_resource().await;

    // Act
    let result = async_feature(async_resource).await;

    // Assert
    assert!(result.is_ok());
}
```

### Add Property-Based Test

```rust
#[cfg(test)]
mod tests {
    use proptest::proptest;

    proptest! {
        #[test]
        fn test_generation_always_valid(s in ".*") {
            let result = generate(&s);
            prop_assert!(is_valid_output(&result));
        }
    }
}
```

---

## Test Result Interpretation

### Successful Run

```
running 145 tests

test graph::tests::test_persistent_store_creation ... ok
test graph::tests::test_multiple_graphs_share_store ... ok
...
test security::tests::test_command_whitelist ... ok

test result: ok. 145 passed; 0 failed; 0 ignored; 0 measured; 0 filtered out

Finished `test` profile [unoptimized + debuginfo] target(s) in 28.45s
```

### Failed Test

```
test graph::tests::test_graph_query ... FAILED

thread 'graph::tests::test_graph_query' panicked at 'assertion failed: !results.is_empty()'

failures:
--- FAILED output for 'graph::tests::test_graph_query' ---
thread 'graph::tests::test_graph_query' panicked at 'assertion failed: ...

failures:
    graph::tests::test_graph_query

test result: FAILED. 144 passed; 1 failed; 0 ignored; 0 measured; 143 filtered out
```

### Timeout

```
timeout 30s cargo test --lib
# Process terminated after 30 seconds
# Indicates: Test hanging or taking too long
```

---

## Expected Test Counts

### Estimated Test Statistics

| Category | Expected | Range |
|----------|----------|-------|
| Unit tests | 50+ | 40-60 |
| Async tests | 20+ | 15-25 |
| Integration tests | 8 suites | 7-10 |
| Benchmarks | 10+ | 10-15 |
| **Total** | **80+** | **70-110** |

---

## Performance SLO Targets

| Metric | Target | Command |
|--------|--------|---------|
| First build | ≤15s | `time cargo build` |
| Incremental | ≤2s | Touch + `time cargo build` |
| Unit tests | ≤150s | `timeout 150s cargo test --lib` |
| RDF processing | ≤5s/1k triples | Benchmark result |
| CLI scaffolding | ≤3s end-to-end | `time ggen init project` |
| Generation memory | ≤100MB | Peak heap usage |

---

## Documentation Files

1. **GGEN_CORE_TEST_AUDIT_REPORT.md** - Comprehensive test analysis
2. **TEST_SUITE_FINDINGS_SUMMARY.md** - Key findings and recommendations
3. **TEST_SUITE_COMPLETION_CHECKLIST.md** - Phase-by-phase completion guide
4. **TEST_EXECUTION_GUIDE.md** - This reference guide

---

## Summary

**Use This Guide To**:
- ✓ Run the test suite correctly
- ✓ Measure test coverage
- ✓ Verify Chicago TDD compliance
- ✓ Troubleshoot test failures
- ✓ Develop new tests
- ✓ Integrate with CI/CD
- ✓ Verify performance SLOs

**Key Commands**:
```bash
# Verify no compiler errors (RED signal)
timeout 30s cargo make check

# Verify all tests pass (RED signal)
timeout 300s cargo test --lib --all

# Verify no warnings (YELLOW signal)
timeout 60s cargo clippy --all -- -D warnings

# Measure coverage
cargo tarpaulin --out Html --output-dir coverage
```

**When in Doubt**: Consult the comprehensive test audit report or check specific test file documentation.
