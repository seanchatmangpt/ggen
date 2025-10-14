# Ultra-Deploy Test Suite

Comprehensive testing framework for the ultra-fast ggen → cleanroom → deploy workflow with <60s execution targets.

## Quick Start

```bash
# Build ggen in release mode
cargo build --release

# Run all ultra-deploy tests
cargo test --test ultra_deploy_test

# Run with detailed output
cargo test --test ultra_deploy_test -- --nocapture

# Run specific test
cargo test --test ultra_deploy_test test_ultra_deploy_cli_under_60s -- --nocapture

# Use test runner script
./scripts/run-ultra-deploy-tests.sh
```

## Test Suite Overview

### 📋 Test Categories

| Category | Tests | Purpose |
|----------|-------|---------|
| **Integration** | 3 | End-to-end workflow validation |
| **Performance** | 2 | Timing and throughput benchmarks |
| **Reliability** | 1 | Consistency across multiple runs |
| **Matrix** | 2 | Multiple templates and scenarios |

### ⏱️ Performance Targets

| Stage | Target | Description |
|-------|--------|-------------|
| Code Generation | <5s | Template-based project generation |
| Validation | <10s | Code validation and checks |
| Test Execution | <20s | Running generated tests |
| Build | <25s | Release build compilation |
| Fake Publish | <5s | Package creation |
| **Total** | **<60s** | **Complete workflow** |

## Test Descriptions

### Integration Tests

#### 1. `test_ultra_deploy_cli_under_60s`
**Purpose**: Validate complete CLI workflow meets 60s target

**What it tests**:
- End-to-end workflow execution
- All stages complete successfully
- Total time <60s
- Artifacts are created correctly

**Success criteria**:
- ✓ Exit code 0
- ✓ Total duration <60s
- ✓ All expected artifacts exist

#### 2. `test_ggen_cleanroom_integration`
**Purpose**: Validate ggen output works in cleanroom environment

**What it tests**:
- Generated code compiles in isolated environment
- No host dependencies
- Hermetic execution works correctly

**Success criteria**:
- ✓ Code generation succeeds
- ✓ Cleanroom validation passes
- ✓ Cargo check succeeds in container

#### 3. `test_output_correctness`
**Purpose**: Validate generated project structure and content

**What it tests**:
- Correct directory structure
- Valid Cargo.toml
- Main/lib files exist
- Binary compilation

**Success criteria**:
- ✓ All expected files exist
- ✓ Cargo.toml is valid
- ✓ Binary builds successfully

### Performance Tests

#### 4. `test_performance_benchmark`
**Purpose**: Comprehensive timing analysis with strict assertions

**What it tests**:
- Each stage meets individual target
- Total workflow <60s
- Stage breakdowns are recorded

**Success criteria**:
- ✓ Total <60s
- ✓ Each stage meets target
- ✓ Detailed timing report generated

#### 5. `test_stage_performance_breakdown`
**Purpose**: Measure each stage independently

**What it tests**:
- Individual stage performance
- Timing isolation
- Bottleneck identification

**Success criteria**:
- ✓ Generation <5s
- ✓ Validation <10s
- ✓ Tests <20s
- ✓ Build <30s

### Reliability Tests

#### 6. `test_workflow_reliability`
**Purpose**: Ensure consistent performance across multiple runs

**What it tests**:
- Performance variance
- Consistency over 3+ iterations
- No performance degradation

**Success criteria**:
- ✓ All runs <60s
- ✓ Variance <50% (max < 1.5x min)
- ✓ No failures

### Matrix Tests

#### 7. `test_template_matrix`
**Purpose**: Validate multiple template types

**What it tests**:
- Different template types
- Template-specific optimizations
- Cross-template consistency

**Success criteria**:
- ✓ Each template completes
- ✓ Template-specific targets met

#### 8. `test_sequential_workflow_performance`
**Purpose**: Test multiple workflows in sequence

**What it tests**:
- No resource leaks
- Consistent performance
- Sequential execution <2min

**Success criteria**:
- ✓ All workflows succeed
- ✓ Total time <120s
- ✓ No resource exhaustion

## Running Tests

### Local Development

```bash
# Quick test
cargo test --test ultra_deploy_test test_ultra_deploy_cli_under_60s

# All tests with output
cargo test --test ultra_deploy_test -- --nocapture --test-threads=1

# Performance benchmark only
cargo test --test ultra_deploy_test test_performance_benchmark -- --nocapture

# With detailed timing
RUST_LOG=debug cargo test --test ultra_deploy_test -- --nocapture
```

### Using Test Runner Script

```bash
# Run all categories
./scripts/run-ultra-deploy-tests.sh

# Run specific categories
RUN_INTEGRATION=1 RUN_PERFORMANCE=0 ./scripts/run-ultra-deploy-tests.sh

# Save detailed report
./scripts/run-ultra-deploy-tests.sh > test-output.log 2>&1
```

### CI/CD

Tests run automatically on:
- **Push**: main, master, develop branches
- **Pull Requests**: All PRs
- **Schedule**: Nightly at 2 AM UTC

View results in GitHub Actions.

## Test Output

### Standard Output

```
=== Ultra-Deploy Workflow Report ===
Success: true
Total Duration: 47.23s

--- Stage Timings ---
✓ Code Generation: 3.21s / 5.00s (64.2%)
✓ Validation: 7.45s / 10.00s (74.5%)
✓ Test Execution: 15.67s / 20.00s (78.4%)
✓ Build: 18.90s / 25.00s (75.6%)
✓ Fake Publish: 2.00s / 5.00s (40.0%)

--- Artifacts ---
  /tmp/test-project/Cargo.toml
  /tmp/test-project/target/release/binary
  /tmp/test-project/target/package
=====================================
```

### Test Reports

Reports are saved to:
- `target/test-reports/ultra-deploy-test-YYYYMMDD_HHMMSS.md`
- Individual test outputs in `target/test-reports/`

## Troubleshooting

### Test Failures

#### Timeout Issues

```bash
# Increase test timeout
cargo test --test ultra_deploy_test -- --test-threads=1 --timeout 300

# Check Docker is running
docker info

# Verify ggen binary
ls -la target/release/ggen
```

#### Performance Issues

```bash
# Profile slow stage
cargo test test_stage_performance_breakdown -- --nocapture

# Check system resources
top
df -h
docker stats
```

#### Build Failures

```bash
# Clean build
cargo clean
cargo build --release

# Check dependencies
cargo tree

# Update dependencies
cargo update
```

### Common Issues

| Issue | Solution |
|-------|----------|
| Docker not available | Install Docker and start daemon |
| ggen binary not found | Run `cargo build --release` |
| Tests timeout | Increase timeout or check resources |
| Inconsistent timing | Run on dedicated system |

## Performance Optimization

### Tips for Faster Execution

1. **Use release builds**: Always test with `--release`
2. **Warm up cache**: Run once to warm cargo cache
3. **Disable parallel tests**: Use `--test-threads=1`
4. **Monitor resources**: Ensure sufficient CPU/memory
5. **Clean work dirs**: Remove old temporary directories

### Expected Performance

| Platform | Typical Time | Target |
|----------|-------------|--------|
| Linux (fast) | 40-45s | <60s |
| Linux (slow) | 50-55s | <60s |
| macOS (M1) | 35-40s | <60s |
| macOS (Intel) | 45-50s | <60s |

## Contributing

### Adding New Tests

1. Follow existing patterns in `tests/ultra_deploy_test.rs`
2. Add timing assertions for performance tests
3. Use `serial_test` for sequential execution
4. Document expected behavior
5. Update this README

### Test Template

```rust
#[tokio::test]
#[serial]
async fn test_my_new_feature() -> Result<()> {
    let tester = UltraDeployTester::new()?;
    let start = Instant::now();

    // Test implementation
    let result = tester.run_workflow("my-template").await?;

    // Assertions
    assert!(start.elapsed() < Duration::from_secs(60));
    result.assert_success();

    Ok(())
}
```

## Best Practices

1. ✓ Always use `tokio::test` for async tests
2. ✓ Use `#[serial]` to prevent parallel execution conflicts
3. ✓ Clean up temporary resources
4. ✓ Assert both success and timing
5. ✓ Print detailed reports for debugging
6. ✓ Handle errors gracefully
7. ✓ Document test purpose and criteria

## References

- [Test Suite Implementation](../../tests/ultra_deploy_test.rs)
- [Test Runner Script](../../scripts/run-ultra-deploy-tests.sh)
- [CI/CD Workflow](../../.github/workflows/ultra-deploy-test.yml)
- [Testing Guide](./ultra-deploy-testing-guide.md)

## Support

For issues or questions:
- **GitHub Issues**: Report test failures
- **Discussions**: Performance optimization tips
- **Slack**: #ultra-deploy-tests channel
