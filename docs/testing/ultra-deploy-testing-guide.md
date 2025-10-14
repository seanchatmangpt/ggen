# Ultra-Deploy Testing Guide

Complete guide for testing the ggen → cleanroom → deploy ultra-fast workflow.

## Overview

The ultra-deploy workflow provides a complete development pipeline from code generation to deployment validation in under 60 seconds. This guide covers all aspects of testing this workflow.

## Performance Targets

| Stage | Target | Description |
|-------|--------|-------------|
| Code Generation | <5s | Template-based project generation |
| Validation | <10s | Code validation and checks |
| Test Execution | <20s | Running generated tests |
| Build | <25s | Release build compilation |
| Fake Publish | <5s | Package creation and validation |
| **Total** | **<60s** | **Complete workflow** |

## Test Suite Structure

### 1. Integration Tests

Test the complete end-to-end workflow:

```rust
#[tokio::test]
async fn test_ultra_deploy_cli_under_60s() -> Result<()> {
    let tester = UltraDeployTester::new()?;
    let result = tester.run_workflow("rust-cli-minimal").await?;

    result
        .assert_success()
        .assert_timing(Duration::from_secs(60))
        .assert_artifacts_exist();

    Ok(())
}
```

### 2. Performance Tests

Measure and validate timing for each stage:

```rust
#[tokio::test]
async fn test_stage_performance_breakdown() -> Result<()> {
    // Measure each stage independently
    let generation_time = measure_generation()?;
    let validation_time = measure_validation()?;
    let test_time = measure_tests()?;
    let build_time = measure_build()?;

    // Verify each meets target
    assert!(generation_time < Duration::from_secs(5));
    assert!(validation_time < Duration::from_secs(10));
    assert!(test_time < Duration::from_secs(20));
    assert!(build_time < Duration::from_secs(25));

    Ok(())
}
```

### 3. Reliability Tests

Ensure consistent performance across multiple runs:

```rust
#[tokio::test]
async fn test_workflow_reliability() -> Result<()> {
    let iterations = 10;
    let mut durations = Vec::new();

    for _ in 0..iterations {
        let result = run_workflow()?;
        durations.push(result.total_duration);
    }

    // Check variance
    let avg = calculate_average(&durations);
    let std_dev = calculate_std_dev(&durations, avg);

    assert!(std_dev < avg * 0.15); // <15% variance

    Ok(())
}
```

### 4. Matrix Tests

Test across multiple scenarios:

- **Templates**: CLI, library, web service
- **Platforms**: Linux, macOS, Windows
- **Rust versions**: Stable, nightly
- **Scenarios**: Sequential, parallel

## Running Tests

### Local Testing

```bash
# Run all ultra-deploy tests
cargo test --test ultra_deploy_test

# Run specific test
cargo test --test ultra_deploy_test test_ultra_deploy_cli_under_60s

# Run with verbose output
cargo test --test ultra_deploy_test -- --nocapture

# Run performance benchmarks
cargo test --test ultra_deploy_test test_performance_benchmark -- --nocapture
```

### CI/CD Testing

Tests run automatically on:
- Push to main/master/develop branches
- Pull requests
- Nightly schedule

View results in GitHub Actions.

## Test Harness

The `UltraDeployTester` provides utilities for testing:

```rust
struct UltraDeployTester {
    ggen_bin: PathBuf,
    work_dir: TempDir,
}

impl UltraDeployTester {
    // Create new tester
    fn new() -> Result<Self>;

    // Run complete workflow
    async fn run_workflow(&self, template: &str) -> Result<WorkflowResult>;

    // Individual stages
    fn generate_project(&self, template: &str, name: &str) -> Result<()>;
    fn validate_project(&self, dir: &Path) -> Result<()>;
    fn run_tests(&self, dir: &Path) -> Result<()>;
    fn build_project(&self, dir: &Path) -> Result<()>;
    fn fake_publish(&self, dir: &Path) -> Result<()>;
}
```

## Workflow Result Analysis

```rust
struct WorkflowResult {
    success: bool,
    total_duration: Duration,
    timings: Vec<TimingReport>,
    output_dir: PathBuf,
    artifacts: Vec<PathBuf>,
}

// Usage
let result = tester.run_workflow("rust-cli-minimal").await?;
result.print_report();  // Pretty-printed analysis

result
    .assert_success()
    .assert_timing(Duration::from_secs(60))
    .assert_artifacts_exist();
```

## Performance Optimization

### Identifying Bottlenecks

```rust
#[tokio::test]
async fn identify_slow_stages() -> Result<()> {
    let result = run_workflow()?;

    for timing in result.timings {
        if !timing.passed() {
            println!("Slow stage: {} - {:.2}s / {:.2}s",
                timing.stage,
                timing.duration.as_secs_f64(),
                timing.target.as_secs_f64()
            );
        }
    }

    Ok(())
}
```

### Optimization Strategies

1. **Parallel Execution**: Run independent stages concurrently
2. **Caching**: Cache dependencies and build artifacts
3. **Incremental Builds**: Use incremental compilation
4. **Template Optimization**: Minimize generated code

## Cleanroom Integration

Test ggen output in hermetic cleanroom environment:

```rust
#[tokio::test]
async fn test_ggen_cleanroom_integration() -> Result<()> {
    // Generate with ggen
    let project_dir = generate_project("rust-cli-minimal")?;

    // Validate in cleanroom
    let config = CleanroomConfig::default();
    let environment = CleanroomEnvironment::new(config).await?;

    // Execute tests in isolated environment
    let result = environment.execute_test("cargo_check", || {
        Command::new("cargo")
            .args(&["check"])
            .current_dir(&project_dir)
            .output()
    }).await?;

    assert!(result.status.success());

    Ok(())
}
```

## Continuous Monitoring

### Performance Trends

Track performance over time:

```bash
# Run benchmark and save results
cargo test --test ultra_deploy_test test_performance_benchmark -- --nocapture \
    > performance-$(date +%Y%m%d).txt

# Compare with previous results
diff performance-20231201.txt performance-20231202.txt
```

### Regression Detection

Automatically detect performance regressions in CI:

```yaml
- name: Performance Regression Check
  run: |
    # Run tests on current commit
    cargo test --test ultra_deploy_test > current.txt

    # Checkout previous commit
    git checkout HEAD~1
    cargo test --test ultra_deploy_test > previous.txt

    # Compare results
    ./scripts/compare-performance.sh current.txt previous.txt
```

## Troubleshooting

### Slow Test Execution

```bash
# Profile test execution
cargo test --test ultra_deploy_test -- --nocapture --test-threads=1

# Check for slow stages
cargo test test_stage_performance_breakdown -- --nocapture
```

### Test Failures

```bash
# Run with verbose output
RUST_BACKTRACE=1 cargo test --test ultra_deploy_test -- --nocapture

# Run specific failing test
cargo test --test ultra_deploy_test test_name -- --nocapture
```

### Docker/Container Issues

```bash
# Check Docker is running
docker info

# Verify testcontainers work
cargo test --test ultra_deploy_test test_ggen_cleanroom_integration
```

## Best Practices

1. **Always use `--release` builds** for performance testing
2. **Run tests serially** (`--test-threads=1`) for accurate timing
3. **Use fresh work directories** for each test run
4. **Monitor resource usage** (CPU, memory, disk)
5. **Test on target platforms** (Linux, macOS, Windows)
6. **Document performance changes** in commit messages

## Reporting

### Test Report Format

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

### CI/CD Reports

View detailed reports in GitHub Actions artifacts:
- `test-report-ubuntu-latest-stable.md`
- `test-report-macos-latest-stable.md`
- `performance-comparison.md`
- `regression-report.md`

## Examples

### Example 1: Quick Performance Check

```bash
# Build in release mode
cargo build --release

# Run performance benchmark
cargo test --test ultra_deploy_test test_performance_benchmark -- --nocapture
```

### Example 2: Full Test Suite

```bash
# Run all tests with detailed output
cargo test --test ultra_deploy_test -- --nocapture --test-threads=1
```

### Example 3: Reliability Testing

```bash
# Run multiple iterations to check consistency
for i in {1..5}; do
    echo "Iteration $i"
    cargo test --test ultra_deploy_test test_workflow_reliability -- --nocapture
done
```

## Contributing

When adding new tests:

1. Follow existing test patterns
2. Add timing assertions
3. Document expected behavior
4. Update this guide
5. Ensure tests pass in CI

## Support

- **Issues**: Report test failures as GitHub issues
- **Performance**: Discuss optimization strategies
- **Documentation**: Improve testing guides

## References

- [Ultra-Deploy Test Suite](../../tests/ultra_deploy_test.rs)
- [Cleanroom Documentation](../../cleanroom/README.md)
- [CI/CD Workflow](../../.github/workflows/ultra-deploy-test.yml)
