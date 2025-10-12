# Coverage Collection and Reporting

Cleanroom provides LLVM-based code coverage collection with path remapping for accurate reporting across different execution environments.

## Overview

Coverage collection works by:

1. **Instrumentation**: Code compiled with coverage instrumentation (`-C instrument-coverage`)
2. **Data Collection**: Runtime collection of execution counts in `.profraw` files
3. **Data Extraction**: Copying coverage data from containers to host
4. **Path Remapping**: Mapping container paths back to host source paths
5. **Merging**: Combining coverage data from multiple test runs
6. **Reporting**: Generating human-readable coverage reports

## Basic Usage

```rust
use cleanroom::{CoverageCollector, scenario};

#[test]
fn test_with_coverage() {
    let collector = CoverageCollector::new().unwrap();

    let result = scenario("coverage_test")
        .step("test", ["cargo", "test", "--", "my_test"])
        .run()
        .unwrap();

    // Collect coverage from the test run
    let coverage_data = collector.collect_from_container("container_id").unwrap();

    // Merge with path remapping
    let remap = PathRemap {
        from: PathBuf::from("/workdir/src"),
        to: PathBuf::from("/home/user/project/src"),
    };

    let merged = collector.merge_with_remap(coverage_data, remap).unwrap();

    println!("Coverage: {:.1}% ({}/{} lines)",
             merged.percentage,
             merged.lines_covered,
             merged.lines_total);

    // Cleanup temporary files
    collector.cleanup().unwrap();
}
```

## Coverage Collector

The `CoverageCollector` manages the coverage collection process:

```rust
impl CoverageCollector {
    pub fn new() -> Result<Self>
    pub fn with_llvm_tools(self, path: PathBuf) -> Self
    pub fn collect_from_container(&self, container_id: &str) -> Result<CoverageData>
    pub fn merge_with_remap(&self, data: CoverageData, remap: PathRemap) -> Result<MergedCoverage>
    pub fn cleanup(&self) -> Result<()>
}
```

### LLVM Tools Path

Specify custom LLVM tools location:

```rust
let collector = CoverageCollector::new()
    .unwrap()
    .with_llvm_tools(PathBuf::from("/usr/local/opt/llvm/bin"));
```

## Coverage Data Formats

### Profraw Format

Raw coverage data from instrumented binaries:

```rust
pub struct CoverageData {
    pub path: PathBuf,        // Path to .profraw file
    pub format: CoverageFormat, // LLVM profraw format
}
```

### Coverage Format Types

```rust
pub enum CoverageFormat {
    Profraw,
    Other(String),
}
```

## Path Remapping

Path remapping corrects source paths in coverage reports:

```rust
pub struct PathRemap {
    pub from: PathBuf, // Container source path
    pub to: PathBuf,   // Host source path
}
```

Example remapping:
- Container: `/workdir/src/lib.rs`
- Host: `/home/user/project/src/lib.rs`

## Coverage Merging

Merge coverage data from multiple sources:

```rust
// Collect from multiple containers/runs
let data1 = collector.collect_from_container("container1").unwrap();
let data2 = collector.collect_from_container("container2").unwrap();

// Merge with remapping
let merged1 = collector.merge_with_remap(data1, remap).unwrap();
let merged2 = collector.merge_with_remap(data2, remap).unwrap();

// Combine results
let total_covered = merged1.lines_covered + merged2.lines_covered;
let total_lines = merged1.lines_total + merged2.lines_total;
let combined_percentage = (total_covered as f64 / total_lines as f64) * 100.0;
```

## Integration with Scenarios

Coverage collection integrates with the Scenario DSL:

```rust
use cleanroom::{scenario, CoverageCollector};

let collector = CoverageCollector::new().unwrap();

let result = scenario("integration_test")
    .step("setup", ["./setup.sh"])
    .step("test", ["cargo", "test"])
    .step("coverage", ["./collect_coverage.sh"])
    .run()
    .unwrap();

// Extract container ID from result for coverage collection
let container_id = extract_container_id(&result);
let coverage_data = collector.collect_from_container(&container_id).unwrap();
```

## Report Generation

Generate detailed coverage reports:

```rust
use cleanroom::CoverageCollector;

let collector = CoverageCollector::new().unwrap();
let coverage_data = collector.collect_from_container("container_id").unwrap();

let remap = PathRemap {
    from: PathBuf::from("/workdir/src"),
    to: PathBuf::from("/home/user/project/src"),
};

let merged = collector.merge_with_remap(coverage_data, remap).unwrap();

// Generate HTML report
let html_report = collector.generate_html_report(merged).unwrap();
println!("HTML report: {}", html_report);

// Generate LCOV format
let lcov_report = collector.generate_lcov_report(merged).unwrap();
println!("LCOV report: {}", lcov_report);
```

## Coverage Metrics

### Basic Metrics

```rust
pub struct MergedCoverage {
    pub lines_covered: u64,
    pub lines_total: u64,
    pub percentage: f64,
}
```

### Advanced Metrics

```rust
pub struct DetailedCoverage {
    pub functions_covered: u64,
    pub functions_total: u64,
    pub branches_covered: u64,
    pub branches_total: u64,
    pub regions_covered: u64,
    pub regions_total: u64,
}
```

## CI/CD Integration

### Coverage in CI

```yaml
# .github/workflows/test.yml
- name: Run Tests with Coverage
  run: |
    cargo test --all-features
    cleanroom collect-coverage --output coverage.lcov

- name: Upload Coverage
  uses: codecov/codecov-action@v3
  with:
    file: coverage.lcov
```

### Coverage Thresholds

Set minimum coverage requirements:

```rust
use cleanroom::CoverageCollector;

let collector = CoverageCollector::new().unwrap();
let merged = collector.merge_with_remap(coverage_data, remap).unwrap();

// Check coverage thresholds
assert!(merged.percentage >= 80.0, "Coverage below 80% threshold");
```

## Troubleshooting

### Missing Coverage Data

If coverage data is empty or missing:

1. **Check instrumentation**: Ensure code is compiled with coverage
   ```bash
   RUSTFLAGS="-C instrument-coverage" cargo build
   ```

2. **Verify binary location**: Ensure instrumented binary is in container
   ```bash
   docker cp target/debug/my_binary container:/workdir/
   ```

3. **Check file permissions**: Ensure coverage files are writable
   ```bash
   chmod 666 /workdir/coverage.profraw
   ```

### Path Remapping Issues

If paths don't remap correctly:

1. **Verify paths exist**: Both source and target paths must exist
2. **Check path format**: Use absolute paths consistently
3. **Test remapping**: Validate remapping with simple test cases

### LLVM Tools Issues

If LLVM tools are missing:

1. **Install LLVM tools**:
   ```bash
   # Ubuntu/Debian
   sudo apt install llvm

   # macOS
   brew install llvm

   # Or use rustup component
   rustup component add llvm-tools-preview
   ```

2. **Specify tools path**:
   ```rust
   let collector = CoverageCollector::new()
       .unwrap()
       .with_llvm_tools(PathBuf::from("/usr/local/opt/llvm/bin"));
   ```

## Performance Impact

Coverage collection has minimal runtime overhead:

| Operation | Overhead | Notes |
|-----------|----------|-------|
| Instrumentation | 5-10% | Compile-time only |
| Runtime collection | <1% | Minimal runtime cost |
| Data extraction | Seconds | Network/disk I/O |
| Merging | Seconds | LLVM processing |
| Report generation | Seconds | Text processing |

## Best Practices

### Coverage Strategy

1. **Focus on critical paths**: Cover error handling and edge cases
2. **Use realistic data**: Test with production-like data sets
3. **Measure regularly**: Track coverage trends over time
4. **Set meaningful thresholds**: Avoid arbitrary percentage targets

### Performance Optimization

1. **Selective instrumentation**: Only instrument relevant code
2. **Parallel collection**: Collect coverage from parallel tests
3. **Incremental merging**: Merge coverage data incrementally
4. **Report caching**: Cache generated reports for CI

### CI/CD Integration

1. **Automated reporting**: Generate reports on every PR
2. **Threshold enforcement**: Fail builds below coverage thresholds
3. **Historical tracking**: Monitor coverage trends
4. **Team notifications**: Alert on coverage regressions
