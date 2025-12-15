# Marketplace Stress Test & Benchmark Infrastructure

## Overview

Comprehensive stress testing and benchmarking infrastructure for the ggen CLI marketplace subsystem. This test suite ensures the marketplace can handle high-load scenarios, concurrent operations, and edge cases in production environments.

## Architecture

```
cli/
├── tests/
│   ├── utils/
│   │   ├── permutation_generator.rs  # Edge case and permutation generation
│   │   └── mod.rs
│   ├── stress/
│   │   ├── marketplace_stress_test.rs  # Core stress testing logic
│   │   └── mod.rs
│   ├── marketplace_concurrent_test.rs  # Concurrent operation tests
│   └── marketplace_stress_suite.rs     # Integrated test suite
└── benches/
    └── marketplace_benchmark.rs        # Performance benchmarks
```

## Components

### 1. Permutation Generator (`tests/utils/permutation_generator.rs`)

Generates comprehensive test input permutations including:

- **String Permutations**: Normal, empty, unicode, special characters, path traversal attempts
- **Numeric Permutations**: Edge values (0, MAX, boundaries)
- **Search Query Permutations**: Simple to complex queries with filters
- **Operation Permutations**: Sequential and concurrent operation patterns

**Key Features:**
- ✅ Configurable permutation limits
- ✅ Edge case inclusion/exclusion
- ✅ Security testing inputs (XSS, SQL injection, path traversal)
- ✅ No `.unwrap()` or `.expect()` - production-safe code

**Example Usage:**
```rust
use utils::{PermutationConfig, StringPermutations};

let config = PermutationConfig {
    max_permutations: 100,
    include_edge_cases: true,
    include_invalid: false,
};

let perms = StringPermutations::new(config);
let test_inputs = perms.all();
```

### 2. Stress Test Runner (`tests/stress/marketplace_stress_test.rs`)

High-load stress testing framework with comprehensive metrics collection.

**Test Scenarios:**
- **Concurrent Search**: Multiple simultaneous search operations
- **Rapid Sequential**: Fast successive operations
- **Large Dataset**: Handling thousands of packages
- **Memory Stress**: Memory allocation and peak usage tracking
- **Filesystem Stress**: File I/O under load

**Metrics Collected:**
- Operations completed/failed
- Average/min/max latency
- Throughput (ops/sec)
- Success rate
- Peak memory usage

**Example Usage:**
```rust
let config = StressConfig {
    concurrency: 10,
    total_operations: 1000,
    timeout: Duration::from_secs(300),
    include_destructive: false,
};

let runner = StressTestRunner::new(config)?;
let metrics = runner.run_concurrent_search_stress().await?;
println!("{}", metrics.report());
```

### 3. Concurrent Operations Tests (`tests/marketplace_concurrent_test.rs`)

Tests for race conditions, deadlocks, and concurrent access patterns.

**Test Coverage:**
- Concurrent reads (10+ simultaneous readers)
- Concurrent writes with proper synchronization
- Mixed read/write operations
- Race condition detection
- Deadlock prevention verification
- Concurrent package installation
- Concurrent search operations

**Key Patterns:**
- Uses `tokio::sync::Barrier` for synchronized starts
- `Arc<RwLock<T>>` for shared state
- Timeout-based deadlock detection
- Proper error handling without panics

### 4. Benchmark Harness (`benches/marketplace_benchmark.rs`)

Criterion.rs-based performance benchmarks with detailed reports.

**Benchmark Categories:**
- Search operations (simple, medium, complex queries)
- Package listing (10 to 10,000 packages)
- Installation simulation
- Concurrent operations (2-16 threads)
- Metadata parsing
- Cache operations
- Filesystem operations
- Sort and filter operations

**Features:**
- HTML report generation
- Throughput measurements
- Latency percentiles
- Async operation support
- Statistical analysis

## Running Tests

### Quick Smoke Test
```bash
# Fast verification that infrastructure works
cargo test smoke_test_stress_infrastructure
```

### Unit Tests
```bash
# Test individual components
cargo test --test marketplace_concurrent_test
cargo test -p ggen-cli-lib permutation_generator
cargo test -p ggen-cli-lib stress
```

### Full Stress Suite
```bash
# Comprehensive stress testing (takes ~5-10 minutes)
cargo test --test marketplace_stress_suite -- --ignored --nocapture
```

### Performance Stress Tests
```bash
# High-load performance validation
cargo test run_performance_stress_suite -- --ignored --nocapture
```

### Edge Case Tests
```bash
# Edge case and boundary condition testing
cargo test run_edge_case_stress_suite -- --ignored --nocapture
```

### Benchmarks
```bash
# Run all benchmarks
cargo bench --bench marketplace_benchmark

# Run specific benchmark
cargo bench --bench marketplace_benchmark -- marketplace_search

# Generate HTML reports (saved to target/criterion/)
cargo bench --bench marketplace_benchmark -- --save-baseline main
```

## Performance Targets

### Stress Test Success Criteria
- **Success Rate**: > 95% operations successful
- **Throughput**: > 10 ops/sec under high load
- **Average Latency**: < 100ms for search operations
- **Max Latency**: < 1000ms for any operation
- **Memory**: Peak usage < 500MB for 1000 operations

### Benchmark Targets
- **Simple Search**: < 1ms
- **Complex Search**: < 10ms
- **Package List (1000)**: < 50ms
- **Concurrent Ops (8 threads)**: > 50 ops/sec

## Configuration

### Stress Test Configuration
```rust
StressConfig {
    concurrency: usize,        // Number of concurrent operations
    total_operations: usize,   // Total operations to perform
    timeout: Duration,         // Test timeout
    include_destructive: bool, // Include destructive operations
}
```

### Permutation Configuration
```rust
PermutationConfig {
    max_permutations: usize,   // Max permutations to generate
    include_edge_cases: bool,  // Include edge cases
    include_invalid: bool,     // Include invalid/security test inputs
}
```

## Code Quality Standards

### Production-Safe Code
❌ **NEVER use:**
- `.unwrap()`
- `.expect()`
- Unhandled panics

✅ **ALWAYS use:**
- `anyhow::Result` for error propagation
- Proper error context with `.map_err()`
- Graceful error handling

### Example
```rust
// ❌ BAD
let data = std::fs::read("file.txt").unwrap();

// ✅ GOOD
let data = std::fs::read("file.txt")
    .map_err(|e| anyhow::anyhow!("Failed to read file: {}", e))?;
```

## CI/CD Integration

### GitHub Actions Example
```yaml
- name: Run stress tests
  run: |
    cargo test smoke_test_stress_infrastructure
    cargo test --test marketplace_concurrent_test

- name: Run benchmarks
  run: |
    cargo bench --bench marketplace_benchmark -- --save-baseline ci-${{ github.sha }}
```

## Metrics & Reporting

### Stress Test Report Format
```
Stress Test Results
===================
Operations:
  Completed: 950
  Failed: 50
  Success Rate: 95.00%

Latency:
  Average: 45.23ms
  Min: 2ms
  Max: 234ms

Performance:
  Throughput: 31.67 ops/sec
  Duration: 30.00s
  Peak Memory: 128.45MB
```

### Benchmark Report
- HTML reports in `target/criterion/`
- JSON data for programmatic analysis
- Statistical significance testing
- Historical comparison

## Troubleshooting

### Test Failures

**High failure rate (> 5%)**
- Check system resources (CPU, memory)
- Reduce concurrency level
- Increase timeout duration

**Deadlock timeout**
- Check lock acquisition order
- Verify barrier count matches task count
- Review RwLock usage patterns

**Memory exhaustion**
- Reduce `total_operations` count
- Check for memory leaks in test code
- Monitor system memory availability

### Benchmark Issues

**High variance**
- Close other applications
- Run on dedicated CI/CD infrastructure
- Increase measurement time
- Use `--warm-up-time` flag

**Slow benchmarks**
- Use `--quick` flag for faster iterations
- Reduce input sizes
- Focus on specific benchmarks with filters

## Future Enhancements

- [ ] Property-based testing integration (proptest)
- [ ] Chaos engineering scenarios
- [ ] Network failure simulation
- [ ] Distributed stress testing across nodes
- [ ] Real-time metrics dashboard
- [ ] Automated performance regression detection
- [ ] Load testing with realistic user patterns

## References

- [Criterion.rs Documentation](https://bheisler.github.io/criterion.rs/book/)
- [Tokio Testing Guide](https://tokio.rs/tokio/topics/testing)
- [Rust Performance Book](https://nnethercote.github.io/perf-book/)

---

**Status**: ✅ Production-Ready
**Last Updated**: 2025-11-01
**Maintainer**: Hive Mind Coder Agent (Gamma)
