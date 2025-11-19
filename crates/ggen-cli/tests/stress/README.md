# Marketplace Container Validation Stress Tests

Comprehensive stress testing suite for the ggen marketplace container validation system.

## Overview

This test suite validates the marketplace container system under extreme conditions and edge cases to ensure production readiness, security, and performance guarantees.

## Test Coverage

### 1. Rapid Sequential Install/Uninstall Tests
- **File**: `container_validation_stress_test.rs::test_rapid_sequential_install_uninstall`
- **Purpose**: Validates system stability during rapid package installation and removal cycles
- **Metrics**: Operations/second, success rate, latency distribution

### 2. Concurrent Marketplace Operations
- **File**: `container_validation_stress_test.rs::test_concurrent_marketplace_operations`
- **Purpose**: Tests system behavior under high concurrent load (search, validate, install)
- **Default Concurrency**: 50 simultaneous operations
- **Metrics**: Throughput, race condition detection, resource contention

### 3. Resource Exhaustion Scenarios
- **File**: `container_validation_stress_test.rs::test_resource_exhaustion`
- **Tests**:
  - Disk space limits (packages up to 90MB)
  - Memory allocation limits (progressive allocation testing)
  - CPU-intensive validation operations
- **Limits**:
  - Max disk: 1GB
  - Max memory per operation: 100MB
  - ZIP bomb protection: 100MB limit

### 4. Container Isolation Verification
- **File**: `container_validation_stress_test.rs::test_container_isolation`
- **Purpose**: Ensures proper container isolation and security boundaries
- **Tests**:
  - Path traversal prevention (`../../etc/passwd`, etc.)
  - Resource isolation between containers
  - Namespace isolation
- **Metrics**: Isolation violations detected

### 5. Network Failure Recovery
- **File**: `container_validation_stress_test.rs::test_network_failure_recovery`
- **Scenarios**:
  - Connection timeouts
  - DNS failures
  - Partial downloads
  - Connection resets
  - Slow response handling
- **Features**:
  - Automatic retry with exponential backoff
  - 3 retry attempts (1s, 2s, 4s delays)
  - Graceful degradation

### 6. Corrupted Package Handling
- **File**: `container_validation_stress_test.rs::test_corrupted_package_handling`
- **Corruption Types**:
  - Invalid checksums (SHA256 verification)
  - Truncated files
  - Malformed ZIP archives
  - Missing manifests
  - Invalid metadata/TOML syntax
  - ZIP bomb attempts
  - Path traversal attempts
- **Metrics**: Corruption detection rate (should be 100%)

### 7. Registry Timeout Scenarios
- **File**: `container_validation_stress_test.rs::test_registry_timeouts`
- **Scenarios**:
  - Fast response (100ms)
  - Slow response (5s)
  - Timeout threshold (30s)
  - Beyond timeout (35s)
- **Default Timeout**: 30 seconds
- **Metrics**: Timeout handling, recovery success rate

### 8. <33s Crates.io Dry-Run Performance Guarantee
- **File**: `container_validation_stress_test.rs::test_dry_run_performance_guarantee`
- **Purpose**: Validates the critical <33 second dry-run performance guarantee
- **Test Packages**:
  - Small (1MB)
  - Medium (10MB)
  - Large (50MB)
  - XLarge (90MB)
- **Operations Simulated**:
  - Package parsing
  - Dependency resolution
  - RDF/SPARQL validation
- **Success Criteria**: All operations must complete within 33 seconds

## Running the Tests

### Run All Stress Tests
```bash
cargo test --package ggen-cli-lib --test stress -- --nocapture
```

### Run Individual Test Suites

#### Container Validation Tests
```bash
cargo test --package ggen-cli-lib container_validation_stress_test -- --nocapture
```

#### Rapid Sequential Tests
```bash
cargo test --package ggen-cli-lib test_rapid_sequential_install_uninstall -- --nocapture
```

#### Concurrent Operations Tests
```bash
cargo test --package ggen-cli-lib test_concurrent_operations -- --nocapture
```

#### Resource Exhaustion Tests
```bash
cargo test --package ggen-cli-lib test_resource_exhaustion_scenarios -- --nocapture
```

#### Container Isolation Tests
```bash
cargo test --package ggen-cli-lib test_container_isolation_verification -- --nocapture
```

#### Network Failure Tests
```bash
cargo test --package ggen-cli-lib test_network_failure_and_recovery -- --nocapture
```

#### Corrupted Package Tests
```bash
cargo test --package ggen-cli-lib test_corrupted_packages -- --nocapture
```

#### Timeout Tests
```bash
cargo test --package ggen-cli-lib test_registry_timeout_handling -- --nocapture
```

#### 33s Performance Guarantee Test
```bash
cargo test --package ggen-cli-lib test_33s_dry_run_guarantee -- --nocapture
```

## Configuration

### ContainerStressConfig

```rust
ContainerStressConfig {
    max_concurrency: 50,              // Max concurrent operations
    sequential_operations: 100,        // Number of sequential ops
    max_disk_bytes: 1_073_741_824,    // 1GB disk limit
    max_memory_bytes: 104_857_600,    // 100MB memory limit
    network_timeout: Duration::from_secs(30),
    enable_destructive: false,         // Destructive tests
    dry_run_max_duration: Duration::from_secs(33), // 33s guarantee
}
```

### Customizing Tests

```rust
let config = ContainerStressConfig {
    max_concurrency: 100,
    sequential_operations: 500,
    ..Default::default()
};

let runner = ContainerStressTestRunner::new(config)?;
let metrics = runner.test_rapid_sequential_install_uninstall().await?;
```

## Metrics and Reporting

Each test produces detailed metrics:

```
Container Validation Stress Test Results
=========================================
Operations:
  Total:       100
  Successful:  98
  Failed:      2
  Success Rate: 98.00%

Performance:
  Avg Duration:    12.45ms
  Min Duration:    5ms
  Max Duration:    156ms
  Throughput:      234.56 ops/sec

Resources:
  Peak Disk:   450.23MB
  Peak Memory: 87.12MB

Reliability:
  Network Failures:      3
  Corruption Detections: 7
  Timeout Events:        1
  Isolation Violations:  0
```

## Security Features Tested

1. **Package Name Injection Prevention**
   - Validates package names (alphanumeric, hyphens, underscores only)
   - Max 100 characters
   - No path separators or traversal

2. **ZIP Bomb Protection**
   - 100MB uncompressed size limit
   - 10,000 file limit
   - Early termination on size violations

3. **Path Traversal Prevention**
   - Validates all extracted paths
   - Prevents directory traversal attacks
   - Ensures files stay within container

4. **Checksum Verification**
   - SHA256 verification for all packages
   - Detects corrupted or tampered packages
   - Atomic lockfile operations

## Performance Guarantees

- **Dry-Run Validation**: < 33 seconds for any valid package
- **Download Timeout**: 60 seconds
- **Search Timeout**: 30 seconds
- **Retry Strategy**: 3 attempts with exponential backoff (1s, 2s, 4s)

## Integration with CI/CD

Add to your CI pipeline:

```yaml
- name: Run Marketplace Stress Tests
  run: |
    cargo test --package ggen-cli-lib --test stress -- --nocapture
```

## Troubleshooting

### Tests Timing Out
- Increase timeout values in `ContainerStressConfig`
- Reduce `sequential_operations` count
- Check system resources

### Memory Issues
- Reduce `max_memory_bytes` in config
- Run tests sequentially instead of in parallel
- Increase system swap space

### Network Test Failures
- Check network connectivity
- Verify firewall settings
- Adjust `network_timeout` duration

## Contributing

When adding new stress tests:

1. Follow the existing test structure
2. Add comprehensive metrics collection
3. Include detailed documentation
4. Update this README with new test descriptions
5. Ensure tests are deterministic and reproducible

## Related Files

- `marketplace_stress_test.rs` - General marketplace stress tests
- `/home/user/ggen/crates/ggen-domain/src/marketplace/install.rs` - Installation logic
- `/home/user/ggen/crates/ggen-domain/src/marketplace/validate.rs` - Validation logic
- `/home/user/ggen/crates/ggen-domain/src/marketplace/registry.rs` - Registry management

## References

- [Marketplace Documentation](../../README.md)
- [Security Model](../../../docs/security.md)
- [Performance Benchmarks](../../benches/marketplace_benchmark.rs)
