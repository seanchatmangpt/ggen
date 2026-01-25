# TAI Testing: Production Hardening & Resilience Testing Framework

[![Crate Badge](https://img.shields.io/crates/v/tai-testing.svg)](https://crates.io/crates/tai-testing)
[![Documentation](https://docs.rs/tai-testing/badge.svg)](https://docs.rs/tai-testing)
[![License](https://img.shields.io/crates/l/tai-testing.svg)](https://github.com/seanchatmangpt/ggen/blob/main/LICENSE)

Comprehensive testing framework for verifying production systems survive failure and meet compliance requirements.

## Features

- **Chaos Engineering**: Deliberately inject failures to verify recovery
- **Property-Based Testing**: Verify invariants hold under arbitrary conditions
- **Compliance Validation**: Automated compliance checking (FISMA, FedRAMP, SOC 2, HIPAA, PCI-DSS)
- **Load Testing**: Determine system capacity before production deployment

## Quick Start

### Chaos Engineering

```rust
use tai_testing::ChaosExperiment;

#[tokio::main]
async fn main() {
    // Create a pod kill experiment
    let exp = ChaosExperiment::pod_kill("prod-cluster", "api-service", 3);

    // Run and collect metrics
    let metrics = exp.execute().await.expect("experiment should succeed");

    println!("Recovery time: {}ms", metrics.recovery_time_ms);
    println!("Error rate: {:.2}%", metrics.error_rate_during_failure * 100.0);
    println!("P99 latency: {:.2}ms", metrics.latency_p99_ms);
}
```

### Property-Based Testing

```rust
use tai_testing::StateInvariant;

#[tokio::main]
async fn main() {
    // Verify circuit breaker invariant
    let invariant = StateInvariant::circuit_breaker_never_stuck_open();
    let result = invariant.verify().await.expect("should succeed");

    assert!(result.passed, "Invariant must hold");
    println!("Code paths exercised: {:?}", result.code_paths_exercised);
}
```

### Compliance Validation

```rust
use tai_testing::ComplianceFramework;

#[tokio::main]
async fn main() {
    // Run FISMA compliance audit
    let framework = ComplianceFramework::fisma();
    let result = framework.audit().await.expect("audit should succeed");

    println!("Compliant: {}", result.is_compliant());
    println!("Controls: {}/{}", result.compliant_controls, result.total_controls);

    if !result.is_compliant() {
        println!("Remediation: {}", result.remediation_summary());
    }
}
```

### Load Testing

```rust
use tai_testing::LoadTest;
use std::time::Duration;

#[tokio::main]
async fn main() {
    // Run load test
    let test = LoadTest::new("http://api.example.com")
        .with_ramp_up(10, 100)
        .with_duration(Duration::from_secs(300));

    let result = test.run().await.expect("load test should succeed");

    println!("Throughput: {:.0} RPS", result.throughput_rps);
    println!("P99 latency: {:.2}ms", result.latencies.p99_ms);
    println!("Error rate: {:.2}%", result.error_rate * 100.0);
    println!("SLO passed: {}", result.passed_slo());
}
```

## Chaos Experiments

### Available Experiments

- **Pod Kill**: Randomly terminate pods
- **Network Partition**: Split cluster into isolated partitions
- **CPU Throttling**: Limit CPU resources
- **Memory Pressure**: Reduce available memory
- **Disk Exhaustion**: Fill disk space
- **Clock Skew**: Advance system time
- **Cascading Failure**: Kill services in dependency order

### Example: Network Partition

```rust
let exp = ChaosExperiment::network_partition("prod", "database-service");
let metrics = exp.execute().await?;

// Verify recovery
assert!(metrics.recovery_time_ms < 120_000); // 2 minutes
assert!(metrics.error_rate_during_failure > 0.5); // Expected to fail
```

## Property-Based Testing

### Invariants Tested

- **Circuit Breaker**: Never stuck open forever
- **Queue Messages**: Never lost
- **Firestore Transactions**: Atomic (all or nothing)
- **Cache Consistency**: Matches source truth
- **Latency Bounded**: Meets SLO
- **Error Rate**: Below threshold

### Example: Queue Preservation

```rust
let invariant = StateInvariant::queue_message_preservation();
let result = invariant.verify().await?;

if !result.passed {
    for violation in &result.violations {
        println!("Lost message: {}", violation.minimal_example);
    }
}
```

## Compliance Frameworks

### Supported Frameworks

- **FISMA**: Federal Information Security Management Act
- **FedRAMP**: Federal Risk and Authorization Management Program
- **SOC 2**: Service Organization Control
- **HIPAA**: Health Insurance Portability and Accountability Act
- **PCI-DSS**: Payment Card Industry Data Security Standard

### Example: HIPAA Audit

```rust
let framework = ComplianceFramework::hipaa();
let result = framework.audit().await?;

println!("Compliant: {}", result.is_compliant());

for violation in result.high_severity_violations() {
    println!("Fix: {}", violation.control_description);
    for step in &violation.remediation {
        println!("  - {}", step);
    }
}
```

## Load Testing

### Test Types

- **Ramp-up**: Gradually increase load
- **Spike**: Sudden load increase
- **Soak**: Sustained load for extended period
- **Stress**: Push beyond capacity

### Example: Stress Test

```rust
let test = LoadTest::new("http://api.example.com")
    .with_stress(10000)
    .with_duration(Duration::from_secs(300))
    .with_read_write_ratio(0.8, 0.2);

let result = test.run().await?;

println!("Breaking point: {:.0} RPS", result.throughput_rps);
println!("Peak latency: {:.0}ms", result.latencies.max_ms);
```

## Integration Tests

```rust
#[tokio::test]
async fn test_system_production_readiness() {
    // 1. Chaos: Verify recovery
    let chaos = ChaosExperiment::pod_kill("prod", "api", 2);
    let chaos_result = chaos.execute().await?;
    assert!(chaos_result.recovered_successfully);

    // 2. Properties: Verify invariants
    let invariant = StateInvariant::latency_bounded();
    let inv_result = invariant.verify().await?;
    assert!(inv_result.passed);

    // 3. Compliance: Verify regulations
    let compliance = ComplianceFramework::fisma();
    let comp_result = compliance.audit().await?;
    assert!(comp_result.is_compliant());

    // 4. Load: Verify capacity
    let load = LoadTest::new("http://api.example.com")
        .with_ramp_up(10, 100);
    let load_result = load.run().await?;
    assert!(load_result.passed_slo());
}
```

## Documentation

For comprehensive documentation, see [TAI Testing Guide](../../docs/tai-testing/90-testing.md)

## Performance

Execution times (approximate):

| Test Type | Duration |
|-----------|----------|
| Property-based (100 iterations) | 1-2 seconds |
| Chaos (pod kill) | 2-5 minutes |
| Load test (5 min ramp-up) | 5-10 minutes |
| Compliance audit | 30-60 seconds |

## Contributing

Contributions welcome! To add new:
- Chaos experiments: See `chaos.rs`
- Invariants: See `property.rs`
- Compliance frameworks: See `compliance.rs`
- Load test types: See `load.rs`

## License

Licensed under either of:

- Apache License, Version 2.0 ([LICENSE-APACHE](LICENSE-APACHE) or http://www.apache.org/licenses/LICENSE-2.0)
- MIT license ([LICENSE-MIT](LICENSE-MIT) or http://opensource.org/licenses/MIT)

at your option.

## Support

For issues and questions:
- **GitHub Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Discussions**: https://github.com/seanchatmangpt/ggen/discussions
