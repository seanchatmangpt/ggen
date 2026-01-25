<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [TPS Integration Testing Strategy & Patterns](#tps-integration-testing-strategy--patterns)
  - [Overview](#overview)
  - [Test Categories](#test-categories)
    - [1. End-to-End Workflow Tests](#1-end-to-end-workflow-tests)
    - [2. Smoke Tests](#2-smoke-tests)
    - [3. Regression Tests](#3-regression-tests)
    - [4. Performance Regression Tests](#4-performance-regression-tests)
    - [5. Chaos Tests](#5-chaos-tests)
    - [6. Test Data Generation](#6-test-data-generation)
      - [Payment Data](#payment-data)
      - [Deployment Data](#deployment-data)
      - [Queue Workloads](#queue-workloads)
      - [Failure Scenarios](#failure-scenarios)
      - [Load Profiles](#load-profiles)
      - [Cross-Principle Scenarios](#cross-principle-scenarios)
  - [Chicago TDD Pattern in Detail](#chicago-tdd-pattern-in-detail)
    - [AAA (Arrange/Act/Assert)](#aaa-arrangeactassert)
    - [Real Objects, Not Mocks](#real-objects-not-mocks)
    - [State-Based Verification](#state-based-verification)
    - [Behavior Verification](#behavior-verification)
  - [Running Tests](#running-tests)
    - [Quick Feedback (Smoke Tests)](#quick-feedback-smoke-tests)
    - [Unit Tests Only](#unit-tests-only)
    - [Full Test Suite](#full-test-suite)
    - [Specific Test Module](#specific-test-module)
    - [With Logging](#with-logging)
    - [Performance Tests Only](#performance-tests-only)
  - [Snapshot Testing Workflow](#snapshot-testing-workflow)
    - [Initial Setup](#initial-setup)
    - [When Tests Fail](#when-tests-fail)
    - [Snapshot Location](#snapshot-location)
  - [Performance Regression Workflow](#performance-regression-workflow)
    - [Establishing Baselines](#establishing-baselines)
    - [Detecting Regressions](#detecting-regressions)
    - [SLO Monitoring](#slo-monitoring)
  - [Test Organization in Codebase](#test-organization-in-codebase)
  - [Integration with CI/CD](#integration-with-cicd)
    - [Pre-Commit Hooks](#pre-commit-hooks)
    - [Pull Request Checks](#pull-request-checks)
    - [Production Deployment](#production-deployment)
  - [Debugging Failed Tests](#debugging-failed-tests)
    - [1. Run with Logging](#1-run-with-logging)
    - [2. Run Single Test](#2-run-single-test)
    - [3. Run with Backtrace](#3-run-with-backtrace)
    - [4. Check Snapshot Diff](#4-check-snapshot-diff)
    - [5. Performance Profile](#5-performance-profile)
  - [Best Practices](#best-practices)
    - [1. Test Independence](#1-test-independence)
    - [2. Clear Assertions](#2-clear-assertions)
    - [3. Realistic Scenarios](#3-realistic-scenarios)
    - [4. Performance-Aware](#4-performance-aware)
    - [5. Documentation](#5-documentation)
  - [Common Issues & Solutions](#common-issues--solutions)
    - [Issue: Snapshot Tests Keep Failing](#issue-snapshot-tests-keep-failing)
    - [Issue: Performance Tests Flaky](#issue-performance-tests-flaky)
    - [Issue: Chaos Tests Timeout](#issue-chaos-tests-timeout)
  - [Extending the Test Suite](#extending-the-test-suite)
    - [Adding a New End-to-End Test](#adding-a-new-end-to-end-test)
    - [Adding a New Smoke Test](#adding-a-new-smoke-test)
    - [Adding a Chaos Scenario](#adding-a-chaos-scenario)
    - [Adding Test Data](#adding-test-data)
  - [References](#references)
  - [Contact & Support](#contact--support)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# TPS Integration Testing Strategy & Patterns

Complete guide to testing Toyota Production System (TPS) principles in ggen.

## Overview

Testing framework for verifying all TPS principle interactions:
- **Jidoka** (autonomic response): Quality checks that trigger automatic failures
- **Kanban** (flow optimization): Work-in-progress limits and queue management
- **Andon** (visibility): Problem signaling, metrics, and alerting
- **Kaizen** (continuous improvement): Monitoring and diagnostics

**Testing Philosophy**: Chicago TDD pattern with real objects, state-based verification, and behavior assertions.

## Test Categories

### 1. End-to-End Workflow Tests

**Location**: `/tests/tps/end_to_end_workflow_tests.rs`

**Purpose**: Verify complete request lifecycle through all system components.

**Test Patterns**:
- Payment workflow: Request → Processing → Andon signals
- Deployment workflow: Deploy → Health checks → Monitoring
- Complete cycle: Payment → Deployment → Monitoring

**Chicago TDD Pattern**:
```rust
// Arrange: Set up system with all components
let system = FullTpsSystem::new().await?;

// Act: Execute complete workflow
let payment_result = system.process_payment(&request).await;
let deploy_result = system.auto_deploy(&deployment).await;

// Assert: Verify observable outputs and state changes
assert!(payment_result.is_ok());
assert!(system.monitoring_status().is_active());
```

**Key Assertions**:
- Request succeeds with correct outputs
- SLOs are met (e.g., payment <5s)
- Andon signals are recorded
- Metrics are collected
- Queue depth is managed

### 2. Smoke Tests

**Location**: `/tests/tps/smoke_tests.rs`

**Purpose**: Rapid validation for CI/CD gates (<30s total).

**Test Patterns**:
- Core system initialization
- Payment processing SLO verification
- Jidoka quality checks
- Kanban WIP limit enforcement
- Andon alert firing
- Metrics collection
- Trace propagation

**Execution**: `cargo make test-unit` (runs smoke tests with fast feedback)

**SLO Targets**:
- Payment: <5s p99
- Deployment: <30s
- Alert response: <5s
- Metrics: <50ms
- Trace export: <500ms

### 3. Regression Tests

**Location**: `/tests/tps/regression_tests.rs`

**Purpose**: Deterministic output verification using snapshot testing.

**Pattern**:
```rust
// Act: Generate output
let result = system.process(&request).await?;

// Assert: Verify against baseline snapshot (using insta crate)
let output = serde_json::to_string_pretty(&result)?;
insta::assert_snapshot!("payment_output_deterministic", output);
```

**Snapshots Stored**: `tests/tps/snapshots/`

**Process**:
1. Test generates output
2. First run: Creates baseline snapshot
3. Subsequent runs: Compares against baseline
4. If output changes: Review and approve with `insta review`

**Coverage**:
- Payment output format
- Andon signal structure
- Deployment configuration
- Error message consistency
- Kanban state transitions
- Metrics format
- Trace format
- Cross-principle interactions

### 4. Performance Regression Tests

**Location**: `/tests/tps/performance_regression_tests.rs`

**Purpose**: Verify system performance meets SLOs with baseline metrics.

**Execution**: `cargo make bench` or `cargo make slo-check`

**Patterns**:
```rust
// Arrange: Run multiple iterations to build statistics
let mut latencies = Vec::new();
for i in 0..100 {
    let start = Instant::now();
    system.process(&request).await;
    latencies.push(start.elapsed());
}

// Act: Calculate percentiles
latencies.sort();
let p99 = latencies[latencies.len() * 99 / 100];

// Assert: Verify SLO
assert!(p99 < Duration::from_secs(5), "Payment P99 SLO violation");
```

**SLO Metrics**:
- **Payment Processing**: p99 <5s, p95 <3s, avg <2s
- **Metrics Recording**: max <50ms, p99 <40ms
- **Deployment**: <30s
- **Alert Response**: <5s, p99 <2s
- **Trace Export**: max <500ms
- **Jidoka Decision**: max <100ms
- **Kanban Queue**: max <50ms
- **Memory**: <100MB
- **Throughput**: >100 req/s
- **Concurrency**: >100 concurrent requests

### 5. Chaos Tests

**Location**: `/tests/tps/chaos_tests.rs`

**Purpose**: Verify resilience under failure conditions.

**Failure Scenarios**:
- Network delays (latency injection)
- Packet loss (dropped requests)
- Service unavailability (outages)
- Resource exhaustion (memory pressure)
- Cascade failures (component failures)
- Circuit breaker protection
- Timeout handling
- Overload degradation

**Pattern**:
```rust
// Arrange: Simulate failure condition
let system = ChaosNetworkSystem::new(delay_ms: 500);

// Act: Process requests under failure
let successes = execute_concurrent_requests(&system, 10).await;

// Assert: System degrades gracefully
assert!(successes >= 7, "70%+ success under network delay");
```

**Chaos Experiment Tracking**:
- Before-failure baseline metrics
- During-failure behavior
- Recovery characteristics
- Time to restore normal operation

### 6. Test Data Generation

**Location**: `/tests/tps/test_data_generation.rs`

**Purpose**: Generate realistic test scenarios programmatically.

**Data Builders**:

#### Payment Data
```rust
let payments = PaymentTestDataBuilder::new()
    .with_amount(150.00)
    .build();
// Generates: small/medium/large amounts across currencies
```

#### Deployment Data
```rust
let deployments = DeploymentTestDataBuilder::new().build();
// Generates: all service × version × replica combinations
```

#### Queue Workloads
```rust
let workloads = QueueWorkloadBuilder::new().build();
// Generates: items with varying priorities and sizes
```

#### Failure Scenarios
```rust
let failures = FailureScenarioBuilder::new().build();
// Generates: quality checks, timeouts, network issues, resource exhaustion
```

#### Load Profiles
```rust
let realistic = LoadProfileBuilder::new().build_realistic_load();
// 24-hour traffic pattern with realistic peaks and valleys

let spike = LoadProfileBuilder::new().build_spike_load();
// Sudden traffic burst to test overload handling
```

#### Cross-Principle Scenarios
```rust
let scenarios = CrossPrincipleScenarioBuilder::new().build();
// Jidoka + Kanban + Andon interaction scenarios
```

## Chicago TDD Pattern in Detail

### AAA (Arrange/Act/Assert)

**Arrange**: Set up system state
```rust
let system = PaymentWorkflow::new().await?;
let request = PaymentRequest { amount: 99.99, ... };
```

**Act**: Execute the behavior
```rust
let result = system.process(&request).await;
```

**Assert**: Verify observable outputs and state changes
```rust
assert!(result.is_ok());
assert_eq!(result.unwrap().status, "SUCCESS");
```

### Real Objects, Not Mocks

❌ **Don't**: Create mock/stub implementations
```rust
// Bad: Using MockPaymentService
let mock_service = MockPaymentService::new();
```

✅ **Do**: Use real implementations with testcontainers
```rust
// Good: Real system initialized
let system = PaymentWorkflow::new().await?;
```

### State-Based Verification

❌ **Don't**: Verify method calls (interaction-based)
```rust
// Bad: Testing the implementation
assert_eq!(mock_logger.call_count, 5);
```

✅ **Do**: Verify observable outputs and state changes
```rust
// Good: Testing observable behavior
let signals = system.andon_signals().await;
assert!(!signals.is_empty());
```

### Behavior Verification

❌ **Don't**: Meaningless assertions
```rust
// Bad: Doesn't verify behavior
assert!(system.process(&request).await.is_ok());
```

✅ **Do**: Verify specific behavior
```rust
// Good: Verifies specific observable behavior
let result = system.process(&request).await?;
assert_eq!(result.transaction_id, expected_id);
assert!(system.queue_depth().await == 0);
assert!(!system.andon_signals().await.is_empty());
```

## Running Tests

### Quick Feedback (Smoke Tests)
```bash
cargo make test-unit
# Runs smoke tests in ~30s
# Good for rapid iteration during development
```

### Unit Tests Only
```bash
cargo make test-unit
# Runs all unit tests and smoke tests
# Local development, CI gates
```

### Full Test Suite
```bash
cargo make test
# Runs all tests: unit + integration + chaos + performance
# Comprehensive validation, slower (~60s)
```

### Specific Test Module
```bash
cargo test --test lifecycle_tests
cargo test --test regression_tests
```

### With Logging
```bash
RUST_LOG=debug cargo test -- --nocapture
```

### Performance Tests Only
```bash
cargo make bench
# Runs benchmarks and performance regression tests
```

## Snapshot Testing Workflow

### Initial Setup
```bash
# Run tests (creates snapshots)
cargo test

# Review snapshots
insta review
# Accept new snapshots (a), reject (r), skip (s)
```

### When Tests Fail
```bash
# Test fails if snapshot differs
# Review the diff
insta review

# Option 1: Accept if change is correct
# Option 2: Reject and fix code
```

### Snapshot Location
```
tests/tps/snapshots/
├── tps_regression/
│   ├── payment_output_deterministic.snap
│   ├── andon_signal_format.snap
│   └── ...
```

## Performance Regression Workflow

### Establishing Baselines
First run creates baseline metrics:
```bash
cargo make bench
# Creates baseline in: target/bench/baselines/
```

### Detecting Regressions
Subsequent runs compare against baseline:
```bash
cargo make bench
# If metrics exceed threshold: FAIL
# If metrics improve: can accept and update baseline
```

### SLO Monitoring
```bash
cargo make slo-check
# Verifies all SLOs are met
# Used in CI/CD gates
```

## Test Organization in Codebase

```
ggen/
├── tests/
│   ├── tps/
│   │   ├── mod.rs                          # Module hub
│   │   ├── end_to_end_workflow_tests.rs    # Complete flows
│   │   ├── smoke_tests.rs                  # Fast validation
│   │   ├── regression_tests.rs             # Snapshots
│   │   ├── performance_regression_tests.rs # SLO tracking
│   │   ├── chaos_tests.rs                  # Resilience
│   │   ├── test_data_generation.rs         # Data builders
│   │   └── snapshots/                      # Snapshot baselines
│   └── ...
├── docs/
│   └── testing/
│       ├── TPS_TESTING_STRATEGY.md         # This file
│       ├── CHICAGO_TDD_PATTERNS.md         # AAA pattern guide
│       └── SLO_DEFINITIONS.md              # SLO targets
└── ...
```

## Integration with CI/CD

### Pre-Commit Hooks
```bash
cargo make pre-commit
# Runs: check → lint → test-unit
# Fast feedback loop (~2min)
```

### Pull Request Checks
```bash
cargo make ci
# Runs: check → lint → test → slo-check
# Comprehensive validation (~5min)
```

### Production Deployment
```bash
cargo make release-validate
# Runs: check → lint → test → slo-check → audit
# Final safety gate before deployment
```

## Debugging Failed Tests

### 1. Run with Logging
```bash
RUST_LOG=debug cargo test --test end_to_end_workflow_tests -- --nocapture
```

### 2. Run Single Test
```bash
cargo test payment_workflow_end_to_end -- --nocapture
```

### 3. Run with Backtrace
```bash
RUST_BACKTRACE=1 cargo test
```

### 4. Check Snapshot Diff
```bash
# Review tests/tps/snapshots/ for *.snap.new files
insta review
```

### 5. Performance Profile
```bash
cargo make profile
# Generate detailed performance traces
```

## Best Practices

### 1. Test Independence
- Each test should be runnable independently
- No shared state between tests
- Use separate test data for each test

### 2. Clear Assertions
```rust
// Bad: Unclear what failed
assert!(result.is_ok());

// Good: Clear failure message
assert!(
    result.is_ok(),
    "Payment should succeed for valid request"
);
assert_eq!(
    result.unwrap().status,
    "SUCCESS",
    "Payment status should be SUCCESS"
);
```

### 3. Realistic Scenarios
- Use `AllTestDataProvider` for realistic test data
- Cover edge cases (0 amount, 1 replica, etc.)
- Test with actual failure conditions (chaos)

### 4. Performance-Aware
- Include SLO assertions in all async operations
- Track latency percentiles (p50, p95, p99)
- Monitor memory and throughput

### 5. Documentation
```rust
/// Test: Verify payment processing under network delay
///
/// Pattern: Chaos test with network latency injection
/// SLO: Should maintain >70% success rate
/// Recovery: System continues after delay clears
#[tokio::test]
async fn chaos_network_delay_recovery() {
    // ...
}
```

## Common Issues & Solutions

### Issue: Snapshot Tests Keep Failing

**Cause**: Output is non-deterministic

**Solution**: Ensure deterministic output
```rust
// Deterministic ordering
let mut results = results;
results.sort_by_key(|r| r.id);

// Fixed timestamps
let timestamp = "2026-01-25T00:00:00Z".to_string();
```

### Issue: Performance Tests Flaky

**Cause**: System load varies, timing sensitive

**Solution**: Use statistical approach
```rust
// Don't rely on single measurement
let avg = latencies.iter().sum::<Duration>() / latencies.len();
let p99 = latencies[latencies.len() * 99 / 100];

// Accept reasonable variance
assert!(p99 < expected * 1.1, "Allow 10% variance");
```

### Issue: Chaos Tests Timeout

**Cause**: Simulated failure blocks indefinitely

**Solution**: Use explicit timeouts
```rust
let result = tokio::time::timeout(
    Duration::from_secs(5),
    system.operation()
).await;
```

## Extending the Test Suite

### Adding a New End-to-End Test

1. Create test case in `end_to_end_workflow_tests.rs`
2. Follow AAA pattern
3. Verify SLOs are met
4. Document with clear comments

### Adding a New Smoke Test

1. Create test in `smoke_tests.rs`
2. Keep execution <30s total
3. Focus on critical paths
4. Use simple assertions

### Adding a Chaos Scenario

1. Create system in `chaos_tests.rs`
2. Simulate specific failure
3. Verify graceful degradation
4. Document expected behavior

### Adding Test Data

1. Create builder in `test_data_generation.rs`
2. Generate realistic variations
3. Add unit test for builder
4. Update `AllTestDataProvider`

## References

- **Chicago TDD**: State-based testing with real objects
- **Chaos Engineering**: Deliberately inject failures to test resilience
- **Performance Testing**: Establish SLOs and track baselines
- **Snapshot Testing**: `insta` crate for deterministic output verification
- **Toyota Production System**: Manufacturing principles applied to software

## Contact & Support

For questions about testing strategy:
1. Review this documentation
2. Check existing test examples
3. Consult CLAUDE.md for Chicago TDD pattern
4. Review ggen-tps-andon crate for Andon principles
