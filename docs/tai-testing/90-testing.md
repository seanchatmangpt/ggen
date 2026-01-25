<!-- START doctoc generated TOC please keep comment here to allow auto update -->
<!-- DON'T EDIT THIS SECTION, INSTEAD RE-RUN doctoc TO UPDATE -->
**Table of Contents**

- [TAI Testing: Production Hardening & Resilience Testing Framework](#tai-testing-production-hardening--resilience-testing-framework)
  - [Overview](#overview)
    - [Core Components](#core-components)
    - [Philosophy](#philosophy)
  - [Chaos Engineering](#chaos-engineering)
    - [Purpose](#purpose)
    - [Benefits](#benefits)
    - [When to Use](#when-to-use)
    - [Chaos Experiments Supported](#chaos-experiments-supported)
      - [Pod Kill](#pod-kill)
      - [Network Partition](#network-partition)
      - [CPU Throttling](#cpu-throttling)
      - [Memory Pressure](#memory-pressure)
      - [Disk Exhaustion](#disk-exhaustion)
      - [Clock Skew](#clock-skew)
      - [Cascading Failure](#cascading-failure)
    - [Running Chaos Experiments](#running-chaos-experiments)
      - [In Staging](#in-staging)
      - [As Scheduled Jobs](#as-scheduled-jobs)
      - [In Load Tests](#in-load-tests)
    - [Interpreting Metrics](#interpreting-metrics)
  - [Property-Based Testing](#property-based-testing)
    - [Purpose](#purpose-1)
    - [Benefits](#benefits-1)
    - [Invariants Verified](#invariants-verified)
      - [Circuit Breaker Never Stuck Open](#circuit-breaker-never-stuck-open)
      - [Queue Never Loses Messages](#queue-never-loses-messages)
      - [Firestore Transactions Are Atomic](#firestore-transactions-are-atomic)
      - [Cache Consistency Maintained](#cache-consistency-maintained)
      - [Latency Bounded by SLO](#latency-bounded-by-slo)
      - [Error Rate Below Threshold](#error-rate-below-threshold)
    - [Running Property Tests](#running-property-tests)
      - [In Unit Tests](#in-unit-tests)
      - [With Custom Iterations](#with-custom-iterations)
      - [Interpreting Results](#interpreting-results)
    - [Implementing Custom Invariants](#implementing-custom-invariants)
  - [Compliance Validation](#compliance-validation)
    - [Purpose](#purpose-2)
    - [Benefits](#benefits-2)
    - [Frameworks Supported](#frameworks-supported)
      - [FISMA (Federal Information Security Management Act)](#fisma-federal-information-security-management-act)
      - [FedRAMP (Federal Risk and Authorization Management Program)](#fedramp-federal-risk-and-authorization-management-program)
      - [SOC 2 (Service Organization Control)](#soc-2-service-organization-control)
      - [HIPAA (Health Insurance Portability and Accountability Act)](#hipaa-health-insurance-portability-and-accountability-act)
      - [PCI-DSS (Payment Card Industry Data Security Standard)](#pci-dss-payment-card-industry-data-security-standard)
    - [Running Compliance Audits](#running-compliance-audits)
      - [Manual Audit](#manual-audit)
      - [Continuous Compliance](#continuous-compliance)
      - [Scheduled Audits](#scheduled-audits)
    - [Interpreting Results](#interpreting-results-1)
    - [Example Compliance Report](#example-compliance-report)
  - [Load Testing](#load-testing)
    - [Purpose](#purpose-3)
    - [Benefits](#benefits-3)
    - [Load Test Types](#load-test-types)
      - [Ramp-Up Test](#ramp-up-test)
      - [Spike Test](#spike-test)
      - [Soak Test](#soak-test)
      - [Stress Test](#stress-test)
    - [Running Load Tests](#running-load-tests)
      - [Basic Load Test](#basic-load-test)
      - [Load Test with Read/Write Mix](#load-test-with-readwrite-mix)
      - [Load Test in CI/CD](#load-test-in-cicd)
    - [SLO Definition](#slo-definition)
    - [Interpreting Load Test Results](#interpreting-load-test-results)
      - [Throughput](#throughput)
      - [Latency Percentiles](#latency-percentiles)
      - [Error Rate](#error-rate)
      - [Resource Utilization](#resource-utilization)
    - [Load Testing Best Practices](#load-testing-best-practices)
  - [Integrating Tests Together](#integrating-tests-together)
    - [Full System Testing](#full-system-testing)
    - [Testing Pipeline](#testing-pipeline)
    - [Continuous Testing](#continuous-testing)
  - [Troubleshooting](#troubleshooting)
    - [Chaos Experiment Fails](#chaos-experiment-fails)
    - [Property Test Reports Violations](#property-test-reports-violations)
    - [Load Test Shows SLO Violation](#load-test-shows-slo-violation)
    - [Compliance Audit Fails](#compliance-audit-fails)
  - [Performance Characteristics](#performance-characteristics)
    - [Execution Times (Approximate)](#execution-times-approximate)
    - [Resource Requirements](#resource-requirements)
  - [API Reference](#api-reference)
    - [Chaos Engineering](#chaos-engineering-1)
    - [Property-Based Testing](#property-based-testing-1)
    - [Compliance Validation](#compliance-validation-1)
    - [Load Testing](#load-testing-1)
  - [Contributing](#contributing)
  - [Support & Resources](#support--resources)
  - [License](#license)

<!-- END doctoc generated TOC please keep comment here to allow auto update -->

# TAI Testing: Production Hardening & Resilience Testing Framework

**Version**: 0.1.0 | **Status**: Production-Ready | **Last Updated**: January 2026

## Overview

TAI Testing is a comprehensive testing framework for verifying production systems can survive failure and meet compliance requirements. It treats testing as a first-class citizen in system design, not an afterthought.

### Core Components

- **Chaos Engineering**: Deliberately inject failures to verify recovery
- **Property-Based Testing**: Verify invariants hold under arbitrary conditions
- **Compliance Validation**: Automated compliance checking (FISMA, FedRAMP, SOC 2, HIPAA, PCI-DSS)
- **Load Testing**: Determine system capacity before production deployment

### Philosophy

1. **Test in Production-Like Conditions**: Chaos experiments run in staging/production replicas
2. **Observable Failures**: All failures instrumented with metrics, logs, traces
3. **Deterministic Resilience**: Know exactly how systems behave under failure
4. **Compliance as Code**: Compliance is automated, not spreadsheets
5. **Continuous Verification**: Tests run in CI/CD, failures block production

---

## Chaos Engineering

### Purpose

Chaos engineering deliberately injects failures to verify systems recover gracefully. It answers:
- Will this service survive a pod kill?
- Can the system handle network partitions?
- What happens under CPU exhaustion?
- How long does recovery take?

### Benefits

- **Find weaknesses before production**: Identify failure points in staging
- **Verify incident response**: Test documented procedures
- **Build confidence**: Know systems can survive failure
- **Prevent outages**: Fix issues before customers see them
- **Measure resilience**: Quantify recovery time

### When to Use

- Before deploying to production
- After major infrastructure changes
- As part of disaster recovery drills
- In load testing (inject failures under load)
- In on-call rotation preparation

### Chaos Experiments Supported

#### Pod Kill
Randomly terminate pods to test service resilience and auto-recovery.

**Use case**: Verify Kubernetes horizontal pod autoscaling works

```rust
let exp = ChaosExperiment::pod_kill("prod-cluster", "api-service", 3);
let metrics = exp.execute().await?;
println!("Recovery time: {}ms", metrics.recovery_time_ms);
```

**Expected behavior**:
- Service detects pod loss within 30-60 seconds
- Kubernetes reschedules replicas
- Service restores full capacity
- Errors are <30% during failure

**Metrics collected**:
- Recovery time
- Error rate during failure
- Latency impact (p50, p95, p99)
- Downstream service impact

#### Network Partition
Splits cluster into isolated partitions to test split-brain handling.

**Use case**: Verify system behaves correctly when a datacenter network fails

```rust
let exp = ChaosExperiment::network_partition("prod", "database-service");
let metrics = exp.execute().await?;
```

**Expected behavior**:
- Services detect partition within 5-10 seconds
- Requests fail fast with clear errors
- System avoids split-brain (conflicting writes)
- Recovery is automatic when partition heals

#### CPU Throttling
Limits CPU resources to test graceful degradation.

**Use case**: Verify service handles CPU contention

```rust
let exp = ChaosExperiment::cpu_throttling("prod", "compute-service");
```

**Expected behavior**:
- Latency increases proportionally
- Throughput decreases gracefully
- No hard failures
- Service recovers when CPU available

#### Memory Pressure
Reduces available memory to test OOM (Out of Memory) handling.

**Use case**: Verify memory-intensive operations have safeguards

```rust
let exp = ChaosExperiment::memory_pressure("prod", "cache-service");
```

**Expected behavior**:
- Service evicts least-recently-used cache entries
- Request latency increases slightly
- No memory leaks
- Service remains responsive

#### Disk Exhaustion
Fills disk to test handling of disk space failures.

**Use case**: Verify database can handle full disks

```rust
let exp = ChaosExperiment::disk_exhaustion("prod", "database");
```

**Expected behavior**:
- Service detects low disk and creates alerts
- Writes fail gracefully (not crash)
- Cleanup jobs delete old data
- Recovery once disk is cleared

#### Clock Skew
Advances system clock to test timeout handling.

**Use case**: Verify JWT expiry and scheduled jobs work correctly

```rust
let exp = ChaosExperiment::clock_skew("prod", "auth-service");
```

**Expected behavior**:
- Token expiry works correctly
- Scheduled jobs trigger properly
- No cascading timeout failures
- Recovery automatic after test

#### Cascading Failure
Kills multiple services in dependency order.

**Use case**: Test graceful degradation when multiple services fail

```rust
let exp = ChaosExperiment::cascading_failure(
    "prod",
    vec!["auth", "payment", "inventory"]
);
```

**Expected behavior**:
- Services fail in dependency order
- Circuit breakers trigger
- User-facing errors are clear
- System recovers once dependencies are restored

### Running Chaos Experiments

#### In Staging

```rust
#[tokio::test]
async fn test_pod_kill_recovery() {
    let exp = ChaosExperiment::pod_kill("staging", "api-service", 2);
    let metrics = exp.execute().await.expect("should succeed");

    // Verify recovery time is acceptable
    assert!(metrics.recovery_time_ms < 120_000); // 2 minutes

    // Verify errors are bounded
    assert!(metrics.error_rate_during_failure < 0.5); // <50%

    // Verify latency impact
    assert!(metrics.latency_p99_ms < 500.0); // <500ms
}
```

#### As Scheduled Jobs

```bash
# Run weekly chaos tests
0 2 * * 0 /opt/ggen/bin/chaos-test --experiment pod-kill --cluster prod-replica
```

#### In Load Tests

```rust
// Combine chaos with load testing
let load_test = LoadTest::new("http://api.example.com")
    .with_ramp_up(100, 1000)
    .with_duration(Duration::from_secs(300));

let exp = ChaosExperiment::pod_kill("prod", "api", 2);

// Run load test while injecting chaos
let (load_result, chaos_result) = tokio::join!(
    load_test.run(),
    inject_chaos_during_load(&exp, 150) // Start chaos at 150 seconds
);
```

### Interpreting Metrics

**Recovery Time**
- < 1 minute: Excellent
- 1-5 minutes: Good
- 5-15 minutes: Acceptable
- > 15 minutes: Needs improvement

**Error Rate During Failure**
- < 10%: Service handles well
- 10-30%: Service is aware of failure
- 30-50%: Some cascading effect
- > 50%: Poor failure isolation

**Latency Impact (p99)**
- < 50ms increase: Excellent
- 50-100ms: Good
- 100-500ms: Acceptable
- > 500ms: Needs optimization

**Recovery Metrics Summary**
- Successful recovery: System can recover from failure
- Partial recovery: System recovers but with manual intervention
- Failed recovery: System cannot recover (requires restart)

---

## Property-Based Testing

### Purpose

Property-based testing verifies that system invariants (properties that must always hold) remain true under arbitrary inputs and execution sequences.

### Benefits

- **Comprehensive coverage**: Tests thousands of input combinations automatically
- **Finds edge cases**: Discovers corner cases humans miss
- **Prevents regression**: Invariants prevent old bugs from returning
- **Documents behavior**: Properties serve as executable specifications
- **Minimal examples**: Failures shrink to minimal failing examples

### Invariants Verified

#### Circuit Breaker Never Stuck Open
**Property**: After being open for timeout period, circuit breaker transitions to half-open

**Why it matters**: Stuck-open circuit breakers cause cascading failures

```rust
let inv = StateInvariant::circuit_breaker_never_stuck_open();
let result = inv.verify().await?;
assert!(result.passed); // Invariant must hold
```

**Test approach**:
1. Generate arbitrary state transitions
2. Track timeout duration
3. Verify CB transitions to half-open
4. Shrink any failing examples

#### Queue Never Loses Messages
**Property**: Every message enqueued appears at dequeue

**Why it matters**: Message loss causes data corruption

```rust
let inv = StateInvariant::queue_message_preservation();
let result = inv.verify().await?;
assert!(result.passed);
```

**Test approach**:
1. Generate arbitrary enqueue/dequeue sequences
2. Simulate concurrent operations
3. Verify all messages eventually dequeue
4. Detect race conditions in queue implementation

#### Firestore Transactions Are Atomic
**Property**: Either all updates in transaction succeed or all fail

**Why it matters**: Partial transactions cause inconsistent data

```rust
let inv = StateInvariant::firestore_atomicity();
let result = inv.verify().await?;
assert!(result.passed);
```

**Test approach**:
1. Generate arbitrary transaction updates
2. Simulate network failures mid-transaction
3. Verify atomicity holds
4. Detect split-brain scenarios

#### Cache Consistency Maintained
**Property**: Cache values match source truth when in sync

**Why it matters**: Stale cache causes incorrect user actions

```rust
let inv = StateInvariant::cache_consistency();
let result = inv.verify().await?;
assert!(result.passed);
```

**Test approach**:
1. Generate arbitrary read/write patterns
2. Simulate cache invalidation delays
3. Verify cache eventually consistent
4. Detect consistency window violations

#### Latency Bounded by SLO
**Property**: All requests complete within SLO (100ms p99)

**Why it matters**: SLO violations degrade user experience

```rust
let inv = StateInvariant::latency_bounded();
let result = inv.verify().await?;
assert!(result.passed);
```

**Test approach**:
1. Generate arbitrary request patterns
2. Measure latency percentiles
3. Verify p99 < 100ms
4. Detect latency tail regressions

#### Error Rate Below Threshold
**Property**: System error rate stays below 1%

**Why it matters**: High error rates indicate reliability issues

```rust
let inv = StateInvariant::error_rate_limited();
let result = inv.verify().await?;
assert!(result.passed);
```

**Test approach**:
1. Generate arbitrary request sequences
2. Track successes and failures
3. Verify error rate < 1%
4. Detect systematic failures

### Running Property Tests

#### In Unit Tests

```rust
#[tokio::test]
async fn test_circuit_breaker_invariant() {
    let inv = StateInvariant::circuit_breaker_never_stuck_open()
        .with_iterations(1000); // Test 1000 scenarios

    let result = inv.verify().await.expect("should succeed");
    assert!(result.passed);
}
```

#### With Custom Iterations

```rust
// More iterations = more thorough but slower
let inv = StateInvariant::cache_consistency()
    .with_iterations(5000); // 5000 scenarios
let result = inv.verify().await?;
```

#### Interpreting Results

**Passed**: Invariant held across all iterations
- Code is correct for tested scenarios
- No detected violations

**Failed**: Invariant violated in at least one scenario
- Details show minimal failing example
- Usually indicates real bug
- Should be fixed before deployment

**Code Coverage**: Which code paths were exercised
- Higher coverage = more confidence
- Lower coverage = need more test scenarios

### Implementing Custom Invariants

```rust
// Create custom invariant
let custom_invariant = StateInvariant::new(
    InvariantType::CustomProperty,
    100 // iterations
);

// Verify it
let result = custom_invariant.verify().await?;
```

---

## Compliance Validation

### Purpose

Compliance validation automates verification that systems meet regulatory requirements. It answers:
- Is the system FISMA compliant?
- Do we meet HIPAA data protection requirements?
- Are PCI-DSS controls implemented?
- Which compliance gaps exist?
- What's the remediation plan?

### Benefits

- **Automated compliance**: No manual spreadsheets
- **Evidence collection**: Automatic proof of compliance
- **Continuous verification**: Run in CI/CD to prevent drift
- **Clear remediation**: Specific steps to fix violations
- **Audit ready**: Full audit trail for regulators

### Frameworks Supported

#### FISMA (Federal Information Security Management Act)

**Use when**: Building systems for US Federal agencies

**Key controls**:
- AC-2: Account Management
- AU-2: Audit Events
- SC-7: Boundary Protection
- SI-4: Information System Monitoring

```rust
let framework = ComplianceFramework::fisma();
let result = framework.audit().await?;

println!("Compliant: {}", result.is_compliant());
println!("Controls: {}/{}",
    result.compliant_controls,
    result.total_controls
);
```

#### FedRAMP (Federal Risk and Authorization Management Program)

**Use when**: Cloud services for US Federal government

**Stricter than FISMA**: Requires continuous monitoring

```rust
let framework = ComplianceFramework::fedramp();
let result = framework.audit().await?;
```

#### SOC 2 (Service Organization Control)

**Use when**: SaaS/cloud service provider

**Covers**:
- Security: Protect data from unauthorized access
- Availability: Services available as expected
- Processing Integrity: Complete and accurate processing
- Confidentiality: Sensitive data protected
- Privacy: Personal data handled per policy

```rust
let framework = ComplianceFramework::soc2();
let result = framework.audit().await?;
```

#### HIPAA (Health Insurance Portability and Accountability Act)

**Use when**: Handling healthcare data

**Key requirements**:
- Encrypt Protected Health Information (ePHI)
- Audit logging of data access
- Access controls by role
- Incident response procedures

```rust
let framework = ComplianceFramework::hipaa();
let result = framework.audit().await?;

// Check for ePHI-specific violations
for violation in result.high_severity_violations() {
    if violation.description.contains("ePHI") {
        println!("Critical: {}", violation.violation_description);
    }
}
```

#### PCI-DSS (Payment Card Industry Data Security Standard)

**Use when**: Handling payment card data

**12 Main Requirements**:
1. Firewall configuration
2. No default passwords
3. Protect cardholder data
4. Encrypt data in transit
5. Antivirus software
6. Security practices
7. Access restricted by business need
8. Identify and authenticate access
9. Restrict physical access
10. Track and monitor network access
11. Security testing
12. Policy that addresses information security

```rust
let framework = ComplianceFramework::pcidss();
let result = framework.audit().await?;

// PCI-DSS requires encryption of card data
assert!(result.compliant_controls > 11);
```

### Running Compliance Audits

#### Manual Audit

```rust
#[tokio::test]
async fn test_system_compliance() {
    let framework = ComplianceFramework::fisma();
    let result = framework.audit().await.expect("audit should succeed");

    // Ensure compliance
    assert!(result.is_compliant(), "System must be FISMA compliant");
    assert_eq!(result.non_compliant_controls, 0);
}
```

#### Continuous Compliance

```bash
# Run compliance audit in CI/CD
ggen test --framework fisma --fail-if-violations

# Output compliance report
ggen compliance report --output compliance-report.json
```

#### Scheduled Audits

```bash
# Weekly compliance audit with email notification
0 2 * * 0 /opt/ggen/bin/compliance-audit --framework fedramp \
    --email security-team@company.com
```

### Interpreting Results

**Compliant Status**
- `Compliant`: All controls implemented and verified
- `In Progress`: Controls are being implemented
- `Not Applicable`: Control doesn't apply to your system
- `Non-Compliant`: Control not implemented

**Violation Severity**
- `High`: Critical security/compliance gap (encryption, access control)
- `Medium`: Important but not critical (audit logging, monitoring)
- `Low`: Nice-to-have improvements (documentation, policies)

**Remediation Guidance**
Each violation includes:
1. Control ID and description
2. What failed and why
3. Step-by-step remediation
4. Target remediation date
5. Responsible team

### Example Compliance Report

```json
{
  "framework": "FISMA",
  "total_controls": 8,
  "compliant_controls": 6,
  "non_compliant_controls": 2,
  "in_progress_controls": 0,
  "not_applicable_controls": 0,
  "compliance_percentage": 75.0,
  "violations": [
    {
      "control_id": "SC-7",
      "control_description": "Boundary Protection",
      "violation_description": "Network ACLs not properly configured",
      "remediation": [
        "Review current network ACL rules",
        "Document required access paths",
        "Update ACLs to principle of least privilege",
        "Test changes in staging",
        "Deploy to production"
      ],
      "severity": "high",
      "target_date": "2026-02-18"
    }
  ]
}
```

---

## Load Testing

### Purpose

Load testing determines system capacity before production deployment. It answers:
- How many requests can the system handle?
- What's the throughput ceiling?
- How does latency degrade?
- What resources are needed?
- Where are the bottlenecks?

### Benefits

- **Capacity planning**: Know how many servers/pods needed
- **Performance validation**: Verify systems meet SLOs under load
- **Bottleneck identification**: Find limiting resources
- **Scaling verification**: Test horizontal/vertical scaling
- **Cost optimization**: Right-size infrastructure

### Load Test Types

#### Ramp-Up Test
Gradually increase load from 0 to peak.

**Use case**: Standard load testing, SLO verification

**Profile**:
- Start: 10 RPS
- Peak: 1000 RPS
- Duration: 5 minutes
- Ramp rate: Linear increase

```rust
let test = LoadTest::new("http://api.example.com")
    .with_ramp_up(10, 1000)
    .with_duration(Duration::from_secs(300));

let result = test.run().await?;
println!("Peak throughput: {} RPS", result.peak_rps as u64);
println!("P99 latency: {:.2}ms", result.latencies.p99_ms);
```

**Expected behavior**:
- Latency increases gradually with load
- Throughput increases linearly
- No sudden failures
- System recovers after peak

#### Spike Test
Sudden load increase to peak.

**Use case**: Test burst capacity, Black Friday scenarios

**Profile**:
- Baseline: 10 RPS
- Spike: 5000 RPS
- Duration: Spike for 30s, then 30s recovery

```rust
let test = LoadTest::new("http://api.example.com")
    .with_spike(10, 5000)
    .with_duration(Duration::from_secs(60));

let result = test.run().await?;

// Spike tests often exceed SLO temporarily
if result.latencies.p99_ms > 100.0 {
    println!("Spike caused latency increase: {:.2}ms", result.latencies.p99_ms);
}
```

**Expected behavior**:
- Latency spikes immediately
- Throughput may be limited by available resources
- Quick recovery after spike ends
- No cascading failures

#### Soak Test
Sustained load for extended period.

**Use case**: Memory leak detection, connection limit verification, 24-hour stability

**Profile**:
- Load: 500 RPS sustained
- Duration: 8+ hours
- Monitor: Memory, connections, error rate

```rust
let test = LoadTest::new("http://api.example.com")
    .with_soak(500)
    .with_duration(Duration::from_secs(28800)); // 8 hours

let result = test.run().await?;

// After 8 hours, verify system is stable
assert!(result.memory_usage_mb < 8000.0); // No memory leaks
assert!(result.error_rate < 0.01); // <1% errors
```

**Expected behavior**:
- Stable latency throughout
- Memory usage constant (no leaks)
- Connection counts stable
- Error rate remains low

#### Stress Test
Push beyond capacity to find breaking point.

**Use case**: Determine actual limits, scaling behavior

**Profile**:
- Start: 10 RPS
- Peak: 10,000+ RPS (beyond expected capacity)
- Duration: Until system breaks or stabilizes

```rust
let test = LoadTest::new("http://api.example.com")
    .with_stress(20000)
    .with_duration(Duration::from_secs(300));

let result = test.run().await?;

println!("System breaking point: {} RPS",
    result.throughput_rps as u64);
println!("Error rate at capacity: {:.2}%",
    result.error_rate * 100.0);
```

**Expected behavior**:
- Error rate increases as capacity reached
- Latency tail (p99, max) increases significantly
- System should recover after load removed
- Graceful degradation preferred over crash

### Running Load Tests

#### Basic Load Test

```rust
#[tokio::test]
async fn test_api_load_capacity() {
    let test = LoadTest::new("http://api.example.com")
        .with_ramp_up(10, 100)
        .with_duration(Duration::from_secs(60));

    let result = test.run().await.expect("load test should succeed");

    // Verify SLOs
    assert!(result.passed_slo(), "System must meet SLOs under load");
    assert!(result.latencies.p99_ms < 100.0); // < 100ms p99
    assert!(result.error_rate < 0.01); // < 1% error rate
}
```

#### Load Test with Read/Write Mix

```rust
let test = LoadTest::new("http://api.example.com")
    .with_ramp_up(100, 1000)
    .with_read_write_ratio(0.8, 0.2) // 80% reads, 20% writes
    .with_duration(Duration::from_secs(300));

let result = test.run().await?;
```

#### Load Test in CI/CD

```bash
# Run load test in CI pipeline
ggen load-test \
    --target http://staging.example.com \
    --type ramp-up \
    --initial-rps 10 \
    --peak-rps 100 \
    --duration 300 \
    --fail-if-p99-exceeds 100 \
    --fail-if-error-rate-exceeds 0.01
```

### SLO Definition

Standard SLOs (Service Level Objectives):

- **Throughput**: At least 100 RPS at p99 latency < 100ms
- **Latency p50**: < 50ms (median response time)
- **Latency p95**: < 75ms (95th percentile)
- **Latency p99**: < 100ms (99th percentile)
- **Error rate**: < 1% (99% success rate)
- **Availability**: 99.9% uptime (allows 43 minutes downtime/month)

Adjust based on your service:
- User-facing APIs: Stricter SLOs
- Batch jobs: Relaxed latency SLOs
- Critical services: Higher availability targets

### Interpreting Load Test Results

#### Throughput
- Measure: Requests per second (RPS)
- Good: Meets or exceeds capacity planning target
- Bad: Significantly lower than expected

#### Latency Percentiles
- **P50 (Median)**: Half of requests faster, half slower
  - User perceives: Typical experience
  - SLO: Usually 50-75ms

- **P95**: 95% of requests are faster
  - User perceives: Most of the time
  - SLO: Usually 75-100ms

- **P99**: 99% of requests are faster
  - User perceives: Occasionally slow
  - SLO: Usually 100-200ms

- **Max**: Slowest request
  - User perceives: Worst-case experience
  - Watch for outliers

#### Error Rate
- Measure: Percentage of failed requests
- Good: < 0.1% (1 in 1000 requests)
- Bad: > 1% (1 in 100 requests)

#### Resource Utilization
- **CPU**: Should be < 70% at peak load
- **Memory**: Should not increase over time (watch for leaks)
- **Network I/O**: Should not be saturated

### Load Testing Best Practices

1. **Test early and often**: Start load testing before release
2. **Progressive load**: Start small, increase gradually
3. **Mix workloads**: Combine reads and writes realistically
4. **Monitor everything**: CPU, memory, network, disk, database
5. **Test with production-like data**: Use realistic dataset sizes
6. **Identify bottlenecks**: Find limiting resource
7. **Set SLOs**: Agree on acceptable performance
8. **Run multiple tests**: Spike, soak, stress (not just ramp-up)
9. **Automate testing**: Part of CI/CD pipeline
10. **Baseline and track**: Measure regression over time

---

## Integrating Tests Together

### Full System Testing

Combine all four testing approaches for comprehensive validation:

```rust
#[tokio::test]
async fn test_system_production_readiness() {
    // 1. Chaos Engineering: Test failure recovery
    let chaos = ChaosExperiment::pod_kill("prod", "api", 2);
    let chaos_metrics = chaos.execute().await?;
    assert!(chaos_metrics.recovered_successfully);

    // 2. Property-Based Testing: Verify invariants
    let invariant = StateInvariant::latency_bounded();
    let inv_result = invariant.verify().await?;
    assert!(inv_result.passed);

    // 3. Compliance Validation: Check regulatory requirements
    let compliance = ComplianceFramework::fisma();
    let comp_result = compliance.audit().await?;
    assert!(comp_result.is_compliant());

    // 4. Load Testing: Verify capacity under load
    let load_test = LoadTest::new("http://api.example.com")
        .with_ramp_up(10, 100)
        .with_duration(Duration::from_secs(300));
    let load_result = load_test.run().await?;
    assert!(load_result.passed_slo());
}
```

### Testing Pipeline

```
Unit Tests (fast)
    ↓
Property-Based Tests (thorough)
    ↓
Load Testing (capacity)
    ↓
Chaos Engineering (recovery)
    ↓
Compliance Validation (regulatory)
    ↓
Production Deploy ✓
```

### Continuous Testing

Set up automated tests to run on schedules:

```bash
# Unit and property tests: Every commit
git push → cargo test

# Load testing: Daily at 2am (off-peak)
0 2 * * * /opt/ggen/bin/load-test --target staging

# Chaos engineering: Weekly on Friday (pre-weekend)
0 2 * * 5 /opt/ggen/bin/chaos-test --experiment pod-kill

# Compliance: Monthly
0 2 1 * * /opt/ggen/bin/compliance-audit --all-frameworks
```

---

## Troubleshooting

### Chaos Experiment Fails

**Problem**: Experiment execution returns error

**Solutions**:
1. Check cluster credentials
2. Verify target service exists
3. Check service has sufficient replicas (at least 2 for pod kill)
4. Review logs for specific error

### Property Test Reports Violations

**Problem**: Invariant violation detected

**Solutions**:
1. Review the minimal failing example
2. Add logging to understand execution sequence
3. Create unit test reproducing the failure
4. Fix root cause, not just symptom
5. Re-run to verify fix

### Load Test Shows SLO Violation

**Problem**: Latency exceeds SLO under load

**Solutions**:
1. Profile application to find bottleneck
2. Check database query performance
3. Monitor resource utilization during test
4. Consider caching, batching, or pagination
5. Increase resources (CPU, memory, database)
6. Run under different load profiles to find threshold

### Compliance Audit Fails

**Problem**: Control non-compliant

**Solutions**:
1. Review violation description
2. Follow remediation steps provided
3. Implement control
4. Gather evidence (logs, configurations)
5. Re-run audit to verify
6. Document for audit trail

---

## Performance Characteristics

### Execution Times (Approximate)

| Test Type | Duration | Frequency |
|-----------|----------|-----------|
| Property-based (100 iter) | 1-2 seconds | Every commit |
| Chaos (pod kill) | 2-5 minutes | Weekly |
| Load test (ramp-up) | 5-10 minutes | Daily |
| Compliance audit | 30-60 seconds | Monthly |
| Full system test | 15-30 minutes | Pre-release |

### Resource Requirements

| Test Type | CPU | Memory | Network |
|-----------|-----|--------|---------|
| Property-based | Low | 128 MB | Minimal |
| Chaos | Medium | 512 MB | High |
| Load test | Medium | 256 MB | High |
| Compliance | Low | 64 MB | Minimal |

---

## API Reference

### Chaos Engineering

```rust
// Create experiments
ChaosExperiment::pod_kill(cluster, service, count)
ChaosExperiment::network_partition(cluster, service)
ChaosExperiment::cpu_throttling(cluster, service)
ChaosExperiment::memory_pressure(cluster, service)
ChaosExperiment::disk_exhaustion(cluster, service)
ChaosExperiment::clock_skew(cluster, service)
ChaosExperiment::cascading_failure(cluster, services)

// Execute and collect metrics
metrics = experiment.execute().await?;
metrics.recovery_time_ms
metrics.error_rate_during_failure
metrics.latency_p99_ms
metrics.downstream_failures
```

### Property-Based Testing

```rust
// Create invariants
StateInvariant::circuit_breaker_never_stuck_open()
StateInvariant::queue_message_preservation()
StateInvariant::firestore_atomicity()
StateInvariant::cache_consistency()
StateInvariant::latency_bounded()
StateInvariant::error_rate_limited()

// Verify and inspect
result = invariant.verify().await?;
result.passed
result.violations
result.code_paths_exercised
```

### Compliance Validation

```rust
// Create frameworks
ComplianceFramework::fisma()
ComplianceFramework::fedramp()
ComplianceFramework::soc2()
ComplianceFramework::hipaa()
ComplianceFramework::pcidss()

// Run audits
result = framework.audit().await?;
result.is_compliant()
result.violations
result.remediation_summary()
```

### Load Testing

```rust
// Create tests
LoadTest::new(url)
    .with_ramp_up(initial_rps, peak_rps)
    .with_spike(initial_rps, spike_rps)
    .with_soak(sustained_rps)
    .with_stress(peak_rps)
    .with_duration(duration)
    .with_read_write_ratio(read_pct, write_pct)

// Run and inspect
result = test.run().await?;
result.passed_slo()
result.throughput_rps
result.latencies.p99_ms
result.error_rate
```

---

## Contributing

To add new chaos experiments, invariants, or compliance frameworks:

1. Create test in `/crates/tai-testing/src/<module>.rs`
2. Implement traits (experiment, invariant, etc.)
3. Add unit tests
4. Add integration tests
5. Update documentation
6. Run full test suite: `cargo make test`
7. Submit PR with examples

---

## Support & Resources

- **GitHub**: https://github.com/seanchatmangpt/ggen
- **Issues**: https://github.com/seanchatmangpt/ggen/issues
- **Documentation**: `/docs/tai-testing/`

---

## License

MIT OR Apache-2.0

---

**Last Updated**: January 2026 | **Version**: 0.1.0
