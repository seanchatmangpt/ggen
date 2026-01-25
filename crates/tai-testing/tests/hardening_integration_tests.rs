//! Integration tests for production hardening & resilience testing
//!
//! These tests verify that:
//! - Chaos experiments run correctly and produce valid metrics
//! - Property-based tests detect invariant violations
//! - Compliance audits identify control gaps
//! - Load tests accurately measure system capacity
//! - All test types work together in realistic scenarios

use std::time::Duration;
use tai_testing::{
    ChaosExperiment, ChaosExperimentType, ComplianceFramework, LoadTest, StateInvariant,
    TestingError,
};

#[tokio::test]
async fn test_pod_kill_experiment_execution() {
    // Arrange
    let experiment = ChaosExperiment::pod_kill("prod-cluster", "api-service", 3);

    // Act
    let metrics = experiment.execute().await.expect("should execute");

    // Assert
    assert_eq!(metrics.experiment_type, ChaosExperimentType::PodKill);
    assert!(metrics.recovery_time_ms > 0);
    assert!(metrics.error_rate_during_failure >= 0.0);
    assert!(metrics.error_rate_during_failure <= 1.0);
    assert!(metrics.latency_p99_ms > 0.0);
    assert_eq!(metrics.downstream_failures.len(), 0); // Pod kill shouldn't cause cascading failures
}

#[tokio::test]
async fn test_network_partition_experiment() {
    // Arrange
    let experiment = ChaosExperiment::network_partition("staging", "database-service");

    // Act
    let metrics = experiment.execute().await.expect("should execute");

    // Assert
    assert_eq!(
        metrics.experiment_type,
        ChaosExperimentType::NetworkPartition
    );
    assert!(metrics.error_rate_during_failure > 0.5); // Network partition causes many errors
    assert!(!metrics.downstream_failures.is_empty()); // Network partition cascades
}

#[tokio::test]
async fn test_cpu_throttling_experiment() {
    // Arrange
    let experiment = ChaosExperiment::cpu_throttling("prod", "compute-service");

    // Act
    let metrics = experiment.execute().await.expect("should execute");

    // Assert
    assert_eq!(metrics.experiment_type, ChaosExperimentType::CpuThrottling);
    assert!(metrics.cpu_usage_percent < 30.0); // CPU is throttled
    assert!(metrics.latency_p99_ms > 100.0); // Latency increases under CPU throttling
}

#[tokio::test]
async fn test_cascading_failure_experiment() {
    // Arrange
    let experiment = ChaosExperiment::cascading_failure(
        "prod",
        vec!["auth-service", "payment-service", "inventory-service"],
    );

    // Act
    let metrics = experiment.execute().await.expect("should execute");

    // Assert
    assert_eq!(
        metrics.experiment_type,
        ChaosExperimentType::CascadingFailure
    );
    assert!(metrics.error_rate_during_failure > 0.9); // Cascading failure causes widespread errors
    assert!(!metrics.downstream_failures.is_empty());
    assert!(metrics.recovery_time_ms > 180_000); // Cascading failures take longer to recover
}

#[tokio::test]
async fn test_circuit_breaker_invariant() {
    // Arrange
    let invariant = StateInvariant::circuit_breaker_never_stuck_open().with_iterations(10);

    // Act
    let result = invariant
        .verify()
        .await
        .expect("verification should succeed");

    // Assert
    assert!(result.passed || !result.violations.is_empty());
    assert_eq!(result.iterations, 10);
    assert!(result.test_duration_secs > 0.0);
}

#[tokio::test]
async fn test_queue_message_preservation_invariant() {
    // Arrange
    let invariant = StateInvariant::queue_message_preservation().with_iterations(20);

    // Act
    let result = invariant
        .verify()
        .await
        .expect("verification should succeed");

    // Assert
    assert_eq!(result.iterations, 20);
    if !result.passed {
        assert!(!result.violations.is_empty());
    }
}

#[tokio::test]
async fn test_cache_consistency_invariant() {
    // Arrange
    let invariant = StateInvariant::cache_consistency().with_iterations(15);

    // Act
    let result = invariant
        .verify()
        .await
        .expect("verification should succeed");

    // Assert
    if !result.passed {
        for violation in &result.violations {
            assert!(!violation.description.is_empty());
        }
    }
}

#[tokio::test]
async fn test_latency_bounded_invariant() {
    // Arrange
    let invariant = StateInvariant::latency_bounded().with_iterations(10);

    // Act
    let result = invariant
        .verify()
        .await
        .expect("verification should succeed");

    // Assert
    assert!(!result.code_paths_exercised.is_empty());
}

#[tokio::test]
async fn test_error_rate_limited_invariant() {
    // Arrange
    let invariant = StateInvariant::error_rate_limited().with_iterations(10);

    // Act
    let result = invariant
        .verify()
        .await
        .expect("verification should succeed");

    // Assert
    if !result.passed {
        for violation in &result.violations {
            assert!(violation.description.contains("Error rate"));
        }
    }
}

#[tokio::test]
async fn test_fisma_compliance_audit() {
    // Arrange
    let framework = ComplianceFramework::fisma();

    // Act
    let result = framework.audit().await.expect("audit should succeed");

    // Assert
    assert_eq!(
        result.framework,
        tai_testing::ComplianceFramework::fisma().framework_type()
    );
    assert!(result.total_controls > 0);
    assert_eq!(
        result.total_controls,
        result.compliant_controls
            + result.non_compliant_controls
            + result.in_progress_controls
            + result.not_applicable_controls
    );
}

#[tokio::test]
async fn test_hipaa_compliance_audit() {
    // Arrange
    let framework = ComplianceFramework::hipaa();

    // Act
    let result = framework.audit().await.expect("audit should succeed");

    // Assert
    assert!(result.total_controls > 0);
    let summary = result.remediation_summary();
    assert!(!summary.is_empty());
}

#[tokio::test]
async fn test_pcidss_compliance_audit() {
    // Arrange
    let framework = ComplianceFramework::pcidss();

    // Act
    let result = framework.audit().await.expect("audit should succeed");

    // Assert
    // PCI-DSS requires 12 requirements
    assert!(result.total_controls >= 12);
}

#[tokio::test]
async fn test_ramp_up_load_test() {
    // Arrange
    let load_test = LoadTest::new("http://api.example.com")
        .with_ramp_up(10, 100)
        .with_duration(Duration::from_secs(5));

    // Act
    let result = load_test.run().await.expect("load test should succeed");

    // Assert
    assert!(result.total_requests > 0);
    assert!(result.throughput_rps > 0.0);
    assert!(result.latencies.p99_ms > result.latencies.p95_ms);
    assert!(result.latencies.p95_ms > result.latencies.p50_ms);
}

#[tokio::test]
async fn test_spike_load_test() {
    // Arrange
    let load_test = LoadTest::new("http://api.example.com")
        .with_spike(10, 500)
        .with_duration(Duration::from_secs(5));

    // Act
    let result = load_test.run().await.expect("load test should succeed");

    // Assert
    assert!(result.total_requests > 0);
    assert!(result.latencies.p99_ms > 0.0);
    // Spike test should have higher latency than ramp-up
    assert!(result.latencies.p99_ms > 50.0);
}

#[tokio::test]
async fn test_soak_load_test() {
    // Arrange
    let load_test = LoadTest::new("http://api.example.com")
        .with_soak(100)
        .with_duration(Duration::from_secs(10));

    // Act
    let result = load_test.run().await.expect("load test should succeed");

    // Assert
    assert!(result.total_requests > 0);
    // Soak test should maintain consistent throughput
    assert!(result.peak_rps > 50.0);
}

#[tokio::test]
async fn test_stress_load_test() {
    // Arrange
    let load_test = LoadTest::new("http://api.example.com")
        .with_stress(5000)
        .with_duration(Duration::from_secs(5));

    // Act
    let result = load_test.run().await.expect("load test should succeed");

    // Assert
    assert!(result.total_requests > 0);
    // Stress test causes higher latency
    assert!(result.latencies.p99_ms > 100.0);
}

#[tokio::test]
async fn test_load_test_read_write_ratio() {
    // Arrange
    let load_test = LoadTest::new("http://api.example.com")
        .with_ramp_up(10, 100)
        .with_duration(Duration::from_secs(5))
        .with_read_write_ratio(0.9, 0.1); // 90% reads, 10% writes

    // Act
    let result = load_test.run().await.expect("load test should succeed");

    // Assert
    assert!(result.total_requests > 0);
    // Read-heavy workloads should have lower latency
    assert!(result.latencies.p99_ms < 200.0);
}

#[tokio::test]
async fn test_load_test_slo_validation() {
    // Arrange
    let load_test = LoadTest::new("http://api.example.com")
        .with_ramp_up(10, 100)
        .with_duration(Duration::from_secs(5));

    // Act
    let result = load_test.run().await.expect("load test should succeed");

    // Assert
    // Even if SLO is not met, the test should complete successfully
    let summary = result.summary();
    assert!(summary.contains("Ramp-up"));
    assert!(summary.contains("requests"));
}

#[tokio::test]
async fn test_chaos_and_compliance_together() {
    // Test that chaos experiments and compliance audits can run together
    let exp = ChaosExperiment::pod_kill("prod", "svc", 1);
    let compliance = ComplianceFramework::fisma();

    let (exp_result, compliance_result) = tokio::join!(exp.execute(), compliance.audit());

    assert!(exp_result.is_ok());
    assert!(compliance_result.is_ok());
}

#[tokio::test]
async fn test_property_and_load_testing_together() {
    // Test that property-based tests and load tests can run together
    let invariant = StateInvariant::latency_bounded();
    let load_test = LoadTest::new("http://api.example.com")
        .with_ramp_up(10, 100)
        .with_duration(Duration::from_secs(5));

    let (invariant_result, load_result) = tokio::join!(invariant.verify(), load_test.run());

    assert!(invariant_result.is_ok());
    assert!(load_result.is_ok());
}

#[tokio::test]
async fn test_multiple_chaos_experiments_sequence() {
    // Run multiple experiments in sequence to simulate full system testing
    let experiments = vec![
        ChaosExperiment::pod_kill("prod", "api", 1),
        ChaosExperiment::cpu_throttling("prod", "api"),
        ChaosExperiment::memory_pressure("prod", "api"),
    ];

    for exp in experiments {
        let metrics = exp.execute().await.expect("each experiment should succeed");
        assert!(metrics.recovery_time_ms > 0);
    }
}

#[tokio::test]
async fn test_compliance_frameworks_comprehensive() {
    // Test all compliance frameworks can run
    let frameworks = vec![
        ComplianceFramework::fisma(),
        ComplianceFramework::fedramp(),
        ComplianceFramework::soc2(),
        ComplianceFramework::hipaa(),
        ComplianceFramework::pcidss(),
    ];

    for framework in frameworks {
        let result = framework.audit().await.expect("audit should succeed");
        assert!(result.total_controls > 0);
    }
}

#[tokio::test]
async fn test_all_property_invariants() {
    // Test all property-based invariants
    let invariants = vec![
        StateInvariant::circuit_breaker_never_stuck_open(),
        StateInvariant::queue_message_preservation(),
        StateInvariant::firestore_atomicity(),
        StateInvariant::cache_consistency(),
        StateInvariant::latency_bounded(),
        StateInvariant::error_rate_limited(),
    ];

    for invariant in invariants {
        let result = invariant
            .verify()
            .await
            .expect("verification should succeed");
        assert_eq!(result.iterations, 100);
    }
}

#[tokio::test]
async fn test_all_load_test_types() {
    // Test all load test types
    let tests = vec![
        LoadTest::new("http://api.example.com")
            .with_ramp_up(10, 100)
            .with_duration(Duration::from_secs(5)),
        LoadTest::new("http://api.example.com")
            .with_spike(10, 500)
            .with_duration(Duration::from_secs(5)),
        LoadTest::new("http://api.example.com")
            .with_soak(100)
            .with_duration(Duration::from_secs(5)),
        LoadTest::new("http://api.example.com")
            .with_stress(1000)
            .with_duration(Duration::from_secs(5)),
    ];

    for test in tests {
        let result = test.run().await.expect("load test should succeed");
        assert!(result.total_requests > 0);
    }
}
