//! Integration tests for resilience patterns
//!
//! These tests verify end-to-end behavior of circuit breakers, traffic management,
//! and outlier detection in realistic scenarios.

use std::time::Duration;
use tai_resilience::{
    circuit_breaker_v2::{CircuitBreaker, CircuitBreakerConfig, CircuitBreakerState},
    outlier_detection::{OutlierDetection, OutlierDetectionConfig},
    traffic_management::{
        ABTestSplit, BlueGreenDeployment, CanaryDeployment, DeploymentStatus, Environment,
        TrafficMirror, TrafficSplit,
    },
};

/// Test circuit breaker with multiple failures and recovery
#[tokio::test]
async fn test_circuit_breaker_state_transitions() {
    let config = CircuitBreakerConfig {
        name: "payment-service".to_string(),
        failure_threshold: 3,
        success_threshold: 2,
        timeout_duration: Duration::from_secs(1),
        half_open_max_requests: 3,
        ..Default::default()
    };

    let mut cb = CircuitBreaker::new(config).unwrap();

    // Initial state should be Closed
    assert_eq!(cb.get_state().await, CircuitBreakerState::Closed);

    // Trigger failures to open circuit
    for i in 0..3 {
        let result = cb
            .execute(|| async { Err::<(), String>("Connection failed".to_string()) })
            .await;
        assert!(result.is_err());
    }

    // Circuit should be Open now
    assert_eq!(cb.get_state().await, CircuitBreakerState::Open);

    // Requests should be rejected
    let result = cb
        .execute(|| async { Ok::<(), String>("ok".to_string()) })
        .await;
    assert!(result.is_err());

    let metrics = cb.get_metrics().await;
    assert!(metrics.rejected_requests > 0);
}

/// Test circuit breaker recovery path
#[tokio::test]
async fn test_circuit_breaker_recovery() {
    let config = CircuitBreakerConfig {
        name: "auth-service".to_string(),
        failure_threshold: 2,
        success_threshold: 1,
        timeout_duration: Duration::from_millis(100),
        enable_slow_start: false,
        ..Default::default()
    };

    let mut cb = CircuitBreaker::new(config).unwrap();

    // Trigger open
    for _ in 0..2 {
        let _ = cb
            .execute(|| async { Err::<(), String>("failed".to_string()) })
            .await;
    }

    assert_eq!(cb.get_state().await, CircuitBreakerState::Open);

    // Wait for timeout
    tokio::time::sleep(Duration::from_millis(150)).await;

    // Should transition to HalfOpen when attempting to check transition
    cb.execute(|| async { Ok::<(), String>("ok".to_string()) })
        .await
        .ok();

    let metrics = cb.get_metrics().await;
    assert_eq!(metrics.state_transitions, 2); // Open transition + HalfOpen transition
}

/// Test canary deployment stages
#[tokio::test]
async fn test_canary_deployment_stages() {
    let mut canary = CanaryDeployment::new("web-app", "1.0.0", "1.1.0");
    canary = canary.with_stages(vec![10, 50, 100]);

    assert_eq!(canary.status, DeploymentStatus::Paused);

    // Start canary with quick stages for testing
    let result = canary
        .start(vec![10, 50, 100], Duration::from_millis(50))
        .await;

    assert!(result.is_ok());
    assert_eq!(canary.status, DeploymentStatus::Completed);
    assert_eq!(canary.traffic_percentage, 100);
    assert_eq!(canary.current_stage, 2); // Final stage index
}

/// Test canary deployment progress tracking
#[tokio::test]
async fn test_canary_progress() {
    let mut canary = CanaryDeployment::new("api-gateway", "2.0.0", "2.1.0");
    canary = canary.with_stages(vec![10, 50, 100]);

    let initial_progress = canary.get_progress();
    assert_eq!(initial_progress, 0.0);

    // Advance stages
    let _ = canary.advance_stage().await;
    let progress_1 = canary.get_progress();
    assert!(progress_1 > initial_progress);

    let _ = canary.advance_stage().await;
    let progress_2 = canary.get_progress();
    assert!(progress_2 > progress_1);
}

/// Test blue-green deployment switching
#[tokio::test]
async fn test_blue_green_deployment() {
    let mut bg = BlueGreenDeployment::new("payment-service", "3.0.0", "3.1.0");

    assert_eq!(bg.active_environment, Environment::Blue);

    // Deploy to green
    let result = bg.deploy_and_switch().await;
    assert!(result.is_ok());
    assert_eq!(bg.active_environment, Environment::Green);
    assert_eq!(bg.status, DeploymentStatus::Completed);
}

/// Test blue-green deployment rollback
#[tokio::test]
async fn test_blue_green_rollback() {
    let mut bg = BlueGreenDeployment::new("notification-service", "1.5.0", "1.6.0");

    // Switch to green
    let _ = bg.deploy_and_switch().await;
    assert_eq!(bg.active_environment, Environment::Green);

    // Rollback to blue
    let result = bg.rollback().await;
    assert!(result.is_ok());
    assert_eq!(bg.active_environment, Environment::Blue);
    assert_eq!(bg.status, DeploymentStatus::RolledBack);
}

/// Test traffic mirroring
#[tokio::test]
async fn test_traffic_mirroring() {
    let mut mirror = TrafficMirror::new("frontend", "production", "staging");

    assert!(!mirror.is_active);

    mirror.enable();
    assert!(mirror.is_active);

    // Set mirror percentage
    let result = mirror.set_mirror_percentage(20);
    assert!(result.is_ok());
    assert_eq!(mirror.mirror_percentage, 20);

    // Test mirroring decision
    let should_mirror = mirror.should_mirror(5); // 5 < 20
    assert!(should_mirror);

    let should_not_mirror = mirror.should_mirror(50); // 50 >= 20
    assert!(!should_not_mirror);
}

/// Test traffic mirroring percentage validation
#[tokio::test]
async fn test_traffic_mirroring_validation() {
    let mut mirror = TrafficMirror::new("api", "primary", "canary");

    let result = mirror.set_mirror_percentage(150);
    assert!(result.is_err());
}

/// Test A/B testing
#[tokio::test]
async fn test_ab_testing() {
    let mut ab_test = ABTestSplit::new("checkout", "algorithm-v1", "algorithm-v2");

    // Start test with 60% to variant A
    let result = ab_test.start(60);
    assert!(result.is_ok());
    assert!(ab_test.is_active);

    // Test routing
    let variant = ab_test.route_request(30); // 30 < 60
    assert_eq!(variant, "algorithm-v1");

    let variant = ab_test.route_request(80); // 80 >= 60
    assert_eq!(variant, "algorithm-v2");
}

/// Test A/B test metrics recording
#[tokio::test]
async fn test_ab_test_metrics() {
    let mut ab_test = ABTestSplit::new("search", "es-v8", "es-v9");
    ab_test.start(50).unwrap();

    // Record metrics for variant A
    ab_test.record_metric_a("latency", 100.0);
    ab_test.record_metric_a("latency", 120.0);

    // Record metrics for variant B
    ab_test.record_metric_b("latency", 200.0);
    ab_test.record_metric_b("latency", 220.0);

    // Variant A should be winner (lower latency)
    let winner = ab_test.get_winner();
    assert_eq!(winner, Some(&"es-v8".to_string()));
}

/// Test traffic split validation
#[tokio::test]
async fn test_traffic_split_validation() {
    let result = TrafficSplit::new("primary", "canary", 50, 60);
    assert!(result.is_err()); // Weights don't sum to 100

    let result = TrafficSplit::new("primary", "canary", 70, 30);
    assert!(result.is_ok());
}

/// Test traffic split routing consistency
#[tokio::test]
async fn test_traffic_split_routing_consistency() {
    let split = TrafficSplit::new("primary", "canary", 80, 20).unwrap();

    let mut primary_count = 0;
    let mut canary_count = 0;

    // Simulate 1000 requests
    for i in 0..1000 {
        let route = split.route_request(i);
        if route == "primary" {
            primary_count += 1;
        } else {
            canary_count += 1;
        }
    }

    // Should be approximately 80/20 split
    let primary_percentage = (primary_count as f32 / 1000.0) * 100.0;
    let canary_percentage = (canary_count as f32 / 1000.0) * 100.0;

    // Allow 5% deviation
    assert!((primary_percentage - 80.0).abs() <= 5.0);
    assert!((canary_percentage - 20.0).abs() <= 5.0);
}

/// Test outlier detection identifies unhealthy instances
#[tokio::test]
async fn test_outlier_detection_unhealthy_instance() {
    let config = OutlierDetectionConfig {
        service_name: "db-service".to_string(),
        min_request_volume: 10,
        error_rate_threshold: 0.1, // 10% error rate threshold
        ..Default::default()
    };

    let mut od = OutlierDetection::new(config);

    // Record normal requests for instance-1
    for _ in 0..90 {
        od.record_request("instance-1", true, 100.0);
    }

    // Record failing requests for instance-2 (20% error rate)
    for _ in 0..8 {
        od.record_request("instance-2", true, 100.0);
    }
    for _ in 0..2 {
        od.record_request("instance-2", false, 100.0);
    }

    // Run analysis
    od.analyze().await.unwrap();

    let ejected = od.get_ejected_instances();
    assert!(ejected.contains(&"instance-2".to_string()));
}

/// Test outlier detection respects max ejection percentage
#[tokio::test]
async fn test_outlier_detection_max_ejection_percentage() {
    let config = OutlierDetectionConfig {
        service_name: "cache".to_string(),
        min_request_volume: 10,
        error_rate_threshold: 0.5,   // High threshold
        max_ejection_percentage: 25, // Max 25% of instances
        consecutive_errors_threshold: 1,
        ..Default::default()
    };

    let mut od = OutlierDetection::new(config);

    // Record failing requests for 4 instances (high error rate)
    for instance in &["instance-1", "instance-2", "instance-3", "instance-4"] {
        for _ in 0..15 {
            od.record_request(*instance, false, 100.0);
        }
    }

    // Run analysis
    od.analyze().await.unwrap();

    let ejected = od.get_ejected_instances();
    // Should eject at most 1 instance (25% of 4)
    assert!(ejected.len() <= 1);
}

/// Test outlier detection metrics
#[tokio::test]
async fn test_outlier_detection_metrics() {
    let config = OutlierDetectionConfig {
        service_name: "api-service".to_string(),
        ..Default::default()
    };

    let mut od = OutlierDetection::new(config);

    // Record requests
    od.record_request("instance-1", true, 100.0);
    od.record_request("instance-1", false, 200.0);
    od.record_request("instance-2", true, 150.0);

    let metrics = od.get_metrics();
    assert_eq!(metrics.instances.len(), 2);

    let instance_1 = &metrics.instances["instance-1"];
    assert_eq!(instance_1.total_requests, 2);
    assert_eq!(instance_1.successful_requests, 1);
    assert_eq!(instance_1.failed_requests, 1);
}

/// Test reset of instance metrics
#[test]
fn test_outlier_detection_reset() {
    let config = OutlierDetectionConfig::default();
    let mut od = OutlierDetection::new(config);

    // Record requests
    od.record_request("instance-1", true, 100.0);
    od.record_request("instance-1", false, 200.0);

    let metrics_before = od.get_instance_metrics("instance-1").unwrap();
    assert_eq!(metrics_before.total_requests, 2);

    // Reset
    od.reset_instance("instance-1");

    let metrics_after = od.get_instance_metrics("instance-1").unwrap();
    assert_eq!(metrics_after.total_requests, 0);
}

/// Test multiple circuit breakers independent operation
#[tokio::test]
async fn test_multiple_circuit_breakers() {
    let config1 = CircuitBreakerConfig {
        name: "service-1".to_string(),
        failure_threshold: 2,
        ..Default::default()
    };

    let config2 = CircuitBreakerConfig {
        name: "service-2".to_string(),
        failure_threshold: 2,
        ..Default::default()
    };

    let mut cb1 = CircuitBreaker::new(config1).unwrap();
    let mut cb2 = CircuitBreaker::new(config2).unwrap();

    // Trigger failures on cb1 only
    for _ in 0..2 {
        let _ = cb1
            .execute(|| async { Err::<(), String>("failed".to_string()) })
            .await;
    }

    assert_eq!(cb1.get_state().await, CircuitBreakerState::Open);
    assert_eq!(cb2.get_state().await, CircuitBreakerState::Closed);
}

/// Test integration of multiple patterns together
#[tokio::test]
async fn test_integrated_resilience_patterns() {
    // Circuit breaker protects the service
    let cb_config = CircuitBreakerConfig {
        name: "payment-api".to_string(),
        ..Default::default()
    };
    let _cb = CircuitBreaker::new(cb_config).unwrap();

    // Canary deployment manages rollout
    let mut canary = CanaryDeployment::new("payment-api", "1.0.0", "1.1.0");
    canary = canary.with_stages(vec![10, 50, 100]);

    // Outlier detection manages instances
    let od_config = OutlierDetectionConfig {
        service_name: "payment-api".to_string(),
        ..Default::default()
    };
    let mut od = OutlierDetection::new(od_config);

    // Record some metrics
    od.record_request("replica-1", true, 100.0);
    od.record_request("replica-2", true, 110.0);

    // Simulate canary stages
    canary
        .start(vec![10, 50, 100], Duration::from_millis(10))
        .await
        .ok();

    assert_eq!(canary.status, DeploymentStatus::Completed);
    assert_eq!(od.get_healthy_instances().len(), 2);
}
