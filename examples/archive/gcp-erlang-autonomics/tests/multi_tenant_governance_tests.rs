//! Comprehensive integration tests for Multi-Tenant Governance Governor
//!
//! Tests the complete FSM lifecycle with realistic multi-tenant scenarios
//! including noisy neighbor detection, fair-share allocation, cascade prevention,
//! and graceful degradation.

use gcp_erlang_autonomics::marketplace::multi_tenant_governance::*;

fn make_metrics(tenant_id: &str, tier: TenantTier, cpu: u32) -> TenantMetrics {
    use chrono::Utc;
    TenantMetrics {
        tenant_id: tenant_id.to_string(),
        tier,
        cpu_usage: cpu,
        memory_usage: 50,
        network_bandwidth: 30,
        disk_io_utilization: 40,
        error_rate: 0.01,
        request_rate: 1000,
        eviction_rate: 0,
        timestamp: Utc::now(),
    }
}

#[tokio::test]
async fn test_complete_healthy_to_recovery_lifecycle() {
    // Arrange: Create governor and simulate normal operation
    let mut governor = MTGovernor::new();
    assert_eq!(governor.current_state(), MTGovernorState::Healthy);

    // Act: Healthy → ResourceContention (noisy neighbor)
    let metrics = vec![
        make_metrics("enterprise", TenantTier::Enterprise, 85),
        make_metrics("professional", TenantTier::Professional, 15),
        make_metrics("starter", TenantTier::Starter, 10),
    ];

    let (state1, action1) = governor
        .transition(MTGovernorEvent::TenantMetricsCollected(metrics))
        .await
        .unwrap();

    // Assert: Should move to ResourceContention
    assert_eq!(state1, MTGovernorState::ResourceContention);
    assert!(action1.is_some());

    // Act: ResourceContention → LoadBalancing
    let (state2, _) = governor
        .transition(MTGovernorEvent::NoisyNeighborDetected {
            culprit_tenant: "enterprise".to_string(),
            metric: "cpu".to_string(),
            value: 85,
        })
        .await
        .unwrap();

    assert_eq!(state2, MTGovernorState::LoadBalancing);
    assert!(governor.throttled_tenants().contains_key("enterprise"));
    assert_eq!(governor.throttled_tenants()["enterprise"], 50);

    // Act: LoadBalancing → Healthy (success)
    let (state3, _) = governor
        .transition(MTGovernorEvent::RebalancingComplete)
        .await
        .unwrap();

    assert_eq!(state3, MTGovernorState::Healthy);
    assert_eq!(governor.consecutive_high_count(), 0);
}

#[tokio::test]
async fn test_cascade_prevention_lifecycle() {
    let mut governor = MTGovernor::new();
    governor._set_state_for_testing(MTGovernorState::CascadePrevention);

    // Simulate cascade detection
    let cascade = CascadeIndicator {
        error_rate_spike: 0.75,
        latency_surge: 3.5,
        circuit_breaker_open: true,
        customer_id: Some("customer-1".to_string()),
    };

    let (state, action) = governor
        .transition(MTGovernorEvent::CascadeDetected(cascade))
        .await
        .unwrap();

    assert_eq!(state, MTGovernorState::EmergencyShutdown);
    assert!(action.is_some());
    assert!(governor.circuit_breaker_open());
}

#[tokio::test]
async fn test_emergency_shutdown_and_recovery() {
    let mut governor = MTGovernor::new();
    governor._set_state_for_testing(MTGovernorState::EmergencyShutdown);

    // Simulate graceful degradation
    let shed = governor.apply_graceful_degradation().unwrap();
    assert!(shed.contains(&"analytics".to_string()));
    assert!(shed.contains(&"caching".to_string()));

    // Transition to recovery
    let (state, _) = governor
        .transition(MTGovernorEvent::ShutdownComplete)
        .await
        .unwrap();

    assert_eq!(state, MTGovernorState::Recovery);

    // Recover
    let (final_state, _) = governor
        .transition(MTGovernorEvent::RecoveryComplete)
        .await
        .unwrap();

    assert_eq!(final_state, MTGovernorState::Healthy);
    assert!(!governor.circuit_breaker_open());
}

#[test]
fn test_fair_share_allocation_three_tier_system() {
    let mut governor = MTGovernor::new();

    // Set up one tenant per tier
    governor._add_tenant_metrics_for_testing(make_metrics("enterprise", TenantTier::Enterprise, 50));
    governor._add_tenant_metrics_for_testing(make_metrics("professional", TenantTier::Professional, 50));
    governor._add_tenant_metrics_for_testing(make_metrics("starter", TenantTier::Starter, 50));

    let quotas = governor.calculate_fair_share_quotas(1000);

    assert_eq!(quotas["enterprise"], 400); // 40%
    assert_eq!(quotas["professional"], 350); // 35%
    assert_eq!(quotas["starter"], 250); // 25%
}

#[test]
fn test_fair_share_allocation_multiple_tenants_per_tier() {
    let mut governor = MTGovernor::new();

    // Two enterprise, two professional, one starter
    for i in 1..=2 {
        governor._add_tenant_metrics_for_testing(make_metrics(&format!("enterprise-{}", i), TenantTier::Enterprise, 50));
        governor._add_tenant_metrics_for_testing(make_metrics(&format!("professional-{}", i), TenantTier::Professional, 50));
    }
    governor._add_tenant_metrics_for_testing(make_metrics("starter", TenantTier::Starter, 50));

    let quotas = governor.calculate_fair_share_quotas(1000);

    // Within tier sharing
    assert_eq!(quotas["enterprise-1"], 200); // 400 / 2
    assert_eq!(quotas["enterprise-2"], 200);
    assert_eq!(quotas["professional-1"], 175); // 350 / 2
    assert_eq!(quotas["professional-2"], 175);
    assert_eq!(quotas["starter"], 250);
}

#[test]
fn test_all_noisy_neighbor_detection_scenarios() {
    // CPU hog scenario
    let metrics_cpu = vec![
        make_metrics("cpu-hog", TenantTier::Enterprise, 82),
        make_metrics("normal-1", TenantTier::Professional, 15),
        make_metrics("normal-2", TenantTier::Starter, 10),
    ];
    let (has_contention, msg) = MTGovernor::detect_noisy_neighbor(&metrics_cpu).unwrap();
    assert!(has_contention);
    assert!(msg.contains("cpu-hog"));

    // Network bandwidth hog
    let mut metrics_net = vec![
        make_metrics("net-hog", TenantTier::Enterprise, 30),
        make_metrics("normal", TenantTier::Professional, 20),
    ];
    metrics_net[0].network_bandwidth = 80;
    let (has_contention, msg) = MTGovernor::detect_noisy_neighbor(&metrics_net).unwrap();
    assert!(has_contention);
    assert!(msg.contains("net-hog"));

    // Disk I/O saturation
    let mut metrics_disk = vec![
        make_metrics("disk-sat", TenantTier::Enterprise, 30),
        make_metrics("normal", TenantTier::Professional, 25),
    ];
    metrics_disk[0].disk_io_utilization = 95;
    let (has_contention, msg) = MTGovernor::detect_noisy_neighbor(&metrics_disk).unwrap();
    assert!(has_contention);
    assert!(msg.contains("disk"));

    // Memory pressure
    let mut metrics_mem = vec![
        make_metrics("mem-pressure", TenantTier::Enterprise, 30),
        make_metrics("normal", TenantTier::Professional, 25),
    ];
    metrics_mem[0].eviction_rate = 60;
    let (has_contention, msg) = MTGovernor::detect_noisy_neighbor(&metrics_mem).unwrap();
    assert!(has_contention);
    assert!(msg.contains("Memory") || msg.contains("eviction"));

    // All healthy
    let metrics_healthy = vec![
        make_metrics("tenant-1", TenantTier::Enterprise, 45),
        make_metrics("tenant-2", TenantTier::Professional, 50),
        make_metrics("tenant-3", TenantTier::Starter, 40),
    ];
    let (has_contention, _) = MTGovernor::detect_noisy_neighbor(&metrics_healthy).unwrap();
    assert!(!has_contention);
}

#[test]
fn test_audit_trail_tracking() {
    let mut governor = MTGovernor::new();

    governor.record_audit_event_public("event1", Some("tenant-1"), Some("action1"), 1);
    governor.record_audit_event_public("event2", None, Some("action2"), 0);
    governor.record_audit_event_public("event3", Some("tenant-2"), None, 2);

    let trail = governor.audit_trail();
    assert_eq!(trail.len(), 3);
    assert_eq!(trail[0].event_type, "event1");
    assert_eq!(trail[0].tenant_id, Some("tenant-1".to_string()));
    assert_eq!(trail[1].event_type, "event2");
    assert_eq!(trail[2].affected_customers, 2);
}

#[tokio::test]
async fn test_load_balancing_strategy_selection() {
    // Healthy state → Geographic
    let gov_healthy = MTGovernor::new();
    assert_eq!(gov_healthy.current_state(), MTGovernorState::Healthy);
    let strategy = gov_healthy.recommend_load_balancing_strategy().unwrap();
    assert!(matches!(strategy, LoadBalancingStrategy::Geographic(..)));

    // Resource contention → Weighted round-robin
    let mut gov_contention = MTGovernor::new();
    let metrics = vec![
        make_metrics("cpu-hog", TenantTier::Enterprise, 85),
        make_metrics("normal", TenantTier::Professional, 15),
    ];
    let _ = gov_contention
        .transition(MTGovernorEvent::TenantMetricsCollected(metrics))
        .await;
    assert_eq!(gov_contention.current_state(), MTGovernorState::ResourceContention);
    let strategy = gov_contention.recommend_load_balancing_strategy().unwrap();
    assert!(matches!(strategy, LoadBalancingStrategy::WeightedRoundRobin));

    // Cascade prevention → Least loaded (test helper)
    let mut gov_cascade = MTGovernor::new();
    gov_cascade._set_state_for_testing(MTGovernorState::CascadePrevention);
    let strategy = gov_cascade.recommend_load_balancing_strategy().unwrap();
    assert!(matches!(strategy, LoadBalancingStrategy::LeastLoadedNode));

    // Recovery → Resource based (test helper)
    let mut gov_recovery = MTGovernor::new();
    gov_recovery._set_state_for_testing(MTGovernorState::Recovery);
    let strategy = gov_recovery.recommend_load_balancing_strategy().unwrap();
    assert!(matches!(strategy, LoadBalancingStrategy::ResourceBased));
}

#[test]
fn test_graceful_degradation_by_state() {
    // Healthy state - no degradation
    let mut gov = MTGovernor::new();
    gov._set_state_for_testing(MTGovernorState::Healthy);
    let shed = gov.apply_graceful_degradation().unwrap();
    assert!(shed.is_empty());

    // Cascade prevention - moderate degradation
    gov._set_state_for_testing(MTGovernorState::CascadePrevention);
    let shed = gov.apply_graceful_degradation().unwrap();
    assert!(shed.len() >= 2);
    assert!(shed.iter().any(|f| f.contains("analytics")));

    // Emergency shutdown - aggressive degradation
    gov._set_state_for_testing(MTGovernorState::EmergencyShutdown);
    let shed = gov.apply_graceful_degradation().unwrap();
    assert!(shed.len() >= 5);
    assert!(shed.iter().any(|f| f.contains("logging")));
    assert!(shed.iter().any(|f| f.contains("caching")));
}

#[test]
fn test_tenant_metrics_validation_comprehensive() {
    // Valid metrics
    let valid = TenantMetrics {
        tenant_id: "tenant-1".to_string(),
        tier: TenantTier::Enterprise,
        cpu_usage: 50,
        memory_usage: 60,
        network_bandwidth: 40,
        disk_io_utilization: 30,
        error_rate: 0.05,
        request_rate: 1000,
        eviction_rate: 5,
        timestamp: chrono::Utc::now(),
    };
    assert!(valid.validate().is_ok());

    // Empty tenant ID
    let mut invalid = valid.clone();
    invalid.tenant_id = String::new();
    assert!(invalid.validate().is_err());

    // CPU out of bounds
    let mut invalid = valid.clone();
    invalid.cpu_usage = 150;
    assert!(invalid.validate().is_err());

    // Error rate out of bounds
    let mut invalid = valid.clone();
    invalid.error_rate = 1.5;
    assert!(invalid.validate().is_err());

    // Overall pressure calculation
    let metrics = make_metrics("test", TenantTier::Enterprise, 50);
    let pressure = metrics.overall_pressure();
    assert!(pressure > 0 && pressure <= 100);
}

#[tokio::test]
async fn test_invalid_state_transitions_rejected() {
    let mut governor = MTGovernor::new();
    governor._set_state_for_testing(MTGovernorState::Healthy);

    // Try to recover while healthy (invalid)
    let result = governor
        .transition(MTGovernorEvent::RecoveryComplete)
        .await;

    assert!(matches!(result, Err(MTGovernorError::InvalidTransition { .. })));

    // Healthy state should not change on invalid event
    assert_eq!(governor.current_state(), MTGovernorState::Healthy);
}

#[tokio::test]
async fn test_contention_timeout_escalation() {
    let mut governor = MTGovernor::new();
    governor._set_state_for_testing(MTGovernorState::ResourceContention);

    // Contention timeout should escalate to load balancing
    let (new_state, action) = governor
        .transition(MTGovernorEvent::Timeout)
        .await
        .unwrap();

    assert_eq!(new_state, MTGovernorState::LoadBalancing);
    assert!(action.is_some());
}

#[tokio::test]
async fn test_load_balancing_failure_cascade_escalation() {
    let mut governor = MTGovernor::new();
    governor._set_state_for_testing(MTGovernorState::LoadBalancing);

    // Load balancing failure should escalate to cascade prevention
    let (new_state, _) = governor
        .transition(MTGovernorEvent::RebalancingFailed(
            "migration timeout".to_string(),
        ))
        .await
        .unwrap();

    assert_eq!(new_state, MTGovernorState::CascadePrevention);
}
