//! Quota & SLA Governor Integration Tests
//!
//! Chicago TDD: Tests verify state machine behavior with real state objects.
//! Governor implements 7-state FSM: WithinLimits → Warning → Exceeded → Throttled → CircuitBreaker
//!
//! Uses real collaborators (quota metrics), state verification (not just asserts).
//! Invariant enforcement tested: guards prevent invalid state transitions.

use gcp_erlang_autonomics::marketplace::{
    QuotaSlaGovernor, QuotaSlaState, QuotaSlaEvent, QuotaType, CustomerTier,
};

fn make_governor(tier: CustomerTier) -> QuotaSlaGovernor {
    let mut gov = QuotaSlaGovernor::new("tenant-1".to_string(), tier);
    gov.register_metric("api_requests".to_string(), 1_000_000.0, QuotaType::Soft);
    gov.register_metric("storage_gb".to_string(), 1_000.0, QuotaType::Hard);
    gov.register_metric("concurrent_calls".to_string(), 1_000.0, QuotaType::Hard);
    gov
}

// Test 1: Initial state is WithinLimits
#[tokio::test]
async fn test_integration_initial_state_is_within_limits() {
    // Arrange & Act
    let gov = make_governor(CustomerTier::Professional);

    // Assert
    assert_eq!(gov.current_state(), QuotaSlaState::WithinLimits);
    assert_eq!(gov.tier, CustomerTier::Professional);
    assert_eq!(gov.overage_charges, 0.0);
}

// Test 2: Usage at 50% → stays WithinLimits
#[tokio::test]
async fn test_integration_usage_50_percent_within_limits() {
    // Arrange
    let mut gov = make_governor(CustomerTier::Professional);
    gov.update_usage("api_requests", 500_000.0).unwrap();

    // Act
    let (state, action) = gov.transition(QuotaSlaEvent::UsageUpdated {
        metric: "api_requests".to_string(),
        current: 500_000.0,
        limit: 1_000_000.0,
    }).await.unwrap();

    // Assert
    assert_eq!(state, QuotaSlaState::WithinLimits);
    assert!(action.is_none());
}

// Test 3: Usage at 80% → Warning state
#[tokio::test]
async fn test_integration_usage_80_percent_warning() {
    // Arrange
    let mut gov = make_governor(CustomerTier::Professional);
    gov.update_usage("api_requests", 800_000.0).unwrap();

    // Act
    let (state, action) = gov.transition(QuotaSlaEvent::UsageUpdated {
        metric: "api_requests".to_string(),
        current: 800_000.0,
        limit: 1_000_000.0,
    }).await.unwrap();

    // Assert
    assert_eq!(state, QuotaSlaState::Warning);
    assert!(matches!(action, Some(_)));
}

// Test 4: Soft limit exceeded → Exceeded state
#[tokio::test]
async fn test_integration_soft_limit_exceeded() {
    // Arrange
    let mut gov = make_governor(CustomerTier::Professional);
    gov.update_usage("api_requests", 1_500_000.0).unwrap();

    // Act
    let (state, action) = gov.transition(QuotaSlaEvent::UsageUpdated {
        metric: "api_requests".to_string(),
        current: 1_500_000.0,
        limit: 1_000_000.0,
    }).await.unwrap();

    // Assert
    assert_eq!(state, QuotaSlaState::Exceeded);
    assert!(action.is_some());
}

// Test 5: Hard limit exceeded returns error
#[tokio::test]
async fn test_integration_hard_limit_cannot_exceed() {
    // Arrange
    let mut gov = make_governor(CustomerTier::Professional);

    // Act: Try to exceed hard limit
    let result = gov.transition(QuotaSlaEvent::UsageUpdated {
        metric: "storage_gb".to_string(),
        current: 2_000.0,
        limit: 1_000.0,
    }).await;

    // Assert
    assert!(result.is_err());
}

// Test 6: Full lifecycle: Warning → Exceeded → Throttled
#[tokio::test]
async fn test_integration_full_lifecycle() {
    // Arrange
    let mut gov = make_governor(CustomerTier::Professional);

    // Act: Transition to Warning
    gov.update_usage("api_requests", 800_000.0).unwrap();
    let (state1, _) = gov.transition(QuotaSlaEvent::UsageUpdated {
        metric: "api_requests".to_string(),
        current: 800_000.0,
        limit: 1_000_000.0,
    }).await.unwrap();
    assert_eq!(state1, QuotaSlaState::Warning);

    // Act: Transition to Exceeded
    gov.update_usage("api_requests", 1_500_000.0).unwrap();
    let (state2, _) = gov.transition(QuotaSlaEvent::LimitExceeded {
        metric: "api_requests".to_string(),
    }).await.unwrap();
    assert_eq!(state2, QuotaSlaState::Exceeded);

    // Act: Transition to Throttled
    let (state3, action) = gov.transition(QuotaSlaEvent::HardLimitBreached {
        metric: "api_requests".to_string(),
    }).await.unwrap();
    assert_eq!(state3, QuotaSlaState::Throttled);
    assert!(action.is_some());
}

// Test 7: Upgrade during Warning restores to WithinLimits
#[tokio::test]
async fn test_integration_upgrade_restores_capacity() {
    // Arrange
    let mut gov = make_governor(CustomerTier::Professional);
    gov.update_usage("api_requests", 800_000.0).unwrap();
    gov.transition(QuotaSlaEvent::UsageUpdated {
        metric: "api_requests".to_string(),
        current: 800_000.0,
        limit: 1_000_000.0,
    }).await.unwrap();
    assert_eq!(gov.current_state(), QuotaSlaState::Warning);

    // Act: Upgrade to Enterprise
    let (state, action) = gov.transition(QuotaSlaEvent::CustomerRequestsUpgrade {
        tier: CustomerTier::Enterprise,
    }).await.unwrap();

    // Assert
    assert_eq!(state, QuotaSlaState::WithinLimits);
    assert!(action.is_some());
}

// Test 8: Overage charges accumulate
#[tokio::test]
async fn test_integration_overage_charges_accumulate() {
    // Arrange
    let mut gov = make_governor(CustomerTier::Professional);
    gov.state = QuotaSlaState::Exceeded;
    let initial = gov.overage_charges;

    // Act: Approve overage charge
    let _ = gov.transition(QuotaSlaEvent::OverageChargesApproved {
        amount: 100.0,
    }).await.unwrap();

    // Assert
    assert_eq!(gov.overage_charges, initial + 100.0);
}

// Test 9: Circuit breaker recovery flow
#[tokio::test]
async fn test_integration_circuit_breaker_recovery() {
    // Arrange
    let mut gov = make_governor(CustomerTier::Professional);
    gov.state = QuotaSlaState::CircuitBreaker;

    // Act: Customer pays for recovery
    let (state1, _) = gov.transition(QuotaSlaEvent::CustomerPaysForRecovery {
        amount: 500.0,
    }).await.unwrap();
    assert_eq!(state1, QuotaSlaState::ResetPending);

    // Act: Reset processing complete
    let (state2, _) = gov.transition(QuotaSlaEvent::ResetProcessingComplete).await.unwrap();

    // Assert
    assert_eq!(state2, QuotaSlaState::Restored);
}

// Test 10: Different tiers have different quotas
#[tokio::test]
async fn test_integration_tier_differences() {
    // Arrange
    let starter = CustomerTier::Starter;
    let professional = CustomerTier::Professional;
    let enterprise = CustomerTier::Enterprise;

    // Assert: Uptime targets
    assert!(enterprise.uptime_sla_percent() > professional.uptime_sla_percent());
    assert!(professional.uptime_sla_percent() > starter.uptime_sla_percent());

    // Assert: Monthly request limits
    assert_eq!(enterprise.monthly_requests(), f64::INFINITY);
    assert!(professional.monthly_requests() > starter.monthly_requests());

    // Assert: Concurrent call limits
    assert!(enterprise.concurrent_api_calls() > professional.concurrent_api_calls());
    assert!(professional.concurrent_api_calls() > starter.concurrent_api_calls());
}

// Test 11: Noisy neighbor detection
#[tokio::test]
async fn test_integration_noisy_neighbor_detection() {
    // Arrange
    let mut gov = make_governor(CustomerTier::Professional);

    // Act: Set concurrent calls to 95% utilization
    gov.update_usage("concurrent_calls", 950.0).unwrap();

    // Assert: Governor detects noisy neighbor
    assert!(gov.is_noisy_neighbor());
}

// Test 12: Most constrained metric
#[tokio::test]
async fn test_integration_most_constrained_metric() {
    // Arrange
    let mut gov = make_governor(CustomerTier::Professional);
    gov.update_usage("api_requests", 500_000.0).unwrap(); // 50%
    gov.update_usage("concurrent_calls", 900.0).unwrap(); // 90%
    gov.update_usage("storage_gb", 100.0).unwrap(); // 10%

    // Act
    let (name, _) = gov.most_constrained_metric().unwrap();

    // Assert
    assert_eq!(name, "concurrent_calls");
}

// Test 13: Burst mode activation
#[tokio::test]
async fn test_integration_burst_mode() {
    // Arrange
    let mut gov = make_governor(CustomerTier::Professional);

    // Act: Activate burst
    gov.activate_burst(300);

    // Assert
    assert_eq!(gov.burst_remaining_secs, 300);
    for metric in gov.metrics.values() {
        assert!(metric.burst_active);
    }

    // Act: Deactivate
    gov.deactivate_burst();

    // Assert
    assert_eq!(gov.burst_remaining_secs, 0);
    for metric in gov.metrics.values() {
        assert!(!metric.burst_active);
    }
}

// Test 14: SLA compliance verification
#[tokio::test]
async fn test_integration_sla_compliance() {
    // Arrange
    let gov = make_governor(CustomerTier::Professional);

    // Assert: Default metrics should be compliant
    assert!(gov.is_sla_compliant());

    // Act: Simulate SLA breach
    let mut gov = gov;
    gov.sla.uptime_percent = 99.0; // Below target (99.9%)

    // Assert
    assert!(!gov.is_sla_compliant());
}

// Test 15: SLA credit calculation
#[tokio::test]
async fn test_integration_sla_credit_calculation() {
    // Arrange
    let mut gov = make_governor(CustomerTier::Professional);
    gov.sla.uptime_percent = 99.0; // 0.9% below target

    // Act
    let credit = gov.sla.calculate_credit_percent(CustomerTier::Professional);

    // Assert
    assert!(credit > 0.0);
    assert!(credit <= 10.0); // Max 10% for uptime breach
}

// Test 16: Invalid state transition rejected
#[tokio::test]
async fn test_integration_invalid_transition_error() {
    // Arrange
    let mut gov = make_governor(CustomerTier::Professional);
    gov.state = QuotaSlaState::Restored;

    // Act: Try invalid transition
    let result = gov.transition(QuotaSlaEvent::OverageChargesApproved { amount: 100.0 }).await;

    // Assert
    assert!(result.is_err());
}

// Test 17: Usage drops below threshold transitions back
#[tokio::test]
async fn test_integration_usage_drop_recovery() {
    // Arrange
    let mut gov = make_governor(CustomerTier::Professional);
    gov.state = QuotaSlaState::Warning;

    // Act
    let (state, _) = gov.transition(QuotaSlaEvent::UsageDropsBelowThreshold).await.unwrap();

    // Assert
    assert_eq!(state, QuotaSlaState::WithinLimits);
}

// Test 18: Cascade failure detection
#[tokio::test]
async fn test_integration_cascade_detection() {
    // Arrange
    let mut gov = make_governor(CustomerTier::Professional);
    gov.state = QuotaSlaState::Throttled;

    // Act: Cascade detected
    let (state, action) = gov.transition(QuotaSlaEvent::CascadeDetected).await.unwrap();

    // Assert
    assert_eq!(state, QuotaSlaState::CircuitBreaker);
    assert!(action.is_some());
}

// Test 19: Manual admin override
#[tokio::test]
async fn test_integration_manual_admin_override() {
    // Arrange
    let mut gov = make_governor(CustomerTier::Professional);
    gov.state = QuotaSlaState::CircuitBreaker;

    // Act
    let (state, action) = gov.transition(QuotaSlaEvent::ManualOverrideByAdmin {
        reason: "Customer dispute".to_string(),
    }).await.unwrap();

    // Assert
    assert_eq!(state, QuotaSlaState::ResetPending);
    assert!(action.is_some());
}

// Test 20: Register and update metrics
#[tokio::test]
async fn test_integration_metric_registration() {
    // Arrange
    let mut gov = make_governor(CustomerTier::Professional);

    // Assert: Metrics are registered
    assert!(gov.metrics.contains_key("api_requests"));
    assert!(gov.metrics.contains_key("storage_gb"));
    assert!(gov.metrics.contains_key("concurrent_calls"));

    // Act: Update usage
    gov.update_usage("api_requests", 500_000.0).unwrap();

    // Assert
    let metric = gov.metrics.get("api_requests").unwrap();
    assert_eq!(metric.current, 500_000.0);
    assert_eq!(metric.utilization_percent(), 50.0);
}
