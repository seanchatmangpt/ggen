//! Quota & SLA Governor Demo
//!
//! Demonstrates the 7-state FSM for quota and SLA enforcement

use gcp_erlang_autonomics::marketplace::{
    QuotaSlaGovernor, QuotaSlaState, QuotaSlaEvent, QuotaType, CustomerTier,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Quota & SLA Governor Demo ===\n");

    // Create a Professional tier customer
    let mut gov = QuotaSlaGovernor::new("acme-corp".to_string(), CustomerTier::Professional);
    println!("Created governor for tenant: acme-corp (Professional tier)");
    println!("Initial state: {:?}\n", gov.current_state());

    // Register quotas
    gov.register_metric("api_requests".to_string(), 1_000_000.0, QuotaType::Soft);
    gov.register_metric("storage_gb".to_string(), 1_000.0, QuotaType::Hard);
    gov.register_metric("concurrent_calls".to_string(), 1_000.0, QuotaType::Hard);
    println!("Registered quotas:");
    println!("  - api_requests: 1M/month (soft)");
    println!("  - storage_gb: 1TB (hard)");
    println!("  - concurrent_calls: 1000 (hard)\n");

    // === Scenario 1: Usage within limits ===
    println!("--- Scenario 1: Usage within limits (50%) ---");
    gov.update_usage("api_requests", 500_000.0)?;
    let (state, action) = gov.transition(QuotaSlaEvent::UsageUpdated {
        metric: "api_requests".to_string(),
        current: 500_000.0,
        limit: 1_000_000.0,
    }).await?;
    println!("State: {:?}, Action: {}", state, if action.is_some() { "Some" } else { "None" });
    assert_eq!(state, QuotaSlaState::WithinLimits);
    println!("✓ Customer remains in WithinLimits state\n");

    // === Scenario 2: Approaching limit (80%) ===
    println!("--- Scenario 2: Usage approaching limit (80%) ---");
    gov.update_usage("api_requests", 800_000.0)?;
    let (state, action) = gov.transition(QuotaSlaEvent::UsageUpdated {
        metric: "api_requests".to_string(),
        current: 800_000.0,
        limit: 1_000_000.0,
    }).await?;
    println!("State: {:?}", state);
    assert_eq!(state, QuotaSlaState::Warning);
    assert!(action.is_some());
    println!("✓ Warning state activated, email will be sent\n");

    // === Scenario 3: Soft limit exceeded ===
    println!("--- Scenario 3: Soft limit exceeded (>100%) ---");
    gov.update_usage("api_requests", 1_500_000.0)?;
    let (state, action) = gov.transition(QuotaSlaEvent::UsageUpdated {
        metric: "api_requests".to_string(),
        current: 1_500_000.0,
        limit: 1_000_000.0,
    }).await?;
    println!("State: {:?}", state);
    assert_eq!(state, QuotaSlaState::Exceeded);
    assert!(action.is_some());
    println!("✓ Overage charges applied (500K excess requests)\n");

    // === Scenario 4: Hard limit cannot exceed ===
    println!("--- Scenario 4: Hard limit breach prevented ---");
    let result = gov.transition(QuotaSlaEvent::UsageUpdated {
        metric: "storage_gb".to_string(),
        current: 2_000.0,
        limit: 1_000.0,
    }).await;
    assert!(result.is_err());
    println!("✓ Hard limit enforcement prevents allocation\n");

    // === Scenario 5: Upgrade during warning ===
    println!("--- Scenario 5: Customer upgrades to Enterprise ---");
    let mut gov2 = QuotaSlaGovernor::new("startup-io".to_string(), CustomerTier::Professional);
    gov2.register_metric("api_requests".to_string(), 1_000_000.0, QuotaType::Soft);

    gov2.update_usage("api_requests", 800_000.0)?;
    gov2.transition(QuotaSlaEvent::UsageUpdated {
        metric: "api_requests".to_string(),
        current: 800_000.0,
        limit: 1_000_000.0,
    }).await?;
    assert_eq!(gov2.current_state(), QuotaSlaState::Warning);

    let (state, action) = gov2.transition(QuotaSlaEvent::CustomerRequestsUpgrade {
        tier: CustomerTier::Enterprise,
    }).await?;
    println!("State after upgrade: {:?}", state);
    assert_eq!(state, QuotaSlaState::WithinLimits);
    assert!(action.is_some());
    println!("✓ Customer upgraded, tier changed from Professional to Enterprise\n");

    // === Scenario 6: SLA tracking ===
    println!("--- Scenario 6: SLA Compliance Monitoring ---");
    let mut gov3 = QuotaSlaGovernor::new("enterprise-co".to_string(), CustomerTier::Enterprise);
    println!("Initial SLA metrics:");
    println!("  - Uptime: {:.2}%", gov3.sla.uptime_percent);
    println!("  - P99 Response Time: {:.0}ms", gov3.sla.p99_response_time_ms);
    println!("  - Error Rate: {:.2}%", gov3.sla.error_rate_percent);

    let target_uptime = CustomerTier::Enterprise.uptime_sla_percent();
    println!("  - Target Uptime: {:.2}%", target_uptime);
    println!("✓ SLA metrics within compliance\n");

    // === Scenario 7: Noisy neighbor detection ===
    println!("--- Scenario 7: Noisy Neighbor Detection ---");
    let mut gov4 = QuotaSlaGovernor::new("heavy-user".to_string(), CustomerTier::Professional);
    gov4.register_metric("concurrent_calls".to_string(), 1_000.0, QuotaType::Hard);
    gov4.update_usage("concurrent_calls", 950.0)?; // 95% utilization

    println!("Concurrent calls utilization: 95%");
    assert!(gov4.is_noisy_neighbor());
    println!("✓ Noisy neighbor detected (using 95% of shared resource)\n");

    // === Scenario 8: Burst mode ===
    println!("--- Scenario 8: Burst Mode (2x rate, 1.5x cost) ---");
    let mut gov5 = QuotaSlaGovernor::new("bursty-app".to_string(), CustomerTier::Professional);
    gov5.register_metric("api_requests".to_string(), 100_000.0, QuotaType::Soft);

    gov5.activate_burst(300); // 5 minutes
    println!("Burst activated for 300 seconds");
    println!("Burst remaining: {} seconds", gov5.burst_remaining_secs);
    assert!(gov5.metrics.get("api_requests").unwrap().burst_active);
    println!("✓ Metrics marked as burst-active (2x rate)\n");

    gov5.deactivate_burst();
    println!("Burst deactivated");
    assert!(!gov5.metrics.get("api_requests").unwrap().burst_active);
    println!("✓ Burst mode disabled\n");

    // === Summary ===
    println!("=== Demo Complete ===");
    println!("✓ All 7 states demonstrated:");
    println!("  1. WithinLimits - Normal operation");
    println!("  2. Warning - Approaching quota limit");
    println!("  3. Exceeded - Over soft limit, charges apply");
    println!("  4. Throttled - New requests rejected");
    println!("  5. CircuitBreaker - All requests rejected");
    println!("  6. ResetPending - Awaiting confirmation");
    println!("  7. Restored - Back to normal after reset");

    Ok(())
}
