//! Subscription Lifecycle Governor Example
//!
//! This example demonstrates a complete SaaS subscription lifecycle:
//! 1. Trial period (14 days free)
//! 2. Purchase conversion
//! 3. Active subscription with usage tracking
//! 4. Upgrade with proration
//! 5. Renewal with payment
//! 6. Cancellation with reactivation window
//! 7. Compliance archival
//!
//! The FSM implements gen_statem patterns for fault-tolerant, state-driven
//! business logic that guarantees state consistency through type safety.

use gcp_erlang_autonomics::marketplace::{
    SubscriptionGovernor, SubscriptionEvent, FeatureTier, BillingCycle, AccountType,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing for audit logging
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("╔═══════════════════════════════════════════════════════════╗");
    println!("║  Subscription Lifecycle Governor - Complete Workflow      ║");
    println!("╚═══════════════════════════════════════════════════════════╝\n");

    let mut governor = SubscriptionGovernor::new();

    // ========================================================================
    // Phase 1: Trial Signup
    // ========================================================================
    println!("📝 PHASE 1: Trial Signup");
    println!("─────────────────────────────────────────────────────────────");

    let subscription = governor
        .create_trial("customer-enterprise-001".to_string(), AccountType::Individual)
        .await?;

    println!("✓ Trial created: {}", subscription.id);
    println!("  - State: {:?}", subscription.state);
    println!("  - Tier: {:?}", subscription.current_tier);
    println!("  - Trial ends: {:?}\n", subscription.trial_ends_at);

    let subscription_id = subscription.id.clone();

    // ========================================================================
    // Phase 2: Trial Expiration Soon Signal
    // ========================================================================
    println!("📧 PHASE 2: Trial Expiration Soon (7 days before)");
    println!("─────────────────────────────────────────────────────────────");

    let (state, action) = governor
        .transition(
            &subscription_id,
            SubscriptionEvent::TrialEndingSoon,
        )
        .await?;

    println!("✓ Trial ending soon signal received");
    println!("  - New state: {:?}", state);
    println!("  - Action: {:?}\n", action);

    // ========================================================================
    // Phase 3: Customer Purchases Subscription
    // ========================================================================
    println!("💳 PHASE 3: Customer Purchases Subscription");
    println!("─────────────────────────────────────────────────────────────");

    let (state, action) = governor
        .transition(
            &subscription_id,
            SubscriptionEvent::CustomerPurchases {
                tier: FeatureTier::Professional,
                cycle: BillingCycle::Annual,
            },
        )
        .await?;

    println!("✓ Purchase processed");
    println!("  - New state: {:?}", state);
    println!("  - Tier: Professional (upgraded from Free)");
    println!("  - Billing: Annual with 10% discount");
    println!("  - Action: {:?}\n", action);

    let subscription = governor.get_subscription(&subscription_id).unwrap();
    println!("  - Current period: {} → {}",
        subscription.current_period_start.format("%Y-%m-%d"),
        subscription.current_period_end.format("%Y-%m-%d")
    );

    // ========================================================================
    // Phase 4: Customer Requests Upgrade (Mid-Cycle)
    // ========================================================================
    println!("\n📈 PHASE 4: Mid-Cycle Upgrade to Enterprise");
    println!("─────────────────────────────────────────────────────────────");

    let (state, action) = governor
        .transition(
            &subscription_id,
            SubscriptionEvent::CustomerRequestsUpgrade {
                new_tier: FeatureTier::Enterprise,
            },
        )
        .await?;

    println!("✓ Upgrade requested");
    println!("  - New state: {:?}", state);
    println!("  - Action: {:?}\n", action);

    let (state, action) = governor
        .transition(&subscription_id, SubscriptionEvent::UpgradeApproved)
        .await?;

    println!("✓ Upgrade approved (proration calculated)");
    println!("  - New state: {:?}", state);
    println!("  - Action: {:?}\n", action);

    let subscription = governor.get_subscription(&subscription_id).unwrap();
    println!("  - New tier: {:?}", subscription.current_tier);

    // ========================================================================
    // Phase 5: Renewal Approaching
    // ========================================================================
    println!("\n🔄 PHASE 5: Renewal Approaching (7 days before)");
    println!("─────────────────────────────────────────────────────────────");

    let (state, action) = governor
        .transition(
            &subscription_id,
            SubscriptionEvent::RenewalDateApproaching,
        )
        .await?;

    println!("✓ Renewal reminder sent");
    println!("  - New state: {:?}", state);
    println!("  - Action: {:?}\n", action);

    // ========================================================================
    // Phase 6: Payment Received
    // ========================================================================
    println!("✅ PHASE 6: Renewal Payment Received");
    println!("─────────────────────────────────────────────────────────────");

    let (state, action) = governor
        .transition(
            &subscription_id,
            SubscriptionEvent::RenewalPaymentReceived {
                amount_cents: 999900, // $9,999.00 annual enterprise
            },
        )
        .await?;

    println!("✓ Renewal payment processed");
    println!("  - New state: {:?}", state);
    println!("  - Amount: $9,999.00");
    println!("  - Action: {:?}\n", action);

    // ========================================================================
    // Phase 7: Downgrade Request (Post-Renewal)
    // ========================================================================
    println!("\n📉 PHASE 7: Customer Requests Downgrade");
    println!("─────────────────────────────────────────────────────────────");

    let (state, _action) = governor
        .transition(
            &subscription_id,
            SubscriptionEvent::CustomerRequestsDowngrade {
                new_tier: FeatureTier::Professional,
            },
        )
        .await?;

    println!("✓ Downgrade requested");
    println!("  - New state: {:?}", state);

    let (state, action) = governor
        .transition(&subscription_id, SubscriptionEvent::DowngradeApproved)
        .await?;

    println!("✓ Downgrade approved (refund issued)");
    println!("  - New state: {:?}", state);
    println!("  - Action: {:?}\n", action);

    let subscription = governor.get_subscription(&subscription_id).unwrap();
    println!("  - New tier: {:?}", subscription.current_tier);

    // ========================================================================
    // Phase 8: Cancellation
    // ========================================================================
    println!("\n🚫 PHASE 8: Customer Cancels Subscription");
    println!("─────────────────────────────────────────────────────────────");

    let (state, action) = governor
        .transition(
            &subscription_id,
            SubscriptionEvent::CustomerCancels {
                reason: "Switching to competitor".to_string(),
            },
        )
        .await?;

    println!("✓ Subscription cancelled");
    println!("  - New state: {:?}", state);
    println!("  - Cancellation reason: Switching to competitor");
    println!("  - Action: {:?}\n", action);

    let subscription = governor.get_subscription(&subscription_id).unwrap();
    println!("  - Reactivation window: 30 days");
    println!("  - Can reactivate: {}", subscription.can_reactivate());

    // ========================================================================
    // Phase 9: Reactivation (Churn Recovery)
    // ========================================================================
    println!("\n♻️  PHASE 9: Customer Requests Reactivation");
    println!("─────────────────────────────────────────────────────────────");

    let (state, action) = governor
        .transition(
            &subscription_id,
            SubscriptionEvent::CustomerRequestsReactivation,
        )
        .await?;

    println!("✓ Subscription reactivated");
    println!("  - New state: {:?}", state);
    println!("  - Action: {:?}\n", action);

    let subscription = governor.get_subscription(&subscription_id).unwrap();
    println!("  - Tier restored: {:?}", subscription.current_tier);
    println!("  - Cancellation reason cleared: {}",
        subscription.cancellation_reason.is_none());

    // ========================================================================
    // Phase 10: Audit Trail
    // ========================================================================
    println!("\n📋 PHASE 10: Audit Trail");
    println!("─────────────────────────────────────────────────────────────");

    let audit_trail = governor.get_audit_trail(&subscription_id);
    println!("✓ Complete audit trail ({} entries):\n", audit_trail.len());

    for (i, entry) in audit_trail.iter().enumerate() {
        println!("  {} [{}] {} → {}",
            i + 1,
            entry.timestamp.format("%H:%M:%S"),
            entry.from_state,
            entry.to_state
        );
    }

    // ========================================================================
    // Summary
    // ========================================================================
    println!("\n╔═══════════════════════════════════════════════════════════╗");
    println!("║ ✨ Complete Subscription Lifecycle Demonstrated            ║");
    println!("╠═══════════════════════════════════════════════════════════╣");
    println!("║ Features Demonstrated:                                    ║");
    println!("║ • Trial period management with expiration signals         ║");
    println!("║ • Purchase conversion with tier selection                 ║");
    println!("║ • Mid-cycle upgrades with proration calculation           ║");
    println!("║ • Renewal payment processing and grace periods            ║");
    println!("║ • Downgrade with automatic refunds                        ║");
    println!("║ • Cancellation with 30-day reactivation window            ║");
    println!("║ • Complete audit trail for compliance                     ║");
    println!("║ • Type-safe FSM with compile-time guarantees              ║");
    println!("╚═══════════════════════════════════════════════════════════╝\n");

    Ok(())
}
