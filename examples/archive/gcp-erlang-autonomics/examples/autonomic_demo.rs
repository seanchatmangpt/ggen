//! Erlang Autonomic System - Complete Demo
//!
//! This example demonstrates the full MAPE-K loop:
//! 1. Monitor: Signal ingestion and normalization
//! 2. Analyze: Entitlement validation
//! 3. Plan: Governor FSM coordination
//! 4. Execute: Action execution via Actuator
//! 5. Knowledge: Receipt ledger audit trail

use gcp_erlang_autonomics::{
    signal_ingest::{SignalIngest, RawEvent},
    entitlement::EntitlementService,
    governor::{Governor, GovernorEvent},
    actuator::Actuator,
    receipt::ReceiptLedger,
};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_target(true)
        .with_level(true)
        .init();

    println!("=== Erlang Autonomic System Demo ===\n");

    // Setup
    let tenant_id = "demo-tenant-1";
    println!("1. Setting up tenant: {}\n", tenant_id);

    // Activate entitlement
    let entitlement = EntitlementService::activate(tenant_id, "pro").await?;
    println!(
        "   ✓ Entitlement activated: {} (State: {:?})\n",
        entitlement.sku, entitlement.state
    );

    // Create governor for tenant
    let mut governor = Governor::new(tenant_id.to_string());
    println!("   ✓ Governor initialized (State: {:?})\n", governor.current_state());

    // === SCENARIO: CPU spike detected ===
    println!("2. Simulating CPU utilization spike...\n");

    // Monitor: Ingest raw signal
    let raw_event_1 = RawEvent {
        tenant_id: tenant_id.to_string(),
        metric: "cpu_utilization".to_string(),
        value: 45.0,
        timestamp_ms: 1000000000,
    };

    let signal_1 = SignalIngest::normalize(raw_event_1).await?;
    println!(
        "   Monitor: Signal ingested (value: {}%)\n",
        signal_1.normalized_value
    );

    // Analyze: Check entitlement
    let _ent = EntitlementService::get_active(tenant_id).await?;
    println!("   Analyze: Entitlement verified ✓\n");

    // Plan: Governor transition
    let (state_1, action_1) =
        governor.transition(GovernorEvent::SignalReceived(signal_1.clone())).await?;
    println!(
        "   Plan: Governor transition → {:?} (action: {:?})\n",
        state_1, action_1
    );

    // === SCENARIO: Sustained high CPU ===
    println!("3. Sustained high CPU (2 more signals at 78%)...\n");

    for i in 0..2 {
        let raw_event = RawEvent {
            tenant_id: tenant_id.to_string(),
            metric: "cpu_utilization".to_string(),
            value: 78.0,
            timestamp_ms: 1000000000 + ((i + 1) as i64 * 10000),
        };

        let signal = SignalIngest::normalize(raw_event).await?;
        let (state, action) =
            governor.transition(GovernorEvent::SignalReceived(signal.clone())).await?;

        println!(
            "   Signal #{}: {}% → State: {:?}, Action: {:?}",
            i + 1,
            signal.normalized_value,
            state,
            action
        );

        if let Some(act) = action {
            // Execute action
            let receipt = Actuator::execute(act).await?;
            println!(
                "   → Executed action (receipt: {}, duration: {}ms)\n",
                receipt.id, receipt.duration_ms
            );
        }
    }

    // === SCENARIO: Critical spike ===
    println!("4. Critical CPU spike detected (92%)...\n");

    let raw_event_critical = RawEvent {
        tenant_id: tenant_id.to_string(),
        metric: "cpu_utilization".to_string(),
        value: 92.0,
        timestamp_ms: 1000000000 + 40000,
    };

    let signal_critical = SignalIngest::normalize(raw_event_critical).await?;
    println!(
        "   Monitor: Critical signal detected ({}%)\n",
        signal_critical.normalized_value
    );

    let (state_critical, action_critical) = governor
        .transition(GovernorEvent::SignalReceived(signal_critical.clone()))
        .await?;

    println!(
        "   Plan: Governor → {:?}\n   Action: {:?}\n",
        state_critical, action_critical
    );

    if let Some(act) = action_critical {
        println!("5. Execute remediation action...\n");

        let receipt = Actuator::execute(act).await?;
        println!(
            "   ✓ Action executed (receipt: {})\n   Duration: {}ms\n   Status: {:?}\n",
            receipt.id, receipt.duration_ms, receipt.status
        );

        // Notify Governor of success
        let (state_after_action, _) =
            governor.transition(GovernorEvent::ActionSucceeded).await?;
        println!(
            "   Governor recovered to: {:?}\n",
            state_after_action
        );
    }

    // === SCENARIO: Recovery ===
    println!("6. Signal normalizes (CPU drops to 55%)...\n");

    let raw_event_recovery = RawEvent {
        tenant_id: tenant_id.to_string(),
        metric: "cpu_utilization".to_string(),
        value: 55.0,
        timestamp_ms: 1000000000 + 60000,
    };

    let signal_recovery = SignalIngest::normalize(raw_event_recovery).await?;
    let (state_final, _) = governor
        .transition(GovernorEvent::SignalReceived(signal_recovery.clone()))
        .await?;

    println!("   Signal normalized ({}%)", signal_recovery.normalized_value);
    println!("   Governor state: {:?}\n", state_final);

    // === AUDIT TRAIL ===
    println!("7. Receipt ledger verification...\n");

    let is_valid = ReceiptLedger::verify_chain().await?;
    let chain_len = ReceiptLedger::len();

    println!("   Chain integrity: {}", if is_valid { "✓ VALID" } else { "✗ CORRUPTED" });
    println!("   Total receipts: {}", chain_len);

    let tail = ReceiptLedger::tail(3).await;
    if !tail.is_empty() {
        println!("\n   Last 3 receipts:");
        for (i, receipt) in tail.iter().enumerate() {
            println!(
                "     {}. [{}] {} → {}",
                i + 1,
                &receipt.id[0..8],
                receipt.action,
                receipt.result
            );
        }
    }

    // === SUMMARY ===
    println!("\n=== Autonomic System Summary ===\n");
    println!(
        "Time in current state: {}ms",
        governor.time_in_state().num_milliseconds()
    );
    println!("Consecutive high signals: {}", governor.consecutive_high_count());
    println!("Active receipts: {}", Actuator::active_receipts().len());

    println!("\n✓ Demo completed successfully!\n");

    Ok(())
}
