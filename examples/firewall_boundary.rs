//! Firewall Boundary Example
//!
//! Demonstrates 3-channel ingress control with admission rules.
//! Everything else gets refused with cryptographic receipt.

use ggen_firewall::{
    channels::{BatchChannel, EmergencyChannel, ScheduledChannel},
    AdmissionResponse, Firewall, IngressChannel, IngressRequest,
};
use chrono::{Duration, Utc};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== Firewall Boundary Example ===\n");
    println!("3 channels control ALL ingress:\n");
    println!("  1. Batch - Rate-limited bulk operations");
    println!("  2. Scheduled - Time-windowed planned operations");
    println!("  3. Emergency - Circuit-breaker bypass\n");

    // Create firewall with custom channels
    let batch = Box::new(BatchChannel::new(100, 10));
    let mut scheduled = ScheduledChannel::new();

    // Configure scheduled window (next hour)
    let now = Utc::now();
    let window_end = now + Duration::hours(1);
    scheduled.add_window(now, window_end);

    let emergency = Box::new(EmergencyChannel::new());

    let mut firewall = Firewall::new(batch, Box::new(scheduled), emergency);
    println!("✓ Firewall initialized with 3 channels\n");

    // Example 1: Batch channel (within limits)
    println!("=== Example 1: Batch Channel ===");
    for i in 1..=5 {
        let request = IngressRequest::new(
            IngressChannel::Batch,
            format!("Batch payload {}", i).into_bytes(),
        )
        .with_metadata(serde_json::json!({
            "batch_id": format!("batch-{}", i),
            "priority": "normal"
        }));

        match firewall.process(request).await {
            AdmissionResponse::Admitted {
                request_id,
                channel,
                admitted_at,
            } => {
                println!("✓ Request {} admitted via {:?}", i, channel);
                println!("  ID: {}", request_id);
                println!("  Admitted at: {}", admitted_at);
            }
            AdmissionResponse::Refused {
                request_id,
                reason,
                receipt,
            } => {
                println!("✗ Request {} refused", i);
                println!("  Reason: {}", reason);
                println!("  Receipt ID: {}", receipt.id);
            }
        }
    }
    println!();

    // Example 2: Batch rate limit exceeded
    println!("=== Example 2: Batch Rate Limit ===");
    println!("Attempting 6 more requests (total 11, limit 10):");

    for i in 6..=11 {
        let request = IngressRequest::new(
            IngressChannel::Batch,
            format!("Batch payload {}", i).into_bytes(),
        );

        match firewall.process(request).await {
            AdmissionResponse::Admitted { request_id, .. } => {
                println!("  ✓ Request {} admitted ({})", i, request_id);
            }
            AdmissionResponse::Refused { reason, receipt, .. } => {
                println!("  ✗ Request {} refused", i);
                println!("    Reason: {}", reason);
                println!("    Receipt hash: {}", receipt.hash);
            }
        }
    }
    println!();

    // Example 3: Scheduled channel (within window)
    println!("=== Example 3: Scheduled Channel ===");
    println!("Window: {} to {}", now.format("%H:%M"), window_end.format("%H:%M"));

    let scheduled_request = IngressRequest::new(
        IngressChannel::Scheduled,
        "Scheduled maintenance task".as_bytes().to_vec(),
    )
    .with_metadata(serde_json::json!({
        "task": "database_backup",
        "scheduled_time": now.to_rfc3339()
    }));

    match firewall.process(scheduled_request).await {
        AdmissionResponse::Admitted {
            request_id,
            channel,
            ..
        } => {
            println!("✓ Scheduled request admitted");
            println!("  ID: {}", request_id);
            println!("  Channel: {:?}", channel);
        }
        AdmissionResponse::Refused { reason, .. } => {
            println!("✗ Scheduled request refused: {}", reason);
        }
    }
    println!();

    // Example 4: Scheduled channel (outside window)
    println!("=== Example 4: Outside Scheduled Window ===");
    let mut past_request = IngressRequest::new(
        IngressChannel::Scheduled,
        "Old maintenance task".as_bytes().to_vec(),
    );
    past_request.timestamp = now - Duration::hours(2);

    match firewall.process(past_request).await {
        AdmissionResponse::Admitted { .. } => {
            println!("✗ Unexpectedly admitted");
        }
        AdmissionResponse::Refused {
            reason,
            receipt,
            request_id,
        } => {
            println!("✓ Request correctly refused");
            println!("  Request ID: {}", request_id);
            println!("  Reason: {}", reason);
            println!("  Receipt ID: {}", receipt.id);
            println!("  Receipt hash: {}", receipt.hash);
        }
    }
    println!();

    // Example 5: Emergency channel
    println!("=== Example 5: Emergency Channel ===");
    let emergency_request = IngressRequest::new(
        IngressChannel::Emergency,
        "Critical security patch".as_bytes().to_vec(),
    )
    .with_metadata(serde_json::json!({
        "severity": "critical",
        "cve": "CVE-2026-1234"
    }));

    match firewall.process(emergency_request).await {
        AdmissionResponse::Admitted {
            request_id,
            channel,
            ..
        } => {
            println!("✓ Emergency request admitted (bypasses limits)");
            println!("  ID: {}", request_id);
            println!("  Channel: {:?}", channel);
        }
        AdmissionResponse::Refused { reason, .. } => {
            println!("✗ Emergency request refused: {}", reason);
        }
    }
    println!();

    // Example 6: Invalid payloads
    println!("=== Example 6: Invalid Requests ===");

    // Empty payload
    println!("Empty payload:");
    let empty_request = IngressRequest::new(IngressChannel::Batch, vec![]);
    match firewall.process(empty_request).await {
        AdmissionResponse::Refused { reason, .. } => {
            println!("  ✓ Correctly refused: {}", reason);
        }
        _ => println!("  ✗ Should have been refused"),
    }

    // Future timestamp
    println!("\nFuture timestamp:");
    let mut future_request = IngressRequest::new(
        IngressChannel::Batch,
        "Future payload".as_bytes().to_vec(),
    );
    future_request.timestamp = Utc::now() + Duration::hours(2);
    match firewall.process(future_request).await {
        AdmissionResponse::Refused { reason, .. } => {
            println!("  ✓ Correctly refused: {}", reason);
        }
        _ => println!("  ✗ Should have been refused"),
    }
    println!();

    // Summary
    println!("=== Summary ===");
    println!("✓ All ingress controlled through 3 channels");
    println!("✓ Rate limits enforced");
    println!("✓ Time windows respected");
    println!("✓ Invalid requests refused with receipts");
    println!("✓ Emergency bypass available");

    println!("\n✓ Firewall boundary example completed");
    println!("Key insight: 3 channels only - everything else refused");

    Ok(())
}
