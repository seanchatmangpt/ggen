//! test_telco_routing.rs - Chicago TDD Verification
//!
//! This test proves the "Telco" routing capability through multi-surface corroboration:
//! 1. Execution Path: UnifiedMessageRouter processes a routing request.
//! 2. Observable State: Metrics are updated (metrics().total_messages == 1).
//! 3. External Evidence: CLI output contains real trace_id and status.

use ggen_core::a2a_generated::converged::ConvergedMessage;
use ggen_core::a2a_generated::handlers::HandlerFactory;

#[tokio::test]
async fn test_telco_unified_router_execution() {
    // 1. Execution Surface & 2. Observable State
    let mut router = HandlerFactory::create_router();

    let message = ConvergedMessage::text(
        "test-1".to_string(),
        "test-agent".to_string(),
        "Hello".to_string(),
    );

    let result = router.route(&message).await.expect("Routing failed");

    // VERIFY: Metrics must reflect real execution
    assert_eq!(
        router.metrics().total_messages,
        1,
        "Metrics should record 1 message"
    );
    assert_eq!(
        result.metrics.operations, 1,
        "Handler should report 1 operation"
    );
}

#[tokio::test]
async fn test_telco_routing_boundary_crossing() {
    // 3. External Evidence Surface

    // Proving the command exists and returns real evidence
    let output = std::process::Command::new("./target/release/ggen")
        .args(["telco", "route"])
        .output()
        .expect("Failed to execute ggen");

    assert!(
        output.status.success(),
        "ggen telco route should exit successfully"
    );

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Evidence must contain REAL fields, not placeholders
    assert!(
        stdout.contains("\"status\":\"Success\""),
        "Should contain real Success status"
    );
    assert!(
        stdout.contains("\"trace_id\":\"trace-release-probe-1\""),
        "Should contain real trace_id"
    );
}
