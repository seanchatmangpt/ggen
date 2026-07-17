//! test_telco_routing.rs - Chicago TDD Verification
//!
//! This test proves the "Telco" routing capability through multi-surface corroboration:
//! 1. Execution Path: UnifiedMessageRouter processes a routing request.
//! 2. Observable State: Metrics are updated (metrics().total_messages == 1).
//! 3. External Evidence: CLI output contains real trace_id and status.
//!
//! ARCHIVED (2026-07-16, publish-safety fix): root no longer depends on ggen-lsp at
//! all -- its `a2a` feature isn't published to crates.io yet, and referencing it
//! anywhere in root's manifest (even inactive/optional) blocks
//! `cargo publish --dry-run -p ggen`. Already gated behind
//! `required-features = ["integration", "a2a-integration-tests"]` in Cargo.toml,
//! both permanently off. Retained on disk, not deleted, per fix-forward doctrine --
//! its real home is crates/ggen-lsp/tests, where ggen-lsp's own a2a feature is
//! native/local and doesn't need external registry resolution to test.

use ggen_lsp::a2a_mcp::a2a_generated::converged::ConvergedMessage;
use ggen_lsp::a2a_mcp::a2a_generated::handlers::HandlerFactory;

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
