//! Direct test of the sync MCP tool
//!
//! Tests that the sync tool can execute the full μ₁-μ₅ pipeline
//! and return results showing files generated.

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use std::path::PathBuf;

#[tokio::test]
async fn test_sync_tool_basic() {
    let server = GgenMcpServer::new();

    // Use a simple example ontology
    let ontology_path = "examples/basic-template-generation/ontology/templates.ttl";

    // Create a temporary output directory
    let temp_dir = std::env::temp_dir().join("ggen_sync_test");
    let _ = std::fs::create_dir_all(&temp_dir);

    let result = server
        .sync(ggen_a2a_mcp::ggen_server::SyncParams {
            ontology_path: ontology_path.to_string(),
            queries_dir: None,
            output_dir: Some(temp_dir.to_string_lossy().to_string()),
            language: Some("rust".to_string()),
            dry_run: Some(false),
        })
        .await;

    assert!(
        result.is_success(),
        "Sync tool should succeed: {:?}",
        result
    );

    let response = result.unwrap().contents.unwrap();
    let response_text = response.iter().filter_map(|c| c.as_text()).next().unwrap();

    println!("=== SYNC TOOL OUTPUT ===");
    println!("{}", response_text);
    println!("=== END SYNC TOOL OUTPUT ===");

    // Verify the response contains expected elements
    assert!(
        response_text.contains("file"),
        "Response should mention files generated"
    );

    assert!(
        response_text.contains("ms"),
        "Response should include timing information"
    );

    // Cleanup
    let _ = std::fs::remove_dir_all(&temp_dir);
}

#[tokio::test]
async fn test_sync_tool_dry_run() {
    let server = GgenMcpServer::new();

    let ontology_path = "examples/basic-template-generation/ontology/templates.ttl";

    let result = server
        .sync(ggen_a2a_mcp::ggen_server::SyncParams {
            ontology_path: ontology_path.to_string(),
            queries_dir: None,
            output_dir: None,
            language: Some("rust".to_string()),
            dry_run: Some(true),
        })
        .await;

    assert!(result.is_success(), "Sync dry run should succeed");

    let response = result.unwrap().contents.unwrap();
    let response_text = response.iter().filter_map(|c| c.as_text()).next().unwrap();

    println!("=== SYNC DRY RUN OUTPUT ===");
    println!("{}", response_text);
    println!("=== END SYNC DRY RUN OUTPUT ===");

    assert!(
        response_text.contains("dry run"),
        "Response should indicate dry run mode"
    );
}
