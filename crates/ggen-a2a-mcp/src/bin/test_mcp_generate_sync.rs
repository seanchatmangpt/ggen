//! Standalone test program for MCP generate/sync tools
//! Tests the GgenMcpServer generate() and sync() methods directly

use std::path::PathBuf;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("=== MCP Generate/Sync Tool Test ===\n");

    // Create server instance
    let server = ggen_a2a_mcp::ggen_server::GgenMcpServer::new();

    // Test 1: generate tool
    println!("Test 1: Calling generate() tool...");
    println!("Ontology: /Users/sac/ggen/examples/basic-template-generation/ontology/templates.ttl");
    println!();

    let generate_params = ggen_a2a_mcp::ggen_server::GenerateParams {
        ontology_path: "/Users/sac/ggen/examples/basic-template-generation/ontology/templates.ttl"
            .to_string(),
        queries_dir: None,
        output_dir: Some("/tmp/mcp_test_generate".to_string()),
        language: Some("rust".to_string()),
    };

    let generate_result = server
        .generate(rmcp::handler::server::wrapper::Parameters(generate_params))
        .await?;

    println!("Generate Result:");
    match &generate_result {
        rmcp::model::CallToolResult::Success { content, .. } => {
            for item in content {
                if let Some(text) = item.as_text() {
                    println!("{}", text);
                }
            }
        }
        rmcp::model::CallToolResult::Error { error, .. } => {
            println!("Error: {:?}", error);
        }
    }
    println!();

    // Test 2: sync tool (dry-run)
    println!("Test 2: Calling sync() tool (dry-run)...");
    println!("Ontology: /Users/sac/ggen/examples/basic-template-generation/ontology/templates.ttl");
    println!();

    let sync_params = ggen_a2a_mcp::ggen_server::SyncParams {
        ontology_path: "/Users/sac/ggen/examples/basic-template-generation/ontology/templates.ttl"
            .to_string(),
        queries_dir: None,
        output_dir: Some("/tmp/mcp_test_sync".to_string()),
        language: Some("rust".to_string()),
        dry_run: true,
    };

    let sync_result = server
        .sync(rmcp::handler::server::wrapper::Parameters(sync_params))
        .await?;

    println!("Sync Result (dry-run):");
    match &sync_result {
        rmcp::model::CallToolResult::Success { content, .. } => {
            for item in content {
                if let Some(text) = item.as_text() {
                    println!("{}", text);
                }
            }
        }
        rmcp::model::CallToolResult::Error { error, .. } => {
            println!("Error: {:?}", error);
        }
    }
    println!();

    // Test 3: sync tool (full run)
    println!("Test 3: Calling sync() tool (full run)...");
    println!("Ontology: /Users/sac/ggen/examples/basic-template-generation/ontology/templates.ttl");
    println!();

    let sync_params_full = ggen_a2a_mcp::ggen_server::SyncParams {
        ontology_path: "/Users/sac/ggen/examples/basic-template-generation/ontology/templates.ttl"
            .to_string(),
        queries_dir: None,
        output_dir: Some("/tmp/mcp_test_sync_full".to_string()),
        language: Some("rust".to_string()),
        dry_run: false,
    };

    let sync_result_full = server
        .sync(rmcp::handler::server::wrapper::Parameters(sync_params_full))
        .await?;

    println!("Sync Result (full):");
    match &sync_result_full {
        rmcp::model::CallToolResult::Success { content, .. } => {
            for item in content {
                if let Some(text) = item.as_text() {
                    println!("{}", text);
                }
            }
        }
        rmcp::model::CallToolResult::Error { error, .. } => {
            println!("Error: {:?}", error);
        }
    }

    println!("\n=== Test Complete ===");

    Ok(())
}
