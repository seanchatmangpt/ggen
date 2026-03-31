//! Direct test of GgenMcpServer generate and sync tools
//!
//! This program bypasses cargo test and directly calls the MCP tools
//! to verify they work correctly.

use std::path::PathBuf;
use ggen_a2a_mcp::ggen_server::{GgenMcpServer, GenerateParams, SyncParams};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("=== GgenMcpServer Direct Tool Test ===\n");

    // Create the MCP server instance
    let server = GgenMcpServer::new();
    println!("✓ GgenMcpServer created\n");

    // Use a small test ontology
    let ontology_path = "/Users/sac/ggen/examples/basic-template-generation/ontology/templates.ttl";

    // Verify the ontology file exists
    if !PathBuf::from(ontology_path).exists() {
        eprintln!("✗ Ontology file not found: {}", ontology_path);
        return Err("Ontology file not found".into());
    }
    println!("✓ Ontology file exists: {}\n", ontology_path);

    // Test 1: generate tool
    println!("=== Test 1: generate tool ===");
    let generate_params = GenerateParams {
        ontology_path: ontology_path.to_string(),
        queries_dir: None,
        output_dir: Some("/tmp/ggen_mcp_test_output".to_string()),
        language: Some("rust".to_string()),
    };

    println!("Calling generate with params:");
    println!("  ontology_path: {}", generate_params.ontology_path);
    println!("  output_dir: {:?}", generate_params.output_dir);
    println!("  language: {:?}\n", generate_params.language);

    match server.generate(rmcp::handler::server::wrapper::Parameters(generate_params)).await {
        Ok(result) => {
            println!("✓ generate tool call succeeded");
            println!("Result: {:?}", result);
        }
        Err(e) => {
            eprintln!("✗ generate tool call failed: {:?}", e);
        }
    }

    println!("\n=== Test 2: sync tool (dry-run) ===");

    // Test 2: sync tool with dry_run
    let sync_params = SyncParams {
        ontology_path: ontology_path.to_string(),
        queries_dir: None,
        output_dir: Some("/tmp/ggen_mcp_sync_output".to_string()),
        language: Some("rust".to_string()),
        dry_run: true,
    };

    println!("Calling sync with params:");
    println!("  ontology_path: {}", sync_params.ontology_path);
    println!("  output_dir: {:?}", sync_params.output_dir);
    println!("  language: {:?}", sync_params.language);
    println!("  dry_run: {}\n", sync_params.dry_run);

    match server.sync(rmcp::handler::server::wrapper::Parameters(sync_params)).await {
        Ok(result) => {
            println!("✓ sync tool call succeeded");
            println!("Result: {:?}", result);
        }
        Err(e) => {
            eprintln!("✗ sync tool call failed: {:?}", e);
        }
    }

    println!("\n=== Test 3: sync tool (full run, not dry-run) ===");

    // Test 3: sync tool without dry_run
    let sync_params_full = SyncParams {
        ontology_path: ontology_path.to_string(),
        queries_dir: None,
        output_dir: Some("/tmp/ggen_mcp_sync_full_output".to_string()),
        language: Some("rust".to_string()),
        dry_run: false,
    };

    println!("Calling sync with params:");
    println!("  ontology_path: {}", sync_params_full.ontology_path);
    println!("  output_dir: {:?}", sync_params_full.output_dir);
    println!("  language: {:?}", sync_params_full.language);
    println!("  dry_run: {}\n", sync_params_full.dry_run);

    match server.sync(rmcp::handler::server::wrapper::Parameters(sync_params_full)).await {
        Ok(result) => {
            println!("✓ sync tool call succeeded");
            println!("Result: {:?}", result);
        }
        Err(e) => {
            eprintln!("✗ sync tool call failed: {:?}", e);
        }
    }

    println!("\n=== All tests complete ===");
    Ok(())
}
