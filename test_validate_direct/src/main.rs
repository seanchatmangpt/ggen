//! Direct test of GgenMcpServer validate tool via MCP protocol
//! This tests the tool through the actual MCP ServerHandler interface

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{
    model::{CallToolRequest, CallToolResult},
    ServerHandler,
};
use serde_json::json;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .init();

    println!("Testing GgenMcpServer validate tool via MCP protocol\n");
    println!("=".repeat(60));

    // Create server instance
    let server = GgenMcpServer::new();
    println!("✓ Created GgenMcpServer instance");

    // Test 1: Valid Turtle content
    println!("\n--- Test 1: Valid Turtle content ---");
    let valid_ttl = r#"@prefix ex: <http://example.org/ns#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Subject a ex:Resource .
ex:Subject ex:name "Test Subject" .
ex:Subject ex:count 42 ."#;

    println!("TTL content:\n{}", valid_ttl);
    println!("\nCalling validate tool...");

    let params = ValidateParams {
        ttl: valid_ttl.to_string(),
    };

    match server.validate(Parameters(params)).await {
        Ok(result) => {
            println!("\nResult: {:?}", result);
            if let Some(content) = result.content.first() {
                println!(
                    "Output text: {}",
                    content.text.as_ref().unwrap_or(&"<no text>".to_string())
                );
            }
        }
        Err(e) => {
            println!("ERROR: {:?}", e);
        }
    }

    // Test 2: Invalid Turtle content (syntax error)
    println!("\n--- Test 2: Invalid Turtle content (syntax error) ---");
    let invalid_ttl = r#"@prefix ex: <http://example.org/ns#> .
ex:Subject a ex:Resource INVALID SYNTAX HERE ."#;

    println!("TTL content:\n{}", invalid_ttl);
    println!("\nCalling validate tool...");

    let params = ValidateParams {
        ttl: invalid_ttl.to_string(),
    };

    match server.validate(Parameters(params)).await {
        Ok(result) => {
            println!("\nResult: {:?}", result);
            if let Some(content) = result.content.first() {
                println!(
                    "Output text: {}",
                    content.text.as_ref().unwrap_or(&"<no text>".to_string())
                );
            }
        }
        Err(e) => {
            println!("ERROR: {:?}", e);
        }
    }

    // Test 3: Empty TTL
    println!("\n--- Test 3: Empty TTL ---");
    let empty_ttl = "";

    println!("TTL content: <empty>");
    println!("\nCalling validate tool...");

    let params = ValidateParams {
        ttl: empty_ttl.to_string(),
    };

    match server.validate(Parameters(params)).await {
        Ok(result) => {
            println!("\nResult: {:?}", result);
            if let Some(content) = result.content.first() {
                println!(
                    "Output text: {}",
                    content.text.as_ref().unwrap_or(&"<no text>".to_string())
                );
            }
        }
        Err(e) => {
            println!("ERROR: {:?}", e);
        }
    }

    println!("\n{}", "=".repeat(60));
    println!("✓ All validate tool tests completed");
    Ok(())
}
