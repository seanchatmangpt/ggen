//! Test the validate MCP tool with invalid TTL
//!
//! This program tests error handling by calling validate with malformed TTL.

use ggen_a2a_mcp::ggen_server::GgenMcpServer;
use rmcp::{model::*, service::RunningService, ClientHandler, RoleClient, ServiceExt};

// ---------------------------------------------------------------------------
// Minimal no-op client handler
// ---------------------------------------------------------------------------

#[derive(Debug, Clone, Default)]
struct TestClientHandler;

impl ClientHandler for TestClientHandler {
    fn get_info(&self) -> ClientInfo {
        ClientInfo::default()
    }
}

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

/// Spin up GgenMcpServer over an in-process duplex transport (65536 buffer).
async fn start_server() -> anyhow::Result<RunningService<RoleClient, TestClientHandler>> {
    let (server_transport, client_transport) = tokio::io::duplex(65536);

    let server = GgenMcpServer::new();
    tokio::spawn(async move {
        let _ = server.serve(server_transport).await;
    });

    let client = TestClientHandler::default().serve(client_transport).await?;
    Ok(client)
}

/// Extract text content from a CallToolResult.
fn extract_text(result: &CallToolResult) -> Option<String> {
    result.content.iter().find_map(|c| {
        if let RawContent::Text(tc) = &c.raw {
            Some(tc.text.clone())
        } else {
            None
        }
    })
}

/// Build tool arguments from a JSON object literal.
fn args(json: serde_json::Value) -> serde_json::Map<String, serde_json::Value> {
    json.as_object().cloned().unwrap_or_default()
}

// ---------------------------------------------------------------------------
// Main
// ---------------------------------------------------------------------------

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into())
                .add_directive("ggen_a2a_mcp=trace".parse()?)
                .add_directive("rmcp=info".parse()?)
        )
        .init();

    println!("=== Validate Tool Error Handling Test ===\n");

    // Start the MCP server and client
    println!("✓ Starting MCP server...");
    let client = start_server().await?;
    println!("✓ MCP server and client connected");

    // Test 1: Invalid Turtle (missing prefix)
    println!("\n--- Test 1: Missing prefix ---");
    let invalid_ttl_1 = r#"
ex:Subject a ex:Resource .
"#;

    println!("Testing TTL:\n{}", invalid_ttl_1);
    let result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": invalid_ttl_1
            })))
        )
        .await?;

    println!("Is Error: {}", result.is_error.unwrap_or(false));
    if let Some(text) = extract_text(&result) {
        println!("Result: {}\n", text);
    }

    // Test 2: Invalid Turtle (malformed triple)
    println!("--- Test 2: Malformed triple ---");
    let invalid_ttl_2 = r#"@prefix ex: <http://example.org/ns#> .
ex:Subject INVALID TRIPLE HERE
"#;

    println!("Testing TTL:\n{}", invalid_ttl_2);
    let result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": invalid_ttl_2
            })))
        )
        .await?;

    println!("Is Error: {}", result.is_error.unwrap_or(false));
    if let Some(text) = extract_text(&result) {
        println!("Result: {}\n", text);
    }

    // Test 3: Valid TTL (control test)
    println!("--- Test 3: Valid TTL (control) ---");
    let valid_ttl = r#"@prefix ex: <http://example.org/ns#> .
ex:Subject a ex:Resource .
"#;

    println!("Testing TTL:\n{}", valid_ttl);
    let result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": valid_ttl
            })))
        )
        .await?;

    println!("Is Error: {}", result.is_error.unwrap_or(false));
    if let Some(text) = extract_text(&result) {
        println!("Result: {}\n", text);
    }

    println!("=== Test Complete ===");

    Ok(())
}
