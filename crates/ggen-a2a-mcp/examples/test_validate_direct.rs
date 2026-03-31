//! Direct test of the validate MCP tool (not via cargo test)
//!
//! This program creates an in-process MCP server and client, then calls the
//! validate tool with valid Turtle content, printing the result.

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
                .add_directive("rmcp=info".parse()?),
        )
        .init();

    println!("=== Direct Validate Tool Test ===\n");

    // Start the MCP server and client
    println!("✓ Starting MCP server...");
    let client = start_server().await?;
    println!("✓ MCP server and client connected");

    // Define valid Turtle content to test
    let ttl_content = r#"@prefix ex: <http://example.org/ns#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Subject a ex:Resource .
ex:Subject ex:name "Test Subject" ."#;

    println!("✓ Test TTL content defined:\n{}", ttl_content);
    println!();

    // Call the validate tool
    println!("✓ Calling validate tool...");
    let result = client
        .call_tool(
            CallToolRequestParams::new("validate").with_arguments(args(serde_json::json!({
                "ttl": ttl_content
            }))),
        )
        .await?;

    // Print the result
    println!("\n=== Validate Tool Result ===\n");
    println!("Is Error: {}\n", result.is_error.unwrap_or(false));

    if let Some(text) = extract_text(&result) {
        println!("{}", text);
    }

    println!("\n=== Test Complete ===");

    Ok(())
}
