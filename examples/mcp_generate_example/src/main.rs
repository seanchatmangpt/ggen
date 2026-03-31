//! MCP Client Example: Calling the `generate` tool via rmcp protocol
//!
//! This example demonstrates how to:
//! 1. Connect to the ggen MCP server via stdio
//! 2. Call the `generate` tool with ontology parameters
//! 3. Receive and display generated files and receipt

use anyhow::Result;
use rmcp::{
    client::{ClientBuilder, ClientError},
    model::{CallToolRequestParams, CallToolResult},
    protocol::JSONRPC_VERSION,
};
use serde_json::json;
use std::path::Path;
use tokio::io::{AsyncBufReadExt, AsyncWriteExt, BufReader};
use tracing::{error, info};

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            tracing_subscriber::EnvFilter::from_default_env()
                .add_directive(tracing::Level::INFO.into()),
        )
        .init();

    info!("Starting MCP generate example");

    // Path to test ontology (using a small test ontology)
    let ontology_path = "/Users/sac/ggen/.specify/specs/016-self-play/ggen-meta.ttl";
    let output_dir = "/tmp/mcp_generate_test_output";

    // Verify ontology exists
    if !Path::new(ontology_path).exists() {
        error!("Ontology file not found: {}", ontology_path);
        anyhow::bail!("Ontology file not found: {}", ontology_path);
    }

    info!("Using ontology: {}", ontology_path);
    info!("Output directory: {}", output_dir);

    // For this example, we'll demonstrate the JSON-RPC request format
    // In a real scenario, you would spawn the ggen MCP server and communicate via stdio

    let request = json!({
        "jsonrpc": JSONRPC_VERSION,
        "id": 1,
        "method": "tools/call",
        "params": {
            "name": "generate",
            "arguments": {
                "ontology_path": ontology_path,
                "output_dir": output_dir,
                "language": "rust"
            }
        }
    });

    println!("=== MCP Request: generate tool ===");
    println!("{}", serde_json::to_string_pretty(&request)?);

    // Expected response format
    println!("\n=== Expected Response Format ===");
    println!("Success response:");
    println!(
        r#"{{
  "jsonrpc": "2.0",
  "result": {{
    "content": [
      {{
        "type": "text",
        "text": "Generated N file(s) in Xms\nFiles: path1.rs, path2.rs, ...\nReceipt: sha256hash..."
      }}
    ]
  }},
  "id": 1
}}"#
    );

    println!("\nError response:");
    println!(
        r#"{{
  "jsonrpc": "2.0",
  "error": {{
    "code": -32602,
    "message": "Ontology file not found: /path/to/file.ttl"
  }},
  "id": 1
}}"#
    );

    // Demonstrate how to call the tool using rmcp client
    println!("\n=== To Run with Actual MCP Server ===");
    println!("1. Start ggen MCP server in one terminal:");
    println!("   cd /Users/sac/ggen && cargo run -p ggen-a2a-mcp -- mcp start-server --transport stdio");
    println!("\n2. In another terminal, run this example with stdio transport:");
    println!("   (Modify this example to use stdio transport instead of JSON-RPC format)");

    println!("\n=== Alternative: Direct ggen CLI ===");
    println!("You can also use the ggen CLI directly:");
    println!("   cd /Users/sac/ggen && cargo run --bin ggen -- sync --ontology {} --output {}", ontology_path, output_dir);

    Ok(())
}

/// Future implementation: Actual rmcp client connection
///
/// This function shows how to connect to the MCP server using rmcp client:
///
/// ```rust,no_run
/// async fn call_generate_tool(ontology_path: &str, output_dir: &str) -> Result<CallToolResult, ClientError> {
///     // Connect to MCP server via stdio
///     let (client, _) = ClientBuilder::new()
///         .connect(stdio())
///         .await
///         .map_err(ClientError::from)?;
///
///     // Call the generate tool
///     let params = CallToolRequestParams {
///         name: "generate".to_string(),
///         arguments: Some(json!({
///             "ontology_path": ontology_path,
///             "output_dir": output_dir,
///             "language": "rust"
///         })),
///     };
///
///     client.call_tool(params).await.map_err(ClientError::from)
/// }
/// ```
///
/// Note: This requires the ggen MCP server to be running with stdio transport.

fn print_response_summary(result: &CallToolResult) {
    println!("\n=== Response Summary ===");

    if result.is_error {
        println!("Status: ERROR");
        if let Some(content) = result.content.first() {
            println!("Error: {}", content.text);
        }
    } else {
        println!("Status: SUCCESS");
        if let Some(content) = result.content.first() {
            println!("Output:");
            println!("{}", content.text);
        }
    }
}
