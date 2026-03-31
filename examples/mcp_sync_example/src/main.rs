//! MCP Client Example: Calling the `sync` tool via rmcp protocol
//!
//! This example demonstrates how to:
//! 1. Connect to the ggen MCP server via stdio
//! 2. Call the `sync` tool with ontology parameters
//! 3. Receive and display sync results and receipt

use anyhow::Result;
use rmcp::{
    client::{ClientBuilder, ClientError},
    model::{CallToolRequestParams, CallToolResult},
    protocol::JSONRPC_VERSION,
};
use serde_json::json;
use std::path::Path;
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

    info!("Starting MCP sync example");

    // Path to test ontology (using a small test ontology)
    let ontology_path = "/Users/sac/ggen/.specify/specs/016-self-play/ggen-meta.ttl";
    let output_dir = "/tmp/mcp_sync_test_output";
    let dry_run = false; // Set to true for preview mode

    // Verify ontology exists
    if !Path::new(ontology_path).exists() {
        error!("Ontology file not found: {}", ontology_path);
        anyhow::bail!("Ontology file not found: {}", ontology_path);
    }

    info!("Using ontology: {}", ontology_path);
    info!("Output directory: {}", output_dir);
    info!("Dry run: {}", dry_run);

    // For this example, we'll demonstrate the JSON-RPC request format
    // In a real scenario, you would spawn the ggen MCP server and communicate via stdio

    let request = json!({
        "jsonrpc": JSONRPC_VERSION,
        "id": 1,
        "method": "tools/call",
        "params": {
            "name": "sync",
            "arguments": {
                "ontology_path": ontology_path,
                "output_dir": output_dir,
                "language": "rust",
                "dry_run": dry_run
            }
        }
    });

    println!("=== MCP Request: sync tool ===");
    println!("{}", serde_json::to_string_pretty(&request)?);

    // Expected response format
    println!("\n=== Expected Response Format ===");
    println!("Success response (full sync):");
    println!(
        r#"{{
  "jsonrpc": "2.0",
  "result": {{
    "content": [
      {{
        "type": "text",
        "text": "Sync (full) complete: N file(s) in Xms\nFiles: path1.rs, path2.rs, ...\nReceipt: sha256hash..."
      }}
    ]
  }},
  "id": 1
}}"#
    );

    println!("\nSuccess response (dry-run):");
    println!(
        r#"{{
  "jsonrpc": "2.0",
  "result": {{
    "content": [
      {{
        "type": "text",
        "text": "Sync (dry-run) complete: N file(s) in Xms\nFiles: path1.rs, path2.rs, ...\nReceipt: sha256hash..."
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
    println!("   cd /Users/sac/ggen && cargo run --bin ggen -- sync --ontology {} --output {}{}", ontology_path, output_dir, if dry_run { " --dry-run" } else { "" });

    println!("\n=== Key Differences: generate vs sync ===");
    println!("generate:");
    println!("  - Runs μ₁-μ₅ pipeline with validation enabled");
    println!("  - Forces regeneration of all files");
    println!("  - Use when: Initial generation or complete rebuild needed");
    println!("\nsync:");
    println!("  - Runs μ₁-μ₅ pipeline with validation enabled");
    println!("  - Detects changes and regenerates only affected files");
    println!("  - Supports dry-run mode for preview");
    println!("  - Use when: Iterative development with incremental updates");

    Ok(())
}

/// Future implementation: Actual rmcp client connection
///
/// This function shows how to connect to the MCP server using rmcp client:
///
/// ```rust,no_run
/// async fn call_sync_tool(
///     ontology_path: &str,
///     output_dir: &str,
///     dry_run: bool,
/// ) -> Result<CallToolResult, ClientError> {
///     // Connect to MCP server via stdio
///     let (client, _) = ClientBuilder::new()
///         .connect(stdio())
///         .await
///         .map_err(ClientError::from)?;
///
///     // Call the sync tool
///     let params = CallToolRequestParams {
///         name: "sync".to_string(),
///         arguments: Some(json!({
///             "ontology_path": ontology_path,
///             "output_dir": output_dir,
///             "language": "rust",
///             "dry_run": dry_run
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

            // Parse and display key information
            if content.text.contains("file(s)") {
                let files: Vec<&str> = content
                    .text
                    .lines()
                    .filter(|line| line.contains(".rs") || line.contains(".toml"))
                    .collect();

                if !files.is_empty() {
                    println!("\nGenerated Files:");
                    for file in files {
                        println!("  {}", file.trim());
                    }
                }
            }

            if content.text.contains("Receipt:") {
                let receipt_line = content
                    .text
                    .lines()
                    .find(|line| line.contains("Receipt:"))
                    .unwrap_or("Receipt: (not found)");
                println!("\n{}", receipt_line);
            }
        }
    }
}
