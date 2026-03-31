//! MCP Client Example: Calling the `generate` tool via rmcp protocol
//!
//! This example demonstrates how to:
//! 1. Connect to the ggen MCP server via stdio
//! 2. Call the `generate` tool with ontology parameters
//! 3. Receive and display generated files and receipt
//!
//! Usage:
//!   cargo run --example mcp_generate_example

use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== MCP Client Example: generate Tool ===\n");

    // Path to test ontology (using a small test ontology)
    let ontology_path = "/Users/sac/ggen/.specify/specs/016-self-play/ggen-meta.ttl";
    let output_dir = "/tmp/mcp_generate_test_output";

    // Verify ontology exists
    if !Path::new(ontology_path).exists() {
        eprintln!("❌ Ontology file not found: {}", ontology_path);
        eprintln!("\nPlease ensure the ontology file exists at this path.");
        eprintln!("You can use a different ontology by modifying the `ontology_path` variable.");
        return Err("Ontology file not found".into());
    }

    println!("✅ Ontology found: {}", ontology_path);
    println!("📁 Output directory: {}", output_dir);

    println!("\n=== JSON-RPC Request Format ===");
    let request = serde_json::json!({
        "jsonrpc": "2.0",
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

    println!("{}", serde_json::to_string_pretty(&request)?);

    println!("\n=== Expected Response Format ===");
    println!("✅ Success response:");
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

    println!("\n❌ Error response:");
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

    println!("\n=== How to Use with Actual MCP Server ===");
    println!("1️⃣  Start ggen MCP server in one terminal:");
    println!("   cd /Users/sac/ggen");
    println!("   cargo run -p ggen-a2a-mcp -- mcp start-server --transport stdio");

    println!("\n2️⃣  Call the generate tool via MCP client:");
    println!("   (You would use rmcp client library to connect via stdio)");

    println!("\n=== Alternative: Direct ggen CLI ===");
    println!("You can also use the ggen CLI directly:");
    println!("   cd /Users/sac/ggen");
    println!(
        "   cargo run --bin ggen -- sync --ontology {} --output {}",
        ontology_path, output_dir
    );

    println!("\n=== Test Command ===");
    println!("Run this command to test the generate pipeline:");
    println!(
        "   cargo run --bin ggen -- sync --ontology {} --output {} --language rust",
        ontology_path, output_dir
    );

    Ok(())
}
