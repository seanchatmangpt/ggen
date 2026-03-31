//! MCP Client Example: Calling the `sync` tool via rmcp protocol
//!
//! This example demonstrates how to:
//! 1. Connect to the ggen MCP server via stdio
//! 2. Call the `sync` tool with ontology parameters
//! 3. Receive and display sync results and receipt
//!
//! Usage:
//!   cargo run --example mcp_sync_example

use std::path::Path;

fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("=== MCP Client Example: sync Tool ===\n");

    // Path to test ontology (using a small test ontology)
    let ontology_path = "/Users/sac/ggen/.specify/specs/016-self-play/ggen-meta.ttl";
    let output_dir = "/tmp/mcp_sync_test_output";
    let dry_run = false; // Set to true for preview mode

    // Verify ontology exists
    if !Path::new(ontology_path).exists() {
        eprintln!("❌ Ontology file not found: {}", ontology_path);
        eprintln!("\nPlease ensure the ontology file exists at this path.");
        eprintln!("You can use a different ontology by modifying the `ontology_path` variable.");
        return Err("Ontology file not found".into());
    }

    println!("✅ Ontology found: {}", ontology_path);
    println!("📁 Output directory: {}", output_dir);
    println!("🔍 Dry run: {}", dry_run);

    println!("\n=== JSON-RPC Request Format ===");
    let request = serde_json::json!({
        "jsonrpc": "2.0",
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

    println!("{}", serde_json::to_string_pretty(&request)?);

    println!("\n=== Expected Response Format ===");
    println!("✅ Success response (full sync):");
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

    println!("\n✅ Success response (dry-run):");
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

    println!("\n2️⃣  Call the sync tool via MCP client:");
    println!("   (You would use rmcp client library to connect via stdio)");

    println!("\n=== Alternative: Direct ggen CLI ===");
    println!("You can also use the ggen CLI directly:");
    println!("   cd /Users/sac/ggen");
    let dry_run_flag = if dry_run { " --dry-run" } else { "" };
    println!("   cargo run --bin ggen -- sync --ontology {} --output {}{}", ontology_path, output_dir, dry_run_flag);

    println!("\n=== Test Command ===");
    println!("Run this command to test the sync pipeline:");
    println!("   cargo run --bin ggen -- sync --ontology {} --output {} --language rust", ontology_path, output_dir);

    println!("\n=== Key Differences: generate vs sync ===");
    println!("📝 generate:");
    println!("   - Runs μ₁-μ₅ pipeline with validation enabled");
    println!("   - Forces regeneration of all files");
    println!("   - Use when: Initial generation or complete rebuild needed");

    println!("\n🔄 sync:");
    println!("   - Runs μ₁-μ₅ pipeline with validation enabled");
    println!("   - Detects changes and regenerates only affected files");
    println!("   - Supports dry-run mode for preview");
    println!("   - Use when: Iterative development with incremental updates");

    Ok(())
}
