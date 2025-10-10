//! Test script for ggen-ai MCP server

use serde_json::{json, Value};
use std::process::{Command, Stdio};
use std::io::{BufRead, BufReader, Write};

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    println!("🔧 Testing ggen-ai MCP Server");
    println!("=============================");

    // Start the MCP server
    let mut server = Command::new("cargo")
        .args(&["run", "--bin", "ggen-ai-mcp"])
        .current_dir("/Users/sac/ggen/ggen-ai")
        .stdin(Stdio::piped())
        .stdout(Stdio::piped())
        .stderr(Stdio::piped())
        .spawn()?;

    let stdin = server.stdin.as_mut().unwrap();
    let stdout = server.stdout.as_mut().unwrap();
    let stderr = server.stderr.as_mut().unwrap();

    // Initialize the MCP connection
    let init_request = json!({
        "jsonrpc": "2.0",
        "id": 1,
        "method": "initialize",
        "params": {
            "protocolVersion": "2024-11-05",
            "capabilities": {
                "tools": {}
            },
            "clientInfo": {
                "name": "ggen-ai-test",
                "version": "1.0.0"
            }
        }
    });

    writeln!(stdin, "{}", init_request)?;
    stdin.flush()?;

    // Read the response
    let reader = BufReader::new(stdout);
    let mut lines = reader.lines();
    
    if let Some(line) = lines.next() {
        let response: Value = serde_json::from_str(&line?)?;
        println!("✅ Server initialized: {}", response);
    }

    // List available tools
    let list_tools_request = json!({
        "jsonrpc": "2.0",
        "id": 2,
        "method": "tools/list"
    });

    writeln!(stdin, "{}", list_tools_request)?;
    stdin.flush()?;

    if let Some(line) = lines.next() {
        let response: Value = serde_json::from_str(&line?)?;
        println!("🔧 Available tools: {}", response);
    }

    // Test template generation
    let generate_template_request = json!({
        "jsonrpc": "2.0",
        "id": 3,
        "method": "tools/call",
        "params": {
            "name": "ai_generate_template",
            "arguments": {
                "description": "Create a user authentication service",
                "examples": ["Include JWT tokens", "Support password hashing", "Add role-based access control"],
                "language": "Rust",
                "framework": "Axum"
            }
        }
    });

    writeln!(stdin, "{}", generate_template_request)?;
    stdin.flush()?;

    if let Some(line) = lines.next() {
        let response: Value = serde_json::from_str(&line?)?;
        println!("📝 Generated template: {}", response);
    }

    // Test SPARQL generation
    let generate_sparql_request = json!({
        "jsonrpc": "2.0",
        "id": 4,
        "method": "tools/call",
        "params": {
            "name": "ai_generate_sparql",
            "arguments": {
                "intent": "Find all users who have made purchases in the last 30 days",
                "graph": "http://example.com/ecommerce"
            }
        }
    });

    writeln!(stdin, "{}", generate_sparql_request)?;
    stdin.flush()?;

    if let Some(line) = lines.next() {
        let response: Value = serde_json::from_str(&line?)?;
        println!("🔍 Generated SPARQL: {}", response);
    }

    // Clean up
    let _ = server.kill();
    println!("\n🎉 MCP server test completed!");

    Ok(())
}

