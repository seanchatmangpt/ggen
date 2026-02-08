//! Rig MCP Integration Example
//!
//! Simple example demonstrating the MCP transport layer.

use anyhow::Result;
use rig_mcp_integration::prelude::*;

#[tokio::main]
async fn main() -> Result<()> {
    println!("Rig MCP Integration Example");
    println!("===========================\n");

    // Create a simple configuration
    let config = Config {
        mcp_servers: vec![],
        transport: None,
        discovery: None,
    };

    // Create MCP client
    let client = McpClient::new(config);
    println!("Created MCP client with {} servers", client.servers().len());

    // Demonstrate transport type parsing
    println!("\nTransport type parsing:");
    println!("  stdio -> {:?}", TransportType::from_str("stdio"));
    println!("  http -> {:?}", TransportType::from_str("http"));
    println!("  sse -> {:?}", TransportType::from_str("sse"));

    println!("\nExample completed successfully!");

    Ok(())
}
