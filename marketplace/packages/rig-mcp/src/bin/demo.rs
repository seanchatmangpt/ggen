//! Rig MCP Integration Demo
//!
//! Demo showcasing MCP transport capabilities.

use anyhow::Result;
use rig_mcp_integration::prelude::*;
use serde_json::json;

#[tokio::main]
async fn main() -> Result<()> {
    println!("Rig MCP Integration Demo");
    println!("=======================\n");

    // Create configuration with server
    let config = Config {
        mcp_servers: vec![ServerConfig {
            url: "http://localhost:3000/mcp".to_string(),
            name: "demo-server".to_string(),
            transport_type: "http".to_string(),
        }],
        transport: None,
        discovery: None,
    };

    // Create MCP client
    let mut client = McpClient::new(config);
    println!("Created MCP client");

    // List servers
    println!("\nConfigured servers:");
    for server in client.servers() {
        println!("  - {} ({})", server.name, server.url);
        println!("    Transport: {}", server.transport_type);
    }

    // Demonstrate schema extraction
    println!("\nSchema extraction demo:");
    let tool_schema = ToolSchema::new("demo_tool", "A demonstration tool")
        .with_tag("demo")
        .with_tag("example");

    println!("  Tool name: {}", tool_schema.name);
    println!("  Description: {}", tool_schema.description);
    println!("  Tags: {:?}", tool_schema.tags);
    println!("  Complexity: {}", tool_schema.complexity());

    // Demonstrate parameter schema
    println!("\nParameter schema demo:");
    let param_schema = ParameterSchema::new("object")
        .with_description("Demo parameters")
        .with_property("name", ParameterSchema::new("string"))
        .with_property("count", ParameterSchema::new("integer"))
        .with_required(vec!["name".to_string()]);

    println!("  Schema type: {}", param_schema.schema_type);
    println!(
        "  Has 'name' property: {}",
        param_schema
            .properties
            .as_ref()
            .map_or(false, |p| p.contains_key("name"))
    );
    println!("  'name' is required: {}", param_schema.is_required("name"));

    println!("\nDemo completed successfully!");

    Ok(())
}
