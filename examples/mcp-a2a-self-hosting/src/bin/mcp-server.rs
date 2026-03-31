// MCP Server Entry Point (rmcp 1.3.0)
// Auto-generated from ontology/agent.ttl
// DO NOT EDIT - This file is generated from RDF ontology

use rmcp::Server;
use mcp_a2a_self_hosting::McpHandler;
use std::env;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_env_filter(
            env::var("RUST_LOG")
                .unwrap_or_else(|_| "info,mcp_a2a_self_hosting=debug".to_string()),
        )
        .init();

    // Load ontology path from environment
    let ontology_path = env::var("ONTOLOGY_PATH")
        .unwrap_or_else(|_| "ontology/agent.ttl".to_string());

    // Create MCP handler
    let handler = McpHandler::new(ontology_path)?;

    // Create and start server
    let server = Server::new(handler)
        .with_name("ggen-codegen")
        .with_version("0.1.0");

    tracing::info!("Starting MCP server on stdio");
    tracing::info!("Tools: generate_code, validate_ontology, explain_schema");

    server.serve().await?;

    Ok(())
}
