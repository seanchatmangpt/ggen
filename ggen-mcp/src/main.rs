use anyhow::Result;
use rmcp::ServerBuilder;
use tracing_subscriber;

mod error;
mod schema;
mod server;
mod tools;

use server::GgenMcpServer;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .with_target(false)
        .init();

    tracing::info!("Starting ggen MCP server");

    // Create server instance
    let server = GgenMcpServer::new();

    // Build and run MCP server with stdio transport
    ServerBuilder::new(server)
        .stdio()
        .serve()
        .await?;

    Ok(())
}
