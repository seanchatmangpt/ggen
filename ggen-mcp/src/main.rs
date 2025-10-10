use anyhow::Result;
use tracing_subscriber;

mod error;
mod schema;
mod server;
mod tools;
mod utils;

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
    let _server = GgenMcpServer::new();

    // TODO: Implement actual server startup
    tracing::info!("GGen MCP Server initialized");
    
    // Keep the server running
    tokio::signal::ctrl_c().await?;
    tracing::info!("Shutting down GGen MCP Server");

    Ok(())
}
