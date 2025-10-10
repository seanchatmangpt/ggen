use anyhow::Result;
use rmcp::{transport::stdio, ServiceExt};
use tracing_subscriber;

mod error;
mod schema;
mod server;
mod tools;
mod utils;

use server::GgenMcpServer;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing to stderr (stdout is used for MCP protocol)
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .with_target(false)
        .with_writer(std::io::stderr)
        .init();

    tracing::info!("Starting ggen MCP server");

    // Create server instance and start stdio transport
    let server = GgenMcpServer::new();

    tracing::info!("GGen MCP Server initialized, serving on stdio");

    let service = server.serve(stdio()).await?;
    service.waiting().await?;

    Ok(())
}
