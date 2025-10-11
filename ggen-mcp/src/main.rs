use anyhow::Result;
use ggen_mcp::GgenMcpServer;
use rmcp::{transport::stdio, ServiceExt};
use tracing_subscriber;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing to stderr (stdout is used for MCP protocol)
    // Use environment variable GGEN_MCP_LOG for log level control
    let log_level = std::env::var("GGEN_MCP_LOG")
        .ok()
        .and_then(|l| l.parse::<tracing::Level>().ok())
        .unwrap_or(tracing::Level::INFO);

    tracing_subscriber::fmt()
        .with_max_level(log_level)
        .with_target(true)
        .with_thread_ids(true)
        .with_line_number(true)
        .with_writer(std::io::stderr)
        .init();

    tracing::info!("Starting ggen MCP server v{}", env!("CARGO_PKG_VERSION"));
    tracing::info!("Log level: {}", log_level);

    // Create server instance and start stdio transport
    let server = GgenMcpServer::new();

    tracing::info!("GGen MCP Server initialized");
    tracing::info!("Serving on stdio transport (MCP protocol)");

    let service = server.serve(stdio()).await?;

    tracing::info!("Service started, waiting for requests...");
    service.waiting().await?;

    tracing::info!("GGen MCP Server shutting down");
    Ok(())
}
