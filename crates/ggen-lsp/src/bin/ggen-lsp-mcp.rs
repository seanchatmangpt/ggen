//! Run the ggen-lsp repair-route MCP server over stdio.

use ggen_lsp::mcp::RepairRouteServer;
use rmcp::ServiceExt;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber_init();
    let (stdin, stdout) = (tokio::io::stdin(), tokio::io::stdout());
    let running = RepairRouteServer::new().serve((stdin, stdout)).await?;
    running.waiting().await?;
    Ok(())
}

fn tracing_subscriber_init() {
    // Best-effort; ignore if a global subscriber is already set.
    let _ = tracing::subscriber::set_global_default(tracing::subscriber::NoSubscriber::default());
}
