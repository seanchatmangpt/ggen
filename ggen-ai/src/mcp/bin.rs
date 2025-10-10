//! MCP server binary for ggen-ai

use anyhow::Result;
use rmcp::ServerBuilder;
use tracing_subscriber;

use ggen_ai::mcp::GgenAiMcpServer;

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt()
        .with_max_level(tracing::Level::INFO)
        .with_target(false)
        .init();

    tracing::info!("Starting ggen-ai MCP server");

    // Get configuration from environment
    let openai_key = std::env::var("OPENAI_API_KEY").ok();
    let anthropic_key = std::env::var("ANTHROPIC_API_KEY").ok();
    let use_ollama = std::env::var("USE_OLLAMA").unwrap_or_default() == "true";

    // Create server with appropriate client
    let server = if let Some(key) = openai_key {
        tracing::info!("Using OpenAI client");
        GgenAiMcpServer::new().with_openai(key)
    } else if let Some(key) = anthropic_key {
        tracing::info!("Using Anthropic client");
        GgenAiMcpServer::new().with_anthropic(key)
    } else if use_ollama {
        tracing::info!("Using Ollama client");
        GgenAiMcpServer::new().with_ollama()
    } else {
        tracing::warn!("No LLM client configured, using mock client");
        GgenAiMcpServer::new()
    };

    // Build and run MCP server with stdio transport
    ServerBuilder::new(server)
        .stdio()
        .serve()
        .await?;

    Ok(())
}
