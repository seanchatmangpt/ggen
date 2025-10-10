//! MCP server binary for ggen-ai

use anyhow::Result;
use rmcp::serve_server;
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

    // Get configuration from environment with proper defaults
    let openai_key = std::env::var("OPENAI_API_KEY").ok();
    let anthropic_key = std::env::var("ANTHROPIC_API_KEY").ok();

    // Parse USE_OLLAMA with proper boolean handling
    let use_ollama = std::env::var("USE_OLLAMA")
        .map(|v| v.to_lowercase() == "true" || v == "1" || v == "yes")
        .unwrap_or(false);

    // Get Ollama model with sensible default
    let ollama_model = std::env::var("OLLAMA_MODEL")
        .unwrap_or_else(|_| {
            tracing::debug!("OLLAMA_MODEL not set, using default: qwen3-coder:30b");
            "qwen3-coder:30b".to_string()
        });

    // Create server with appropriate client (default to Ollama with qwen3-coder:30b)
    let server = if let Some(key) = openai_key {
        tracing::info!("Using OpenAI client");
        GgenAiMcpServer::new().with_openai(key)
    } else if let Some(key) = anthropic_key {
        tracing::info!("Using Anthropic client");
        GgenAiMcpServer::new().with_anthropic(key)
    } else if use_ollama || (std::env::var("OPENAI_API_KEY").is_err() && std::env::var("ANTHROPIC_API_KEY").is_err()) {
        tracing::info!("Using Ollama client with model: {}", ollama_model);
        GgenAiMcpServer::new().with_ollama_model(&ollama_model)
    } else {
        tracing::warn!("No LLM client configured, using mock client");
        GgenAiMcpServer::new()
    };

    // Build and run MCP server with stdio transport
    serve_server(server, rmcp::transport::stdio()).await?;

    Ok(())
}

