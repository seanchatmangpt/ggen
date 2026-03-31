// A2A HTTP Server -- refactored to AgentBuilder API
use a2a_agents::core::AgentBuilder;
use mcp_a2a_self_hosting::GgenAgentHandler;

#[tokio::main]
async fn main() -> anyhow::Result<()> {
    tracing_subscriber::fmt()
        .with_env_filter(
            std::env::var("RUST_LOG")
                .unwrap_or_else(|_| "info,mcp_a2a_self_hosting=debug".to_string()),
        )
        .init();

    let handler = GgenAgentHandler::new();

    AgentBuilder::from_file("a2a-server.toml")?
        .with_handler(handler)
        .build_with_auto_storage()
        .await?
        .run()
        .await?;

    Ok(())
}
