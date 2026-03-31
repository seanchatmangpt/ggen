// A2A HTTP Server Entry Point
// Auto-generated from ontology/agent.ttl
// DO NOT EDIT - This file is generated from RDF ontology

use a2a_rs::server::HttpServer;
use a2a_rs::auth::BearerTokenAuthenticator;
use a2a_rs::agent::SimpleAgentInfo;
use mcp_a2a_self_hosting::GgenAgentHandler;
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

    // Load configuration
    let agent_token = env::var("AGENT_TOKEN").unwrap_or_else(|_| "dev-token".to_string());
    let server_host = env::var("A2A_HOST").unwrap_or_else(|_| "127.0.0.1".to_string());
    let server_port = env::var("A2A_PORT")
        .unwrap_or_else(|_| "8080".to_string())
        .parse::<u16>()?;

    // Create agent handler
    let handler = GgenAgentHandler::new();

    // Create agent info
    let agent_info = SimpleAgentInfo::new(
        "CodeGeneratorAgent",
        "1.0.0",
        "AI-powered code generation agent for RDF ontologies",
    );

    // Create authenticator
    let authenticator = BearerTokenAuthenticator::new(agent_token);

    // Create and start server
    let server = HttpServer::new(server_host, server_port, handler, agent_info, authenticator);

    tracing::info!("Starting A2A server on http://{}:{}", server_host, server_port);
    tracing::info!("Agent card available at http://{}:{}/.well-known/agent.json", server_host, server_port);

    server.serve().await?;

    Ok(())
}
