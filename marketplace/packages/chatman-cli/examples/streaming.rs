//! Streaming example
//!
//! Demonstrates real-time streaming responses (placeholder).

use anyhow::Result;
use chatman_cli::{ChatManager, Config, Message};

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt::init();

    let config = Config {
        provider: "openai".to_string(),
        model: "gpt-4".to_string(),
        ..Default::default()
    };

    let mut manager = ChatManager::new(config)?;

    println!("ChatMan CLI - Streaming Example\n");

    // In a real implementation, this would stream tokens
    let response = manager
        .send_message(Message::user("Tell me a story about Rust"))
        .await?;

    println!("Streamed response: {}", response.content);

    Ok(())
}
