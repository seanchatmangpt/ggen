//! Basic ChatMan CLI example
//!
//! This example demonstrates a simple chat interaction using ChatMan CLI.

use anyhow::Result;
use chatman_cli::{ChatManager, Config, Message};

#[tokio::main]
async fn main() -> Result<()> {
    // Initialize tracing
    tracing_subscriber::fmt::init();

    // Create default configuration
    let config = Config::default();

    // Create chat manager
    let mut manager = ChatManager::new(config)?;

    println!("ChatMan CLI - Basic Example\n");

    // Send some messages
    let messages = vec![
        "What is Rust?",
        "Tell me about async programming",
        "How do knowledge hooks work?",
    ];

    for question in messages {
        println!("User: {}", question);

        let response = manager
            .send_message(Message::user(question))
            .await?;

        println!("Assistant: {}\n", response.content);
    }

    // Export conversation
    let json = manager.export_json()?;
    println!("Conversation history:\n{}", json);

    Ok(())
}
