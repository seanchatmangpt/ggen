//! Batch processing example
//!
//! Demonstrates processing multiple conversations in batch.

use anyhow::Result;
use chatman_cli::{ChatManager, Config, Message};

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt::init();

    println!("ChatMan CLI - Batch Processing Example\n");

    let questions = vec![
        "What is async/await?",
        "Explain tokio runtime",
        "How does error handling work?",
        "What are traits?",
        "Explain lifetimes",
    ];

    let config = Config::default();

    for (i, question) in questions.iter().enumerate() {
        let mut manager = ChatManager::new(config.clone())?;

        println!("Batch {}: {}", i + 1, question);

        let response = manager.send_message(Message::user(*question)).await?;

        println!("Response: {}\n", response.content);
    }

    Ok(())
}
