//! Knowledge Hooks example
//!
//! Demonstrates using RDF ontologies for semantic understanding.

use anyhow::Result;
use chatman_cli::{ChatManager, Config, KnowledgeHook, Message};

#[tokio::main]
async fn main() -> Result<()> {
    tracing_subscriber::fmt::init();

    let mut manager = ChatManager::new(Config::default())?;

    // Add knowledge hook from ontology
    let hook = KnowledgeHook::from_ontology("rdf/ontology.ttl")?;
    manager.add_hook(hook)?;

    println!("ChatMan CLI - Knowledge Hooks Example\n");

    // Ask questions that leverage the ontology
    let questions = vec![
        "What conversation patterns do you support?",
        "Which AI providers are available?",
        "How do you handle message quality?",
    ];

    for question in questions {
        println!("User: {}", question);

        let response = manager.send_message(Message::user(question)).await?;

        println!("Assistant: {}\n", response.content);
    }

    Ok(())
}
