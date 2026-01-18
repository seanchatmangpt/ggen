//! Example: Retrieve pattern for RAG
//!
//! Demonstrates basic retrieval-augmented generation with vector search.

use ggen_dspy::{Module, Retrieve, InMemoryRetriever};
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    // Initialize logging
    tracing_subscriber::fmt::init();

    println!("=== Retrieve Pattern Example ===\n");

    // Create a knowledge base
    let documents = vec![
        "Rust is a systems programming language that runs blazingly fast.".to_string(),
        "Rust prevents segfaults and guarantees thread safety.".to_string(),
        "Rust has zero-cost abstractions and no garbage collector.".to_string(),
        "Python is an interpreted, high-level programming language.".to_string(),
        "Python emphasizes code readability with significant indentation.".to_string(),
    ];

    println!("Knowledge base has {} documents", documents.len());

    // Create retriever backend
    let backend = Arc::new(InMemoryRetriever::new(documents));

    // Create Retrieve module
    let retrieve = Retrieve::new(backend, 3);

    // Query 1: About Rust
    println!("\n--- Query 1: What is Rust? ---");
    let inputs = vec![("query", "What is Rust programming language?")];
    let output = retrieve.forward(&inputs).await?;

    let passages = output.get("passages")?;
    let num_passages = output.get("num_passages")?;

    println!("Retrieved {} passages:", num_passages);
    println!("{}", passages);

    // Query 2: About Python
    println!("\n--- Query 2: What is Python? ---");
    let inputs = vec![("query", "Python programming language features")];
    let output = retrieve.forward(&inputs).await?;

    let passages = output.get("passages")?;
    println!("Retrieved passages:");
    println!("{}", passages);

    // Query 3: Performance comparison
    println!("\n--- Query 3: Performance comparison ---");
    let inputs = vec![("query", "fast performance systems programming")];
    let output = retrieve.forward(&inputs).await?;

    let passages = output.get("passages")?;
    println!("Retrieved passages:");
    println!("{}", passages);

    println!("\n=== Example Complete ===");

    Ok(())
}
