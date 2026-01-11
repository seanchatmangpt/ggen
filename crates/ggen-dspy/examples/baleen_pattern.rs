//! Example: SimplifiedBaleen pattern
//!
//! Demonstrates multi-hop reasoning with retrieval refinement.

use ggen_dspy::{Module, SimplifiedBaleen, BaleenConfig, InMemoryRetriever};
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt::init();

    println!("=== SimplifiedBaleen Pattern Example ===\n");

    // Create a comprehensive knowledge base
    let documents = vec![
        // Rust basics
        "Rust is a systems programming language focused on safety and performance.".to_string(),
        "Rust's ownership system prevents common memory bugs without garbage collection.".to_string(),
        "Rust provides zero-cost abstractions and minimal runtime overhead.".to_string(),

        // Rust features
        "Rust has algebraic data types with pattern matching.".to_string(),
        "Rust's type system prevents data races at compile time.".to_string(),
        "Rust has excellent support for concurrent programming.".to_string(),

        // Rust ecosystem
        "Cargo is Rust's build system and package manager.".to_string(),
        "Rust has a growing ecosystem of libraries called crates.".to_string(),
        "The Rust compiler (rustc) provides helpful error messages.".to_string(),

        // Rust history
        "Rust was created by Mozilla Research and first released in 2015.".to_string(),
        "Rust has been voted most loved programming language multiple years.".to_string(),
    ];

    println!("Knowledge base has {} documents", documents.len());

    // Create retriever backend
    let backend = Arc::new(InMemoryRetriever::new(documents));

    // Configure Baleen with 2 hops
    let config = BaleenConfig {
        max_hops: 2,
        k: 3,
        use_chain_of_thought: true,
    };

    let baleen = SimplifiedBaleen::new(backend, config);

    // Complex question requiring refinement
    println!("\n--- Question: What makes Rust unique for systems programming? ---\n");

    let inputs = vec![("question", "What makes Rust unique for systems programming?")];
    let output = baleen.forward(&inputs).await?;

    let answer = output.get("answer")?;
    let question = output.get("question")?;
    let num_hops = output.get("num_hops")?;
    let context = output.get("context")?;

    println!("Question: {}", question);
    println!("Number of hops: {}", num_hops);
    println!("\nAnswer: {}", answer);

    // Parse hop details
    let hops_json = output.get("hops")?;
    let hops: Vec<serde_json::Value> = serde_json::from_str(hops_json)?;

    println!("\n--- Reasoning Trace ---");
    for (i, hop) in hops.iter().enumerate() {
        let hop_num = hop["hop_num"].as_u64().unwrap_or(0);
        let query = hop["query"].as_str().unwrap_or("");
        let reasoning = hop["reasoning"].as_str().unwrap_or("");
        let passages = hop["passages"].as_array().map(|a| a.len()).unwrap_or(0);

        println!("\nHop {}: {}", hop_num, query);
        println!("Retrieved {} passages", passages);
        println!("Reasoning: {}", reasoning);
    }

    println!("\n--- Aggregated Context ---");
    println!("{}", context);

    println!("\n=== Example Complete ===");

    Ok(())
}
