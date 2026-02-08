//! Example: Composing multiple patterns
//!
//! Demonstrates how to combine Retrieve, MultiHopQA, and SimplifiedBaleen
//! for advanced reasoning pipelines.

use ggen_dspy::{
    BaleenConfig, InMemoryRetriever, Module, MultiHopConfig, MultiHopQA, Retrieve, SimplifiedBaleen,
};
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt::init();

    println!("=== Pattern Composition Example ===\n");

    // Create a rich knowledge base
    let documents = vec![
        // Programming languages
        "Rust is known for memory safety without garbage collection.".to_string(),
        "Python is popular for data science and machine learning.".to_string(),
        "JavaScript is the language of the web.".to_string(),
        // Language features
        "Rust has ownership system that prevents memory bugs.".to_string(),
        "Python has dynamic typing and is interpreted.".to_string(),
        "JavaScript supports both functional and object-oriented programming.".to_string(),
        // Performance
        "Rust provides performance comparable to C and C++.".to_string(),
        "Python is slower than compiled languages but faster to develop in.".to_string(),
        "JavaScript performance has improved significantly with modern engines.".to_string(),
        // Use cases
        "Rust is used for systems programming, embedded devices, and WebAssembly.".to_string(),
        "Python is used for web development, data analysis, and AI.".to_string(),
        "JavaScript is used for frontend, backend (Node.js), and mobile apps.".to_string(),
    ];

    println!("Knowledge base has {} documents\n", documents.len());

    let backend = Arc::new(InMemoryRetriever::new(documents));

    // === Pattern 1: Simple Retrieval ===
    println!("=== Pattern 1: Simple Retrieval ===\n");

    let retrieve = Retrieve::new(backend.clone(), 3);
    let inputs = vec![("query", "fast systems programming")];
    let output = retrieve.forward(&inputs).await?;

    println!("Query: fast systems programming");
    println!("Retrieved passages:");
    println!("{}\n", output.get("passages")?);

    // === Pattern 2: Multi-Hop QA ===
    println!("=== Pattern 2: Multi-Hop Question Answering ===\n");

    let config = MultiHopConfig {
        max_hops: 2,
        passages_per_hop: 3,
        enable_reasoning: true,
    };
    let multihop = MultiHopQA::new(backend.clone(), config);

    let question = "Compare Rust and Python for performance and use cases";
    let inputs = vec![("question", question)];
    let output = multihop.forward(&inputs).await?;

    println!("Question: {}", question);
    println!("Number of hops: {}", output.get("num_hops")?);
    println!("Context gathered:");
    println!("{}\n", output.get("context")?);

    // === Pattern 3: SimplifiedBaleen ===
    println!("=== Pattern 3: SimplifiedBaleen with Reasoning ===\n");

    let config = BaleenConfig {
        max_hops: 2,
        k: 3,
        use_chain_of_thought: true,
    };
    let baleen = SimplifiedBaleen::new(backend.clone(), config);

    let question = "What makes each programming language unique?";
    let inputs = vec![("question", question)];
    let output = baleen.forward(&inputs).await?;

    println!("Question: {}", question);
    println!("Answer: {}", output.get("answer")?);
    println!("Number of hops: {}", output.get("num_hops")?);

    // Parse hop details
    let hops_json = output.get("hops")?;
    let hops: Vec<serde_json::Value> = serde_json::from_str(hops_json)?;

    println!("\nReasoning trace:");
    for hop in hops.iter() {
        let reasoning = hop["reasoning"].as_str().unwrap_or("");
        println!("- {}", reasoning);
    }

    // === Parallel Execution ===
    println!("\n=== Pattern 4: Parallel Execution ===\n");

    println!("Running all three patterns in parallel...");

    let retrieve = Retrieve::new(backend.clone(), 2);
    let multihop = MultiHopQA::with_backend(backend.clone());
    let baleen = SimplifiedBaleen::with_backend(backend);

    let query = "programming language features";

    let (retrieve_result, multihop_result, baleen_result) = tokio::join!(
        retrieve.forward(&[("query", query)]),
        multihop.forward(&[("question", query)]),
        baleen.forward(&[("question", query)])
    );

    println!("All patterns completed!");
    println!(
        "- Retrieve: {}",
        if retrieve_result.is_ok() {
            "✓"
        } else {
            "✗"
        }
    );
    println!(
        "- MultiHopQA: {}",
        if multihop_result.is_ok() {
            "✓"
        } else {
            "✗"
        }
    );
    println!(
        "- SimplifiedBaleen: {}",
        if baleen_result.is_ok() { "✓" } else { "✗" }
    );

    println!("\n=== Example Complete ===");

    Ok(())
}
