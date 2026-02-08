//! Example: Multi-Hop Question Answering
//!
//! Demonstrates iterative retrieval for complex questions requiring
//! information from multiple sources.

use ggen_dspy::{InMemoryRetriever, Module, MultiHopConfig, MultiHopQA};
use std::sync::Arc;

#[tokio::main]
async fn main() -> Result<(), Box<dyn std::error::Error>> {
    tracing_subscriber::fmt::init();

    println!("=== Multi-Hop QA Pattern Example ===\n");

    // Create a knowledge base with interconnected facts
    let documents = vec![
        // About Paris
        "Paris is the capital and largest city of France.".to_string(),
        "The Eiffel Tower is located in Paris on the Champ de Mars.".to_string(),
        // About the Eiffel Tower
        "The Eiffel Tower was built in 1889 for the World's Fair.".to_string(),
        "The Eiffel Tower was designed by Gustave Eiffel's company.".to_string(),
        "The Eiffel Tower is 330 meters tall and was the world's tallest structure until 1930."
            .to_string(),
        // About France
        "France is a country in Western Europe.".to_string(),
        "France has a population of about 67 million people.".to_string(),
        // About Gustave Eiffel
        "Gustave Eiffel was a French civil engineer and architect.".to_string(),
        "Eiffel also designed the internal structure of the Statue of Liberty.".to_string(),
    ];

    println!("Knowledge base has {} documents", documents.len());

    // Create retriever backend
    let backend = Arc::new(InMemoryRetriever::new(documents));

    // Configure multi-hop QA with 3 hops
    let config = MultiHopConfig {
        max_hops: 3,
        passages_per_hop: 2,
        enable_reasoning: true,
    };

    let multihop = MultiHopQA::new(backend, config);

    // Complex question requiring multiple hops
    println!("\n--- Question: Where is the Eiffel Tower and who designed it? ---\n");

    let inputs = vec![("question", "Where is the Eiffel Tower and who designed it?")];
    let output = multihop.forward(&inputs).await?;

    let context = output.get("context")?;
    let num_hops = output.get("num_hops")?;
    let question = output.get("question")?;

    println!("Question: {}", question);
    println!("Number of hops: {}", num_hops);
    println!("\nAggregated context:");
    println!("{}", context);

    // Parse hop details
    let hops_json = output.get("hops")?;
    let hops: Vec<serde_json::Value> = serde_json::from_str(hops_json)?;

    println!("\n--- Hop Details ---");
    for (i, hop) in hops.iter().enumerate() {
        println!("\nHop {}: {}", i + 1, hop["query"].as_str().unwrap_or(""));
        println!(
            "Passages retrieved: {}",
            hop["passages"].as_array().map(|a| a.len()).unwrap_or(0)
        );
    }

    println!("\n=== Example Complete ===");

    Ok(())
}
