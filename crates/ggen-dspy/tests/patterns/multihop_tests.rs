//! Tests for MultiHopQA pattern

use ggen_dspy::{InMemoryRetriever, Module, MultiHopConfig, MultiHopQA, MultiHopQABuilder};
use std::sync::Arc;

#[tokio::test]
async fn test_multihop_qa_basic() {
    let docs = vec![
        "Paris is the capital of France".to_string(),
        "The Eiffel Tower is located in Paris".to_string(),
        "France is a country in Europe".to_string(),
    ];

    let backend = Arc::new(InMemoryRetriever::new(docs));
    let multihop = MultiHopQA::with_backend(backend);

    let inputs = vec![("question", "Where is the Eiffel Tower?")];
    let output = multihop.forward(&inputs).await.unwrap();

    let context = output.get("context").unwrap();
    assert!(context.contains("Paris") || context.contains("Eiffel"));
}

#[tokio::test]
async fn test_multihop_qa_hops() {
    let docs = vec![
        "Rust was created by Mozilla".to_string(),
        "Mozilla is a technology company".to_string(),
        "Rust 1.0 was released in 2015".to_string(),
    ];

    let backend = Arc::new(InMemoryRetriever::new(docs));
    let config = MultiHopConfig {
        max_hops: 2,
        passages_per_hop: 2,
        enable_reasoning: true,
    };
    let multihop = MultiHopQA::new(backend, config);

    let inputs = vec![("question", "Who created Rust?")];
    let output = multihop.forward(&inputs).await.unwrap();

    let num_hops: usize = output.get("num_hops").unwrap().parse().unwrap();
    assert!(num_hops > 0 && num_hops <= 2);
}

#[tokio::test]
async fn test_multihop_qa_builder() {
    let docs = vec!["Test".to_string()];
    let backend = Arc::new(InMemoryRetriever::new(docs));

    let multihop = MultiHopQABuilder::new()
        .backend(backend)
        .max_hops(3)
        .passages_per_hop(5)
        .enable_reasoning(false)
        .name("TestMultiHop")
        .build()
        .unwrap();

    assert_eq!(multihop.name(), "TestMultiHop");
}

#[tokio::test]
async fn test_multihop_qa_empty_docs() {
    let backend = Arc::new(InMemoryRetriever::new(vec![]));
    let multihop = MultiHopQA::with_backend(backend);

    let inputs = vec![("question", "What is the answer?")];
    let output = multihop.forward(&inputs).await.unwrap();

    // Should complete even with no documents
    let num_hops: usize = output.get("num_hops").unwrap().parse().unwrap();
    assert!(num_hops > 0);
}

#[tokio::test]
async fn test_multihop_qa_missing_question() {
    let backend = Arc::new(InMemoryRetriever::new(vec![]));
    let multihop = MultiHopQA::with_backend(backend);

    let inputs = vec![]; // No question
    let result = multihop.forward(&inputs).await;

    assert!(result.is_err());
}
