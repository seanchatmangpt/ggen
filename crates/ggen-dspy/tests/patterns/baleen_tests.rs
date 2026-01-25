//! Tests for SimplifiedBaleen pattern

use ggen_dspy::{BaleenBuilder, BaleenConfig, InMemoryRetriever, Module, SimplifiedBaleen};
use std::sync::Arc;

#[tokio::test]
async fn test_baleen_basic() {
    let docs = vec![
        "Rust provides memory safety".to_string(),
        "Rust has zero-cost abstractions".to_string(),
        "Rust was created by Mozilla Research".to_string(),
    ];

    let backend = Arc::new(InMemoryRetriever::new(docs));
    let baleen = SimplifiedBaleen::with_backend(backend);

    let inputs = vec![("question", "What is Rust?")];
    let output = baleen.forward(&inputs).await.unwrap();

    let answer = output.get("answer").unwrap();
    assert!(!answer.is_empty());
}

#[tokio::test]
async fn test_baleen_multiple_hops() {
    let docs = vec![
        "Python is interpreted".to_string(),
        "Python has dynamic typing".to_string(),
        "Python was created by Guido van Rossum".to_string(),
    ];

    let backend = Arc::new(InMemoryRetriever::new(docs));
    let config = BaleenConfig {
        max_hops: 2,
        k: 2,
        use_chain_of_thought: true,
    };
    let baleen = SimplifiedBaleen::new(backend, config);

    let inputs = vec![("question", "Tell me about Python")];
    let output = baleen.forward(&inputs).await.unwrap();

    let num_hops: usize = output.get("num_hops").unwrap().parse().unwrap();
    assert_eq!(num_hops, 2);
}

#[tokio::test]
async fn test_baleen_builder() {
    let docs = vec!["Test".to_string()];
    let backend = Arc::new(InMemoryRetriever::new(docs));

    let baleen = BaleenBuilder::new()
        .backend(backend)
        .max_hops(3)
        .k(4)
        .use_chain_of_thought(false)
        .name("TestBaleen")
        .build()
        .unwrap();

    assert_eq!(baleen.name(), "TestBaleen");
}

#[tokio::test]
async fn test_baleen_context_aggregation() {
    let docs = vec![
        "First fact".to_string(),
        "Second fact".to_string(),
        "Third fact".to_string(),
    ];

    let backend = Arc::new(InMemoryRetriever::new(docs));
    let baleen = SimplifiedBaleen::with_backend(backend);

    let inputs = vec![("question", "What are the facts?")];
    let output = baleen.forward(&inputs).await.unwrap();

    let context = output.get("context").unwrap();
    // Should contain multiple passages
    assert!(context.contains("fact"));
}

#[tokio::test]
async fn test_baleen_missing_question() {
    let backend = Arc::new(InMemoryRetriever::new(vec![]));
    let baleen = SimplifiedBaleen::with_backend(backend);

    let inputs = vec![]; // No question
    let result = baleen.forward(&inputs).await;

    assert!(result.is_err());
}
