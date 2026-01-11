//! Tests for pattern composition

use ggen_dspy::{
    Module, Retrieve, MultiHopQA, SimplifiedBaleen, InMemoryRetriever,
};
use std::sync::Arc;

#[tokio::test]
async fn test_compose_retrieve_and_multihop() {
    let docs = vec![
        "Rust is fast".to_string(),
        "Rust is safe".to_string(),
        "Rust is concurrent".to_string(),
    ];

    let backend = Arc::new(InMemoryRetriever::new(docs));

    // First use Retrieve
    let retrieve = Retrieve::new(backend.clone(), 2);
    let inputs = vec![("query", "Rust features")];
    let retrieve_output = retrieve.forward(&inputs).await.unwrap();

    // Then use MultiHopQA
    let multihop = MultiHopQA::with_backend(backend);
    let multihop_inputs = vec![("question", "What is Rust?")];
    let multihop_output = multihop.forward(&multihop_inputs).await.unwrap();

    // Both should work independently
    assert!(retrieve_output.get("passages").is_ok());
    assert!(multihop_output.get("context").is_ok());
}

#[tokio::test]
async fn test_compose_multihop_and_baleen() {
    let docs = vec![
        "Python is easy to learn".to_string(),
        "Python has many libraries".to_string(),
    ];

    let backend = Arc::new(InMemoryRetriever::new(docs));

    // Use MultiHopQA
    let multihop = MultiHopQA::with_backend(backend.clone());
    let inputs = vec![("question", "Why use Python?")];
    let multihop_output = multihop.forward(&inputs).await.unwrap();

    // Use SimplifiedBaleen
    let baleen = SimplifiedBaleen::with_backend(backend);
    let baleen_output = baleen.forward(&inputs).await.unwrap();

    // Both patterns should work on same data
    assert!(multihop_output.get("context").is_ok());
    assert!(baleen_output.get("answer").is_ok());
}

#[tokio::test]
async fn test_pattern_reusability() {
    let docs = vec!["Test document".to_string()];
    let backend = Arc::new(InMemoryRetriever::new(docs));

    let retrieve = Retrieve::new(backend, 1);

    // Use the same module multiple times
    let inputs1 = vec![("query", "Test 1")];
    let output1 = retrieve.forward(&inputs1).await.unwrap();

    let inputs2 = vec![("query", "Test 2")];
    let output2 = retrieve.forward(&inputs2).await.unwrap();

    // Both calls should succeed
    assert!(output1.get("passages").is_ok());
    assert!(output2.get("passages").is_ok());
}

#[tokio::test]
async fn test_pattern_parallel_execution() {
    let docs = vec![
        "Document A".to_string(),
        "Document B".to_string(),
    ];

    let backend = Arc::new(InMemoryRetriever::new(docs));

    let retrieve1 = Retrieve::new(backend.clone(), 1);
    let retrieve2 = Retrieve::new(backend, 1);

    // Execute in parallel
    let (result1, result2) = tokio::join!(
        retrieve1.forward(&[("query", "A")]),
        retrieve2.forward(&[("query", "B")])
    );

    assert!(result1.is_ok());
    assert!(result2.is_ok());
}
