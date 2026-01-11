//! Tests for Retrieve pattern

use ggen_dspy::{
    Module, Retrieve, RetrieverBackend, InMemoryRetriever, Passage, RetrieveBuilder,
};
use std::sync::Arc;

#[tokio::test]
async fn test_retrieve_basic() {
    let docs = vec![
        "Rust is a systems programming language".to_string(),
        "Python is great for data science".to_string(),
        "JavaScript runs in browsers".to_string(),
    ];

    let backend = Arc::new(InMemoryRetriever::new(docs));
    let retrieve = Retrieve::new(backend, 2);

    let inputs = vec![("query", "Rust programming")];
    let output = retrieve.forward(&inputs).await.unwrap();

    let passages = output.get("passages").unwrap();
    assert!(passages.contains("Rust"));
}

#[tokio::test]
async fn test_retrieve_empty_query() {
    let backend = Arc::new(InMemoryRetriever::new(vec![]));
    let retrieve = Retrieve::new(backend, 5);

    let inputs = vec![]; // Missing query
    let result = retrieve.forward(&inputs).await;

    assert!(result.is_err());
}

#[tokio::test]
async fn test_retrieve_with_k() {
    let docs = vec![
        "Document 1".to_string(),
        "Document 2".to_string(),
        "Document 3".to_string(),
    ];

    let backend = Arc::new(InMemoryRetriever::new(docs));
    let retrieve = Retrieve::new(backend, 2);

    let inputs = vec![("query", "Document")];
    let output = retrieve.forward(&inputs).await.unwrap();

    let num_passages: usize = output.get("num_passages").unwrap().parse().unwrap();
    assert!(num_passages <= 2);
}

#[tokio::test]
async fn test_retrieve_builder() {
    let backend = Arc::new(InMemoryRetriever::new(vec!["Test".to_string()]));

    let retrieve = RetrieveBuilder::new()
        .backend(backend)
        .k(5)
        .name("CustomRetriever")
        .build()
        .unwrap();

    assert_eq!(retrieve.k(), 5);
    assert_eq!(retrieve.name(), "CustomRetriever");
}

#[tokio::test]
async fn test_retrieve_passages_json() {
    let docs = vec!["Test document".to_string()];
    let backend = Arc::new(InMemoryRetriever::new(docs));
    let retrieve = Retrieve::new(backend, 1);

    let inputs = vec![("query", "Test")];
    let output = retrieve.forward(&inputs).await.unwrap();

    let passages_json = output.get("passages_json").unwrap();
    let passages: Vec<Passage> = serde_json::from_str(passages_json).unwrap();
    assert!(!passages.is_empty());
}
