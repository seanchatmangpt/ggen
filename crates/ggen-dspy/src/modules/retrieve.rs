//! Retrieve module - RAG with vector search integration
//!
//! Provides pluggable retrieval backends for vector databases and search engines.

use crate::{DspyError, Module, ModuleOutput, Result};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;

/// A retrieved passage with metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Passage {
    /// The text content
    pub text: String,

    /// Relevance score (0.0 to 1.0)
    pub score: f32,

    /// Additional metadata
    pub metadata: HashMap<String, serde_json::Value>,
}

impl Passage {
    /// Create a new passage
    pub fn new(text: impl Into<String>, score: f32) -> Self {
        Self {
            text: text.into(),
            score,
            metadata: HashMap::new(),
        }
    }

    /// Add metadata
    pub fn with_metadata(mut self, key: impl Into<String>, value: serde_json::Value) -> Self {
        self.metadata.insert(key.into(), value);
        self
    }
}

/// Trait for retrieval backends (vector databases, search engines, etc.)
#[async_trait]
pub trait RetrieverBackend: Send + Sync {
    /// Search for relevant passages
    async fn search(&self, query: &str, k: usize) -> Result<Vec<Passage>>;

    /// Get backend name
    fn name(&self) -> &str;

    /// Optional: Batch search for multiple queries
    async fn batch_search(&self, queries: &[String], k: usize) -> Result<Vec<Vec<Passage>>> {
        let mut results = Vec::with_capacity(queries.len());
        for query in queries {
            results.push(self.search(query, k).await?);
        }
        Ok(results)
    }
}

/// In-memory retriever for testing
pub struct InMemoryRetriever {
    documents: Vec<String>,
}

impl InMemoryRetriever {
    /// Create a new in-memory retriever
    pub fn new(documents: Vec<String>) -> Self {
        Self { documents }
    }

    /// Simple keyword-based scoring
    fn score(&self, query: &str, document: &str) -> f32 {
        let query_lower = query.to_lowercase();
        let doc_lower = document.to_lowercase();

        let query_words: Vec<_> = query_lower.split_whitespace().collect();
        let matches = query_words
            .iter()
            .filter(|word| doc_lower.contains(*word))
            .count();

        (matches as f32) / (query_words.len() as f32)
    }
}

#[async_trait]
impl RetrieverBackend for InMemoryRetriever {
    async fn search(&self, query: &str, k: usize) -> Result<Vec<Passage>> {
        let mut scored: Vec<_> = self
            .documents
            .iter()
            .map(|doc| {
                let score = self.score(query, doc);
                Passage::new(doc.clone(), score)
            })
            .collect();

        scored.sort_by(|a, b| b.score.partial_cmp(&a.score).unwrap());
        scored.truncate(k);

        Ok(scored)
    }

    fn name(&self) -> &str {
        "InMemoryRetriever"
    }
}

/// Retrieve module for RAG patterns
pub struct Retrieve {
    backend: Arc<dyn RetrieverBackend>,
    k: usize,
    name: String,
}

impl Retrieve {
    /// Create a new retrieve module
    pub fn new(backend: Arc<dyn RetrieverBackend>, k: usize) -> Self {
        Self {
            backend,
            k,
            name: "Retrieve".to_string(),
        }
    }

    /// Create with custom name
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = name.into();
        self
    }

    /// Get the backend
    pub fn backend(&self) -> &Arc<dyn RetrieverBackend> {
        &self.backend
    }

    /// Get k value
    pub fn k(&self) -> usize {
        self.k
    }
}

#[async_trait]
impl Module for Retrieve {
    async fn forward(&self, inputs: &[(&str, &str)]) -> Result<ModuleOutput> {
        // Extract query from inputs
        let query = inputs
            .iter()
            .find(|(key, _)| *key == "query" || *key == "question")
            .map(|(_, value)| *value)
            .ok_or_else(|| DspyError::MissingInput("query or question".to_string()))?;

        // Retrieve passages
        let passages = self.backend.search(query, self.k).await?;

        // Format output
        let mut output = ModuleOutput::new();

        // Join passages into context
        let context = passages
            .iter()
            .map(|p| &p.text)
            .cloned()
            .collect::<Vec<_>>()
            .join("\n\n");

        output.set("passages", context);
        output.set("num_passages", passages.len().to_string());

        // Add individual passages as JSON
        let passages_json =
            serde_json::to_string(&passages).map_err(|e| DspyError::SerializationError(e))?;
        output.set("passages_json", passages_json);

        Ok(output)
    }

    fn name(&self) -> &str {
        &self.name
    }
}

/// Builder for Retrieve module
pub struct RetrieveBuilder {
    backend: Option<Arc<dyn RetrieverBackend>>,
    k: usize,
    name: String,
}

impl RetrieveBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            backend: None,
            k: 3,
            name: "Retrieve".to_string(),
        }
    }

    /// Set the backend
    pub fn backend(mut self, backend: Arc<dyn RetrieverBackend>) -> Self {
        self.backend = Some(backend);
        self
    }

    /// Set k (number of passages to retrieve)
    pub fn k(mut self, k: usize) -> Self {
        self.k = k;
        self
    }

    /// Set the name
    pub fn name(mut self, name: impl Into<String>) -> Self {
        self.name = name.into();
        self
    }

    /// Build the Retrieve module
    pub fn build(self) -> Result<Retrieve> {
        let backend = self
            .backend
            .ok_or_else(|| DspyError::ConfigError("Retriever backend not set".to_string()))?;

        Ok(Retrieve {
            backend,
            k: self.k,
            name: self.name,
        })
    }
}

impl Default for RetrieveBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_passage_creation() {
        let passage = Passage::new("Test content", 0.95)
            .with_metadata("source", serde_json::json!("test.txt"));

        assert_eq!(passage.text, "Test content");
        assert_eq!(passage.score, 0.95);
        assert!(passage.metadata.contains_key("source"));
    }

    #[tokio::test]
    async fn test_in_memory_retriever() {
        let docs = vec![
            "Rust is a systems programming language".to_string(),
            "Python is good for data science".to_string(),
            "JavaScript runs in browsers".to_string(),
        ];

        let retriever = InMemoryRetriever::new(docs);
        let results = retriever.search("Rust programming", 2).await.unwrap();

        assert_eq!(results.len(), 2);
        assert!(results[0].text.contains("Rust"));
    }

    #[tokio::test]
    async fn test_retrieve_module() {
        let docs = vec![
            "The capital of France is Paris".to_string(),
            "Paris is known for the Eiffel Tower".to_string(),
            "London is the capital of England".to_string(),
        ];

        let backend = Arc::new(InMemoryRetriever::new(docs));
        let retrieve = Retrieve::new(backend, 2);

        let inputs = vec![("question", "What is the capital of France?")];
        let output = retrieve.forward(&inputs).await.unwrap();

        let passages = output.get("passages").unwrap();
        assert!(passages.contains("Paris"));
    }

    #[tokio::test]
    async fn test_batch_search() {
        let docs = vec!["Rust is fast".to_string(), "Python is easy".to_string()];

        let retriever = InMemoryRetriever::new(docs);
        let queries = vec!["Rust".to_string(), "Python".to_string()];

        let results = retriever.batch_search(&queries, 1).await.unwrap();
        assert_eq!(results.len(), 2);
        assert_eq!(results[0].len(), 1);
    }

    #[test]
    fn test_retrieve_builder() {
        let backend = Arc::new(InMemoryRetriever::new(vec![]));
        let retrieve = RetrieveBuilder::new()
            .backend(backend)
            .k(5)
            .name("TestRetriever")
            .build()
            .unwrap();

        assert_eq!(retrieve.k(), 5);
        assert_eq!(retrieve.name(), "TestRetriever");
    }
}
