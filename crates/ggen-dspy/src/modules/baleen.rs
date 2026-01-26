//! SimplifiedBaleen pattern - Multi-hop reasoning with retrieval
//!
//! Named after Baleen whales that filter-feed, this pattern filters
//! and refines information across multiple retrieval hops.

use super::retrieve::{Retrieve, RetrieverBackend};
use crate::{DspyError, Module, ModuleOutput, Result};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// Baleen hop result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BaleenHop {
    /// Generated query for this hop
    pub query: String,

    /// Retrieved passages
    pub passages: Vec<String>,

    /// Reasoning trace
    pub reasoning: String,

    /// Hop number
    pub hop_num: usize,
}

/// SimplifiedBaleen configuration
#[derive(Debug, Clone)]
pub struct BaleenConfig {
    /// Number of retrieval hops
    pub max_hops: usize,

    /// Passages to retrieve per hop
    pub k: usize,

    /// Whether to use reasoning chains
    pub use_chain_of_thought: bool,
}

impl Default for BaleenConfig {
    fn default() -> Self {
        Self {
            max_hops: 2,
            k: 3,
            use_chain_of_thought: true,
        }
    }
}

/// SimplifiedBaleen module
///
/// Implements a simplified version of the Baleen pattern for multi-hop
/// question answering with iterative refinement.
pub struct SimplifiedBaleen {
    retrieve: Retrieve,
    config: BaleenConfig,
    name: String,
}

impl SimplifiedBaleen {
    /// Create a new SimplifiedBaleen module
    pub fn new(backend: Arc<dyn RetrieverBackend>, config: BaleenConfig) -> Self {
        let retrieve = Retrieve::new(backend, config.k);

        Self {
            retrieve,
            config,
            name: "SimplifiedBaleen".to_string(),
        }
    }

    /// Create with default configuration
    pub fn with_backend(backend: Arc<dyn RetrieverBackend>) -> Self {
        Self::new(backend, BaleenConfig::default())
    }

    /// Set custom name
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = name.into();
        self
    }

    /// Generate next query based on question and previous context
    async fn generate_next_query(
        &self, question: &str, previous_context: &[String], hop_num: usize,
    ) -> Result<String> {
        // TODO: Use LLM to generate refined queries
        // For now, use heuristic approach

        if hop_num == 0 {
            Ok(question.to_string())
        } else {
            // Generate query that builds on previous information
            let context_summary = if previous_context.is_empty() {
                String::new()
            } else {
                format!(" (context: {})", previous_context.join(", "))
            };

            Ok(format!("{}{}", question, context_summary))
        }
    }

    /// Extract reasoning from current state
    async fn extract_reasoning(
        &self, question: &str, passages: &[String], hop_num: usize,
    ) -> Result<String> {
        // TODO: Use LLM for reasoning
        // For now, return simple summary

        Ok(format!(
            "Hop {}: Found {} relevant passages for: {}",
            hop_num,
            passages.len(),
            question
        ))
    }

    /// Synthesize final answer from all hops
    async fn synthesize_answer(&self, _question: &str, hops: &[BaleenHop]) -> Result<String> {
        // TODO: Use LLM to synthesize answer
        // For now, return placeholder

        let total_passages: usize = hops.iter().map(|h| h.passages.len()).sum();
        Ok(format!(
            "Answer based on {} hops and {} total passages",
            hops.len(),
            total_passages
        ))
    }
}

#[async_trait]
impl Module for SimplifiedBaleen {
    async fn forward(&self, inputs: &[(&str, &str)]) -> Result<ModuleOutput> {
        // Extract question
        let question = inputs
            .iter()
            .find(|(key, _)| *key == "question")
            .map(|(_, value)| *value)
            .ok_or_else(|| DspyError::MissingInput("question".to_string()))?;

        let mut hops = Vec::new();
        let mut all_passages = Vec::new();

        // Multi-hop retrieval with reasoning
        for hop_num in 0..self.config.max_hops {
            // Generate query for this hop
            let query = self
                .generate_next_query(question, &all_passages, hop_num)
                .await?;

            // Retrieve passages
            let retrieve_inputs = vec![("query", query.as_str())];
            let retrieve_output = self.retrieve.forward(&retrieve_inputs).await?;

            let passages_str = retrieve_output.get("passages")?;
            let passages: Vec<String> = passages_str
                .split("\n\n")
                .map(|s| s.to_string())
                .filter(|s: &String| !s.is_empty())
                .collect();

            // Extract reasoning for this hop
            let reasoning = self.extract_reasoning(question, &passages, hop_num).await?;

            // Store hop result
            hops.push(BaleenHop {
                query: query.clone(),
                passages: passages.clone(),
                reasoning,
                hop_num,
            });

            // Accumulate passages
            all_passages.extend(passages);
        }

        // Synthesize final answer
        let answer = self.synthesize_answer(question, &hops).await?;

        // Build output
        let mut output = ModuleOutput::new();
        output.set("answer", answer);
        output.set("question", question);
        output.set("num_hops", hops.len().to_string());

        // Add all passages as context
        let context = all_passages.join("\n\n");
        output.set("context", context);

        // Add hop details as JSON
        let hops_json =
            serde_json::to_string(&hops).map_err(|e| DspyError::SerializationError(e))?;
        output.set("hops", hops_json);

        Ok(output)
    }

    fn name(&self) -> &str {
        &self.name
    }
}

/// Builder for SimplifiedBaleen
pub struct BaleenBuilder {
    backend: Option<Arc<dyn RetrieverBackend>>,
    config: BaleenConfig,
    name: String,
}

impl BaleenBuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            backend: None,
            config: BaleenConfig::default(),
            name: "SimplifiedBaleen".to_string(),
        }
    }

    /// Set the retriever backend
    pub fn backend(mut self, backend: Arc<dyn RetrieverBackend>) -> Self {
        self.backend = Some(backend);
        self
    }

    /// Set maximum hops
    pub fn max_hops(mut self, max_hops: usize) -> Self {
        self.config.max_hops = max_hops;
        self
    }

    /// Set passages per hop
    pub fn k(mut self, k: usize) -> Self {
        self.config.k = k;
        self
    }

    /// Enable/disable chain of thought
    pub fn use_chain_of_thought(mut self, use_cot: bool) -> Self {
        self.config.use_chain_of_thought = use_cot;
        self
    }

    /// Set custom name
    pub fn name(mut self, name: impl Into<String>) -> Self {
        self.name = name.into();
        self
    }

    /// Build the SimplifiedBaleen module
    pub fn build(self) -> Result<SimplifiedBaleen> {
        let backend = self
            .backend
            .ok_or_else(|| DspyError::ConfigError("Retriever backend not set".to_string()))?;

        Ok(SimplifiedBaleen {
            retrieve: Retrieve::new(backend, self.config.k),
            config: self.config,
            name: self.name,
        })
    }
}

impl Default for BaleenBuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::modules::retrieve::InMemoryRetriever;

    #[test]
    fn test_baleen_config() {
        let config = BaleenConfig::default();
        assert_eq!(config.max_hops, 2);
        assert_eq!(config.k, 3);
        assert!(config.use_chain_of_thought);
    }

    #[tokio::test]
    async fn test_simplified_baleen() {
        let docs = vec![
            "Rust is a systems programming language".to_string(),
            "Rust provides memory safety without garbage collection".to_string(),
            "Rust was created by Mozilla Research".to_string(),
            "Rust 1.0 was released in 2015".to_string(),
        ];

        let backend = Arc::new(InMemoryRetriever::new(docs));
        let baleen = SimplifiedBaleen::with_backend(backend);

        let inputs = vec![("question", "What is Rust and when was it released?")];
        let output = baleen.forward(&inputs).await.unwrap();

        let answer = output.get("answer").unwrap();
        assert!(!answer.is_empty());

        let num_hops: usize = output.get("num_hops").unwrap().parse().unwrap();
        assert_eq!(num_hops, 2);
    }

    #[tokio::test]
    async fn test_baleen_builder() {
        let docs = vec!["Test document".to_string()];
        let backend = Arc::new(InMemoryRetriever::new(docs));

        let baleen = BaleenBuilder::new()
            .backend(backend)
            .max_hops(3)
            .k(5)
            .name("TestBaleen")
            .build()
            .unwrap();

        assert_eq!(baleen.name(), "TestBaleen");
        assert_eq!(baleen.config.max_hops, 3);
        assert_eq!(baleen.config.k, 5);
    }

    #[test]
    fn test_baleen_hop() {
        let hop = BaleenHop {
            query: "test query".to_string(),
            passages: vec!["passage 1".to_string()],
            reasoning: "test reasoning".to_string(),
            hop_num: 0,
        };

        assert_eq!(hop.query, "test query");
        assert_eq!(hop.hop_num, 0);
    }
}
