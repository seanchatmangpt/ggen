//! Multi-Hop Question Answering pattern
//!
//! Iteratively retrieves and reasons across multiple documents to answer complex questions.

use crate::{Module, ModuleOutput, Result, DspyError};
use super::retrieve::{Retrieve, RetrieverBackend};
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// Intermediate hop state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HopState {
    /// The query for this hop
    pub query: String,

    /// Retrieved passages
    pub passages: Vec<String>,

    /// Reasoning from this hop
    pub reasoning: Option<String>,
}

/// Multi-hop QA configuration
#[derive(Debug, Clone)]
pub struct MultiHopConfig {
    /// Maximum number of hops
    pub max_hops: usize,

    /// Passages per hop
    pub passages_per_hop: usize,

    /// Enable intermediate reasoning
    pub enable_reasoning: bool,
}

impl Default for MultiHopConfig {
    fn default() -> Self {
        Self {
            max_hops: 3,
            passages_per_hop: 3,
            enable_reasoning: true,
        }
    }
}

/// Multi-Hop Question Answering module
///
/// Decomposes complex questions into multiple retrieval hops,
/// gathering information iteratively before generating final answer.
pub struct MultiHopQA {
    retrieve: Retrieve,
    config: MultiHopConfig,
    name: String,
}

impl MultiHopQA {
    /// Create a new Multi-Hop QA module
    pub fn new(backend: Arc<dyn RetrieverBackend>, config: MultiHopConfig) -> Self {
        let retrieve = Retrieve::new(backend, config.passages_per_hop);

        Self {
            retrieve,
            config,
            name: "MultiHopQA".to_string(),
        }
    }

    /// Create with default configuration
    pub fn with_backend(backend: Arc<dyn RetrieverBackend>) -> Self {
        Self::new(backend, MultiHopConfig::default())
    }

    /// Set custom name
    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = name.into();
        self
    }

    /// Generate follow-up query based on current state
    async fn generate_query(&self, question: &str, hop_states: &[HopState]) -> Result<String> {
        // TODO: Use LLM to generate better queries
        // For now, use simple heuristic

        if hop_states.is_empty() {
            Ok(question.to_string())
        } else {
            // Generate query that builds on previous information
            Ok(format!("{} additional context", question))
        }
    }

    /// Check if we have enough information to answer
    fn should_continue(&self, hop_states: &[HopState]) -> bool {
        if hop_states.len() >= self.config.max_hops {
            return false;
        }

        // Simple heuristic: continue if we haven't reached max hops
        // TODO: Use LLM to determine if more information is needed
        true
    }

    /// Aggregate context from all hops
    fn aggregate_context(&self, hop_states: &[HopState]) -> String {
        hop_states.iter()
            .flat_map(|hop| hop.passages.iter())
            .cloned()
            .collect::<Vec<_>>()
            .join("\n\n")
    }
}

#[async_trait]
impl Module for MultiHopQA {
    async fn forward(&self, inputs: &[(&str, &str)]) -> Result<ModuleOutput> {
        // Extract question
        let question = inputs.iter()
            .find(|(key, _)| *key == "question")
            .map(|(_, value)| *value)
            .ok_or_else(|| DspyError::MissingInput("question".to_string()))?;

        let mut hop_states = Vec::new();

        // Multi-hop retrieval loop
        for _hop_num in 0..self.config.max_hops {
            // Generate query for this hop
            let query = self.generate_query(question, &hop_states).await?;

            // Retrieve passages
            let retrieve_inputs = vec![("query", query.as_str())];
            let retrieve_output = self.retrieve.forward(&retrieve_inputs).await?;

            let passages_str = retrieve_output.get("passages")?;
            let passages: Vec<String> = passages_str
                .split("\n\n")
                .map(|s| s.to_string())
                .collect();

            // Store hop state
            hop_states.push(HopState {
                query: query.clone(),
                passages,
                reasoning: None,
            });

            // Check if we should continue
            if !self.should_continue(&hop_states) {
                break;
            }
        }

        // Aggregate all context
        let aggregated_context = self.aggregate_context(&hop_states);

        // Build output
        let mut output = ModuleOutput::new();
        output.set("context", aggregated_context);
        output.set("num_hops", hop_states.len().to_string());
        output.set("question", question);

        // Add hop details as JSON
        let hops_json = serde_json::to_string(&hop_states)
            .map_err(|e| DspyError::SerializationError(e))?;
        output.set("hops", hops_json);

        Ok(output)
    }

    fn name(&self) -> &str {
        &self.name
    }
}

/// Builder for MultiHopQA
pub struct MultiHopQABuilder {
    backend: Option<Arc<dyn RetrieverBackend>>,
    config: MultiHopConfig,
    name: String,
}

impl MultiHopQABuilder {
    /// Create a new builder
    pub fn new() -> Self {
        Self {
            backend: None,
            config: MultiHopConfig::default(),
            name: "MultiHopQA".to_string(),
        }
    }

    /// Set the retriever backend
    pub fn backend(mut self, backend: Arc<dyn RetrieverBackend>) -> Self {
        self.backend = Some(backend);
        self
    }

    /// Set maximum number of hops
    pub fn max_hops(mut self, max_hops: usize) -> Self {
        self.config.max_hops = max_hops;
        self
    }

    /// Set passages per hop
    pub fn passages_per_hop(mut self, passages_per_hop: usize) -> Self {
        self.config.passages_per_hop = passages_per_hop;
        self
    }

    /// Enable/disable reasoning
    pub fn enable_reasoning(mut self, enable: bool) -> Self {
        self.config.enable_reasoning = enable;
        self
    }

    /// Set custom name
    pub fn name(mut self, name: impl Into<String>) -> Self {
        self.name = name.into();
        self
    }

    /// Build the MultiHopQA module
    pub fn build(self) -> Result<MultiHopQA> {
        let backend = self.backend
            .ok_or_else(|| DspyError::ConfigError("Retriever backend not set".to_string()))?;

        Ok(MultiHopQA {
            retrieve: Retrieve::new(backend, self.config.passages_per_hop),
            config: self.config,
            name: self.name,
        })
    }
}

impl Default for MultiHopQABuilder {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::modules::retrieve::InMemoryRetriever;

    #[test]
    fn test_multihop_config() {
        let config = MultiHopConfig::default();
        assert_eq!(config.max_hops, 3);
        assert_eq!(config.passages_per_hop, 3);
        assert!(config.enable_reasoning);
    }

    #[tokio::test]
    async fn test_multihop_qa() {
        let docs = vec![
            "Paris is the capital of France".to_string(),
            "The Eiffel Tower is in Paris".to_string(),
            "France is in Europe".to_string(),
            "The Eiffel Tower was built in 1889".to_string(),
        ];

        let backend = Arc::new(InMemoryRetriever::new(docs));
        let multihop = MultiHopQA::with_backend(backend);

        let inputs = vec![("question", "Where is the Eiffel Tower and when was it built?")];
        let output = multihop.forward(&inputs).await.unwrap();

        let context = output.get("context").unwrap();
        assert!(context.contains("Eiffel Tower"));

        let num_hops: usize = output.get("num_hops").unwrap().parse().unwrap();
        assert!(num_hops > 0);
    }

    #[tokio::test]
    async fn test_multihop_builder() {
        let docs = vec!["Test document".to_string()];
        let backend = Arc::new(InMemoryRetriever::new(docs));

        let multihop = MultiHopQABuilder::new()
            .backend(backend)
            .max_hops(2)
            .passages_per_hop(5)
            .name("TestMultiHop")
            .build()
            .unwrap();

        assert_eq!(multihop.name(), "TestMultiHop");
        assert_eq!(multihop.config.max_hops, 2);
    }

    #[test]
    fn test_hop_state() {
        let state = HopState {
            query: "test query".to_string(),
            passages: vec!["passage 1".to_string()],
            reasoning: Some("test reasoning".to_string()),
        };

        assert_eq!(state.query, "test query");
        assert_eq!(state.passages.len(), 1);
        assert!(state.reasoning.is_some());
    }
}
