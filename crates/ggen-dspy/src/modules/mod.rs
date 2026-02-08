//! Core module types and traits for ggen-dspy
//!
//! This module defines the Module trait and related types for building
//! composable LLM agent pipelines.

use crate::Result;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

pub mod baleen;
pub mod chain_of_thought;
pub mod multihop_qa;
pub mod predictor;
pub mod program_of_thought;
pub mod react;
pub mod retrieve;

pub use baleen::{BaleenBuilder, BaleenConfig, BaleenHop, SimplifiedBaleen};
pub use chain_of_thought::ChainOfThought;
pub use multihop_qa::{HopState, MultiHopConfig, MultiHopQA, MultiHopQABuilder};
pub use predictor::Predictor;
pub use program_of_thought::{
    CodeLanguage, ExecutionResult, ProgramOfThought, ProgramOfThoughtBuilder,
    ProgramOfThoughtConfig,
};
pub use react::{ReAct, ReactAgent, Tool};
pub use retrieve::{InMemoryRetriever, Passage, Retrieve, RetrieveBuilder, RetrieverBackend};

/// Module trait for composable LLM components
#[async_trait]
pub trait Module: Send + Sync {
    /// Execute the module with given inputs
    async fn forward(&self, inputs: &[(&str, &str)]) -> Result<ModuleOutput>;

    /// Get module name
    fn name(&self) -> &str;
}

/// Output from a module execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ModuleOutput {
    /// Output fields
    fields: HashMap<String, String>,
}

impl ModuleOutput {
    /// Create a new empty module output
    pub fn new() -> Self {
        Self {
            fields: HashMap::new(),
        }
    }

    /// Set a field value
    pub fn set(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.fields.insert(key.into(), value.into());
    }

    /// Get a field value
    pub fn get(&self, key: &str) -> Result<&str> {
        self.fields
            .get(key)
            .map(|s| s.as_str())
            .ok_or_else(|| crate::DspyError::MissingField(key.to_string()))
    }

    /// Get all fields
    pub fn fields(&self) -> &HashMap<String, String> {
        &self.fields
    }

    /// Create from fields
    pub fn from_fields(fields: HashMap<String, String>) -> Self {
        Self { fields }
    }
}

impl Default for ModuleOutput {
    fn default() -> Self {
        Self::new()
    }
}

/// Context passed to modules during execution
#[derive(Debug, Clone, Default)]
pub struct ModuleContext {
    /// Additional metadata
    pub metadata: HashMap<String, String>,
}

impl ModuleContext {
    /// Create a new module context
    pub fn new() -> Self {
        Self::default()
    }

    /// Set metadata
    pub fn set_metadata(&mut self, key: impl Into<String>, value: impl Into<String>) {
        self.metadata.insert(key.into(), value.into());
    }

    /// Get metadata
    pub fn get_metadata(&self, key: &str) -> Option<&str> {
        self.metadata.get(key).map(|s| s.as_str())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_module_output() {
        let mut output = ModuleOutput::new();
        output.set("answer", "42");
        assert_eq!(output.get("answer").ok(), Some("42"));
    }

    #[test]
    fn test_module_output_missing_field() {
        let output = ModuleOutput::new();
        assert!(output.get("missing").is_err());
    }

    #[test]
    fn test_module_context() {
        let mut context = ModuleContext::new();
        context.set_metadata("user_id", "123");
        assert_eq!(context.get_metadata("user_id"), Some("123"));
    }
}
