//! AI-powered ontology generator
//!
//! # WHAT THIS MODULE SHOULD DO (Intent-Driven Architecture)
//!
//! ## PURPOSE
//! This module should bridge the gap between LLM-generated content and valid RDF/Turtle,
//! extracting ontology definitions from varied AI responses and converting them into
//! machine-readable semantic graphs.
//!
//! ## RESPONSIBILITIES
//! 1. **Prompt Engineering**: Should craft prompts that elicit valid Turtle/RDF from LLMs
//! 2. **Content Extraction**: Should parse varied response formats (code blocks, plain text, etc.)
//! 3. **Format Normalization**: Should ensure extracted content is valid Turtle
//! 4. **Streaming Support**: Should enable real-time ontology generation for UX
//! 5. **Error Recovery**: Should suggest fixes when LLM returns invalid format
//!
//! ## CONSTRAINTS
//! - Must handle multiple LLM providers (OpenAI, Anthropic, Ollama, etc.)
//! - Must support both streaming and batch modes
//! - Must extract Turtle from code blocks with various language markers
//! - Must validate extracted content is parseable Turtle before returning
//! - Must never hallucinate prefixes or namespaces
//!
//! ## DEPENDENCIES
//! - `LlmClient`: Should be provider-agnostic for multi-LLM support
//! - `OntologyPromptBuilder`: Should generate effective prompts
//! - Turtle parser: Should validate extracted content
//!
//! ## INVARIANTS
//! - Extracted content must be valid Turtle (prefix-complete, well-formed)
//! - All responses must include provenance metadata
//! - Stream chunks must be concatenatable into valid Turtle
//!
//! ## ERROR HANDLING STRATEGY
//! - No Turtle found → Provide response preview, suggest prompt improvements
//! - Invalid Turtle → Attempt auto-repair (add prefixes), then fail with details
//! - Streaming errors → Buffer partial content, return what's valid
//! - Multiple code blocks → Extract first valid Turtle block
//!
//! ## TESTING STRATEGY
//! - Test extraction with multiple code block formats (turtle, ttl, rdf, none)
//! - Test with/without prefixes
//! - Test streaming with incomplete chunks
//! - Mock different LLM response patterns
//!
//! ## REFACTORING PRIORITIES
//! - [P0] Add Turtle validation after extraction (currently trusts LLM)
//! - [P0] Standardize error messages with examples
//! - [P1] Extract code block parsing into reusable utility
//! - [P1] Add caching for repeated prompts
//! - [P2] Support SHACL constraint generation

use crate::client::{LlmClient, LlmConfig};
use crate::error::Result;
use crate::prompts::OntologyPromptBuilder;
use futures::StreamExt;
use serde::{Deserialize, Serialize};
use std::sync::Arc;

/// JSON representation of graph evolution operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphEvolution {
    /// Operations to perform on the graph
    pub operations: Vec<GraphOperation>,
    /// Reasoning for the changes
    pub reasoning: String,
    /// Confidence score for the evolution
    pub confidence: f64,
}

/// Individual graph operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GraphOperation {
    /// Operation type
    pub operation_type: OperationType,
    /// Subject URI
    pub subject: String,
    /// Predicate URI
    pub predicate: String,
    /// Object (URI, literal, or variable)
    pub object: String,
    /// Optional operation metadata
    pub metadata: Option<OperationMetadata>,
}

/// Types of graph operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum OperationType {
    /// Add a new triple
    Add,
    /// Remove a triple
    Remove,
    /// Modify an existing triple
    Modify,
    /// Add a class definition
    AddClass,
    /// Add a property definition
    AddProperty,
    /// Add a subclass relationship
    AddSubclass,
}

/// Operation metadata
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OperationMetadata {
    /// Human-readable description
    pub description: String,
    /// Impact assessment
    pub impact: String,
    /// Validation requirements
    pub validation: Vec<String>,
}

/// AI-powered ontology generator
#[derive(Debug)]
pub struct OntologyGenerator {
    client: Arc<dyn LlmClient>,
}

impl OntologyGenerator {
    /// Create a new ontology generator
    pub fn new(client: Arc<dyn LlmClient>) -> Self {
        Self { client }
    }

    /// Create a new ontology generator with custom config
    pub fn with_config(client: Arc<dyn LlmClient>, _config: LlmConfig) -> Self {
        Self { client }
    }

    /// Create a new ontology generator with a client
    pub fn with_client(client: Arc<dyn LlmClient>) -> Self {
        Self { client }
    }

    /// Generate an ontology from a natural language description
    pub async fn generate_ontology(&self, domain: &str, requirements: Vec<&str>) -> Result<String> {
        let prompt = OntologyPromptBuilder::new(domain.to_string())
            .with_requirements(requirements.iter().map(|s| s.to_string()).collect())
            .build()?;

        let response = self.client.complete(&prompt).await?;

        // Extract ontology content from response
        self.extract_ontology_content(&response.content)
    }

    /// Stream ontology generation from a natural language description
    pub async fn stream_ontology(
        &self, domain: &str, requirements: Vec<&str>,
    ) -> Result<futures::stream::BoxStream<'static, Result<String>>> {
        let prompt = OntologyPromptBuilder::new(domain.to_string())
            .with_requirements(requirements.iter().map(|s| s.to_string()).collect())
            .build()?;

        let stream = self.client.complete_stream(&prompt).await?;

        Ok(Box::pin(stream.map(|chunk| Ok(chunk.content))))
    }

    /// Get the LLM client
    pub fn client(&self) -> &Arc<dyn LlmClient> {
        &self.client
    }

    /// Get the current configuration
    pub fn config(&self) -> &LlmConfig {
        self.client.get_config()
    }

    /// Update the configuration
    pub fn set_config(&mut self, _config: LlmConfig) {
        // Note: This would require mutable access to the client
        // For now, we'll skip this functionality
    }

    /// Extract ontology content from AI response
    ///
    /// Uses the parsing_utils::extract_turtle_content() utility to handle:
    /// - Code blocks with ```turtle, ```ttl, or ```rdf markers
    /// - Any code block containing RDF-like patterns
    /// - Plain text responses with RDF content
    ///
    /// After extraction, validates the Turtle syntax using oxigraph parser
    /// to ensure the LLM generated valid RDF.
    ///
    /// # Core Team Best Practices Applied
    /// - **Lenient Parsing**: Accepts many response formats
    /// - **Strict Validation**: Validates extracted content is syntactically valid Turtle
    /// - **Defensive**: Never trusts LLM output without validation
    /// - **Fail Fast**: Catches syntax errors at generation time, not later
    /// - **Clear Errors**: Returns descriptive error with response preview and suggestions
    fn extract_ontology_content(&self, response: &str) -> Result<String> {
        // Step 1: Lenient extraction - try to find Turtle content in varied formats
        let content = crate::parsing_utils::extract_turtle_content(response).ok_or_else(|| {
            crate::error_utils::no_valid_content_error::<String>(
                "Turtle/RDF content (@prefix declarations or RDF triples)",
                response,
                crate::error_utils::ErrorContext::OntologyGeneration,
            )
            .unwrap_err()
        })?;

        // Step 2: Strict validation - verify the extracted content is valid Turtle
        crate::parsing_utils::validate_turtle_syntax(&content).map_err(|validation_error| {
            crate::error_utils::turtle_validation_error::<String>(
                &validation_error,
                &content,
                crate::error_utils::ErrorContext::OntologyGeneration,
            )
            .unwrap_err()
        })?;

        // Content is both extracted and validated
        Ok(content)
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test_helpers::create_ontology_test_generator;

    #[tokio::test]
    async fn test_ontology_generation() {
        let generator = create_ontology_test_generator();

        let ontology = generator
            .generate_ontology(
                "Person management",
                vec!["Include Person class", "Use RDF/OWL"],
            )
            .await
            .unwrap();

        assert!(ontology.contains("@prefix"));
        assert!(ontology.contains("Person"));
    }

    #[tokio::test]
    async fn test_ontology_generation_validates_syntax() {
        // Valid Turtle should pass
        let valid_response = r#"```turtle
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person a rdfs:Class ;
    rdfs:label "Person" ;
    rdfs:comment "A human being" .
```"#;

        let generator = crate::test_helpers::create_ontology_generator_with_response(valid_response);

        let result = generator
            .generate_ontology("Person", vec!["Include Person class"])
            .await;

        assert!(result.is_ok(), "Valid Turtle should pass validation");
        let ontology = result.unwrap();
        assert!(ontology.contains("@prefix ex:"));
        assert!(ontology.contains("ex:Person"));
    }

    #[tokio::test]
    async fn test_ontology_generation_rejects_invalid_turtle() {
        // Invalid Turtle: missing prefix declaration
        let invalid_response = "```turtle\nex:Person a ex:Class .\n```";

        let generator = crate::test_helpers::create_ontology_generator_with_response(invalid_response);

        let result = generator
            .generate_ontology("Person", vec!["Include Person class"])
            .await;

        assert!(result.is_err(), "Invalid Turtle should be rejected");
        let err = result.unwrap_err();
        let err_msg = err.to_string();
        assert!(
            err_msg.contains("Turtle validation failed") || err_msg.contains("prefix"),
            "Error should mention validation failure or prefix issue: {}",
            err_msg
        );
    }

    #[tokio::test]
    async fn test_ontology_generation_rejects_empty_content() {
        let empty_response = "```turtle\n\n```";

        let generator = crate::test_helpers::create_ontology_generator_with_response(empty_response);

        let result = generator
            .generate_ontology("Empty", vec!["Test"])
            .await;

        assert!(result.is_err(), "Empty Turtle should be rejected");
        let err = result.unwrap_err();
        let err_msg = err.to_string();
        assert!(
            err_msg.contains("Empty") || err_msg.contains("validation"),
            "Error should mention empty content or validation: {}",
            err_msg
        );
    }

    #[tokio::test]
    async fn test_ontology_generation_rejects_malformed_triples() {
        // Malformed: triple missing object
        let malformed_response = "```turtle\n@prefix ex: <http://example.org/> .\nex:Thing ex:property\n```";

        let generator = crate::test_helpers::create_ontology_generator_with_response(malformed_response);

        let result = generator
            .generate_ontology("Test", vec!["Test"])
            .await;

        assert!(result.is_err(), "Malformed Turtle should be rejected");
    }

    #[tokio::test]
    async fn test_ontology_generation_helpful_error_messages() {
        // Invalid: undefined prefix
        let invalid_response = "```turtle\nex:Thing a ex:Class .\n```";

        let generator = crate::test_helpers::create_ontology_generator_with_response(invalid_response);

        let result = generator
            .generate_ontology("Test", vec![])
            .await;

        assert!(result.is_err());
        let err = result.unwrap_err();
        let err_msg = err.to_string();

        // Should contain helpful suggestions
        assert!(
            err_msg.contains("Possible fixes") || err_msg.contains("Suggestion"),
            "Error should contain helpful suggestions: {}",
            err_msg
        );
    }

    #[tokio::test]
    async fn test_extract_ontology_content_with_ttl_marker() {
        let response = "```ttl\n@prefix ex: <http://example.org/> .\nex:Thing a ex:Class .\n```";
        let generator = crate::test_helpers::create_ontology_generator_with_response(response);

        let result = generator.extract_ontology_content(response);
        assert!(result.is_ok(), "Valid ttl marker should work");
    }

    #[tokio::test]
    async fn test_extract_ontology_content_with_rdf_marker() {
        let response = "```rdf\n@prefix ex: <http://example.org/> .\nex:Thing a ex:Class .\n```";
        let generator = crate::test_helpers::create_ontology_generator_with_response(response);

        let result = generator.extract_ontology_content(response);
        assert!(result.is_ok(), "Valid rdf marker should work");
    }

    #[tokio::test]
    async fn test_extract_ontology_content_plain_text() {
        let response = "@prefix ex: <http://example.org/> .\nex:Thing a ex:Class .";
        let generator = crate::test_helpers::create_ontology_generator_with_response(response);

        let result = generator.extract_ontology_content(response);
        assert!(result.is_ok(), "Valid plain text Turtle should work");
    }
}
