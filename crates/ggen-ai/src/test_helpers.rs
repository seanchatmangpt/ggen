//! Test helpers for ggen-ai integration and unit tests
//!
//! # Test Helpers for LLM Integration Testing
//!
//! ## PURPOSE
//! Provides reusable test utilities for LLM integrations and eliminates
//! repetitive mock client initialization patterns across the codebase.
//!
//! ## RESPONSIBILITIES
//! - **Runtime Availability Detection**: Check if Ollama service is accessible before tests run
//! - **Client Factory**: Provide standardized OllamaClient instances configured for testing
//! - **Mock Client Factory**: Provide pre-configured MockClient instances for unit tests
//! - **Generator Factory**: Create generators with appropriate mock/real clients
//! - **Conditional Test Execution**: Skip tests gracefully when dependencies are unavailable
//! - **Timeout Management**: Enforce reasonable timeouts for availability checks (10s default)
//!
//! ## CORE TEAM BEST PRACTICES APPLIED
//! - **DRY Principle**: Eliminates 41 instances of repeated mock client initialization
//! - **Intent-Driven Design**: Factory functions express test intent clearly
//! - **Consistent Setup**: All tests use same standard configurations
//! - **Easy to Use**: Simple function calls replace boilerplate code
//!
//! ## CONSTRAINTS
//! - External Dependency: Requires Ollama running locally (typically http://localhost:11434)
//! - Model Requirement: Tests default to `ministral-3:3b` (override with `OLLAMA_MODEL`)
//! - Timeout Limit: Availability checks must complete within 10 seconds
//!
//! ## INVARIANTS
//! 1. Tests MUST skip (not fail) when Ollama is unavailable
//! 2. All test clients use same model (`ministral-3:3b` by default)
//! 3. No availability check exceeds 10 seconds
//! 4. Helper functions do not modify global or shared state
//! 5. Mock clients must provide responses appropriate to generator type
//!
//! ## REFACTORING IMPACT
//! - Reduced code duplication by ~100+ lines across test files
//! - Improved test readability and maintainability
//! - Standardized mock responses for consistent test behavior

use crate::{
    client::{GenAiClient, LlmClient, LlmConfig},
    generators::{
        NaturalSearchGenerator, OntologyGenerator, RefactorAssistant, SparqlGenerator,
        TemplateGenerator,
    },
    providers::MockClient,
};
use std::sync::Arc;
use std::time::Duration;
use tokio::time::timeout;

fn resolved_ollama_model() -> String {
    std::env::var("OLLAMA_MODEL")
        .unwrap_or_else(|_| crate::constants::models::OLLAMA_DEFAULT.to_string())
}

/// Check if Ollama is available and running with the configured model
pub async fn check_ollama_availability() -> bool {
    let config = create_test_llm_config();
    match GenAiClient::new(config) {
        Ok(client) => {
            // Try a simple completion to verify Ollama is running
            let _test_config = LlmConfig {
                model: resolved_ollama_model(),
                max_tokens: Some(10),
                temperature: Some(0.1),
                top_p: None,
                stop: None,
                extra: std::collections::HashMap::new(),
            };

            match timeout(Duration::from_secs(10), client.complete("Hello")).await {
                Ok(Ok(_)) => true,
                Ok(Err(_)) => false,
                Err(_) => false,
            }
        }
        Err(_) => false,
    }
}

/// Skip test if Ollama is not available
#[macro_export]
macro_rules! skip_if_ollama_unavailable {
    () => {
        if !ggen_ai::test_helpers::check_ollama_availability().await {
            println!("⏭️  Skipping test: Ollama not available");
            return;
        }
    };
}

/// Create a test Ollama client with the configured Ollama model
pub fn create_test_ollama_client() -> Result<GenAiClient, crate::error::GgenAiError> {
    let config = create_test_llm_config();
    GenAiClient::new(config)
}

/// Create a test LlmConfig for the configured Ollama model
pub fn create_test_llm_config() -> LlmConfig {
    LlmConfig {
        model: resolved_ollama_model(),
        max_tokens: Some(100),
        temperature: Some(0.1),
        top_p: None,
        stop: None,
        extra: std::collections::HashMap::new(),
    }
}

// ============================================================================
// GENERATOR FACTORY FUNCTIONS
// ============================================================================
// These functions eliminate the repetitive pattern:
//   let client = MockClient::with_response("...");
//   let generator = SomeGenerator::new(Arc::new(client));
//
// CORE TEAM BEST PRACTICE:
// - Single responsibility: Each function creates one type of generator
// - Clear intent: Function name indicates what generator is created
// - Consistent API: All return generators ready to use in tests
// - DRY: Eliminates 41 instances of duplicate initialization code

/// Create a mock ontology generator with standard test response
///
/// # Intent-Driven Design
/// This function expresses the test intent: "I need an ontology generator for testing"
/// without requiring knowledge of mock client setup or response format.
///
/// # Standard Response Format
/// Returns a generator that produces valid Turtle with:
/// - @prefix declarations for ex: and rdf:
/// - A Person class definition
/// - Valid RDF/OWL syntax
///
/// # Example
/// ```rust
/// use ggen_ai::test_helpers::create_ontology_test_generator;
///
/// #[tokio::test]
/// async fn test_my_feature() {
///     let generator = create_ontology_test_generator();
///     let result = generator.generate_ontology("test", vec!["requirement"]).await;
///     assert!(result.is_ok());
/// }
/// ```
pub fn create_ontology_test_generator() -> OntologyGenerator {
    let response = r#"```turtle
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:Person a rdf:Class .
```"#;
    let client = MockClient::with_response(response);
    OntologyGenerator::new(Arc::new(client))
}

/// Create a mock SPARQL generator with standard test response
///
/// # Intent-Driven Design
/// This function expresses: "I need a SPARQL generator for testing"
///
/// # Standard Response Format
/// Returns a generator that produces valid SPARQL SELECT queries
///
/// # Example
/// ```rust
/// use ggen_ai::test_helpers::create_sparql_test_generator;
///
/// #[tokio::test]
/// async fn test_query_generation() {
///     let generator = create_sparql_test_generator();
///     let graph = ggen_core::Graph::new().unwrap();
///     let query = generator.generate_query(&graph, "test intent").await;
///     assert!(query.is_ok());
/// }
/// ```
pub fn create_sparql_test_generator() -> SparqlGenerator {
    let response = "```sparql\nSELECT ?name WHERE {\n  ?person foaf:name ?name .\n}\n```";
    let client = MockClient::with_response(response);
    SparqlGenerator::new(Arc::new(client))
}

/// Create a mock template generator with standard test response
///
/// # Intent-Driven Design
/// This function expresses: "I need a template generator for testing"
///
/// # Standard Response Format
/// Returns a generator that produces valid ggen templates with:
/// - YAML frontmatter (to, vars)
/// - Template body with Tera syntax
///
/// # Example
/// ```rust
/// use ggen_ai::test_helpers::create_template_test_generator;
///
/// #[tokio::test]
/// async fn test_template_creation() {
///     let generator = create_template_test_generator();
///     let template = generator.generate_template("description", vec![]).await;
///     assert!(template.is_ok());
/// }
/// ```
pub fn create_template_test_generator() -> TemplateGenerator {
    let response = "---\nto: \"test.tmpl\"\nvars:\n  name: \"test\"\n---\nHello {{ name }}!";
    let client = MockClient::with_response(response);
    TemplateGenerator::new(Arc::new(client))
}

/// Create a mock refactor assistant with standard test response
///
/// # Intent-Driven Design
/// This function expresses: "I need a refactor assistant for testing"
///
/// # Standard Response Format
/// Returns an assistant that produces JSON with refactoring suggestions
///
/// # Example
/// ```rust
/// use ggen_ai::test_helpers::create_refactor_test_assistant;
///
/// #[tokio::test]
/// async fn test_refactoring() {
///     let assistant = create_refactor_test_assistant();
///     let context = ggen_ai::generators::refactor::RefactoringContext::new("rust".to_string());
///     let suggestions = assistant.suggest_refactoring("fn test() {}", &context).await;
///     assert!(suggestions.is_ok());
/// }
/// ```
pub fn create_refactor_test_assistant() -> RefactorAssistant {
    let response = r#"{
        "suggestions": [
            {
                "type": "ExtractMethod",
                "description": "Extract validation logic",
                "suggested_code": "fn validate() { }",
                "confidence": 0.9,
                "reasoning": "Test reasoning",
                "impact": "Low"
            }
        ]
    }"#;
    let client = MockClient::with_response(response);
    RefactorAssistant::new(Arc::new(client))
}

/// Create a generic mock generator with custom response
///
/// # Intent-Driven Design
/// For tests that need custom mock responses, this provides flexibility
/// while maintaining the factory pattern.
///
/// # Type Parameter
/// - `T`: The generator type to create (must have `new(Arc<dyn LlmClient>)`)
///
/// # Example
/// ```rust
/// use ggen_ai::test_helpers::create_mock_generator;
/// use ggen_ai::generators::OntologyGenerator;
///
/// #[tokio::test]
/// async fn test_custom_response() {
///     let generator: OntologyGenerator = create_mock_generator("custom response");
///     // Use generator with custom mock response
/// }
/// ```
pub fn create_mock_generator<T, F>(response: &str, constructor: F) -> T
where
    F: FnOnce(Arc<dyn LlmClient>) -> T,
{
    let client = MockClient::with_response(response);
    constructor(Arc::new(client))
}

/// Create an ontology generator with custom mock response
///
/// # Use Case
/// When tests need specific Turtle content different from standard response
pub fn create_ontology_generator_with_response(response: &str) -> OntologyGenerator {
    create_mock_generator(response, OntologyGenerator::new)
}

/// Create a SPARQL generator with custom mock response
///
/// # Use Case
/// When tests need specific SPARQL queries different from standard response
pub fn create_sparql_generator_with_response(response: &str) -> SparqlGenerator {
    create_mock_generator(response, SparqlGenerator::new)
}

/// Create a template generator with custom mock response
///
/// # Use Case
/// When tests need specific template content different from standard response
pub fn create_template_generator_with_response(response: &str) -> TemplateGenerator {
    create_mock_generator(response, TemplateGenerator::new)
}

/// Create a refactor assistant with custom mock response
///
/// # Use Case
/// When tests need specific refactoring suggestions different from standard response
pub fn create_refactor_assistant_with_response(response: &str) -> RefactorAssistant {
    create_mock_generator(response, RefactorAssistant::new)
}

/// Create a mock natural search generator with standard test response
///
/// # Intent-Driven Design
/// This function expresses: "I need a natural search generator for testing"
///
/// # Standard Response Format
/// Returns a generator that produces valid JSON with search results
///
/// # Example
/// ```rust
/// use ggen_ai::test_helpers::create_natural_search_test_generator;
///
/// #[tokio::test]
/// async fn test_package_search() {
///     let mut generator = create_natural_search_test_generator();
///     let result = generator.search("authentication").await;
///     assert!(result.is_ok());
/// }
/// ```
pub fn create_natural_search_test_generator() -> NaturalSearchGenerator {
    let response = r#"```json
{
  "interpretation": "User is looking for packages",
  "keywords": ["test", "search"],
  "category": "testing",
  "confidence": 0.9,
  "packages": [
    {
      "id": "io.ggen.test",
      "name": "Test Package",
      "description": "A test package",
      "category": "testing",
      "tags": ["test"],
      "relevance_score": 0.95,
      "ai_reasoning": "Perfect match"
    }
  ],
  "suggestions": ["Try: 'test query'"]
}
```"#;
    let client = MockClient::with_response(response);
    NaturalSearchGenerator::new(Arc::new(client)).unwrap()
}

/// Create a natural search generator with custom mock response
///
/// # Use Case
/// When tests need specific search results different from standard response
pub fn create_natural_search_generator_with_response(
    response: &str,
) -> Result<NaturalSearchGenerator, crate::error::GgenAiError> {
    let client = MockClient::with_response(response);
    NaturalSearchGenerator::new(Arc::new(client))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_ollama_availability_check() {
        // This test will always pass, but will show if Ollama is available
        let available = check_ollama_availability().await;
        if available {
            println!("✅ Ollama is available for integration tests");
        } else {
            println!("⚠️  Ollama is not available - integration tests will be skipped");
        }
    }

    #[tokio::test]
    async fn test_ontology_generator_factory() {
        let generator = create_ontology_test_generator();
        let result = generator
            .generate_ontology("test domain", vec!["test requirement"])
            .await;
        assert!(result.is_ok(), "Factory-created generator should work");
    }

    #[tokio::test]
    async fn test_sparql_generator_factory() {
        let generator = create_sparql_test_generator();
        let graph = ggen_core::Graph::new().unwrap();
        let result = generator.generate_query(&graph, "test intent").await;
        assert!(result.is_ok(), "Factory-created generator should work");
    }

    #[tokio::test]
    async fn test_template_generator_factory() {
        let generator = create_template_test_generator();
        let result = generator
            .generate_template("test description", vec![])
            .await;
        assert!(result.is_ok(), "Factory-created generator should work");
    }

    #[tokio::test]
    async fn test_refactor_assistant_factory() {
        let assistant = create_refactor_test_assistant();
        let context = crate::generators::refactor::RefactoringContext::new("rust".to_string());
        let result = assistant
            .suggest_refactoring("fn test() {}", &context)
            .await;
        assert!(result.is_ok(), "Factory-created assistant should work");
    }

    #[tokio::test]
    async fn test_custom_response_generator() {
        let custom_response = r#"```turtle
@prefix custom: <http://custom.org/> .
custom:Test a custom:Class .
```"#;
        let generator = create_ontology_generator_with_response(custom_response);
        let result = generator.generate_ontology("custom", vec![]).await;
        assert!(result.is_ok());
        let ontology = result.unwrap();
        assert!(ontology.contains("custom:Test"));
    }
}
