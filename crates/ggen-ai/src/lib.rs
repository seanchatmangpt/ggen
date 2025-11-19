//! # ggen-ai - LLM integration layer for ggen
//!
//! Thin wrapper around genai for ggen with environment support and caching.
//!
//! This crate provides a simplified LLM integration layer for ggen, focusing on:
//! - Environment-based configuration
//! - Response caching
//! - Template generation
//! - SPARQL query generation
//! - Ontology generation
//! - Code refactoring assistance
//!
//! ## Features
//!
//! - **Multi-provider LLM support**: OpenAI, Anthropic, Ollama, Gemini, DeepSeek, xAI/Grok, Groq, Cohere (via genai)
//! - **Environment-based configuration**: Automatic API key detection and model selection
//! - **Response caching**: Reduce API costs and latency with intelligent caching
//! - **Template generation**: Natural language to ggen templates
//! - **SPARQL query generation**: Intent-based query construction
//! - **Ontology generation**: Domain descriptions to RDF/OWL
//! - **Code refactoring**: AI-assisted code improvement suggestions
//! - **RDF-based CLI generation**: Generate CLI projects from RDF ontologies
//!
//! ## Quick Start
//!
//! ```rust,no_run
//! use ggen_ai::{GenAiClient, LlmClient, LlmConfig};
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! // Initialize client with default configuration
//! let config = LlmConfig::default();
//! let client = GenAiClient::new(config)?;
//!
//! // Generate response
//! let response = client.complete("Explain Rust ownership").await?;
//! println!("{}", response.content);
//! # Ok(())
//! # }
//! ```
//!
//! ## Module Organization
//!
//! - `cache` - LLM response caching
//! - `client` - LLM client abstraction
//! - `config` - Configuration management
//! - `generators` - Specialized generators (templates, SPARQL, ontologies)
//! - `providers` - LLM provider implementations
//! - `prompts` - Prompt templates and builders
//! - `rdf` - RDF-based CLI generation
//! - `security` - API key masking and security
//! - `streaming` - Streaming response support
//! - `types` - Type definitions
//! - `ontology_evolution` - AI-native ontology evolution engine

#![deny(warnings)] // Poka-Yoke: Prevent warnings at compile time - compiler enforces correctness

pub mod cache;
pub mod client;
pub mod config;
pub mod constants;
pub mod error;
pub mod error_utils;
pub mod generators;
pub mod ontology_evolution;
pub mod parsing_utils;
pub mod prompts;
pub mod providers;
pub mod rdf;
pub mod security;
pub mod streaming;
pub mod types;

// Test helpers for mock clients
#[cfg(test)]
pub mod test_helpers;

// Re-export main types for convenience
pub use cache::{CacheConfig, CacheStats, LlmCache};
pub use client::{GenAiClient, LlmChunk, LlmClient, LlmConfig, LlmResponse, UsageStats};
pub use config::{get_global_config, init_global_config, AiConfig, GlobalLlmConfig, LlmProvider};
pub use error::{GgenAiError, Result};
pub use generators::{
    NaturalSearchGenerator, OntologyGenerator, QualityMetrics, RefactorAssistant, SparqlGenerator,
    TemplateGenerator, TemplateValidator, ValidationIssue,
};
pub use ontology_evolution::{
    EvolutionConfig, EvolutionMetrics, FeedbackSignal, LanguageTarget, NeuralRepresentation,
    NeuralSymbolicReasoner, OntologyEvolutionCoordinator, PolyglotTypeInferencer,
    QuantumTemplateSelector, RLFeedbackLoop, SchemaEvolution, SymbolicKnowledge, TypeSignature,
};
pub use providers::adapter::{ollama_default_config, ollama_qwen3_coder_config, MockClient};
pub use rdf::{
    Argument, ArgumentType, CliGenerator, CliProject, Dependency, Noun, QueryExecutor, RdfParser,
    TemplateRenderer, Validation, Verb,
};
pub use security::{MaskApiKey, SecretString};
pub use streaming::StreamConfig;
pub use types::{DecisionId, PolicyId, RequestId, RuleId};

/// Version information
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Initialize tracing for the ggen-ai crate
pub fn init_logging() {
    use tracing_subscriber::{fmt, EnvFilter};

    if std::env::var("RUST_LOG").is_err() {
        std::env::set_var("RUST_LOG", "ggen_ai=info");
    }

    let _ = fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_target(false)
        .try_init();
}
