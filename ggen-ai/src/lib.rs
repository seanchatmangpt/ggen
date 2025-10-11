//! # ggen-ai
//!
//! AI-powered code generation capabilities for ggen.
//!
//! This crate provides LLM integration for intelligent template generation,
//! SPARQL query construction, ontology generation, and code refactoring.
//!
//! ## Features
//!
//! - **Multi-provider LLM support**: OpenAI, Anthropic, Ollama, Gemini, DeepSeek, xAI/Grok, Groq, Cohere (via genai)
//! - **Intelligent template generation**: Natural language to ggen templates
//! - **SPARQL query generation**: Intent-based query construction
//! - **Ontology generation**: Domain descriptions to RDF/OWL
//! - **Code refactoring**: AI-assisted code improvement suggestions
//! - **MCP server integration**: Expose AI capabilities via Model Context Protocol
//!
//! ## Quick Start
//!
//! ```rust
//! use ggen_ai::{LlmClient, TemplateGenerator, MockClient};
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     // Initialize mock client for demonstration
//!     let client = MockClient::with_response("Generated template content");
//!     
//!     // Generate template from description
//!     let generator = TemplateGenerator::new(Box::new(client));
//!     let _template = generator.generate_template(
//!         "Generate a REST API controller for user management",
//!         vec!["Include CRUD operations", "Use TypeScript"]
//!     ).await?;
//!     
//!     println!("Template generated successfully!");
//!     Ok(())
//! }
//! ```

pub mod agents;
pub mod autonomous;
pub mod client;
pub mod config;
pub mod constants;
pub mod generators;
pub mod parsing_utils;
pub mod prompts;
pub mod ultrathink;
// Swarm module is implemented as a directory
// pub mod swarm;
pub mod cache;
pub mod cli;
pub mod error;
pub mod error_utils;
pub mod governance;
pub mod mcp;
pub mod providers;
pub mod security;
pub mod streaming;
pub mod types;
// pub mod wip_integration; // TODO: Implement WIP integration

// Test helpers are always available (not gated by features)
// They provide mock client factories that work without external dependencies
#[cfg(test)]
pub mod test_helpers;

// Re-export main types for convenience
pub use agents::{Agent, AgentHealth, AgentInput, AgentOutput, AgentRegistry, HealthStatus};
pub use cache::{CacheConfig, CacheStats, LlmCache};
pub use cli::{
    add_llm_args, create_client_from_args, create_client_with_config, extract_llm_config,
    CliConfigBuilder,
};
pub use client::{GenAiClient, LlmChunk, LlmClient, LlmConfig, LlmResponse, UsageStats};
pub use config::{get_global_config, init_global_config, AiConfig, GlobalLlmConfig, LlmProvider};
pub use error::{GgenAiError, Result};
pub use generators::{
    OntologyGenerator, QualityMetrics, RefactorAssistant, SparqlGenerator, TemplateGenerator,
    TemplateValidator, ValidationIssue,
};
pub use providers::adapter::{MockClient, ollama_default_config, ollama_qwen3_coder_config};
pub use security::{MaskApiKey, SecretString};
pub use streaming::StreamConfig;

// Note: Use LlmClient::complete_stream() for streaming - it uses genai's native streaming
// which supports all major providers (OpenAI, Anthropic, Gemini, Ollama, etc.)
// NOTE: Ultrathink swarm types are implemented in ggen-mcp, not ggen-ai
// pub use ultrathink::{UltrathinkSwarm, UltrathinkConfig, UltrathinkTask, SwarmAgent, WipEntry, initialize_ultrathink_swarm};
pub use autonomous::{
    DeltaDetector, EvolutionConfig, EvolutionResult, GraphEvolutionEngine, NaturalLanguageParser,
    SelfValidator,
};
// pub use swarm::{SwarmAgent, SwarmCoordinator, SwarmConfig};
pub use governance::{
    ApprovalWorkflow, AuditTrail, Dashboard, Decision, DecisionOutcome, GovernanceConfig,
    GovernanceCoordinator, Policy, PolicyEngine, SafetyController,
};
pub use types::{DecisionId, PolicyId, RequestId, RuleId};
// pub use wip_integration::{WipConnector, WipConfig, WipEvent, WipEventType, WipResponse, load_wip_config};

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
