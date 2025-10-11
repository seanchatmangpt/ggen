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

pub mod client;
pub mod config;
pub mod prompts;
pub mod generators;
pub mod autonomous;
pub mod ultrathink;
// Swarm module is implemented as a directory
// pub mod swarm;
pub mod mcp;
pub mod error;
pub mod providers;
pub mod cli;
pub mod security;
pub mod governance;
// pub mod wip_integration; // TODO: Implement WIP integration

#[cfg(feature = "ollama-integration")]
pub mod test_helpers;

// Re-export main types for convenience
pub use client::{LlmClient, LlmConfig, LlmResponse, LlmChunk, UsageStats};
pub use providers::adapter::{MockClient, OllamaClient, OpenAIClient, AnthropicClient};
pub use config::{AiConfig, GlobalLlmConfig, LlmProvider, get_global_config, init_global_config};
pub use cli::{CliConfigBuilder, extract_llm_config, create_client_from_args, create_client_with_config, add_llm_args};
pub use generators::{TemplateGenerator, SparqlGenerator, OntologyGenerator, RefactorAssistant, TemplateValidator, ValidationIssue, QualityMetrics};
pub use error::{GgenAiError, Result};
pub use security::{SecretString, MaskApiKey};
// NOTE: Ultrathink swarm types are implemented in ggen-mcp, not ggen-ai
// pub use ultrathink::{UltrathinkSwarm, UltrathinkConfig, UltrathinkTask, SwarmAgent, WipEntry, initialize_ultrathink_swarm};
pub use autonomous::{GraphEvolutionEngine, EvolutionConfig, EvolutionResult, NaturalLanguageParser, SelfValidator, DeltaDetector};
// pub use swarm::{SwarmAgent, SwarmCoordinator, SwarmConfig};
pub use governance::{
    GovernanceCoordinator, GovernanceConfig, Policy, PolicyEngine, AuditTrail,
    Dashboard, SafetyController, ApprovalWorkflow, Decision, DecisionOutcome,
};
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
