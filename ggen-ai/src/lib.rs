//! # ggen-ai
//!
//! AI-powered code generation capabilities for ggen.
//! 
//! This crate provides LLM integration for intelligent template generation,
//! SPARQL query construction, ontology generation, and code refactoring.
//!
//! ## Features
//!
//! - **Multi-provider LLM support**: OpenAI, Anthropic, Ollama
//! - **Intelligent template generation**: Natural language to ggen templates
//! - **SPARQL query generation**: Intent-based query construction
//! - **Ontology generation**: Domain descriptions to RDF/OWL
//! - **Code refactoring**: AI-assisted code improvement suggestions
//! - **MCP server integration**: Expose AI capabilities via Model Context Protocol
//!
//! ## Quick Start
//!
//! ```rust
//! use ggen_ai::{LlmClient, TemplateGenerator};
//! use ggen_ai::providers::MockClient;
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     // Initialize mock client for demonstration
//!     let client = MockClient::new(vec!["Generated template content".to_string()]);
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
pub mod providers;
pub mod prompts;
pub mod generators;
pub mod mcp;
pub mod error;

// Re-export main types for convenience
pub use client::{LlmClient, LlmAdapter};
pub use generators::{TemplateGenerator, SparqlGenerator, OntologyGenerator, RefactorAssistant};
pub use error::{GgenAiError, Result};

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
