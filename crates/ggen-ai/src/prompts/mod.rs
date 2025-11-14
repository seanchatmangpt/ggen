//! Prompt engineering for AI-powered generators
//!
//! This module provides prompt builders and templates for various AI generation
//! tasks. It includes specialized prompt builders for code generation, ontology
//! creation, SPARQL query generation, and template creation.
//!
//! ## Prompt Builders
//!
//! - **CodePromptBuilder**: Prompts for code generation and refactoring
//! - **OntologyPromptBuilder**: Prompts for RDF/OWL ontology generation
//! - **SparqlPromptBuilder**: Prompts for SPARQL query generation
//! - **TemplatePromptBuilder**: Prompts for ggen template generation
//!
//! ## Features
//!
//! - **Template Loading**: Load prompt templates from files
//! - **Structured Prompts**: Type-safe prompt building with validation
//! - **Context Injection**: Inject context and examples into prompts
//! - **Multi-step Prompts**: Support for complex, multi-step generation workflows

pub mod code;
pub mod loader;
pub mod ontology;
pub mod sparql;
pub mod template;

// Re-export prompt builders
pub use code::{CodePromptBuilder, CodePrompts};
pub use loader::PromptTemplateLoader;
pub use ontology::{OntologyPromptBuilder, OntologyPrompts};
pub use sparql::{SparqlPromptBuilder, SparqlPrompts};
pub use template::{TemplatePromptBuilder, TemplatePrompts};
