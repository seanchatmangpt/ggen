//! AI-powered generators for ggen
//!
//! This module provides AI-powered code and content generators that use LLMs
//! to create templates, ontologies, SPARQL queries, and assist with code refactoring.
//!
//! ## Generators
//!
//! - **TemplateGenerator**: Generate ggen templates from natural language descriptions
//! - **OntologyGenerator**: Generate RDF/OWL ontologies from domain descriptions
//! - **SparqlGenerator**: Generate SPARQL queries from natural language intents
//! - **NaturalSearchGenerator**: Natural language search query generation
//! - **RefactorAssistant**: AI-assisted code refactoring suggestions
//! - **TemplateValidator**: Validate and improve template quality
//!
//! ## Features
//!
//! - **Multi-provider Support**: Works with any LLM provider configured in ggen-ai
//! - **Caching**: Intelligent response caching to reduce API costs
//! - **Quality Metrics**: Template quality scoring and validation
//! - **Error Handling**: Comprehensive error handling with actionable messages

pub mod natural_search;
pub mod ontology;
pub mod refactor;
pub mod sparql;
pub mod template;
pub mod validator;

// Re-export generator types
pub use natural_search::NaturalSearchGenerator;
pub use ontology::OntologyGenerator;
pub use refactor::RefactorAssistant;
pub use sparql::SparqlGenerator;
pub use template::TemplateGenerator;
pub use validator::{QualityMetrics, TemplateValidator, ValidationIssue, ValidationResult};
