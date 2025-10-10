//! AI-powered generators for ggen

pub mod template;
pub mod sparql;
pub mod ontology;
pub mod refactor;
pub mod validator;

// Re-export generator types
pub use template::TemplateGenerator;
pub use sparql::SparqlGenerator;
pub use ontology::OntologyGenerator;
pub use refactor::RefactorAssistant;
pub use validator::{TemplateValidator, ValidationResult, ValidationIssue, QualityMetrics};

