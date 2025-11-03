//! AI-powered generators for ggen

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
