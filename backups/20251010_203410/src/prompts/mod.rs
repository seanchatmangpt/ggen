//! Prompt engineering for AI-powered generators

pub mod template;
pub mod sparql;
pub mod code;
pub mod ontology;

// Re-export prompt builders
pub use template::{TemplatePromptBuilder, TemplatePrompts};
pub use sparql::{SparqlPromptBuilder, SparqlPrompts};
pub use code::{CodePromptBuilder, CodePrompts};
pub use ontology::{OntologyPromptBuilder, OntologyPrompts};
