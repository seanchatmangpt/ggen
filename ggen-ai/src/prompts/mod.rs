//! Prompt engineering for AI-powered generators

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
