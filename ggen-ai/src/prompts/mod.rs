//! Prompt engineering for AI-powered generators

pub mod template;
pub mod sparql;
pub mod code;

// Re-export prompt builders
pub use template::{TemplatePromptBuilder, TemplatePrompts};
pub use sparql::{SparqlPromptBuilder, SparqlPrompts};
pub use code::{CodePromptBuilder, CodePrompts};
