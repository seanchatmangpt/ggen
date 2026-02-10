//! Prompt manufacturing via CONSTRUCT queries
//!
//! This crate provides deterministic prompt compilation from RDF ontologies.
//! Prompts are compiled, not written by hand. Same input → same prompt.
//!
//! # Architecture
//!
//! - `ir` - Intermediate representation (structured, pre-shaped)
//! - `emitter` - Deterministic Tera emission
//! - `validator` - Prompt schema validation
//! - `hash` - Prompt hash for deduplication
//!
//! # Examples
//!
//! ```no_run
//! use ggen_prompt_mfg::{PromptCompiler, Result};
//!
//! # fn example() -> Result<()> {
//! let compiler = PromptCompiler::new()?;
//! let prompt = compiler.compile_from_construct("SELECT * WHERE { ?s ?p ?o }")?;
//! # Ok(())
//! # }
//! ```

use std::fmt;

pub mod emitter;
pub mod hash;
pub mod ir;
pub mod validator;

/// Error types for prompt manufacturing
#[derive(Debug, thiserror::Error)]
pub enum PromptError {
    /// SPARQL query error
    #[error("SPARQL error: {0}")]
    Sparql(String),

    /// Template compilation error
    #[error("Template error: {0}")]
    Template(String),

    /// Validation error
    #[error("Validation error: {0}")]
    Validation(String),

    /// Hash computation error
    #[error("Hash error: {0}")]
    Hash(String),

    /// IO error
    #[error("IO error: {0}")]
    Io(#[from] std::io::Error),

    /// Serialization error
    #[error("Serialization error: {0}")]
    Serialization(String),

    /// Invalid prompt structure
    #[error("Invalid prompt: {0}")]
    Invalid(String),
}

/// Result type for prompt manufacturing operations
pub type Result<T> = std::result::Result<T, PromptError>;

/// Prompt compiler - transforms RDF ontologies into deterministic prompts
///
/// # Guarantees
///
/// - Same input → same prompt (deterministic)
/// - Reproducible across runs
/// - Cryptographic hash for deduplication
/// - Schema validation enforced
pub struct PromptCompiler {
    emitter: emitter::PromptEmitter,
    validator: validator::PromptValidator,
}

impl PromptCompiler {
    /// Create a new prompt compiler
    ///
    /// # Errors
    ///
    /// Returns error if Tera templates cannot be initialized
    pub fn new() -> Result<Self> {
        Ok(Self {
            emitter: emitter::PromptEmitter::new()?,
            validator: validator::PromptValidator::new(),
        })
    }

    /// Compile a prompt from a CONSTRUCT query
    ///
    /// # Arguments
    ///
    /// * `construct_query` - SPARQL CONSTRUCT query that shapes the prompt IR
    ///
    /// # Errors
    ///
    /// Returns error if query fails, validation fails, or emission fails
    pub fn compile_from_construct(&self, construct_query: &str) -> Result<CompiledPrompt> {
        // Parse CONSTRUCT query → extract prompt IR
        let prompt_ir = ir::PromptIR::from_construct(construct_query)?;

        // Validate prompt structure
        self.validator.validate(&prompt_ir)?;

        // Emit deterministic prompt via Tera
        let content = self.emitter.emit(&prompt_ir)?;

        // Compute cryptographic hash
        let hash = hash::compute_prompt_hash(&content)?;

        Ok(CompiledPrompt {
            content,
            hash,
            ir: prompt_ir,
        })
    }

    /// Compile from RDF graph store
    ///
    /// # Arguments
    ///
    /// * `store` - Oxigraph store containing ontology
    /// * `construct_query` - SPARQL CONSTRUCT query
    ///
    /// # Errors
    ///
    /// Returns error if query execution or compilation fails
    pub fn compile_from_store(
        &self,
        store: &oxigraph::store::Store,
        construct_query: &str,
    ) -> Result<CompiledPrompt> {
        let prompt_ir = ir::PromptIR::from_store(store, construct_query)?;
        self.validator.validate(&prompt_ir)?;
        let content = self.emitter.emit(&prompt_ir)?;
        let hash = hash::compute_prompt_hash(&content)?;

        Ok(CompiledPrompt {
            content,
            hash,
            ir: prompt_ir,
        })
    }
}

impl Default for PromptCompiler {
    fn default() -> Self {
        Self::new().expect("Failed to initialize PromptCompiler")
    }
}

/// Compiled prompt output
///
/// Contains the final prompt text, its cryptographic hash, and the intermediate representation
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CompiledPrompt {
    /// Final prompt text (deterministic)
    pub content: String,

    /// Cryptographic hash for deduplication
    pub hash: String,

    /// Intermediate representation
    pub ir: ir::PromptIR,
}

impl CompiledPrompt {
    /// Get the prompt content
    pub fn content(&self) -> &str {
        &self.content
    }

    /// Get the prompt hash
    pub fn hash(&self) -> &str {
        &self.hash
    }

    /// Get the intermediate representation
    pub fn ir(&self) -> &ir::PromptIR {
        &self.ir
    }
}

impl fmt::Display for CompiledPrompt {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        write!(f, "{}", self.content)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_compiler_initialization() {
        let compiler = PromptCompiler::new();
        assert!(compiler.is_ok());
    }

    #[test]
    fn test_default_compiler() {
        let _compiler = PromptCompiler::default();
    }
}
