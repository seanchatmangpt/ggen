//! LLM-Construct Pattern - Orchestrate OWL → SHACL → DSPy pipeline
//!
//! This module provides high-level APIs to build LLM-Constructs from OWL ontologies.
//! LLM-Constructs are executable modules that enforce domain constraints from
//! formal ontologies (like FIBO) during LLM inference.
//!
//! ## Pipeline
//!
//! 1. **OWL Extraction**: Extract class definitions and restrictions from RDF ontology
//! 2. **SHACL Generation**: Transform OWL restrictions into SHACL validation shapes
//! 3. **DSPy Mapping**: Map SHACL constraints to DSPy field constraints
//! 4. **Code Generation**: Generate executable Rust modules (future phase)
//!
//! ## Example
//!
//! ```rust,no_run
//! use ggen_ai::llm_construct::{LLMConstructBuilder, LLMConstructSpec};
//! use oxigraph::store::Store;
//!
//! # fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let store = Store::new()?;
//! let mut builder = LLMConstructBuilder::new(store);
//!
//! let spec = LLMConstructSpec {
//!     name: "BondExtractor".to_string(),
//!     intent: "Extract bond data from documents".to_string(),
//!     source_ontology_path: "examples/fibo-bond.ttl".to_string(),
//!     target_class_uri: "http://example.com/Bond".to_string(),
//!     prompt_template: None,
//! };
//!
//! let construct = builder.build(spec)?;
//! println!("Generated {} DSPy fields", construct.dspy_fields.len());
//! # Ok(())
//! # }
//! ```

pub mod builder;
pub mod codegen;

pub use builder::{LLMConstruct, LLMConstructBuilder, LLMConstructSpec};
pub use codegen::LLMConstructCodeGen;
