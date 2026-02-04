//! # ggen-craftplan: RDF → Elixir Code Generation Pipeline
//!
//! This crate implements a five-stage deterministic pipeline (μ) that transforms
//! RDF ontology specifications into production-ready Elixir code for the Craftplan ERP system.
//!
//! ## Pipeline Architecture
//!
//! The transformation follows the equation: **A = μ(O)**
//!
//! Where:
//! - **A** = Generated Elixir code (Ash resources, Phoenix LiveViews, contexts)
//! - **μ** = Five-stage transformation pipeline
//! - **O** = RDF ontology (immutable source of truth)
//!
//! ### Pipeline Stages
//!
//! 1. **μ₁ (Normalize)**: RDF validation, SHACL shapes, dependency resolution
//! 2. **μ₂ (Extract)**: SPARQL queries, OWL inference, rule execution
//! 3. **μ₃ (Emit)**: Tera template rendering, code generation
//! 4. **μ₄ (Canonicalize)**: Deterministic formatting, content hashing
//! 5. **μ₅ (Receipt)**: Cryptographic proof generation, audit trail
//!
//! ## Design Principles
//!
//! - **Determinism**: Same RDF → identical Elixir code (byte-for-byte)
//! - **Type-Safety**: Result<T, E> throughout, no unwrap/expect in production
//! - **Chicago TDD**: State-based tests, real collaborators, AAA pattern
//! - **Poka-Yoke**: Compiler-enforced correctness via Rust's type system
//!
//! ## Usage
//!
//! ```rust,no_run
//! use ggen_craftplan::pipeline::CodeGenerator;
//!
//! # fn main() -> Result<(), Box<dyn std::error::Error>> {
//! let generator = CodeGenerator::new()?;
//! let elixir_code = generator.generate_from_rdf("ontology.ttl")?;
//! # Ok(())
//! # }
//! ```

pub mod error;
pub mod models;
pub mod normalize;
pub mod extract;
pub mod emit;
pub mod canonicalize;
pub mod receipt;
pub mod pipeline;

// Re-export key types for convenience
pub use error::{Result, CraftplanError};
pub use pipeline::CodeGenerator;
