//! # ggen v6: Fully-Rendered Libraries via Ontology-First Compilation
//!
//! This module implements the v6 constitutional law: **A = μ(O)**
//!
//! ## Core Concepts
//!
//! - **O (Ontology Substrate)**: The only authored input (ontologies, constraints, manifests)
//! - **μ (Projection Function)**: Deterministic transformation function
//! - **A (Artifacts)**: Output files (never edited directly, only projected)
//! - **Λ (Pass Ordering)**: Total ordering of projection passes
//! - **H (Guards)**: Forbidden output class constraints
//!
//! ## Staged Compilation Pipeline
//!
//! The projection function μ is implemented as a sequence of passes:
//!
//! 1. **μ₁: Normalization** - Ontology → Ontology (CONSTRUCT rewrites)
//! 2. **μ₂: Extraction** - Ontology → Bindings (SELECT queries)
//! 3. **μ₃: Emission** - Bindings → Files (Tera templates)
//! 4. **μ₄: Canonicalization** - Files → Canonical Files (formatting)
//! 5. **μ₅: Receipt** - Files → Receipt (provenance binding)
//!
//! ## Constitutional Invariants
//!
//! - **Idempotence**: μ∘μ = μ
//! - **Determinism**: Same O always produces same A
//! - **Provenance**: hash(A) = hash(μ(O))
//! - **No Edit**: A is never modified, only regenerated
//! - **Substrate Only**: Only O is authored
//!
//! ## Quick Start
//!
//! ```rust,no_run
//! use ggen_core::v6::{StagedPipeline, PipelineConfig, Epoch};
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! // Create pipeline configuration
//! let config = PipelineConfig::new("project", "1.0.0")
//!     .with_ontology("ontology/domain.ttl")
//!     .with_output_dir("src/generated");
//!
//! // Build and run pipeline
//! let mut pipeline = StagedPipeline::new(config)?;
//! let receipt = pipeline.run()?;
//!
//! // Verify the projection
//! assert!(receipt.is_valid());
//! # Ok(())
//! # }
//! ```

// Core types
pub mod epoch;
pub mod guard;
pub mod pass;
pub mod receipt;
pub mod vocabulary;

// Staged compilation passes
pub mod passes;

// Pipeline orchestration
pub mod pipeline;

// Re-exports
pub use epoch::{Epoch, EpochId, OntologyInput};
pub use guard::{Guard, GuardAction, GuardViolation, PathGuard, SecretGuard};
pub use pass::{Pass, PassContext, PassExecution, PassResult, PassType};
pub use pipeline::{PipelineConfig, StagedPipeline, VerifyMode};
pub use receipt::{BuildReceipt, OutputFile};
pub use vocabulary::{AllowedVocabulary, ForbiddenVocabulary, VocabularyRegistry};

// Pass implementations
pub use passes::{
    CanonicalizationPass, EmissionPass, ExtractionPass, ExtractionReceipt, NormalizationPass,
    ParallelStats, QueryExecution, ReceiptGenerationPass, TensorQuery,
};
