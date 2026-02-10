//! Staged compilation pass implementations
//!
//! This module provides the standard v6 pass implementations:
//!
//! - **μ₁: Normalization** - CONSTRUCT-based ontology rewrites
//! - **μ₂: Extraction** - CONSTRUCT queries to IR graph generation
//! - **μ₃: Emission** - Tera template rendering to files
//! - **μ₄: Canonicalization** - Code formatting
//! - **μ₅: Receipt** - Provenance binding

mod canonicalization;
mod emission;
mod extraction;
mod normalization;
mod receipt_gen;

pub use canonicalization::CanonicalizationPass;
pub use emission::{EmissionPass, EmissionReceipt, EmissionRule, EmittedFile};
pub use extraction::{
    ExtractionPass, ExtractionReceipt, ParallelStats, QueryExecution, TensorQuery,
};
pub use normalization::{NormalizationPass, NormalizationRule};
pub use receipt_gen::ReceiptGenerationPass;
