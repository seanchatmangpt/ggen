//! Staged compilation pass implementations
//!
//! This module provides the standard v6 pass implementations:
//!
//! - **μ₁: Normalization** - CONSTRUCT-based ontology rewrites
//! - **μ₂: Extraction** - SELECT queries to template bindings
//! - **μ₃: Emission** - Tera template rendering to files
//! - **μ₄: Canonicalization** - Code formatting
//! - **μ₅: Receipt** - Provenance binding

mod canonicalization;
mod emission;
mod extraction;
mod normalization;
mod receipt_gen;

pub use canonicalization::CanonicalizationPass;
pub use emission::{EmissionPass, EmissionRule};
pub use extraction::{ExtractionPass, ExtractionRule};
pub use normalization::{NormalizationPass, NormalizationRule};
pub use receipt_gen::ReceiptGenerationPass;
