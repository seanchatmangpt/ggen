//! Reverse synchronization - Code to RDF extraction
//!
//! This module provides functionality to extract service definitions from source code
//! in multiple languages (Rust, Elixir, Go) and convert them into RDF format for
//! integration with the ggen ontology system.
//!
//! The formal inverse pipeline (μ⁻¹) is exposed via [`inverse_pipeline`].

pub mod ast_extractor;
pub mod inverse_pipeline;

pub use ast_extractor::{
    convert_to_rdf, extract_elixir_genserver, extract_go_service, extract_rust_service,
    extract_bounds_from_type_params, Field, Language, Method, ServiceDef,
};
pub use inverse_pipeline::{
    InversePipeline, InversePipelineError, InverseReceipt, InverseReceiptChain, InverseResult,
    InverseStage,
};
