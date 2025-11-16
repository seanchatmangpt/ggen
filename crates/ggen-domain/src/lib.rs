//! # ggen-domain - Domain Logic Layer
//!
//! This crate contains all domain/business logic for ggen, completely separated
//! from CLI concerns. It provides pure business logic functions that can be used
//! by CLI, web APIs, or other interfaces.
//!
//! ## Architecture
//!
//! - **No CLI dependencies**: This crate has ZERO dependencies on clap or clap-noun-verb
//! - **Infrastructure dependencies**: Uses ggen-core, ggen-ai, ggen-marketplace for operations
//! - **Async by default**: Domain functions are async for non-blocking operations
//!
//! ## Module Organization
//!
//! Domain logic is organized by functional area:
//! - `ai` - AI operations (code analysis, generation)
//! - `graph` - Graph operations (RDF loading, SPARQL queries)
//! - `marketplace` - Marketplace operations (search, install, publish)
//! - `template` - Template operations (generate, lint, render)
//! - `project` - Project operations (create, generate, plan)
//! - `hook` - Hook management
//! - `utils` - Utility functions
//! - `rdf` - RDF metadata operations
//! - `audit` - Security auditing
//! - `ci` - CI/CD operations
//! - `shell` - Shell completion generation

#![deny(warnings)] // Poka-Yoke: Prevent warnings at compile time - compiler enforces correctness

pub mod ai;
pub mod audit;
pub mod ci;
pub mod graph;
pub mod hook;
pub mod marketplace;
pub mod project;
pub mod rdf;
pub mod shell;
pub mod template;
pub mod utils;
pub mod mape_k;

// AHI (Autonomic Hyper-Intelligence) subsystem
pub mod ahi_contract;
pub mod doctrine_engine;
pub mod ontology_proposal_engine;
pub mod marketplace_scorer;
pub mod proof_carrier;
pub mod auto_promotion_pipeline;

// Re-export commonly used types for convenience
pub use ggen_utils::error::{Error, Result};
