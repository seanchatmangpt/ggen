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
//! - `rdf` - RDF metadata operations
//! - `audit` - Security auditing
//! - `ci` - CI/CD operations
//! - `shell` - Shell completion generation

#![deny(warnings)] // Poka-Yoke: Prevent warnings at compile time - compiler enforces correctness

pub mod ai;
pub mod audit;
pub mod ci;
pub mod graph;
pub mod mape_k;
#[cfg(feature = "marketplace-v2")]
pub mod marketplace;
pub mod ontology;
#[cfg(feature = "marketplace-v2")]
pub mod packs;
pub mod project;
pub mod rdf;
pub mod shell;
pub mod template;
pub mod utils;

// AHI (Autonomic Hyper-Intelligence) subsystem
pub mod ahi_contract;
pub mod auto_promotion_pipeline;
pub mod doctrine_engine;
#[cfg(feature = "marketplace-v2")]
pub mod marketplace_scorer;
pub mod ontology_proposal_engine;
pub mod proof_carrier;

// AHI Type-System Hardening (Phase 1-5)
// Makes governance impossible-to-violate at compile time
pub mod action_types; // Phase 1: Type-indexed actions (Risk, TickBudget, Mutation)
pub mod capability_system; // Phase 3: Capability-based effects
pub mod proof_types; // Phase 4: Proof-carrying decisions
pub mod swarm_coordination;
pub mod temporal_fabric; // Phase 2: MAPE-K typestate + causality // Phase 5: Lock-free snapshots + conflict-free aggregation

// Re-export commonly used types for convenience
pub use ggen_utils::error::{Error, Result};
