//! # ggen-marketplace - Package marketplace system
//!
//! Marketplace plugin providing package management, discovery, and installation for ggen.

// Crate-level lint exceptions for marketplace implementation and test code:
// - expect_used: RwLock/Mutex expect() on poisoned locks is intentional (panics on invariant violation)
// - unwrap_used: test setup and iterator chaining in non-fallible contexts
// - panic: test assertions and unreachable code paths in tests
// - unwrap_err: test error-path assertions
// - into_iter: single-item iteration patterns
#![allow(clippy::expect_used, clippy::unwrap_used, clippy::panic)]

pub mod agent;
pub mod marketplace;
pub mod ontology_core;
pub mod packs;
pub mod packs_registry;
pub mod sync_profile;

pub use marketplace::{
    Manifest, Package, PackageId, QualityScore, RdfRegistry, SparqlSearchEngine,
};
