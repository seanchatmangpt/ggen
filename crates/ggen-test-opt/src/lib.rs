//! Test optimization and selection tooling for ggen
//!
//! This crate provides test value scoring, 80/20 Pareto selection, parallel execution,
//! and budget enforcement for the ggen test suite. Part of Feature 004: Test Quality Audit
//! and Performance Optimization.
//!
//! # Overview
//!
//! - Test value scoring algorithm (failure frequency + coverage + speed + criticality)
//! - 80/20 Pareto selection (1,178 tests → 200 high-value tests)
//! - Parallel execution with rayon and cargo-nextest
//! - Performance budget enforcement (unit: ≤1s, integration: ≤10s)
//!
//! # Usage
//!
//! ```rust,no_run
//! use ggen_test_opt::OptResult;
//!
//! // Coming soon: Actual implementation
//! ```

#![deny(warnings)]
#![deny(unsafe_code)]
#![deny(clippy::unwrap_used)]
#![deny(clippy::expect_used)]
#![deny(clippy::panic)]

// Core modules
pub mod test_value_scorer;
pub mod types;

// Re-export main types and structs
pub use test_value_scorer::TestValueScorer;
pub use types::*;
