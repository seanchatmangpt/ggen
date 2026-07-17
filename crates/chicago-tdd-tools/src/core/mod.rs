//! Core Testing Infrastructure
//!
//! Foundational testing primitives that all tests use: fixtures, builders,
//! assertions, macros, state management, compile-time assertions, alert helpers,
//! and common test utilities.
//!
//! ## Fail-Fast Hardening
//!
//! The `invariants` and `fail_fast` modules provide strict, zero-tolerance verification
//! of framework invariants. Every invariant violation results in immediate test failure.
//! No graceful degradation, no warnings that are ignored.

pub mod alert;
pub mod assertions;
pub mod async_fixture;
pub mod builders;
pub mod config;
pub mod const_assert;
pub mod contract;
/// Strict verification pipeline with fail-fast semantics for all 12 phases.
pub mod fail_fast;
pub mod fixture;
pub mod governance;
/// Property-based tests validating invariant detection using proptest.
pub mod invariant_properties;
/// Unrecoverable invariant violations - core type system for hardening.
pub mod invariants;
pub mod macros;
pub mod poka_yoke;

// Note: poka_yoke is NOT re-exported via glob to avoid conflicts with
// poka_yoke modules in otel and testcontainers features
pub mod receipt;
pub mod state;
pub mod test_utils;
pub mod type_level;
pub mod verification_pipeline;

// Re-export commonly used items
pub use alert::*;
pub use assertions::*;
#[cfg(feature = "async")]
pub use async_fixture::*;
pub use builders::*;
pub use const_assert::*;
pub use contract::*;
pub use fail_fast::*;
pub use fixture::*;
pub use governance::*;
pub use invariant_properties::helpers;
pub use invariants::*;
// poka_yoke types are accessed via core::poka_yoke::* to avoid glob conflicts
pub use receipt::*;
pub use state::*;
pub use test_utils::*;
pub use type_level::*;
pub use verification_pipeline::*;
