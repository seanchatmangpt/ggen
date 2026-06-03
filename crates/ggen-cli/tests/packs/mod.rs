#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic, clippy::needless_raw_string_hashes, clippy::duration_suboptimal_units, clippy::branches_sharing_code, clippy::used_underscore_binding, clippy::single_char_pattern, clippy::ignore_without_reason, clippy::cloned_ref_to_slice_refs, clippy::doc_overindented_list_items, clippy::match_wildcard_for_single_variants, clippy::ignored_unit_patterns, clippy::needless_collect, clippy::unnecessary_map_or, clippy::manual_flatten, clippy::manual_strip, clippy::future_not_send, clippy::unnested_or_patterns, clippy::no_effect_underscore_binding, clippy::literal_string_with_formatting_args)]
//! Comprehensive test suite for ggen packs Phase 2-3
//!
//! This module contains 100+ tests covering:
//! - Installation system (download, extraction, verification, rollback)
//! - SPARQL execution
//! - Template generation
//! - Dependency resolution
//! - Registry operations
//! - Cloud distribution
//!
//! All tests are mapped to FMEA failure modes to ensure comprehensive coverage.

#[cfg(test)]
mod unit {
    pub mod installation;
}

#[cfg(test)]
mod integration;

#[cfg(test)]
mod performance;

#[cfg(test)]
mod security;

// Re-export test utilities
pub use unit::installation::*;
