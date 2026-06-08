#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic, clippy::needless_raw_string_hashes, clippy::duration_suboptimal_units, clippy::branches_sharing_code, clippy::used_underscore_binding, clippy::single_char_pattern, clippy::ignore_without_reason, clippy::cloned_ref_to_slice_refs, clippy::doc_overindented_list_items, clippy::match_wildcard_for_single_variants, clippy::ignored_unit_patterns, clippy::needless_collect, clippy::unnecessary_map_or, clippy::manual_flatten, clippy::manual_strip, clippy::future_not_send, clippy::unnested_or_patterns, clippy::no_effect_underscore_binding, clippy::literal_string_with_formatting_args)]
//! Integration tests for ggen-marketplace and lifecycle
//!
//! These tests validate end-to-end functionality including:
//! - Complete package publish/retrieve flows
//! - Multi-node registry scenarios
//! - Search engine integration with real tantivy index
//! - Clnrm-based test harness for isolated testing
//! - Lifecycle phase transitions and deployment workflows
//! - Production readiness validation

pub mod clnrm_harness;
pub mod clnrm_harness_examples;
pub mod end_to_end_flow;
pub mod marketplace_validation;
pub mod multi_node_scenario;
pub mod registry_api_integration;
pub mod search_integration;

// Lifecycle integration tests
pub mod lifecycle_clnrm_tests;
pub mod lifecycle_tests;

// Performance benchmarks
pub mod performance_benchmarks;
