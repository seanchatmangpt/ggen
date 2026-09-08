#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic, clippy::needless_raw_string_hashes, clippy::duration_suboptimal_units, clippy::branches_sharing_code, clippy::used_underscore_binding, clippy::single_char_pattern, clippy::ignore_without_reason, clippy::cloned_ref_to_slice_refs, clippy::doc_overindented_list_items, clippy::match_wildcard_for_single_variants, clippy::ignored_unit_patterns, clippy::needless_collect, clippy::unnecessary_map_or, clippy::manual_flatten, clippy::manual_strip, clippy::future_not_send, clippy::unnested_or_patterns, clippy::no_effect_underscore_binding, clippy::literal_string_with_formatting_args)]
//! Integration tests for marketplace CLI commands
//!
//! This module contains integration tests that verify:
//! - Complete command workflows
//! - Real CLI execution
//! - Output format validation
//! - Error handling and edge cases

// Existing tests
mod cli_commands_test;
mod edge_cases_test;

// V2 migration integration tests
mod backward_compat_test;
mod cross_backend_test;
mod v2_workflows_test;
