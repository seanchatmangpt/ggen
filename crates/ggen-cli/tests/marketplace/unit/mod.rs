#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic, clippy::needless_raw_string_hashes, clippy::duration_suboptimal_units, clippy::branches_sharing_code, clippy::used_underscore_binding, clippy::single_char_pattern, clippy::ignore_without_reason, clippy::cloned_ref_to_slice_refs, clippy::doc_overindented_list_items, clippy::match_wildcard_for_single_variants, clippy::ignored_unit_patterns, clippy::needless_collect, clippy::unnecessary_map_or, clippy::manual_flatten, clippy::manual_strip, clippy::future_not_send, clippy::unnested_or_patterns, clippy::no_effect_underscore_binding, clippy::literal_string_with_formatting_args)]
//! Unit tests for marketplace domain logic
//!
//! This module contains unit tests following London School TDD:
//! - Test individual components in isolation
//! - Use mocks/stubs for dependencies
//! - Fast execution (<100ms per test)
//! - Clear arrange-act-assert structure

// Existing tests
mod maturity_scoring_test;
mod package_filtering_test;
mod search_ranking_test;

// V2 migration tests
mod adapter_conversion_test;
mod rdf_mapping_test;
