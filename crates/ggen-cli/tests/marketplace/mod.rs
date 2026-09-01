#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
//! Marketplace test suite entry point (`crates/ggen-cli/tests/marketplace_registry_test.rs`
//! and `crates/ggen-cli/tests/marketplace_comprehensive.rs` both `mod marketplace;` this
//! file as their harness body).
//!
//! ## 2026-08-24: seven v1/v2-transitional modules archived, not repaired
//!
//! `docs/jira/v26.8.16/04-MARKETPLACE-TEST-SUITE-DISABLED.md` — the seven modules this
//! file used to `pub mod` in commented-out form (`install_tests`, `registry_tests`,
//! `fixtures`, `integration`, `performance`, `security`, `unit`) targeted an architecture
//! that no longer exists on disk: `ggen_core` (fully deleted), `ggen_cli_lib::domain::
//! marketplace::registry` (no such module), `ggen_marketplace_v2` (never a workspace
//! member), and the `ggen marketplace` CLI noun (renamed to `ggen pack`). Porting them
//! would be a rewrite, not a repair — see `crates/ggen-cli/tests/archive/marketplace/
//! README.md` for the per-file breakdown. They are preserved (non-deletion doctrine)
//! under `tests/archive/marketplace/`, excluded from every build (no `mod` references
//! them from anywhere reachable), and this module is intentionally empty of test code.
//!
//! Real, new coverage against the live `ggen pack` surface lives in
//! `crates/ggen-cli/tests/pack_cli_test.rs` instead (a standalone top-level harness, not
//! wired through this file).
