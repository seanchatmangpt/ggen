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
//! Unit tests for permission handling during installation

use ggen_cli_lib::validation_lib::security::{Permission, PermissionModel};
use std::path::Path;

#[test]
fn test_permission_validation() {
    let model = PermissionModel::new();

    // Path traversal should be rejected
    assert!(model
        .check_permission(Path::new("../dangerous"), Permission::Read)
        .is_err());

    // Safe relative path should be accepted
    assert!(model
        .check_permission(Path::new("src/lib.rs"), Permission::Read)
        .is_ok());
}
