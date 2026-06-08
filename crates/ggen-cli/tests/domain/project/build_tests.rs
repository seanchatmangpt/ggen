#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic, clippy::needless_raw_string_hashes, clippy::duration_suboptimal_units, clippy::branches_sharing_code, clippy::used_underscore_binding, clippy::single_char_pattern, clippy::ignore_without_reason, clippy::cloned_ref_to_slice_refs, clippy::doc_overindented_list_items, clippy::match_wildcard_for_single_variants, clippy::ignored_unit_patterns, clippy::needless_collect, clippy::unnecessary_map_or, clippy::manual_flatten, clippy::manual_strip, clippy::future_not_send, clippy::unnested_or_patterns, clippy::no_effect_underscore_binding, clippy::literal_string_with_formatting_args)]
use ggen_cli_lib::domain::project::*;
use tempfile::TempDir;

#[tokio::test]
async fn test_build_project_success() {
    let temp_dir = TempDir::new().unwrap();
    let result = build_project(temp_dir.path()).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_build_project_nonexistent() {
    let result = build_project(std::path::Path::new("/nonexistent")).await;
    assert!(result.is_err());
}

#[tokio::test]
async fn test_clean_project() {
    let temp_dir = TempDir::new().unwrap();
    let target_dir = temp_dir.path().join("target");
    std::fs::create_dir(&target_dir).unwrap();

    let result = clean_project(temp_dir.path()).await;
    assert!(result.is_ok());
    assert!(!target_dir.exists());
}
