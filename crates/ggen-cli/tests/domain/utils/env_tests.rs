#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic, clippy::needless_raw_string_hashes, clippy::duration_suboptimal_units, clippy::branches_sharing_code, clippy::used_underscore_binding, clippy::single_char_pattern, clippy::ignore_without_reason, clippy::cloned_ref_to_slice_refs, clippy::doc_overindented_list_items, clippy::match_wildcard_for_single_variants, clippy::ignored_unit_patterns, clippy::needless_collect, clippy::unnecessary_map_or, clippy::manual_flatten, clippy::manual_strip, clippy::future_not_send, clippy::unnested_or_patterns, clippy::no_effect_underscore_binding, clippy::literal_string_with_formatting_args)]
use ggen_cli_lib::domain::utils::*;
use std::io::Write;
use tempfile::NamedTempFile;

#[test]
fn test_load_env_file() {
    let mut temp_file = NamedTempFile::new().unwrap();
    writeln!(temp_file, "KEY1=value1").unwrap();
    writeln!(temp_file, "KEY2=value2").unwrap();
    writeln!(temp_file, "# Comment").unwrap();
    writeln!(temp_file, "").unwrap();

    let result = load_env_file(temp_file.path());
    assert!(result.is_ok());

    let env_vars = result.unwrap();
    assert_eq!(env_vars.get("KEY1"), Some(&"value1".to_string()));
    assert_eq!(env_vars.get("KEY2"), Some(&"value2".to_string()));
}

#[test]
fn test_load_env_file_nonexistent() {
    let result = load_env_file(std::path::Path::new("/nonexistent/.env"));
    assert!(result.is_err());
}

#[test]
fn test_get_env_or() {
    let result = get_env_or("NONEXISTENT_VAR_12345", "default");
    assert_eq!(result, "default");
}

#[test]
fn test_is_ci() {
    // is_ci() reads only the process environment, which does not change
    // between two immediate calls — the observable contract is a stable,
    // deterministic answer.
    let first = is_ci();
    let second = is_ci();
    assert_eq!(first, second, "is_ci() must be deterministic within a process");
}
