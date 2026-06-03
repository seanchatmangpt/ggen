#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic, clippy::needless_raw_string_hashes, clippy::duration_suboptimal_units, clippy::branches_sharing_code, clippy::used_underscore_binding, clippy::single_char_pattern, clippy::ignore_without_reason, clippy::cloned_ref_to_slice_refs, clippy::doc_overindented_list_items, clippy::match_wildcard_for_single_variants, clippy::ignored_unit_patterns, clippy::needless_collect, clippy::unnecessary_map_or, clippy::manual_flatten, clippy::manual_strip, clippy::future_not_send, clippy::unnested_or_patterns, clippy::no_effect_underscore_binding, clippy::literal_string_with_formatting_args)]
use ggen_cli_lib::domain::utils::*;

#[tokio::test]
async fn test_run_diagnostics() {
    let result = run_diagnostics().await;
    assert!(result.is_ok());
    let report = result.unwrap();
    // At least one tool should be available on test system
    assert!(report.rust_installed || report.cargo_installed || report.git_installed);
}

#[test]
fn test_check_tool() {
    // Test with a command that should exist
    let result = check_tool("ls");
    // On Unix systems, ls should exist
    #[cfg(unix)]
    assert!(result);
}
