//! Required negative-fixture tests: for each of the 4 rules, prove the
//! scanner actually catches a real example of the violation (positive
//! fixture flags the rule) and does not false-positive on a corresponding
//! clean fixture (mirrors the fixture-test discipline in
//! `~/bcinr/tools/bcinr-cheat-scanner`'s `tests/test_scanner.rs`).

use ggen_cheat_scanner::{collect_impls, find_mock_substitutes, scan_source};
use std::fs;
use std::path::Path;

fn fixture(name: &str) -> (String, std::path::PathBuf) {
    let path = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests/fixtures")
        .join(name);
    let src = fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("failed to read fixture {}: {e}", path.display()));
    (src, path)
}

fn rule_ids(findings: &[ggen_cheat_scanner::Finding]) -> Vec<&'static str> {
    findings.iter().map(|f| f.rule_id).collect()
}

#[test]
fn cheat_t01_vacuous_assert_flags_positive_fixture() {
    let (src, path) = fixture("t01_vacuous_positive.rs");
    let findings = scan_source(&src, &path);
    assert!(
        rule_ids(&findings).contains(&"CHEAT-T01"),
        "expected CHEAT-T01 on vacuous assert!(true) test, got: {findings:?}"
    );
}

#[test]
fn cheat_t01_vacuous_assert_does_not_flag_clean_fixture() {
    let (src, path) = fixture("t01_vacuous_clean.rs");
    let findings = scan_source(&src, &path);
    assert!(
        !rule_ids(&findings).contains(&"CHEAT-T01"),
        "did not expect CHEAT-T01 on a real assert_eq! check, got: {findings:?}"
    );
}

#[test]
fn cheat_t02_tautological_result_check_flags_positive_fixture() {
    let (src, path) = fixture("t02_tautology_positive.rs");
    let findings = scan_source(&src, &path);
    assert!(
        rule_ids(&findings).contains(&"CHEAT-T02"),
        "expected CHEAT-T02 on `result.is_ok() || result.is_err()`, got: {findings:?}"
    );
}

#[test]
fn cheat_t02_tautological_result_check_does_not_flag_clean_fixture() {
    let (src, path) = fixture("t02_tautology_clean.rs");
    let findings = scan_source(&src, &path);
    assert!(
        !rule_ids(&findings).contains(&"CHEAT-T02"),
        "did not expect CHEAT-T02 on a single-branch assertion, got: {findings:?}"
    );
}

#[test]
fn cheat_t03_no_assertion_test_flags_positive_fixture() {
    let (src, path) = fixture("t03_no_assertion_positive.rs");
    let findings = scan_source(&src, &path);
    assert!(
        rule_ids(&findings).contains(&"CHEAT-T03"),
        "expected CHEAT-T03 on a test with no assert/unwrap/expect/panic, got: {findings:?}"
    );
}

#[test]
fn cheat_t03_no_assertion_test_does_not_flag_clean_fixture() {
    let (src, path) = fixture("t03_no_assertion_clean.rs");
    let findings = scan_source(&src, &path);
    assert!(
        !rule_ids(&findings).contains(&"CHEAT-T03"),
        "did not expect CHEAT-T03 on a test with a real assert_eq!, got: {findings:?}"
    );
}

#[test]
fn cheat_t04_mock_import_flags_positive_fixture() {
    let (src, path) = fixture("t04_mock_import_positive.rs");
    let findings = scan_source(&src, &path);
    assert!(
        rule_ids(&findings).contains(&"CHEAT-T04"),
        "expected CHEAT-T04 on `use mockall::mock` + #[automock], got: {findings:?}"
    );
}

#[test]
fn cheat_t04_mock_import_does_not_flag_clean_fixture() {
    let (src, path) = fixture("t04_mock_import_clean.rs");
    let findings = scan_source(&src, &path);
    assert!(
        !rule_ids(&findings).contains(&"CHEAT-T04"),
        "did not expect CHEAT-T04 on a real testcontainer merely named Mock*, got: {findings:?}"
    );
}

#[test]
fn cheat_t04_mock_substitute_flags_cross_file_positive_pair() {
    let (real_src, real_path) = fixture("t04_substitute_real.rs");
    let (mock_src, mock_path) = fixture("t04_substitute_mock.rs");
    let mut records = collect_impls(&real_src, &real_path);
    records.extend(collect_impls(&mock_src, &mock_path));
    let findings = find_mock_substitutes(&records);
    assert!(
        rule_ids(&findings).contains(&"CHEAT-T04"),
        "expected CHEAT-T04 when MockStorage substitutes for a trait RealStorage also implements, got: {findings:?}"
    );
    assert!(
        findings.iter().any(|f| f.file == mock_path),
        "finding should be anchored to the mock's own file, got: {findings:?}"
    );
}

#[test]
fn cheat_t04_mock_substitute_does_not_flag_lone_trait_shape_stub() {
    let (src, path) = fixture("t04_substitute_stub_clean.rs");
    let records = collect_impls(&src, &path);
    let findings = find_mock_substitutes(&records);
    assert!(
        findings.is_empty(),
        "did not expect CHEAT-T04 when MockOnly is the sole implementer of UniqueTrait, got: {findings:?}"
    );
}

#[test]
fn cheat_t03_does_not_flag_should_panic_test() {
    let (src, path) = fixture("t03_should_panic_clean.rs");
    let findings = scan_source(&src, &path);
    assert!(
        !rule_ids(&findings).contains(&"CHEAT-T03"),
        "did not expect CHEAT-T03 on a #[should_panic] test (it fails when no panic occurs), got: {findings:?}"
    );
}

#[test]
fn cheat_t03_does_not_flag_try_operator_result_test() {
    let (src, path) = fixture("t03_try_operator_clean.rs");
    let findings = scan_source(&src, &path);
    assert!(
        !rule_ids(&findings).contains(&"CHEAT-T03"),
        "did not expect CHEAT-T03 on a Result-returning test using `?` (fails on Err), got: {findings:?}"
    );
}

#[test]
fn cheat_t03_does_not_flag_assert_helper_delegation() {
    let (src, path) = fixture("t03_assert_helper_clean.rs");
    let findings = scan_source(&src, &path);
    assert!(
        !rule_ids(&findings).contains(&"CHEAT-T03"),
        "did not expect CHEAT-T03 on a test delegating to an assert_* helper fn, got: {findings:?}"
    );
}
