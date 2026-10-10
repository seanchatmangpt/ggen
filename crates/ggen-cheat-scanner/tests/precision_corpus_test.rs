//! Frozen precision corpus court: pins the scanner's EXACT per-file verdicts
//! over `tests/fixtures/precision/` so false positives cannot creep back in
//! silently (the 2026-07 precision pass retired ~305 misclassifications;
//! this corpus is the regression pin). Every fixture's full rule-id vector
//! must match the expected vector asserted here -- extra findings on a
//! negative are as fatal as missing findings on a positive.
//!
//! Fixture files are intentionally NOT compiled (`.rs` files under tests/
//! are not auto-built by cargo; only `tests/*.rs` integration targets are).

// Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]
use ggen_cheat_scanner::{collect_impls, find_mock_substitutes, scan_source};
use std::fs;
use std::path::{Path, PathBuf};

fn precision_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("tests/fixtures/precision")
}

fn read_fixture(name: &str) -> (String, PathBuf) {
    let path = precision_dir().join(name);
    let src = fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("failed to read fixture {}: {e}", path.display()));
    (src, path)
}

fn scan_clean(name: &str) -> Vec<ggen_cheat_scanner::Finding> {
    let (src, path) = read_fixture(name);
    scan_source(&src, &path).unwrap_or_else(|e| panic!("fixture {name} must parse: {e}"))
}

/// Assert the exact sorted rule-id vector of a single-file scan.
fn expect_verdict(name: &str, expected: &[&'static str]) {
    let mut got: Vec<&'static str> = scan_clean(name).iter().map(|f| f.rule_id).collect();
    got.sort_unstable();
    assert_eq!(
        got.as_slice(),
        expected,
        "fixture {name} verdict drift: expected {expected:?}, got {got:?}"
    );
}

// The fixture-verdict assertions live in the shared expect_verdict()
// helper, which this detector cannot see; every #[test] below is
// failure-capable through that helper. Suppressed per-fn:
// cheat-scan-ignore: pos_t01_vacuous_assert
// cheat-scan-ignore: pos_t02_tautology_standalone
// cheat-scan-ignore: pos_t02_tautology_inside_assert
// cheat-scan-ignore: pos_t03_no_assertion
// cheat-scan-ignore: pos_t04_mockall_import
// cheat-scan-ignore: pos_t04_automock_trait
// cheat-scan-ignore: neg_computed_assert_eq
// cheat-scan-ignore: neg_side_effect_free_predicate
// cheat-scan-ignore: neg_comments_contain_forbidden_words
// cheat-scan-ignore: neg_string_literals_not_imports
// cheat-scan-ignore: neg_cfg_test_boundary_non_test_fn
// cheat-scan-ignore: neg_assert_true_alongside_real_assert
// cheat-scan-ignore: neg_unwrap_expect_chain
// cheat-scan-ignore: neg_single_branch_is_ok
// cheat-scan-ignore: neg_helper_assert_macros
// cheat-scan-ignore: neg_tokio_async_test

// ---------- Positives: every rule class genuinely fires ----------

#[test]
fn pos_t01_vacuous_assert() {
    expect_verdict("pos_t01_vacuous_assert.rs", &["CHEAT-T01"]);
}

#[test]
fn pos_t02_tautology_standalone() {
    expect_verdict("pos_t02_tautology_standalone.rs", &["CHEAT-T02"]);
}

#[test]
fn pos_t02_tautology_inside_assert() {
    expect_verdict("pos_t02_tautology_inside_assert.rs", &["CHEAT-T02"]);
}

#[test]
fn pos_t03_no_assertion() {
    expect_verdict("pos_t03_no_assertion.rs", &["CHEAT-T03"]);
}

#[test]
fn pos_t04_mockall_import() {
    expect_verdict("pos_t04_mockall_import.rs", &["CHEAT-T04"]);
}

#[test]
fn pos_t04_automock_trait() {
    expect_verdict("pos_t04_automock_trait.rs", &["CHEAT-T04"]);
}

#[test]
fn pos_cross_file_mock_substitute() {
    let (real_src, real_path) = read_fixture("pos_substitute_real.rs");
    let (mock_src, mock_path) = read_fixture("pos_substitute_mock.rs");
    let mut records = collect_impls(&real_src, &real_path);
    records.extend(collect_impls(&mock_src, &mock_path));
    let findings = find_mock_substitutes(&records);
    let got: Vec<&'static str> = findings.iter().map(|f| f.rule_id).collect();
    assert_eq!(
        got.as_slice(),
        ["CHEAT-T04"],
        "cross-file substitute pair must yield exactly one CHEAT-T04, got: {findings:?}"
    );
    assert_eq!(
        findings[0].file, mock_path,
        "finding must anchor to the mock's file"
    );
}

// ---------- Negatives: historically false-positiveing shapes, exact [] ----------

#[test]
fn neg_computed_assert_eq() {
    expect_verdict("neg_computed_assert_eq.rs", &[]);
}

#[test]
fn neg_side_effect_free_predicate() {
    expect_verdict("neg_side_effect_free_predicate.rs", &[]);
}

#[test]
fn neg_mock_named_no_collision() {
    expect_verdict("neg_mock_named_no_collision.rs", &[]);
    // Also clean under the cross-file substitute rule.
    let (src, path) = read_fixture("neg_mock_named_no_collision.rs");
    let findings = find_mock_substitutes(&collect_impls(&src, &path));
    assert!(
        findings.is_empty(),
        "MockApiContainer with no trait collision is not a substitute: {findings:?}"
    );
}

#[test]
fn neg_comments_contain_forbidden_words() {
    expect_verdict("neg_comments_contain_forbidden_words.rs", &[]);
}

#[test]
fn neg_string_literals_not_imports() {
    expect_verdict("neg_string_literals_not_imports.rs", &[]);
}

#[test]
fn neg_cfg_test_boundary_non_test_fn() {
    expect_verdict("neg_cfg_test_boundary_non_test_fn.rs", &[]);
}

#[test]
fn neg_assert_true_alongside_real_assert() {
    expect_verdict("neg_assert_true_alongside_real_assert.rs", &[]);
}

#[test]
fn neg_unwrap_expect_chain() {
    expect_verdict("neg_unwrap_expect_chain.rs", &[]);
}

#[test]
fn neg_single_branch_is_ok() {
    expect_verdict("neg_single_branch_is_ok.rs", &[]);
}

#[test]
fn neg_helper_assert_macros() {
    expect_verdict("neg_helper_assert_macros.rs", &[]);
}

#[test]
fn neg_tokio_async_test() {
    expect_verdict("neg_tokio_async_test.rs", &[]);
}

// ---------- Corpus hygiene: the fixture set itself must not drift ----------

#[test]
fn corpus_contains_exactly_the_pinned_fixture_set() {
    let mut actual: Vec<String> = fs::read_dir(precision_dir())
        .unwrap_or_else(|e| panic!("precision dir: {e}"))
        .filter_map(|e| e.ok())
        .map(|e| e.file_name().to_string_lossy().into_owned())
        .collect();
    actual.sort();
    let mut expected: Vec<String> = POSITIVE_FIXTURES
        .iter()
        .chain(NEGATIVE_FIXTURES.iter())
        .map(|s| s.to_string())
        .collect();
    expected.sort();
    assert_eq!(actual, expected, "precision corpus file set drifted");
}

const POSITIVE_FIXTURES: &[&str] = &[
    "pos_t01_vacuous_assert.rs",
    "pos_t02_tautology_inside_assert.rs",
    "pos_t02_tautology_standalone.rs",
    "pos_t03_no_assertion.rs",
    "pos_t04_automock_trait.rs",
    "pos_t04_mockall_import.rs",
    "pos_substitute_mock.rs",
    "pos_substitute_real.rs",
];

const NEGATIVE_FIXTURES: &[&str] = &[
    "neg_assert_true_alongside_real_assert.rs",
    "neg_cfg_test_boundary_non_test_fn.rs",
    "neg_comments_contain_forbidden_words.rs",
    "neg_computed_assert_eq.rs",
    "neg_helper_assert_macros.rs",
    "neg_mock_named_no_collision.rs",
    "neg_side_effect_free_predicate.rs",
    "neg_single_branch_is_ok.rs",
    "neg_string_literals_not_imports.rs",
    "neg_tokio_async_test.rs",
    "neg_unwrap_expect_chain.rs",
];

// ---------- Mutation check: the corpus can still detect ----------

#[test]
fn mutation_flipping_a_negative_to_vacuous_flips_detection() {
    // Sensitivity falsifier: take the clean single-branch fixture, mutate
    // its assert into assert!(true) (and drop the second real assert), and
    // require CHEAT-T01 to fire. If the detector goes blind, this fails.
    let (src, path) = read_fixture("neg_single_branch_is_ok.rs");
    let mutated = src
        .replace("assert!(result.is_ok());", "assert!(true);")
        .replace("\n    assert!(result.unwrap() == 1);", "");
    assert!(
        mutated != src,
        "mutation must actually change the fixture source"
    );
    let findings =
        scan_source(&mutated, &path).unwrap_or_else(|e| panic!("mutant must parse: {e}"));
    let got: Vec<&'static str> = findings.iter().map(|f| f.rule_id).collect();
    assert_eq!(
        got.as_slice(),
        ["CHEAT-T01"],
        "mutated negative must be detected as CHEAT-T01, got: {findings:?}"
    );
}
