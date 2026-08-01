//! v26.8.1 G-close-unknowns evidence for the `verification` subsystem
//! (`.ggen/v26.8.1/subsystem-evidence-manifest.json`'s `verification`
//! record).
//!
//! `verification` is about the repo's OWN verification apparatus being
//! itself verified, not about any one crate's business logic. The real,
//! checkable claim this file proves: `ggen-cheat-scanner`'s AST rules
//! actually catch a known-bad pattern planted fresh in this test file
//! (not one of its pre-existing `tests/fixtures/*.rs` files, so this is
//! independent of that fixture set), and do not false-positive on a
//! genuinely good test written the same way. No mocks: `scan_source` is
//! called directly against real, freshly-constructed Rust source text and
//! its real `syn`-based AST walk.
//!
//! Positive witness: `verification_scanner_detects_a_freshly_planted_cheat_pattern`.
//! Negative falsifier (fn name contains "rejects", matching
//! `subsystem_evidence_manifest.py`'s `NEGATIVE_CONTROL_PATTERN`, so the
//! manifest generator classifies it as a true negative control, not a
//! second positive witness): `verification_scanner_rejects_a_false_positive_on_clean_code`.

use ggen_cheat_scanner::scan_source;
use std::path::PathBuf;

fn rule_ids(findings: &[ggen_cheat_scanner::Finding]) -> Vec<&'static str> {
    findings.iter().map(|f| f.rule_id).collect()
}

/// A deliberately planted CHEAT-T01 (vacuous-assert) violation, written
/// fresh in this file rather than reused from `tests/fixtures/` -- proves
/// the scanner's detection is not merely re-confirming one hardcoded
/// fixture the scanner was tuned against.
const PLANTED_BAD_TEST: &str = r#"
#[test]
fn some_feature_definitely_works() {
    let _result = compute_something();
    assert!(true);
}
"#;

/// A genuinely good test with a real, content-sensitive assertion -- must
/// NOT be flagged.
const PLANTED_GOOD_TEST: &str = r#"
#[test]
fn compute_something_returns_expected_sum() {
    let result = compute_something();
    assert_eq!(result, 42);
}
"#;

#[test]
fn verification_scanner_detects_a_freshly_planted_cheat_pattern() {
    let path = PathBuf::from("verification_subsystem_evidence_test::planted_bad");
    let findings = scan_source(PLANTED_BAD_TEST, &path);
    assert!(
        rule_ids(&findings).contains(&"CHEAT-T01"),
        "expected the real ggen-cheat-scanner AST walk to flag a freshly \
         planted assert!(true) test as CHEAT-T01, got: {findings:?}"
    );
}

#[test]
fn verification_scanner_rejects_a_false_positive_on_clean_code() {
    let path = PathBuf::from("verification_subsystem_evidence_test::planted_good");
    let findings = scan_source(PLANTED_GOOD_TEST, &path);
    assert!(
        findings.is_empty(),
        "did not expect the scanner to flag a real assert_eq! comparison \
         against a concrete expected value, got: {findings:?}"
    );
}

/// Sabotage-adjacent control: prove the CHEAT-T01 detector is actually
/// sensitive to the specific `assert!(true)` shape and not to `#[test]`
/// presence in general -- a test with no assertions at all is a *different*
/// rule (CHEAT-T03), so CHEAT-T01 specifically must stay silent on it.
#[test]
fn verification_scanner_does_not_conflate_t01_with_a_missing_assertion() {
    let path = PathBuf::from("verification_subsystem_evidence_test::no_assert_at_all");
    let src = r#"
#[test]
fn some_feature_probably_works() {
    let _ = compute_something();
}
"#;
    let findings = scan_source(src, &path);
    assert!(
        !rule_ids(&findings).contains(&"CHEAT-T01"),
        "CHEAT-T01 (vacuous-assert) must not fire on a test with no assert \
         at all (that is CHEAT-T03's job), got: {findings:?}"
    );
}
