//! Chicago court: ggen-layout-court bin subject (Hop-5 live consume fence).
//!
//! Falsifier:
//! `CARGO_TARGET_DIR=/tmp/ggen-layout-court-bin CARGO_BUILD_JOBS=1 cargo test -p ggen-layout-court --test layout_court_chicago`

use std::path::PathBuf;

use ggen_layout_court::{
    metrics::{parse_product_subject, run_product_subject, VIEWPORTS},
    parse_cli_args, run_subject_json, run_subject_path, CourtStanding, EXIT_PASS, EXIT_REFUSED,
    EXIT_SUBJECT_MISSING,
};

fn fixture(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("fixtures")
        .join(name)
}

fn load(name: &str) -> String {
    let path = fixture(name);
    std::fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("expected fixture {}: {e}", path.display()))
}

#[test]
fn product_subject_fixture_admits_across_aot_viewports() {
    let json = load("product-subject.json");
    let subject = parse_product_subject(&json).expect("product-subject parses");
    let receipt = run_product_subject(&subject, VIEWPORTS).expect("green product admits");
    assert_eq!(receipt.root, "product");
    assert_eq!(receipt.viewports_px.len(), VIEWPORTS.len());
    assert_eq!(receipt.node_count, 5);
}

#[test]
fn product_subject_path_court_exits_alive() {
    let standing = run_subject_path(&fixture("product-subject.json"), VIEWPORTS);
    assert_eq!(standing.exit_code(), EXIT_PASS, "{standing:?}");
}

#[test]
fn column_fixture_consumes_aot_seal_alive() {
    let standing = run_subject_path(&fixture("ui-layout-valid.column.json"), VIEWPORTS);
    assert_eq!(standing.exit_code(), EXIT_PASS, "{standing:?}");
}

#[test]
fn missing_path_is_subject_missing() {
    let standing = run_subject_path(
        &PathBuf::from("/tmp/ggen-layout-court-NO-SUCH-SUBJECT.json"),
        VIEWPORTS,
    );
    assert_eq!(standing.exit_code(), EXIT_SUBJECT_MISSING);
}

#[test]
fn empty_subject_file_is_subject_missing() {
    let dir = std::env::temp_dir().join(format!(
        "ggen-layout-court-chicago-{}",
        std::process::id()
    ));
    let _ = std::fs::remove_dir_all(&dir);
    std::fs::create_dir_all(&dir).expect("tmpdir");
    let path = dir.join("empty.json");
    std::fs::write(&path, "   ").expect("write empty");
    let standing = run_subject_path(&path, VIEWPORTS);
    assert_eq!(standing.exit_code(), EXIT_SUBJECT_MISSING);
}

#[test]
fn mismatched_expected_digest_refuses() {
    let mut case: serde_json::Value =
        serde_json::from_str(&load("ui-layout-valid.column.json")).expect("parse");
    case["expected_seal_digest"] = serde_json::json!("0".repeat(64));
    let standing = run_subject_json(&case.to_string(), VIEWPORTS);
    assert_eq!(standing.exit_code(), EXIT_REFUSED, "{standing:?}");
}

#[test]
fn cli_parse_requires_subject() {
    let err = parse_cli_args(&[]).expect_err("no subject");
    assert_eq!(err.exit_code(), EXIT_SUBJECT_MISSING);
}

#[test]
fn bin_layout_court_source_exists_for_adapter_probe() {
    let src = PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("src/bin/layout_court.rs");
    assert!(
        src.is_file(),
        "zcode UiLayoutCourtAdapter requires {}",
        src.display()
    );
}

#[test]
fn standing_vocabulary_is_closed() {
    let alive = CourtStanding::Alive {
        kind: "t",
        detail: "d".into(),
    };
    let refused = CourtStanding::Refused {
        code: "X".into(),
        detail: "d".into(),
    };
    let missing = CourtStanding::SubjectMissing {
        detail: "d".into(),
    };
    assert_eq!(alive.exit_code(), 0);
    assert_eq!(refused.exit_code(), 1);
    assert_eq!(missing.exit_code(), 2);
}
