//! Chicago court: Gate-1 `curve25519-dalek` lock pin policy.
//!
//! Admits exactly the documented dual majors `{4.1.3, 5.0.0}` from
//! `docs/rfc/v27.1.1/CURVE25519-DALEK-PIN-STANDING.md`. Any other version
//! set in root `Cargo.lock` is `REFUSED_UNDOCUMENTED_MAJOR` until the
//! standing doc + gap-matrix row are bumped in the same change.
//!
//! Falsifier:
//! `CARGO_BUILD_JOBS=1 CARGO_TARGET_DIR=/tmp/ggen-curve25519-court \
//!   cargo test -p ggen-config --test curve25519_dalek_pin_court --locked \
//!   -- --test-threads=1`
//!
//! Chicago TDD: reads the real workspace `Cargo.lock` (no mocks). Pure
//! adjudication helpers are unit-tested with fabricated lock snippets so
//! undocumented majors fail closed without mutating the live lock.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::collections::BTreeSet;
use std::path::PathBuf;

/// Exact admitted `curve25519-dalek` versions (standing §1 / §3).
const ADMITTED_CURVE25519_DALEK: &[&str] = &["4.1.3", "5.0.0"];

/// Standing doc that must cite the admitted set when the court passes.
const STANDING_DOC: &str = "docs/rfc/v27.1.1/CURVE25519-DALEK-PIN-STANDING.md";

#[derive(Debug, Clone, PartialEq, Eq)]
enum DalekPinStanding {
    Alive,
    RefusedUndocumentedMajor { observed: BTreeSet<String> },
}

impl DalekPinStanding {
    fn as_label(&self) -> &'static str {
        match self {
            Self::Alive => "ALIVE",
            Self::RefusedUndocumentedMajor { .. } => "REFUSED_UNDOCUMENTED_MAJOR",
        }
    }
}

fn workspace_root() -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn cargo_lock_path() -> PathBuf {
    workspace_root().join("Cargo.lock")
}

fn standing_doc_path() -> PathBuf {
    workspace_root().join(STANDING_DOC)
}

/// Parse every `[[package]]` block whose `name = "curve25519-dalek"` and
/// collect the `version = "..."` values. Ignores `curve25519-dalek-derive`.
fn parse_curve25519_dalek_versions(lock_text: &str) -> BTreeSet<String> {
    let mut versions = BTreeSet::new();
    let mut pending_name: Option<&str> = None;
    for line in lock_text.lines() {
        let trimmed = line.trim();
        if trimmed == "[[package]]" {
            pending_name = None;
            continue;
        }
        if let Some(rest) = trimmed.strip_prefix("name = \"") {
            if let Some(name) = rest.strip_suffix('"') {
                pending_name = Some(name);
            }
            continue;
        }
        if pending_name == Some("curve25519-dalek") {
            if let Some(rest) = trimmed.strip_prefix("version = \"") {
                if let Some(ver) = rest.strip_suffix('"') {
                    versions.insert(ver.to_string());
                    pending_name = None;
                }
            }
        }
    }
    versions
}

fn adjudicate(versions: &BTreeSet<String>) -> DalekPinStanding {
    let admitted: BTreeSet<String> = ADMITTED_CURVE25519_DALEK
        .iter()
        .map(|s| (*s).to_string())
        .collect();
    if *versions == admitted {
        DalekPinStanding::Alive
    } else {
        DalekPinStanding::RefusedUndocumentedMajor {
            observed: versions.clone(),
        }
    }
}

fn refusal_message(observed: &BTreeSet<String>) -> String {
    format!(
        "REFUSED_UNDOCUMENTED_MAJOR: curve25519-dalek lock versions {observed:?} \
         are not exactly the admitted set {ADMITTED_CURVE25519_DALEK:?}; bump \
         {STANDING_DOC} + GATE-1-GAP-MATRIX before admitting drift"
    )
}

#[test]
fn admitted_set_constant_is_exactly_4_1_3_and_5_0_0() {
    // Arrange / Act
    let admitted: BTreeSet<&str> = ADMITTED_CURVE25519_DALEK.iter().copied().collect();

    // Assert — pin the documented dual majors; any silent change fails here
    assert_eq!(
        admitted,
        BTreeSet::from(["4.1.3", "5.0.0"]),
        "ADMITTED_CURVE25519_DALEK must stay {{4.1.3, 5.0.0}} until standing bumps"
    );
}

#[test]
fn fixture_with_undocumented_6_x_is_refused() {
    // Arrange — fabricated lock introducing 6.0.0 alongside admitted majors
    let lock = r#"
[[package]]
name = "curve25519-dalek"
version = "4.1.3"

[[package]]
name = "curve25519-dalek"
version = "5.0.0"

[[package]]
name = "curve25519-dalek"
version = "6.0.0"

[[package]]
name = "curve25519-dalek-derive"
version = "0.1.1"
"#;
    let versions = parse_curve25519_dalek_versions(lock);

    // Act
    let standing = adjudicate(&versions);

    // Assert
    match &standing {
        DalekPinStanding::RefusedUndocumentedMajor { observed } => {
            assert!(observed.contains("6.0.0"));
            let msg = refusal_message(observed);
            assert!(
                msg.starts_with("REFUSED_UNDOCUMENTED_MAJOR:"),
                "refusal must carry typed prefix: {msg}"
            );
        }
        other => panic!("expected REFUSED_UNDOCUMENTED_MAJOR, got {other:?}"),
    }
    assert_eq!(standing.as_label(), "REFUSED_UNDOCUMENTED_MAJOR");
}

#[test]
fn fixture_dropping_5_0_0_is_refused() {
    // Arrange — only the transitive 4.x remains (workspace 3.x path gone)
    let lock = r#"
[[package]]
name = "curve25519-dalek"
version = "4.1.3"
"#;
    let versions = parse_curve25519_dalek_versions(lock);

    // Act
    let standing = adjudicate(&versions);

    // Assert
    match &standing {
        DalekPinStanding::RefusedUndocumentedMajor { observed } => {
            assert_eq!(observed, &BTreeSet::from(["4.1.3".to_string()]));
            assert!(!observed.contains("5.0.0"));
        }
        other => panic!("expected REFUSED_UNDOCUMENTED_MAJOR, got {other:?}"),
    }
    assert_eq!(standing.as_label(), "REFUSED_UNDOCUMENTED_MAJOR");
}

#[test]
fn fixture_matching_admitted_set_is_alive() {
    // Arrange
    let lock = r#"
[[package]]
name = "curve25519-dalek"
version = "5.0.0"

[[package]]
name = "curve25519-dalek-derive"
version = "0.1.1"

[[package]]
name = "curve25519-dalek"
version = "4.1.3"
"#;
    let versions = parse_curve25519_dalek_versions(lock);

    // Act
    let standing = adjudicate(&versions);

    // Assert
    assert_eq!(standing, DalekPinStanding::Alive);
    assert_eq!(standing.as_label(), "ALIVE");
}

#[test]
fn real_cargo_lock_matches_admitted_dual_majors() {
    // Arrange — real workspace lock on tip
    let text = std::fs::read_to_string(cargo_lock_path())
        .unwrap_or_else(|e| panic!("failed to read Cargo.lock: {e}"));
    let versions = parse_curve25519_dalek_versions(&text);

    // Act
    let standing = adjudicate(&versions);

    // Assert — Gate-1 ALIVE falsifier: exact set equality, fail-closed otherwise
    assert_eq!(
        standing,
        DalekPinStanding::Alive,
        "Cargo.lock curve25519-dalek versions {versions:?} drifted from admitted \
         {ADMITTED_CURVE25519_DALEK:?}; update standing + gap-matrix or restore lock. \
         {}",
        match &standing {
            DalekPinStanding::RefusedUndocumentedMajor { observed } => refusal_message(observed),
            DalekPinStanding::Alive => String::new(),
        }
    );
    assert!(
        versions.contains("4.1.3") && versions.contains("5.0.0"),
        "both documented majors must be present: {versions:?}"
    );
}

#[test]
fn standing_doc_cites_admitted_versions() {
    // Arrange
    let text = std::fs::read_to_string(standing_doc_path())
        .unwrap_or_else(|e| panic!("failed to read {STANDING_DOC}: {e}"));

    // Act / Assert — doc must still name both admitted exact versions
    for ver in ADMITTED_CURVE25519_DALEK {
        assert!(
            text.contains(ver),
            "{STANDING_DOC} must cite admitted version {ver}"
        );
    }
    assert!(
        text.contains("REFUSED_UNDOCUMENTED_MAJOR"),
        "{STANDING_DOC} must name the typed refusal for silent major drift"
    );
}
