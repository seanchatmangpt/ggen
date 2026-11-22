//! Chicago court: ui-layout consume of Taffy AOT six-width seal digests.
//!
//! ECOSYSTEM-SPEC-27.1.1 Gate 1 D5 in-repo fence — AAA, no mocks, state-based.
//! Consumes [`AotSealReceipt`] without re-implementing geometry law; fails closed
//! on missing / stale / mismatched digests. Does not authorize DO.
//!
//! Falsifier:
//! `cargo test -p ggen-graph --test ui_layout_court`

use std::fs;
use std::path::PathBuf;

use ggen_graph::layout::remediation_bound::DEFAULT_MAX_REMEDIATION_ATTEMPTS;
use ggen_graph::layout::taffy_aot::{
    aot_seal_digest, preflight_column_six_widths, AotColumn, AotLeaf, AotRefusal, AotSealReceipt,
    VIEWPORT_WIDTHS_PX, WCAG_1412_TEXT_SPACING,
};
use ggen_graph::layout::ui_layout::{
    consume_aot_seal, court_ui_layout_column, parse_layout_court_case, run_layout_court_case,
    run_layout_court_json, UiLayoutRefusal, UiLayoutStanding, LAYOUT_COURT_NOUN,
    LAYOUT_COURT_SCHEMA_V1,
};

fn fixture(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../docs/rfc/v27.1.1/fixtures")
        .join(name)
}

fn load_fixture(name: &str) -> String {
    let path = fixture(name);
    fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("expected layout-court fixture at {}: {e}", path.display()))
}

fn valid_column() -> AotColumn {
    AotColumn {
        gap_px: 8.0,
        children: vec![
            AotLeaf {
                id: "body_copy".into(),
                width_px: 200.0,
                height_px: 32.0,
                is_text: true,
                is_interactive: false,
            },
            AotLeaf {
                id: "primary_cta".into(),
                width_px: 120.0,
                height_px: 44.0,
                is_text: false,
                is_interactive: true,
            },
        ],
    }
}

#[test]
fn six_width_seal_under_1_5x_is_consumed_alive() {
    // Arrange — real Taffy six-width seal (no mock digest)
    let column = valid_column();

    // Act
    let receipt = match court_ui_layout_column(&column) {
        Ok(r) => r,
        Err(e) => panic!("valid layout must court ALIVE: {e:?}"),
    };

    // Assert — seal lists every Gate-1 width; digest is 64-hex; standing ALIVE
    let expected_widths: Vec<u32> = VIEWPORT_WIDTHS_PX
        .iter()
        .map(|w| w.round() as u32)
        .collect();
    assert_eq!(
        expected_widths,
        vec![320, 375, 414, 768, 1024, 1440],
        "Gate-1 ui-layout viewport set must stay 320/375/414/768/1024/1440"
    );
    assert_eq!(receipt.seal.widths_px, expected_widths);
    assert!((receipt.seal.text_spacing - WCAG_1412_TEXT_SPACING).abs() < f32::EPSILON);
    assert_eq!(receipt.seal.leaf_count, 2);
    assert_eq!(receipt.digest.len(), 64);
    assert!(
        receipt.digest.chars().all(|c| c.is_ascii_hexdigit()),
        "digest must be hex: {}",
        receipt.digest
    );
    assert_eq!(receipt.digest, receipt.seal.digest);
    assert_eq!(
        receipt.digest,
        aot_seal_digest(
            receipt.seal.text_spacing,
            &receipt.seal.widths_px,
            receipt.seal.leaf_count
        )
    );
    assert_eq!(receipt.standing, UiLayoutStanding::Alive);
    assert_eq!(receipt.standing.as_str(), "ALIVE");
    assert_eq!(
        receipt.max_remediation_attempts,
        DEFAULT_MAX_REMEDIATION_ATTEMPTS
    );
}

#[test]
fn missing_seal_is_refused() {
    // Arrange / Act
    let result = consume_aot_seal(None, Some("deadbeef"));

    // Assert
    match result {
        Err(UiLayoutRefusal::MissingSeal) => {
            assert_eq!(
                UiLayoutRefusal::MissingSeal.code(),
                "REFUSED_UI_LAYOUT_MISSING_SEAL"
            );
        }
        other => panic!("expected MissingSeal, got {other:?}"),
    }
}

#[test]
fn missing_pinned_digest_is_refused() {
    // Arrange — live seal, absent pin
    let seal = preflight_column_six_widths(&valid_column(), WCAG_1412_TEXT_SPACING)
        .unwrap_or_else(|e| panic!("fixture must seal: {e:?}"));

    // Act
    let missing = consume_aot_seal(Some(&seal), None);
    let empty = consume_aot_seal(Some(&seal), Some("   "));

    // Assert
    assert!(
        matches!(missing, Err(UiLayoutRefusal::MissingDigest)),
        "None pin must refuse: {missing:?}"
    );
    assert!(
        matches!(empty, Err(UiLayoutRefusal::MissingDigest)),
        "blank pin must refuse: {empty:?}"
    );
    assert_eq!(
        UiLayoutRefusal::MissingDigest.code(),
        "REFUSED_UI_LAYOUT_MISSING_DIGEST"
    );
}

#[test]
fn mismatched_digest_is_refused() {
    // Arrange
    let seal = preflight_column_six_widths(&valid_column(), WCAG_1412_TEXT_SPACING)
        .unwrap_or_else(|e| panic!("fixture must seal: {e:?}"));
    let wrong = "0".repeat(64);

    // Act
    let result = consume_aot_seal(Some(&seal), Some(&wrong));

    // Assert
    match result {
        Err(UiLayoutRefusal::MismatchedDigest { pinned, observed }) => {
            assert_eq!(pinned, wrong);
            assert_eq!(observed, seal.digest);
            assert_ne!(pinned, observed);
        }
        other => panic!("expected MismatchedDigest, got {other:?}"),
    }
}

#[test]
fn stale_embedded_digest_is_refused() {
    // Arrange — tamper the embedded digest while keeping sealed fields
    let mut seal = preflight_column_six_widths(&valid_column(), WCAG_1412_TEXT_SPACING)
        .unwrap_or_else(|e| panic!("fixture must seal: {e:?}"));
    let recomputed = seal.recompute_digest();
    seal.digest = "f".repeat(64);
    assert_ne!(seal.digest, recomputed);

    // Act
    let result = consume_aot_seal(Some(&seal), Some(&seal.digest.clone()));

    // Assert — even a matching pin cannot revive a stale/tampered embedding
    match result {
        Err(UiLayoutRefusal::StaleDigest {
            embedded,
            recomputed: recomputed_obs,
        }) => {
            assert_eq!(embedded, "f".repeat(64));
            assert_eq!(recomputed_obs, recomputed);
        }
        other => panic!("expected StaleDigest, got {other:?}"),
    }
}

#[test]
fn geometry_overflow_refuses_before_consume() {
    // Arrange — 220×1.5 = 330 overflows 320 only (same fixture as taffy court)
    let column = AotColumn {
        gap_px: 8.0,
        children: vec![
            AotLeaf {
                id: "narrow_copy".into(),
                width_px: 220.0,
                height_px: 40.0,
                is_text: true,
                is_interactive: false,
            },
            AotLeaf {
                id: "cta".into(),
                width_px: 120.0,
                height_px: 44.0,
                is_text: false,
                is_interactive: true,
            },
        ],
    };

    // Act
    let result = court_ui_layout_column(&column);

    // Assert — consume never runs; typed geometry refusal names width 320
    match &result {
        Err(UiLayoutRefusal::Geometry(AotRefusal::Overflow {
            viewport_width_px,
            text_spacing,
            leaf_id,
            ..
        })) => {
            assert_eq!(*viewport_width_px, 320);
            assert!((*text_spacing - WCAG_1412_TEXT_SPACING).abs() < f32::EPSILON);
            assert_eq!(leaf_id, "narrow_copy");
            assert_eq!(
                result.as_ref().unwrap_err().code(),
                "REFUSED_UI_LAYOUT_GEOMETRY"
            );
        }
        other => panic!("expected Geometry(Overflow@320), got {other:?}"),
    }
}

#[test]
fn consume_does_not_reimplement_geometry_on_prebuilt_seal() {
    // Arrange — hand a valid seal; consume path must not need the column
    let seal = preflight_column_six_widths(&valid_column(), WCAG_1412_TEXT_SPACING)
        .unwrap_or_else(|e| panic!("fixture must seal: {e:?}"));
    let pin = seal.digest.clone();

    // Act — digest-only consume (no Taffy re-entry)
    let receipt = match consume_aot_seal(Some(&seal), Some(&pin)) {
        Ok(r) => r,
        Err(e) => panic!("pin must admit: {e:?}"),
    };

    // Assert
    assert_eq!(receipt.standing, UiLayoutStanding::Alive);
    assert_eq!(receipt.digest, pin);
    // Structural shape still carries the six widths without re-layout
    assert_eq!(receipt.seal.widths_px.len(), VIEWPORT_WIDTHS_PX.len());
}

#[test]
fn empty_embedded_digest_on_seal_is_refused() {
    // Arrange
    let seal = AotSealReceipt {
        text_spacing: WCAG_1412_TEXT_SPACING,
        widths_px: VIEWPORT_WIDTHS_PX
            .iter()
            .map(|w| w.round() as u32)
            .collect(),
        leaf_count: 1,
        digest: String::new(),
    };

    // Act
    let result = consume_aot_seal(Some(&seal), Some("abc"));

    // Assert
    assert!(
        matches!(result, Err(UiLayoutRefusal::MissingDigest)),
        "empty embedded digest must refuse: {result:?}"
    );
}

#[test]
fn json_fixture_feeds_nodes_into_taffy_aot_and_admits() {
    // Arrange — on-disk zcode-cli wire fixture (not an inline double)
    let json = load_fixture("ui-layout-valid.column.json");

    // Act
    let receipt = match run_layout_court_json(&json) {
        Ok(r) => r,
        Err(e) => panic!("valid JSON fixture must admit via Taffy AOT: {e:?}"),
    };

    // Assert
    assert_eq!(receipt.standing, UiLayoutStanding::Alive);
    assert_eq!(receipt.seal.widths_px, vec![320, 375, 414, 768, 1024, 1440]);
    assert_eq!(receipt.digest.len(), 64);
}

#[test]
fn ts_contract_stub_documents_zcode_cli_boundary() {
    let stub = load_fixture("ui-layout-court.ts");
    assert!(stub.contains(LAYOUT_COURT_NOUN));
    assert!(stub.contains(LAYOUT_COURT_SCHEMA_V1));
    assert!(stub.contains("BLOCKED_ZCODE_CLI_OUT_OF_REPO"));
    assert!(stub.contains("ui-layout-valid.column.json"));
}

#[test]
fn anti_vacuity_mutation_widened_text_is_refused_not_vacuous_pass() {
    // Arrange — admit the real fixture, then deliberately break geometry
    let json = load_fixture("ui-layout-valid.column.json");
    let baseline = run_layout_court_json(&json);
    assert!(
        baseline.is_ok(),
        "anti-vacuity requires a green baseline; got {baseline:?}"
    );

    let mut case = parse_layout_court_case(&json).expect("valid fixture parses");
    let before = case.column.children[0].width_px;
    // 220 × 1.5 = 330 overflows Gate-1 width 320 only
    case.column.children[0].width_px = 220.0;
    assert_ne!(
        case.column.children[0].width_px, before,
        "mutation must actually change the layout node"
    );

    // Act
    let standing = run_layout_court_case(&case);

    // Assert — REFUSED with geometry Overflow@320 (not a vacuous pass)
    match standing {
        Err(UiLayoutRefusal::Geometry(AotRefusal::Overflow {
            viewport_width_px,
            text_spacing,
            leaf_id,
            ..
        })) => {
            assert_eq!(viewport_width_px, 320);
            assert!((text_spacing - WCAG_1412_TEXT_SPACING).abs() < f32::EPSILON);
            assert_eq!(leaf_id, "body_copy");
        }
        other => {
            panic!("mutated layout must be REFUSED (Overflow@320), got {other:?} — vacuous pass")
        }
    }
}
