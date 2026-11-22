//! Chicago court: ui-layout-court adapter feeds layout nodes into Taffy AOT.
//!
//! ECOSYSTEM-SPEC-27.1.1 Gate 1 D5 — AAA, no mocks, state-based.
//! zcode-cli `ui-layout-court.ts` is out-of-repo; this court seals the ggen-side
//! seam + anti-vacuity mutation (deliberately broken layout is REFUSED).
//!
//! Falsifier: `cargo test -p ggen-graph --test ui_layout_court`

use std::fs;
use std::path::PathBuf;

use ggen_graph::layout::taffy_aot::{AotRefusal, WCAG_1412_TEXT_SPACING};
use ggen_graph::layout::ui_layout_court::{
    parse_layout_court_case, run_layout_court, run_layout_court_json, seal_digest,
    LayoutCourtRefusal, LayoutCourtStanding, LAYOUT_COURT_NOUN, LAYOUT_COURT_SCHEMA_V1,
};

fn fixture(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../docs/rfc/v27.1.1/fixtures")
        .join(name)
}

fn load_fixture(name: &str) -> String {
    let path = fixture(name);
    fs::read_to_string(&path).unwrap_or_else(|e| {
        panic!("expected layout-court fixture at {}: {e}", path.display())
    })
}

#[test]
fn valid_fixture_admits_via_adapter_into_taffy_aot() {
    // Arrange — real on-disk fixture (zcode-cli wire shape), not an inline double
    let json = load_fixture("ui-layout-valid.column.json");

    // Act
    let standing = run_layout_court_json(&json);

    // Assert
    match standing {
        LayoutCourtStanding::Admitted(receipt) => {
            assert!(
                (receipt.seal.text_spacing - WCAG_1412_TEXT_SPACING).abs() < f32::EPSILON
            );
            assert_eq!(receipt.seal.widths_px, vec![320, 375, 393, 600, 840, 1280]);
            assert_eq!(receipt.seal.leaf_count, 2);
            assert_eq!(receipt.seal_digest, seal_digest(&receipt.seal));
            assert_eq!(receipt.seal_digest.len(), 64);
        }
        other => panic!("valid fixture must admit via Taffy AOT adapter; got {other:?}"),
    }
}

#[test]
fn ts_contract_stub_documents_zcode_cli_boundary() {
    // Arrange — documented stub lives beside the JSON fixture
    let stub = load_fixture("ui-layout-court.ts");

    // Assert — contract nouns + BLOCKED standing are present (not executed here)
    assert!(
        stub.contains(LAYOUT_COURT_NOUN),
        "TS stub must name the court noun"
    );
    assert!(
        stub.contains(LAYOUT_COURT_SCHEMA_V1),
        "TS stub must name the admitted schema"
    );
    assert!(
        stub.contains("BLOCKED_ZCODE_CLI_OUT_OF_REPO"),
        "TS stub must refuse ambient ALIVE while zcode-cli is unmounted"
    );
}

#[test]
fn anti_vacuity_mutation_widened_text_is_refused_not_vacuous_pass() {
    // Arrange — start from the admitted fixture, then deliberately break geometry
    let json = load_fixture("ui-layout-valid.column.json");
    let baseline = run_layout_court_json(&json);
    assert!(
        baseline.is_admitted(),
        "anti-vacuity requires a green baseline; got {baseline:?}"
    );

    let mut case = parse_layout_court_case(&json).expect("valid fixture parses");
    let before = case.column.children[0].width_px;
    // 220 × 1.5 = 330 overflows Gate-1 width 320 only — typed Overflow at 320
    case.column.children[0].width_px = 220.0;
    assert_ne!(
        case.column.children[0].width_px, before,
        "mutation must actually change the layout node"
    );

    // Act
    let standing = run_layout_court(&case);

    // Assert — REFUSED with geometry Overflow naming width 320 (not a vacuous pass)
    match standing {
        LayoutCourtStanding::Refused(LayoutCourtRefusal::Geometry(AotRefusal::Overflow {
            viewport_width_px,
            text_spacing,
            leaf_id,
            ..
        })) => {
            assert_eq!(viewport_width_px, 320);
            assert!((text_spacing - WCAG_1412_TEXT_SPACING).abs() < f32::EPSILON);
            assert_eq!(leaf_id, "body_copy");
        }
        other => panic!(
            "mutated layout must be REFUSED (Overflow@320), got {other:?} — vacuous pass"
        ),
    }
}

#[test]
fn mismatched_expected_seal_digest_is_refused() {
    // Arrange
    let json = load_fixture("ui-layout-valid.column.json");
    let mut case = parse_layout_court_case(&json).expect("valid fixture parses");
    case.expected_seal_digest = Some("0".repeat(64));

    // Act
    let standing = run_layout_court(&case);

    // Assert
    match standing {
        LayoutCourtStanding::Refused(LayoutCourtRefusal::DigestMismatch {
            expected,
            actual,
        }) => {
            assert_eq!(expected.len(), 64);
            assert_eq!(actual.len(), 64);
            assert_ne!(expected, actual);
        }
        other => panic!("expected DigestMismatch refusal, got {other:?}"),
    }
}

#[test]
fn unsupported_schema_is_refused() {
    // Arrange
    let json = load_fixture("ui-layout-valid.column.json");
    let mut case = parse_layout_court_case(&json).expect("valid fixture parses");
    case.schema = "ggen.layout.aot.column/v0-experimental".into();

    // Act
    let standing = run_layout_court(&case);

    // Assert
    match standing {
        LayoutCourtStanding::Refused(LayoutCourtRefusal::SchemaUnsupported { schema }) => {
            assert!(schema.contains("v0-experimental"));
        }
        other => panic!("expected SchemaUnsupported, got {other:?}"),
    }
}
