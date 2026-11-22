//! Chicago court: ui-layout-court adapter feeds layout nodes into Taffy AOT.
//!
//! ECOSYSTEM-SPEC-27.1.1 Gate 1 D5 — AAA, no mocks, state-based.
//! zcode-cli live `ui-layout-court.ts` runner is out-of-repo; this court seals
//! the ggen-side seam + anti-vacuity mutations (overflow / overlap / touch
//! refuse with typed [`LayoutRefusal`]).
//!
//! Falsifier: `cargo test -p ggen-graph --test ui_layout_court`

use std::fs;
use std::path::PathBuf;

use ggen_graph::layout::taffy_aot::{LayoutRefusal, WCAG_1412_TEXT_SPACING};
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

fn admitted_baseline_case() -> ggen_graph::layout::ui_layout_court::LayoutCourtCase {
    let json = load_fixture("ui-layout-valid.column.json");
    let baseline = run_layout_court_json(&json);
    assert!(
        baseline.is_admitted(),
        "anti-vacuity requires a green baseline; got {baseline:?}"
    );
    parse_layout_court_case(&json).expect("valid fixture parses")
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
    assert!(
        stub.contains("LayoutRefusal"),
        "TS stub must name the typed geometry refusal surface"
    );
}

#[test]
fn mutation_overflow_refuses_with_typed_layout_refusal() {
    // Arrange — start from admitted fixture, widen text so 220×1.5 overflows 320
    let mut case = admitted_baseline_case();
    let before = case.column.children[0].width_px;
    case.column.children[0].width_px = 220.0;
    assert_ne!(case.column.children[0].width_px, before);

    // Act
    let standing = run_layout_court(&case);

    // Assert
    match standing {
        LayoutCourtStanding::Refused(LayoutCourtRefusal::Geometry(LayoutRefusal::Overflow {
            viewport_width_px,
            text_spacing,
            leaf_id,
            ..
        })) => {
            assert_eq!(viewport_width_px, 320);
            assert!((text_spacing - WCAG_1412_TEXT_SPACING).abs() < f32::EPSILON);
            assert_eq!(leaf_id, "body_copy");
        }
        other => panic!("mutated overflow must refuse LayoutRefusal::Overflow; got {other:?}"),
    }
}

#[test]
fn mutation_touch_target_refuses_with_typed_layout_refusal() {
    // Arrange — shrink interactive CTA below 44×44 floor
    let mut case = admitted_baseline_case();
    case.column.children[1].width_px = 40.0;
    case.column.children[1].height_px = 40.0;

    // Act
    let standing = run_layout_court(&case);

    // Assert
    match standing {
        LayoutCourtStanding::Refused(LayoutCourtRefusal::Geometry(
            LayoutRefusal::TouchTargetTooSmall {
                leaf_id,
                min_px,
                width_px,
                height_px,
                ..
            },
        )) => {
            assert_eq!(leaf_id, "primary_cta");
            assert_eq!(min_px, 44);
            assert!(width_px + f32::EPSILON < 44.0);
            assert!(height_px + f32::EPSILON < 44.0);
        }
        other => {
            panic!("mutated touch target must refuse LayoutRefusal::TouchTargetTooSmall; got {other:?}")
        }
    }
}

#[test]
fn mutation_overlap_refuses_with_typed_layout_refusal() {
    // Arrange — negative gap is the adversarial path that surfaces Overlap
    let mut case = admitted_baseline_case();
    case.column.gap_px = -20.0;

    // Act
    let standing = run_layout_court(&case);

    // Assert
    match standing {
        LayoutCourtStanding::Refused(LayoutCourtRefusal::Geometry(LayoutRefusal::Overlap {
            a_id,
            b_id,
            text_spacing,
            ..
        })) => {
            assert_eq!(a_id, "body_copy");
            assert_eq!(b_id, "primary_cta");
            assert!((text_spacing - WCAG_1412_TEXT_SPACING).abs() < f32::EPSILON);
        }
        other => panic!("mutated overlap must refuse LayoutRefusal::Overlap; got {other:?}"),
    }
}

#[test]
fn mismatched_expected_seal_digest_is_refused() {
    // Arrange
    let mut case = admitted_baseline_case();
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
    let mut case = admitted_baseline_case();
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
