//! Chicago court: in-repo ui-layout consume of Taffy AOT `AotSealReceipt`.
//!
//! ECOSYSTEM-SPEC-27.1.1 Gate 1 D2/D5 fence — AAA, no mocks, state-based.
//! Proves the six Gate-1 widths under WCAG 1.4.12 1.5× seal is consumable
//! without re-implementing geometry law (consumer matches digest / structure).
//! Falsifier:
//! `cargo test -p ggen-graph --test ui_layout_court`

use ggen_graph::layout::taffy_aot::{
    consume_aot_seal, gate1_widths_u32, preflight_column_six_widths, seal_digest, AotColumn,
    AotLeaf, UiLayoutConsumeRefusal, VIEWPORT_WIDTHS_PX, WCAG_1412_TEXT_SPACING,
};

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
    // Arrange — real Taffy six-width seal (geometry owned by taffy_aot)
    let seal = preflight_column_six_widths(&valid_column(), WCAG_1412_TEXT_SPACING)
        .expect("valid layout must seal at all six Gate-1 widths under 1.5×");
    let pinned = seal_digest(&seal);

    // Act — consume without re-layout
    let consumed = consume_aot_seal(Some(&seal), Some(&pinned))
        .expect("fresh seal digest must admit at the consume boundary");

    // Assert — Gate-1 widths + WCAG 1.5× + digest identity
    assert_eq!(VIEWPORT_WIDTHS_PX.len(), 6, "Gate-1 seals exactly six widths");
    assert_eq!(consumed.widths_px, gate1_widths_u32());
    assert_eq!(
        consumed.widths_px,
        vec![320, 375, 393, 600, 840, 1280],
        "consume receipt must list every Gate-1 width in order"
    );
    assert!((consumed.text_spacing - 1.5).abs() < f32::EPSILON);
    assert_eq!(consumed.digest, pinned);
    assert_eq!(consumed.leaf_count, 2);
}

#[test]
fn missing_seal_receipt_is_refused() {
    // Arrange / Act
    let result = consume_aot_seal(None, None);

    // Assert — fail closed (zcode missing-digest companion)
    assert_eq!(result, Err(UiLayoutConsumeRefusal::MissingReceipt));
}

#[test]
fn stale_digest_is_refused() {
    // Arrange
    let seal = preflight_column_six_widths(&valid_column(), WCAG_1412_TEXT_SPACING)
        .expect("valid layout must seal");
    let stale = "0".repeat(64);

    // Act
    let result = consume_aot_seal(Some(&seal), Some(&stale));

    // Assert
    match result {
        Err(UiLayoutConsumeRefusal::StaleDigest {
            expected,
            observed,
        }) => {
            assert_eq!(expected, stale);
            assert_eq!(observed, seal_digest(&seal));
            assert_ne!(observed, expected);
        }
        other => panic!("expected StaleDigest, got {other:?}"),
    }
}

#[test]
fn width_mismatch_is_refused() {
    // Arrange — tamper seal widths (drop 320) without re-running geometry
    let mut seal = preflight_column_six_widths(&valid_column(), WCAG_1412_TEXT_SPACING)
        .expect("valid layout must seal");
    seal.widths_px.remove(0);

    // Act
    let result = consume_aot_seal(Some(&seal), None);

    // Assert
    match result {
        Err(UiLayoutConsumeRefusal::WidthMismatch {
            expected,
            observed,
        }) => {
            assert_eq!(expected, gate1_widths_u32());
            assert_eq!(observed.len(), 5);
            assert!(!observed.contains(&320));
        }
        other => panic!("expected WidthMismatch, got {other:?}"),
    }
}

#[test]
fn text_spacing_not_1_5x_is_refused() {
    // Arrange — structural tamper of spacing factor
    let mut seal = preflight_column_six_widths(&valid_column(), WCAG_1412_TEXT_SPACING)
        .expect("valid layout must seal");
    seal.text_spacing = 1.0;

    // Act
    let result = consume_aot_seal(Some(&seal), None);

    // Assert
    match result {
        Err(UiLayoutConsumeRefusal::TextSpacingMismatch {
            expected,
            observed,
        }) => {
            assert!((expected - WCAG_1412_TEXT_SPACING).abs() < f32::EPSILON);
            assert!((observed - 1.0).abs() < f32::EPSILON);
        }
        other => panic!("expected TextSpacingMismatch, got {other:?}"),
    }
}
