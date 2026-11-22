//! Chicago court: Taffy AOT six-width seal under WCAG 1.4.12 1.5× text spacing.
//!
//! ECOSYSTEM-SPEC-27.1.1 Gate 1 D2 — AAA, no mocks, state-based.
//! Falsifier:
//! `cargo test -p ggen-graph --test taffy_aot_preflight_court -- six_width_seal_admits_valid_layout_under_1_5x`

use ggen_graph::layout::taffy_aot::{
    preflight_column, preflight_column_six_widths, AotColumn, AotLeaf, AotRefusal, LayoutRefusal,
    VIEWPORT_WIDTHS_PX, WCAG_1412_TEXT_SPACING,
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
fn overflow_at_375_with_text_spacing_1_5x_is_refused() {
    // Arrange — text leaf fits at 1.0× (280 < 375) but overflows at 1.5× (420 > 375)
    let column = AotColumn {
        gap_px: 8.0,
        children: vec![AotLeaf {
            id: "hero_copy".into(),
            width_px: 280.0,
            height_px: 40.0,
            is_text: true,
            is_interactive: false,
        }],
    };

    // Act
    let at_1x = preflight_column(&column, 375.0, 1.0);
    let at_1_5x = preflight_column(&column, 375.0, WCAG_1412_TEXT_SPACING);

    // Assert — baseline admits so the 1.5× refusal is spacing-driven, not ambient
    assert!(
        at_1x.is_ok(),
        "fixture must admit at 1.0× spacing; got {at_1x:?}"
    );
    match at_1_5x {
        Err(LayoutRefusal::Overflow {
            viewport_width_px,
            text_spacing,
            leaf_id,
            content_right_px,
        }) => {
            assert_eq!(
                viewport_width_px, 375,
                "Overflow must name the failing width"
            );
            assert_eq!(leaf_id, "hero_copy");
            assert!(content_right_px > 375.0);
            assert!(
                (text_spacing - 1.5).abs() < f32::EPSILON,
                "expected 1.5× spacing, got {text_spacing}"
            );
        }
        other => panic!("expected Overflow refusal at 1.5× / 375, got {other:?}"),
    }
}

#[test]
fn valid_layout_at_375_with_text_spacing_1_5x_is_admitted() {
    // Arrange — after 1.5× text expansion still fits; touch ≥44; gap ≥8
    let column = valid_column();

    // Act
    let result = preflight_column(&column, 375.0, WCAG_1412_TEXT_SPACING);

    // Assert
    assert!(
        result.is_ok(),
        "valid layout must be admitted under 1.5× at 375; got {result:?}"
    );
}

#[test]
fn layout_refusal_touch_target_too_small_names_width_and_node() {
    // Arrange — interactive leaf below 44×44 floor at Gate-1 width 393
    let column = AotColumn {
        gap_px: 8.0,
        children: vec![AotLeaf {
            id: "tiny_hit".into(),
            width_px: 40.0,
            height_px: 40.0,
            is_text: false,
            is_interactive: true,
        }],
    };

    // Act
    let result = preflight_column(&column, 393.0, WCAG_1412_TEXT_SPACING);

    // Assert
    match result {
        Err(LayoutRefusal::TouchTargetTooSmall {
            viewport_width_px,
            text_spacing,
            leaf_id,
            width_px,
            height_px,
            min_px,
        }) => {
            assert_eq!(
                viewport_width_px, 393,
                "TouchTargetTooSmall must name the failing width"
            );
            assert!((text_spacing - WCAG_1412_TEXT_SPACING).abs() < f32::EPSILON);
            assert_eq!(leaf_id, "tiny_hit", "must name the failing node");
            assert!(width_px + f32::EPSILON < 44.0);
            assert!(height_px + f32::EPSILON < 44.0);
            assert_eq!(min_px, 44);
        }
        other => panic!("expected TouchTargetTooSmall, got {other:?}"),
    }
}

#[test]
fn layout_refusal_gap_too_small_names_width() {
    // Arrange — declared sibling gap 4 < 8 at Gate-1 width 600
    let column = AotColumn {
        gap_px: 4.0,
        children: vec![
            AotLeaf {
                id: "a".into(),
                width_px: 100.0,
                height_px: 44.0,
                is_text: false,
                is_interactive: true,
            },
            AotLeaf {
                id: "b".into(),
                width_px: 100.0,
                height_px: 44.0,
                is_text: false,
                is_interactive: true,
            },
        ],
    };

    // Act
    let result = preflight_column(&column, 600.0, WCAG_1412_TEXT_SPACING);

    // Assert
    match result {
        Err(LayoutRefusal::GapTooSmall {
            viewport_width_px,
            text_spacing,
            gap_px,
            min_px,
        }) => {
            assert_eq!(
                viewport_width_px, 600,
                "GapTooSmall must name the failing width"
            );
            assert!((text_spacing - WCAG_1412_TEXT_SPACING).abs() < f32::EPSILON);
            assert!((gap_px - 4.0).abs() < f32::EPSILON);
            assert_eq!(min_px, 8);
        }
        other => panic!("expected GapTooSmall, got {other:?}"),
    }
}

#[test]
fn six_width_seal_admits_valid_layout_under_1_5x() {
    // Arrange — text 200→300 at 1.5× fits 320; touch ≥44; gap ≥8
    let column = valid_column();

    // Act
    let seal = preflight_column_six_widths(&column, WCAG_1412_TEXT_SPACING);

    // Assert
    let receipt = seal.expect("valid layout must admit at all six Gate-1 widths under 1.5×");
    assert!((receipt.text_spacing - WCAG_1412_TEXT_SPACING).abs() < f32::EPSILON);
    let expected: Vec<u32> = VIEWPORT_WIDTHS_PX
        .iter()
        .map(|w| w.round() as u32)
        .collect();
    assert_eq!(
        receipt.widths_px, expected,
        "seal receipt must list every Gate-1 width in order"
    );
    assert_eq!(receipt.leaf_count, 2);
    assert_eq!(
        receipt.digest,
        receipt.recompute_digest(),
        "seal digest must match domain-separated recomputation"
    );
    assert_eq!(receipt.digest.len(), 64);
}

#[test]
fn six_width_seal_refuses_overflow_only_at_320_naming_failing_width() {
    // Arrange — 220×1.5 = 330 overflows 320 only; admits at 375+
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

    // Act — prove single-width baseline: fails only at 320 under 1.5×
    let at_320 = preflight_column(&column, 320.0, WCAG_1412_TEXT_SPACING);
    let at_375 = preflight_column(&column, 375.0, WCAG_1412_TEXT_SPACING);
    let seal = preflight_column_six_widths(&column, WCAG_1412_TEXT_SPACING);

    // Assert
    assert!(
        matches!(
            at_320,
            Err(AotRefusal::Overflow {
                viewport_width_px: 320,
                ..
            })
        ),
        "fixture must overflow at 320 under 1.5×; got {at_320:?}"
    );
    assert!(
        at_375.is_ok(),
        "fixture must admit at 375 under 1.5× so the seal refusal is width-local; got {at_375:?}"
    );
    match seal {
        Err(AotRefusal::Overflow {
            viewport_width_px,
            text_spacing,
            leaf_id,
            ..
        }) => {
            assert_eq!(
                viewport_width_px, 320,
                "typed refusal must name the failing width"
            );
            assert!((text_spacing - WCAG_1412_TEXT_SPACING).abs() < f32::EPSILON);
            assert_eq!(leaf_id, "narrow_copy");
        }
        other => panic!("expected Overflow at 320 from six-width seal, got {other:?}"),
    }
}

#[test]
fn layout_refusal_overlap_names_width_and_nodes() {
    // Arrange — negative gap stacks siblings into overlapping rects at 840
    let column = AotColumn {
        gap_px: -20.0,
        children: vec![
            AotLeaf {
                id: "upper_card".into(),
                width_px: 200.0,
                height_px: 48.0,
                is_text: false,
                is_interactive: true,
            },
            AotLeaf {
                id: "lower_card".into(),
                width_px: 200.0,
                height_px: 48.0,
                is_text: false,
                is_interactive: true,
            },
        ],
    };
    let result = preflight_column(&column, 840.0, WCAG_1412_TEXT_SPACING);
    match result {
        Err(LayoutRefusal::Overlap {
            viewport_width_px,
            text_spacing,
            a_id,
            b_id,
        }) => {
            assert_eq!(
                viewport_width_px, 840,
                "Overlap must name the failing width"
            );
            assert!((text_spacing - WCAG_1412_TEXT_SPACING).abs() < f32::EPSILON);
            assert_eq!(a_id, "upper_card");
            assert_eq!(b_id, "lower_card");
        }
        other => panic!("expected Overlap refusal naming nodes, got {other:?}"),
    }
}
