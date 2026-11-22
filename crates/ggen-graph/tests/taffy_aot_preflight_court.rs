//! Chicago court: Taffy AOT pre-flight under WCAG 1.4.12 1.5× text spacing.
//!
//! ECOSYSTEM-SPEC-27.1.1 Gate 1 D2 — AAA, no mocks, state-based.
//! Falsifier:
//! `cargo test -p ggen-graph --test taffy_aot_preflight_court -- overflow_at_375_with_text_spacing_1_5x_is_refused`

use ggen_graph::layout::taffy_aot::{
    preflight_column, AotColumn, AotLeaf, AotRefusal, WCAG_1412_TEXT_SPACING,
};

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
        Err(AotRefusal::Overflow {
            viewport_width_px,
            text_spacing,
            ..
        }) => {
            assert_eq!(viewport_width_px, 375);
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
    let column = AotColumn {
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
    };

    // Act
    let result = preflight_column(&column, 375.0, WCAG_1412_TEXT_SPACING);

    // Assert
    assert!(
        result.is_ok(),
        "valid layout must be admitted under 1.5× at 375; got {result:?}"
    );
}

#[test]
fn touch_target_under_44px_is_refused() {
    // Arrange
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
    let result = preflight_column(&column, 375.0, WCAG_1412_TEXT_SPACING);

    // Assert
    match result {
        Err(AotRefusal::TouchTargetTooSmall { min_px, .. }) => {
            assert_eq!(min_px, 44);
        }
        other => panic!("expected TouchTargetTooSmall, got {other:?}"),
    }
}

#[test]
fn gap_under_8px_is_refused() {
    // Arrange
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
    let result = preflight_column(&column, 375.0, WCAG_1412_TEXT_SPACING);

    // Assert
    match result {
        Err(AotRefusal::GapTooSmall { min_px, .. }) => {
            assert_eq!(min_px, 8);
        }
        other => panic!("expected GapTooSmall, got {other:?}"),
    }
}
