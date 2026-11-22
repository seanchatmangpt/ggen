//! Chicago court: DTCG `*.brand.toml` Gate-1 completeness (missing facet refusals).
//!
//! ECOSYSTEM-SPEC-27.1.1 Gate 1 D4 — AAA, no mocks, state-based.
//! Extends `layout::brand` (does not fork). Falsifier:
//! `cargo test -p ggen-graph --test brand_toml_completeness_court`

use std::fs;
use std::path::PathBuf;

use ggen_graph::layout::brand::{
    parse_and_admit_brand, parse_brand_toml, BrandCompletenessFacet, BrandRefusal, DTCG_VERSION,
    GRID_BASE_PX, GRID_HALF_PX, INTER_TARGET_GAP_PX, TOUCH_FLOOR_APPLE_PX, TOUCH_FLOOR_MATERIAL_PX,
};

fn fixture(name: &str) -> PathBuf {
    PathBuf::from(env!("CARGO_MANIFEST_DIR"))
        .join("../../docs/rfc/v27.1.1/fixtures")
        .join(name)
}

fn load_fixture(name: &str) -> String {
    let path = fixture(name);
    fs::read_to_string(&path)
        .unwrap_or_else(|e| panic!("expected admitted OEM fixture at {}: {e}", path.display()))
}

/// Drop a single `key = …` line from a TOML document (completeness mutilation).
fn strip_rhythm_key(raw: &str, key: &str) -> String {
    let needle = format!("{key} =");
    let mut out = String::new();
    let mut removed = false;
    for line in raw.lines() {
        if !removed && line.trim_start().starts_with(&needle) {
            removed = true;
            continue;
        }
        out.push_str(line);
        out.push('\n');
    }
    assert!(
        removed,
        "expected to strip key `{key}` from fixture; needle `{needle}` not found"
    );
    out
}

#[test]
fn complete_oem_fixtures_encode_grid_touch_gap_and_fluid() {
    // Arrange — real OEM stubs on disk
    let fourth = load_fixture("fourth-watch.brand.toml");
    let cindy = load_fixture("cindycat.brand.toml");

    // Act
    let fourth_m = parse_brand_toml(&fourth).expect("fourth-watch parses");
    let cindy_m = parse_brand_toml(&cindy).expect("cindycat parses");
    let fourth_ok = parse_and_admit_brand(&fourth).expect("fourth-watch admits");
    let cindy_ok = parse_and_admit_brand(&cindy).expect("cindycat admits");

    // Assert — DTCG + 4px/8px grid + touch floors + gap + fluid clamps
    assert_eq!(fourth_ok.brand_id, "fourth-watch");
    assert_eq!(cindy_ok.brand_id, "cindycat");
    assert_eq!(fourth_ok.dtcg_version, DTCG_VERSION);
    assert_eq!(cindy_ok.dtcg_version, DTCG_VERSION);

    for (label, m) in [("fourth-watch", &fourth_m), ("cindycat", &cindy_m)] {
        assert_eq!(m.rhythm.grid_base_px, GRID_BASE_PX, "{label} grid_base");
        assert_eq!(m.rhythm.grid_half_px, GRID_HALF_PX, "{label} grid_half");
        assert!(
            (m.rhythm.touch_floor_apple_px - TOUCH_FLOOR_APPLE_PX).abs() < f32::EPSILON,
            "{label} apple floor"
        );
        assert!(
            (m.rhythm.touch_floor_material_px - TOUCH_FLOOR_MATERIAL_PX).abs() < f32::EPSILON,
            "{label} material floor"
        );
        assert!(
            (m.rhythm.inter_target_gap_px - INTER_TARGET_GAP_PX).abs() < f32::EPSILON,
            "{label} gap"
        );
        assert!(
            m.rhythm.spacing.len() >= 5,
            "{label} must encode a spacing scale on the 4px/8px grid; got {}",
            m.rhythm.spacing.len()
        );
        assert!(
            m.rhythm.fluid.len() >= 1,
            "{label} must encode at least one fluid clamp/cqi slope"
        );
        for token in &m.rhythm.spacing {
            let half = GRID_HALF_PX as f32;
            let steps = (token.px / half).round();
            assert!(
                (token.px - steps * half).abs() <= f32::EPSILON * 8.0_f32.max(token.px.abs()),
                "{label} spacing `{}` = {}px off {half}px grid",
                token.name,
                token.px
            );
        }
    }
}

#[test]
fn missing_touch_floor_is_refused_incomplete() {
    // Arrange — strip Apple floor from fourth-watch
    let raw = strip_rhythm_key(
        &load_fixture("fourth-watch.brand.toml"),
        "touch_floor_apple_px",
    );

    // Act
    let result = parse_and_admit_brand(&raw);

    // Assert
    match result {
        Err(BrandRefusal::Incomplete { missing, field }) => {
            assert_eq!(missing, BrandCompletenessFacet::TouchFloor);
            assert_eq!(field, "touch_floor_apple_px");
        }
        other => panic!("expected Incomplete(TouchFloor), got {other:?}"),
    }
}

#[test]
fn missing_inter_target_gap_is_refused_incomplete() {
    // Arrange
    let raw = strip_rhythm_key(&load_fixture("cindycat.brand.toml"), "inter_target_gap_px");

    // Act
    let result = parse_and_admit_brand(&raw);

    // Assert
    match result {
        Err(BrandRefusal::Incomplete { missing, field }) => {
            assert_eq!(missing, BrandCompletenessFacet::Gap);
            assert_eq!(field, "inter_target_gap_px");
        }
        other => panic!("expected Incomplete(Gap), got {other:?}"),
    }
}

#[test]
fn missing_grid_base_is_refused_incomplete() {
    // Arrange
    let raw = strip_rhythm_key(&load_fixture("fourth-watch.brand.toml"), "grid_base_px");

    // Act
    let result = parse_and_admit_brand(&raw);

    // Assert
    match result {
        Err(BrandRefusal::Incomplete { missing, field }) => {
            assert_eq!(missing, BrandCompletenessFacet::Grid);
            assert_eq!(field, "grid_base_px");
        }
        other => panic!("expected Incomplete(Grid), got {other:?}"),
    }
}

#[test]
fn missing_grid_half_is_refused_incomplete() {
    // Arrange
    let raw = strip_rhythm_key(&load_fixture("cindycat.brand.toml"), "grid_half_px");

    // Act
    let result = parse_and_admit_brand(&raw);

    // Assert
    match result {
        Err(BrandRefusal::Incomplete { missing, field }) => {
            assert_eq!(missing, BrandCompletenessFacet::Grid);
            assert_eq!(field, "grid_half_px");
        }
        other => panic!("expected Incomplete(Grid), got {other:?}"),
    }
}
