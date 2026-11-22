//! Chicago court: brand fluid clamp / cqi slope admission (Gate-1 D4 dedicated).
//!
//! ECOSYSTEM-SPEC-27.1.1 — AAA, no mocks. Falsifier:
//! `cargo test -p ggen-graph --test brand_fluid_clamp_court`

use std::fs;
use std::path::PathBuf;

use ggen_graph::layout::brand::{
    admit_brand, parse_and_admit_brand, parse_brand_toml, parse_cqi_slope, BrandRefusal,
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

#[test]
fn oem_fixtures_encode_positive_matching_cqi_slopes() {
    let fourth = load_fixture("fourth-watch.brand.toml");
    let cindy = load_fixture("cindycat.brand.toml");
    let fourth_ok = parse_and_admit_brand(&fourth).expect("fourth-watch must admit");
    assert!(fourth_ok.fluid_count >= 1);
    let fourth_parsed = parse_brand_toml(&fourth).expect("parse fourth-watch");
    for fluid in &fourth_parsed.rhythm.fluid {
        assert!(fluid.slope_cqi > 0.0, "{} slope must be positive", fluid.name);
        let preferred_cqi = parse_cqi_slope(&fluid.preferred).expect("Ncqi preferred");
        assert!((fluid.slope_cqi - preferred_cqi).abs() < 1e-5);
    }
    let cindy_ok = parse_and_admit_brand(&cindy).expect("cindycat must admit");
    assert!(cindy_ok.fluid_count >= 1);
    let cindy_parsed = parse_brand_toml(&cindy).expect("parse cindycat");
    for fluid in &cindy_parsed.rhythm.fluid {
        assert!(fluid.slope_cqi > 0.0);
        let preferred_cqi = parse_cqi_slope(&fluid.preferred).expect("Ncqi preferred");
        assert!((fluid.slope_cqi - preferred_cqi).abs() < 1e-5);
    }
}

#[test]
fn non_positive_clamp_slope_is_refused() {
    // Arrange — zero slope is not a fluid clamp
    let mut manifest = parse_brand_toml(&load_fixture("fourth-watch.brand.toml"))
        .expect("valid fixture parses");
    manifest.rhythm.fluid[0].slope_cqi = 0.0;

    // Act
    let result = admit_brand(&manifest);

    // Assert
    match result {
        Err(BrandRefusal::InvalidClampSlope { token, slope_cqi }) => {
            assert_eq!(token, "space-fluid-sm");
            assert!((slope_cqi - 0.0).abs() < f32::EPSILON);
        }
        other => panic!("expected InvalidClampSlope(0), got {other:?}"),
    }
}

#[test]
fn non_cqi_preferred_rhythm_is_refused() {
    // Arrange — rem preferred is not the Gate-1 cqi rhythm
    let mut manifest =
        parse_brand_toml(&load_fixture("cindycat.brand.toml")).expect("valid fixture parses");
    manifest.rhythm.fluid[1].preferred = "3rem".to_string();

    // Act
    let result = admit_brand(&manifest);

    // Assert
    match result {
        Err(BrandRefusal::InvalidCqiPreferred { token, preferred }) => {
            assert_eq!(token, "space-fluid-md");
            assert_eq!(preferred, "3rem");
        }
        other => panic!("expected InvalidCqiPreferred, got {other:?}"),
    }
}

#[test]
fn clamp_slope_mismatch_is_refused() {
    // Arrange — explicit slope disagrees with preferred coefficient
    let mut manifest = parse_brand_toml(&load_fixture("fourth-watch.brand.toml"))
        .expect("valid fixture parses");
    manifest.rhythm.fluid[0].slope_cqi = 9.0;

    // Act
    let result = admit_brand(&manifest);

    // Assert
    match result {
        Err(BrandRefusal::ClampSlopeMismatch {
            token,
            slope_cqi,
            preferred_cqi,
        }) => {
            assert_eq!(token, "space-fluid-sm");
            assert!((slope_cqi - 9.0).abs() < f32::EPSILON);
            assert!((preferred_cqi - 2.0).abs() < f32::EPSILON);
        }
        other => panic!("expected ClampSlopeMismatch, got {other:?}"),
    }
}

#[test]
fn inverted_clamp_min_gt_max_is_refused() {
    // Arrange
    let mut manifest =
        parse_brand_toml(&load_fixture("cindycat.brand.toml")).expect("valid fixture parses");
    manifest.rhythm.fluid[0].min_px = 24.0;
    manifest.rhythm.fluid[0].max_px = 8.0;

    // Act
    let result = admit_brand(&manifest);

    // Assert
    match result {
        Err(BrandRefusal::InvertedClamp {
            token,
            min_px,
            max_px,
        }) => {
            assert_eq!(token, "space-fluid-sm");
            assert!((min_px - 24.0).abs() < f32::EPSILON);
            assert!((max_px - 8.0).abs() < f32::EPSILON);
        }
        other => panic!("expected InvertedClamp, got {other:?}"),
    }
}

#[test]
fn off_grid_fluid_min_is_refused() {
    // Arrange — 6px is off the 4px/8px grid
    let mut manifest = parse_brand_toml(&load_fixture("fourth-watch.brand.toml"))
        .expect("valid fixture parses");
    manifest.rhythm.fluid[0].min_px = 6.0;

    // Act
    let result = admit_brand(&manifest);

    // Assert
    match result {
        Err(BrandRefusal::OffGridFluid {
            token,
            field,
            value_px,
            grid_half_px,
        }) => {
            assert_eq!(token, "space-fluid-sm");
            assert_eq!(field, "min_px");
            assert!((value_px - 6.0).abs() < f32::EPSILON);
            assert_eq!(grid_half_px, 4);
        }
        other => panic!("expected OffGridFluid(min_px), got {other:?}"),
    }
}
