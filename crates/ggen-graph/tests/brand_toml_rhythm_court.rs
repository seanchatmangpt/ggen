//! Chicago court: DTCG `*.brand.toml` visual-rhythm admission.
//!
//! ECOSYSTEM-SPEC-27.1.1 Gate 1 D4 slice — AAA, no mocks, state-based.
//! Falsifier: `cargo test -p ggen-graph --test brand_toml_rhythm_court`

use std::fs;
use std::path::PathBuf;

use ggen_graph::layout::brand::{
    admit_brand, parse_and_admit_brand, parse_brand_toml, parse_cqi_slope, BrandRefusal,
    DTCG_VERSION, INTER_TARGET_GAP_PX, TOUCH_FLOOR_APPLE_PX, TOUCH_FLOOR_MATERIAL_PX,
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
fn fourth_watch_and_cindycat_fixtures_are_admitted() {
    // Arrange — real OEM stubs on disk (not invented inline doubles)
    let fourth = load_fixture("fourth-watch.brand.toml");
    let cindy = load_fixture("cindycat.brand.toml");

    // Act
    let fourth_receipt = parse_and_admit_brand(&fourth);
    let cindy_receipt = parse_and_admit_brand(&cindy);

    // Assert
    let fourth_ok = fourth_receipt.expect("fourth-watch.brand.toml must admit");
    assert_eq!(fourth_ok.brand_id, "fourth-watch");
    assert_eq!(fourth_ok.dtcg_version, DTCG_VERSION);
    assert!(
        fourth_ok.spacing_count >= 5,
        "fourth-watch must encode a spacing scale; got {}",
        fourth_ok.spacing_count
    );
    assert!(
        fourth_ok.fluid_count >= 1,
        "fourth-watch must encode at least one fluid clamp/cqi slope"
    );

    let cindy_ok = cindy_receipt.expect("cindycat.brand.toml must admit");
    assert_eq!(cindy_ok.brand_id, "cindycat");
    assert_eq!(cindy_ok.dtcg_version, DTCG_VERSION);

    let fourth_parsed = parse_brand_toml(&fourth).expect("parse fourth-watch");
    assert!(
        (fourth_parsed.rhythm.touch_floor_apple_px - TOUCH_FLOOR_APPLE_PX).abs() < f32::EPSILON
    );
    assert!(
        (fourth_parsed.rhythm.touch_floor_material_px - TOUCH_FLOOR_MATERIAL_PX).abs()
            < f32::EPSILON
    );
    assert!((fourth_parsed.rhythm.inter_target_gap_px - INTER_TARGET_GAP_PX).abs() < f32::EPSILON);

    // Clamp slopes on disk must be explicit, positive, and match preferred cqi.
    for fluid in &fourth_parsed.rhythm.fluid {
        assert!(
            fluid.slope_cqi > 0.0,
            "fourth-watch fluid {} must declare positive slope_cqi",
            fluid.name
        );
        let preferred_cqi =
            parse_cqi_slope(&fluid.preferred).expect("preferred must be positive Ncqi");
        assert!(
            (fluid.slope_cqi - preferred_cqi).abs() < 1e-5,
            "fourth-watch fluid {} slope_cqi={} must match preferred {}",
            fluid.name,
            fluid.slope_cqi,
            fluid.preferred
        );
    }

    let cindy_parsed = parse_brand_toml(&cindy).expect("parse cindycat");
    assert!(
        !cindy_parsed.rhythm.fluid.is_empty(),
        "cindycat must encode fluid clamp slopes"
    );
    for fluid in &cindy_parsed.rhythm.fluid {
        assert!(fluid.slope_cqi > 0.0);
        let preferred_cqi =
            parse_cqi_slope(&fluid.preferred).expect("cindycat preferred must be Ncqi");
        assert!((fluid.slope_cqi - preferred_cqi).abs() < 1e-5);
    }
}

#[test]
fn cindycat_fixture_encodes_patronage_messaging_contract() {
    // Arrange — CindyCat-first patronage slots on disk (plain TOML, no private NS)
    let cindy = parse_brand_toml(&load_fixture("cindycat.brand.toml")).expect("parse cindycat");

    // Act — rhythm admit still holds with messaging present
    let receipt = admit_brand(&cindy).expect("cindycat with patronage must admit");

    // Assert — identity + patronage copy slots
    assert_eq!(receipt.brand_id, "cindycat");
    assert_eq!(cindy.brand.id.as_deref(), Some("cindycat"));
    assert_eq!(cindy.brand.tagline.as_deref(), Some("Reverse Gloo"));
    assert_eq!(cindy.brand.house.as_deref(), Some("woman-owned house"));

    let patronage = &cindy.messaging.patronage;
    assert_eq!(patronage.headline.as_deref(), Some("Her Patronage Restored"));
    assert_eq!(patronage.fair_pay.as_deref(), Some("Luke 10:7 fair pay"));
    assert_eq!(patronage.custody.as_deref(), Some("0% custody"));
    assert_eq!(patronage.skimming.as_deref(), Some("0.00% skimming"));
    assert_eq!(patronage.tagline.as_deref(), Some("Reverse Gloo"));
    assert_eq!(patronage.house.as_deref(), Some("woman-owned house"));

    let commercial = &cindy.messaging.commercial;
    assert_eq!(commercial.custody_pct, Some(0.0));
    assert_eq!(commercial.skimming_pct, Some(0.0));
    assert_eq!(commercial.transaction_rent_pct, Some(0.0));

    // Rhythm floors unchanged (touch≥44, gap≥8)
    assert!((cindy.rhythm.touch_floor_apple_px - TOUCH_FLOOR_APPLE_PX).abs() < f32::EPSILON);
    assert!((cindy.rhythm.inter_target_gap_px - INTER_TARGET_GAP_PX).abs() < f32::EPSILON);
}

#[test]
fn fourth_watch_fixture_admits_without_patronage_slots() {
    // Arrange — Fourth Watch Gate-1 rhythm stub omits CindyCat patronage
    let fourth =
        parse_brand_toml(&load_fixture("fourth-watch.brand.toml")).expect("parse fourth-watch");

    // Act
    let receipt = admit_brand(&fourth).expect("fourth-watch must still admit");

    // Assert — optional messaging defaults empty
    assert_eq!(receipt.brand_id, "fourth-watch");
    assert!(fourth.messaging.patronage.headline.is_none());
    assert!(fourth.messaging.commercial.custody_pct.is_none());
}

#[test]
fn touch_floor_under_44_is_refused() {
    // Arrange — Apple floor dropped below 44px
    let mut manifest =
        parse_brand_toml(&load_fixture("fourth-watch.brand.toml")).expect("valid fixture parses");
    manifest.rhythm.touch_floor_apple_px = 40.0;

    // Act
    let result = admit_brand(&manifest);

    // Assert
    match result {
        Err(BrandRefusal::TouchFloorTooSmall {
            platform,
            min_px,
            value_px,
        }) => {
            assert_eq!(platform, "apple");
            assert_eq!(min_px, 44);
            assert!((value_px - 40.0).abs() < f32::EPSILON);
        }
        other => panic!("expected TouchFloorTooSmall(<44), got {other:?}"),
    }
}

#[test]
fn inter_target_gap_under_8_is_refused() {
    // Arrange
    let mut manifest =
        parse_brand_toml(&load_fixture("cindycat.brand.toml")).expect("valid fixture parses");
    manifest.rhythm.inter_target_gap_px = 4.0;

    // Act
    let result = admit_brand(&manifest);

    // Assert
    match result {
        Err(BrandRefusal::GapTooSmall { gap_px, min_px }) => {
            assert_eq!(min_px, 8);
            assert!((gap_px - 4.0).abs() < f32::EPSILON);
        }
        other => panic!("expected GapTooSmall(<8), got {other:?}"),
    }
}

#[test]
fn non_grid_spacing_is_refused() {
    // Arrange — 6px is off the 4px/8px grid
    let mut manifest =
        parse_brand_toml(&load_fixture("fourth-watch.brand.toml")).expect("valid fixture parses");
    manifest.rhythm.spacing[2].px = 6.0;

    // Act
    let result = admit_brand(&manifest);

    // Assert
    match result {
        Err(BrandRefusal::OffGridSpacing {
            value_px,
            grid_half_px,
            ..
        }) => {
            assert!((value_px - 6.0).abs() < f32::EPSILON);
            assert_eq!(grid_half_px, 4);
        }
        other => panic!("expected OffGridSpacing, got {other:?}"),
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
