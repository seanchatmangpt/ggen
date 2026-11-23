//! Chicago court: brand.toml OEM hot-swap admit/refuse fence.
//!
//! ECOSYSTEM-SPEC-27.1.1 Gate 1 D4 companion — AAA, no mocks, state-based.
//! Proves Fourth Watch ↔ CindyCat in-memory swap under rhythm law and typed
//! refusals for unknown OEM / same-brand / rhythm failures.
//! Standing ceiling is PARTIAL_ALIVE — never production theme/CASTLE ALIVE.
//!
//! Falsifier: `cargo test -p ggen-graph --test brand_toml_hotswap_court`

use std::fs;
use std::path::PathBuf;

use ggen_graph::layout::brand::{
    hot_swap_brand, seed_hot_swap_slot, BrandHotSwapRefusal, BrandRefusal, HotSwapStanding,
    ADMITTED_OEM_BRAND_IDS, HOT_SWAP_STANDING_CEILING,
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
fn fourth_watch_to_cindycat_hot_swap_is_partial_alive() {
    // Arrange — real OEM stubs on disk
    let fourth = load_fixture("fourth-watch.brand.toml");
    let cindy = load_fixture("cindycat.brand.toml");

    // Act — seed Fourth Watch, swap to CindyCat
    let (mut slot, seed) = seed_hot_swap_slot(&fourth).expect("fourth-watch must seed");
    assert_eq!(seed.from_brand_id, None);
    assert_eq!(seed.to_brand_id, "fourth-watch");
    assert_eq!(seed.standing, HotSwapStanding::PartialAlive);
    assert_eq!(seed.standing.as_str(), HOT_SWAP_STANDING_CEILING);
    assert_eq!(slot.brand_id(), "fourth-watch");

    let swap = hot_swap_brand(&mut slot, &cindy).expect("cindycat hot-swap must admit");

    // Assert — transition receipt + active slot; never ALIVE string
    assert_eq!(swap.from_brand_id.as_deref(), Some("fourth-watch"));
    assert_eq!(swap.to_brand_id, "cindycat");
    assert_eq!(swap.standing, HotSwapStanding::PartialAlive);
    assert_eq!(swap.standing.as_str(), "PARTIAL_ALIVE");
    assert_ne!(swap.standing.as_str(), "ALIVE");
    assert_eq!(slot.brand_id(), "cindycat");
    assert_eq!(slot.receipt().brand_id, "cindycat");
    assert_eq!(
        slot.manifest().brand.tagline.as_deref(),
        Some("Reverse Gloo")
    );
}

#[test]
fn cindycat_back_to_fourth_watch_round_trips() {
    // Arrange
    let fourth = load_fixture("fourth-watch.brand.toml");
    let cindy = load_fixture("cindycat.brand.toml");
    let (mut slot, _) = seed_hot_swap_slot(&cindy).expect("cindycat seed");

    // Act
    let to_fourth = hot_swap_brand(&mut slot, &fourth).expect("swap to fourth-watch");
    let back = hot_swap_brand(&mut slot, &cindy).expect("swap back to cindycat");

    // Assert
    assert_eq!(to_fourth.from_brand_id.as_deref(), Some("cindycat"));
    assert_eq!(to_fourth.to_brand_id, "fourth-watch");
    assert_eq!(back.from_brand_id.as_deref(), Some("fourth-watch"));
    assert_eq!(back.to_brand_id, "cindycat");
    assert_eq!(slot.brand_id(), "cindycat");
    assert!(ADMITTED_OEM_BRAND_IDS.contains(&"fourth-watch"));
    assert!(ADMITTED_OEM_BRAND_IDS.contains(&"cindycat"));
}

#[test]
fn same_brand_hot_swap_is_refused() {
    // Arrange
    let fourth = load_fixture("fourth-watch.brand.toml");
    let (mut slot, _) = seed_hot_swap_slot(&fourth).expect("seed");

    // Act
    let result = hot_swap_brand(&mut slot, &fourth);

    // Assert
    match result {
        Err(BrandHotSwapRefusal::SameBrand { brand_id }) => {
            assert_eq!(brand_id, "fourth-watch");
            assert_eq!(
                BrandHotSwapRefusal::SameBrand {
                    brand_id: "fourth-watch".into()
                }
                .code(),
                "REFUSED_BRAND_HOTSWAP_SAME"
            );
        }
        other => panic!("expected SameBrand, got {other:?}"),
    }
    assert_eq!(
        slot.brand_id(),
        "fourth-watch",
        "slot must stay put on refuse"
    );
}

#[test]
fn unknown_oem_brand_id_is_refused() {
    // Arrange — valid rhythm shape, invented brand_id (not OEM allowlist)
    let mut forged = load_fixture("fourth-watch.brand.toml");
    forged = forged.replacen(
        "brand_id = \"fourth-watch\"",
        "brand_id = \"invented-brand\"",
        1,
    );
    let fourth = load_fixture("fourth-watch.brand.toml");
    let (mut slot, _) = seed_hot_swap_slot(&fourth).expect("seed");

    // Act
    let result = hot_swap_brand(&mut slot, &forged);

    // Assert
    match result {
        Err(BrandHotSwapRefusal::OemBrandUnsupported { brand_id }) => {
            assert_eq!(brand_id, "invented-brand");
            assert_eq!(
                BrandHotSwapRefusal::OemBrandUnsupported {
                    brand_id: "x".into()
                }
                .code(),
                "REFUSED_BRAND_HOTSWAP_OEM"
            );
        }
        other => panic!("expected OemBrandUnsupported, got {other:?}"),
    }
}

#[test]
fn rhythm_refusal_blocks_hot_swap() {
    // Arrange — Apple touch floor dropped below 44px on target
    let mut bad = load_fixture("cindycat.brand.toml");
    bad = bad.replacen("touch_floor_apple_px = 44", "touch_floor_apple_px = 40", 1);
    let fourth = load_fixture("fourth-watch.brand.toml");
    let (mut slot, _) = seed_hot_swap_slot(&fourth).expect("seed");

    // Act
    let result = hot_swap_brand(&mut slot, &bad);

    // Assert
    match result {
        Err(BrandHotSwapRefusal::Rhythm(BrandRefusal::TouchFloorTooSmall {
            platform,
            min_px,
            value_px,
        })) => {
            assert_eq!(platform, "apple");
            assert_eq!(min_px, 44);
            assert!((value_px - 40.0).abs() < f32::EPSILON);
            assert_eq!(
                BrandHotSwapRefusal::Rhythm(BrandRefusal::Parse("x".into())).code(),
                "REFUSED_BRAND_HOTSWAP_RHYTHM"
            );
        }
        other => panic!("expected Rhythm(TouchFloorTooSmall), got {other:?}"),
    }
    assert_eq!(slot.brand_id(), "fourth-watch");
}

#[test]
fn seed_of_unknown_oem_is_refused() {
    // Arrange
    let mut forged = load_fixture("cindycat.brand.toml");
    forged = forged.replacen("brand_id = \"cindycat\"", "brand_id = \"not-an-oem\"", 1);

    // Act
    let result = seed_hot_swap_slot(&forged);

    // Assert
    match result {
        Err(BrandHotSwapRefusal::OemBrandUnsupported { brand_id }) => {
            assert_eq!(brand_id, "not-an-oem");
        }
        other => panic!("expected OemBrandUnsupported on seed, got {other:?}"),
    }
}
