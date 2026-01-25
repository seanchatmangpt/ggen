//! Cargo.toml Profile Configuration Tests
//!
//! Tests verify that all 4 compilation profiles (dev, release, test, bench) are correctly configured
//! with appropriate optimization levels, LTO settings, and codegen-units for SLO compliance.

use std::fs;
use std::path::PathBuf;
use toml::Value;

/// State: Parsed Cargo.toml configuration
#[derive(Debug, Clone)]
struct CargoManifest {
    dev: ProfileConfig,
    release: ProfileConfig,
    test: ProfileConfig,
    bench: ProfileConfig,
}

/// State: Individual profile configuration
#[derive(Debug, Clone, PartialEq)]
struct ProfileConfig {
    opt_level: i64,
    debug: bool,
    lto: String,
    codegen_units: i64,
    panic: Option<String>,
    strip: Option<bool>,
}

impl CargoManifest {
    fn load_from_workspace_root() -> anyhow::Result<Self> {
        let cargo_toml = std::env::var("CARGO_MANIFEST_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|_| PathBuf::from("."))
            .parent()
            .and_then(|p| p.parent())
            .ok_or_else(|| anyhow::anyhow!("Cannot find workspace root"))?
            .join("Cargo.toml");

        let content = fs::read_to_string(&cargo_toml)?;
        let toml: Value = toml::from_str(&content)?;

        let extract_profile = |name: &str| -> anyhow::Result<ProfileConfig> {
            let profile = toml
                .get("profile")
                .and_then(|p| p.get(name))
                .ok_or_else(|| anyhow::anyhow!("Profile {} not found", name))?;

            Ok(ProfileConfig {
                opt_level: profile
                    .get("opt-level")
                    .and_then(|v| v.as_integer())
                    .unwrap_or(0),
                debug: profile
                    .get("debug")
                    .and_then(|v| v.as_bool())
                    .unwrap_or(false),
                lto: profile
                    .get("lto")
                    .and_then(|v| v.as_str())
                    .map(|s| s.to_string())
                    .unwrap_or_default(),
                codegen_units: profile
                    .get("codegen-units")
                    .and_then(|v| v.as_integer())
                    .unwrap_or(16),
                panic: profile.get("panic").and_then(|v| v.as_str()).map(|s| s.to_string()),
                strip: profile.get("strip").and_then(|v| v.as_bool()),
            })
        };

        Ok(Self {
            dev: extract_profile("dev")?,
            release: extract_profile("release")?,
            test: extract_profile("test")?,
            bench: extract_profile("bench")?,
        })
    }
}

#[test]
fn test_dev_profile_fast_compilation() {
    // Arrange: Load workspace Cargo.toml
    let manifest = CargoManifest::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Inspect dev profile configuration
    let dev_profile = manifest.dev.clone();

    // Assert: Dev profile should prioritize compilation speed
    assert_eq!(dev_profile.opt_level, 0, "Dev profile must use opt-level=0 for fast builds");
    assert_eq!(dev_profile.debug, true, "Dev profile must enable debug symbols");
    assert_eq!(
        dev_profile.codegen_units, 256,
        "Dev profile must use codegen-units=256 for parallel compilation"
    );
    assert_eq!(dev_profile.lto, "", "Dev profile should disable LTO");
}

#[test]
fn test_release_profile_optimized() {
    // Arrange: Load workspace Cargo.toml
    let manifest = CargoManifest::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Inspect release profile configuration
    let release_profile = manifest.release.clone();

    // Assert: Release profile should be fully optimized with SLO compliance
    assert_eq!(
        release_profile.opt_level, 3,
        "Release profile must use opt-level=3 for maximum optimization"
    );
    assert_eq!(release_profile.debug, false, "Release profile must disable debug");
    assert_eq!(
        release_profile.codegen_units, 4,
        "Release profile must use codegen-units=4 per CARGO_OPTIMIZATION_PLAN"
    );
    assert_eq!(release_profile.lto, "thin", "Release profile must use thin LTO");
    assert_eq!(
        release_profile.panic,
        Some("abort".to_string()),
        "Release profile must use panic=abort for smaller binaries"
    );
    assert_eq!(
        release_profile.strip,
        Some(true),
        "Release profile must strip symbols"
    );
}

#[test]
fn test_test_profile_fast_compilation() {
    // Arrange: Load workspace Cargo.toml
    let manifest = CargoManifest::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Inspect test profile configuration
    let test_profile = manifest.test.clone();

    // Assert: Test profile should balance speed and debuggability
    assert_eq!(test_profile.opt_level, 0, "Test profile must use opt-level=0 for fast compilation");
    assert_eq!(test_profile.debug, true, "Test profile must enable debug symbols");
    assert_eq!(
        test_profile.codegen_units, 256,
        "Test profile must use codegen-units=256 for parallel compilation"
    );
    assert_eq!(test_profile.lto, "", "Test profile should disable LTO");
}

#[test]
fn test_bench_profile_fully_optimized() {
    // Arrange: Load workspace Cargo.toml
    let manifest = CargoManifest::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Inspect bench profile configuration
    let bench_profile = manifest.bench.clone();

    // Assert: Bench profile should be fully optimized
    assert_eq!(bench_profile.opt_level, 3, "Bench profile must use opt-level=3");
    assert_eq!(bench_profile.debug, false, "Bench profile must disable debug");
    assert_eq!(
        bench_profile.codegen_units, 1,
        "Bench profile must use codegen-units=1 for best optimization"
    );
    assert_eq!(bench_profile.lto, "true", "Bench profile must use full LTO");
    assert_eq!(
        bench_profile.panic,
        Some("abort".to_string()),
        "Bench profile must use panic=abort"
    );
}

#[test]
fn test_profile_codegen_units_hierarchy() {
    // Arrange: Load all profiles
    let manifest = CargoManifest::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Examine codegen-units progression across all profiles
    let dev_units = manifest.dev.codegen_units;
    let test_units = manifest.test.codegen_units;
    let release_units = manifest.release.codegen_units;
    let bench_units = manifest.bench.codegen_units;

    // Assert: Verify the hierarchy: dev/test (fast build) > release > bench (best optimization)
    assert!(
        dev_units >= release_units && release_units > bench_units,
        "Codegen-units should decrease from dev ({}) to release ({}) to bench ({})",
        dev_units,
        release_units,
        bench_units
    );
}

#[test]
fn test_lto_progression_for_optimization_levels() {
    // Arrange: Load all profiles
    let manifest = CargoManifest::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Collect LTO settings
    let dev_lto = manifest.dev.lto.clone();
    let release_lto = manifest.release.lto.clone();
    let bench_lto = manifest.bench.lto.clone();

    // Assert: Verify LTO increases with optimization priority
    assert!(
        dev_lto.is_empty() || dev_lto == "false",
        "Dev profile should disable LTO (got: {})",
        dev_lto
    );
    assert_eq!(release_lto, "thin", "Release profile should use thin LTO (got: {})", release_lto);
    assert_eq!(bench_lto, "true", "Bench profile should use full LTO (got: {})", bench_lto);
}

#[test]
fn test_debug_symbols_for_debugging_profiles() {
    // Arrange: Load all profiles
    let manifest = CargoManifest::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Check debug symbol configuration
    let dev_debug = manifest.dev.debug;
    let test_debug = manifest.test.debug;
    let release_debug = manifest.release.debug;

    // Assert: Debug profile should enable symbols for development and testing
    assert!(dev_debug, "Dev profile must enable debug symbols");
    assert!(test_debug, "Test profile must enable debug symbols");
    assert!(!release_debug, "Release profile should disable debug symbols");
}

#[test]
fn test_profile_panic_abort_for_binary_size() {
    // Arrange: Load profiles that care about binary size
    let manifest = CargoManifest::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Check panic setting
    let release_panic = manifest.release.panic.clone();
    let bench_panic = manifest.bench.panic.clone();

    // Assert: Production profiles should use panic=abort for smaller binaries
    assert_eq!(
        release_panic,
        Some("abort".to_string()),
        "Release profile must use panic=abort for smaller binary size"
    );
    assert_eq!(
        bench_panic,
        Some("abort".to_string()),
        "Bench profile must use panic=abort for consistency"
    );
}

#[test]
fn test_dev_and_test_profiles_match_for_incremental_builds() {
    // Arrange: Load workspace Cargo.toml
    let manifest = CargoManifest::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Compare dev and test profile settings
    let dev = manifest.dev.clone();
    let test = manifest.test.clone();

    // Assert: Dev and test should have similar settings for consistent incremental builds
    assert_eq!(
        dev.opt_level, test.opt_level,
        "Dev and test profiles should match opt-level for consistent builds"
    );
    assert_eq!(
        dev.codegen_units, test.codegen_units,
        "Dev and test profiles should match codegen-units"
    );
}

#[test]
fn test_profile_lto_and_codegen_units_inverse_relationship() {
    // Arrange: Load release and bench profiles
    let manifest = CargoManifest::load_from_workspace_root().expect("Failed to load Cargo.toml");

    // Act: Inspect the inverse relationship between LTO and codegen-units
    let release = manifest.release.clone();
    let bench = manifest.bench.clone();

    // Assert: Higher LTO should correlate with lower codegen-units
    // Release: thin LTO + 4 units (balance)
    // Bench: full LTO + 1 unit (maximum optimization)
    assert!(
        release.codegen_units > bench.codegen_units,
        "Release should have higher codegen-units than bench"
    );
    assert!(
        bench.lto == "true" && release.lto == "thin",
        "Bench should have full LTO, release should have thin LTO"
    );
}
