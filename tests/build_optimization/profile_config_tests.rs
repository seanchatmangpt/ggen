//! Profile Configuration Tests
//!
//! Validates that all Cargo profiles (dev, test, release, bench) are configured
//! correctly with appropriate optimization levels and build settings.
//!
//! Chicago TDD Pattern:
//! - State-based testing (verify profile settings affect build output)
//! - Real objects (actual Cargo.toml, profile configurations)
//! - AAA pattern (Arrange/Act/Assert)

use std::path::Path;
use std::process::Command;

/// Helper to run cargo command and capture output
fn run_cargo_command(args: &[&str]) -> Result<String, String> {
    let output = Command::new("cargo")
        .args(args)
        .current_dir("/home/user/ggen")
        .output()
        .map_err(|e| format!("Failed to run cargo: {}", e))?;

    if output.status.success() {
        Ok(String::from_utf8_lossy(&output.stdout).to_string())
    } else {
        Err(String::from_utf8_lossy(&output.stderr).to_string())
    }
}

// ============================================================================
// Profile Configuration Tests (15 total)
// ============================================================================

/// Test 1: Dev Profile Settings Verification
/// Verifies that dev profile has correct optimization level and debug settings
#[test]
fn test_dev_profile_opt_level_zero() {
    // Arrange: Read Cargo.toml and parse profile.dev section
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    // Act: Verify dev profile settings
    let has_opt_level_0 = cargo_toml.contains("[profile.dev]")
        && cargo_toml.contains("opt-level = 0");

    // Assert: Dev profile should have opt-level = 0 for fast compilation
    assert!(
        has_opt_level_0,
        "Dev profile must have opt-level = 0 for fast development builds"
    );
}

/// Test 2: Dev Profile Debug Enabled
/// Verifies that dev profile has debug symbols enabled
#[test]
fn test_dev_profile_debug_enabled() {
    let cargo_toml =
        std::fs::read_to_string("/home/user/ggen/Cargo.toml").expect("Failed to read Cargo.toml");

    let dev_profile_section = cargo_toml
        .split("[profile.dev]")
        .nth(1)
        .and_then(|section| section.split('[').next());

    let has_debug_true = dev_profile_section
        .map(|s| s.contains("debug = true"))
        .unwrap_or(false);

    assert!(
        has_debug_true,
        "Dev profile should have debug = true for debugging support"
    );
}

/// Test 3: Dev Profile Codegen Units High
/// Verifies that dev profile has high codegen-units for parallel compilation
#[test]
fn test_dev_profile_codegen_units_high() {
    let cargo_toml =
        std::fs::read_to_string("/home/user/ggen/Cargo.toml").expect("Failed to read Cargo.toml");

    let dev_profile_section = cargo_toml
        .split("[profile.dev]")
        .nth(1)
        .and_then(|section| section.split('[').next());

    let has_high_codegen = dev_profile_section
        .map(|s| s.contains("codegen-units = 256"))
        .unwrap_or(false);

    assert!(
        has_high_codegen,
        "Dev profile should have codegen-units = 256 for faster parallel compilation"
    );
}

/// Test 4: Dev Profile LTO Disabled
/// Verifies that dev profile has LTO disabled for faster builds
#[test]
fn test_dev_profile_lto_disabled() {
    let cargo_toml =
        std::fs::read_to_string("/home/user/ggen/Cargo.toml").expect("Failed to read Cargo.toml");

    let dev_profile_section = cargo_toml
        .split("[profile.dev]")
        .nth(1)
        .and_then(|section| section.split('[').next());

    let has_lto_false = dev_profile_section
        .map(|s| s.contains("lto = false"))
        .unwrap_or(false);

    assert!(
        has_lto_false,
        "Dev profile should have lto = false for faster builds"
    );
}

/// Test 5: Dev Profile Incremental Enabled
/// Verifies that dev profile has incremental compilation enabled
#[test]
fn test_dev_profile_incremental_enabled() {
    let cargo_toml =
        std::fs::read_to_string("/home/user/ggen/Cargo.toml").expect("Failed to read Cargo.toml");

    let dev_profile_section = cargo_toml
        .split("[profile.dev]")
        .nth(1)
        .and_then(|section| section.split('[').next());

    let has_incremental = dev_profile_section
        .map(|s| s.contains("incremental = true"))
        .unwrap_or(false);

    assert!(
        has_incremental,
        "Dev profile should have incremental = true for faster rebuilds"
    );
}

/// Test 6: Release Profile Optimization Level 3
/// Verifies that release profile has maximum optimization
#[test]
fn test_release_profile_opt_level_3() {
    let cargo_toml =
        std::fs::read_to_string("/home/user/ggen/Cargo.toml").expect("Failed to read Cargo.toml");

    let release_profile = cargo_toml
        .split("[profile.release]")
        .nth(1)
        .and_then(|section| section.split('[').next());

    let has_opt_level_3 = release_profile
        .map(|s| s.contains("opt-level = 3"))
        .unwrap_or(false);

    assert!(
        has_opt_level_3,
        "Release profile should have opt-level = 3 for maximum optimization"
    );
}

/// Test 7: Release Profile LTO Thin
/// Verifies that release profile has thin LTO enabled
#[test]
fn test_release_profile_lto_thin() {
    let cargo_toml =
        std::fs::read_to_string("/home/user/ggen/Cargo.toml").expect("Failed to read Cargo.toml");

    let release_profile = cargo_toml
        .split("[profile.release]")
        .nth(1)
        .and_then(|section| section.split('[').next());

    let has_lto_thin = release_profile
        .map(|s| s.contains("lto = \"thin\""))
        .unwrap_or(false);

    assert!(
        has_lto_thin,
        "Release profile should have lto = \"thin\" for balanced build time and optimization"
    );
}

/// Test 8: Release Profile Codegen Units Low
/// Verifies that release profile has low codegen-units for better optimization
#[test]
fn test_release_profile_codegen_units_low() {
    let cargo_toml =
        std::fs::read_to_string("/home/user/ggen/Cargo.toml").expect("Failed to read Cargo.toml");

    let release_profile = cargo_toml
        .split("[profile.release]")
        .nth(1)
        .and_then(|section| section.split('[').next());

    let has_low_codegen = release_profile
        .map(|s| s.contains("codegen-units = 4"))
        .unwrap_or(false);

    assert!(
        has_low_codegen,
        "Release profile should have codegen-units = 4 for better optimization"
    );
}

/// Test 9: Release Profile Strip Enabled
/// Verifies that release profile has symbol stripping enabled
#[test]
fn test_release_profile_strip_enabled() {
    let cargo_toml =
        std::fs::read_to_string("/home/user/ggen/Cargo.toml").expect("Failed to read Cargo.toml");

    let release_profile = cargo_toml
        .split("[profile.release]")
        .nth(1)
        .and_then(|section| section.split('[').next());

    let has_strip = release_profile
        .map(|s| s.contains("strip = true"))
        .unwrap_or(false);

    assert!(
        has_strip,
        "Release profile should have strip = true for smaller binary size"
    );
}

/// Test 10: Release Profile Panic Abort
/// Verifies that release profile uses panic = abort for smaller binaries
#[test]
fn test_release_profile_panic_abort() {
    let cargo_toml =
        std::fs::read_to_string("/home/user/ggen/Cargo.toml").expect("Failed to read Cargo.toml");

    let release_profile = cargo_toml
        .split("[profile.release]")
        .nth(1)
        .and_then(|section| section.split('[').next());

    let has_panic_abort = release_profile
        .map(|s| s.contains("panic = \"abort\""))
        .unwrap_or(false);

    assert!(
        has_panic_abort,
        "Release profile should have panic = \"abort\" for smaller binary size"
    );
}

/// Test 11: Test Profile Optimization Level 0
/// Verifies that test profile has opt-level 0 for fast compilation
#[test]
fn test_profile_opt_level_zero() {
    let cargo_toml =
        std::fs::read_to_string("/home/user/ggen/Cargo.toml").expect("Failed to read Cargo.toml");

    let test_profile = cargo_toml
        .split("[profile.test]")
        .nth(1)
        .and_then(|section| section.split('[').next());

    let has_opt_level_0 = test_profile
        .map(|s| s.contains("opt-level = 0"))
        .unwrap_or(false);

    assert!(
        has_opt_level_0,
        "Test profile should have opt-level = 0 for fast test compilation"
    );
}

/// Test 12: Test Profile LTO Disabled
/// Verifies that test profile has LTO disabled
#[test]
fn test_profile_lto_disabled() {
    let cargo_toml =
        std::fs::read_to_string("/home/user/ggen/Cargo.toml").expect("Failed to read Cargo.toml");

    let test_profile = cargo_toml
        .split("[profile.test]")
        .nth(1)
        .and_then(|section| section.split('[').next());

    let has_lto_false = test_profile
        .map(|s| s.contains("lto = false"))
        .unwrap_or(false);

    assert!(
        has_lto_false,
        "Test profile should have lto = false for faster test compilation"
    );
}

/// Test 13: Bench Profile Codegen Units One
/// Verifies that bench profile has codegen-units = 1 for maximum optimization
#[test]
fn test_bench_profile_codegen_units_one() {
    let cargo_toml =
        std::fs::read_to_string("/home/user/ggen/Cargo.toml").expect("Failed to read Cargo.toml");

    let bench_profile = cargo_toml
        .split("[profile.bench]")
        .nth(1)
        .and_then(|section| section.split('[').next());

    let has_codegen_one = bench_profile
        .map(|s| s.contains("codegen-units = 1"))
        .unwrap_or(false);

    assert!(
        has_codegen_one,
        "Bench profile should have codegen-units = 1 for maximum optimization"
    );
}

/// Test 14: Bench Profile LTO Enabled
/// Verifies that bench profile has full LTO enabled
#[test]
fn test_bench_profile_lto_enabled() {
    let cargo_toml =
        std::fs::read_to_string("/home/user/ggen/Cargo.toml").expect("Failed to read Cargo.toml");

    let bench_profile = cargo_toml
        .split("[profile.bench]")
        .nth(1)
        .and_then(|section| section.split('[').next());

    let has_lto_full = bench_profile
        .map(|s| s.contains("lto = true"))
        .unwrap_or(false);

    assert!(
        has_lto_full,
        "Bench profile should have lto = true for maximum optimization"
    );
}

/// Test 15: Profile Interaction - Dev and Release Different
/// Verifies that dev and release profiles have different settings
#[test]
fn test_profile_interaction_dev_release_different() {
    let cargo_toml =
        std::fs::read_to_string("/home/user/ggen/Cargo.toml").expect("Failed to read Cargo.toml");

    let dev_section = cargo_toml
        .split("[profile.dev]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");
    let release_section = cargo_toml
        .split("[profile.release]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let dev_opt = dev_section.contains("opt-level = 0");
    let release_opt = release_section.contains("opt-level = 3");
    let dev_lto = dev_section.contains("lto = false");
    let release_lto = release_section.contains("lto = \"thin\"");

    assert!(
        dev_opt && release_opt && dev_lto && release_lto,
        "Dev and release profiles should have different optimization settings"
    );
}
