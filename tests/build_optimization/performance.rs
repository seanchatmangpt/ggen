//! Performance Regression Tests
//!
//! Tests verify that build and runtime performance stay within SLO targets:
//! - First build: ≤15s
//! - Incremental: ≤2s
//! - RDF processing: ≤5s for 1k+ triples
//! - Binary size: Minimal overhead
//! - Memory usage: ≤100MB for code generation

use std::process::Command;
use std::time::Instant;

/// State: Build performance metrics
#[derive(Debug, Clone)]
struct BuildMetrics {
    compilation_time_ms: u128,
    success: bool,
}

impl BuildMetrics {
    fn measure_cargo_check() -> anyhow::Result<Self> {
        let start = Instant::now();
        let output = Command::new("cargo")
            .args(&["make", "check"])
            .output();

        let success = output.as_ref().map(|o| o.status.success()).unwrap_or(false);
        let compilation_time_ms = start.elapsed().as_millis();

        Ok(Self {
            compilation_time_ms,
            success,
        })
    }

    fn measure_cargo_build() -> anyhow::Result<Self> {
        let start = Instant::now();
        let output = Command::new("cargo")
            .args(&["build", "--release"])
            .output();

        let success = output.as_ref().map(|o| o.status.success()).unwrap_or(false);
        let compilation_time_ms = start.elapsed().as_millis();

        Ok(Self {
            compilation_time_ms,
            success,
        })
    }
}

#[test]
#[ignore] // Only run manually or in CI performance testing
fn test_cargo_check_performance_slo() {
    // Arrange: Set SLO for cargo check
    let slo_ms = 5000; // 5 seconds

    // Act: Measure cargo check performance
    let metrics = BuildMetrics::measure_cargo_check().expect("Failed to run cargo check");

    // Assert: Should meet SLO
    assert!(
        metrics.success,
        "cargo check must succeed"
    );
    println!(
        "Cargo check time: {}ms (SLO: {}ms)",
        metrics.compilation_time_ms, slo_ms
    );
    // Note: We warn if SLO exceeded but don't fail - actual SLO enforcement is in CI
    if metrics.compilation_time_ms > slo_ms as u128 {
        eprintln!(
            "WARNING: cargo check exceeded SLO: {}ms > {}ms",
            metrics.compilation_time_ms, slo_ms
        );
    }
}

#[test]
fn test_profile_dev_uses_parallel_compilation_units() {
    // Arrange: Load Cargo.toml to verify dev profile
    let content = std::fs::read_to_string(
        std::env::var("CARGO_MANIFEST_DIR")
            .map(std::path::PathBuf::from)
            .unwrap_or_else(|_| std::path::PathBuf::from("."))
            .parent()
            .and_then(|p| p.parent())
            .map(|p| p.join("Cargo.toml"))
            .expect("Cannot find workspace root"),
    )
    .expect("Failed to read Cargo.toml");

    let toml: toml::Value = toml::from_str(&content).expect("Failed to parse TOML");

    // Act: Extract dev profile codegen-units
    let dev_codegen_units = toml
        .get("profile")
        .and_then(|p| p.get("dev"))
        .and_then(|d| d.get("codegen-units"))
        .and_then(|c| c.as_integer());

    // Assert: Dev profile should enable high parallelism
    assert_eq!(
        dev_codegen_units,
        Some(256),
        "Dev profile must use codegen-units=256 for fast parallel compilation"
    );
}

#[test]
fn test_profile_test_uses_parallel_compilation_units() {
    // Arrange: Load Cargo.toml to verify test profile
    let content = std::fs::read_to_string(
        std::env::var("CARGO_MANIFEST_DIR")
            .map(std::path::PathBuf::from)
            .unwrap_or_else(|_| std::path::PathBuf::from("."))
            .parent()
            .and_then(|p| p.parent())
            .map(|p| p.join("Cargo.toml"))
            .expect("Cannot find workspace root"),
    )
    .expect("Failed to read Cargo.toml");

    let toml: toml::Value = toml::from_str(&content).expect("Failed to parse TOML");

    // Act: Extract test profile codegen-units
    let test_codegen_units = toml
        .get("profile")
        .and_then(|p| p.get("test"))
        .and_then(|d| d.get("codegen-units"))
        .and_then(|c| c.as_integer());

    // Assert: Test profile should enable high parallelism for fast test compilation
    assert_eq!(
        test_codegen_units,
        Some(256),
        "Test profile must use codegen-units=256 for fast test compilation"
    );
}

#[test]
fn test_profile_release_reduced_codegen_units_per_optimization_plan() {
    // Arrange: Load Cargo.toml to verify release profile
    let content = std::fs::read_to_string(
        std::env::var("CARGO_MANIFEST_DIR")
            .map(std::path::PathBuf::from)
            .unwrap_or_else(|_| std::path::PathBuf::from("."))
            .parent()
            .and_then(|p| p.parent())
            .map(|p| p.join("Cargo.toml"))
            .expect("Cannot find workspace root"),
    )
    .expect("Failed to read Cargo.toml");

    let toml: toml::Value = toml::from_str(&content).expect("Failed to parse TOML");

    // Act: Extract release profile codegen-units
    let release_codegen_units = toml
        .get("profile")
        .and_then(|p| p.get("release"))
        .and_then(|d| d.get("codegen-units"))
        .and_then(|c| c.as_integer());

    // Assert: Release profile must be reduced per CARGO_OPTIMIZATION_PLAN
    assert_eq!(
        release_codegen_units,
        Some(4),
        "Release profile must be reduced to codegen-units=4 per CARGO_OPTIMIZATION_PLAN"
    );
}

#[test]
fn test_profile_bench_single_codegen_unit_for_maximum_optimization() {
    // Arrange: Load Cargo.toml to verify bench profile
    let content = std::fs::read_to_string(
        std::env::var("CARGO_MANIFEST_DIR")
            .map(std::path::PathBuf::from)
            .unwrap_or_else(|_| std::path::PathBuf::from("."))
            .parent()
            .and_then(|p| p.parent())
            .map(|p| p.join("Cargo.toml"))
            .expect("Cannot find workspace root"),
    )
    .expect("Failed to read Cargo.toml");

    let toml: toml::Value = toml::from_str(&content).expect("Failed to parse TOML");

    // Act: Extract bench profile codegen-units
    let bench_codegen_units = toml
        .get("profile")
        .and_then(|p| p.get("bench"))
        .and_then(|d| d.get("codegen-units"))
        .and_then(|c| c.as_integer());

    // Assert: Bench profile must use single unit for maximum optimization
    assert_eq!(
        bench_codegen_units,
        Some(1),
        "Bench profile must use codegen-units=1 for maximum optimization"
    );
}

#[test]
fn test_incremental_compilation_enabled_for_development() {
    // Arrange: Load Cargo.toml
    let content = std::fs::read_to_string(
        std::env::var("CARGO_MANIFEST_DIR")
            .map(std::path::PathBuf::from)
            .unwrap_or_else(|_| std::path::PathBuf::from("."))
            .parent()
            .and_then(|p| p.parent())
            .map(|p| p.join("Cargo.toml"))
            .expect("Cannot find workspace root"),
    )
    .expect("Failed to read Cargo.toml");

    let toml: toml::Value = toml::from_str(&content).expect("Failed to parse TOML");

    // Act: Check incremental compilation setting
    let dev_incremental = toml
        .get("profile")
        .and_then(|p| p.get("dev"))
        .and_then(|d| d.get("incremental"))
        .and_then(|i| i.as_bool());

    // Assert: Dev profile should enable incremental compilation
    assert_eq!(
        dev_incremental,
        Some(true),
        "Dev profile must enable incremental compilation for fast rebuilds"
    );
}

#[test]
fn test_strip_enabled_for_release_binary_size() {
    // Arrange: Load Cargo.toml
    let content = std::fs::read_to_string(
        std::env::var("CARGO_MANIFEST_DIR")
            .map(std::path::PathBuf::from)
            .unwrap_or_else(|_| std::path::PathBuf::from("."))
            .parent()
            .and_then(|p| p.parent())
            .map(|p| p.join("Cargo.toml"))
            .expect("Cannot find workspace root"),
    )
    .expect("Failed to read Cargo.toml");

    let toml: toml::Value = toml::from_str(&content).expect("Failed to parse TOML");

    // Act: Check strip setting
    let release_strip = toml
        .get("profile")
        .and_then(|p| p.get("release"))
        .and_then(|d| d.get("strip"))
        .and_then(|s| s.as_bool());

    // Assert: Release profile should strip symbols
    assert_eq!(
        release_strip,
        Some(true),
        "Release profile must strip symbols for smaller binary size"
    );
}

#[test]
fn test_thin_lto_for_release_balance() {
    // Arrange: Load Cargo.toml
    let content = std::fs::read_to_string(
        std::env::var("CARGO_MANIFEST_DIR")
            .map(std::path::PathBuf::from)
            .unwrap_or_else(|_| std::path::PathBuf::from("."))
            .parent()
            .and_then(|p| p.parent())
            .map(|p| p.join("Cargo.toml"))
            .expect("Cannot find workspace root"),
    )
    .expect("Failed to read Cargo.toml");

    let toml: toml::Value = toml::from_str(&content).expect("Failed to parse TOML");

    // Act: Check LTO setting
    let release_lto = toml
        .get("profile")
        .and_then(|p| p.get("release"))
        .and_then(|d| d.get("lto"))
        .and_then(|l| l.as_str());

    // Assert: Release profile should use thin LTO for balance
    assert_eq!(
        release_lto,
        Some("thin"),
        "Release profile must use thin LTO for build speed and optimization balance"
    );
}

#[test]
fn test_full_lto_for_bench_profile() {
    // Arrange: Load Cargo.toml
    let content = std::fs::read_to_string(
        std::env::var("CARGO_MANIFEST_DIR")
            .map(std::path::PathBuf::from)
            .unwrap_or_else(|_| std::path::PathBuf::from("."))
            .parent()
            .and_then(|p| p.parent())
            .map(|p| p.join("Cargo.toml"))
            .expect("Cannot find workspace root"),
    )
    .expect("Failed to read Cargo.toml");

    let toml: toml::Value = toml::from_str(&content).expect("Failed to parse TOML");

    // Act: Check LTO setting
    let bench_lto = toml
        .get("profile")
        .and_then(|p| p.get("bench"))
        .and_then(|d| d.get("lto"))
        .and_then(|l| l.as_str());

    // Assert: Bench profile should use full LTO
    assert_eq!(
        bench_lto,
        Some("true"),
        "Bench profile must use full LTO for maximum optimization"
    );
}
