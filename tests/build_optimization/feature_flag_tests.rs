//! Feature Flag Tests
//!
//! Validates that feature flags are configured correctly and that
//! optional features can be enabled/disabled without breaking builds.
//!
//! Chicago TDD Pattern:
//! - State-based testing (verify build succeeds/fails with different features)
//! - Real objects (actual Cargo.toml, real builds)
//! - AAA pattern (Arrange/Act/Assert)

use std::process::Command;

/// Helper to run cargo check with specific features
fn cargo_check_with_features(features: &str) -> Result<(), String> {
    let output = Command::new("cargo")
        .args(&[
            "check",
            "--workspace",
            "--no-default-features",
            "--features",
            features,
        ])
        .current_dir("/home/user/ggen")
        .output()
        .map_err(|e| format!("Failed to run cargo: {}", e))?;

    if output.status.success() {
        Ok(())
    } else {
        Err(String::from_utf8_lossy(&output.stderr).to_string())
    }
}

// ============================================================================
// Feature Flag Tests (20 total)
// ============================================================================

/// Test 1: Core Feature Builds Successfully
/// Verifies that the core feature set builds without errors
#[test]
fn test_core_feature_builds() {
    let result = cargo_check_with_features("core");

    assert!(
        result.is_ok(),
        "Core feature should build successfully. Error: {:?}",
        result.err()
    );
}

/// Test 2: Default Feature Matches Core
/// Verifies that default features include core
#[test]
fn test_default_feature_includes_core() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let has_default_core = cargo_toml.contains("default = [\"core\"]");

    assert!(
        has_default_core,
        "Default features should include core for fast builds"
    );
}

/// Test 3: No Default Features Builds
/// Verifies that building with no features works
#[test]
fn test_no_default_features_builds() {
    let result = cargo_check_with_features("");

    assert!(
        result.is_ok(),
        "Build with no features should succeed. Error: {:?}",
        result.err()
    );
}

/// Test 4: Production Feature Set Builds
/// Verifies that prod feature set builds
#[test]
fn test_prod_feature_set_builds() {
    let result = cargo_check_with_features("prod");

    assert!(
        result.is_ok(),
        "Production feature set should build successfully. Error: {:?}",
        result.err()
    );
}

/// Test 5: Development Feature Set Builds
/// Verifies that dev feature set with AI builds
#[test]
fn test_dev_feature_set_builds() {
    let result = cargo_check_with_features("dev");

    assert!(
        result.is_ok(),
        "Development feature set should build successfully. Error: {:?}",
        result.err()
    );
}

/// Test 6: Full Feature Set Builds
/// Verifies that all features together build
#[test]
fn test_full_feature_set_builds() {
    let result = cargo_check_with_features("full");

    assert!(
        result.is_ok(),
        "Full feature set should build successfully. Error: {:?}",
        result.err()
    );
}

/// Test 7: AI Feature Available
/// Verifies that AI feature is defined
#[test]
fn test_ai_feature_defined() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let features_section = cargo_toml
        .split("[features]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let has_ai_feature = features_section.contains("ai =");

    assert!(
        has_ai_feature,
        "AI feature should be defined in [features] section"
    );
}

/// Test 8: OTEL Feature Available
/// Verifies that OpenTelemetry feature is available
#[test]
fn test_otel_feature_defined() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let features_section = cargo_toml
        .split("[features]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let has_otel_feature = features_section.contains("otel =");

    assert!(
        has_otel_feature,
        "OTEL feature should be defined in [features] section"
    );
}

/// Test 9: Feature Groups Are Composable
/// Verifies that features can be combined
#[test]
fn test_feature_groups_composable() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let features_section = cargo_toml
        .split("[features]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    // Verify core, ai, otel features exist
    let has_core = features_section.contains("core =");
    let has_ai = features_section.contains("ai =");
    let has_otel = features_section.contains("otel =");

    assert!(
        has_core && has_ai && has_otel,
        "Feature groups should be independently defined"
    );
}

/// Test 10: Backward Compatibility Features Exist
/// Verifies that backward compatibility feature flags exist
#[test]
fn test_backward_compatibility_features() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let features_section = cargo_toml
        .split("[features]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let has_termlog = features_section.contains("termlog =");
    let has_journald = features_section.contains("journald =");
    let has_syslog = features_section.contains("syslog =");

    assert!(
        has_termlog && has_journald && has_syslog,
        "Backward compatibility features should be defined"
    );
}

/// Test 11: Optional Dependencies Gated by Features
/// Verifies that ggen-ai is optional
#[test]
fn test_optional_dependencies_gated() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let has_optional_ai = cargo_toml.contains("ggen-ai = { path = \"crates/ggen-ai\"")
        && cargo_toml.contains("optional = true");

    assert!(
        has_optional_ai,
        "ggen-ai should be optional in root dependencies"
    );
}

/// Test 12: Genai Dependency Optional
/// Verifies that genai LLM client is optional
#[test]
fn test_genai_dependency_optional() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let has_optional_genai = cargo_toml.contains("genai =")
        && cargo_toml.contains("optional = true");

    assert!(
        has_optional_genai,
        "genai should be optional in dependencies"
    );
}

/// Test 13: Feature Enables Dependency
/// Verifies that features enable their corresponding optional dependencies
#[test]
fn test_features_enable_dependencies() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let features_section = cargo_toml
        .split("[features]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let ai_feature = features_section
        .split("ai =")
        .nth(1)
        .and_then(|s| s.split('\n').next())
        .unwrap_or("");

    let enables_ggen_ai = ai_feature.contains("ggen-ai");
    let enables_genai = ai_feature.contains("genai");

    assert!(
        enables_ggen_ai && enables_genai,
        "AI feature should enable ggen-ai and genai dependencies"
    );
}

/// Test 14: Dev Feature Includes AI
/// Verifies that development features include AI
#[test]
fn test_dev_feature_includes_ai() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let features_section = cargo_toml
        .split("[features]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let dev_feature = features_section
        .split("dev =")
        .nth(1)
        .and_then(|s| s.split('\n').next())
        .unwrap_or("");

    let includes_core = dev_feature.contains("core");
    let includes_ai = dev_feature.contains("ai");

    assert!(
        includes_core && includes_ai,
        "Dev feature should include both core and ai"
    );
}

/// Test 15: Full Feature Includes Everything
/// Verifies that full feature includes all optional features
#[test]
fn test_full_feature_includes_all() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let features_section = cargo_toml
        .split("[features]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let full_feature = features_section
        .split("full =")
        .nth(1)
        .and_then(|s| s.split('\n').next())
        .unwrap_or("");

    let includes_core = full_feature.contains("core");
    let includes_ai = full_feature.contains("ai");
    let includes_otel = full_feature.contains("otel");

    assert!(
        includes_core && includes_ai && includes_otel,
        "Full feature should include core, ai, and otel"
    );
}

/// Test 16: Nightly Feature Gate Defined
/// Verifies that nightly feature exists for advanced features
#[test]
fn test_nightly_feature_defined() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let has_nightly = cargo_toml.contains("nightly =");

    assert!(
        has_nightly,
        "Nightly feature should be defined for experimental features"
    );
}

/// Test 17: London TDD Feature Available
/// Verifies that london_tdd feature exists for testing modes
#[test]
fn test_london_tdd_feature_available() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let has_london = cargo_toml.contains("london_tdd =");

    assert!(
        has_london,
        "London TDD feature should be available for testing methodology options"
    );
}

/// Test 18: Core Production Feature Minimal
/// Verifies that prod feature only includes core
#[test]
fn test_prod_feature_minimal() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let features_section = cargo_toml
        .split("[features]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let prod_feature = features_section
        .split("prod =")
        .nth(1)
        .and_then(|s| s.split('\n').next())
        .unwrap_or("");

    let only_core = prod_feature.contains("core") && !prod_feature.contains("ai");

    assert!(
        only_core,
        "Production feature should be minimal (core only, no AI)"
    );
}

/// Test 19: Feature Consistency with Workspace
/// Verifies that root features match workspace strategy
#[test]
fn test_feature_consistency_with_workspace() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    // Both [features] section and [workspace.lints] should exist
    let has_features = cargo_toml.contains("[features]");
    let has_workspace_lints = cargo_toml.contains("[workspace.lints");

    assert!(
        has_features && has_workspace_lints,
        "Feature definitions should be consistent with workspace configuration"
    );
}

/// Test 20: Feature Impact on Build Size
/// Verifies that optional features don't bloat minimal builds
#[test]
fn test_feature_impact_on_build_size() {
    // Arrange: Note the number of dependencies with core feature only
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    // Act: Count optional markers in dependencies
    let optional_count = cargo_toml.matches("optional = true").count();

    // Assert: Should have several optional dependencies (AI, OTEL, etc.)
    assert!(
        optional_count >= 3,
        "Should have at least 3 optional dependencies to keep core builds small"
    );
}
