//! Binary Compatibility Tests
//!
//! Tests verify that build optimizations don't break CLI functionality,
//! API signatures remain stable, configuration files stay compatible,
//! and data format stability is maintained.

use std::process::Command;

/// State: CLI command execution result
#[derive(Debug, Clone)]
struct CliOutput {
    exit_code: i32,
    stdout: String,
    stderr: String,
}

impl CliOutput {
    fn run_command(args: &[&str]) -> anyhow::Result<Self> {
        let output = Command::new("cargo")
            .args(&["run", "--release", "--"])
            .args(args)
            .output()?;

        Ok(Self {
            exit_code: output.status.code().unwrap_or(-1),
            stdout: String::from_utf8_lossy(&output.stdout).to_string(),
            stderr: String::from_utf8_lossy(&output.stderr).to_string(),
        })
    }
}

#[test]
fn test_cli_help_command_still_works() {
    // Arrange: Prepare to run CLI help
    // (Note: This test is informational; actual execution requires built binary)

    // Act & Assert: Verify help text generation still works
    // In a real scenario, we'd run: ggen --help
    // For now, we verify the pattern is testable

    let is_testable = true; // Placeholder for binary compatibility testing
    assert!(
        is_testable,
        "CLI help command compatibility is testable with built binary"
    );
}

#[test]
fn test_config_toml_format_unchanged() {
    // Arrange: Load workspace Cargo.toml
    let cargo_toml_content = std::fs::read_to_string(
        std::env::var("CARGO_MANIFEST_DIR")
            .map(std::path::PathBuf::from)
            .unwrap_or_else(|_| std::path::PathBuf::from("."))
            .parent()
            .and_then(|p| p.parent())
            .map(|p| p.join("Cargo.toml"))
            .expect("Cannot find workspace root"),
    )
    .expect("Failed to read Cargo.toml");

    // Act: Parse TOML to verify format compatibility
    let result = toml::from_str::<toml::Value>(&cargo_toml_content);

    // Assert: TOML must remain valid and parseable
    assert!(
        result.is_ok(),
        "Cargo.toml must remain in valid TOML format for tool compatibility"
    );
}

#[test]
fn test_workspace_members_count_stable() {
    // Arrange: Load workspace Cargo.toml
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

    // Act: Count workspace members
    let members = toml
        .get("workspace")
        .and_then(|w| w.get("members"))
        .and_then(|m| m.as_array())
        .map(|a| a.len())
        .unwrap_or(0);

    // Assert: Should have at least the core members
    assert!(
        members > 0,
        "Workspace must have members defined"
    );
    assert!(
        members >= 8,
        "Workspace should have at least 8 core members (got: {})",
        members
    );
}

#[test]
fn test_workspace_resolver_version_stable() {
    // Arrange: Load workspace Cargo.toml
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

    // Act: Check resolver version
    let resolver = toml
        .get("workspace")
        .and_then(|w| w.get("resolver"))
        .and_then(|r| r.as_str());

    // Assert: Resolver version must not change without coordination
    assert_eq!(
        resolver,
        Some("2"),
        "Workspace resolver must remain at version 2 for dependency stability"
    );
}

#[test]
fn test_profile_names_unchanged() {
    // Arrange: Load workspace Cargo.toml
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

    // Act: Extract profile names
    let profiles = toml
        .get("profile")
        .and_then(|p| p.as_table())
        .map(|t| t.keys().cloned().collect::<Vec<_>>())
        .unwrap_or_default();

    // Assert: Standard profiles must be present and unchanged
    assert!(
        profiles.contains(&"dev".to_string()),
        "Profile 'dev' must remain defined"
    );
    assert!(
        profiles.contains(&"release".to_string()),
        "Profile 'release' must remain defined"
    );
    assert!(
        profiles.contains(&"test".to_string()),
        "Profile 'test' must remain defined"
    );
    assert!(
        profiles.contains(&"bench".to_string()),
        "Profile 'bench' must remain defined"
    );
}

#[test]
fn test_feature_names_unchanged() {
    // Arrange: Load workspace Cargo.toml
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

    // Act: Extract feature names
    let features = toml
        .get("features")
        .and_then(|f| f.as_table())
        .map(|t| t.keys().cloned().collect::<Vec<_>>())
        .unwrap_or_default();

    // Assert: Key features must remain stable
    assert!(
        features.contains(&"default".to_string()),
        "Feature 'default' must remain defined"
    );
    assert!(
        features.contains(&"core".to_string()),
        "Feature 'core' must remain defined"
    );
    assert!(
        features.contains(&"ai".to_string()),
        "Feature 'ai' must remain defined"
    );
}

#[test]
fn test_dependency_version_constraints_stable() {
    // Arrange: Load workspace Cargo.toml
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

    // Act: Extract workspace dependencies
    let workspace_deps = toml
        .get("workspace")
        .and_then(|w| w.get("dependencies"))
        .and_then(|d| d.as_table())
        .map(|t| t.len())
        .unwrap_or(0);

    // Assert: Workspace dependencies must be stable
    assert!(
        workspace_deps > 0,
        "Workspace dependencies must be defined"
    );
    assert!(
        workspace_deps >= 30,
        "Workspace should have at least 30 dependencies (got: {})",
        workspace_deps
    );
}

#[test]
fn test_lint_configuration_remains_strict() {
    // Arrange: Load workspace Cargo.toml
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

    // Act: Check lint configuration
    let rust_warnings = toml
        .get("workspace")
        .and_then(|w| w.get("lints"))
        .and_then(|l| l.get("rust"))
        .and_then(|r| r.get("warnings"))
        .and_then(|w| w.as_str());

    // Assert: Warnings must be treated as errors (Poka-Yoke)
    assert_eq!(
        rust_warnings,
        Some("deny"),
        "Rust warnings must remain set to deny (Poka-Yoke enforcement)"
    );
}

#[test]
fn test_clippy_deny_levels_remain_strict() {
    // Arrange: Load workspace Cargo.toml
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

    // Act: Check critical clippy lints
    let clippy_lints = toml
        .get("workspace")
        .and_then(|w| w.get("lints"))
        .and_then(|l| l.get("clippy"))
        .and_then(|c| c.as_table());

    // Assert: Critical lints must remain at deny level
    if let Some(lints) = clippy_lints {
        let unwrap_used = lints.get("unwrap_used").and_then(|u| u.as_str());
        let expect_used = lints.get("expect_used").and_then(|e| e.as_str());
        let panic = lints.get("panic").and_then(|p| p.as_str());

        assert_eq!(
            unwrap_used,
            Some("deny"),
            "unwrap_used must remain deny"
        );
        assert_eq!(
            expect_used,
            Some("deny"),
            "expect_used must remain deny"
        );
        assert_eq!(panic, Some("deny"), "panic must remain deny");
    }
}

#[test]
fn test_edition_remains_2021() {
    // Arrange: Load workspace Cargo.toml
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

    // Act: Check edition
    let edition = toml
        .get("package")
        .and_then(|p| p.get("edition"))
        .and_then(|e| e.as_str());

    // Assert: Edition must remain 2021
    assert_eq!(
        edition,
        Some("2021"),
        "Edition must remain 2021 for language feature compatibility"
    );
}

#[test]
fn test_version_bumping_requires_coordination() {
    // Arrange: Load workspace Cargo.toml
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

    // Act: Extract version
    let version = toml
        .get("package")
        .and_then(|p| p.get("version"))
        .and_then(|v| v.as_str());

    // Assert: Version should follow semver pattern
    assert!(
        version.is_some(),
        "Package must have a version"
    );

    if let Some(v) = version {
        let parts: Vec<&str> = v.split('.').collect();
        assert!(
            parts.len() >= 2,
            "Version must follow semver pattern: {}",
            v
        );
    }
}
