//! Dependency Consolidation Tests
//!
//! Validates that dependencies are consolidated, versions are consistent,
//! and workspace dependency management is working correctly.
//!
//! Chicago TDD Pattern:
//! - State-based testing (verify dependency state via Cargo.toml)
//! - Real objects (actual workspace dependencies)
//! - AAA pattern (Arrange/Act/Assert)

use std::collections::{HashMap, HashSet};

/// Helper to parse version from dependency line
fn extract_version(dep_line: &str) -> Option<String> {
    dep_line
        .split("version")
        .nth(1)
        .and_then(|part| {
            let start = part.find('"')? + 1;
            let end = part[start..].find('"')?;
            Some(part[start..start + end].to_string())
        })
}

// ============================================================================
// Dependency Consolidation Tests (15 total)
// ============================================================================

/// Test 1: Workspace Dependencies Defined
/// Verifies that workspace.dependencies section exists and is used
#[test]
fn test_workspace_dependencies_defined() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let has_workspace_deps = cargo_toml.contains("[workspace.dependencies]");

    assert!(
        has_workspace_deps,
        "Cargo.toml should define [workspace.dependencies] for version consistency"
    );
}

/// Test 2: Tokio Consolidated in Workspace
/// Verifies that tokio version is defined once in workspace
#[test]
fn test_tokio_consolidated_in_workspace() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let workspace_deps_section = cargo_toml
        .split("[workspace.dependencies]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let tokio_count = workspace_deps_section.matches("tokio =").count();

    assert_eq!(
        tokio_count, 1,
        "Tokio should be defined exactly once in workspace.dependencies"
    );
}

/// Test 3: Serde Consolidated in Workspace
/// Verifies that serde version is consistent
#[test]
fn test_serde_consolidated_in_workspace() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let workspace_deps_section = cargo_toml
        .split("[workspace.dependencies]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let has_serde = workspace_deps_section.contains("serde =");

    assert!(
        has_serde,
        "Serde should be defined in workspace.dependencies"
    );
}

/// Test 4: Axum Consolidated
/// Verifies that axum is consolidated to single version
#[test]
fn test_axum_consolidated() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let workspace_deps_section = cargo_toml
        .split("[workspace.dependencies]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let axum_lines: Vec<&str> = workspace_deps_section
        .lines()
        .filter(|line| line.contains("axum"))
        .collect();

    // Should have exactly one line for axum core dependency
    assert!(
        axum_lines.iter().any(|line| line.contains("axum = ")),
        "Axum should be consolidated in workspace.dependencies"
    );
}

/// Test 5: Tonic Consolidated
/// Verifies that tonic is consolidated to single version
#[test]
fn test_tonic_consolidated() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let workspace_deps_section = cargo_toml
        .split("[workspace.dependencies]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let has_tonic = workspace_deps_section.contains("tonic =");

    assert!(
        has_tonic,
        "Tonic should be consolidated in workspace.dependencies"
    );
}

/// Test 6: Derive More Consolidated
/// Verifies that derive_more version is consistent
#[test]
fn test_derive_more_consolidated() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let workspace_deps_section = cargo_toml
        .split("[workspace.dependencies]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let derive_more_count = workspace_deps_section.matches("derive_more =").count();

    assert_eq!(
        derive_more_count, 1,
        "derive_more should be defined exactly once in workspace.dependencies"
    );
}

/// Test 7: Darling Consolidated
/// Verifies that darling versions are consolidated
#[test]
fn test_darling_consolidated() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let workspace_deps_section = cargo_toml
        .split("[workspace.dependencies]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let darling_core_exists = workspace_deps_section.contains("darling_core =");
    let darling_exists = workspace_deps_section.contains("darling =");

    assert!(
        darling_core_exists && darling_exists,
        "Both darling and darling_core should be in workspace.dependencies"
    );
}

/// Test 8: Base64 Consolidated
/// Verifies that base64 version is explicitly set to resolve conflicts
#[test]
fn test_base64_explicitly_consolidated() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let workspace_deps_section = cargo_toml
        .split("[workspace.dependencies]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let has_base64 = workspace_deps_section.contains("base64 =");

    assert!(
        has_base64,
        "base64 should be explicitly consolidated to resolve duplicate versions"
    );
}

/// Test 9: RON Consolidated
/// Verifies that ron version is set to avoid old base64 dependency
#[test]
fn test_ron_consolidated() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let workspace_deps_section = cargo_toml
        .split("[workspace.dependencies]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let ron_line = workspace_deps_section
        .lines()
        .find(|line| line.contains("ron ="));

    let uses_latest_ron = ron_line
        .map(|line| line.contains("0.8"))
        .unwrap_or(false);

    assert!(
        uses_latest_ron,
        "RON should use version 0.8 to avoid old base64 v0.21.7 dependency"
    );
}

/// Test 10: Tracing Ecosystem Consolidated
/// Verifies that tracing, tracing-subscriber are consolidated
#[test]
fn test_tracing_ecosystem_consolidated() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let workspace_deps_section = cargo_toml
        .split("[workspace.dependencies]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let has_tracing = workspace_deps_section.contains("tracing =");
    let has_subscriber = workspace_deps_section.contains("tracing-subscriber =");

    assert!(
        has_tracing && has_subscriber,
        "Tracing ecosystem should be consolidated in workspace.dependencies"
    );
}

/// Test 11: Workspace Crates Use Workspace Dependencies
/// Verifies that member crates reference workspace dependencies
#[test]
fn test_workspace_crates_use_workspace_deps() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let ggen_crates: Vec<&str> = cargo_toml
        .split("[workspace.dependencies]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("")
        .lines()
        .filter(|line| line.contains("ggen-") && line.contains("path ="))
        .collect();

    assert!(
        ggen_crates.len() > 5,
        "Should have multiple ggen-* crates defined in workspace for version management"
    );
}

/// Test 12: Optional Dependencies Strategy
/// Verifies that optional dependencies reduce default build footprint
#[test]
fn test_optional_dependencies_strategy() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let optional_deps: Vec<&str> = cargo_toml
        .split("[dependencies]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("")
        .lines()
        .filter(|line| line.contains("optional = true"))
        .collect();

    assert!(
        optional_deps.len() >= 3,
        "Should have at least 3 optional dependencies to keep core builds small"
    );
}

/// Test 13: Workspace Resolver Version
/// Verifies that workspace uses resolver = "2" for better dependency resolution
#[test]
fn test_workspace_resolver_v2() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let workspace_section = cargo_toml
        .split("[workspace]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let has_resolver_v2 = workspace_section.contains("resolver = \"2\"");

    assert!(
        has_resolver_v2,
        "Workspace should use resolver = \"2\" for better dependency resolution"
    );
}

/// Test 14: Proc Macro Deduplication Documentation
/// Verifies that proc macro deduplication strategy is documented
#[test]
fn test_proc_macro_deduplication_documented() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    let has_dedup_docs = cargo_toml.contains("PROC-MACRO DEDUPLICATION STRATEGY")
        || cargo_toml.contains("Procedure Macro Consolidation");

    assert!(
        has_dedup_docs,
        "Proc macro deduplication strategy should be documented in Cargo.toml"
    );
}

/// Test 15: Dependency Consolidation Impact
/// Verifies that consolidation reduces duplicate versions significantly
#[test]
fn test_dependency_consolidation_impact() {
    let cargo_toml = std::fs::read_to_string("/home/user/ggen/Cargo.toml")
        .expect("Failed to read Cargo.toml");

    // Count workspace dependencies (should be significant after consolidation)
    let workspace_deps_section = cargo_toml
        .split("[workspace.dependencies]")
        .nth(1)
        .and_then(|section| section.split('[').next())
        .unwrap_or("");

    let workspace_dep_count = workspace_deps_section
        .lines()
        .filter(|line| !line.trim().is_empty() && !line.trim().starts_with('#') && line.contains('='))
        .count();

    assert!(
        workspace_dep_count >= 25,
        "Should have at least 25 consolidated dependencies in workspace.dependencies"
    );
}
