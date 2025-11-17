//! Integration tests for Phase 2 marketplace features
//!
//! Tests the complete marketplace pipeline:
//! - Receipt emission from package validation
//! - Artifact generation (JSON + Markdown)
//! - Quality autopilot improvement suggestions
//! - Bundle management
//! - End-to-end validation workflow

#![allow(clippy::unwrap_used)]

use crate::marketplace::{
    artifact_generator::{generate_packages_markdown, generate_registry_index},
    bundles::BundleRegistry,
    quality_autopilot::generate_improvement_plan,
    receipt_emitter::{emit_receipt_for_package, generate_validation_report},
};
use tempfile::TempDir;

// ============================================================================
// Phase 3: Integration Tests for Receipt Emission
// ============================================================================

#[test]
fn test_receipt_emission_single_package() {
    // Arrange: Create a test package structure
    let temp_dir = TempDir::new().unwrap();
    let marketplace_root = temp_dir.path();
    let packages_dir = marketplace_root.join("marketplace").join("packages");
    let test_pkg_dir = packages_dir.join("test-package");
    std::fs::create_dir_all(&test_pkg_dir).unwrap();

    // Create minimal package files
    create_test_package(&test_pkg_dir);

    // Act: Emit receipt for the package
    let result = emit_receipt_for_package(&test_pkg_dir, "test-package", "1.0.0", marketplace_root);

    // Assert: Receipt should be created successfully
    assert!(result.is_ok(), "Receipt emission should succeed");
    let receipt_path = result.unwrap();
    assert!(
        receipt_path.exists(),
        "Receipt file should exist at: {:?}",
        receipt_path
    );

    // Verify receipt is valid JSON
    let content = std::fs::read_to_string(&receipt_path).unwrap();
    let receipt: serde_json::Value =
        serde_json::from_str(&content).expect("Receipt should be valid JSON");
    assert_eq!(receipt["package_id"], "test-package");
    assert_eq!(receipt["version"], "1.0.0");
    assert!(receipt["overall_score"].is_number());
}

#[test]
fn test_receipt_has_guard_results() {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let marketplace_root = temp_dir.path();
    let packages_dir = marketplace_root.join("marketplace").join("packages");
    let test_pkg_dir = packages_dir.join("validated-package");
    std::fs::create_dir_all(&test_pkg_dir).unwrap();
    create_test_package(&test_pkg_dir);

    // Act
    let result = emit_receipt_for_package(
        &test_pkg_dir,
        "validated-package",
        "2.0.0",
        marketplace_root,
    );
    assert!(result.is_ok());

    // Assert: Receipt should contain guard results
    let receipt_path = result.unwrap();
    let content = std::fs::read_to_string(&receipt_path).unwrap();
    let receipt: serde_json::Value = serde_json::from_str(&content).unwrap();

    assert!(
        receipt["guard_results"].is_array(),
        "Should have guard results"
    );
    let guard_results = receipt["guard_results"].as_array().unwrap();
    assert!(
        !guard_results.is_empty(),
        "Should have at least one guard result"
    );

    // Verify guard result structure
    for result in guard_results {
        assert!(result["guard_type"].is_string());
        assert!(result["guard_name"].is_string());
        assert!(result["passed"].is_boolean());
        assert!(result["severity"].is_string());
    }
}

#[test]
fn test_receipt_checksum_immutability() {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let marketplace_root = temp_dir.path();
    let packages_dir = marketplace_root.join("marketplace").join("packages");
    let test_pkg_dir = packages_dir.join("immutable-package");
    std::fs::create_dir_all(&test_pkg_dir).unwrap();
    create_test_package(&test_pkg_dir);

    // Act
    let receipt_result = emit_receipt_for_package(
        &test_pkg_dir,
        "immutable-package",
        "1.0.0",
        marketplace_root,
    );
    assert!(receipt_result.is_ok());

    // Assert: Receipt should have checksum
    let receipt_path = receipt_result.unwrap();
    let content = std::fs::read_to_string(&receipt_path).unwrap();
    let receipt: serde_json::Value = serde_json::from_str(&content).unwrap();

    let checksum = receipt["checksum"].as_str();
    assert!(checksum.is_some(), "Receipt should have checksum");
    assert!(
        checksum.unwrap().len() == 64,
        "Checksum should be SHA256 (64 hex chars)"
    );
}

// ============================================================================
// Phase 3: Integration Tests for Artifact Generation
// ============================================================================

#[test]
fn test_artifact_generation_json_registry() {
    // Arrange: Create marketplace with receipts
    let temp_dir = TempDir::new().unwrap();
    let marketplace_root = temp_dir.path();
    setup_test_marketplace_with_receipts(marketplace_root);

    // Act: Generate JSON registry
    let result = generate_registry_index(marketplace_root);

    // Assert
    assert!(result.is_ok(), "JSON generation should succeed");
    let registry = result.unwrap();

    assert!(registry["version"].is_string());
    assert_eq!(registry["version"], "1.0.0");
    assert!(registry["packages"].is_array());
    assert!(registry["categories"].is_object());
    assert!(registry["updated_at"].is_string());
}

#[test]
fn test_artifact_generation_markdown_packages() {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let marketplace_root = temp_dir.path();
    setup_test_marketplace_with_receipts(marketplace_root);

    // Act: Generate Markdown
    let result = generate_packages_markdown(marketplace_root);

    // Assert
    assert!(result.is_ok(), "Markdown generation should succeed");
    let markdown = result.unwrap();

    assert!(markdown.contains("# Complete Package Directory"));
    assert!(markdown.contains("Quick Navigation"));
    assert!(markdown.contains("---"));
    assert!(markdown.contains("##")); // Section headers
}

#[test]
fn test_artifacts_from_real_receipts() {
    // Arrange: Create real receipts then generate artifacts
    let temp_dir = TempDir::new().unwrap();
    let marketplace_root = temp_dir.path();

    // Create multiple packages with receipts
    for i in 1..=3 {
        let pkg_name = format!("package-{}", i);
        let packages_dir = marketplace_root.join("marketplace").join("packages");
        let pkg_dir = packages_dir.join(&pkg_name);
        std::fs::create_dir_all(&pkg_dir).unwrap();
        create_test_package(&pkg_dir);

        let _ = emit_receipt_for_package(&pkg_dir, &pkg_name, "1.0.0", marketplace_root);
    }

    // Act
    let json_result = generate_registry_index(marketplace_root);
    let md_result = generate_packages_markdown(marketplace_root);

    // Assert
    assert!(json_result.is_ok());
    assert!(md_result.is_ok());

    let json = json_result.unwrap();
    let packages = json["packages"].as_array().unwrap();
    assert_eq!(packages.len(), 3, "Should have 3 packages");

    let md = md_result.unwrap();
    assert!(md.contains("package-1"));
    assert!(md.contains("package-2"));
    assert!(md.contains("package-3"));
}

// ============================================================================
// Phase 3: Integration Tests for Quality Autopilot
// ============================================================================

#[test]
fn test_improvement_plan_generation() {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let marketplace_root = temp_dir.path();
    let packages_dir = marketplace_root.join("marketplace").join("packages");
    let test_pkg_dir = packages_dir.join("low-quality-pkg");
    std::fs::create_dir_all(&test_pkg_dir).unwrap();
    create_test_package(&test_pkg_dir);

    // Emit receipt
    let _ = emit_receipt_for_package(&test_pkg_dir, "low-quality-pkg", "1.0.0", marketplace_root);

    // Act: Generate improvement plan
    let plan_result = generate_improvement_plan("low-quality-pkg", marketplace_root);

    // Assert
    assert!(plan_result.is_ok(), "Improvement plan should generate");
    let plan = plan_result.unwrap();

    assert_eq!(plan.package_id, "low-quality-pkg");
    assert!(plan.current_score >= 0.0 && plan.current_score <= 100.0);
    assert_eq!(plan.target_score, 95.0); // Production ready threshold
    assert!(plan.estimated_effort_hours >= 0.0);
    // Projected score should be at least equal to current (may have no suggestions)
    assert!(plan.projected_new_score >= plan.current_score - 0.001); // Allow for float rounding
}

#[test]
fn test_improvement_suggestions_with_efforts() {
    // Arrange
    let temp_dir = TempDir::new().unwrap();
    let marketplace_root = temp_dir.path();
    let packages_dir = marketplace_root.join("marketplace").join("packages");
    let test_pkg_dir = packages_dir.join("improvement-test");
    std::fs::create_dir_all(&test_pkg_dir).unwrap();
    create_minimal_package(&test_pkg_dir); // Missing features

    let _ = emit_receipt_for_package(&test_pkg_dir, "improvement-test", "1.0.0", marketplace_root);

    // Act
    let plan = generate_improvement_plan("improvement-test", marketplace_root).unwrap();

    // Assert: Should have suggestions with valid effort levels
    assert!(
        !plan.suggestions.is_empty(),
        "Should have improvement suggestions"
    );

    for suggestion in &plan.suggestions {
        assert!(!suggestion.guard_name.is_empty());
        assert!(["low", "medium", "high"].contains(&suggestion.effort_level.as_str()));
        assert!(suggestion.potential_score_gain > 0.0);
    }
}

// ============================================================================
// Phase 3: Integration Tests for Bundle Management
// ============================================================================

#[test]
fn test_bundle_registry_initialization() {
    // Act
    let bundles = BundleRegistry::list_bundles();

    // Assert
    assert!(!bundles.is_empty(), "Should have sector bundles");

    // Verify known bundles exist
    let bundle_ids: Vec<String> = bundles.iter().map(|b| b.id.clone()).collect();
    assert!(bundle_ids.iter().any(|id| id.contains("academic")));
    assert!(bundle_ids.iter().any(|id| id.contains("enterprise")));
    assert!(bundle_ids.iter().any(|id| id.contains("data")));
    assert!(bundle_ids.iter().any(|id| id.contains("healthcare")));
    assert!(bundle_ids.iter().any(|id| id.contains("fintech")));
}

#[test]
fn test_bundle_info_retrieval() {
    // Act
    let bundle = BundleRegistry::get_bundle("sector-academic-papers");

    // Assert
    assert!(bundle.is_some(), "Should find academic bundle");
    let bundle = bundle.unwrap();
    assert_eq!(bundle.id, "sector-academic-papers");
    assert!(!bundle.description.is_empty());
    assert!(bundle.minimum_score >= 75.0 && bundle.minimum_score <= 100.0);
}

#[test]
fn test_bundle_packages_and_features() {
    // Act
    let bundle = BundleRegistry::get_bundle("sector-enterprise-saas").unwrap();

    // Assert
    assert!(!bundle.packages.is_empty(), "Bundle should have packages");
    assert!(!bundle.features.is_empty(), "Bundle should have features");

    for feature in &bundle.features {
        assert!(!feature.is_empty());
    }
}

// ============================================================================
// Phase 3: Integration Tests for Validation Report
// ============================================================================

#[test]
fn test_validation_report_generation() {
    // Arrange: Create marketplace with multiple packages
    let temp_dir = TempDir::new().unwrap();
    let marketplace_root = temp_dir.path();

    for i in 1..=5 {
        let pkg_name = format!("report-test-{}", i);
        let packages_dir = marketplace_root.join("marketplace").join("packages");
        let pkg_dir = packages_dir.join(&pkg_name);
        std::fs::create_dir_all(&pkg_dir).unwrap();
        create_test_package(&pkg_dir);

        let _ = emit_receipt_for_package(&pkg_dir, &pkg_name, "1.0.0", marketplace_root);
    }

    // Act
    let report = generate_validation_report(marketplace_root).unwrap();

    // Assert
    assert_eq!(report.total_packages, 5);
    assert!(report.production_ready_count <= 5);
    assert!(report.average_score >= 0.0 && report.average_score <= 100.0);
    assert!(report.median_score >= 0.0 && report.median_score <= 100.0);
    assert_eq!(
        report.score_95_plus + report.score_80_94 + report.score_below_80,
        5,
        "Score buckets should sum to total packages"
    );
}

// ============================================================================
// Phase 3: End-to-End Workflow Tests
// ============================================================================

#[test]
fn test_complete_marketplace_pipeline() {
    // Arrange: Setup marketplace
    let temp_dir = TempDir::new().unwrap();
    let marketplace_root = temp_dir.path();

    // Create 3 packages
    let packages = vec!["pkg-a", "pkg-b", "pkg-c"];
    for pkg in &packages {
        let pkg_dir = marketplace_root
            .join("marketplace")
            .join("packages")
            .join(pkg);
        std::fs::create_dir_all(&pkg_dir).unwrap();
        create_test_package(&pkg_dir);
    }

    // Step 1: Emit receipts
    for pkg in &packages {
        let pkg_dir = marketplace_root
            .join("marketplace")
            .join("packages")
            .join(pkg);
        let result = emit_receipt_for_package(&pkg_dir, pkg, "1.0.0", marketplace_root);
        assert!(
            result.is_ok(),
            "Receipt emission should succeed for {}",
            pkg
        );
    }

    // Step 2: Generate artifacts
    let json_result = generate_registry_index(marketplace_root);
    let md_result = generate_packages_markdown(marketplace_root);
    assert!(json_result.is_ok());
    assert!(md_result.is_ok());

    // Step 3: Generate validation report
    let report = generate_validation_report(marketplace_root).unwrap();
    assert_eq!(report.total_packages, 3);

    // Step 4: Generate improvement plans for each
    for pkg in &packages {
        let plan = generate_improvement_plan(pkg, marketplace_root);
        assert!(plan.is_ok(), "Improvement plan should generate for {}", pkg);
    }

    // Assert: All steps completed successfully
    let json = json_result.unwrap();
    assert!(json["packages"].is_array());
    let md = md_result.unwrap();
    assert!(md.contains("# Complete Package Directory"));
}

// ============================================================================
// Helper Functions
// ============================================================================

fn create_test_package(pkg_dir: &std::path::Path) {
    // Create package.toml
    let package_toml = r#"[package]
id = "test-package"
version = "1.0.0"
description = "Test package for marketplace integration"
"#;
    std::fs::write(pkg_dir.join("package.toml"), package_toml).unwrap();

    // Create README.md
    let readme = r#"# Test Package

This is a test package for marketplace integration testing.

## Features
- Feature 1
- Feature 2

## Usage
Basic usage example.
"#;
    std::fs::write(pkg_dir.join("README.md"), readme).unwrap();

    // Create LICENSE
    let license = "MIT License\n\nCopyright (c) 2025 Test Contributors";
    std::fs::write(pkg_dir.join("LICENSE"), license).unwrap();

    // Create src directory with main.rs
    let src_dir = pkg_dir.join("src");
    std::fs::create_dir_all(&src_dir).unwrap();
    let main_rs = r#"fn main() {
    println!("Hello from test package");
}
"#;
    std::fs::write(src_dir.join("main.rs"), main_rs).unwrap();

    // Create tests directory
    let tests_dir = pkg_dir.join("tests");
    std::fs::create_dir_all(&tests_dir).unwrap();
    let test_file = r#"#[test]
fn test_basic() {
    assert!(true);
}
"#;
    std::fs::write(tests_dir.join("integration_test.rs"), test_file).unwrap();
}

fn create_minimal_package(pkg_dir: &std::path::Path) {
    // Minimal package - only package.toml
    let package_toml = r#"[package]
version = "1.0.0"
"#;
    std::fs::write(pkg_dir.join("package.toml"), package_toml).unwrap();
}

fn setup_test_marketplace_with_receipts(marketplace_root: &std::path::Path) {
    // Create test packages and emit receipts
    for i in 1..=3 {
        let pkg_name = format!("artifact-test-{}", i);
        let pkg_dir = marketplace_root
            .join("marketplace")
            .join("packages")
            .join(&pkg_name);
        std::fs::create_dir_all(&pkg_dir).unwrap();
        create_test_package(&pkg_dir);

        let _ = emit_receipt_for_package(&pkg_dir, &pkg_name, "1.0.0", marketplace_root);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_helper_functions() {
        let temp_dir = TempDir::new().unwrap();
        create_test_package(temp_dir.path());

        assert!(temp_dir.path().join("package.toml").exists());
        assert!(temp_dir.path().join("README.md").exists());
        assert!(temp_dir.path().join("LICENSE").exists());
        assert!(temp_dir.path().join("src").exists());
        assert!(temp_dir.path().join("tests").exists());
    }
}
