//! Marketplace Validation Integration Tests
//!
//! These tests verify the marketplace validation system works end-to-end.

use ggen_domain::marketplace::{validate_all_packages, validate_package, PackageValidation};
use std::path::PathBuf;
use tempfile::TempDir;

/// Test validation of a complete package
#[test]
fn test_validate_complete_package() {
    let temp_dir = TempDir::new().unwrap();
    let package_path = temp_dir.path().to_path_buf();

    // Create required files
    std::fs::create_dir_all(package_path.join("src")).unwrap();
    std::fs::write(
        package_path.join("package.toml"),
        r#"
[package]
name = "test-package"
version = "1.0.0"
description = "Test package for validation"
"#,
    )
    .unwrap();
    std::fs::write(
        package_path.join("README.md"),
        "Test package documentation with enough content to pass validation requirements".repeat(2),
    )
    .unwrap();
    std::fs::write(package_path.join("LICENSE-MIT"), "MIT License").unwrap();
    std::fs::write(package_path.join("src/main.rs"), "fn main() {}").unwrap();

    // Create quality files
    std::fs::create_dir_all(package_path.join("rdf")).unwrap();
    std::fs::write(
        package_path.join("rdf/ontology.ttl"),
        "ontology content\n".repeat(250),
    )
    .unwrap();
    std::fs::create_dir_all(package_path.join("examples")).unwrap();
    std::fs::write(package_path.join("examples/test.rs"), "// example").unwrap();
    std::fs::create_dir_all(package_path.join("tests")).unwrap();
    std::fs::write(package_path.join("tests/test.rs"), "#[test] fn test() {}").unwrap();

    // Validate package
    let validation = validate_package(&package_path).unwrap();

    // Verify validation results
    assert_eq!(validation.package_name, "test-package");
    assert!(
        validation.score >= 95.0,
        "Score should be >= 95%, got {}",
        validation.score
    );
    assert!(
        validation.production_ready,
        "Package should be production ready"
    );
    assert!(validation.errors.is_empty(), "Should have no errors");
}

/// Test validation of incomplete package
#[test]
fn test_validate_incomplete_package() {
    let temp_dir = TempDir::new().unwrap();
    let package_path = temp_dir.path().to_path_buf();

    // Create only package.toml (incomplete)
    std::fs::write(
        package_path.join("package.toml"),
        r#"
[package]
name = "incomplete"
version = "1.0.0"
description = "Incomplete package"
"#,
    )
    .unwrap();

    // Validate package
    let validation = validate_package(&package_path).unwrap();

    // Verify validation results
    assert_eq!(validation.package_name, "incomplete");
    assert!(
        validation.score < 95.0,
        "Score should be < 95%, got {}",
        validation.score
    );
    assert!(
        !validation.production_ready,
        "Package should not be production ready"
    );
    assert!(!validation.errors.is_empty(), "Should have errors");
}

/// Test validation of all packages in directory
#[test]
fn test_validate_all_packages() {
    let temp_dir = TempDir::new().unwrap();
    let packages_dir = temp_dir.path().to_path_buf();

    // Create two packages
    for i in 1..=2 {
        let package_path = packages_dir.join(format!("package-{}", i));
        std::fs::create_dir_all(&package_path).unwrap();
        std::fs::write(
            package_path.join("package.toml"),
            &format!(
                r#"
[package]
name = "package-{}"
version = "1.0.0"
description = "Test package {}"
"#,
                i, i
            ),
        )
        .unwrap();
        std::fs::write(
            package_path.join("README.md"),
            "Test documentation".repeat(10),
        )
        .unwrap();
        std::fs::write(package_path.join("LICENSE-MIT"), "MIT").unwrap();
        std::fs::create_dir_all(package_path.join("src")).unwrap();
        std::fs::write(package_path.join("src/main.rs"), "fn main() {}").unwrap();
    }

    // Validate all packages
    let validations = validate_all_packages(&packages_dir).unwrap();

    // Verify results
    assert_eq!(validations.len(), 2, "Should validate 2 packages");
    for validation in &validations {
        assert!(!validation.package_name.is_empty());
        assert!(validation.score >= 0.0 && validation.score <= 100.0);
    }
}

/// Test validation scoring calculation
#[test]
fn test_validation_scoring() {
    let temp_dir = TempDir::new().unwrap();
    let package_path = temp_dir.path().to_path_buf();

    // Create package with all required files
    std::fs::create_dir_all(package_path.join("src")).unwrap();
    std::fs::write(
        package_path.join("package.toml"),
        r#"
[package]
name = "scored-package"
version = "1.0.0"
description = "Package for scoring test"
"#,
    )
    .unwrap();
    std::fs::write(package_path.join("README.md"), "Documentation".repeat(10)).unwrap();
    std::fs::write(package_path.join("LICENSE-MIT"), "MIT").unwrap();
    std::fs::write(package_path.join("src/main.rs"), "fn main() {}").unwrap();

    // Add quality files to boost score
    std::fs::create_dir_all(package_path.join("rdf")).unwrap();
    std::fs::write(
        package_path.join("rdf/ontology.ttl"),
        "ontology\n".repeat(250),
    )
    .unwrap();
    std::fs::create_dir_all(package_path.join("examples")).unwrap();
    std::fs::write(package_path.join("examples/test.rs"), "// example").unwrap();

    let validation = validate_package(&package_path).unwrap();

    // Verify scoring
    assert!(
        validation.score >= 80.0,
        "Score should be >= 80% with quality files"
    );
    assert_eq!(
        validation.required_checks.len(),
        4,
        "Should have 4 required checks"
    );
    assert!(
        validation.quality_checks.len() > 0,
        "Should have quality checks"
    );
}
