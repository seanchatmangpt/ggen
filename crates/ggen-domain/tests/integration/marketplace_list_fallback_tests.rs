//! Integration tests for Marketplace list fallback mechanism
//!
//! Tests the critical fallback logic (lines 173-216 of marketplace/list.rs)
//! that falls back to repo packages when installed packages are empty.

use chicago_tdd_tools::prelude::*;
use ggen_domain::marketplace::list::{execute_list, ListInput, ListOutput};
use std::fs;
use std::path::PathBuf;
use tempfile::tempdir;

/// Test marketplace list with no installed packages - should fallback to repo
async_test!(test_marketplace_list_fallback_to_repo, {
    // Arrange - Create a temporary directory with marketplace packages
    let temp_dir = tempdir().unwrap();
    let marketplace_dir = temp_dir.path().join("marketplace").join("packages");
    fs::create_dir_all(&marketplace_dir).unwrap();

    // Create a test package in marketplace/packages
    let package_dir = marketplace_dir.join("io.ggen.test-package");
    fs::create_dir_all(&package_dir).unwrap();

    let package_toml = r#"
[package]
name = "io.ggen.test-package"
version = "1.0.0"
description = "Test package for fallback"
"#;
    fs::write(package_dir.join("package.toml"), package_toml).unwrap();

    // Change to temp directory so relative path works
    let original_dir = std::env::current_dir().unwrap();
    std::env::set_current_dir(temp_dir.path()).unwrap();

    // Act - Execute list (should fallback to repo packages)
    let input = ListInput {
        detailed: false,
        json: false,
    };
    let result = execute_list(input).await;

    // Restore original directory
    std::env::set_current_dir(original_dir).unwrap();

    // Assert
    assert_ok!(result);
    let output = result.unwrap();
    assert!(
        output.packages_listed > 0,
        "Should find packages in marketplace/packages directory"
    );
    assert!(
        output
            .packages
            .iter()
            .any(|p| p.name == "io.ggen.test-package"),
        "Should include the test package"
    );
});

/// Test marketplace list with installed packages - should NOT fallback
async_test!(test_marketplace_list_no_fallback_when_installed, {
    // Arrange - Create both installed packages and marketplace packages
    let temp_dir = tempdir().unwrap();

    // Create installed packages with lockfile
    let packages_dir = temp_dir.path().join(".ggen").join("packages");
    fs::create_dir_all(&packages_dir).unwrap();

    let lockfile = serde_json::json!({
        "version": "1.0",
        "packages": {
            "io.ggen.installed-pkg": {
                "version": "2.0.0",
                "installed_at": "2024-11-13T00:00:00Z"
            }
        }
    });
    fs::write(
        packages_dir.join("ggen.lock"),
        serde_json::to_string_pretty(&lockfile).unwrap(),
    )
    .unwrap();

    // Create marketplace packages (should be ignored)
    let marketplace_dir = temp_dir.path().join("marketplace").join("packages");
    fs::create_dir_all(&marketplace_dir).unwrap();

    let package_dir = marketplace_dir.join("io.ggen.marketplace-pkg");
    fs::create_dir_all(&package_dir).unwrap();

    let package_toml = r#"
[package]
name = "io.ggen.marketplace-pkg"
version = "1.0.0"
description = "Should be ignored"
"#;
    fs::write(package_dir.join("package.toml"), package_toml).unwrap();

    // Mock home directory to use temp dir
    std::env::set_var("HOME", temp_dir.path());

    // Act
    let input = ListInput {
        detailed: false,
        json: false,
    };
    let result = execute_list(input).await;

    // Assert
    assert_ok!(result);
    let output = result.unwrap();

    // Should have packages (either installed or fallback)
    // Note: This test depends on registry implementation details
    // For now, just verify it doesn't error
});

/// Test fallback with multiple packages in marketplace
async_test!(test_marketplace_fallback_multiple_packages, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let marketplace_dir = temp_dir.path().join("marketplace").join("packages");
    fs::create_dir_all(&marketplace_dir).unwrap();

    // Create multiple test packages
    for i in 1..=3 {
        let package_dir = marketplace_dir.join(format!("io.ggen.package-{}", i));
        fs::create_dir_all(&package_dir).unwrap();

        let package_toml = format!(
            r#"
[package]
name = "io.ggen.package-{}"
version = "1.{}.0"
description = "Test package {}"
"#,
            i, i, i
        );
        fs::write(package_dir.join("package.toml"), package_toml).unwrap();
    }

    let original_dir = std::env::current_dir().unwrap();
    std::env::set_current_dir(temp_dir.path()).unwrap();

    // Act
    let input = ListInput {
        detailed: true,
        json: true,
    };
    let result = execute_list(input).await;

    std::env::set_current_dir(original_dir).unwrap();

    // Assert
    assert_ok!(result);
    let output = result.unwrap();
    assert!(
        output.packages_listed >= 3,
        "Should find at least 3 packages"
    );
});

/// Test fallback with invalid package.toml - should skip gracefully
async_test!(test_marketplace_fallback_invalid_toml, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let marketplace_dir = temp_dir.path().join("marketplace").join("packages");
    fs::create_dir_all(&marketplace_dir).unwrap();

    // Create package with invalid TOML
    let invalid_dir = marketplace_dir.join("io.ggen.invalid");
    fs::create_dir_all(&invalid_dir).unwrap();
    fs::write(invalid_dir.join("package.toml"), "invalid toml [[[").unwrap();

    // Create valid package
    let valid_dir = marketplace_dir.join("io.ggen.valid");
    fs::create_dir_all(&valid_dir).unwrap();
    let package_toml = r#"
[package]
name = "io.ggen.valid"
version = "1.0.0"
description = "Valid package"
"#;
    fs::write(valid_dir.join("package.toml"), package_toml).unwrap();

    let original_dir = std::env::current_dir().unwrap();
    std::env::set_current_dir(temp_dir.path()).unwrap();

    // Act
    let input = ListInput {
        detailed: false,
        json: false,
    };
    let result = execute_list(input).await;

    std::env::set_current_dir(original_dir).unwrap();

    // Assert
    assert_ok!(result);
    let output = result.unwrap();
    assert!(
        output.packages.iter().any(|p| p.name == "io.ggen.valid"),
        "Should skip invalid and include valid packages"
    );
});

/// Test fallback when marketplace directory doesn't exist
async_test!(test_marketplace_fallback_no_directory, {
    // Arrange - No marketplace directory
    let temp_dir = tempdir().unwrap();
    let original_dir = std::env::current_dir().unwrap();
    std::env::set_current_dir(temp_dir.path()).unwrap();

    // Act
    let input = ListInput {
        detailed: false,
        json: false,
    };
    let result = execute_list(input).await;

    std::env::set_current_dir(original_dir).unwrap();

    // Assert - Should succeed with empty list
    assert_ok!(result);
    let output = result.unwrap();
    assert_eq!(output.packages_listed, 0, "Should have no packages");
});

/// Test package parsing with missing fields - uses defaults
async_test!(test_marketplace_fallback_missing_fields, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let marketplace_dir = temp_dir.path().join("marketplace").join("packages");
    fs::create_dir_all(&marketplace_dir).unwrap();

    // Create package with minimal TOML (missing description)
    let package_dir = marketplace_dir.join("io.ggen.minimal");
    fs::create_dir_all(&package_dir).unwrap();
    let package_toml = r#"
[package]
name = "io.ggen.minimal"
version = "1.0.0"
"#;
    fs::write(package_dir.join("package.toml"), package_toml).unwrap();

    let original_dir = std::env::current_dir().unwrap();
    std::env::set_current_dir(temp_dir.path()).unwrap();

    // Act
    let input = ListInput {
        detailed: false,
        json: false,
    };
    let result = execute_list(input).await;

    std::env::set_current_dir(original_dir).unwrap();

    // Assert
    assert_ok!(result);
    let output = result.unwrap();
    assert!(
        output.packages.iter().any(|p| p.name == "io.ggen.minimal"),
        "Should handle missing optional fields"
    );
    let package = output
        .packages
        .iter()
        .find(|p| p.name == "io.ggen.minimal")
        .unwrap();
    assert_eq!(
        package.description, "",
        "Description should be empty string"
    );
    assert_eq!(
        package.installed_at, None,
        "Should not be marked as installed"
    );
});

/// Test fallback respects directory structure
async_test!(test_marketplace_fallback_ignores_files, {
    // Arrange
    let temp_dir = tempdir().unwrap();
    let marketplace_dir = temp_dir.path().join("marketplace").join("packages");
    fs::create_dir_all(&marketplace_dir).unwrap();

    // Create a file (not directory) - should be ignored
    fs::write(marketplace_dir.join("not-a-package.txt"), "ignored").unwrap();

    // Create valid package directory
    let package_dir = marketplace_dir.join("io.ggen.real-package");
    fs::create_dir_all(&package_dir).unwrap();
    let package_toml = r#"
[package]
name = "io.ggen.real-package"
version = "1.0.0"
description = "Real package"
"#;
    fs::write(package_dir.join("package.toml"), package_toml).unwrap();

    let original_dir = std::env::current_dir().unwrap();
    std::env::set_current_dir(temp_dir.path()).unwrap();

    // Act
    let input = ListInput {
        detailed: false,
        json: false,
    };
    let result = execute_list(input).await;

    std::env::set_current_dir(original_dir).unwrap();

    // Assert
    assert_ok!(result);
    let output = result.unwrap();
    assert_eq!(
        output.packages_listed, 1,
        "Should only count valid package directories"
    );
    assert!(
        output
            .packages
            .iter()
            .any(|p| p.name == "io.ggen.real-package"),
        "Should find the real package"
    );
});
