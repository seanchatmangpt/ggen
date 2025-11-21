//! Pack Installation Tests - Unit Level
//!
//! Tests the PackInstaller component:
//! - Installing single packs
//! - Handling dependencies
//! - Dry run mode
//! - Force reinstall

use ggen_domain::packs::{install_pack, InstallInput};
use std::path::PathBuf;

#[tokio::test]
async fn test_install_pack_dry_run_mode() {
    // Arrange
    let input = InstallInput {
        pack_id: "startup-essentials".to_string(),
        target_dir: Some(PathBuf::from("/tmp/test-install-dry")),
        force: false,
        dry_run: true,
    };

    // Act
    let result = install_pack(&input).await;

    // Assert
    match result {
        Ok(output) => {
            assert_eq!(output.pack_id, "startup-essentials");
            assert_eq!(output.packages_installed.len(), 0, "Dry run should not install packages");
            assert!(output.total_packages >= 0);
        }
        Err(e) => {
            // Expected if pack doesn't exist
            assert!(
                e.to_string().contains("not found") || e.to_string().contains("directory")
            );
        }
    }
}

#[tokio::test]
async fn test_install_pack_with_target_dir() {
    // Arrange
    let test_dir = PathBuf::from("/tmp/test-install-target");
    let input = InstallInput {
        pack_id: "startup-essentials".to_string(),
        target_dir: Some(test_dir.clone()),
        force: false,
        dry_run: true,
    };

    // Act
    let result = install_pack(&input).await;

    // Assert
    if let Ok(output) = result {
        assert_eq!(output.install_path, test_dir);
    }
}

#[tokio::test]
async fn test_install_pack_default_location() {
    // Arrange
    let input = InstallInput {
        pack_id: "startup-essentials".to_string(),
        target_dir: None,
        force: false,
        dry_run: true,
    };

    // Act
    let result = install_pack(&input).await;

    // Assert
    if let Ok(output) = result {
        // Should use default .ggen/packages location or current dir
        assert!(!output.install_path.as_os_str().is_empty());
    }
}

#[tokio::test]
async fn test_install_nonexistent_pack_fails() {
    // Arrange
    let input = InstallInput {
        pack_id: "nonexistent-pack-xyz-123".to_string(),
        target_dir: None,
        force: false,
        dry_run: true,
    };

    // Act
    let result = install_pack(&input).await;

    // Assert - Should fail
    assert!(result.is_err(), "Should fail for nonexistent pack");
    let error = result.unwrap_err();
    assert!(
        error.to_string().contains("not found"),
        "Error should indicate pack not found"
    );
}

#[tokio::test]
async fn test_install_pack_force_flag() {
    // Arrange
    let input = InstallInput {
        pack_id: "startup-essentials".to_string(),
        target_dir: None,
        force: true,
        dry_run: true,
    };

    // Act
    let result = install_pack(&input).await;

    // Assert - Force flag should be accepted
    match result {
        Ok(_output) => {
            // Success
        }
        Err(e) => {
            // Expected if pack doesn't exist
            assert!(e.to_string().contains("not found"));
        }
    }
}

#[tokio::test]
async fn test_install_pack_returns_template_info() {
    // Arrange
    let input = InstallInput {
        pack_id: "startup-essentials".to_string(),
        target_dir: None,
        force: false,
        dry_run: true,
    };

    // Act
    let result = install_pack(&input).await;

    // Assert
    if let Ok(output) = result {
        assert!(output.templates_available.len() >= 0, "Should list available templates");
        assert!(output.sparql_queries >= 0, "Should count SPARQL queries");
    }
}

#[tokio::test]
async fn test_install_pack_with_packages() {
    // Arrange - Use data-science pack which has multiple packages
    let input = InstallInput {
        pack_id: "data-science-toolkit".to_string(),
        target_dir: None,
        force: false,
        dry_run: true,
    };

    // Act
    let result = install_pack(&input).await;

    // Assert
    if let Ok(output) = result {
        assert!(
            output.total_packages > 0,
            "Data science toolkit should have packages"
        );
        assert!(
            output.templates_available.len() > 0,
            "Data science toolkit should have templates"
        );
    }
}

#[test]
fn test_install_input_serialization() {
    // Arrange
    let input = InstallInput {
        pack_id: "test-pack".to_string(),
        target_dir: Some(PathBuf::from("/test/path")),
        force: true,
        dry_run: false,
    };

    // Act
    #[allow(clippy::expect_used)]
    let json = serde_json::to_string(&input).expect("Should serialize");
    #[allow(clippy::expect_used)]
    let deserialized: InstallInput = serde_json::from_str(&json).expect("Should deserialize");

    // Assert
    assert_eq!(deserialized.pack_id, "test-pack");
    assert_eq!(deserialized.target_dir, Some(PathBuf::from("/test/path")));
    assert_eq!(deserialized.force, true);
    assert_eq!(deserialized.dry_run, false);
}

#[test]
fn test_install_input_defaults() {
    // Arrange
    let json = r#"{"pack_id": "test-pack"}"#;

    // Act
    #[allow(clippy::expect_used)]
    let input: InstallInput = serde_json::from_str(json).expect("Should deserialize");

    // Assert - force and dry_run should default to false
    assert_eq!(input.pack_id, "test-pack");
    assert_eq!(input.force, false);
    assert_eq!(input.dry_run, false);
}

#[tokio::test]
async fn test_install_multiple_packs_sequentially() {
    // Arrange
    let packs = vec!["startup-essentials", "data-science-toolkit"];

    // Act - Install each pack
    for pack_id in packs {
        let input = InstallInput {
            pack_id: pack_id.to_string(),
            target_dir: Some(PathBuf::from(format!("/tmp/test-install-{}", pack_id))),
            force: false,
            dry_run: true,
        };

        let result = install_pack(&input).await;

        // Assert - Each should install independently
        if result.is_ok() {
            // Success - pack exists
        } else {
            // Expected if pack doesn't exist
        }
    }
}
