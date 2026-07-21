//! Pack Installation Tests - Unit Level
//!
//! Tests the PackInstaller component:
//! - Installing single packs
//! - Handling dependencies
//! - Dry run mode
//! - Force reinstall

// Re-pointed from `ggen_core::domain::packs::{install_pack, InstallInput}`
// (T053, specs/014-ggen-core-replacement). T024 deliberately renamed these
// during the marketplace merge (`ggen-core`'s free-function install path vs.
// `ggen-marketplace`'s own signed-registry `Installer::install_pack` method
// are genuinely different install paths, not unifiable without redesign --
// see T024's tasks.md notes): `install_pack` -> `install_pack_by_id`,
// `InstallInput` -> `InstallByIdInput`. `InstallByIdOutput`'s field set is a
// superset of what this test needs (pack_id, packages_installed,
// total_packages, install_path, templates_available, sparql_queries all
// present, confirmed via crates/ggen-marketplace/src/marketplace/install.rs).
//
// KNOWN GAP (pre-existing, not a migration regression): `InstallByIdInput`
// has no `#[derive(Serialize, Deserialize)]` -- confirmed neither did the
// original `ggen_core::domain::packs::install::InstallInput`
// (crates/ggen-core/src/domain/packs/install.rs:13, no derive at all) -- so
// `test_install_input_serialization`/`test_install_input_defaults` below
// were already broken pre-migration (this file was orphaned, never
// compiled). Adding the derive is a one-line ggen-marketplace source change,
// out of this task's file scope (root package only).
use ggen_marketplace::marketplace::install::{install_pack_by_id, InstallByIdInput};
use std::path::PathBuf;

#[tokio::test]
async fn test_install_pack_dry_run_mode() {
    // Arrange
    let input = InstallByIdInput {
        pack_id: "startup-essentials".to_string(),
        target_dir: Some(PathBuf::from("/tmp/test-install-dry")),
        force: false,
        dry_run: true,
    };

    // Act
    let result = install_pack_by_id(&input).await;

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
    let input = InstallByIdInput {
        pack_id: "startup-essentials".to_string(),
        target_dir: Some(test_dir.clone()),
        force: false,
        dry_run: true,
    };

    // Act
    let result = install_pack_by_id(&input).await;

    // Assert
    if let Ok(output) = result {
        assert_eq!(output.install_path, test_dir);
    }
}

#[tokio::test]
async fn test_install_pack_default_location() {
    // Arrange
    let input = InstallByIdInput {
        pack_id: "startup-essentials".to_string(),
        target_dir: None,
        force: false,
        dry_run: true,
    };

    // Act
    let result = install_pack_by_id(&input).await;

    // Assert
    if let Ok(output) = result {
        // Should use default .ggen/packages location or current dir
        assert!(!output.install_path.as_os_str().is_empty());
    }
}

#[tokio::test]
async fn test_install_nonexistent_pack_fails() {
    // Arrange
    let input = InstallByIdInput {
        pack_id: "nonexistent-pack-xyz-123".to_string(),
        target_dir: None,
        force: false,
        dry_run: true,
    };

    // Act
    let result = install_pack_by_id(&input).await;

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
    let input = InstallByIdInput {
        pack_id: "startup-essentials".to_string(),
        target_dir: None,
        force: true,
        dry_run: true,
    };

    // Act
    let result = install_pack_by_id(&input).await;

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
    let input = InstallByIdInput {
        pack_id: "startup-essentials".to_string(),
        target_dir: None,
        force: false,
        dry_run: true,
    };

    // Act
    let result = install_pack_by_id(&input).await;

    // Assert
    if let Ok(output) = result {
        assert!(output.templates_available.len() >= 0, "Should list available templates");
        assert!(output.sparql_queries >= 0, "Should count SPARQL queries");
    }
}

#[tokio::test]
async fn test_install_pack_with_packages() {
    // Arrange - Use data-science pack which has multiple packages
    let input = InstallByIdInput {
        pack_id: "data-science-toolkit".to_string(),
        target_dir: None,
        force: false,
        dry_run: true,
    };

    // Act
    let result = install_pack_by_id(&input).await;

    // Assert
    if let Ok(output) = result {
        assert!(
            output.total_packages > 0,
            "Data science toolkit should have packages"
        );
        assert!(
            output.templates_available.is_empty() == false,
            "Data science toolkit should have templates"
        );
    }
}

#[test]
fn test_install_input_serialization() {
    // Arrange
    let input = InstallByIdInput {
        pack_id: "test-pack".to_string(),
        target_dir: Some(PathBuf::from("/test/path")),
        force: true,
        dry_run: false,
    };

    // Act
    let json = serde_json::to_string(&input).expect("Should serialize");
    let deserialized: InstallByIdInput = serde_json::from_str(&json).expect("Should deserialize");

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
    let input: InstallByIdInput = serde_json::from_str(json).expect("Should deserialize");

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
        let input = InstallByIdInput {
            pack_id: pack_id.to_string(),
            target_dir: Some(PathBuf::from(format!("/tmp/test-install-{}", pack_id))),
            force: false,
            dry_run: true,
        };

        let result = install_pack_by_id(&input).await;

        // Assert - both packs exist in this repo's marketplace/packs catalog,
        // and dry_run installs must succeed independently.
        assert!(
            result.is_ok(),
            "dry-run install of known pack {pack_id} must succeed: {result:?}"
        );
    }
}
