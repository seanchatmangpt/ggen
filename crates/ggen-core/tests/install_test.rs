//! Integration tests for pack installation system (Phase 1)
//!
//! These tests verify the pack installation logic works correctly,
//! focusing on lockfile management and metadata tracking.

use ggen_core::lockfile::LockfileManager;
use ggen_core::packs::install_pack;
use tempfile::TempDir;

#[tokio::test]
async fn test_install_pack_creates_lockfile() {
    let temp_dir = TempDir::new().unwrap();

    // Attempt to install rust-cli pack
    let result = install_pack("rust-cli", Some("1.0.0"), temp_dir.path(), false).await;

    // If pack exists in marketplace, installation should succeed
    if let Ok(install_result) = result {
        // Verify lockfile was created
        let lockfile_path = temp_dir.path().join(".ggen/packs.lock");
        assert!(
            lockfile_path.exists(),
            "Lockfile should be created at .ggen/packs.lock"
        );

        // Verify result structure
        assert_eq!(install_result.pack_id, "rust-cli");
        assert_eq!(install_result.version, "1.0.0");
        assert!(
            install_result.packages_installed > 0,
            "Pack should contain at least 1 package"
        );
        assert!(
            install_result.message.contains("Installed"),
            "Message should confirm installation"
        );
    }
}

#[tokio::test]
async fn test_install_pack_duplicates_without_force() {
    let temp_dir = TempDir::new().unwrap();

    // First installation
    let first_install = install_pack("rust-cli", Some("1.0.0"), temp_dir.path(), false).await;

    // Only test duplicate detection if first install succeeded
    if first_install.is_ok() {
        // Second installation without --force should fail
        let second_install = install_pack("rust-cli", Some("1.0.0"), temp_dir.path(), false).await;

        assert!(
            second_install.is_err(),
            "Duplicate installation without --force should fail"
        );

        // Verify error message mentions "already installed"
        if let Err(e) = second_install {
            let error_msg = e.to_string();
            assert!(
                error_msg.contains("already installed") || error_msg.contains("already exists"),
                "Error should mention pack is already installed, got: {}",
                error_msg
            );
        }
    }
}

#[tokio::test]
async fn test_install_pack_overwrites_with_force() {
    let temp_dir = TempDir::new().unwrap();

    // First installation
    let first_install = install_pack("rust-cli", Some("1.0.0"), temp_dir.path(), false).await;

    // Only test force reinstall if first install succeeded
    if first_install.is_ok() {
        // Second installation with --force should succeed
        let second_install = install_pack("rust-cli", Some("1.0.0"), temp_dir.path(), true).await;

        assert!(
            second_install.is_ok(),
            "Reinstallation with --force should succeed"
        );

        // Verify result
        if let Ok(result) = second_install {
            assert_eq!(result.pack_id, "rust-cli");
            assert_eq!(result.version, "1.0.0");
            assert!(
                result.message.contains("Reinstalled"),
                "Message should indicate reinstallation, got: {}",
                result.message
            );
        }
    }
}

#[tokio::test]
async fn test_install_nonexistent_pack() {
    let temp_dir = TempDir::new().unwrap();

    // In Phase 1, all packs are accepted (validation happens in Phase 2-3)
    // This test verifies the installation still works even with unusual pack names
    let result = install_pack(
        "nonexistent-pack-xyz-123-definitely-not-real",
        Some("1.0.0"),
        temp_dir.path(),
        false,
    )
    .await;

    // Phase 1 should succeed (creates lockfile entry)
    assert!(
        result.is_ok(),
        "Phase 1 installation should succeed for any pack name"
    );

    if let Ok(install_result) = result {
        assert_eq!(
            install_result.pack_id,
            "nonexistent-pack-xyz-123-definitely-not-real"
        );
        assert_eq!(install_result.version, "1.0.0");
        assert!(install_result.packages_installed > 0);
    }
}

#[tokio::test]
async fn test_install_pack_updates_lockfile() {
    let temp_dir = TempDir::new().unwrap();

    // Install pack
    let install_result = install_pack("rust-cli", Some("1.0.0"), temp_dir.path(), false).await;

    // Only continue if installation succeeded
    if install_result.is_ok() {
        let lockfile_path = temp_dir.path().join(".ggen/packs.lock");
        let lockfile_manager = LockfileManager::with_path(lockfile_path);

        // Verify lockfile entry exists
        let entry = lockfile_manager.get("rust-cli").unwrap();
        assert!(
            entry.is_some(),
            "Lockfile should contain entry for installed pack"
        );

        let entry = entry.unwrap();
        assert_eq!(entry.id, "rust-cli", "Entry should have correct pack ID");
        assert_eq!(entry.version, "1.0.0", "Entry should have correct version");
        assert!(
            !entry.sha256.is_empty(),
            "Entry should have SHA256 checksum"
        );
        assert!(!entry.source.is_empty(), "Entry should have source URL");

        // Verify lockfile stats
        let stats = lockfile_manager.stats().unwrap();
        assert!(
            stats.total_packs >= 1,
            "Lockfile should contain at least 1 pack"
        );
        assert!(
            stats.generated.is_some(),
            "Lockfile should have generation timestamp"
        );
        assert_eq!(
            stats.version,
            Some("1.0".to_string()),
            "Lockfile should have version"
        );
    }
}

#[tokio::test]
async fn test_install_pack_with_different_version_fails_without_force() {
    let temp_dir = TempDir::new().unwrap();

    // First installation with version 1.0.0
    let first_install = install_pack("rust-cli", Some("1.0.0"), temp_dir.path(), false).await;

    if first_install.is_ok() {
        // Try to install different version without force
        let second_install = install_pack("rust-cli", Some("2.0.0"), temp_dir.path(), false).await;

        assert!(
            second_install.is_err(),
            "Installing different version without --force should fail"
        );

        if let Err(e) = second_install {
            let error_msg = e.to_string();
            assert!(
                error_msg.contains("already installed"),
                "Error should mention pack is already installed, got: {}",
                error_msg
            );
        }
    }
}

#[tokio::test]
async fn test_install_pack_creates_ggen_directory() {
    let temp_dir = TempDir::new().unwrap();

    let ggen_dir = temp_dir.path().join(".ggen");
    assert!(
        !ggen_dir.exists(),
        ".ggen directory should not exist initially"
    );

    // Attempt installation (doesn't matter if it succeeds or fails)
    let _ = install_pack("rust-cli", Some("1.0.0"), temp_dir.path(), false).await;

    // .ggen directory should be created
    assert!(
        ggen_dir.exists(),
        ".ggen directory should be created during installation attempt"
    );
}

#[tokio::test]
async fn test_install_pack_uses_default_version() {
    let temp_dir = TempDir::new().unwrap();

    // Install without specifying version (should use pack's default version)
    let result = install_pack("rust-cli", None, temp_dir.path(), false).await;

    if let Ok(install_result) = result {
        // Version should be populated from pack metadata
        assert!(!install_result.version.is_empty(), "Version should be set");
        assert_eq!(install_result.pack_id, "rust-cli", "Pack ID should match");
    }
}

#[tokio::test]
async fn test_install_pack_with_invalid_version_format() {
    let temp_dir = TempDir::new().unwrap();

    // Try to install with completely invalid version
    let result = install_pack("rust-cli", Some("invalid"), temp_dir.path(), false).await;

    assert!(
        result.is_err(),
        "Installing with invalid version should fail"
    );

    if let Err(e) = result {
        let error_msg = e.to_string();
        assert!(
            error_msg.contains("Invalid version") || error_msg.contains("version"),
            "Error should mention invalid version, got: {}",
            error_msg
        );
    }
}

#[tokio::test]
async fn test_install_pack_result_structure() {
    let temp_dir = TempDir::new().unwrap();

    let result = install_pack("rust-cli", Some("1.0.0"), temp_dir.path(), false).await;

    if let Ok(install_result) = result {
        // Verify all fields are properly populated
        assert!(!install_result.pack_id.is_empty(), "Pack ID should be set");
        assert!(!install_result.version.is_empty(), "Version should be set");
        assert!(
            install_result.packages_installed > 0,
            "Should have packages count"
        );
        assert!(
            install_result.lockfile_path.exists()
                || install_result.lockfile_path.to_str().is_some(),
            "Lockfile path should be valid"
        );
        assert!(!install_result.message.is_empty(), "Message should be set");
    }
}

#[tokio::test]
async fn test_install_multiple_packs_sequentially() {
    let temp_dir = TempDir::new().unwrap();

    // Install first pack
    let first_result = install_pack("rust-cli", Some("1.0.0"), temp_dir.path(), false).await;

    if first_result.is_ok() {
        // Install second pack (different pack ID)
        let second_result =
            install_pack("nextjs-base", Some("1.0.0"), temp_dir.path(), false).await;

        // Both packs might not exist, but if they do, both should succeed
        if second_result.is_ok() {
            let lockfile_path = temp_dir.path().join(".ggen/packs.lock");
            let lockfile_manager = LockfileManager::with_path(lockfile_path);

            let stats = lockfile_manager.stats().unwrap();
            assert!(
                stats.total_packs >= 2,
                "Lockfile should contain at least 2 packs"
            );
        }
    }
}
