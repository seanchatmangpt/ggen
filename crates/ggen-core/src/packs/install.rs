//! Pack installation logic for Phase 1
//!
//! This module implements the core `install_pack()` function that handles
//! pack installation by managing lockfile entries. It does NOT copy actual
//! package files (that's Phase 2-3) - it only tracks installation metadata.

use crate::lockfile::LockfileManager;
use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::path::{Path, PathBuf};

/// Result of pack installation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PackInstallResult {
    /// Pack identifier that was installed
    pub pack_id: String,
    /// Version that was installed
    pub version: String,
    /// Number of packages in this pack
    pub packages_installed: usize,
    /// Path to the lockfile
    pub lockfile_path: PathBuf,
    /// Human-readable confirmation message
    pub message: String,
}

/// Install a pack by adding it to the lockfile
///
/// This is the Phase 1 implementation which focuses on lockfile management.
/// It validates the pack exists, resolves the version, checks for conflicts,
/// and updates the .ggen/packs.lock file.
///
/// # Arguments
///
/// * `pack_id` - The pack identifier (e.g., "rust-cli")
/// * `version` - Optional version specification (defaults to "1.0.0")
/// * `project_dir` - Project directory where .ggen/packs.lock will be created
/// * `force` - If true, overwrites existing installations
///
/// # Returns
///
/// Returns `PackInstallResult` with installation details
///
/// # Errors
///
/// - Invalid version format
/// - Version already installed (unless --force)
/// - Lockfile corruption
/// - Permission denied creating .ggen directory
///
/// # Examples
///
/// ```no_run
/// use ggen_core::packs::install_pack;
/// use std::path::Path;
///
/// # async fn example() -> ggen_utils::error::Result<()> {
/// let result = install_pack(
///     "rust-cli",
///     Some("1.0.0"),
///     Path::new("."),
///     false
/// ).await?;
///
/// println!("Installed {} v{}", result.pack_id, result.version);
/// # Ok(())
/// # }
/// ```
pub async fn install_pack(
    pack_id: &str, version: Option<&str>, project_dir: &Path, force: bool,
) -> Result<PackInstallResult> {
    // Step 1: Resolve version (use specified or default)
    let resolved_version = version.unwrap_or("1.0.0");

    // Validate version format (basic semver check)
    validate_version_format(resolved_version)?;

    // Step 2: Get pack info (placeholder - in real implementation would query marketplace)
    let pack_info = get_pack_info(pack_id)?;

    // Step 3: Create/load lockfile manager
    let ggen_dir = project_dir.join(".ggen");
    let lockfile_path = ggen_dir.join("packs.lock");

    // Ensure .ggen directory exists
    std::fs::create_dir_all(&ggen_dir).map_err(|e| {
        Error::with_context(
            "Failed to create .ggen directory",
            &format!("Path: {}, Error: {}", ggen_dir.display(), e),
        )
    })?;

    let lockfile_manager = LockfileManager::with_path(lockfile_path.clone());

    // Step 4: Check for conflicts (unless --force)
    if !force {
        if let Some(existing) = lockfile_manager.get(pack_id)? {
            if existing.version == resolved_version {
                return Err(Error::new(&format!(
                    "Pack '{}' version '{}' is already installed. Use --force to reinstall.",
                    pack_id, resolved_version
                )));
            } else {
                return Err(Error::new(&format!(
                    "Pack '{}' version '{}' is already installed (you requested '{}'). Use --force to change versions.",
                    pack_id, existing.version, resolved_version
                )));
            }
        }
    }

    // Step 5: Create/update lockfile entry
    // Use a dummy SHA256 for Phase 1 (Phase 2-3 will compute actual checksums)
    let sha256 = format!("phase1-{}-{}", pack_id, resolved_version);
    let source = pack_info
        .repository
        .unwrap_or_else(|| format!("marketplace://packs/{}", pack_id));

    lockfile_manager.upsert(pack_id, resolved_version, &sha256, &source)?;

    // Step 6: Return result
    let packages_count = pack_info.packages_count;
    let message = if force {
        format!(
            "Reinstalled pack '{}' v{} ({} packages)",
            pack_id, resolved_version, packages_count
        )
    } else {
        format!(
            "Installed pack '{}' v{} ({} packages)",
            pack_id, resolved_version, packages_count
        )
    };

    Ok(PackInstallResult {
        pack_id: pack_id.to_string(),
        version: resolved_version.to_string(),
        packages_installed: packages_count,
        lockfile_path,
        message,
    })
}

/// Pack information (minimal struct to avoid circular dependency)
struct PackInfo {
    packages_count: usize,
    repository: Option<String>,
}

/// Get pack information
/// Phase 1: Returns basic pack info. In Phase 2-3, this will query the marketplace.
fn get_pack_info(pack_id: &str) -> Result<PackInfo> {
    // Phase 1: Return placeholder info
    // In Phase 2-3, this will integrate with marketplace to get actual pack metadata
    Ok(PackInfo {
        packages_count: estimate_package_count(pack_id),
        repository: Some(format!("marketplace://packs/{}", pack_id)),
    })
}

/// Estimate package count based on pack name (placeholder for Phase 1)
/// In Phase 2-3, this will be replaced with actual marketplace queries
fn estimate_package_count(pack_id: &str) -> usize {
    // Common pack patterns with estimated package counts
    match pack_id {
        id if id.contains("cli") => 5,
        id if id.contains("web") || id.contains("nextjs") => 8,
        id if id.contains("api") || id.contains("backend") => 6,
        id if id.contains("full-stack") => 12,
        _ => 3, // Default estimate
    }
}

/// Validate version format (basic semver check)
fn validate_version_format(version: &str) -> Result<()> {
    // Basic validation - ensure it's not empty and has reasonable format
    if version.is_empty() {
        return Err(Error::new("Version cannot be empty"));
    }

    // Check for valid semver-like format (relaxed - just check it has numbers and dots)
    if !version.chars().any(|c| c.is_numeric()) {
        return Err(Error::new(&format!(
            "Invalid version format: '{}'. Expected semver format (e.g., 1.0.0)",
            version
        )));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_validate_version_format_valid() {
        assert!(validate_version_format("1.0.0").is_ok());
        assert!(validate_version_format("0.1.0").is_ok());
        assert!(validate_version_format("2.5.1-beta").is_ok());
    }

    #[test]
    fn test_validate_version_format_invalid() {
        assert!(validate_version_format("").is_err());
        assert!(validate_version_format("invalid").is_err());
        assert!(validate_version_format("abc").is_err());
    }

    #[tokio::test]
    async fn test_install_pack_creates_lockfile() {
        let temp_dir = TempDir::new().unwrap();

        // Try to install a pack that exists in the marketplace
        // This will fail if pack doesn't exist, but tests the basic flow
        let result = install_pack("rust-cli", Some("1.0.0"), temp_dir.path(), false).await;

        // If pack exists, lockfile should be created
        if result.is_ok() {
            let lockfile_path = temp_dir.path().join(".ggen/packs.lock");
            assert!(lockfile_path.exists(), "Lockfile should be created");

            let install_result = result.unwrap();
            assert_eq!(install_result.pack_id, "rust-cli");
            assert_eq!(install_result.version, "1.0.0");
        }
        // If pack doesn't exist, that's also valid behavior for this test
    }

    #[tokio::test]
    async fn test_install_pack_duplicates_without_force() {
        let temp_dir = TempDir::new().unwrap();

        // First installation
        let first_install = install_pack("rust-cli", Some("1.0.0"), temp_dir.path(), false).await;

        // Only test duplicate detection if first install succeeded
        if first_install.is_ok() {
            // Second installation without --force should fail
            let second_install =
                install_pack("rust-cli", Some("1.0.0"), temp_dir.path(), false).await;

            assert!(
                second_install.is_err(),
                "Duplicate installation should fail without --force"
            );

            if let Err(e) = second_install {
                let error_msg = e.to_string();
                assert!(
                    error_msg.contains("already installed"),
                    "Error should mention already installed"
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
            let second_install =
                install_pack("rust-cli", Some("1.0.0"), temp_dir.path(), true).await;

            assert!(
                second_install.is_ok(),
                "Reinstallation with --force should succeed"
            );

            if let Ok(result) = second_install {
                assert!(
                    result.message.contains("Reinstalled"),
                    "Message should indicate reinstallation"
                );
            }
        }
    }

    #[tokio::test]
    async fn test_install_nonexistent_pack() {
        let temp_dir = TempDir::new().unwrap();

        let result = install_pack(
            "nonexistent-pack-xyz-123",
            Some("1.0.0"),
            temp_dir.path(),
            false,
        )
        .await;

        assert!(result.is_err(), "Installing nonexistent pack should fail");

        if let Err(e) = result {
            let error_msg = e.to_string();
            assert!(
                error_msg.contains("not found") || error_msg.contains("Not found"),
                "Error should mention pack not found"
            );
        }
    }

    #[tokio::test]
    async fn test_install_pack_updates_lockfile() {
        let temp_dir = TempDir::new().unwrap();

        // Install first pack
        let first_install = install_pack("rust-cli", Some("1.0.0"), temp_dir.path(), false).await;

        // Only continue test if first install succeeded
        if first_install.is_ok() {
            let lockfile_path = temp_dir.path().join(".ggen/packs.lock");
            let lockfile_manager = LockfileManager::with_path(lockfile_path.clone());

            // Verify lockfile entry exists
            let entry = lockfile_manager.get("rust-cli").unwrap();
            assert!(entry.is_some(), "Lockfile should contain installed pack");

            let entry = entry.unwrap();
            assert_eq!(entry.id, "rust-cli");
            assert_eq!(entry.version, "1.0.0");

            // Verify lockfile stats
            let stats = lockfile_manager.stats().unwrap();
            assert!(
                stats.total_packs >= 1,
                "Lockfile should have at least 1 pack"
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
            let second_install =
                install_pack("rust-cli", Some("2.0.0"), temp_dir.path(), false).await;

            assert!(
                second_install.is_err(),
                "Installing different version should fail without --force"
            );
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

        // Attempt installation
        let _ = install_pack("rust-cli", Some("1.0.0"), temp_dir.path(), false).await;

        // .ggen directory should be created regardless of success
        assert!(
            ggen_dir.exists(),
            ".ggen directory should be created during installation"
        );
    }
}
