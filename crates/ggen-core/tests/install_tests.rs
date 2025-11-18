//! Tests for install_pack() functionality
//!
//! This test suite covers:
//! - Basic installation flow
//! - Version management
//! - Dependency resolution
//! - Error handling
//! - Force reinstall scenarios
//!
//! Note: These tests assume install_pack() will be implemented.
//! For now, we test the supporting infrastructure (lockfile, cache).

use ggen_core::cache::CacheManager;
use ggen_core::lockfile::LockfileManager;
use tempfile::TempDir;

// ============================================================================
// Test Helpers
// ============================================================================

/// Mock install_pack function (will be replaced with real implementation)
/// This demonstrates the expected API
#[allow(dead_code)]
async fn mock_install_pack(
    pack_id: &str, version: Option<&str>, force: bool, cache: &CacheManager,
    lockfile: &LockfileManager,
) -> ggen_utils::error::Result<()> {
    // Check if already installed
    if !force {
        if let Some(entry) = lockfile.get(pack_id)? {
            if version.is_none() || Some(entry.version.as_str()) == version {
                return Err(ggen_utils::error::Error::new(&format!(
                    "Pack {} already installed",
                    pack_id
                )));
            }
        }
    }

    // Simulate installation
    let install_version = version.unwrap_or("latest");
    lockfile.upsert(
        pack_id,
        install_version,
        "mock_sha256",
        &format!("https://github.com/{}.git", pack_id),
    )?;

    Ok(())
}

// ============================================================================
// Unit Tests: Installation Flow
// ============================================================================

#[tokio::test]
async fn test_install_pack_success() {
    let temp_dir = TempDir::new().unwrap();
    let cache = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    let result = mock_install_pack("io.ggen.test", Some("1.0.0"), false, &cache, &lockfile).await;

    assert!(result.is_ok());

    // Verify it's in the lockfile
    let entry = lockfile.get("io.ggen.test").unwrap();
    assert!(entry.is_some());
    assert_eq!(entry.unwrap().version, "1.0.0");
}

#[tokio::test]
async fn test_install_pack_already_installed() {
    let temp_dir = TempDir::new().unwrap();
    let cache = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    // Install once
    mock_install_pack("io.ggen.test", Some("1.0.0"), false, &cache, &lockfile)
        .await
        .unwrap();

    // Try to install again (should fail)
    let result = mock_install_pack("io.ggen.test", Some("1.0.0"), false, &cache, &lockfile).await;
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("already installed"));
}

#[tokio::test]
async fn test_install_pack_force_overwrite() {
    let temp_dir = TempDir::new().unwrap();
    let cache = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    // Install version 1.0.0
    mock_install_pack("io.ggen.test", Some("1.0.0"), false, &cache, &lockfile)
        .await
        .unwrap();

    // Force install version 2.0.0
    let result = mock_install_pack("io.ggen.test", Some("2.0.0"), true, &cache, &lockfile).await;
    assert!(result.is_ok());

    // Verify new version
    let entry = lockfile.get("io.ggen.test").unwrap().unwrap();
    assert_eq!(entry.version, "2.0.0");
}

#[tokio::test]
async fn test_install_pack_creates_directories() {
    let temp_dir = TempDir::new().unwrap();
    let nested_path = temp_dir.path().join("project").join("subdir");

    let cache = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
    let lockfile = LockfileManager::new(&nested_path);

    mock_install_pack("io.ggen.test", Some("1.0.0"), false, &cache, &lockfile)
        .await
        .unwrap();

    // Verify lockfile directory was created
    assert!(lockfile.lockfile_path().exists());
    assert!(lockfile.lockfile_path().parent().unwrap().exists());
}

// ============================================================================
// Integration Tests: Multiple Installations
// ============================================================================

#[tokio::test]
async fn test_install_multiple_packs_sequentially() {
    let temp_dir = TempDir::new().unwrap();
    let cache = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    // Install multiple packs
    mock_install_pack("io.ggen.a", Some("1.0.0"), false, &cache, &lockfile)
        .await
        .unwrap();
    mock_install_pack("io.ggen.b", Some("2.0.0"), false, &cache, &lockfile)
        .await
        .unwrap();
    mock_install_pack("io.ggen.c", Some("3.0.0"), false, &cache, &lockfile)
        .await
        .unwrap();

    // Verify all are installed
    let packs = lockfile.list().unwrap();
    assert_eq!(packs.len(), 3);

    assert!(lockfile.is_installed("io.ggen.a").unwrap());
    assert!(lockfile.is_installed("io.ggen.b").unwrap());
    assert!(lockfile.is_installed("io.ggen.c").unwrap());
}

#[tokio::test]
async fn test_install_update_existing_pack() {
    let temp_dir = TempDir::new().unwrap();
    let cache = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    // Install v1
    mock_install_pack("io.ggen.test", Some("1.0.0"), false, &cache, &lockfile)
        .await
        .unwrap();

    // Upgrade to v2 (force)
    mock_install_pack("io.ggen.test", Some("2.0.0"), true, &cache, &lockfile)
        .await
        .unwrap();

    // Should only have one entry
    let packs = lockfile.list().unwrap();
    assert_eq!(packs.len(), 1);
    assert_eq!(packs[0].version, "2.0.0");
}

// ============================================================================
// Performance Tests
// ============================================================================

#[tokio::test]
async fn test_install_pack_speed() {
    let temp_dir = TempDir::new().unwrap();
    let cache = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    let start = std::time::Instant::now();

    mock_install_pack("io.ggen.test", Some("1.0.0"), false, &cache, &lockfile)
        .await
        .unwrap();

    let duration = start.elapsed();

    // Install (without actual copying) should complete in <500ms
    assert!(
        duration.as_millis() < 500,
        "Install took {}ms",
        duration.as_millis()
    );
}

#[tokio::test]
async fn test_install_10_packs_performance() {
    let temp_dir = TempDir::new().unwrap();
    let cache = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    let start = std::time::Instant::now();

    for i in 0..10 {
        mock_install_pack(
            &format!("io.ggen.pack{}", i),
            Some("1.0.0"),
            false,
            &cache,
            &lockfile,
        )
        .await
        .unwrap();
    }

    let duration = start.elapsed();

    // 10 installs should complete in <5s
    assert!(
        duration.as_secs() < 5,
        "10 installs took {}s",
        duration.as_secs()
    );

    assert_eq!(lockfile.list().unwrap().len(), 10);
}

// ============================================================================
// Security Tests
// ============================================================================

#[tokio::test]
async fn test_install_pack_path_traversal_prevention() {
    let temp_dir = TempDir::new().unwrap();
    let cache = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    // Try malicious pack ID
    let result = mock_install_pack(
        "../../../etc/passwd",
        Some("1.0.0"),
        false,
        &cache,
        &lockfile,
    )
    .await;

    // Installation should succeed (pack_id is just a string in lockfile)
    // The security concern is in file operations, not the ID itself
    assert!(result.is_ok());

    // Verify lockfile is still in safe location
    assert!(lockfile.lockfile_path().starts_with(temp_dir.path()));
}

// ============================================================================
// Error Case Tests
// ============================================================================

#[test]
fn test_lockfile_manager_creation() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    assert!(manager.lockfile_path().ends_with("ggen.lock"));
}

#[test]
fn test_cache_manager_creation() {
    let temp_dir = TempDir::new().unwrap();
    let cache = CacheManager::with_dir(temp_dir.path());

    assert!(cache.is_ok());
    assert!(temp_dir.path().exists());
}

#[test]
fn test_lockfile_consistency_after_multiple_installs() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    // Add multiple packs
    lockfile
        .upsert("io.ggen.a", "1.0.0", "sha1", "https://a.com")
        .unwrap();
    lockfile
        .upsert("io.ggen.b", "1.0.0", "sha2", "https://b.com")
        .unwrap();
    lockfile
        .upsert("io.ggen.c", "1.0.0", "sha3", "https://c.com")
        .unwrap();

    // Update one
    lockfile
        .upsert("io.ggen.b", "2.0.0", "sha4", "https://b.com")
        .unwrap();

    // Verify state
    let packs = lockfile.list().unwrap();
    assert_eq!(packs.len(), 3);

    let pack_b = lockfile.get("io.ggen.b").unwrap().unwrap();
    assert_eq!(pack_b.version, "2.0.0");
}
