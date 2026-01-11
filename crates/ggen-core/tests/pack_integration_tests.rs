//! Full integration tests for ggen Pack Installation System (Phase 1)
//!
//! This test suite covers end-to-end workflows:
//! - Complete installation workflow
//! - Lockfile + Cache integration
//! - Dependency resolution
//! - Upgrade/downgrade scenarios
//! - Error recovery

use ggen_core::cache::CacheManager;
use ggen_core::lockfile::LockfileManager;
use tempfile::TempDir;

// ============================================================================
// Integration Tests: Full Workflow
// ============================================================================

#[tokio::test]
async fn test_full_install_workflow() {
    let temp_dir = TempDir::new().unwrap();
    let project_dir = temp_dir.path().join("my-project");
    std::fs::create_dir_all(&project_dir).unwrap();

    let cache = CacheManager::with_dir(temp_dir.path().join("cache")).unwrap();
    let lockfile = LockfileManager::new(&project_dir);

    // Step 1: Verify clean slate
    assert!(lockfile.load().unwrap().is_none());

    // Step 2: Install a pack
    lockfile
        .upsert(
            "io.ggen.rust.cli",
            "1.0.0",
            "abc123",
            "https://github.com/example/pack.git",
        )
        .unwrap();

    // Step 3: Verify installation
    assert!(lockfile.is_installed("io.ggen.rust.cli").unwrap());
    let entry = lockfile.get("io.ggen.rust.cli").unwrap().unwrap();
    assert_eq!(entry.version, "1.0.0");

    // Step 4: Verify lockfile was created
    assert!(lockfile.lockfile_path().exists());

    // Step 5: Verify cache directory exists
    assert!(cache.cache_dir().exists());
}

#[tokio::test]
async fn test_install_with_dependencies() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    // Install main pack
    lockfile
        .upsert(
            "io.ggen.rust.web",
            "2.0.0",
            "abc123",
            "https://github.com/example/web.git",
        )
        .unwrap();

    // Manually add dependencies (in real implementation, this would be automatic)
    lockfile
        .upsert(
            "io.ggen.macros.std",
            "0.1.0",
            "def456",
            "https://github.com/example/macros.git",
        )
        .unwrap();

    lockfile
        .upsert(
            "io.ggen.templates.base",
            "1.2.0",
            "ghi789",
            "https://github.com/example/templates.git",
        )
        .unwrap();

    // Verify all are installed
    let packs = lockfile.list().unwrap();
    assert_eq!(packs.len(), 3);

    assert!(lockfile.is_installed("io.ggen.rust.web").unwrap());
    assert!(lockfile.is_installed("io.ggen.macros.std").unwrap());
    assert!(lockfile.is_installed("io.ggen.templates.base").unwrap());
}

#[tokio::test]
async fn test_upgrade_pack_version() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    // Install v1.0.0
    lockfile
        .upsert(
            "io.ggen.test",
            "1.0.0",
            "sha_v1",
            "https://example.com/test.git",
        )
        .unwrap();

    let stats_before = lockfile.stats().unwrap();
    assert_eq!(stats_before.total_packs, 1);

    // Upgrade to v2.0.0
    lockfile
        .upsert(
            "io.ggen.test",
            "2.0.0",
            "sha_v2",
            "https://example.com/test.git",
        )
        .unwrap();

    // Should still only have 1 pack (updated)
    let stats_after = lockfile.stats().unwrap();
    assert_eq!(stats_after.total_packs, 1);

    let entry = lockfile.get("io.ggen.test").unwrap().unwrap();
    assert_eq!(entry.version, "2.0.0");
    assert_eq!(entry.sha256, "sha_v2");
}

#[tokio::test]
async fn test_downgrade_pack_version() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    // Install v2.0.0
    lockfile
        .upsert(
            "io.ggen.test",
            "2.0.0",
            "sha_v2",
            "https://example.com/test.git",
        )
        .unwrap();

    // Downgrade to v1.0.0
    lockfile
        .upsert(
            "io.ggen.test",
            "1.0.0",
            "sha_v1",
            "https://example.com/test.git",
        )
        .unwrap();

    let entry = lockfile.get("io.ggen.test").unwrap().unwrap();
    assert_eq!(entry.version, "1.0.0");
}

#[tokio::test]
async fn test_uninstall_pack() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    // Install
    lockfile
        .upsert("io.ggen.test", "1.0.0", "sha", "https://example.com")
        .unwrap();
    assert!(lockfile.is_installed("io.ggen.test").unwrap());

    // Uninstall
    let removed = lockfile.remove("io.ggen.test").unwrap();
    assert!(removed);
    assert!(!lockfile.is_installed("io.ggen.test").unwrap());

    // Verify lockfile still exists but is empty
    assert!(lockfile.lockfile_path().exists());
    let stats = lockfile.stats().unwrap();
    assert_eq!(stats.total_packs, 0);
}

// ============================================================================
// Integration Tests: Lockfile Persistence
// ============================================================================

#[test]
fn test_lockfile_persists_across_sessions() {
    let temp_dir = TempDir::new().unwrap();

    // Session 1: Create and save
    {
        let lockfile = LockfileManager::new(temp_dir.path());
        lockfile
            .upsert("io.ggen.a", "1.0.0", "sha1", "https://a.com")
            .unwrap();
        lockfile
            .upsert("io.ggen.b", "2.0.0", "sha2", "https://b.com")
            .unwrap();
    }

    // Session 2: Load and verify
    {
        let lockfile = LockfileManager::new(temp_dir.path());
        let packs = lockfile.list().unwrap();
        assert_eq!(packs.len(), 2);

        assert!(lockfile.is_installed("io.ggen.a").unwrap());
        assert!(lockfile.is_installed("io.ggen.b").unwrap());
    }
}

#[test]
fn test_lockfile_timestamp_tracking() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    // Create lockfile
    lockfile
        .upsert("io.ggen.test", "1.0.0", "sha", "https://example.com")
        .unwrap();

    let stats1 = lockfile.stats().unwrap();
    let time1 = stats1.generated.unwrap();

    // Wait a bit
    std::thread::sleep(std::time::Duration::from_millis(10));

    // Update
    lockfile.touch().unwrap();

    let stats2 = lockfile.stats().unwrap();
    let time2 = stats2.generated.unwrap();

    assert!(time2 > time1);
}

// ============================================================================
// Integration Tests: Error Recovery
// ============================================================================

#[test]
fn test_recover_from_corrupted_lockfile() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    // Create valid lockfile
    lockfile
        .upsert("io.ggen.test", "1.0.0", "sha", "https://example.com")
        .unwrap();

    // Corrupt it
    std::fs::write(lockfile.lockfile_path(), "corrupted data!!!").unwrap();

    // Try to load (should fail)
    let result = lockfile.load();
    assert!(result.is_err());

    // Recover by creating new lockfile
    let new_lockfile = lockfile.create().unwrap();
    lockfile.save(&new_lockfile).unwrap();

    // Should now work
    let loaded = lockfile.load().unwrap();
    assert!(loaded.is_some());
    assert_eq!(loaded.unwrap().packs.len(), 0);
}

#[test]
fn test_partial_install_recovery() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    // Simulate partial install: lockfile updated but cache failed
    lockfile
        .upsert("io.ggen.partial", "1.0.0", "sha", "https://example.com")
        .unwrap();

    // Verify pack is in lockfile
    assert!(lockfile.is_installed("io.ggen.partial").unwrap());

    // User can retry or remove
    lockfile.remove("io.ggen.partial").unwrap();
    assert!(!lockfile.is_installed("io.ggen.partial").unwrap());
}

// ============================================================================
// Integration Tests: Complex Scenarios
// ============================================================================

#[tokio::test]
async fn test_install_multiple_versions_different_packs() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    // Install different packs with different versions
    lockfile
        .upsert("io.ggen.a", "1.0.0", "sha1", "https://a.com")
        .unwrap();
    lockfile
        .upsert("io.ggen.b", "2.5.3", "sha2", "https://b.com")
        .unwrap();
    lockfile
        .upsert("io.ggen.c", "0.1.0-beta", "sha3", "https://c.com")
        .unwrap();

    let packs = lockfile.list().unwrap();
    assert_eq!(packs.len(), 3);

    // Verify each has correct version
    assert_eq!(lockfile.get("io.ggen.a").unwrap().unwrap().version, "1.0.0");
    assert_eq!(lockfile.get("io.ggen.b").unwrap().unwrap().version, "2.5.3");
    assert_eq!(
        lockfile.get("io.ggen.c").unwrap().unwrap().version,
        "0.1.0-beta"
    );
}

#[tokio::test]
async fn test_lockfile_sorted_order() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    // Install in random order
    lockfile
        .upsert("io.ggen.zebra", "1.0.0", "sha1", "https://z.com")
        .unwrap();
    lockfile
        .upsert("io.ggen.alpha", "1.0.0", "sha2", "https://a.com")
        .unwrap();
    lockfile
        .upsert("io.ggen.middle", "1.0.0", "sha3", "https://m.com")
        .unwrap();

    // Should be sorted alphabetically by ID
    let packs = lockfile.list().unwrap();
    assert_eq!(packs[0].id, "io.ggen.alpha");
    assert_eq!(packs[1].id, "io.ggen.middle");
    assert_eq!(packs[2].id, "io.ggen.zebra");
}

#[tokio::test]
async fn test_lockfile_with_pqc_signatures() {
    let temp_dir = TempDir::new().unwrap();
    let lockfile = LockfileManager::new(temp_dir.path());

    // Install pack with PQC signature
    lockfile
        .upsert_with_pqc(
            "io.ggen.secure",
            "1.0.0",
            "sha256_hash",
            "https://secure.com/pack.git",
            Some("pqc_signature_base64".to_string()),
            Some("pqc_pubkey_base64".to_string()),
        )
        .unwrap();

    let entry = lockfile.get("io.ggen.secure").unwrap().unwrap();
    assert!(entry.pqc_signature.is_some());
    assert!(entry.pqc_pubkey.is_some());

    // Verify signature data
    assert_eq!(entry.pqc_signature.unwrap(), "pqc_signature_base64");
    assert_eq!(entry.pqc_pubkey.unwrap(), "pqc_pubkey_base64");
}

// ============================================================================
// Integration Tests: Cache + Lockfile
// ============================================================================

#[test]
fn test_cache_and_lockfile_directories() {
    let temp_dir = TempDir::new().unwrap();

    let cache_dir = temp_dir.path().join("cache");
    let project_dir = temp_dir.path().join("project");

    let cache = CacheManager::with_dir(cache_dir.clone()).unwrap();
    let lockfile = LockfileManager::new(&project_dir);

    // Install pack
    lockfile
        .upsert("io.ggen.test", "1.0.0", "sha", "https://example.com")
        .unwrap();

    // Verify directories exist
    assert!(cache.cache_dir().exists());
    assert!(lockfile.lockfile_path().exists());

    // Verify they're in different locations
    assert_ne!(
        cache.cache_dir().canonicalize().unwrap(),
        lockfile
            .lockfile_path()
            .parent()
            .unwrap()
            .canonicalize()
            .unwrap()
    );
}
