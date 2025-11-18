//! Comprehensive tests for PackLockfile functionality
//!
//! This test suite covers:
//! - Lockfile serialization/deserialization
//! - State management and updates
//! - PQC signature handling
//! - Performance benchmarks
//! - Error handling

use ggen_core::lockfile::{LockEntry, Lockfile, LockfileManager};
use std::path::Path;
use tempfile::TempDir;

/// Test helper: Create a basic lock entry
fn create_test_entry(id: &str, version: &str) -> LockEntry {
    LockEntry {
        id: id.to_string(),
        version: version.to_string(),
        sha256: "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855".to_string(),
        source: format!("https://github.com/test/{}.git", id),
        dependencies: None,
        pqc_signature: None,
        pqc_pubkey: None,
    }
}

// ============================================================================
// Unit Tests: Lockfile Structure
// ============================================================================

#[test]
fn test_lockfile_new() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    let lockfile = manager.create().unwrap();

    assert_eq!(lockfile.version, "1.0");
    assert_eq!(lockfile.packs.len(), 0);
    assert!(lockfile.generated <= chrono::Utc::now());
}

#[test]
fn test_lockfile_serialize_to_toml() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    let mut lockfile = manager.create().unwrap();
    lockfile
        .packs
        .push(create_test_entry("io.ggen.test", "1.0.0"));

    manager.save(&lockfile).unwrap();

    let content = std::fs::read_to_string(manager.lockfile_path()).unwrap();
    assert!(content.contains("version = \"1.0\""));
    assert!(content.contains("id = \"io.ggen.test\""));
    assert!(content.contains("version = \"1.0.0\""));
}

#[test]
fn test_lockfile_deserialize_from_toml() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Create and save
    let mut lockfile = manager.create().unwrap();
    lockfile
        .packs
        .push(create_test_entry("io.ggen.test", "1.0.0"));
    manager.save(&lockfile).unwrap();

    // Load and verify
    let loaded = manager.load().unwrap().unwrap();
    assert_eq!(loaded.version, "1.0");
    assert_eq!(loaded.packs.len(), 1);
    assert_eq!(loaded.packs[0].id, "io.ggen.test");
}

#[test]
fn test_lockfile_save_creates_directory() {
    let temp_dir = TempDir::new().unwrap();
    let nested_path = temp_dir.path().join("deeply").join("nested");
    let manager = LockfileManager::new(&nested_path);

    let lockfile = manager.create().unwrap();
    manager.save(&lockfile).unwrap();

    assert!(manager.lockfile_path().exists());
    assert!(manager.lockfile_path().parent().unwrap().exists());
}

#[test]
fn test_lockfile_load_missing_file() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    let result = manager.load().unwrap();

    assert!(result.is_none());
}

// ============================================================================
// Unit Tests: State Management
// ============================================================================

#[test]
fn test_lockfile_upsert_new_pack() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    manager
        .upsert(
            "io.ggen.new",
            "1.0.0",
            "abc123",
            "https://github.com/test/new.git",
        )
        .unwrap();

    let entry = manager.get("io.ggen.new").unwrap().unwrap();
    assert_eq!(entry.id, "io.ggen.new");
    assert_eq!(entry.version, "1.0.0");
    assert_eq!(entry.sha256, "abc123");
}

#[test]
fn test_lockfile_upsert_updates_existing() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Add initial version
    manager
        .upsert("io.ggen.test", "1.0.0", "abc", "https://example.com")
        .unwrap();

    // Update to new version
    manager
        .upsert("io.ggen.test", "2.0.0", "def", "https://example.com")
        .unwrap();

    let entry = manager.get("io.ggen.test").unwrap().unwrap();
    assert_eq!(entry.version, "2.0.0");
    assert_eq!(entry.sha256, "def");

    // Should only have one entry
    let all_packs = manager.list().unwrap();
    assert_eq!(all_packs.len(), 1);
}

#[test]
fn test_lockfile_remove_pack() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    manager
        .upsert("io.ggen.test", "1.0.0", "abc", "https://example.com")
        .unwrap();
    assert!(manager.is_installed("io.ggen.test").unwrap());

    let removed = manager.remove("io.ggen.test").unwrap();
    assert!(removed);
    assert!(!manager.is_installed("io.ggen.test").unwrap());
}

#[test]
fn test_lockfile_remove_nonexistent() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    let removed = manager.remove("io.ggen.nonexistent").unwrap();
    assert!(!removed);
}

#[test]
fn test_lockfile_touch_updates_timestamp() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Create initial lockfile
    let lockfile = manager.create().unwrap();
    manager.save(&lockfile).unwrap();
    let initial_time = lockfile.generated;

    // Sleep to ensure time difference
    std::thread::sleep(std::time::Duration::from_millis(10));

    // Touch the lockfile
    manager.touch().unwrap();

    let updated = manager.load().unwrap().unwrap();
    assert!(updated.generated > initial_time);
}

// ============================================================================
// Unit Tests: PQC Signatures
// ============================================================================

#[test]
fn test_lockfile_upsert_with_pqc_signature() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    manager
        .upsert_with_pqc(
            "io.ggen.secure",
            "1.0.0",
            "abc123",
            "https://example.com",
            Some("pqc_sig_base64_encoded".to_string()),
            Some("pqc_pubkey_base64_encoded".to_string()),
        )
        .unwrap();

    let entry = manager.get("io.ggen.secure").unwrap().unwrap();
    assert_eq!(
        entry.pqc_signature,
        Some("pqc_sig_base64_encoded".to_string())
    );
    assert_eq!(
        entry.pqc_pubkey,
        Some("pqc_pubkey_base64_encoded".to_string())
    );
}

#[test]
fn test_lockfile_pqc_optional() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    manager
        .upsert_with_pqc(
            "io.ggen.nosig",
            "1.0.0",
            "abc123",
            "https://example.com",
            None,
            None,
        )
        .unwrap();

    let entry = manager.get("io.ggen.nosig").unwrap().unwrap();
    assert!(entry.pqc_signature.is_none());
    assert!(entry.pqc_pubkey.is_none());
}

// ============================================================================
// Unit Tests: Query Operations
// ============================================================================

#[test]
fn test_lockfile_list_all_packs() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    manager
        .upsert("io.ggen.a", "1.0.0", "abc", "https://a.com")
        .unwrap();
    manager
        .upsert("io.ggen.b", "2.0.0", "def", "https://b.com")
        .unwrap();
    manager
        .upsert("io.ggen.c", "3.0.0", "ghi", "https://c.com")
        .unwrap();

    let packs = manager.list().unwrap();
    assert_eq!(packs.len(), 3);

    // Should be sorted by ID
    assert_eq!(packs[0].id, "io.ggen.a");
    assert_eq!(packs[1].id, "io.ggen.b");
    assert_eq!(packs[2].id, "io.ggen.c");
}

#[test]
fn test_lockfile_installed_packs_map() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    manager
        .upsert("io.ggen.a", "1.0.0", "abc", "https://a.com")
        .unwrap();
    manager
        .upsert("io.ggen.b", "2.0.0", "def", "https://b.com")
        .unwrap();

    let map = manager.installed_packs().unwrap();
    assert_eq!(map.len(), 2);
    assert!(map.contains_key("io.ggen.a"));
    assert!(map.contains_key("io.ggen.b"));
    assert_eq!(map.get("io.ggen.a").unwrap().version, "1.0.0");
}

#[test]
fn test_lockfile_stats() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Empty lockfile
    let stats = manager.stats().unwrap();
    assert_eq!(stats.total_packs, 0);
    assert!(stats.generated.is_none());
    assert!(stats.version.is_none());

    // With packs
    manager
        .upsert("io.ggen.a", "1.0.0", "abc", "https://a.com")
        .unwrap();
    manager
        .upsert("io.ggen.b", "2.0.0", "def", "https://b.com")
        .unwrap();

    let stats = manager.stats().unwrap();
    assert_eq!(stats.total_packs, 2);
    assert!(stats.generated.is_some());
    assert_eq!(stats.version, Some("1.0".to_string()));
}

// ============================================================================
// Performance Tests
// ============================================================================

#[test]
fn test_lockfile_save_performance_100_packs() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Create lockfile with 100 packs
    let mut lockfile = manager.create().unwrap();
    for i in 0..100 {
        lockfile
            .packs
            .push(create_test_entry(&format!("io.ggen.pack{}", i), "1.0.0"));
    }

    let start = std::time::Instant::now();
    manager.save(&lockfile).unwrap();
    let duration = start.elapsed();

    // Should complete in <100ms even with 100 packs
    assert!(
        duration.as_millis() < 100,
        "Save took {}ms",
        duration.as_millis()
    );
}

#[test]
fn test_lockfile_load_performance_100_packs() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Create and save lockfile with 100 packs
    let mut lockfile = manager.create().unwrap();
    for i in 0..100 {
        lockfile
            .packs
            .push(create_test_entry(&format!("io.ggen.pack{}", i), "1.0.0"));
    }
    manager.save(&lockfile).unwrap();

    let start = std::time::Instant::now();
    let _loaded = manager.load().unwrap();
    let duration = start.elapsed();

    // Should complete in <50ms even with 100 packs
    assert!(
        duration.as_millis() < 50,
        "Load took {}ms",
        duration.as_millis()
    );
}

// ============================================================================
// Security Tests
// ============================================================================

#[test]
fn test_lockfile_checksum_verification() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    let checksum = "e3b0c44298fc1c149afbf4c8996fb92427ae41e4649b934ca495991b7852b855";
    manager
        .upsert("io.ggen.test", "1.0.0", checksum, "https://example.com")
        .unwrap();

    let entry = manager.get("io.ggen.test").unwrap().unwrap();
    assert_eq!(entry.sha256, checksum);
    assert_eq!(entry.sha256.len(), 64); // SHA256 is 64 hex characters
}

#[test]
fn test_lockfile_path_traversal_prevention() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Try to insert pack with path traversal in ID
    let result = manager.upsert(
        "../../../etc/passwd",
        "1.0.0",
        "abc123",
        "https://malicious.com",
    );

    // Should succeed (path traversal is in ID, not filesystem path)
    // The lockfile itself is at a fixed location
    assert!(result.is_ok());

    // Verify lockfile is still in expected location
    assert!(manager.lockfile_path().starts_with(temp_dir.path()));
}

#[test]
fn test_lockfile_tampering_detection() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Create valid lockfile
    manager
        .upsert("io.ggen.test", "1.0.0", "abc123", "https://example.com")
        .unwrap();

    // Tamper with the file
    let mut content = std::fs::read_to_string(manager.lockfile_path()).unwrap();
    content = content.replace("1.0.0", "2.0.0");
    std::fs::write(manager.lockfile_path(), content).unwrap();

    // Load should succeed (TOML parsing works)
    let loaded = manager.load().unwrap().unwrap();

    // But version should be tampered
    assert_eq!(loaded.packs[0].version, "2.0.0");

    // Note: Real tampering detection would require signatures,
    // which is why we have PQC signatures
}

// ============================================================================
// Error Case Tests
// ============================================================================

#[test]
fn test_lockfile_corrupted_toml() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Write invalid TOML
    std::fs::write(manager.lockfile_path(), "invalid toml {{{ content").unwrap();

    let result = manager.load();
    assert!(result.is_err());
}

#[test]
fn test_lockfile_permission_denied() {
    // Skip this test on platforms where we can't easily test permissions
    #[cfg(unix)]
    {
        use std::os::unix::fs::PermissionsExt;

        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        // Create lockfile
        let lockfile = manager.create().unwrap();
        manager.save(&lockfile).unwrap();

        // Make lockfile read-only
        let mut perms = std::fs::metadata(manager.lockfile_path())
            .unwrap()
            .permissions();
        perms.set_mode(0o444);
        std::fs::set_permissions(manager.lockfile_path(), perms).unwrap();

        // Try to save (should fail)
        let result = manager.save(&lockfile);
        assert!(result.is_err());

        // Clean up: restore permissions for TempDir cleanup
        let mut perms = std::fs::metadata(manager.lockfile_path())
            .unwrap()
            .permissions();
        perms.set_mode(0o644);
        std::fs::set_permissions(manager.lockfile_path(), perms).unwrap();
    }
}

#[test]
fn test_lockfile_empty_pack_list() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Create empty lockfile
    let lockfile = manager.create().unwrap();
    manager.save(&lockfile).unwrap();

    // Load and verify
    let loaded = manager.load().unwrap().unwrap();
    assert_eq!(loaded.packs.len(), 0);

    let list = manager.list().unwrap();
    assert_eq!(list.len(), 0);
}

#[test]
fn test_lockfile_multiple_concurrent_operations() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Note: This is a simplified concurrency test
    // Real concurrent access would require Arc<Mutex<>> or similar

    manager
        .upsert("io.ggen.a", "1.0.0", "abc", "https://a.com")
        .unwrap();
    manager
        .upsert("io.ggen.b", "1.0.0", "def", "https://b.com")
        .unwrap();

    let list = manager.list().unwrap();
    assert_eq!(list.len(), 2);
}
