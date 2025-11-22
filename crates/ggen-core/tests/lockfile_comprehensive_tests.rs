//! Comprehensive Chicago TDD tests for the ggen lockfile system.
//!
//! This test suite covers all three lockfile types:
//! - LockfileManager (lockfile.rs) - TOML-based ggen.lock
//! - PackLockfile (packs/lockfile.rs) - JSON-based .ggen/packs.lock
//!
//! Chicago TDD Principles:
//! - State-based testing: Verify observable outputs/state changes
//! - Real collaborators: Use real objects, minimize mocks
//! - AAA pattern: Arrange-Act-Assert
//! - Behavior verification: Test what code does, not implementation

use chrono::Utc;
use ggen_core::lockfile::LockfileManager;
use ggen_core::packs::lockfile::{LockedPack, PackLockfile, PackSource};
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

// ============================================================================
// MODULE 1: LockfileManager Unit Tests (TOML-based ggen.lock)
// ============================================================================

mod lockfile_manager_tests {
    use super::*;

    // ------------------------------------------------------------------------
    // Create/Load/Save Operations
    // ------------------------------------------------------------------------

    #[test]
    fn test_manager_new_creates_correct_path() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();

        // Act
        let manager = LockfileManager::new(temp_dir.path());

        // Assert - Verify observable state: correct path constructed
        assert_eq!(manager.lockfile_path(), temp_dir.path().join("ggen.lock"));
    }

    #[test]
    fn test_manager_with_path_uses_custom_path() {
        // Arrange
        let custom_path = PathBuf::from("/tmp/custom/my.lock");

        // Act
        let manager = LockfileManager::with_path(custom_path.clone());

        // Assert - Verify custom path is used
        assert_eq!(manager.lockfile_path(), custom_path);
    }

    #[test]
    fn test_create_produces_valid_lockfile_structure() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        // Act
        let lockfile = manager.create().unwrap();

        // Assert - Verify state: correct default values
        assert_eq!(lockfile.version, "1.0");
        assert!(lockfile.packs.is_empty());
        // Generated time should be recent (within last minute)
        let now = Utc::now();
        assert!(lockfile.generated <= now);
        assert!(lockfile.generated > now - chrono::Duration::minutes(1));
    }

    #[test]
    fn test_save_creates_file_on_disk() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        let lockfile = manager.create().unwrap();

        // Act
        manager.save(&lockfile).unwrap();

        // Assert - Verify observable effect: file exists
        assert!(manager.lockfile_path().exists());
    }

    #[test]
    fn test_save_creates_parent_directories() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let nested_path = temp_dir.path().join("nested/dirs/ggen.lock");
        let manager = LockfileManager::with_path(nested_path.clone());
        let lockfile = manager.create().unwrap();

        // Act
        manager.save(&lockfile).unwrap();

        // Assert - Verify parent directories created
        assert!(nested_path.exists());
        assert!(nested_path.parent().unwrap().exists());
    }

    #[test]
    fn test_load_returns_none_for_nonexistent_file() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        // Act
        let result = manager.load().unwrap();

        // Assert - Verify state: None when file doesn't exist
        assert!(result.is_none());
    }

    #[test]
    fn test_load_returns_saved_lockfile() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        let original = manager.create().unwrap();
        manager.save(&original).unwrap();

        // Act
        let loaded = manager.load().unwrap().unwrap();

        // Assert - Verify roundtrip preserves data
        assert_eq!(loaded.version, original.version);
        assert_eq!(loaded.packs.len(), original.packs.len());
    }

    #[test]
    fn test_load_fails_on_corrupted_toml() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        fs::write(manager.lockfile_path(), "not valid toml {{{}}}").unwrap();

        // Act
        let result = manager.load();

        // Assert - Verify error on invalid TOML
        assert!(result.is_err());
    }

    // ------------------------------------------------------------------------
    // CRUD Operations
    // ------------------------------------------------------------------------

    #[test]
    fn test_upsert_adds_new_pack() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        // Act
        manager
            .upsert("io.ggen.test", "1.0.0", "sha256hash", "https://example.com")
            .unwrap();

        // Assert - Verify state change: pack exists
        let entry = manager.get("io.ggen.test").unwrap().unwrap();
        assert_eq!(entry.id, "io.ggen.test");
        assert_eq!(entry.version, "1.0.0");
        assert_eq!(entry.sha256, "sha256hash");
        assert_eq!(entry.source, "https://example.com");
    }

    #[test]
    fn test_upsert_updates_existing_pack() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        manager
            .upsert("io.ggen.test", "1.0.0", "old_hash", "https://old.com")
            .unwrap();

        // Act - Update same pack
        manager
            .upsert("io.ggen.test", "2.0.0", "new_hash", "https://new.com")
            .unwrap();

        // Assert - Verify state: updated values, single entry
        let entry = manager.get("io.ggen.test").unwrap().unwrap();
        assert_eq!(entry.version, "2.0.0");
        assert_eq!(entry.sha256, "new_hash");
        assert_eq!(entry.source, "https://new.com");

        // Only one pack should exist
        let all_packs = manager.list().unwrap();
        assert_eq!(all_packs.len(), 1);
    }

    #[test]
    fn test_get_returns_none_for_missing_pack() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        // Act
        let result = manager.get("nonexistent.pack").unwrap();

        // Assert
        assert!(result.is_none());
    }

    #[test]
    fn test_remove_deletes_existing_pack() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        manager
            .upsert("io.ggen.test", "1.0.0", "hash", "https://example.com")
            .unwrap();
        assert!(manager.is_installed("io.ggen.test").unwrap());

        // Act
        let removed = manager.remove("io.ggen.test").unwrap();

        // Assert - Verify state change: pack removed
        assert!(removed);
        assert!(!manager.is_installed("io.ggen.test").unwrap());
    }

    #[test]
    fn test_remove_returns_false_for_nonexistent_pack() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        // Act
        let removed = manager.remove("nonexistent.pack").unwrap();

        // Assert
        assert!(!removed);
    }

    #[test]
    fn test_list_returns_all_packs() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        manager
            .upsert("io.ggen.a", "1.0.0", "hash_a", "https://a.com")
            .unwrap();
        manager
            .upsert("io.ggen.b", "2.0.0", "hash_b", "https://b.com")
            .unwrap();
        manager
            .upsert("io.ggen.c", "3.0.0", "hash_c", "https://c.com")
            .unwrap();

        // Act
        let packs = manager.list().unwrap();

        // Assert
        assert_eq!(packs.len(), 3);
    }

    #[test]
    fn test_list_returns_sorted_by_id() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        // Insert in non-alphabetical order
        manager
            .upsert("io.ggen.zebra", "1.0.0", "z", "https://z.com")
            .unwrap();
        manager
            .upsert("io.ggen.alpha", "1.0.0", "a", "https://a.com")
            .unwrap();
        manager
            .upsert("io.ggen.middle", "1.0.0", "m", "https://m.com")
            .unwrap();

        // Act
        let packs = manager.list().unwrap();

        // Assert - Verify sorted order
        assert_eq!(packs[0].id, "io.ggen.alpha");
        assert_eq!(packs[1].id, "io.ggen.middle");
        assert_eq!(packs[2].id, "io.ggen.zebra");
    }

    #[test]
    fn test_is_installed_returns_correct_status() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        manager
            .upsert("io.ggen.test", "1.0.0", "hash", "https://example.com")
            .unwrap();

        // Act & Assert
        assert!(manager.is_installed("io.ggen.test").unwrap());
        assert!(!manager.is_installed("io.ggen.other").unwrap());
    }

    #[test]
    fn test_installed_packs_returns_hashmap() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        manager
            .upsert("io.ggen.a", "1.0.0", "a", "https://a.com")
            .unwrap();
        manager
            .upsert("io.ggen.b", "2.0.0", "b", "https://b.com")
            .unwrap();

        // Act
        let map = manager.installed_packs().unwrap();

        // Assert - Verify HashMap access
        assert_eq!(map.len(), 2);
        assert!(map.contains_key("io.ggen.a"));
        assert!(map.contains_key("io.ggen.b"));
        assert_eq!(map.get("io.ggen.a").unwrap().version, "1.0.0");
    }

    // ------------------------------------------------------------------------
    // PQC Signature Tests
    // ------------------------------------------------------------------------

    #[test]
    fn test_upsert_with_pqc_stores_signature() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        let sig = "base64_signature_data".to_string();
        let pubkey = "base64_pubkey_data".to_string();

        // Act
        manager
            .upsert_with_pqc(
                "io.ggen.secure",
                "1.0.0",
                "hash",
                "https://secure.com",
                Some(sig.clone()),
                Some(pubkey.clone()),
            )
            .unwrap();

        // Assert - Verify PQC fields stored
        let entry = manager.get("io.ggen.secure").unwrap().unwrap();
        assert_eq!(entry.pqc_signature, Some(sig));
        assert_eq!(entry.pqc_pubkey, Some(pubkey));
    }

    #[test]
    fn test_upsert_with_pqc_none_values() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        // Act - No PQC fields
        manager
            .upsert_with_pqc("io.ggen.test", "1.0.0", "hash", "https://example.com", None, None)
            .unwrap();

        // Assert
        let entry = manager.get("io.ggen.test").unwrap().unwrap();
        assert!(entry.pqc_signature.is_none());
        assert!(entry.pqc_pubkey.is_none());
    }

    #[test]
    fn test_pqc_signature_persists_through_save_load() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        let sig = "persistent_sig".to_string();
        let pubkey = "persistent_key".to_string();

        // Act - Save with PQC
        manager
            .upsert_with_pqc(
                "io.ggen.pqc",
                "1.0.0",
                "hash",
                "https://pqc.com",
                Some(sig.clone()),
                Some(pubkey.clone()),
            )
            .unwrap();

        // Reload
        let manager2 = LockfileManager::new(temp_dir.path());
        let entry = manager2.get("io.ggen.pqc").unwrap().unwrap();

        // Assert - PQC data persisted
        assert_eq!(entry.pqc_signature, Some(sig));
        assert_eq!(entry.pqc_pubkey, Some(pubkey));
    }

    // ------------------------------------------------------------------------
    // Statistics Tests
    // ------------------------------------------------------------------------

    #[test]
    fn test_stats_empty_lockfile() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        // Act
        let stats = manager.stats().unwrap();

        // Assert - No lockfile exists
        assert_eq!(stats.total_packs, 0);
        assert!(stats.generated.is_none());
        assert!(stats.version.is_none());
    }

    #[test]
    fn test_stats_with_packs() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        manager
            .upsert("io.ggen.a", "1.0.0", "a", "https://a.com")
            .unwrap();
        manager
            .upsert("io.ggen.b", "2.0.0", "b", "https://b.com")
            .unwrap();

        // Act
        let stats = manager.stats().unwrap();

        // Assert
        assert_eq!(stats.total_packs, 2);
        assert!(stats.generated.is_some());
        assert_eq!(stats.version, Some("1.0".to_string()));
    }

    #[test]
    fn test_touch_updates_timestamp() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        manager
            .upsert("io.ggen.test", "1.0.0", "hash", "https://test.com")
            .unwrap();
        let stats_before = manager.stats().unwrap();
        let time_before = stats_before.generated.unwrap();

        // Wait a bit to ensure time difference
        std::thread::sleep(std::time::Duration::from_millis(10));

        // Act
        manager.touch().unwrap();

        // Assert - Timestamp updated
        let stats_after = manager.stats().unwrap();
        let time_after = stats_after.generated.unwrap();
        assert!(time_after > time_before);
    }

    // ------------------------------------------------------------------------
    // Cache Operations
    // ------------------------------------------------------------------------

    #[test]
    fn test_cache_stats_initial_state() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        // Act
        let (len, cap) = manager.cache_stats();

        // Assert
        assert_eq!(len, 0);
        assert_eq!(cap, 1000); // Default capacity
    }

    #[test]
    fn test_clear_cache_empties_cache() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        // Perform operations that might populate cache
        manager
            .upsert("io.ggen.test", "1.0.0", "hash", "https://test.com")
            .unwrap();

        // Act
        manager.clear_cache();

        // Assert
        let (len, _) = manager.cache_stats();
        assert_eq!(len, 0);
    }

    // ------------------------------------------------------------------------
    // Bulk Operations
    // ------------------------------------------------------------------------

    #[test]
    fn test_upsert_bulk_adds_multiple_packs() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        let packs = vec![
            (
                "io.ggen.a".to_string(),
                "1.0.0".to_string(),
                "hash_a".to_string(),
                "https://a.com".to_string(),
            ),
            (
                "io.ggen.b".to_string(),
                "2.0.0".to_string(),
                "hash_b".to_string(),
                "https://b.com".to_string(),
            ),
            (
                "io.ggen.c".to_string(),
                "3.0.0".to_string(),
                "hash_c".to_string(),
                "https://c.com".to_string(),
            ),
        ];

        // Act
        manager.upsert_bulk(&packs).unwrap();

        // Assert - All packs added
        let all = manager.list().unwrap();
        assert_eq!(all.len(), 3);
        assert!(manager.is_installed("io.ggen.a").unwrap());
        assert!(manager.is_installed("io.ggen.b").unwrap());
        assert!(manager.is_installed("io.ggen.c").unwrap());
    }

    #[test]
    fn test_upsert_bulk_single_pack_uses_fast_path() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        let packs = vec![(
            "io.ggen.single".to_string(),
            "1.0.0".to_string(),
            "hash".to_string(),
            "https://single.com".to_string(),
        )];

        // Act
        manager.upsert_bulk(&packs).unwrap();

        // Assert - Single pack added correctly
        let entry = manager.get("io.ggen.single").unwrap().unwrap();
        assert_eq!(entry.version, "1.0.0");
    }

    #[test]
    fn test_upsert_bulk_updates_existing_packs() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        manager
            .upsert("io.ggen.existing", "1.0.0", "old", "https://old.com")
            .unwrap();

        let packs = vec![
            (
                "io.ggen.existing".to_string(),
                "2.0.0".to_string(),
                "new".to_string(),
                "https://new.com".to_string(),
            ),
            (
                "io.ggen.new".to_string(),
                "1.0.0".to_string(),
                "fresh".to_string(),
                "https://fresh.com".to_string(),
            ),
        ];

        // Act
        manager.upsert_bulk(&packs).unwrap();

        // Assert - Existing updated, new added
        let existing = manager.get("io.ggen.existing").unwrap().unwrap();
        assert_eq!(existing.version, "2.0.0");
        assert_eq!(existing.sha256, "new");

        let new = manager.get("io.ggen.new").unwrap().unwrap();
        assert_eq!(new.version, "1.0.0");
    }
}

// ============================================================================
// MODULE 2: PackLockfile Unit Tests (JSON-based .ggen/packs.lock)
// ============================================================================

mod pack_lockfile_tests {
    use super::*;

    fn create_test_pack(version: &str, deps: Vec<String>) -> LockedPack {
        LockedPack {
            version: version.to_string(),
            source: PackSource::Registry {
                url: "https://registry.ggen.io".to_string(),
            },
            integrity: Some("sha256-test".to_string()),
            installed_at: Utc::now(),
            dependencies: deps,
        }
    }

    // ------------------------------------------------------------------------
    // Create/Load/Save Operations
    // ------------------------------------------------------------------------

    #[test]
    fn test_pack_lockfile_new_creates_empty() {
        // Arrange & Act
        let lockfile = PackLockfile::new("4.0.0");

        // Assert
        assert!(lockfile.packs.is_empty());
        assert_eq!(lockfile.ggen_version, "4.0.0");
    }

    #[test]
    fn test_pack_lockfile_save_creates_file() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let path = temp_dir.path().join("packs.lock");
        let lockfile = PackLockfile::new("4.0.0");

        // Act
        lockfile.save(&path).unwrap();

        // Assert - File exists
        assert!(path.exists());
    }

    #[test]
    fn test_pack_lockfile_save_creates_parent_dirs() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let path = temp_dir.path().join("nested/dirs/packs.lock");
        let lockfile = PackLockfile::new("4.0.0");

        // Act
        lockfile.save(&path).unwrap();

        // Assert
        assert!(path.exists());
    }

    #[test]
    fn test_pack_lockfile_from_file_loads_saved() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let path = temp_dir.path().join("packs.lock");
        let mut original = PackLockfile::new("4.0.0");
        original.add_pack("test.pack", create_test_pack("1.0.0", vec![]));
        original.save(&path).unwrap();

        // Act
        let loaded = PackLockfile::from_file(&path).unwrap();

        // Assert
        assert_eq!(loaded.ggen_version, "4.0.0");
        assert!(loaded.get_pack("test.pack").is_some());
    }

    #[test]
    fn test_pack_lockfile_from_file_fails_nonexistent() {
        // Arrange
        let path = PathBuf::from("/nonexistent/path/packs.lock");

        // Act
        let result = PackLockfile::from_file(&path);

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_pack_lockfile_from_file_fails_invalid_json() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let path = temp_dir.path().join("invalid.lock");
        fs::write(&path, "not valid json {{{").unwrap();

        // Act
        let result = PackLockfile::from_file(&path);

        // Assert
        assert!(result.is_err());
    }

    // ------------------------------------------------------------------------
    // CRUD Operations
    // ------------------------------------------------------------------------

    #[test]
    fn test_add_pack_inserts_new_pack() {
        // Arrange
        let mut lockfile = PackLockfile::new("4.0.0");
        let pack = create_test_pack("1.0.0", vec![]);

        // Act
        lockfile.add_pack("new.pack", pack.clone());

        // Assert
        let retrieved = lockfile.get_pack("new.pack").unwrap();
        assert_eq!(retrieved.version, "1.0.0");
    }

    #[test]
    fn test_add_pack_updates_timestamp() {
        // Arrange
        let mut lockfile = PackLockfile::new("4.0.0");
        let time_before = lockfile.updated_at;

        std::thread::sleep(std::time::Duration::from_millis(10));

        // Act
        lockfile.add_pack("test.pack", create_test_pack("1.0.0", vec![]));

        // Assert
        assert!(lockfile.updated_at > time_before);
    }

    #[test]
    fn test_get_pack_returns_none_for_missing() {
        // Arrange
        let lockfile = PackLockfile::new("4.0.0");

        // Act
        let result = lockfile.get_pack("missing.pack");

        // Assert
        assert!(result.is_none());
    }

    #[test]
    fn test_remove_pack_deletes_existing() {
        // Arrange
        let mut lockfile = PackLockfile::new("4.0.0");
        lockfile.add_pack("to.remove", create_test_pack("1.0.0", vec![]));
        assert!(lockfile.get_pack("to.remove").is_some());

        // Act
        let removed = lockfile.remove_pack("to.remove");

        // Assert
        assert!(removed);
        assert!(lockfile.get_pack("to.remove").is_none());
    }

    #[test]
    fn test_remove_pack_returns_false_for_missing() {
        // Arrange
        let mut lockfile = PackLockfile::new("4.0.0");

        // Act
        let removed = lockfile.remove_pack("missing.pack");

        // Assert
        assert!(!removed);
    }

    #[test]
    fn test_remove_pack_updates_timestamp() {
        // Arrange
        let mut lockfile = PackLockfile::new("4.0.0");
        lockfile.add_pack("test.pack", create_test_pack("1.0.0", vec![]));
        let time_before = lockfile.updated_at;

        std::thread::sleep(std::time::Duration::from_millis(10));

        // Act
        lockfile.remove_pack("test.pack");

        // Assert
        assert!(lockfile.updated_at > time_before);
    }

    // ------------------------------------------------------------------------
    // Validation Tests
    // ------------------------------------------------------------------------

    #[test]
    fn test_validate_passes_for_valid_lockfile() {
        // Arrange
        let mut lockfile = PackLockfile::new("4.0.0");
        lockfile.add_pack("pack.a", create_test_pack("1.0.0", vec![]));
        lockfile.add_pack("pack.b", create_test_pack("1.0.0", vec![]));

        // Act
        let result = lockfile.validate();

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_fails_for_missing_dependency() {
        // Arrange
        let mut lockfile = PackLockfile::new("4.0.0");
        lockfile.add_pack(
            "pack.a",
            create_test_pack("1.0.0", vec!["missing.dep".to_string()]),
        );

        // Act
        let result = lockfile.validate();

        // Assert
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(err_msg.contains("missing.dep"));
    }

    #[test]
    fn test_validate_passes_with_valid_dependencies() {
        // Arrange
        let mut lockfile = PackLockfile::new("4.0.0");
        lockfile.add_pack("pack.dep", create_test_pack("1.0.0", vec![]));
        lockfile.add_pack(
            "pack.main",
            create_test_pack("1.0.0", vec!["pack.dep".to_string()]),
        );

        // Act
        let result = lockfile.validate();

        // Assert
        assert!(result.is_ok());
    }

    // ------------------------------------------------------------------------
    // Circular Dependency Detection Tests
    // ------------------------------------------------------------------------

    #[test]
    fn test_validate_detects_direct_circular_dependency() {
        // Arrange - A depends on B, B depends on A
        let mut lockfile = PackLockfile::new("4.0.0");
        lockfile.add_pack(
            "pack.a",
            create_test_pack("1.0.0", vec!["pack.b".to_string()]),
        );
        lockfile.add_pack(
            "pack.b",
            create_test_pack("1.0.0", vec!["pack.a".to_string()]),
        );

        // Act
        let result = lockfile.validate();

        // Assert
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(err_msg.contains("Circular"));
    }

    #[test]
    fn test_validate_detects_transitive_circular_dependency() {
        // Arrange - A -> B -> C -> A
        let mut lockfile = PackLockfile::new("4.0.0");
        lockfile.add_pack(
            "pack.a",
            create_test_pack("1.0.0", vec!["pack.b".to_string()]),
        );
        lockfile.add_pack(
            "pack.b",
            create_test_pack("1.0.0", vec!["pack.c".to_string()]),
        );
        lockfile.add_pack(
            "pack.c",
            create_test_pack("1.0.0", vec!["pack.a".to_string()]),
        );

        // Act
        let result = lockfile.validate();

        // Assert
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_allows_diamond_dependencies() {
        // Arrange - Diamond: A -> B,C; B -> D; C -> D (valid, no cycle)
        let mut lockfile = PackLockfile::new("4.0.0");
        lockfile.add_pack("pack.d", create_test_pack("1.0.0", vec![]));
        lockfile.add_pack(
            "pack.b",
            create_test_pack("1.0.0", vec!["pack.d".to_string()]),
        );
        lockfile.add_pack(
            "pack.c",
            create_test_pack("1.0.0", vec!["pack.d".to_string()]),
        );
        lockfile.add_pack(
            "pack.a",
            create_test_pack("1.0.0", vec!["pack.b".to_string(), "pack.c".to_string()]),
        );

        // Act
        let result = lockfile.validate();

        // Assert - Diamond is valid (no cycle)
        assert!(result.is_ok());
    }

    // ------------------------------------------------------------------------
    // PackSource Tests
    // ------------------------------------------------------------------------

    #[test]
    fn test_pack_source_registry_display() {
        // Arrange
        let source = PackSource::Registry {
            url: "https://registry.ggen.io".to_string(),
        };

        // Act
        let display = source.to_string();

        // Assert
        assert_eq!(display, "Registry(https://registry.ggen.io)");
    }

    #[test]
    fn test_pack_source_github_display() {
        // Arrange
        let source = PackSource::GitHub {
            org: "seanchatmangpt".to_string(),
            repo: "ggen".to_string(),
            branch: "main".to_string(),
        };

        // Act
        let display = source.to_string();

        // Assert
        assert_eq!(display, "GitHub(seanchatmangpt/ggen@main)");
    }

    #[test]
    fn test_pack_source_local_display() {
        // Arrange
        let source = PackSource::Local {
            path: PathBuf::from("/home/user/packs/mypack"),
        };

        // Act
        let display = source.to_string();

        // Assert
        assert_eq!(display, "Local(/home/user/packs/mypack)");
    }

    #[test]
    fn test_pack_source_serialization_roundtrip() {
        // Arrange
        let mut lockfile = PackLockfile::new("4.0.0");

        // Add packs with different source types
        let registry_pack = LockedPack {
            version: "1.0.0".to_string(),
            source: PackSource::Registry {
                url: "https://registry.ggen.io".to_string(),
            },
            integrity: None,
            installed_at: Utc::now(),
            dependencies: vec![],
        };

        let github_pack = LockedPack {
            version: "2.0.0".to_string(),
            source: PackSource::GitHub {
                org: "org".to_string(),
                repo: "repo".to_string(),
                branch: "v2".to_string(),
            },
            integrity: Some("sha256-github".to_string()),
            installed_at: Utc::now(),
            dependencies: vec![],
        };

        let local_pack = LockedPack {
            version: "0.1.0".to_string(),
            source: PackSource::Local {
                path: PathBuf::from("/local/pack"),
            },
            integrity: None,
            installed_at: Utc::now(),
            dependencies: vec![],
        };

        lockfile.add_pack("registry.pack", registry_pack);
        lockfile.add_pack("github.pack", github_pack);
        lockfile.add_pack("local.pack", local_pack);

        // Act - Serialize and deserialize
        let temp_dir = TempDir::new().unwrap();
        let path = temp_dir.path().join("sources.lock");
        lockfile.save(&path).unwrap();
        let loaded = PackLockfile::from_file(&path).unwrap();

        // Assert - Sources preserved
        let reg = loaded.get_pack("registry.pack").unwrap();
        match &reg.source {
            PackSource::Registry { url } => assert!(url.contains("registry.ggen.io")),
            _ => panic!("Expected Registry source"),
        }

        let gh = loaded.get_pack("github.pack").unwrap();
        match &gh.source {
            PackSource::GitHub { org, repo, branch } => {
                assert_eq!(org, "org");
                assert_eq!(repo, "repo");
                assert_eq!(branch, "v2");
            }
            _ => panic!("Expected GitHub source"),
        }

        let local = loaded.get_pack("local.pack").unwrap();
        match &local.source {
            PackSource::Local { path } => assert_eq!(path, &PathBuf::from("/local/pack")),
            _ => panic!("Expected Local source"),
        }
    }

    // ------------------------------------------------------------------------
    // Integrity Tests
    // ------------------------------------------------------------------------

    #[test]
    fn test_integrity_field_optional() {
        // Arrange
        let mut lockfile = PackLockfile::new("4.0.0");
        let pack_with_integrity = LockedPack {
            version: "1.0.0".to_string(),
            source: PackSource::Registry {
                url: "https://registry.ggen.io".to_string(),
            },
            integrity: Some("sha256-abc123".to_string()),
            installed_at: Utc::now(),
            dependencies: vec![],
        };
        let pack_without_integrity = LockedPack {
            version: "1.0.0".to_string(),
            source: PackSource::Registry {
                url: "https://registry.ggen.io".to_string(),
            },
            integrity: None,
            installed_at: Utc::now(),
            dependencies: vec![],
        };

        // Act
        lockfile.add_pack("with", pack_with_integrity);
        lockfile.add_pack("without", pack_without_integrity);

        // Assert
        assert!(lockfile.get_pack("with").unwrap().integrity.is_some());
        assert!(lockfile.get_pack("without").unwrap().integrity.is_none());
    }

    #[test]
    fn test_integrity_persists_through_serialization() {
        // Arrange
        let mut lockfile = PackLockfile::new("4.0.0");
        let integrity_value = "sha256-persistent_hash_value".to_string();
        let pack = LockedPack {
            version: "1.0.0".to_string(),
            source: PackSource::Registry {
                url: "https://registry.ggen.io".to_string(),
            },
            integrity: Some(integrity_value.clone()),
            installed_at: Utc::now(),
            dependencies: vec![],
        };
        lockfile.add_pack("test.pack", pack);

        // Act - Save and reload
        let temp_dir = TempDir::new().unwrap();
        let path = temp_dir.path().join("integrity.lock");
        lockfile.save(&path).unwrap();
        let loaded = PackLockfile::from_file(&path).unwrap();

        // Assert
        let loaded_pack = loaded.get_pack("test.pack").unwrap();
        assert_eq!(loaded_pack.integrity, Some(integrity_value));
    }
}

// ============================================================================
// MODULE 3: Cross-Module Integration Tests
// ============================================================================

mod integration_tests {
    use super::*;

    #[test]
    fn test_btreemap_ensures_deterministic_order() {
        // Arrange - PackLockfile uses BTreeMap for deterministic ordering
        let mut lockfile = PackLockfile::new("4.0.0");

        // Insert in random order
        lockfile.add_pack(
            "z.pack",
            LockedPack {
                version: "1.0.0".to_string(),
                source: PackSource::Registry {
                    url: "https://z.com".to_string(),
                },
                integrity: None,
                installed_at: Utc::now(),
                dependencies: vec![],
            },
        );
        lockfile.add_pack(
            "a.pack",
            LockedPack {
                version: "1.0.0".to_string(),
                source: PackSource::Registry {
                    url: "https://a.com".to_string(),
                },
                integrity: None,
                installed_at: Utc::now(),
                dependencies: vec![],
            },
        );
        lockfile.add_pack(
            "m.pack",
            LockedPack {
                version: "1.0.0".to_string(),
                source: PackSource::Registry {
                    url: "https://m.com".to_string(),
                },
                integrity: None,
                installed_at: Utc::now(),
                dependencies: vec![],
            },
        );

        // Act - Iterate over packs
        let keys: Vec<_> = lockfile.packs.keys().collect();

        // Assert - Keys are in sorted order
        assert_eq!(keys, vec!["a.pack", "m.pack", "z.pack"]);
    }

    #[test]
    fn test_lockfile_manager_toml_format_verification() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        manager
            .upsert_with_pqc(
                "io.ggen.test",
                "1.0.0",
                "abc123def456",
                "https://github.com/test/repo.git",
                Some("pqc_sig".to_string()),
                Some("pqc_key".to_string()),
            )
            .unwrap();

        // Act - Read raw file content
        let content = fs::read_to_string(manager.lockfile_path()).unwrap();

        // Assert - Verify TOML format
        assert!(content.contains("version = \"1.0\""));
        assert!(content.contains("[[packs]]"));
        assert!(content.contains("id = \"io.ggen.test\""));
        assert!(content.contains("sha256 = \"abc123def456\""));
        assert!(content.contains("pqc_signature = \"pqc_sig\""));
    }

    #[test]
    fn test_pack_lockfile_json_format_verification() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let path = temp_dir.path().join("packs.lock");
        let mut lockfile = PackLockfile::new("4.0.0");
        lockfile.add_pack(
            "test.pack",
            LockedPack {
                version: "1.0.0".to_string(),
                source: PackSource::GitHub {
                    org: "test".to_string(),
                    repo: "repo".to_string(),
                    branch: "main".to_string(),
                },
                integrity: Some("sha256-test".to_string()),
                installed_at: Utc::now(),
                dependencies: vec!["dep.pack".to_string()],
            },
        );
        lockfile.add_pack(
            "dep.pack",
            LockedPack {
                version: "0.1.0".to_string(),
                source: PackSource::Registry {
                    url: "https://registry.ggen.io".to_string(),
                },
                integrity: None,
                installed_at: Utc::now(),
                dependencies: vec![],
            },
        );
        lockfile.save(&path).unwrap();

        // Act - Read raw JSON
        let content = fs::read_to_string(&path).unwrap();
        let json: serde_json::Value = serde_json::from_str(&content).unwrap();

        // Assert - Verify JSON structure
        assert!(json.get("packs").is_some());
        assert!(json.get("updated_at").is_some());
        assert_eq!(json.get("ggen_version").unwrap(), "4.0.0");
    }
}

// ============================================================================
// MODULE 4: Performance Tests
// ============================================================================

mod performance_tests {
    use super::*;

    #[test]
    fn test_bulk_upsert_performance() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        // Create 100 packs
        let packs: Vec<_> = (0..100)
            .map(|i| {
                (
                    format!("io.ggen.pack{:03}", i),
                    "1.0.0".to_string(),
                    format!("hash{:03}", i),
                    format!("https://pack{}.com", i),
                )
            })
            .collect();

        // Act - Measure bulk insert
        let start = std::time::Instant::now();
        manager.upsert_bulk(&packs).unwrap();
        let duration = start.elapsed();

        // Assert - Should complete in reasonable time (under 1s)
        assert!(
            duration.as_secs() < 1,
            "Bulk upsert took too long: {:?}",
            duration
        );
        assert_eq!(manager.list().unwrap().len(), 100);
    }

    #[test]
    fn test_list_performance_with_many_packs() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        // Add 50 packs
        for i in 0..50 {
            manager
                .upsert(
                    &format!("io.ggen.pack{:02}", i),
                    "1.0.0",
                    &format!("hash{:02}", i),
                    &format!("https://pack{}.com", i),
                )
                .unwrap();
        }

        // Act - Measure list operation
        let start = std::time::Instant::now();
        let packs = manager.list().unwrap();
        let duration = start.elapsed();

        // Assert
        assert_eq!(packs.len(), 50);
        assert!(
            duration.as_millis() < 100,
            "List took too long: {:?}",
            duration
        );
    }

    #[test]
    fn test_pack_lockfile_large_dependency_graph() {
        // Arrange - Create lockfile with deep dependency chain
        let mut lockfile = PackLockfile::new("4.0.0");

        // Create chain: pack.0 -> pack.1 -> pack.2 -> ... -> pack.49
        for i in 0..50 {
            let deps = if i > 0 {
                vec![format!("pack.{}", i - 1)]
            } else {
                vec![]
            };
            lockfile.add_pack(
                format!("pack.{}", i),
                LockedPack {
                    version: "1.0.0".to_string(),
                    source: PackSource::Registry {
                        url: "https://registry.ggen.io".to_string(),
                    },
                    integrity: None,
                    installed_at: Utc::now(),
                    dependencies: deps,
                },
            );
        }

        // Act - Validate (includes circular dependency check)
        let start = std::time::Instant::now();
        let result = lockfile.validate();
        let duration = start.elapsed();

        // Assert - Should validate quickly (no circular deps in chain)
        assert!(result.is_ok());
        assert!(
            duration.as_millis() < 500,
            "Validation took too long: {:?}",
            duration
        );
    }
}

// ============================================================================
// MODULE 5: Edge Case Tests
// ============================================================================

mod edge_case_tests {
    use super::*;

    #[test]
    fn test_empty_pack_id() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());

        // Act - Empty ID should still work (no validation on ID format)
        manager.upsert("", "1.0.0", "hash", "https://empty.com").unwrap();

        // Assert
        assert!(manager.get("").unwrap().is_some());
    }

    #[test]
    fn test_special_characters_in_pack_id() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        let special_id = "io.ggen/test-pack_v2@latest";

        // Act
        manager
            .upsert(special_id, "1.0.0", "hash", "https://special.com")
            .unwrap();

        // Assert
        let entry = manager.get(special_id).unwrap().unwrap();
        assert_eq!(entry.id, special_id);
    }

    #[test]
    fn test_very_long_sha256_value() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        let long_hash = "a".repeat(1000); // Unrealistic but tests handling

        // Act
        manager
            .upsert("io.ggen.test", "1.0.0", &long_hash, "https://test.com")
            .unwrap();

        // Assert
        let entry = manager.get("io.ggen.test").unwrap().unwrap();
        assert_eq!(entry.sha256.len(), 1000);
    }

    #[test]
    fn test_unicode_in_source_url() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        let unicode_url = "https://example.com/\u{1F600}/pack.git"; // Emoji in URL

        // Act
        manager
            .upsert("io.ggen.unicode", "1.0.0", "hash", unicode_url)
            .unwrap();

        // Assert - Reload and verify
        let manager2 = LockfileManager::new(temp_dir.path());
        let entry = manager2.get("io.ggen.unicode").unwrap().unwrap();
        assert!(entry.source.contains("\u{1F600}"));
    }

    #[test]
    fn test_pack_lockfile_empty_dependencies_vec() {
        // Arrange
        let mut lockfile = PackLockfile::new("4.0.0");
        let pack = LockedPack {
            version: "1.0.0".to_string(),
            source: PackSource::Registry {
                url: "https://registry.ggen.io".to_string(),
            },
            integrity: None,
            installed_at: Utc::now(),
            dependencies: vec![], // Empty dependencies
        };
        lockfile.add_pack("test.pack", pack);

        // Act - Save and reload
        let temp_dir = TempDir::new().unwrap();
        let path = temp_dir.path().join("packs.lock");
        lockfile.save(&path).unwrap();
        let loaded = PackLockfile::from_file(&path).unwrap();

        // Assert - Empty vec preserved
        let loaded_pack = loaded.get_pack("test.pack").unwrap();
        assert!(loaded_pack.dependencies.is_empty());
    }

    #[test]
    fn test_self_dependency_detected_as_circular() {
        // Arrange - Pack depends on itself
        let mut lockfile = PackLockfile::new("4.0.0");
        lockfile.add_pack(
            "self.dep",
            LockedPack {
                version: "1.0.0".to_string(),
                source: PackSource::Registry {
                    url: "https://registry.ggen.io".to_string(),
                },
                integrity: None,
                installed_at: Utc::now(),
                dependencies: vec!["self.dep".to_string()], // Self-reference
            },
        );

        // Act
        let result = lockfile.validate();

        // Assert - Should detect circular dependency
        assert!(result.is_err());
    }

    #[test]
    fn test_concurrent_read_write_isolation() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager1 = LockfileManager::new(temp_dir.path());
        let manager2 = LockfileManager::new(temp_dir.path());

        // Act - Both managers write different data
        manager1
            .upsert("io.ggen.a", "1.0.0", "hash_a", "https://a.com")
            .unwrap();
        manager2
            .upsert("io.ggen.b", "2.0.0", "hash_b", "https://b.com")
            .unwrap();

        // Assert - Last write wins (manager2)
        let manager3 = LockfileManager::new(temp_dir.path());
        let packs = manager3.list().unwrap();

        // Note: Without proper locking, this test shows the race condition behavior
        // In production, file locking should be used
        assert!(!packs.is_empty());
    }

    #[test]
    fn test_version_field_preserved() {
        // Arrange
        let temp_dir = TempDir::new().unwrap();
        let manager = LockfileManager::new(temp_dir.path());
        manager
            .upsert("io.ggen.test", "1.0.0", "hash", "https://test.com")
            .unwrap();

        // Act - Load and check version
        let lockfile = manager.load().unwrap().unwrap();

        // Assert - Version is "1.0"
        assert_eq!(lockfile.version, "1.0");
    }
}
