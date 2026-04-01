//! Test lockfile persistence operations
//!
//! This test verifies that PackLockfile::save() and PackLockfile::from_file()
//! work correctly for pack installation tracking.

use ggen_core::packs::lockfile::{LockedPack, PackLockfile, PackSource};
use std::path::PathBuf;
use tempfile::TempDir;

#[test]
fn test_lockfile_save_and_load() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let lock_path = temp_dir.path().join("test-packs.lock");

    // Create a new lockfile
    let mut lockfile = PackLockfile::new("4.0.0");

    // Add a test pack
    let pack = LockedPack {
        version: "1.0.0".to_string(),
        source: PackSource::Local {
            path: PathBuf::from("/tmp/test-pack"),
        },
        integrity: None,
        installed_at: chrono::Utc::now(),
        dependencies: vec![],
    };

    lockfile.add_pack("test-pack", pack);

    // Save the lockfile
    lockfile
        .save(&lock_path)
        .expect("Failed to save lockfile");

    // Verify the file exists
    assert!(lock_path.exists(), "Lockfile should exist after save");

    // Load the lockfile
    let loaded = PackLockfile::from_file(&lock_path).expect("Failed to load lockfile");

    // Verify content
    assert_eq!(loaded.ggen_version, "4.0.0");
    assert_eq!(loaded.packs.len(), 1);
    assert!(loaded.packs.contains_key("test-pack"));

    let loaded_pack = loaded.get_pack("test-pack").expect("Pack not found");
    assert_eq!(loaded_pack.version, "1.0.0");
    assert_eq!(loaded_pack.dependencies.len(), 0);

    // Test validation
    loaded.validate().expect("Lockfile should validate");
}

#[test]
fn test_lockfile_format_correctness() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let lock_path = temp_dir.path().join("test-packs.lock");

    let mut lockfile = PackLockfile::new("4.0.0");

    // Add pack with all fields populated
    let pack = LockedPack {
        version: "1.0.0".to_string(),
        source: PackSource::GitHub {
            org: "test-org".to_string(),
            repo: "test-repo".to_string(),
            branch: "main".to_string(),
        },
        integrity: Some("sha256-abc123".to_string()),
        installed_at: chrono::Utc::now(),
        dependencies: vec!["dep1".to_string(), "dep2".to_string()],
    };

    lockfile.add_pack("test-pack", pack.clone());

    // Add dependency packs
    lockfile.add_pack(
        "dep1",
        LockedPack {
            version: "2.0.0".to_string(),
            source: PackSource::Registry {
                url: "https://registry.ggen.io".to_string(),
            },
            integrity: None,
            installed_at: chrono::Utc::now(),
            dependencies: vec![],
        },
    );

    lockfile.add_pack(
        "dep2",
        LockedPack {
            version: "3.0.0".to_string(),
            source: PackSource::Registry {
                url: "https://registry.ggen.io".to_string(),
            },
            integrity: None,
            installed_at: chrono::Utc::now(),
            dependencies: vec![],
        },
    );

    lockfile
        .save(&lock_path)
        .expect("Failed to save lockfile");

    // Load and verify JSON structure
    let content = std::fs::read_to_string(&lock_path).expect("Failed to read lockfile");
    let json: serde_json::Value =
        serde_json::from_str(&content).expect("Failed to parse JSON");

    // Verify top-level fields
    assert!(json.get("ggen_version").is_some());
    assert!(json.get("updated_at").is_some());
    assert!(json.get("packs").is_some());

    // Verify pack structure
    let packs = json.get("packs").unwrap().as_object().unwrap();
    assert!(packs.contains_key("test-pack"));

    let test_pack = &packs["test-pack"];
    assert!(test_pack.get("version").is_some());
    assert!(test_pack.get("source").is_some());
    assert!(test_pack.get("installed_at").is_some());
    assert!(test_pack.get("dependencies").is_some());

    // Verify source has "type" field (tagged enum)
    let source = test_pack.get("source").unwrap();
    assert!(source.get("type").is_some());
}

#[test]
fn test_lockfile_validation_detects_missing_dependencies() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let lock_path = temp_dir.path().join("test-packs.lock");

    let mut lockfile = PackLockfile::new("4.0.0");

    // Add a pack with a dependency that doesn't exist
    let pack = LockedPack {
        version: "1.0.0".to_string(),
        source: PackSource::Registry {
            url: "https://registry.ggen.io".to_string(),
        },
        integrity: None,
        installed_at: chrono::Utc::now(),
        dependencies: vec!["missing-dep".to_string()],
    };

    lockfile.add_pack("test-pack", pack);

    // Save should succeed (validation happens on load)
    lockfile
        .save(&lock_path)
        .expect("Failed to save lockfile");

    // Loading should fail validation
    let result = PackLockfile::from_file(&lock_path);
    assert!(result.is_err(), "Should fail validation for missing dependency");

    let err_msg = result.unwrap_err().to_string();
    assert!(
        err_msg.contains("missing-dep") || err_msg.contains("not in lockfile"),
        "Error should mention missing dependency"
    );
}

#[test]
fn test_lockfile_updates_timestamp() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let lock_path = temp_dir.path().join("test-packs.lock");

    let mut lockfile = PackLockfile::new("4.0.0");
    let original_timestamp = lockfile.updated_at;

    // Wait a bit to ensure timestamp difference
    std::thread::sleep(std::time::Duration::from_millis(10));

    // Add a pack
    let pack = LockedPack {
        version: "1.0.0".to_string(),
        source: PackSource::Local {
            path: PathBuf::from("/tmp/test"),
        },
        integrity: None,
        installed_at: chrono::Utc::now(),
        dependencies: vec![],
    };

    lockfile.add_pack("test-pack", pack);

    // Timestamp should be updated
    assert!(
        lockfile.updated_at > original_timestamp,
        "updated_at should change when pack is added"
    );

    // Save, load, and verify
    lockfile
        .save(&lock_path)
        .expect("Failed to save lockfile");
    let loaded = PackLockfile::from_file(&lock_path).expect("Failed to load lockfile");

    assert_eq!(loaded.updated_at, lockfile.updated_at);
}
