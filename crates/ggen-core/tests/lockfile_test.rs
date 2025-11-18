//! Comprehensive tests for Pack Installation System lockfile
//!
//! Tests cover:
//! - Serialization/deserialization
//! - File I/O operations
//! - Dependency validation
//! - Circular dependency detection
//! - Pack management operations

use chrono::Utc;
use ggen_core::packs::lockfile::{LockedPack, PackLockfile, PackSource};
use std::path::PathBuf;
use tempfile::TempDir;

/// Helper to create a test pack with registry source
fn create_registry_pack(version: &str, deps: Vec<String>) -> LockedPack {
    LockedPack {
        version: version.to_string(),
        source: PackSource::Registry {
            url: "https://registry.ggen.io".to_string(),
        },
        integrity: Some(format!("sha256-test-{}", version)),
        installed_at: Utc::now(),
        dependencies: deps,
    }
}

/// Helper to create a test pack with GitHub source
fn create_github_pack(version: &str, org: &str, repo: &str, branch: &str) -> LockedPack {
    LockedPack {
        version: version.to_string(),
        source: PackSource::GitHub {
            org: org.to_string(),
            repo: repo.to_string(),
            branch: branch.to_string(),
        },
        integrity: None, // GitHub sources might not have integrity checksums
        installed_at: Utc::now(),
        dependencies: vec![],
    }
}

/// Helper to create a test pack with local source
fn create_local_pack(version: &str, path: &str) -> LockedPack {
    LockedPack {
        version: version.to_string(),
        source: PackSource::Local {
            path: PathBuf::from(path),
        },
        integrity: None,
        installed_at: Utc::now(),
        dependencies: vec![],
    }
}

#[test]
fn test_lockfile_serialization() {
    // Create a lockfile with various pack sources
    let mut lockfile = PackLockfile::new("4.0.0");

    lockfile.add_pack(
        "io.ggen.rust.cli",
        create_registry_pack("1.0.0", vec!["io.ggen.macros.std".to_string()]),
    );

    lockfile.add_pack(
        "io.ggen.github.pack",
        create_github_pack("2.0.0", "seanchatmangpt", "ggen", "main"),
    );

    lockfile.add_pack(
        "io.ggen.local.pack",
        create_local_pack("3.0.0", "/tmp/local-pack"),
    );

    // Serialize to JSON
    let json = serde_json::to_string_pretty(&lockfile).expect("Failed to serialize");

    // Verify JSON structure
    assert!(json.contains("io.ggen.rust.cli"));
    assert!(json.contains("io.ggen.github.pack"));
    assert!(json.contains("io.ggen.local.pack"));
    assert!(json.contains("\"ggen_version\": \"4.0.0\""));
    assert!(json.contains("Registry"));
    assert!(json.contains("GitHub"));
    assert!(json.contains("Local"));
}

#[test]
fn test_lockfile_deserialization() {
    // Create JSON representing a lockfile
    let json = r#"{
        "packs": {
            "io.ggen.test.pack": {
                "version": "1.5.0",
                "source": {
                    "type": "Registry",
                    "url": "https://registry.ggen.io"
                },
                "integrity": "sha256-abc123def456",
                "installed_at": "2024-01-01T00:00:00Z",
                "dependencies": ["io.ggen.dependency"]
            }
        },
        "updated_at": "2024-01-01T12:00:00Z",
        "ggen_version": "4.0.0"
    }"#;

    // Deserialize
    let lockfile: PackLockfile = serde_json::from_str(json).expect("Failed to deserialize");

    // Verify structure
    assert_eq!(lockfile.ggen_version, "4.0.0");
    assert_eq!(lockfile.packs.len(), 1);

    let pack = lockfile
        .get_pack("io.ggen.test.pack")
        .expect("Pack not found");
    assert_eq!(pack.version, "1.5.0");
    assert_eq!(pack.dependencies.len(), 1);
    assert_eq!(pack.dependencies[0], "io.ggen.dependency");
    assert_eq!(pack.integrity.as_ref().unwrap(), "sha256-abc123def456");

    match &pack.source {
        PackSource::Registry { url } => {
            assert_eq!(url, "https://registry.ggen.io");
        }
        _ => panic!("Expected Registry source"),
    }
}

#[test]
fn test_lockfile_save_and_load() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let lockfile_path = temp_dir.path().join(".ggen").join("packs.lock");

    // Create and save lockfile
    let mut original = PackLockfile::new("4.0.0");
    original.add_pack("pack.one", create_registry_pack("1.0.0", vec![]));
    original.add_pack(
        "pack.two",
        create_registry_pack("2.0.0", vec!["pack.one".to_string()]),
    );

    original
        .save(&lockfile_path)
        .expect("Failed to save lockfile");

    // Verify file exists
    assert!(lockfile_path.exists());

    // Load lockfile
    let loaded = PackLockfile::from_file(&lockfile_path).expect("Failed to load lockfile");

    // Verify contents match
    assert_eq!(loaded.ggen_version, original.ggen_version);
    assert_eq!(loaded.packs.len(), 2);
    assert!(loaded.get_pack("pack.one").is_some());
    assert!(loaded.get_pack("pack.two").is_some());

    let pack_two = loaded.get_pack("pack.two").unwrap();
    assert_eq!(pack_two.dependencies.len(), 1);
    assert_eq!(pack_two.dependencies[0], "pack.one");
}

#[test]
fn test_lockfile_dependency_validation() {
    let mut lockfile = PackLockfile::new("4.0.0");

    // Add pack with valid dependencies
    lockfile.add_pack("base.pack", create_registry_pack("1.0.0", vec![]));
    lockfile.add_pack(
        "dependent.pack",
        create_registry_pack("1.0.0", vec!["base.pack".to_string()]),
    );

    // Validation should pass
    assert!(lockfile.validate().is_ok());

    // Add pack with missing dependency
    lockfile.add_pack(
        "broken.pack",
        create_registry_pack("1.0.0", vec!["missing.pack".to_string()]),
    );

    // Validation should fail
    let result = lockfile.validate();
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("missing.pack"));
    assert!(err.to_string().contains("not in lockfile"));
}

#[test]
fn test_lockfile_add_pack() {
    let mut lockfile = PackLockfile::new("4.0.0");
    assert_eq!(lockfile.packs.len(), 0);

    // Add first pack
    let pack1 = create_registry_pack("1.0.0", vec![]);
    let initial_time = lockfile.updated_at;

    lockfile.add_pack("first.pack", pack1);
    assert_eq!(lockfile.packs.len(), 1);
    assert!(lockfile.updated_at > initial_time);

    // Add second pack
    let pack2 = create_github_pack("2.0.0", "org", "repo", "main");
    lockfile.add_pack("second.pack", pack2);
    assert_eq!(lockfile.packs.len(), 2);

    // Update existing pack (should replace)
    let pack1_updated = create_registry_pack("1.1.0", vec![]);
    lockfile.add_pack("first.pack", pack1_updated);
    assert_eq!(lockfile.packs.len(), 2); // Still 2 packs

    let retrieved = lockfile.get_pack("first.pack").unwrap();
    assert_eq!(retrieved.version, "1.1.0"); // Version updated
}

#[test]
fn test_lockfile_circular_dependency_detection() {
    let mut lockfile = PackLockfile::new("4.0.0");

    // Create circular dependency: A -> B -> C -> A
    lockfile.add_pack(
        "pack.a",
        create_registry_pack("1.0.0", vec!["pack.b".to_string()]),
    );
    lockfile.add_pack(
        "pack.b",
        create_registry_pack("1.0.0", vec!["pack.c".to_string()]),
    );
    lockfile.add_pack(
        "pack.c",
        create_registry_pack("1.0.0", vec!["pack.a".to_string()]),
    );

    // Validation should detect circular dependency
    let result = lockfile.validate();
    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("Circular dependency"));
}

#[test]
fn test_lockfile_remove_pack() {
    let mut lockfile = PackLockfile::new("4.0.0");

    lockfile.add_pack("pack.one", create_registry_pack("1.0.0", vec![]));
    lockfile.add_pack("pack.two", create_registry_pack("2.0.0", vec![]));
    assert_eq!(lockfile.packs.len(), 2);

    // Remove existing pack
    let removed = lockfile.remove_pack("pack.one");
    assert!(removed);
    assert_eq!(lockfile.packs.len(), 1);
    assert!(lockfile.get_pack("pack.one").is_none());
    assert!(lockfile.get_pack("pack.two").is_some());

    // Try to remove non-existent pack
    let not_removed = lockfile.remove_pack("pack.nonexistent");
    assert!(!not_removed);
    assert_eq!(lockfile.packs.len(), 1);
}

#[test]
fn test_lockfile_display_trait() {
    let mut lockfile = PackLockfile::new("4.0.0");

    lockfile.add_pack(
        "display.pack",
        create_registry_pack("1.0.0", vec!["dep.pack".to_string()]),
    );

    let display_str = format!("{}", lockfile);

    // Verify display output contains key information
    assert!(display_str.contains("Pack Lockfile"));
    assert!(display_str.contains("ggen v4.0.0"));
    assert!(display_str.contains("display.pack"));
    assert!(display_str.contains("1.0.0"));
    assert!(display_str.contains("Registry"));
    assert!(display_str.contains("Dependencies: dep.pack"));
}

#[test]
fn test_lockfile_btreemap_ordering() {
    let mut lockfile = PackLockfile::new("4.0.0");

    // Add packs in non-alphabetical order
    lockfile.add_pack("zebra.pack", create_registry_pack("1.0.0", vec![]));
    lockfile.add_pack("alpha.pack", create_registry_pack("1.0.0", vec![]));
    lockfile.add_pack("middle.pack", create_registry_pack("1.0.0", vec![]));

    // BTreeMap should maintain sorted order
    let pack_ids: Vec<&String> = lockfile.packs.keys().collect();
    assert_eq!(pack_ids, vec!["alpha.pack", "middle.pack", "zebra.pack"]);
}

#[test]
fn test_lockfile_missing_file_error() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let nonexistent_path = temp_dir.path().join("does_not_exist.lock");

    // Trying to load non-existent file should error
    let result = PackLockfile::from_file(&nonexistent_path);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("not found"));
}

#[test]
fn test_lockfile_invalid_json_error() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let lockfile_path = temp_dir.path().join("invalid.lock");

    // Write invalid JSON
    std::fs::write(&lockfile_path, "{ this is not valid json }").expect("Failed to write file");

    // Trying to load invalid JSON should error
    let result = PackLockfile::from_file(&lockfile_path);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("parse"));
}

#[test]
fn test_pack_source_variants() {
    // Test all PackSource variants
    let registry_pack = LockedPack {
        version: "1.0.0".to_string(),
        source: PackSource::Registry {
            url: "https://custom-registry.io".to_string(),
        },
        integrity: Some("sha256-registry".to_string()),
        installed_at: Utc::now(),
        dependencies: vec![],
    };

    let github_pack = LockedPack {
        version: "2.0.0".to_string(),
        source: PackSource::GitHub {
            org: "myorg".to_string(),
            repo: "myrepo".to_string(),
            branch: "v2.0.0".to_string(),
        },
        integrity: None,
        installed_at: Utc::now(),
        dependencies: vec![],
    };

    let local_pack = LockedPack {
        version: "3.0.0".to_string(),
        source: PackSource::Local {
            path: PathBuf::from("/custom/path/to/pack"),
        },
        integrity: None,
        installed_at: Utc::now(),
        dependencies: vec![],
    };

    // Verify serialization includes type tag
    let registry_json = serde_json::to_string(&registry_pack).unwrap();
    assert!(registry_json.contains("\"type\":\"Registry\""));

    let github_json = serde_json::to_string(&github_pack).unwrap();
    assert!(github_json.contains("\"type\":\"GitHub\""));

    let local_json = serde_json::to_string(&local_pack).unwrap();
    assert!(local_json.contains("\"type\":\"Local\""));
}

#[test]
fn test_lockfile_complex_dependency_tree() {
    let mut lockfile = PackLockfile::new("4.0.0");

    // Create a complex but valid dependency tree:
    // root -> [a, b]
    // a -> [c, d]
    // b -> [d, e]
    // c, d, e -> []

    lockfile.add_pack("pack.c", create_registry_pack("1.0.0", vec![]));
    lockfile.add_pack("pack.d", create_registry_pack("1.0.0", vec![]));
    lockfile.add_pack("pack.e", create_registry_pack("1.0.0", vec![]));

    lockfile.add_pack(
        "pack.a",
        create_registry_pack("1.0.0", vec!["pack.c".to_string(), "pack.d".to_string()]),
    );
    lockfile.add_pack(
        "pack.b",
        create_registry_pack("1.0.0", vec!["pack.d".to_string(), "pack.e".to_string()]),
    );
    lockfile.add_pack(
        "pack.root",
        create_registry_pack("1.0.0", vec!["pack.a".to_string(), "pack.b".to_string()]),
    );

    // Validation should pass - no circular deps
    assert!(lockfile.validate().is_ok());
    assert_eq!(lockfile.packs.len(), 6);
}

#[test]
fn test_lockfile_optional_integrity_field() {
    let mut lockfile = PackLockfile::new("4.0.0");

    // Pack with integrity
    let with_integrity = create_registry_pack("1.0.0", vec![]);
    assert!(with_integrity.integrity.is_some());
    lockfile.add_pack("with.integrity", with_integrity);

    // Pack without integrity
    let without_integrity = LockedPack {
        version: "2.0.0".to_string(),
        source: PackSource::Local {
            path: PathBuf::from("/tmp/pack"),
        },
        integrity: None,
        installed_at: Utc::now(),
        dependencies: vec![],
    };
    lockfile.add_pack("without.integrity", without_integrity);

    // Serialize and verify
    let json = serde_json::to_string_pretty(&lockfile).unwrap();

    // Pack with integrity should have the field
    assert!(json.contains("sha256-test-1.0.0"));

    // Pack without integrity should skip the field (serde skip_serializing_if)
    let without_section = json.split("\"without.integrity\"").nth(1).unwrap();
    let next_brace = without_section.find('}').unwrap();
    let pack_json = &without_section[..next_brace];
    assert!(!pack_json.contains("\"integrity\""));
}
