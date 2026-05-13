//! Integration tests for pack lockfile management.

use ggen_core::lockfile::{Lockfile, LockfileEntry, ProfileRef, RegistrySource};
use ggen_core::marketplace::trust::TrustTier;
use std::path::PathBuf;
use tempfile::TempDir;

fn create_test_profile() -> ProfileRef {
    ProfileRef {
        profile_id: "development".to_string(),
        runtime_constraints: vec![],
        trust_requirement: TrustTier::Experimental,
    }
}

#[test]
fn test_lockfile_lifecycle() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path();
    let profile = create_test_profile();

    // 1. Initial state (no lockfile)
    assert!(Lockfile::load(project_root).unwrap().is_none());

    // 2. Create and add a pack
    let mut lockfile = Lockfile::new(profile);
    let entry = LockfileEntry::new(
        "io.ggen.rust.cli".to_string(),
        "1.0.0".to_string(),
        RegistrySource::Registry {
            url: "https://registry.ggen.io".to_string(),
        },
        "sha256:abc123".to_string(),
        "local:unsigned".to_string(),
        TrustTier::Experimental,
        vec![],
    );
    lockfile.add_pack(entry);

    // 3. Save lockfile
    lockfile.save(project_root).unwrap();

    // 4. Verify lockfile exists
    assert!(project_root.join(".ggen").join("packs.lock").exists());

    // 5. Load and verify
    let loaded = Lockfile::load(project_root)
        .unwrap()
        .expect("Lockfile should exist");
    assert_eq!(loaded.packs.len(), 1);
    assert_eq!(loaded.packs[0].pack_id, "io.ggen.rust.cli");
    assert!(loaded.verify().unwrap());
}

#[test]
fn test_lockfile_pack_management() {
    let temp_dir = TempDir::new().unwrap();
    let project_root = temp_dir.path();
    let profile = create_test_profile();
    let mut lockfile = Lockfile::new(profile);

    // Add multiple packs
    for i in 1..=3 {
        let entry = LockfileEntry::new(
            format!("pack-{}", i),
            format!("1.0.{}", i),
            RegistrySource::Local {
                path: PathBuf::from(format!("/tmp/pack-{}", i)),
            },
            format!("digest-{}", i),
            "sig".to_string(),
            TrustTier::Experimental,
            vec![],
        );
        lockfile.add_pack(entry);
    }

    assert_eq!(lockfile.packs.len(), 3);
    assert!(lockfile.get_pack("pack-1").is_some());
    assert!(lockfile.get_pack("pack-2").is_some());
    assert!(lockfile.get_pack("pack-3").is_some());

    // Update a pack
    let updated_entry = LockfileEntry::new(
        "pack-2".to_string(),
        "1.1.0".to_string(),
        RegistrySource::Local {
            path: PathBuf::from("/tmp/pack-2-new"),
        },
        "new-digest".to_string(),
        "sig".to_string(),
        TrustTier::Experimental,
        vec![],
    );
    lockfile.add_pack(updated_entry);

    assert_eq!(lockfile.packs.len(), 3);
    assert_eq!(lockfile.get_pack("pack-2").unwrap().version, "1.1.0");

    // Save and check
    lockfile.save(project_root).unwrap();
    let loaded = Lockfile::load(project_root).unwrap().unwrap();
    assert_eq!(loaded.packs.len(), 3);
}
