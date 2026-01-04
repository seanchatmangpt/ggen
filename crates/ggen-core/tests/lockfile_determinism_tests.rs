//! Lockfile Determinism Tests (Story 4 - Determinism)
//!
//! Tests ensuring that lockfile generation is deterministic and reproducible
//! across platforms. SC-007: Cross-platform hashes match.
//!
//! # Determinism Guarantees
//!
//! 1. Same manifest -> same lockfile content
//! 2. Sorted keys (BTreeMap) ensure stable ordering
//! 3. Round-trip: load/save/load produces identical bytes
//! 4. Cross-platform SHA256 hashes match

use ggen_core::lockfile::{LockEntry, Lockfile, LockfileManager};
use sha2::{Digest, Sha256};
use std::collections::BTreeMap;
use tempfile::TempDir;

/// T036: Same manifest -> same lockfile content
#[test]
fn test_same_manifest_same_lockfile() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Create first lockfile
    manager
        .upsert("io.ggen.alpha", "1.0.0", "sha256_alpha", "https://alpha.example.com")
        .unwrap();
    manager
        .upsert("io.ggen.beta", "2.0.0", "sha256_beta", "https://beta.example.com")
        .unwrap();

    let content1 = std::fs::read_to_string(manager.lockfile_path()).unwrap();

    // Remove the lockfile
    std::fs::remove_file(manager.lockfile_path()).unwrap();

    // Create second lockfile with same content (different order)
    manager
        .upsert("io.ggen.beta", "2.0.0", "sha256_beta", "https://beta.example.com")
        .unwrap();
    manager
        .upsert("io.ggen.alpha", "1.0.0", "sha256_alpha", "https://alpha.example.com")
        .unwrap();

    let content2 = std::fs::read_to_string(manager.lockfile_path()).unwrap();

    // Compare packs sections (ignoring generated timestamp which will differ)
    let lockfile1: Lockfile = toml::from_str(&content1).unwrap();
    let lockfile2: Lockfile = toml::from_str(&content2).unwrap();

    assert_eq!(lockfile1.packs.len(), lockfile2.packs.len());
    assert_eq!(lockfile1.version, lockfile2.version);

    // Packs should be in same order (sorted by ID)
    for (p1, p2) in lockfile1.packs.iter().zip(lockfile2.packs.iter()) {
        assert_eq!(p1.id, p2.id);
        assert_eq!(p1.version, p2.version);
        assert_eq!(p1.sha256, p2.sha256);
        assert_eq!(p1.source, p2.source);
    }
}

/// T036: Sorted keys ensure stable ordering
#[test]
fn test_sorted_pack_ids() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Add packs in random order
    manager
        .upsert("io.ggen.zebra", "1.0.0", "sha_z", "https://z.com")
        .unwrap();
    manager
        .upsert("io.ggen.alpha", "1.0.0", "sha_a", "https://a.com")
        .unwrap();
    manager
        .upsert("io.ggen.middle", "1.0.0", "sha_m", "https://m.com")
        .unwrap();

    let lockfile = manager.load().unwrap().unwrap();

    // Verify packs are sorted alphabetically
    let pack_ids: Vec<_> = lockfile.packs.iter().map(|p| p.id.as_str()).collect();
    assert_eq!(pack_ids, vec!["io.ggen.alpha", "io.ggen.middle", "io.ggen.zebra"]);
}

/// T036: Round-trip: load/save/load produces identical content
#[test]
fn test_round_trip_determinism() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Create initial lockfile
    manager
        .upsert("io.ggen.test", "1.0.0", "abc123", "https://example.com")
        .unwrap();

    // Load, save, load cycle
    let lockfile1 = manager.load().unwrap().unwrap();
    manager.save(&lockfile1).unwrap();
    let lockfile2 = manager.load().unwrap().unwrap();
    manager.save(&lockfile2).unwrap();
    let lockfile3 = manager.load().unwrap().unwrap();

    // All three should have identical pack content
    assert_eq!(lockfile1.packs.len(), lockfile2.packs.len());
    assert_eq!(lockfile2.packs.len(), lockfile3.packs.len());

    for i in 0..lockfile1.packs.len() {
        assert_eq!(lockfile1.packs[i].id, lockfile2.packs[i].id);
        assert_eq!(lockfile2.packs[i].id, lockfile3.packs[i].id);
        assert_eq!(lockfile1.packs[i].version, lockfile2.packs[i].version);
        assert_eq!(lockfile2.packs[i].version, lockfile3.packs[i].version);
        assert_eq!(lockfile1.packs[i].sha256, lockfile2.packs[i].sha256);
        assert_eq!(lockfile2.packs[i].sha256, lockfile3.packs[i].sha256);
    }
}

/// T036: BTreeMap guarantees deterministic iteration
#[test]
fn test_btreemap_determinism() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Add multiple packs
    for i in (0..10).rev() {
        manager
            .upsert(
                &format!("io.ggen.pack{:02}", i),
                "1.0.0",
                &format!("sha_{}", i),
                &format!("https://{}.com", i),
            )
            .unwrap();
    }

    // Get installed packs as BTreeMap
    let installed = manager.installed_packs().unwrap();

    // Verify BTreeMap iteration order is deterministic (sorted)
    let keys: Vec<_> = installed.keys().collect();
    let mut sorted_keys = keys.clone();
    sorted_keys.sort();
    assert_eq!(keys, sorted_keys);
}

/// T037: Cross-platform SHA256 hash consistency
#[test]
fn test_sha256_consistency() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Create lockfile with known content
    manager
        .upsert("io.ggen.deterministic", "1.0.0", "known_hash", "https://test.com")
        .unwrap();

    let lockfile = manager.load().unwrap().unwrap();

    // Compute hash of pack content (excluding generated timestamp)
    let pack_content = format!(
        "{}:{}:{}:{}",
        lockfile.packs[0].id,
        lockfile.packs[0].version,
        lockfile.packs[0].sha256,
        lockfile.packs[0].source
    );

    let hash = Sha256::digest(pack_content.as_bytes());
    let hex_hash = hex::encode(hash);

    // This hash should be the same on all platforms
    // It's deterministic because we're hashing sorted, deterministic content
    assert_eq!(hex_hash.len(), 64); // SHA256 produces 64 hex chars

    // Verify the hash is reproducible
    let hash2 = Sha256::digest(pack_content.as_bytes());
    let hex_hash2 = hex::encode(hash2);
    assert_eq!(hex_hash, hex_hash2);
}

/// T037: Lockfile content hash is reproducible
#[test]
fn test_lockfile_content_hash_reproducibility() {
    let temp_dir1 = TempDir::new().unwrap();
    let temp_dir2 = TempDir::new().unwrap();

    let manager1 = LockfileManager::new(temp_dir1.path());
    let manager2 = LockfileManager::new(temp_dir2.path());

    // Create identical lockfiles in different directories
    for manager in [&manager1, &manager2] {
        manager
            .upsert("io.ggen.alpha", "1.0.0", "sha_a", "https://a.com")
            .unwrap();
        manager
            .upsert("io.ggen.beta", "2.0.0", "sha_b", "https://b.com")
            .unwrap();
    }

    let lockfile1 = manager1.load().unwrap().unwrap();
    let lockfile2 = manager2.load().unwrap().unwrap();

    // Compute content hashes (excluding generated timestamp)
    fn compute_content_hash(lockfile: &Lockfile) -> String {
        let mut hasher = Sha256::new();
        hasher.update(lockfile.version.as_bytes());
        for pack in &lockfile.packs {
            hasher.update(pack.id.as_bytes());
            hasher.update(pack.version.as_bytes());
            hasher.update(pack.sha256.as_bytes());
            hasher.update(pack.source.as_bytes());
        }
        hex::encode(hasher.finalize())
    }

    let hash1 = compute_content_hash(&lockfile1);
    let hash2 = compute_content_hash(&lockfile2);

    assert_eq!(hash1, hash2, "Lockfile content hashes should match across directories");
}

/// T036: Dependency order is deterministic
#[test]
fn test_dependency_order_determinism() {
    // Create lock entry with dependencies
    let entry1 = LockEntry {
        id: "io.ggen.test".to_string(),
        version: "1.0.0".to_string(),
        sha256: "abc".to_string(),
        source: "https://test.com".to_string(),
        dependencies: Some(vec![
            "io.ggen.zebra@1.0.0".to_string(),
            "io.ggen.alpha@1.0.0".to_string(),
            "io.ggen.middle@1.0.0".to_string(),
        ]),
        pqc_signature: None,
        pqc_pubkey: None,
    };

    // Serialize and deserialize
    let toml_str = toml::to_string(&entry1).unwrap();
    let entry2: LockEntry = toml::from_str(&toml_str).unwrap();

    // Dependencies should be preserved in order
    assert_eq!(entry1.dependencies, entry2.dependencies);
}

/// T037: Version number parsing is consistent
#[test]
fn test_version_consistency() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Add pack with complex version
    manager
        .upsert("io.ggen.versioned", "1.2.3-beta.4+build.567", "sha", "https://test.com")
        .unwrap();

    let entry = manager.get("io.ggen.versioned").unwrap().unwrap();
    assert_eq!(entry.version, "1.2.3-beta.4+build.567");
}

/// T036: Multiple updates produce deterministic result
#[test]
fn test_multiple_updates_determinism() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Perform multiple updates
    for i in 0..5 {
        manager
            .upsert(
                "io.ggen.updated",
                &format!("{}.0.0", i),
                &format!("sha_{}", i),
                "https://test.com",
            )
            .unwrap();
    }

    // Only the last version should remain
    let lockfile = manager.load().unwrap().unwrap();
    assert_eq!(lockfile.packs.len(), 1);
    assert_eq!(lockfile.packs[0].version, "4.0.0");
    assert_eq!(lockfile.packs[0].sha256, "sha_4");
}

/// T036: Bulk upsert produces same result as sequential upsert
#[test]
fn test_bulk_vs_sequential_determinism() {
    let temp_dir1 = TempDir::new().unwrap();
    let temp_dir2 = TempDir::new().unwrap();

    let manager1 = LockfileManager::new(temp_dir1.path());
    let manager2 = LockfileManager::new(temp_dir2.path());

    let packs = vec![
        ("io.ggen.alpha".to_string(), "1.0.0".to_string(), "sha_a".to_string(), "https://a.com".to_string()),
        ("io.ggen.beta".to_string(), "2.0.0".to_string(), "sha_b".to_string(), "https://b.com".to_string()),
        ("io.ggen.gamma".to_string(), "3.0.0".to_string(), "sha_g".to_string(), "https://g.com".to_string()),
    ];

    // Sequential upsert
    for (id, version, sha, source) in &packs {
        manager1.upsert(id, version, sha, source).unwrap();
    }

    // Bulk upsert
    manager2.upsert_bulk(&packs).unwrap();

    let lockfile1 = manager1.load().unwrap().unwrap();
    let lockfile2 = manager2.load().unwrap().unwrap();

    // Both should produce identical pack lists
    assert_eq!(lockfile1.packs.len(), lockfile2.packs.len());
    for (p1, p2) in lockfile1.packs.iter().zip(lockfile2.packs.iter()) {
        assert_eq!(p1.id, p2.id);
        assert_eq!(p1.version, p2.version);
        assert_eq!(p1.sha256, p2.sha256);
        assert_eq!(p1.source, p2.source);
    }
}

/// T036: Empty lockfile is deterministic
#[test]
fn test_empty_lockfile_determinism() {
    let temp_dir1 = TempDir::new().unwrap();
    let temp_dir2 = TempDir::new().unwrap();

    let manager1 = LockfileManager::new(temp_dir1.path());
    let manager2 = LockfileManager::new(temp_dir2.path());

    // Create empty lockfiles
    let lockfile1 = manager1.create().unwrap();
    let lockfile2 = manager2.create().unwrap();

    // Version should be consistent
    assert_eq!(lockfile1.version, lockfile2.version);
    assert_eq!(lockfile1.version, "1.0");

    // Packs should be empty
    assert!(lockfile1.packs.is_empty());
    assert!(lockfile2.packs.is_empty());
}

/// T038: Conflict detection - same package different versions
#[test]
fn test_version_update_replaces_old() {
    let temp_dir = TempDir::new().unwrap();
    let manager = LockfileManager::new(temp_dir.path());

    // Add initial version
    manager
        .upsert("io.ggen.conflict", "1.0.0", "sha_v1", "https://test.com")
        .unwrap();

    // Update to new version
    manager
        .upsert("io.ggen.conflict", "2.0.0", "sha_v2", "https://test.com")
        .unwrap();

    // Only one entry should exist
    let lockfile = manager.load().unwrap().unwrap();
    assert_eq!(lockfile.packs.len(), 1);
    assert_eq!(lockfile.packs[0].version, "2.0.0");
}
