//! Chicago TDD integration tests for `manifest_cache::ManifestCache` and
//! `manifest_cache::hash_file`.
//!
//! All tests use real filesystem I/O via `TempDir`.  No mocks or test doubles.

use std::fs;
use tempfile::TempDir;

use ggen_daemon::manifest_cache::{hash_file, ManifestCache};

// ---------------------------------------------------------------------------
// hash_file
// ---------------------------------------------------------------------------

#[test]
fn hash_file_returns_some_for_existing_file() {
    let dir = TempDir::new().unwrap();
    let path = dir.path().join("manifest.toml");
    fs::write(&path, b"[manifest]\nname = \"test\"").unwrap();

    let hash = hash_file(&path);
    assert!(hash.is_some(), "hash_file must return Some for a readable file");
}

#[test]
fn hash_file_is_deterministic() {
    let dir = TempDir::new().unwrap();
    let path = dir.path().join("manifest.toml");
    fs::write(&path, b"stable content").unwrap();

    let h1 = hash_file(&path).expect("first hash must succeed");
    let h2 = hash_file(&path).expect("second hash must succeed");
    assert_eq!(h1, h2, "same content must produce the same hash");
}

#[test]
fn hash_file_returns_none_for_missing_file() {
    let dir = TempDir::new().unwrap();
    let missing = dir.path().join("nonexistent.toml");
    assert!(!missing.exists());

    assert_eq!(
        hash_file(&missing),
        None,
        "hash_file must return None for a missing file"
    );
}

#[test]
fn hash_changes_when_file_content_changes() {
    let dir = TempDir::new().unwrap();
    let path = dir.path().join("manifest.toml");
    fs::write(&path, b"version = 1").unwrap();

    let h1 = hash_file(&path).expect("first hash");
    fs::write(&path, b"version = 2").unwrap();
    let h2 = hash_file(&path).expect("second hash");

    assert_ne!(h1, h2, "different content must produce different hashes");
}

#[test]
fn hash_is_64_hex_chars_sha256() {
    let dir = TempDir::new().unwrap();
    let path = dir.path().join("m.toml");
    fs::write(&path, b"data").unwrap();

    let hash = hash_file(&path).unwrap();
    assert_eq!(hash.len(), 64, "SHA-256 hex digest must be 64 characters");
    assert!(
        hash.chars().all(|c| c.is_ascii_hexdigit()),
        "hash must contain only hex digits"
    );
}

// ---------------------------------------------------------------------------
// ManifestCache::read
// ---------------------------------------------------------------------------

#[test]
fn read_returns_none_when_cache_file_absent() {
    let dir = TempDir::new().unwrap();
    // No .ggen/last-applied.json created
    let result = ManifestCache::read(dir.path());
    assert!(
        result.is_none(),
        "read must return None when no cache file exists"
    );
}

#[test]
fn read_returns_none_on_corrupt_json() {
    let dir = TempDir::new().unwrap();
    let ggen_dir = dir.path().join(".ggen");
    fs::create_dir_all(&ggen_dir).unwrap();
    fs::write(ggen_dir.join("last-applied.json"), b"not valid json {{{").unwrap();

    let result = ManifestCache::read(dir.path());
    assert!(
        result.is_none(),
        "read must return None for malformed JSON"
    );
}

// ---------------------------------------------------------------------------
// ManifestCache::write
// ---------------------------------------------------------------------------

#[test]
fn write_creates_dot_ggen_directory_automatically() {
    let dir = TempDir::new().unwrap();
    // .ggen/ does not exist yet
    assert!(!dir.path().join(".ggen").exists());

    let entry = ManifestCache {
        manifest_hash: "abc123".to_owned(),
        commit_sha: "deadbeef".to_owned(),
        applied_at: "2026-06-24T00:00:00Z".to_owned(),
    };
    entry.write(dir.path()).expect("write must succeed");

    assert!(
        dir.path().join(".ggen").join("last-applied.json").exists(),
        ".ggen/last-applied.json must be created"
    );
}

#[test]
fn write_then_read_round_trip_preserves_all_fields() {
    let dir = TempDir::new().unwrap();
    let original = ManifestCache {
        manifest_hash: "sha256hex".to_owned(),
        commit_sha: "cafebabe".to_owned(),
        applied_at: "2026-06-24T12:34:56Z".to_owned(),
    };
    original.write(dir.path()).expect("write must succeed");

    let loaded = ManifestCache::read(dir.path()).expect("read must succeed after write");
    assert_eq!(loaded.manifest_hash, original.manifest_hash);
    assert_eq!(loaded.commit_sha, original.commit_sha);
    assert_eq!(loaded.applied_at, original.applied_at);
}

#[test]
fn write_is_idempotent_second_write_overwrites_first() {
    let dir = TempDir::new().unwrap();

    let first = ManifestCache {
        manifest_hash: "hash-v1".to_owned(),
        commit_sha: "sha-v1".to_owned(),
        applied_at: "2026-06-24T00:00:00Z".to_owned(),
    };
    first.write(dir.path()).expect("first write");

    let second = ManifestCache {
        manifest_hash: "hash-v2".to_owned(),
        commit_sha: "sha-v2".to_owned(),
        applied_at: "2026-06-24T01:00:00Z".to_owned(),
    };
    second.write(dir.path()).expect("second write");

    let loaded = ManifestCache::read(dir.path()).expect("read after two writes");
    assert_eq!(
        loaded.manifest_hash, "hash-v2",
        "second write must overwrite the first"
    );
}

// ---------------------------------------------------------------------------
// Integration: hash_file + ManifestCache round-trip mirrors the dispatch flow
// ---------------------------------------------------------------------------

#[test]
fn cache_hit_detected_when_manifest_unchanged() {
    let repo_dir = TempDir::new().unwrap();
    let manifest_dir = TempDir::new().unwrap();
    let manifest_path = manifest_dir.path().join("ggen.toml");
    fs::write(&manifest_path, b"[spec]\nversion = 1\n").unwrap();

    let hash = hash_file(&manifest_path).expect("manifest must be hashable");

    // Simulate a previous successful dispatch writing the cache
    let entry = ManifestCache {
        manifest_hash: hash.clone(),
        commit_sha: "initial-commit".to_owned(),
        applied_at: "2026-06-24T00:00:00Z".to_owned(),
    };
    entry.write(repo_dir.path()).expect("cache write");

    // Now simulate the next dispatch: hash the manifest and check the cache
    let new_hash = hash_file(&manifest_path).expect("re-hash");
    let cached = ManifestCache::read(repo_dir.path()).expect("cache must be readable");

    assert_eq!(
        new_hash, cached.manifest_hash,
        "unchanged manifest must produce a cache hit"
    );
}

#[test]
fn cache_miss_detected_when_manifest_changes() {
    let repo_dir = TempDir::new().unwrap();
    let manifest_dir = TempDir::new().unwrap();
    let manifest_path = manifest_dir.path().join("ggen.toml");
    fs::write(&manifest_path, b"[spec]\nversion = 1\n").unwrap();

    let original_hash = hash_file(&manifest_path).expect("original hash");
    let entry = ManifestCache {
        manifest_hash: original_hash,
        commit_sha: "old-commit".to_owned(),
        applied_at: "2026-06-24T00:00:00Z".to_owned(),
    };
    entry.write(repo_dir.path()).expect("cache write");

    // Manifest is updated on disk
    fs::write(&manifest_path, b"[spec]\nversion = 2\n").unwrap();
    let new_hash = hash_file(&manifest_path).expect("new hash");

    let cached = ManifestCache::read(repo_dir.path()).expect("cache readable");
    assert_ne!(
        new_hash, cached.manifest_hash,
        "changed manifest must produce a cache miss"
    );
}
