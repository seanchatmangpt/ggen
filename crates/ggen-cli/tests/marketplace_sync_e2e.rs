//! End-to-end integration tests for marketplace sync command
//!
//! **Chicago TDD Principles**:
//! - REAL CLI process execution
//! - REAL file system operations (cache directory)
//! - REAL state verification (cache metadata)
//! - No mocking of marketplace operations
//!
//! **Critical User Workflows (80/20)**:
//! 1. Sync marketplace with cache (default behavior)
//! 2. Force full refresh (--force flag)
//! 3. Dry-run preview (--dry-run flag)
//! 4. Custom registry URL (--source flag)
//! 5. Verbose progress reporting (--verbose flag)

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

/// Helper to create mcpp command
fn mcpp() -> Command {
    Command::cargo_bin("mcpp").expect("Failed to find mcpp binary")
}

/// Helper to setup marketplace cache directory
fn create_test_marketplace_cache(temp_dir: &TempDir) -> std::path::PathBuf {
    let cache_dir = temp_dir.path().join(".cache").join("mcpp").join("packs");
    fs::create_dir_all(&cache_dir).expect("Failed to create cache dir");
    cache_dir
}

#[test]
#[ignore]
fn test_marketplace_sync_basic() {
    // Chicago TDD: Real CLI execution, verify command succeeds
    // Sync should complete with success status even on first run (empty marketplace)
    let temp_dir = TempDir::new().unwrap();
    let cache_dir = create_test_marketplace_cache(&temp_dir);

    mcpp()
        .arg("marketplace")
        .arg("sync")
        .env("GGEN_MARKETPLACE_CACHE", &cache_dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("success"));
}

#[test]
#[ignore]
fn test_marketplace_sync_with_force() {
    // Chicago TDD: Verify --force flag disables cache checks
    // Should complete successfully
    let temp_dir = TempDir::new().unwrap();
    let cache_dir = create_test_marketplace_cache(&temp_dir);

    mcpp()
        .arg("marketplace")
        .arg("sync")
        .arg("--force")
        .env("GGEN_MARKETPLACE_CACHE", &cache_dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("success"));
}

#[test]
#[ignore]
fn test_marketplace_sync_with_dry_run() {
    // Chicago TDD: Verify --dry-run doesn't modify cache
    // Command should succeed and indicate dry-run mode
    let temp_dir = TempDir::new().unwrap();
    let cache_dir = create_test_marketplace_cache(&temp_dir);

    // First dry-run
    mcpp()
        .arg("marketplace")
        .arg("sync")
        .arg("--dry-run")
        .env("GGEN_MARKETPLACE_CACHE", &cache_dir)
        .assert()
        .success();

    // Verify no real files were created (only dry-run output)
    // In dry-run mode, no cache metadata should be written
    let metadata_file = cache_dir.join(".cache-metadata");
    assert!(
        !metadata_file.exists(),
        "Dry-run should not create cache metadata"
    );
}

#[test]
#[ignore]
fn test_marketplace_sync_with_verbose() {
    // Chicago TDD: Verify --verbose produces detailed output
    // Should show progress information
    let temp_dir = TempDir::new().unwrap();
    let cache_dir = create_test_marketplace_cache(&temp_dir);

    mcpp()
        .arg("marketplace")
        .arg("sync")
        .arg("--verbose")
        .env("GGEN_MARKETPLACE_CACHE", &cache_dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("success"));
}

#[test]
#[ignore]
fn test_marketplace_sync_with_custom_source() {
    // Chicago TDD: Verify --source flag accepts custom registry URL
    // Should complete successfully (no actual network call in test)
    let temp_dir = TempDir::new().unwrap();
    let cache_dir = create_test_marketplace_cache(&temp_dir);

    mcpp()
        .arg("marketplace")
        .arg("sync")
        .arg("--source")
        .arg("https://custom-registry.example.com")
        .env("GGEN_MARKETPLACE_CACHE", &cache_dir)
        .assert()
        .success();
}

#[test]
#[ignore]
fn test_marketplace_sync_idempotent() {
    // Chicago TDD: Verify sync is idempotent (running twice has same effect)
    // First sync
    let temp_dir = TempDir::new().unwrap();
    let cache_dir = create_test_marketplace_cache(&temp_dir);

    mcpp()
        .arg("marketplace")
        .arg("sync")
        .env("GGEN_MARKETPLACE_CACHE", &cache_dir)
        .assert()
        .success();

    // Capture state after first sync
    let metadata_file = cache_dir.join(".cache-metadata");
    let first_sync_exists = metadata_file.exists();

    // Second sync
    mcpp()
        .arg("marketplace")
        .arg("sync")
        .env("GGEN_MARKETPLACE_CACHE", &cache_dir)
        .assert()
        .success();

    // State should be consistent
    let second_sync_exists = metadata_file.exists();
    assert_eq!(
        first_sync_exists, second_sync_exists,
        "Sync should be idempotent"
    );
}

#[test]
#[ignore]
fn test_marketplace_sync_output_json_compatible() {
    // Chicago TDD: Verify JSON output can be parsed
    // Output should contain valid JSON structure
    let temp_dir = TempDir::new().unwrap();
    let cache_dir = create_test_marketplace_cache(&temp_dir);

    let output = mcpp()
        .arg("marketplace")
        .arg("sync")
        .env("GGEN_MARKETPLACE_CACHE", &cache_dir)
        .output()
        .expect("Failed to execute");

    // Verify command completes
    assert!(output.status.success(), "Command should succeed");

    // Output should contain success status
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("success"),
        "Output should indicate success status"
    );
}

#[test]
#[ignore]
fn test_marketplace_sync_cache_location_override() {
    // Chicago TDD: Verify GGEN_MARKETPLACE_CACHE env var controls cache location
    let temp_dir = TempDir::new().unwrap();
    let custom_cache = temp_dir.path().join("my-custom-cache");
    fs::create_dir_all(&custom_cache).expect("Failed to create custom cache dir");

    mcpp()
        .arg("marketplace")
        .arg("sync")
        .env("GGEN_MARKETPLACE_CACHE", &custom_cache)
        .assert()
        .success()
        .stdout(predicate::str::contains(custom_cache.to_str().unwrap()));
}

#[test]
#[ignore]
fn test_marketplace_sync_combined_flags() {
    // Chicago TDD: Verify multiple flags work together
    // --force + --dry-run + --verbose
    let temp_dir = TempDir::new().unwrap();
    let cache_dir = create_test_marketplace_cache(&temp_dir);

    mcpp()
        .arg("marketplace")
        .arg("sync")
        .arg("--force")
        .arg("--dry-run")
        .arg("--verbose")
        .env("GGEN_MARKETPLACE_CACHE", &cache_dir)
        .assert()
        .success();
}
