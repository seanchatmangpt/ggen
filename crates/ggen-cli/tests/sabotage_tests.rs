#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
#![allow(
    dead_code,
    unused_imports,
    unused_variables,
    deprecated,
    clippy::all,
    unused_mut
)]

//! Sabotage Tests for Lockfile + Receipt Invariants
//!
//! These tests verify that the sync command correctly enforces invariants by
//! sabotaging preconditions and confirming hard failures (non-zero exit codes).
//!
//! Following Chicago TDD: no mocks, real collaborators, state-based verification.

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("ggen binary not found")
}

/// Test 1: Remove pack TOML after install, sync --locked should exit non-zero
#[test]
fn test_sabotage_remove_pack_toml_sync_locked_exits_nonzero() {
    let temp_dir = TempDir::new().unwrap();
    let lock_path = temp_dir.path().join(".ggen").join("packs.lock");
    fs::create_dir_all(lock_path.parent().unwrap()).unwrap();

    // Write a lockfile that references a pack with a specific integrity hash
    // The pack will not actually exist (simulating deletion after install)
    let lockfile_json = r#"{"packs":{"acme/base":{"version":"1.0.0","source":{"type":"Local","path":"/nonexistent/pack"},"integrity":"sha256-abc123def456","installed_at":"2024-01-01T00:00:00Z","dependencies":[]}},"updated_at":"2024-01-01T00:00:00Z","ggen_version":"6.1.0"}"#;
    fs::write(&lock_path, lockfile_json).unwrap();

    let cache_dir = TempDir::new().unwrap();

    ggen()
        .arg("sync")
        .arg("--locked")
        .current_dir(temp_dir.path())
        .env("GGEN_PACK_CACHE_DIR", cache_dir.path())
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("digest")
                .or(predicate::str::contains("not found"))
                .or(predicate::str::contains("integrity")),
        );
}

/// Test 2: Corrupt lockfile JSON, sync --locked should exit non-zero
#[test]
fn test_sabotage_corrupt_lockfile_sync_locked_exits_nonzero() {
    let temp_dir = TempDir::new().unwrap();
    let lock_path = temp_dir.path().join(".ggen").join("packs.lock");
    fs::create_dir_all(lock_path.parent().unwrap()).unwrap();

    // Write garbage JSON that will fail to parse
    fs::write(&lock_path, "this is not JSON {{{{{").unwrap();

    ggen()
        .arg("sync")
        .arg("--locked")
        .current_dir(temp_dir.path())
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("Invalid")
                .or(predicate::str::contains("parse"))
                .or(predicate::str::contains("JSON")),
        );
}

/// Test 3: Empty signature in receipt, verify should return is_valid:false
#[test]
fn test_sabotage_empty_signature_receipt_verify_returns_invalid() {
    use ggen_core::receipt::{generate_keypair, Receipt};

    let temp_dir = TempDir::new().unwrap();
    let receipt_path = temp_dir.path().join("receipt.json");

    // Create a receipt without signing (signature field will be empty string)
    let receipt = Receipt::new(
        "test-operation-id".to_string(),
        vec!["sha256:input-hash".to_string()],
        vec!["sha256:output-hash".to_string()],
        None,
    );

    let receipt_json = serde_json::to_string_pretty(&receipt).unwrap();
    fs::write(&receipt_path, receipt_json).unwrap();

    ggen()
        .arg("receipt")
        .arg("verify")
        .arg(&receipt_path)
        .current_dir(temp_dir.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("\"is_valid\":false"));
}

/// Test 4: Delete verifying key, receipt verify should return is_valid:false
#[test]
fn test_sabotage_delete_verifying_key_receipt_verify_returns_invalid() {
    let temp_dir = TempDir::new().unwrap();
    let keys_dir = temp_dir.path().join(".ggen").join("keys");
    fs::create_dir_all(&keys_dir).unwrap();

    // The key directory exists but verifying.key is absent
    // ggen receipt verify will look for .ggen/keys/verifying.key relative to cwd
    // With missing key, verification must return is_valid:false

    ggen()
        .arg("receipt")
        .arg("verify")
        .arg(".ggen/receipts/latest.json")
        .current_dir(temp_dir.path())
        .assert()
        .success()
        .stdout(predicate::str::contains("\"is_valid\":false"));
}

/// Test 5: Empty packs cache dir with GGEN_OFFLINE=true, install should exit non-zero
#[test]
fn test_sabotage_empty_packs_dir_install_exits_nonzero() {
    let empty_cache = TempDir::new().unwrap();
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .arg("packs")
        .arg("install")
        .arg("acme/base")
        .current_dir(temp_dir.path())
        .env("GGEN_PACK_CACHE_DIR", empty_cache.path())
        .env("GGEN_OFFLINE", "true")
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("not found")
                .or(predicate::str::contains("GGEN_OFFLINE"))
                .or(predicate::str::contains("cache")),
        );
}
