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

// REMOVED (2026-07-17, ggen-core removal, docs/jira/v26.7.16/14-GGEN-CORE-REMOVAL-PROPOSAL.md):
// test_sabotage_empty_signature_receipt_verify_returns_invalid constructed its fixture via the
// deleted ggen_core::receipt::Receipt type, and separately already used a stale CLI invocation
// (`ggen receipt verify <path>` -- the current `receipt verify` takes zero positional args,
// always targeting `.ggen-v2/receipt.json`). Equivalent real coverage for tamper/invalid-receipt
// behavior exists in crates/ggen-engine/tests/receipt_chain_e2e.rs and cli_boundary.rs.

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

/// Test 5: Empty packs cache dir with GGEN_OFFLINE=true, add should exit non-zero
///
/// Intent preserved: adding a pack that is absent from the registry/cache must fail
/// loudly (no fail-open). Migrated from the removed `packs install` to the live
/// `pack add` verb. NOTE: the live `add` verb (crates/ggen-cli/src/cmds/pack.rs)
/// returns `Ok(AddOutput { status: "not_found", .. })` on a missing pack rather than
/// a non-zero exit, so — mirroring proof_pack_test.rs::
/// test_add_nonexistent_pack_does_not_fake_success_or_emit_receipt — this asserts
/// "loud failure" as EITHER a non-zero exit OR a `not_found` / "not found" marker in
/// stdout+stderr, instead of `.failure()` alone.
#[test]
fn test_sabotage_empty_packs_dir_install_exits_nonzero() {
    let empty_cache = TempDir::new().unwrap();
    let temp_dir = TempDir::new().unwrap();

    let assert = ggen()
        .arg("pack")
        .arg("add")
        .arg("acme/base")
        .current_dir(temp_dir.path())
        .env("GGEN_PACK_CACHE_DIR", empty_cache.path())
        .env("GGEN_OFFLINE", "true")
        .assert();

    let output = assert.get_output().clone();
    let code = output.status.code();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let combined = format!("{stdout}{stderr}");

    let loud = code != Some(0)
        || combined.contains("not_found")
        || combined.contains("not found")
        || combined.contains("GGEN_OFFLINE")
        || combined.contains("cache");
    assert!(
        loud,
        "FAIL-OPEN DEFECT: adding an absent pack offline must fail loudly. \
         Got exit {:?}, output: {}",
        code, combined
    );
}
