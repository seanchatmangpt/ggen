//! T3 Layer: Refusal Tests — Negative-Path Verification
//!
//! Validates that the system loudly refuses invalid states:
//! - SAB-01: Corrupt packs.lock → sync --locked fails
//! - SAB-02: Empty receipt signature → receipt verify fails
//! - SAB-03: Missing pack directory → pack add fails

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("find ggen binary")
}

/// SAB-01: Corrupt packs.lock with garbage JSON → sync --locked fails hard
/// failure_class: contract_drift (lockfile is invalid)
#[test]
#[ignore]
fn sab_01_corrupt_packs_lock_garbage_json() {
    // Arrange: Create temp dir with corrupt .ggen/packs.lock
    let temp = TempDir::new().expect("create temp dir");
    let ggen_dir = temp.path().join(".ggen");
    fs::create_dir_all(&ggen_dir).expect("create .ggen dir");

    // Write garbage JSON to packs.lock
    let packs_lock = ggen_dir.join("packs.lock");
    fs::write(&packs_lock, "{this is not valid json at all}").expect("write corrupt packs.lock");

    // Act: Run sync --locked (should fail because lockfile is corrupted)
    let mut cmd = ggen();
    cmd.current_dir(temp.path()).arg("sync").arg("--locked");

    // Assert: Command fails with clear error about JSON/parse
    cmd.assert().failure().stderr(
        predicate::str::contains("JSON")
            .or(predicate::str::contains("parse"))
            .or(predicate::str::contains("invalid"))
            .or(predicate::str::contains("packs.lock")),
    );
}

/// SAB-01b: Truncate packs.lock mid-JSON → sync --locked fails hard
/// failure_class: contract_drift (lockfile incomplete)
#[test]
#[ignore]
fn sab_01b_truncate_packs_lock_mid_json() {
    // Arrange: Create temp dir with truncated .ggen/packs.lock (mid-JSON)
    let temp = TempDir::new().expect("create temp dir");
    let ggen_dir = temp.path().join(".ggen");
    fs::create_dir_all(&ggen_dir).expect("create .ggen dir");

    // Write truncated JSON (incomplete structure)
    let packs_lock = ggen_dir.join("packs.lock");
    fs::write(&packs_lock, r#"{"packages": [{"id": "acme/base""#)
        .expect("write truncated packs.lock");

    // Act: Run sync --locked (should fail due to malformed JSON)
    let mut cmd = ggen();
    cmd.current_dir(temp.path()).arg("sync").arg("--locked");

    // Assert: Command fails with clear error about malformed JSON
    cmd.assert().failure().stderr(
        predicate::str::contains("JSON")
            .or(predicate::str::contains("parse"))
            .or(predicate::str::contains("EOF"))
            .or(predicate::str::contains("invalid")),
    );
}

/// SAB-02: Empty receipt signature → receipt verification considers it invalid
/// failure_class: contract_drift (receipt has no proof)
#[test]
#[ignore]
fn sab_02_empty_receipt_signature() {
    // Arrange: Create temp dir with init, then corrupt receipt signature
    let temp = TempDir::new().expect("create temp dir");
    let ggen_dir = temp.path().join(".ggen");
    fs::create_dir_all(&ggen_dir).expect("create .ggen dir");

    let receipts_dir = ggen_dir.join("receipts");
    fs::create_dir_all(&receipts_dir).expect("create receipts dir");

    // Create a fake receipt with empty signature
    let receipt_json = r#"{
  "operation_id": "00000000-0000-0000-0000-000000000000",
  "timestamp": "2026-05-29T00:00:00Z",
  "input_hashes": {},
  "output_hashes": {},
  "signature": ""
}"#;
    let receipt_path = receipts_dir.join("latest.json");
    fs::write(&receipt_path, receipt_json).expect("write receipt with empty signature");

    // Act: Run receipt verify (should fail or indicate invalid)
    let mut cmd = ggen();
    cmd.current_dir(temp.path()).arg("receipt").arg("verify");

    // Assert: Command fails due to empty signature (no proof)
    cmd.assert().failure().stderr(
        predicate::str::contains("signature")
            .or(predicate::str::contains("invalid"))
            .or(predicate::str::contains("verify"))
            .or(predicate::str::contains("empty")),
    );
}

/// SAB-03: pack add nonexistent/pack → pack add fails (pack not found)
/// failure_class: fail_open (system should reject missing pack)
#[test]
#[ignore]
fn sab_03_pack_add_nonexistent() {
    // Arrange: Create temp dir with valid ggen.toml
    let temp = TempDir::new().expect("create temp dir");
    let ggen_toml = temp.path().join("ggen.toml");
    fs::write(
        &ggen_toml,
        r#"[package]
name = "test-project"
version = "0.1.0"
"#,
    )
    .expect("write ggen.toml");

    // Act: Try to add a pack that doesn't exist
    let mut cmd = ggen();
    cmd.current_dir(temp.path())
        .arg("packs")
        .arg("add")
        .arg("nonexistent/pack");

    // Assert: Command fails with clear error about pack not found
    cmd.assert().failure().stderr(
        predicate::str::contains("not found")
            .or(predicate::str::contains("pack"))
            .or(predicate::str::contains("nonexistent"))
            .or(predicate::str::contains("does not exist")),
    );
}

/// SAB-04: Delete ggen.toml after init → doctor fails (missing manifest)
/// failure_class: fail_open (system should reject missing config)
#[test]
#[ignore]
fn sab_04_missing_ggen_toml_doctor_fails() {
    // Arrange: Create temp dir, init, then delete ggen.toml
    let temp = TempDir::new().expect("create temp dir");
    let ggen_toml = temp.path().join("ggen.toml");

    // Write a valid ggen.toml
    fs::write(
        &ggen_toml,
        r#"[package]
name = "test-project"
version = "0.1.0"
"#,
    )
    .expect("write initial ggen.toml");

    // Corrupt by deleting ggen.toml (simulate corruption/missing file)
    fs::remove_file(&ggen_toml).expect("delete ggen.toml");

    // Act: Run doctor command (should fail because manifest is missing)
    let mut cmd = ggen();
    cmd.current_dir(temp.path()).arg("doctor");

    // Assert: Command fails with clear error about missing config/manifest
    cmd.assert().failure().stderr(
        predicate::str::contains("manifest")
            .or(predicate::str::contains("config"))
            .or(predicate::str::contains("ggen.toml"))
            .or(predicate::str::contains("not found"))
            .or(predicate::str::contains("No such file")),
    );
}
