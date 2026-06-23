//! End-to-End CLI workflow tests for the `packs` and `capability` nouns.
//!
//! These exercise the lockfile-oriented multi-pack project workflow an agent
//! drives to bring a project up, through the REAL `ggen` binary against a real
//! hermetic project dir (`assert_cmd` + `tempfile`), asserting on the JSON each
//! verb emits AND on the durable state (`.ggen/packs.lock`, signed receipts).
//! No mocks.
//!
//! Surfaces under test:
//!   - `ggen packs install --pack-id <id>`  → lockfile entry + signed receipt
//!   - `ggen packs list / validate / show`
//!   - `ggen capability enable <surface> [--projection <p>] / list / inspect`
//!
//! Gated behind the `integration` feature (heavy: spawns the binary many times):
//!   cargo test -p ggen-cli-lib --features integration --test e2e_pack_workflow_test

#![cfg(feature = "integration")]
#![allow(clippy::unwrap_used, clippy::expect_used)]

use assert_cmd::Command;
use serde_json::Value;
use std::fs;
use std::path::Path;
use tempfile::TempDir;

// ── helpers ─────────────────────────────────────────────────────────────────

/// A `ggen` command rooted in the given project dir, with a hermetic `$HOME`.
fn ggen_in(temp: &TempDir) -> Command {
    let mut cmd = Command::cargo_bin("ggen").expect("ggen binary");
    cmd.current_dir(temp.path()).env("HOME", temp.path()).env(
        "GGEN_PACK_CACHE_DIR",
        temp.path().join(".ggen").join("packs"),
    );
    cmd
}

/// Run a command, assert success, parse stdout as JSON.
fn run_json(mut cmd: Command, args: &[&str]) -> Value {
    for a in args {
        cmd.arg(a);
    }
    cmd.arg("--format").arg("json");
    let out = cmd.output().expect("run ggen");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        out.status.success(),
        "command {args:?} failed: status={:?}\nstdout={stdout}\nstderr={stderr}",
        out.status.code()
    );
    serde_json::from_str(stdout.trim())
        .unwrap_or_else(|e| panic!("stdout is not JSON ({e}): {stdout}"))
}

fn lock_path(temp: &TempDir) -> std::path::PathBuf {
    temp.path().join(".ggen").join("packs.lock")
}

fn receipt_json_files(temp: &TempDir) -> Vec<std::path::PathBuf> {
    let dir = temp.path().join(".ggen").join("receipts");
    match fs::read_dir(&dir) {
        Ok(rd) => rd
            .filter_map(|e| e.ok())
            .map(|e| e.path())
            .filter(|p| p.extension().and_then(|s| s.to_str()) == Some("json"))
            .collect(),
        Err(_) => Vec::new(),
    }
}

fn count_lockfile_packs(temp: &TempDir) -> usize {
    let content = fs::read_to_string(lock_path(temp)).unwrap();
    let json: Value = serde_json::from_str(&content).unwrap();
    json["packs"].as_object().map(|o| o.len()).unwrap_or(0)
}

// ── packs install ───────────────────────────────────────────────────────────

#[test]
fn packs_install_creates_lockfile_with_tracked_pack() {
    let temp = TempDir::new().unwrap();
    let out = run_json(
        ggen_in(&temp),
        &["packs", "install", "--pack-id", "surface-mcp"],
    );

    assert_eq!(out["pack_id"], "surface-mcp");
    assert!(out.get("status").is_some(), "must report a status: {out}");

    // Durable: lockfile exists, has valid structure, tracks the pack.
    assert!(lock_path(&temp).exists(), "lockfile must be created");
    let lf: Value = serde_json::from_str(&fs::read_to_string(lock_path(&temp)).unwrap()).unwrap();
    assert!(lf.get("packs").is_some() && lf.get("ggen_version").is_some());
    assert!(
        lf["packs"].get("surface-mcp").is_some(),
        "lockfile must track surface-mcp: {lf}"
    );
}

#[test]
fn packs_install_emits_a_valid_signed_receipt() {
    let temp = TempDir::new().unwrap();
    run_json(
        ggen_in(&temp),
        &["packs", "install", "--pack-id", "surface-mcp"],
    );

    let receipts = receipt_json_files(&temp);
    assert!(!receipts.is_empty(), "install must emit a receipt");

    let r: Value = serde_json::from_str(&fs::read_to_string(&receipts[0]).unwrap()).unwrap();
    for field in [
        "operation_id",
        "timestamp",
        "input_hashes",
        "output_hashes",
        "signature",
    ] {
        assert!(r.get(field).is_some(), "receipt must have {field}: {r}");
    }
    assert!(
        !r["signature"].as_str().unwrap_or("").is_empty(),
        "receipt signature must be non-empty"
    );
}

#[test]
fn packs_install_unknown_pack_is_tracked_not_rejected() {
    // Lenient: an id absent from any registry is recorded as a declared dep.
    let temp = TempDir::new().unwrap();
    let out = run_json(
        ggen_in(&temp),
        &["packs", "install", "--pack-id", "unknown-pack-xyz"],
    );
    assert_eq!(out["pack_id"], "unknown-pack-xyz");
    assert_eq!(out["status"], "declared");
    assert!(lock_path(&temp).exists());
}

#[test]
fn packs_list_reports_installed_packs() {
    let temp = TempDir::new().unwrap();
    run_json(
        ggen_in(&temp),
        &["packs", "install", "--pack-id", "surface-mcp"],
    );

    let out = run_json(ggen_in(&temp), &["packs", "list"]);
    assert!(out["packs"].is_array(), "packs must be an array: {out}");
    assert!(out.get("total").is_some());
    assert!(
        out["packs"]
            .as_array()
            .unwrap()
            .iter()
            .any(|p| p["pack_id"] == "surface-mcp"),
        "list must include surface-mcp: {out}"
    );
}

#[test]
fn packs_list_on_empty_project_is_empty_not_error() {
    let temp = TempDir::new().unwrap();
    let out = run_json(ggen_in(&temp), &["packs", "list"]);
    assert_eq!(out["total"], 0);
    assert_eq!(out["packs"].as_array().unwrap().len(), 0);
}

#[test]
fn packs_validate_returns_a_verdict() {
    let temp = TempDir::new().unwrap();
    let out = run_json(
        ggen_in(&temp),
        &["packs", "validate", "--pack-id", "surface-mcp"],
    );
    assert_eq!(out["pack_id"], "surface-mcp");
    assert!(out.get("is_valid").is_some(), "must report is_valid: {out}");
}

#[test]
fn packs_show_unknown_pack_is_graceful() {
    let temp = TempDir::new().unwrap();
    let out = run_json(
        ggen_in(&temp),
        &["packs", "show", "--pack-id", "nonexistent-pack-xyz"],
    );
    assert_eq!(out["pack_id"], "nonexistent-pack-xyz");
    assert_eq!(out["found"], Value::Bool(false));
}

// ── capability ──────────────────────────────────────────────────────────────

#[test]
fn capability_enable_expands_to_atomic_packs() {
    let temp = TempDir::new().unwrap();
    let out = run_json(ggen_in(&temp), &["capability", "enable", "mcp"]);

    assert_eq!(out["capability"], "mcp");
    let packs = out["atomic_packs"].as_array().expect("atomic_packs array");
    assert!(!packs.is_empty(), "must expand to at least one pack: {out}");
}

#[test]
fn capability_enable_with_projection_includes_projection_pack() {
    let temp = TempDir::new().unwrap();
    let out = run_json(
        ggen_in(&temp),
        &["capability", "enable", "mcp", "--projection", "rust"],
    );

    assert_eq!(out["projection"], "rust");
    let packs = out["atomic_packs"].as_array().unwrap();
    assert!(
        packs.iter().any(|p| p
            .as_str()
            .map(|s| s.contains("projection"))
            .unwrap_or(false)),
        "projection must add a projection pack: {out}"
    );
}

#[test]
fn capability_enable_writes_atomic_packs_to_lockfile() {
    let temp = TempDir::new().unwrap();
    run_json(ggen_in(&temp), &["capability", "enable", "mcp"]);

    assert!(lock_path(&temp).exists(), "enable must write a lockfile");
    assert!(count_lockfile_packs(&temp) >= 1, "lockfile must gain packs");
}

#[test]
fn capability_list_shows_surfaces() {
    let temp = TempDir::new().unwrap();
    let out = run_json(ggen_in(&temp), &["capability", "list"]);
    assert!(out["capabilities"].is_array());
    assert!(out["capabilities"]
        .as_array()
        .unwrap()
        .iter()
        .any(|c| c["id"] == "mcp"));
}

#[test]
fn capability_inspect_shows_atomic_packs() {
    let temp = TempDir::new().unwrap();
    let out = run_json(ggen_in(&temp), &["capability", "inspect", "mcp"]);
    assert_eq!(out["capability"], "mcp");
    assert!(out.get("atomic_packs").is_some());
}

// ── integration: an AGI brings a project up ─────────────────────────────────

#[test]
fn full_workflow_capability_then_install_then_list() {
    let temp = TempDir::new().unwrap();

    // 1. Discover capabilities, then enable one (records atomic packs).
    let caps = run_json(ggen_in(&temp), &["capability", "list"]);
    assert!(!caps["capabilities"].as_array().unwrap().is_empty());
    let enabled = run_json(
        ggen_in(&temp),
        &["capability", "enable", "mcp", "--projection", "rust"],
    );
    let atomic = enabled["atomic_packs"].as_array().unwrap().len();
    assert!(atomic >= 1);

    // 2. Install an additional pack (lockfile + receipt).
    run_json(
        ggen_in(&temp),
        &["packs", "install", "--pack-id", "surface-mcp"],
    );

    // 3. The lockfile now holds the capability packs AND the installed pack.
    assert!(count_lockfile_packs(&temp) >= atomic + 1);
    assert!(
        !receipt_json_files(&temp).is_empty(),
        "install left a receipt"
    );

    // 4. `packs list` reflects the full set; `validate` returns a verdict.
    let listed = run_json(ggen_in(&temp), &["packs", "list"]);
    assert!(listed["packs"]
        .as_array()
        .unwrap()
        .iter()
        .any(|p| p["pack_id"] == "surface-mcp"));
    let v = run_json(
        ggen_in(&temp),
        &["packs", "validate", "--pack-id", "surface-mcp"],
    );
    assert!(v.get("is_valid").is_some());
}

#[test]
fn full_workflow_multiple_packs_produce_distinct_receipts() {
    let temp = TempDir::new().unwrap();
    run_json(
        ggen_in(&temp),
        &["packs", "install", "--pack-id", "surface-mcp"],
    );
    run_json(
        ggen_in(&temp),
        &["packs", "install", "--pack-id", "projection-rust"],
    );

    assert_eq!(count_lockfile_packs(&temp), 2, "both packs must be tracked");
    assert!(
        receipt_json_files(&temp).len() >= 2,
        "each install must leave a distinct receipt"
    );

    let listed = run_json(ggen_in(&temp), &["packs", "list"]);
    assert!(listed["packs"].as_array().unwrap().len() >= 2);
}

#[test]
fn full_workflow_state_is_consistent_across_steps() {
    let temp = TempDir::new().unwrap();
    run_json(
        ggen_in(&temp),
        &["packs", "install", "--pack-id", "surface-mcp"],
    );
    let before = count_lockfile_packs(&temp);

    run_json(ggen_in(&temp), &["capability", "enable", "mcp"]);
    let after = count_lockfile_packs(&temp);

    assert!(after >= before, "enabling a capability must not lose packs");
    assert!(Path::new(&lock_path(&temp)).exists());
}
