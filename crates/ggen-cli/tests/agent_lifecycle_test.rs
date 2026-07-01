//! End-to-end lifecycle: an AGI completing a project with `ggen agent`.
//!
//! This drives the full agent lifecycle through the REAL `ggen` binary against a
//! real hermetic project (a temp registry, a temp `$HOME`, and a temp project
//! dir), parsing the JSON each verb emits and asserting on it AND on the durable
//! state it produces (the `.ggen/packs.lock` entry and the signed receipt on
//! disk). It exercises the agent surface's third transport — the CLI — exactly
//! as an autonomous agent would chain it to bring a project up and verify it:
//!
//! ```text
//!   capabilities → search → compatibility → install → status → verify → remove
//! ```
//!
//! Chicago TDD: real CLI process execution (`assert_cmd`), real filesystem, real
//! Ed25519 receipts. No mocks. Sabotage paths prove the surface is fail-closed.

use assert_cmd::Command;
use serde_json::Value;
use std::fs;
use std::path::Path;
use tempfile::TempDir;

/// A real, hermetic test world for the `ggen agent` CLI.
struct World {
    home: TempDir,
    project: TempDir,
    registry: TempDir,
}

impl World {
    fn new() -> Self {
        let project = TempDir::new().expect("project tempdir");
        fs::create_dir_all(project.path().join(".ggen")).expect("create .ggen");
        World {
            home: TempDir::new().expect("home tempdir"),
            project,
            registry: TempDir::new().expect("registry tempdir"),
        }
    }

    /// Write a real, loader-compatible pack TOML declaring `package`.
    fn write_pack(&self, id: &str, version: &str, package: &str) {
        let toml = format!(
            r#"[pack]
id = "{id}"
name = "Agent Pack {id}"
version = "{version}"
description = "Real fixture for agent_lifecycle_test"
category = "test"
license = "MIT"
production_ready = true
packages = ["{package}"]
"#
        );
        fs::write(self.registry.path().join(format!("{id}.toml")), toml).expect("write pack toml");
    }

    fn lock_path(&self) -> std::path::PathBuf {
        self.project.path().join(".ggen").join("packs.lock")
    }

    /// `ggen agent ...` with the hermetic env wired up.
    fn agent(&self) -> Command {
        let mut cmd = Command::cargo_bin("ggen").expect("ggen binary");
        cmd.arg("agent")
            .current_dir(self.project.path())
            .env("HOME", self.home.path())
            .env("GGEN_PACKS_DIR", self.registry.path())
            .env(
                "GGEN_PACK_CACHE_DIR",
                self.home.path().join(".ggen").join("packs"),
            );
        cmd
    }
}

/// Run `ggen agent <args> --format json`, assert success, parse stdout as JSON.
fn run_json(mut cmd: Command, args: &[&str]) -> Value {
    for a in args {
        cmd.arg(a);
    }
    cmd.arg("--format").arg("json");
    let out = cmd.output().expect("run ggen agent");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        out.status.success(),
        "`ggen agent {args:?}` failed: status={:?}\nstdout={stdout}\nstderr={stderr}",
        out.status.code(),
    );
    serde_json::from_str(stdout.trim())
        .unwrap_or_else(|e| panic!("stdout is not JSON ({e}): {stdout}"))
}

#[test]
fn agi_completes_project_lifecycle_through_ggen_agent() {
    let world = World::new();
    world.write_pack("alpha", "1.2.3", "alpha-core");
    world.write_pack("beta", "0.1.0", "beta-core");

    // 1. DISCOVER — capabilities advertises the operations the agent can chain.
    let caps = run_json(world.agent(), &["capabilities"]);
    let ops: Vec<&str> = caps["operations"]
        .as_array()
        .expect("operations array")
        .iter()
        .filter_map(|o| o["name"].as_str())
        .collect();
    for expected in [
        "search",
        "compatibility",
        "install",
        "status",
        "verify",
        "remove",
    ] {
        assert!(
            ops.contains(&expected),
            "capabilities missing {expected}: {ops:?}"
        );
    }

    // 2. SEARCH — find the pack to install (search emits a ranked array).
    let hits = run_json(world.agent(), &["search", "alpha"]);
    assert!(
        hits.as_array()
            .expect("search array")
            .iter()
            .any(|h| h["pack"]["id"] == "alpha"),
        "search must find alpha: {hits}"
    );

    // 3. CHECK COMPATIBILITY — the two packs compose (distinct package sets).
    let compat = run_json(world.agent(), &["compatibility", "alpha,beta"]);
    assert_eq!(
        compat["compatible"],
        Value::Bool(true),
        "alpha+beta must be compatible: {compat}"
    );

    // 4. INSTALL — write the lockfile (non-empty digest) + a signed receipt.
    let inst = run_json(world.agent(), &["install", "alpha"]);
    assert_eq!(inst["dry_run"], Value::Bool(false));
    assert!(
        inst["digest"]
            .as_str()
            .map(|d| !d.is_empty())
            .unwrap_or(false),
        "real install must pin a digest: {inst}"
    );
    assert_eq!(inst["receipt"]["signature_present"], Value::Bool(true));
    let receipt_path = inst["receipt"]["receipt_path"]
        .as_str()
        .expect("receipt path")
        .to_string();
    // Durable evidence on disk: lockfile + receipt both exist.
    assert!(world.lock_path().exists(), "lockfile must exist on disk");
    assert!(
        Path::new(&receipt_path).exists(),
        "receipt file must exist: {receipt_path}"
    );

    // 5. STATUS — the project now reports alpha installed.
    let status = run_json(world.agent(), &["status"]);
    assert_eq!(status["lockfile_present"], Value::Bool(true));
    assert!(
        status["installed"]
            .as_array()
            .expect("installed array")
            .iter()
            .any(|p| p["pack_id"] == "alpha" && p["version"] == "1.2.3"),
        "status must report alpha@1.2.3: {status}"
    );

    // 6. VERIFY — the emitted provenance receipt verifies against the project key.
    let verify = run_json(world.agent(), &["verify", &receipt_path]);
    assert_eq!(
        verify["is_valid"],
        Value::Bool(true),
        "emitted receipt must verify: {verify}"
    );

    // 7. REMOVE — tear the pack out; the lifecycle is reversible.
    let removed = run_json(world.agent(), &["remove", "alpha"]);
    assert_eq!(removed["removed"], Value::Bool(true));
    let status2 = run_json(world.agent(), &["status"]);
    assert!(
        !status2["installed"]
            .as_array()
            .expect("installed array")
            .iter()
            .any(|p| p["pack_id"] == "alpha"),
        "alpha must be gone after remove: {status2}"
    );
}

#[test]
fn agent_install_nonexistent_pack_is_fail_closed() {
    let world = World::new(); // empty registry — the pack does not exist
    let mut cmd = world.agent();
    cmd.arg("install").arg("ghost").arg("--format").arg("json");
    let out = cmd.output().expect("run");
    assert!(
        !out.status.success(),
        "installing a nonexistent pack must fail loudly, got success"
    );
    assert!(
        !world.lock_path().exists(),
        "a failed install must not write a lockfile"
    );
}

#[test]
fn agent_verify_tampered_receipt_is_invalid_via_cli() {
    let world = World::new();
    world.write_pack("alpha", "1.0.0", "alpha-core");

    let inst = run_json(world.agent(), &["install", "alpha"]);
    let receipt_path = inst["receipt"]["receipt_path"]
        .as_str()
        .expect("receipt path")
        .to_string();

    // Tamper the signature on disk.
    let mut body: Value = serde_json::from_slice(&fs::read(&receipt_path).unwrap()).unwrap();
    body["signature"] = Value::String("00".repeat(64));
    fs::write(&receipt_path, serde_json::to_vec_pretty(&body).unwrap()).unwrap();

    let verify = run_json(world.agent(), &["verify", &receipt_path]);
    assert_eq!(
        verify["is_valid"],
        Value::Bool(false),
        "a tampered receipt must not verify through the CLI: {verify}"
    );
}

#[test]
fn agent_install_dry_run_writes_no_durable_state() {
    let world = World::new();
    world.write_pack("alpha", "1.0.0", "alpha-core");

    let inst = run_json(world.agent(), &["install", "alpha", "--dry_run", "true"]);
    assert_eq!(inst["dry_run"], Value::Bool(true));
    assert!(
        inst["digest"]
            .as_str()
            .map(|d| d.is_empty())
            .unwrap_or(true),
        "dry run must pin no digest: {inst}"
    );
    assert!(
        !world.lock_path().exists(),
        "dry run must not write a lockfile"
    );
}
