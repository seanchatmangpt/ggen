#![allow(
    dead_code,
    unused_imports,
    unused_variables,
    deprecated,
    clippy::all,
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    unused_mut
)]

//! Executable-Proof Integration Tests for `ggen packs` (plural) Verbs
//!
//! **Chicago TDD Principles** (no mocks, no test doubles):
//! - REAL CLI process execution via `assert_cmd::Command::cargo_bin("ggen")`.
//! - REAL filesystem state in TempDirs (a real pack registry + `.ggen/` project layout).
//! - State-based verification of OBSERVABLE durable state: stdout JSON, `.ggen/packs.lock`
//!   contents, and receipts under `.ggen/receipts/`.
//!
//! ## Why this file exists (not an extension of `proof_pack_test.rs`)
//!
//! `ggen pack` (singular, `crates/ggen-cli/src/cmds/pack.rs`) and `ggen packs` (plural,
//! `crates/ggen-cli/src/cmds/packs.rs`) are two DIFFERENT nouns with different semantics:
//! `pack` is a fail-closed registry install; `packs` is a lenient, lockfile-oriented
//! dependency-tracking surface — an unresolved pack is still recorded (`status: declared`)
//! rather than erroring. `packs.rs` had ZERO live automated test coverage before this file.
//!
//! ## CLI shape (confirmed via `ggen packs <verb> --help`, not assumed)
//!
//! The plural noun's verb args are declared WITHOUT `#[arg(index = 1)]` in `packs.rs`, so
//! `clap-noun-verb-macros` kebab-cases them into REQUIRED FLAGS: `packs install --pack-id
//! <ID>` (hyphenated flag, NOT a bare positional, NOT `--pack_id`).
//!
//! ## Source map (read before editing)
//! - Verbs: `crates/ggen-cli/src/cmds/packs.rs`
//! - Registry resolution: `crates/ggen-marketplace/src/packs_registry/metadata.rs::get_packs_dir`
//!   — `GGEN_PACKS_DIR` env var is checked FIRST (highest priority).
//! - Pack shape: `crates/ggen-marketplace/src/packs_registry/types.rs::{Pack, PackFile}`.
//! - Lockfile model: `crates/ggen-marketplace/src/packs/lockfile.rs::PackLockfile`
//!   (`<cwd>/.ggen/packs.lock`, JSON, `packs: BTreeMap<id, LockedPack>`).
//! - Receipt emission: `crates/ggen-marketplace/src/agent/receipt.rs::emit_install_receipt`
//!   — roots receipts at `<root>/.ggen/receipts/` and auto-generates an Ed25519 keypair
//!   under `<root>/.ggen/keys/` on first use. `packs install` passes the CLI's resolved
//!   project root (`std::env::current_dir()`, see `packs.rs::project_root`) as `root`, so
//!   NO `HOME` env override is needed — only `.current_dir()` on the test process.
//!
//! ## Verified live behavior this file's assertions are built from (not assumed)
//!
//! Every JSON field name and exit-code claim below was confirmed by running the real
//! `ggen` binary against hand-written fixtures before this file was written (see the
//! authoring session's transcript). In particular:
//! - `packs install` on an UNRESOLVED pack still writes a full lockfile entry
//!   (`status: "declared"`, `version: "0.0.0"`, a real `sha256-` digest) and still emits a
//!   receipt — this is deliberately lenient, unlike singular `pack add`.
//! - `packs validate` and `packs show` on an unresolved/missing pack both exit 0 with a
//!   `is_valid: false` / `found: false` payload — NOT a process failure, unlike singular
//!   `pack show` (which exits nonzero for a missing pack).
//! - `packs install --pack-id ""` (or whitespace-only) DOES fail loudly: `packs.rs`'s
//!   `validate_pack_id` rejects an empty/whitespace pack id with `NounVerbError::argument_error`
//!   before any lockfile/receipt work happens, producing a nonzero exit.

use assert_cmd::Command;
use serde_json::Value;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

// ============================================================================
// Test Utilities — REAL state setup (no mocks)
// ============================================================================

/// Real `ggen` binary command.
fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("ggen binary not found")
}

/// A real, hermetic test world for the `packs` (plural) noun:
/// - `project`:  temp working directory; `.ggen/packs.lock`, `.ggen/receipts/`, and
///               `.ggen/keys/` are all rooted here by `packs.rs::project_root()`.
/// - `registry`: temp dir of `<id>.toml` pack files, pointed at by `GGEN_PACKS_DIR`
///               (highest-priority resolution order in `metadata.rs::get_packs_dir`).
///
/// No `HOME` override is needed: the plural noun never touches `$HOME`.
struct World {
    project: TempDir,
    registry: TempDir,
}

impl World {
    fn new() -> Self {
        World {
            project: TempDir::new().expect("project tempdir"),
            registry: TempDir::new().expect("registry tempdir"),
        }
    }

    fn lock_path(&self) -> PathBuf {
        self.project.path().join(".ggen").join("packs.lock")
    }

    fn receipts_dir(&self) -> PathBuf {
        self.project.path().join(".ggen").join("receipts")
    }

    /// Write a real, valid pack TOML into the registry. Mirrors the on-disk format
    /// deserialized by `PackFile`/`Pack` (`packs_registry/types.rs`): `id`, `name`,
    /// `version`, `description`, `category` are required; everything else defaults.
    fn write_pack(&self, id: &str, version: &str, packages: &[&str]) {
        let packages_toml = packages
            .iter()
            .map(|p| format!("\"{p}\""))
            .collect::<Vec<_>>()
            .join(", ");
        let toml = format!(
            r#"[pack]
id = "{id}"
name = "Demo {id}"
version = "{version}"
description = "d"
category = "test"
packages = [{packages_toml}]
"#
        );
        let path = self.registry.path().join(format!("{id}.toml"));
        fs::write(&path, toml).expect("write pack toml");
        assert!(path.exists(), "registry pack file must exist after write");
    }

    /// `ggen packs <verb> ...` with the hermetic env wired up. `--format json` is
    /// always requested so stdout is pure, parseable JSON (confirmed live: the
    /// tracing `INFO ggen.cli{...}` line goes to stderr, never stdout).
    fn packs(&self) -> Command {
        let mut cmd = ggen();
        cmd.arg("packs")
            .arg("--format")
            .arg("json")
            .current_dir(self.project.path())
            .env("GGEN_PACKS_DIR", self.registry.path());
        cmd
    }

    /// Read `.ggen/packs.lock` as JSON, or None if absent/unparseable.
    fn read_lockfile(&self) -> Option<Value> {
        let p = self.lock_path();
        if !p.exists() {
            return None;
        }
        let content = fs::read_to_string(&p).ok()?;
        serde_json::from_str(&content).ok()
    }

    /// The `integrity` string recorded for `id` in the lockfile, if any.
    fn lockfile_integrity(&self, id: &str) -> Option<String> {
        self.read_lockfile()?
            .get("packs")?
            .get(id)?
            .get("integrity")?
            .as_str()
            .map(|s| s.to_string())
    }

    /// List `*.json` receipt files under `.ggen/receipts/`.
    fn receipt_files(&self) -> Vec<PathBuf> {
        match fs::read_dir(self.receipts_dir()) {
            Ok(rd) => rd
                .filter_map(|e| e.ok())
                .map(|e| e.path())
                .filter(|p| p.extension().and_then(|s| s.to_str()) == Some("json"))
                .collect(),
            Err(_) => Vec::new(),
        }
    }
}

/// Parse a command's stdout as JSON, asserting the process succeeded first.
fn stdout_json(mut cmd: Command) -> Value {
    let assert = cmd.assert().success();
    let output = assert.get_output();
    let stdout = String::from_utf8_lossy(&output.stdout);
    serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout was not valid JSON ({e}): {stdout}"))
}

// ============================================================================
// install
// ============================================================================

#[test]
fn install_resolvable_pack_reports_installed_and_writes_lockfile() {
    let world = World::new();
    world.write_pack("demo", "1.2.3", &["demo-core"]);

    let json = stdout_json({
        let mut cmd = world.packs();
        cmd.arg("install").arg("--pack-id").arg("demo");
        cmd
    });

    assert_eq!(json["status"], "installed", "got: {json}");
    assert_eq!(json["version"], "1.2.3", "got: {json}");
    let digest = json["digest"].as_str().unwrap_or_default();
    assert!(!digest.is_empty(), "digest must be non-empty, got: {json}");

    assert!(
        world.lock_path().exists(),
        "`packs install` must write .ggen/packs.lock"
    );
    let integrity = world
        .lockfile_integrity("demo")
        .expect("lockfile entry for demo must carry an integrity digest");
    assert!(
        integrity.starts_with("sha256-"),
        "integrity must start with 'sha256-', got: {integrity:?}"
    );
}

#[test]
fn install_unresolvable_pack_reports_declared_at_zero_version() {
    let world = World::new();
    // Registry exists but has no ghost.toml.

    let json = stdout_json({
        let mut cmd = world.packs();
        cmd.arg("install").arg("--pack-id").arg("ghost");
        cmd
    });

    assert_eq!(json["status"], "declared", "got: {json}");
    assert_eq!(json["version"], "0.0.0", "got: {json}");

    // Lenient by design: the lockfile is still written with a non-empty integrity.
    let integrity = world
        .lockfile_integrity("ghost")
        .expect("even a declared (unresolved) pack must get a lockfile entry with integrity");
    assert!(
        !integrity.is_empty(),
        "declared pack's integrity must be non-empty, got: {integrity:?}"
    );
}

#[test]
fn install_emits_receipt_under_project() {
    let world = World::new();
    world.write_pack("demo", "1.2.3", &["demo-core"]);

    world
        .packs()
        .arg("install")
        .arg("--pack-id")
        .arg("demo")
        .assert()
        .success();

    let receipts = world.receipt_files();
    assert_eq!(
        receipts.len(),
        1,
        "expected exactly one receipt *.json file under .ggen/receipts/, got: {receipts:?}"
    );
}

// ============================================================================
// list
// ============================================================================

#[test]
fn list_empty_project_reports_zero() {
    let world = World::new();
    // No install has happened; no lockfile yet.
    assert!(!world.lock_path().exists());

    let json = stdout_json({
        let mut cmd = world.packs();
        cmd.arg("list");
        cmd
    });

    assert_eq!(json["total"], 0, "got: {json}");
    assert_eq!(
        json["packs"].as_array().map(|a| a.len()),
        Some(0),
        "got: {json}"
    );
}

#[test]
fn list_after_install_reports_the_pack() {
    let world = World::new();
    world.write_pack("demo", "1.2.3", &["demo-core"]);
    world
        .packs()
        .arg("install")
        .arg("--pack-id")
        .arg("demo")
        .assert()
        .success();

    let json = stdout_json({
        let mut cmd = world.packs();
        cmd.arg("list");
        cmd
    });

    assert_eq!(json["total"], 1, "got: {json}");
    let packs = json["packs"].as_array().expect("packs must be an array");
    assert_eq!(packs.len(), 1, "got: {json}");
    assert_eq!(packs[0]["pack_id"], "demo", "got: {json}");
    assert!(
        packs[0].get("integrity").is_some(),
        "list entry must carry an integrity field, got: {json}"
    );
}

// ============================================================================
// validate
// ============================================================================

#[test]
fn validate_valid_pack_is_valid() {
    let world = World::new();
    world.write_pack("demo", "1.2.3", &["demo-core"]);

    let json = stdout_json({
        let mut cmd = world.packs();
        cmd.arg("validate").arg("--pack-id").arg("demo");
        cmd
    });

    assert_eq!(json["is_valid"], true, "got: {json}");
}

#[test]
fn validate_unresolvable_pack_is_invalid() {
    let world = World::new();
    // Empty registry: no ghost.toml.

    let mut cmd = world.packs();
    cmd.arg("validate").arg("--pack-id").arg("ghost");
    let assert = cmd.assert().success(); // exit code 0, not an error exit
    let output = assert.get_output();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let json: Value = serde_json::from_str(&stdout).expect("stdout must be valid JSON");

    assert_eq!(json["is_valid"], false, "got: {json}");
    let errors = json["errors"]
        .as_array()
        .expect("errors must be an array");
    assert!(
        !errors.is_empty(),
        "errors array must be non-empty for an unresolvable pack, got: {json}"
    );
}

// ============================================================================
// show
// ============================================================================

#[test]
fn show_found_reflects_metadata() {
    let world = World::new();
    world.write_pack("demo", "3.1.4", &["demo-core"]);

    let json = stdout_json({
        let mut cmd = world.packs();
        cmd.arg("show").arg("--pack-id").arg("demo");
        cmd
    });

    assert_eq!(json["found"], true, "got: {json}");
    assert_eq!(json["version"], "3.1.4", "got: {json}");
    let packages = json["packages"]
        .as_array()
        .expect("packages must be an array");
    assert!(
        packages.iter().any(|p| p == "demo-core"),
        "packages must contain demo-core, got: {json}"
    );
}

#[test]
fn show_missing_is_graceful_not_found() {
    let world = World::new();
    // Empty registry: no ghost.toml.

    let mut cmd = world.packs();
    cmd.arg("show").arg("--pack-id").arg("ghost");
    let assert = cmd.assert().success(); // exit code 0 — deliberately graceful
    let output = assert.get_output();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let json: Value = serde_json::from_str(&stdout).expect("stdout must be valid JSON");

    assert_eq!(json["found"], false, "got: {json}");
    let message = json["message"].as_str().unwrap_or_default().to_lowercase();
    assert!(
        message.contains("not found"),
        "message must mention 'not found', got: {json}"
    );
}

// ============================================================================
// install — sabotage / negative path
// ============================================================================

#[test]
fn install_empty_pack_id_errors() {
    let world = World::new();

    world
        .packs()
        .arg("install")
        .arg("--pack-id")
        .arg("  ")
        .assert()
        .failure();

    assert!(
        !world.lock_path().exists(),
        "an empty/whitespace pack id must not write a lockfile"
    );
}
