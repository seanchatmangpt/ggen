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

//! Executable-Proof Integration Tests for `ggen pack` Verbs
//!
//! **Chicago TDD Principles** (no mocks, no test doubles):
//! - REAL CLI process execution via `assert_cmd::Command::cargo_bin("ggen")`.
//! - REAL filesystem state in a `TempDir` (a real pack registry + `.ggen/` project layout).
//! - State-based verification of OBSERVABLE durable state: the registry TOML on disk,
//!   `.ggen/packs.lock` contents, installed pack directories, and signed receipts.
//! - At least one SABOTAGE / negative-path assertion per durable verb.
//!
//! **Authoritative invariant under test** (see `.claude/rules/coding-agent-mistakes.md`):
//! A real `pack add` MUST write a NON-EMPTY entry to `.ggen/packs.lock` (lockfile
//! invariant 4.1) with a NON-EMPTY digest (`integrity`), and MUST emit a signed
//! provenance receipt under `.ggen/receipts/`. A verb that exits 0 and prints success
//! while leaving the world unchanged is "decorative completion" and is a DEFECT.
//!
//! ## Source map (read before editing)
//! - Verbs: `crates/ggen-cli/src/cmds/pack.rs`
//! - Registry resolution: `crates/ggen-core/src/domain/packs/metadata.rs::get_packs_dir`
//!   honours `GGEN_PACKS_DIR` (a dir of `<id>.toml` pack files), highest priority.
//! - Lockfile model: `crates/ggen-core/src/packs/lockfile.rs::PackLockfile`
//!   (`.ggen/packs.lock`, JSON, `packs: BTreeMap<id, LockedPack>`).
//! - `add` install path: `crates/ggen-core/src/domain/packs/install.rs::install_pack`
//!   installs into `$HOME/.ggen/packs/<id>` AND writes `<cwd>/.ggen/packs.lock` with a
//!   non-empty `integrity` digest, then `pack add` emits a signed receipt.
//! - Receipt closure: `crates/ggen-cli/src/cmds/packs_receipt.rs::generate_pack_install_receipt`
//!   binds pack id+version+digest + packages as input_hashes and installed artifacts as
//!   output_hashes; refuses to emit on an empty digest (fail-closed).

use assert_cmd::Command;
use predicates::prelude::*;
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

/// A real, hermetic test world:
/// - `home`:     temp $HOME so installs land in `<home>/.ggen/packs/<id>`.
/// - `project`:  temp working directory containing `.ggen/`.
/// - `registry`: temp dir of `<id>.toml` pack files (pointed at by `GGEN_PACKS_DIR`).
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

    fn registry_path(&self) -> &Path {
        self.registry.path()
    }

    fn project_path(&self) -> &Path {
        self.project.path()
    }

    fn lock_path(&self) -> PathBuf {
        self.project.path().join(".ggen").join("packs.lock")
    }

    fn receipts_dir(&self) -> PathBuf {
        self.project.path().join(".ggen").join("receipts")
    }

    /// Write a REAL, valid pack TOML into the registry. Mirrors the on-disk format
    /// deserialized by `PackFile`/`Pack`.
    fn write_pack(&self, id: &str, version: &str) {
        let toml = format!(
            r#"[pack]
id = "{id}"
name = "Proof Pack {id}"
version = "{version}"
description = "Real pack fixture for proof_pack_test"
category = "test"
author = "proof-test"
license = "MIT"
production_ready = true
packages = ["{id}-core"]
tags = ["proof", "test"]
keywords = ["{id}"]
"#
        );
        let path = self.registry.path().join(format!("{id}.toml"));
        fs::write(&path, toml).expect("write pack toml");
        assert!(path.exists(), "registry pack file must exist after write");
    }

    /// `ggen pack <verb> ...` with the hermetic env wired up.
    fn pack(&self) -> Command {
        let mut cmd = ggen();
        cmd.arg("pack")
            .current_dir(self.project.path())
            .env("HOME", self.home.path())
            .env("GGEN_PACKS_DIR", self.registry.path())
            .env(
                "GGEN_PACK_CACHE_DIR",
                self.home.path().join(".ggen").join("packs"),
            );
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

    /// True iff the lockfile exists and contains a NON-EMPTY entry for `id`
    /// (present key AND non-empty version string — lockfile invariant 4.1).
    fn lockfile_has_pack(&self, id: &str) -> bool {
        match self.read_lockfile() {
            Some(json) => json
                .get("packs")
                .and_then(|p| p.get(id))
                .and_then(|entry| entry.get("version"))
                .and_then(|v| v.as_str())
                .map(|v| !v.is_empty())
                .unwrap_or(false),
            None => false,
        }
    }

    /// The `integrity` (digest) string recorded for `id`, if any.
    fn lockfile_digest(&self, id: &str) -> Option<String> {
        self.read_lockfile()?
            .get("packs")?
            .get(id)?
            .get("integrity")?
            .as_str()
            .map(|s| s.to_string())
    }

    /// List the signed receipt files under `.ggen/receipts/`.
    fn receipt_files(&self) -> Vec<PathBuf> {
        let dir = self.receipts_dir();
        match fs::read_dir(&dir) {
            Ok(rd) => rd
                .filter_map(|e| e.ok())
                .map(|e| e.path())
                .filter(|p| p.extension().and_then(|s| s.to_str()) == Some("json"))
                .collect(),
            Err(_) => Vec::new(),
        }
    }

    /// Seed a real lockfile with one pack so lockfile-reading verbs (`remove`) have
    /// authoritative state to mutate.
    fn seed_lockfile(&self, id: &str, version: &str) {
        let json = format!(
            r#"{{
  "packs": {{
    "{id}": {{
      "version": "{version}",
      "source": {{ "type": "Registry", "url": "https://registry.ggen.io" }},
      "integrity": "sha256-seeded",
      "installed_at": "2024-01-01T00:00:00Z",
      "dependencies": []
    }}
  }},
  "updated_at": "2024-01-01T00:00:00Z",
  "ggen_version": "6.0.0"
}}"#
        );
        let p = self.lock_path();
        fs::create_dir_all(p.parent().unwrap()).unwrap();
        fs::write(&p, json).expect("seed lockfile");
        assert!(
            self.lockfile_has_pack(id),
            "seed must produce a readable non-empty lockfile entry"
        );
    }
}

// ============================================================================
// add — durable-state proof: lockfile entry (digest) + signed receipt
// ============================================================================
//
// OBSERVABLE assertions after `pack add <id>` against a REAL registry entry
// (lockfile invariant 4.1 + receipt invariant 4.2):
//   1. Real install dir created under $HOME.
//   2. `.ggen/packs.lock` contains a non-empty entry for <id> with a NON-EMPTY
//      `integrity` digest (CRACK #57 fixed).
//   3. A signed provenance receipt exists under `.ggen/receipts/` whose
//      input_hashes bind the pack closure and whose signature is non-empty
//      (CRACK #56 fixed).
#[test]
fn test_add_writes_lockfile_with_digest_and_emits_signed_receipt() {
    let world = World::new();
    world.write_pack("alpha", "1.2.3");

    world
        .pack()
        .arg("add")
        .arg("alpha")
        .assert()
        .success()
        .stdout(predicate::str::contains("installed").or(predicate::str::contains("alpha")));

    // (1) PROOF of REAL durable install work: $HOME/.ggen/packs/alpha.
    let install_dir = world.home.path().join(".ggen").join("packs").join("alpha");
    assert!(
        install_dir.exists(),
        "`pack add` must perform real durable work. Expected install dir {} to exist.",
        install_dir.display()
    );

    // (2) AUTHORITATIVE invariant 4.1: lockfile entry exists with a non-empty digest.
    const EXPECT_LOCKFILE: bool = true; // CRACK #57: add now writes the lockfile.
    let lockfile_ok = world.lockfile_has_pack("alpha");
    if EXPECT_LOCKFILE {
        assert!(
            lockfile_ok,
            "`pack add` must write a non-empty .ggen/packs.lock entry (invariant 4.1)"
        );
        let digest = world
            .lockfile_digest("alpha")
            .expect("lockfile entry must carry an integrity digest");
        assert!(
            digest.starts_with("sha256-") && digest.len() > "sha256-".len(),
            "lockfile digest must be a non-empty sha256 (invariant 4.1), got {:?}",
            digest
        );
    }

    // (3) Receipt invariant 4.2: a signed receipt must exist, with a non-empty
    // signature and input_hashes that bind the pack closure (id@version:digest).
    let receipts = world.receipt_files();
    assert!(
        !receipts.is_empty(),
        "`pack add` must emit a provenance receipt under .ggen/receipts/ (CRACK #56)"
    );
    let receipt_json: Value =
        serde_json::from_str(&fs::read_to_string(&receipts[0]).expect("read receipt"))
            .expect("receipt must be valid JSON");

    let signature = receipt_json
        .get("signature")
        .and_then(|v| v.as_str())
        .unwrap_or("");
    assert!(
        !signature.is_empty(),
        "receipt signature must be non-empty (no decorative completion)"
    );

    let input_hashes = receipt_json
        .get("input_hashes")
        .and_then(|v| v.as_array())
        .cloned()
        .unwrap_or_default();
    let bound_closure = input_hashes.iter().any(|h| {
        h.as_str()
            .map(|s| s.contains("pack:alpha@1.2.3:"))
            .unwrap_or(false)
    });
    assert!(
        bound_closure,
        "receipt input_hashes must bind the real pack closure (pack:alpha@1.2.3:<digest>), got {:?}",
        input_hashes
    );
}

// SABOTAGE / negative path for add: adding a pack absent from the registry must NOT
// fake success, must NOT forge a lockfile entry, must NOT create an install dir, and
// must NOT emit a receipt (fail-closed).
#[test]
fn test_add_nonexistent_pack_does_not_fake_success_or_emit_receipt() {
    let world = World::new();
    // Registry is empty: no nonexistent.toml written.

    let assert = world.pack().arg("add").arg("nonexistent").assert();
    let output = assert.get_output().clone();
    let code = output.status.code();
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);
    let combined = format!("{stdout}{stderr}");

    let loud = code != Some(0) || combined.contains("not_found") || combined.contains("not found");
    assert!(
        loud,
        "FAIL-OPEN DEFECT: adding a nonexistent pack must fail loudly. Got exit {:?}, output: {}",
        code, combined
    );

    // No durable lockfile entry may have been forged.
    assert!(
        !world.lockfile_has_pack("nonexistent"),
        "nonexistent pack must never appear in .ggen/packs.lock"
    );
    // No install dir may have been created for it.
    assert!(
        !world
            .home
            .path()
            .join(".ggen")
            .join("packs")
            .join("nonexistent")
            .exists(),
        "nonexistent pack must never create an install directory"
    );
    // No receipt may have been emitted for a failed install (fail-closed, CRACK #56).
    assert!(
        world.receipt_files().is_empty(),
        "a failed install must not emit a provenance receipt (fail-open defect)"
    );
}

// ============================================================================
// list — reads the REAL registry
// ============================================================================
#[test]
fn test_list_reports_packs_present_in_real_registry() {
    let world = World::new();
    world.write_pack("alpha", "1.0.0");
    world.write_pack("beta", "2.0.0");

    world
        .pack()
        .arg("list")
        .assert()
        .success()
        .stdout(predicate::str::contains("alpha"))
        .stdout(predicate::str::contains("beta"));
}

// SABOTAGE for list: an empty/absent registry must not invent packs.
#[test]
fn test_list_empty_registry_reports_no_packs() {
    let world = World::new();

    let assert = world.pack().arg("list").assert();
    let output = assert.get_output().clone();
    let stdout = String::from_utf8_lossy(&output.stdout);

    assert!(
        !stdout.contains("alpha") && !stdout.contains("beta"),
        "empty registry must not list packs that were never written: {}",
        stdout
    );
}

// ============================================================================
// show — reads a REAL pack's metadata from the registry
// ============================================================================
#[test]
fn test_show_reflects_real_pack_metadata() {
    let world = World::new();
    world.write_pack("gamma", "3.1.4");

    world
        .pack()
        .arg("show")
        .arg("gamma")
        .assert()
        .success()
        .stdout(predicate::str::contains("gamma"))
        .stdout(predicate::str::contains("3.1.4"));
}

// SABOTAGE for show: showing a pack absent from the registry must fail loudly.
#[test]
fn test_show_nonexistent_pack_exits_nonzero() {
    let world = World::new();

    world.pack().arg("show").arg("ghost").assert().failure();
}

// ============================================================================
// remove — REAL mutation of .ggen/packs.lock
// ============================================================================
#[test]
fn test_remove_mutates_real_lockfile() {
    let world = World::new();
    world.seed_lockfile("delta", "1.0.0");
    assert!(
        world.lockfile_has_pack("delta"),
        "precondition: seeded lockfile must contain delta"
    );

    world
        .pack()
        .arg("remove")
        .arg("delta")
        .env(
            "GGEN_PACK_CACHE_DIR",
            world.home.path().join(".ggen").join("packs"),
        )
        .assert()
        .success()
        .stdout(predicate::str::contains("removed").or(predicate::str::contains("delta")));

    assert!(
        !world.lockfile_has_pack("delta"),
        "`pack remove` must delete the entry from .ggen/packs.lock (real mutation)"
    );
    assert!(
        world.read_lockfile().is_some(),
        "lockfile must remain valid JSON after removal"
    );
}

// SABOTAGE for remove (1): removing when no lockfile exists must fail loudly.
#[test]
fn test_remove_without_lockfile_exits_nonzero() {
    let world = World::new();
    assert!(!world.lock_path().exists());

    world
        .pack()
        .arg("remove")
        .arg("delta")
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("No packs installed")
                .or(predicate::str::contains("not found"))
                .or(predicate::str::contains("packs.lock")),
        );
}

// SABOTAGE for remove (2): removing a pack not present in an existing lockfile must
// fail loudly and must NOT silently rewrite/empty the lockfile.
#[test]
fn test_remove_absent_pack_exits_nonzero_and_preserves_lockfile() {
    let world = World::new();
    world.seed_lockfile("delta", "1.0.0");

    world
        .pack()
        .arg("remove")
        .arg("not-installed")
        .env(
            "GGEN_PACK_CACHE_DIR",
            world.home.path().join(".ggen").join("packs"),
        )
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("not installed").or(predicate::str::contains("not-installed")),
        );

    assert!(
        world.lockfile_has_pack("delta"),
        "failed removal must not corrupt or empty the existing lockfile"
    );
}

// ============================================================================
// add → remove round-trip: the lockfile add wrote is the one remove consumes
// ============================================================================
//
// This proves the formats are compatible by construction: `add` writes a lockfile
// entry (with digest) that `remove` can read and delete — the authoritative pack
// resolution path is end-to-end consistent.
#[test]
fn test_add_then_remove_roundtrip_on_real_lockfile() {
    let world = World::new();
    world.write_pack("epsilon", "0.9.0");

    world.pack().arg("add").arg("epsilon").assert().success();
    assert!(
        world.lockfile_has_pack("epsilon"),
        "add must have written epsilon to the lockfile"
    );

    world
        .pack()
        .arg("remove")
        .arg("epsilon")
        .env(
            "GGEN_PACK_CACHE_DIR",
            world.home.path().join(".ggen").join("packs"),
        )
        .assert()
        .success();

    assert!(
        !world.lockfile_has_pack("epsilon"),
        "remove must delete the entry that add wrote (compatible formats)"
    );
}
