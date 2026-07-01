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

//! Executable-Proof Sabotage Tests for `ggen sync --locked` digest RE-VERIFICATION
//!
//! **Authoritative invariant under test** (`.claude/rules/coding-agent-mistakes.md`):
//! - Lockfile invariant 4.1: every entry's `digest` (`integrity`) must be a
//!   non-empty SHA-256.
//! - Sync invariant 4.3.3: `digest` must be RE-VERIFIED at `ggen sync --locked`
//!   time; a mismatch (or a missing pack on disk) must HARD-FAIL (exit non-zero).
//!
//! Before this fix, `validate_sync_preconditions` only asserted the `integrity`
//! field was non-empty — it never recomputed the digest from the pack on disk
//! and compared it. That is FAIL-OPEN (Mistake Class 1.3): a pack mutated after
//! it was locked would still pass `--locked`. These tests sabotage the on-disk
//! pack and prove the binary now fails loudly.
//!
//! **Chicago TDD** (no mocks, no test doubles):
//! - REAL CLI process execution via `assert_cmd::Command::cargo_bin("ggen")`.
//! - REAL filesystem state in `TempDir`s: a real pack registry, a real `$HOME`
//!   install tree, a real `.ggen/packs.lock`.
//! - State-based / observable verification: process exit code + emitted error
//!   text. The sabotage is a real file mutation/removal on disk.
//!
//! ## Source map (read before editing)
//! - Precondition entry: `crates/ggen-cli/src/cmds/sync.rs::check_profile_preconditions`
//!   (runs FIRST in the `sync` verb, before any manifest work).
//! - Re-verification: `crates/ggen-core/src/domain/sync_profile.rs::verify_pack_digests`.
//! - Digest algorithm: `crates/ggen-core/src/domain/packs/install.rs::compute_pack_digest`
//!   (hashes pack id/version/packages/deps — NOT raw file bytes).
//! - Registry resolution: `crates/ggen-core/src/domain/packs/metadata.rs::get_packs_dir`
//!   honours `GGEN_PACKS_DIR` (a dir of `<id>.toml` files), highest priority.
//! - `add` install path: `install.rs::install_pack` installs into
//!   `$HOME/.ggen/packs/<id>` AND writes `<cwd>/.ggen/packs.lock` recording the
//!   source as `Local { path = <install dir> }`.

use assert_cmd::Command;
use std::fs;
use std::path::{Path, PathBuf};
use tempfile::TempDir;

// ============================================================================
// Test Utilities — REAL state setup (no mocks). Mirrors proof_pack_test.rs.
// ============================================================================

/// Real `ggen` binary command.
fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("ggen binary not found")
}

/// A real, hermetic test world:
/// - `home`:     temp `$HOME` so installs land in `<home>/.ggen/packs/<id>`.
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

    fn project_path(&self) -> &Path {
        self.project.path()
    }

    fn registry_pack_path(&self, id: &str) -> PathBuf {
        self.registry.path().join(format!("{id}.toml"))
    }

    fn install_dir(&self, id: &str) -> PathBuf {
        self.home.path().join(".ggen").join("packs").join(id)
    }

    fn lock_path(&self) -> PathBuf {
        self.project.path().join(".ggen").join("packs.lock")
    }

    /// Write a REAL, valid pack TOML into the registry.
    fn write_pack(&self, id: &str, version: &str) {
        let toml = format!(
            r#"[pack]
id = "{id}"
name = "Reverify Pack {id}"
version = "{version}"
description = "Real pack fixture for proof_digest_reverify_test"
category = "test"
author = "proof-test"
license = "MIT"
production_ready = true
packages = ["{id}-core"]
tags = ["proof", "test"]
keywords = ["{id}"]
"#
        );
        let path = self.registry_pack_path(id);
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

    /// `ggen sync ...` with the same hermetic env (so re-verification resolves
    /// the registry via `GGEN_PACKS_DIR` and the install dir under this `$HOME`).
    fn sync(&self) -> Command {
        let mut cmd = ggen();
        cmd.arg("sync")
            .current_dir(self.project.path())
            .env("HOME", self.home.path())
            .env("GGEN_PACKS_DIR", self.registry.path())
            .env(
                "GGEN_PACK_CACHE_DIR",
                self.home.path().join(".ggen").join("packs"),
            );
        cmd
    }

    /// Install `id` via the real `pack add` path. Asserts the durable lockfile +
    /// install dir exist afterwards so re-verification has real state to check.
    fn install(&self, id: &str, version: &str) {
        self.write_pack(id, version);
        self.pack().arg("add").arg(id).assert().success();
        assert!(
            self.lock_path().exists(),
            "precondition: `pack add` must write .ggen/packs.lock"
        );
        assert!(
            self.install_dir(id).exists(),
            "precondition: `pack add` must create the install dir under $HOME"
        );
    }
}

/// Combined stdout+stderr of a `Command` assertion's captured output.
fn combined_output(cmd: &mut Command) -> (Option<i32>, String) {
    let output = cmd.assert().get_output().clone();
    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    let stderr = String::from_utf8_lossy(&output.stderr).to_string();
    (output.status.code(), format!("{stdout}{stderr}"))
}

// ============================================================================
// HAPPY PATH — unmodified pack passes the --locked digest re-verification.
// ============================================================================
//
// The precondition runs BEFORE any manifest work (sync.rs line ~357). With no
// ggen.toml the downstream pipeline may still fail, so we do NOT assert overall
// success here — we assert the OBSERVABLE precondition outcome: the digest
// re-verification did NOT fire (no "digest mismatch" / "missing pack" / failed
// re-verification text).
#[test]
fn test_sync_locked_passes_for_unmodified_pack() {
    let world = World::new();
    world.install("io.ggen.happy", "1.0.0");

    let (_code, combined) =
        combined_output(world.sync().arg("--locked").arg("--dry-run").arg("true"));

    assert!(
        !combined.contains("digest mismatch"),
        "unmodified pack must NOT trip digest-mismatch re-verification; output: {combined}"
    );
    assert!(
        !combined.contains("missing pack"),
        "unmodified pack must NOT trip missing-pack re-verification; output: {combined}"
    );
    assert!(
        !combined.contains("digest re-verification failed"),
        "unmodified pack must pass --locked re-verification; output: {combined}"
    );
}

// ============================================================================
// SABOTAGE 1 — mutate the pack on disk after locking => DIGEST MISMATCH.
// ============================================================================
//
// Lock the pack at v1.0.0 (digest pinned), then rewrite the registry TOML to a
// different version so the recomputed digest diverges from the stored one.
#[test]
fn test_sync_locked_fails_on_digest_mismatch_after_mutation() {
    let world = World::new();
    world.install("io.ggen.drift", "1.0.0");

    // Sabotage: mutate the on-disk pack definition (version bump changes digest).
    world.write_pack("io.ggen.drift", "2.0.0");

    let (code, combined) =
        combined_output(world.sync().arg("--locked").arg("--dry-run").arg("true"));

    assert_ne!(
        code,
        Some(0),
        "FAIL-OPEN DEFECT: `ggen sync --locked` must exit non-zero when a locked \
         pack was mutated on disk. Got exit {code:?}, output: {combined}"
    );
    assert!(
        combined.contains("digest mismatch"),
        "error must reference 'digest mismatch'; output: {combined}"
    );
    assert!(
        combined.contains("io.ggen.drift"),
        "error must name the drifted pack; output: {combined}"
    );
}

// ============================================================================
// SABOTAGE 2 — remove the pack definition on disk => MISSING PACK.
// ============================================================================
//
// Delete the registry TOML so the pack can no longer be re-loaded to recompute
// its digest. Re-verification must treat this as a missing pack and hard-fail.
#[test]
fn test_sync_locked_fails_when_pack_definition_removed() {
    let world = World::new();
    world.install("io.ggen.gone", "1.0.0");

    // Sabotage: remove the on-disk pack definition.
    fs::remove_file(world.registry_pack_path("io.ggen.gone")).expect("remove registry pack toml");
    assert!(!world.registry_pack_path("io.ggen.gone").exists());

    let (code, combined) =
        combined_output(world.sync().arg("--locked").arg("--dry-run").arg("true"));

    assert_ne!(
        code,
        Some(0),
        "FAIL-OPEN DEFECT: `ggen sync --locked` must exit non-zero when a locked \
         pack's definition is missing on disk. Got exit {code:?}, output: {combined}"
    );
    assert!(
        combined.contains("missing pack"),
        "error must reference 'missing pack'; output: {combined}"
    );
    assert!(
        combined.contains("io.ggen.gone"),
        "error must name the missing pack; output: {combined}"
    );
}
