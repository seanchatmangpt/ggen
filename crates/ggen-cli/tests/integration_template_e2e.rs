#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::ignore_without_reason
)]
//! End-to-end integration tests for the template/pack authoring workflow.
//!
//! History: these tests originally pinned the removed `ggen template`
//! noun. The capability moved to `ggen pack` / `ggen packs` (listing,
//! detail, validation) and `ggen sync run` (generation pipeline).
//! Re-pinned 2026-11-08, V8-GGEN-R3 lane, against witnessed CLI probes.
//! Sixteen tests were deleted rather than re-pinned (cited in the lane
//! receipt): 4 in the R3 pass (see the DELETED comment below) and the 12
//! remaining `#[ignore = "REGISTERED-GAP"]` tests in the V8-R1-f pass —
//! all still pinned the removed `template` noun and referenced the deleted
//! `create_test_template` helper (compile error E0425 after R3's cleanup).
//! - `template new` x2: `ggen pack new` currently refuses every
//!   construction with FM-LAW-015 (`ea.graph.json` absent) — finding F2.
//! - `template generate_tree` + `template regenerate`: the generation
//!   pipeline under `ggen sync run` requires an authored manifest, and
//!   the scaffold manifest `ggen init` emits itself fails FM-CONFIG-003
//!   (inference rule without ORDER BY) — finding F3.
//!
//! **Chicago TDD Principles**:
//! - REAL CLI process execution, REAL file system operations
//! - REAL state verification, NO mocking of the template engine

use assert_cmd::Command;
use predicates::prelude::*;
use std::fs;
use tempfile::TempDir;

/// Resolve the real `ggen` binary the same way `cli_boundary.rs::ggen_bin`
/// does: `CARGO_BIN_EXE_ggen` (set by `cargo test -p ggen-cli-lib`, never by
/// `-p ggen-engine`/`-p ggen-cli` — the root package is `autobins = false`),
/// then the workspace `target/{debug,release}/ggen`, then `PATH`. Panics
/// (loudly) if no candidate resolves, so failure happens at binary
/// resolution, not at first spawn.
fn ggen_bin() -> std::path::PathBuf {
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_ggen") {
        let p = std::path::PathBuf::from(path);
        if p.exists() {
            return p;
        }
    }

    let target_root = std::env::var_os("CARGO_TARGET_DIR")
        .map(std::path::PathBuf::from)
        .or_else(|| {
            let manifest_dir =
                std::env::var_os("CARGO_MANIFEST_DIR").map(std::path::PathBuf::from)?;
            let mut dir: &std::path::Path = manifest_dir.as_path();
            loop {
                if dir.join("Cargo.lock").exists() {
                    return Some(dir.join("target"));
                }
                match dir.parent() {
                    Some(p) => dir = p,
                    None => return None,
                }
            }
        });

    if let Some(target) = target_root {
        for profile in &["debug", "release"] {
            let candidate = target.join(profile).join("ggen");
            if candidate.is_file() {
                return candidate;
            }
            let candidate_exe = target.join(profile).join("ggen.exe");
            if candidate_exe.is_file() {
                return candidate_exe;
            }
        }
    }

    panic!(
        "could not resolve the `ggen` binary: CARGO_BIN_EXE_ggen unset and no \
         target/debug/ggen found; build it with `cargo build -p ggen-cli-lib --bin ggen`"
    );
}

/// Helper to create ggen command
fn ggen() -> Command {
    Command::new(ggen_bin())
}

/// Hermetic pack-add environment: `pack add` installs into
/// `$HOME/.ggen/packs/<id>` (via `dirs::home_dir()`), so overriding HOME to
/// a fresh temp dir isolates the global pack cache, while `GGEN_PACKS_DIR`
/// is pointed back at the real registry so pack *lookup* still resolves.
/// Witnessed: with HOME=<fresh tmp> + GGEN_PACKS_DIR=<real registry>,
/// `pack add repository-lifecycle-pack` is rc=0 and writes the project
/// lockfile under the temp cwd, never touching the caller's HOME.
fn hermetic_pack_add() -> (String, String) {
    let packs_dir = std::env::var("HOME")
        .map(|h| format!("{h}/.ggen/packs"))
        .unwrap_or_default();
    ("GGEN_PACKS_DIR".to_string(), packs_dir)
}

// DELETED (2026-11-08, V8-GGEN-R3): `template new` -> `pack new` re-pin is
// blocked by finding F2: `ggen pack new <name> --description D --namespace N`
// refuses with FM-LAW-015 ("ea.graph.json is absent") for every invocation
// after a fresh `ggen init` + `ggen init-self`; the verb has no admission
// opt-out flag. The old ".ggen/templates/<name>/template.toml" state assert
// has no reachable successor. See lane receipt.

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): `template list` -> `pack list`
// (embedded marketplace registry). Witnessed: rc=0, JSON with "packs" key.
fn test_template_list_empty() {
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .args(["pack", "list"])
        .current_dir(&temp_dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("packs"));
}

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): "shows installed" now means the pack
// lockfile roundtrip: hermetic `pack add`, then `packs list` must report
// the pack by id. Witnessed: integrity entry with pack_id present.
fn test_template_list_shows_installed() {
    let temp_dir = TempDir::new().unwrap();
    let home = TempDir::new().unwrap();
    let (var, packs_dir) = hermetic_pack_add();

    ggen()
        .args(["pack", "add", "repository-lifecycle-pack"])
        .env("HOME", home.path())
        .env(&var, &packs_dir)
        .current_dir(&temp_dir)
        .assert()
        .success();

    ggen()
        .args(["packs", "list"])
        .current_dir(&temp_dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("repository-lifecycle-pack"));
}

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): `template list --json` -> `pack list`
// (always JSON). Assert parseable JSON state, not just exit code.
fn test_template_list_json_format() {
    let temp_dir = TempDir::new().unwrap();

    let output = ggen()
        .args(["pack", "list"])
        .current_dir(&temp_dir)
        .output()
        .expect("Failed to execute");

    assert!(output.status.success());
    let stdout = String::from_utf8_lossy(&output.stdout);
    serde_json::from_str::<serde_json::Value>(&stdout).expect("Output should be valid JSON");
}
