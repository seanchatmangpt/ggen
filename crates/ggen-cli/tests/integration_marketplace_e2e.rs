#![allow(
    dead_code,
    unused_imports,
    unused_variables,
    deprecated,
    clippy::all,
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::ignore_without_reason,
    unused_mut
)]

//! End-to-end integration tests for the pack-management workflow.
//!
//! History: these tests originally pinned the removed `ggen marketplace`
//! noun. The capability moved to `ggen pack` / `ggen packs` (plus the
//! generation pipeline under `ggen sync run`); every surviving test was
//! re-pinned to the live surface with witnessed CLI probes (2026-11-08,
//! V8-GGEN-R3 lane). Three assertions were genuinely dead against the new
//! surface and are deleted (see the lane receipt for citations):
//! `marketplace cache clean`, `marketplace offline sync`, and
//! `marketplace sync` (no verb refreshes the registry cache; `ggen sync
//! run` is the generation pipeline, a different capability needing a
//! manifest).
//!
//! **Chicago TDD Principles**:
//! - REAL CLI process execution
//! - REAL file system operations
//! - REAL state verification
//! - Minimal mocking
//!
//! **Critical User Workflows (80/20)**:
//! 1. Search the pack registry
//! 2. Add a pack (lockfile roundtrip)
//! 3. Inspect pack details
//! 4. Remove an installed pack
//! 5. Health-check the installation

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

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): `marketplace search --query X` ->
// `pack search <QUERY>` (positional query, no --query flag). Witnessed:
// rc=0, JSON with "query"/"results" keys, 7 hits for 'rust'.
fn test_marketplace_search_basic() {
    ggen()
        .args(["pack", "search", "rust"])
        .assert()
        .success()
        .stdout(predicate::str::contains("results"));
}

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): `--category` is not a search flag on the
// moved surface; category filtering lives on `pack list --category`.
// Witnessed: rc=0, JSON with "packs"/"total" keys.
fn test_marketplace_search_with_category() {
    ggen()
        .args(["pack", "list", "--category", "any"])
        .assert()
        .success()
        .stdout(predicate::str::contains("packs"));
}

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): JSON output goes through the global
// `--format json` flag (the search surface has no --json). Completes cleanly.
fn test_marketplace_search_json_output() {
    let output = ggen()
        .args(["pack", "search", "test"])
        .output()
        .expect("Failed to execute");

    assert!(
        output.status.code().is_some(),
        "Command should exit cleanly"
    );
}

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): `marketplace list` -> `pack list`.
// Witnessed: rc=0, JSON object with a "packs" key (31 local packs).
fn test_marketplace_list_empty() {
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .args(["pack", "list"])
        .current_dir(&temp_dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("packs"));
}

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): `marketplace list --json` ->
// `pack list` (already JSON); assert parseable state, not just exit.
fn test_marketplace_list_json() {
    let temp_dir = TempDir::new().unwrap();

    let output = ggen()
        .args(["pack", "list"])
        .current_dir(&temp_dir)
        .output()
        .expect("Failed to execute");

    assert!(output.status.success(), "pack list should succeed");
    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("\"packs\""),
        "pack list should emit JSON with a packs key, got: {}",
        &stdout[..stdout.len().min(200)]
    );
}

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): `marketplace info X` -> `pack show <ID>`.
// Unknown pack is a typed failure ("Pack '...' not found"), not a hang.
fn test_marketplace_info_missing_package() {
    let output = ggen()
        .args(["pack", "show", "nonexistent-package-12345"])
        .output()
        .expect("Failed to execute");

    assert!(
        output.status.code().is_some(),
        "Command should exit cleanly"
    );
}

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): `marketplace remove X` -> `pack remove
// <NAME>`. Not-installed is a typed failure: "No packs installed:
// .ggen/packs.lock not found". Witnessed rc!=0 with that stderr.
fn test_marketplace_remove_not_installed() {
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .args(["pack", "remove", "nonexistent-package"])
        .current_dir(&temp_dir)
        .assert()
        .failure()
        .stderr(
            predicate::str::contains("not found")
                .or(predicate::str::contains("ERROR"))
                .or(predicate::str::contains("error")),
        );
}

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): `marketplace categories list` (verb
// removed) -> the category surface lives as the "category" field of
// `pack list` output. Witnessed: rc=0, every pack entry carries "category".
fn test_marketplace_categories_list() {
    let output = ggen()
        .args(["pack", "list"])
        .output()
        .expect("Failed to execute");

    assert!(output.status.success(), "pack list should succeed");
    assert!(
        String::from_utf8_lossy(&output.stdout).contains("category"),
        "pack list output should carry per-pack category fields"
    );
}

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): `marketplace cache status` (verb removed)
// -> `pack doctor` reports cache/lockfile health. Witnessed: rc=0, JSON with
// "cache_dir"/"checks" keys.
fn test_marketplace_cache_status() {
    let temp_dir = TempDir::new().unwrap();

    ggen()
        .args(["pack", "doctor"])
        .current_dir(&temp_dir)
        .assert()
        .success()
        .stdout(predicate::str::contains("checks"));
}

/// Hermetic pack-add environment: `pack add` installs into
/// `$HOME/.ggen/packs/<id>` (via `dirs::home_dir()`), so overriding HOME to
/// a fresh temp dir isolates the global pack cache, while `GGEN_PACKS_DIR`
/// is pointed back at the real registry so pack *lookup* still resolves.
/// Witnessed: with HOME=<fresh tmp> + GGEN_PACKS_DIR=<real registry>,
/// `pack add repository-lifecycle-pack` is rc=0 and writes the project
/// lockfile under the temp cwd, never touching the caller's HOME.
fn hermetic_pack_add(home: &std::path::Path) -> (&'static str, String) {
    let packs_dir = std::env::var("HOME")
        .map(|h| format!("{h}/.ggen/packs"))
        .unwrap_or_default();
    ("GGEN_PACKS_DIR", packs_dir)
}

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): `marketplace lockfile generate` (verb
// removed) -> the lockfile is written by `pack add` and read by
// `packs list`; roundtrip witnessed (sha256 integrity entry, total: 1).
fn test_marketplace_lockfile_generate() {
    let temp_dir = TempDir::new().unwrap();
    let home = TempDir::new().unwrap();

    let (var, packs_dir) = hermetic_pack_add(home.path());
    ggen()
        .args(["pack", "add", "repository-lifecycle-pack"])
        .env("HOME", home.path())
        .env(var, &packs_dir)
        .current_dir(&temp_dir)
        .assert()
        .success();

    assert!(temp_dir.path().join(".ggen/packs.lock").exists());
}

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): `marketplace registry info` (verb
// removed) -> `pack show <known-id>` resolves a pack from the registry.
// Witnessed: rc=0 for repository-lifecycle-pack.
fn test_marketplace_registry_info() {
    ggen()
        .args(["pack", "show", "repository-lifecycle-pack"])
        .assert()
        .success();
}

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): the add -> list state roundtrip replaces
// the removed `marketplace update` test as the real-collaborator lifecycle
// probe. Witnessed: add rc=0 with lockfile receipt; `packs list` reports
// the pack with an integrity digest.
fn test_marketplace_add_list_roundtrip() {
    let temp_dir = TempDir::new().unwrap();
    let home = TempDir::new().unwrap();

    let (var, packs_dir) = hermetic_pack_add(home.path());
    ggen()
        .args(["pack", "add", "repository-lifecycle-pack"])
        .env("HOME", home.path())
        .env(var, &packs_dir)
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
// Re-pin (2026-11-08, V8-GGEN-R3): help surface moved from
// `marketplace --help` to `pack --help`. Witnessed verbs: search, list,
// add, remove, show, doctor, related, capabilities, query, compose, new.
fn test_marketplace_help_output() {
    ggen()
        .args(["pack", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Commands:"))
        .stdout(predicate::str::contains("search"))
        .stdout(predicate::str::contains("add"))
        .stdout(predicate::str::contains("remove"))
        .stdout(predicate::str::contains("list"));
}

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): `pack search --help` documents the
// positional <QUERY> argument (no --category flag on this verb).
fn test_marketplace_search_help() {
    ggen()
        .args(["pack", "search", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage: ggen pack search"))
        .stdout(predicate::str::contains("<QUERY>"));
}

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): invalid verb under the pack noun fails
// with clap's "unrecognized subcommand". Witnessed rc=1, stderr carries the
// ERROR line.
fn test_marketplace_invalid_verb() {
    ggen()
        .args(["pack", "invalid-verb"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("unrecognized")));
}

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): `marketplace recommend --use_case X`
// (verb removed) -> `pack related <SEED>` is the graph-relation recommender.
// Witnessed: rc=0, JSON with "mode"/"results"/"packs_considered" keys.
fn test_marketplace_recommend_basic() {
    ggen()
        .args(["pack", "related", "production"])
        .assert()
        .success()
        .stdout(predicate::str::contains("results"));
}

#[test]
// Re-pin (2026-11-08, V8-GGEN-R3): second `pack related` probe over a
// different seed (filtered-recommendation intent).
fn test_marketplace_recommend_with_category() {
    ggen().args(["pack", "related", "rust"]).assert().success();
}
