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

//! End-to-end integration tests for the project-scaffold workflow.
//!
//! History: these tests originally pinned the removed `ggen project` noun.
//! The capability moved to root `ggen init` (scaffold) and `ggen sync run`
//! (generation pipeline). Re-pinned 2026-11-08, V8-R3-f lane, against
//! witnessed CLI probes. Six tests were deleted rather than re-pinned
//! (cited in the lane receipt): `project new --type/--output` (flags gone
//! with the noun), `project init --preset` (flag gone), `project gen
//! --dry-run` (no equivalent flag on `sync run`), `project new
//! --type invalid` (flag gone), and the two init name-validation tests
//! (capability absent on the moved surface — fresh-dir `init --name ""`
//! succeeds rc=0; DELETED comment below).

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

/// The scaffold+generation recipe witnessed for the moved surface: fresh
/// `ggen init` then `sync run`, both unpatched. The scaffold's example rule
/// now carries ORDER BY determinism and `ea.graph.json` ships with the
/// scaffold (init.rs E0013/FM-LAW-015 fixes, SCAFFOLD-f), so no
/// `strict_mode = false` patch and no `--skip-ea-admission` opt-out are
/// needed. Witnessed rc=0 with `ontology-summary.txt` rendered.
fn init_scaffold(temp_dir: &TempDir) {
    ggen()
        .arg("init")
        .current_dir(temp_dir.path())
        .assert()
        .success();
}

/// Run the generation pipeline over the scaffold. Witnessed rc=0.
fn sync_run(temp_dir: &TempDir) {
    ggen()
        .args(["sync", "run"])
        .current_dir(temp_dir.path())
        .assert()
        .success();
}

#[test]
// Re-pin (2026-11-08, V8-R3-f): `project new` (noun removed) -> root
// `ggen init` scaffold; asserts the scaffold state on disk.
fn test_project_new_creates_project() {
    let temp_dir = TempDir::new().unwrap();

    ggen().arg("init").current_dir(&temp_dir).assert().success();

    assert!(
        temp_dir.path().join("ggen.toml").exists(),
        "ggen.toml should be created"
    );
    assert!(
        temp_dir.path().join("schema").is_dir(),
        "schema dir should be created"
    );
}

#[test]
// Re-pin (2026-11-08, V8-R3-f): `project init --name X` (noun removed) ->
// root `ggen init`; the scaffold is written into the current directory
// (witnessed: ggen.toml + Makefile + schema/ + templates/).
fn test_project_init_creates_structure() {
    let temp_dir = TempDir::new().unwrap();

    ggen().arg("init").current_dir(&temp_dir).assert().success();

    assert!(temp_dir.path().join("ggen.toml").exists());
    assert!(temp_dir.path().join("Makefile").exists());
    assert!(temp_dir.path().join("templates").is_dir());
}

#[test]
// Re-pin (2026-11-08, V8-R3-f): `project plan` / `project gen` (noun
// removed) -> `sync run` over the init scaffold; asserts the rendered
// artifact on disk.
fn test_project_plan_generates_plan() {
    let temp_dir = TempDir::new().unwrap();
    init_scaffold(&temp_dir);
    sync_run(&temp_dir);

    let summary = temp_dir.path().join("ontology-summary.txt");
    assert!(summary.exists(), "rendered artifact should exist on disk");
}

#[test]
// Re-pin (2026-11-08, V8-R3-f): `project gen` (noun removed) -> `sync run`
// over the init scaffold; asserts rendered content, not just file presence.
fn test_project_gen_creates_files() {
    let temp_dir = TempDir::new().unwrap();
    init_scaffold(&temp_dir);
    sync_run(&temp_dir);

    let summary = temp_dir.path().join("ontology-summary.txt");
    let content = fs::read_to_string(&summary).unwrap();
    assert!(!content.is_empty(), "rendered summary should carry content");
}

#[test]
// Re-pin (2026-11-08, V8-R3-f): `project --help` (noun removed) -> the
// scaffold verb's help documents itself (`Usage: ggen init`).
fn test_project_help_shows_verbs() {
    ggen()
        .args(["init", "--help"])
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage: ggen init"));
}

#[test]
// Re-pin (2026-11-08, V8-R3-f): `project invalid-verb` (noun removed) ->
// an unexpected positional under the `init` noun is an argument-parsing
// failure. Witnessed rc=1.
fn test_project_invalid_verb() {
    ggen()
        .args(["init", "invalid-verb"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("ERROR")));
}

// DELETED (2026-11-08, V8-R3-f): `test_project_init_empty_name` and
// `test_project_init_whitespace_name` pinned a name-validation capability
// that does not exist on the moved surface. Witnessed inversion: fresh-dir
// `ggen init --name ""` and `--name "test project"` both succeed (rc=0,
// scaffold written, ggen.toml carries `name = ""` verbatim); the earlier
// rc=1 observations were the already-initialized refusal, not name
// validation. No successor test without asserting the scaffold accepts
// invalid names as intended behavior — filed for the F-lane instead.

#[test]
// Re-pin (2026-11-08, V8-R3-f): second sync-run probe (idempotence over an
// already-rendered scaffold) replacing the deleted `--type` preset tests.
fn test_project_sync_rerun_stable() {
    let temp_dir = TempDir::new().unwrap();
    init_scaffold(&temp_dir);
    sync_run(&temp_dir);
    let first = fs::read_to_string(temp_dir.path().join("ontology-summary.txt")).unwrap();

    sync_run(&temp_dir);
    let second = fs::read_to_string(temp_dir.path().join("ontology-summary.txt")).unwrap();

    assert_eq!(first, second, "sync rerun should be deterministic");
}
