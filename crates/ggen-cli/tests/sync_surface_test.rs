//! Chicago-TDD pins for the real `ggen sync` CLI surface, driven through the
//! real `ggen` binary (assert_cmd) against a real fixture project in
//! `TempDir`s. No mocks.
//!
//! Pinned surface (probed against ggen 26.10.10, 2026-10-10):
//! 1. `ggen sync` (no subcommand) **defaults to `sync run`** — on a valid
//!    fixture it exits 0 and writes outputs; on a manifest-less dir it fails
//!    with FM-CONFIG-001 (proving the run verb executed, not a usage error).
//! 2. `ggen sync run --dry-run` exits 0, writes NO template outputs and NO
//!    `.ggen-v2/` receipt; JSON reports `"planned: write (dry-run)"`. (The
//!    JSON `written` array still lists the filename — a cosmetic quirk, NOT
//!    evidence of disk writes; this test pins the disk state.)
//! 3. `ggen sync run` exits 0, writes outputs + `.ggen-v2/receipt.json`.
//! 4. `ggen sync run --watch` starts a real watch loop and does not exit on
//!    its own: spawned with a try_wait poll, still running after 3s (no
//!    instant error), then killed. We assert liveness, not hang-forever.
//! 5. `ggen sync run --audit` is refused by clap with a message naming the
//!    flag (exit code 2) — regression guard for the justfile bug class
//!    (`--audit true` / `--dry_run true` value-taking forms never existed).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::{Path, PathBuf};
use std::process::{Command, Stdio};
use std::time::{Duration, Instant};

use assert_cmd::Command as AssertCommand;
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "demo"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = "@prefix ex: <http://example.org/> .\nex:alice ex:name \"alice\" .\n";

const TEMPLATE: &str = "---\nto: out/names.txt\nforce: true\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}";

/// Resolve the real `ggen` binary: `CARGO_BIN_EXE_ggen` when set, else the
/// workspace `target/{debug,release}/ggen`. Panics loudly if unresolvable.
fn ggen_bin() -> PathBuf {
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_ggen") {
        let p = PathBuf::from(path);
        if p.exists() {
            return p;
        }
    }
    let manifest_dir = std::env::var_os("CARGO_MANIFEST_DIR")
        .map(PathBuf::from)
        .expect("CARGO_MANIFEST_DIR set");
    let mut dir: &Path = manifest_dir.as_path();
    loop {
        if dir.join("Cargo.lock").exists() {
            for profile in ["debug", "release"] {
                let candidate = dir.join("target").join(profile).join("ggen");
                if candidate.is_file() {
                    return candidate;
                }
            }
        }
        match dir.parent() {
            Some(p) => dir = p,
            None => panic!("could not resolve `ggen` binary; build with `cargo build -p ggen-cli-lib --bin ggen`"),
        }
    }
}

/// Real fixture project: ggen.toml + ontology.ttl + templates/one.tmpl.
fn scaffold() -> TempDir {
    let dir = TempDir::new().expect("tempdir");
    std::fs::write(dir.path().join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(dir.path().join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir templates");
    std::fs::write(dir.path().join("templates/one.tmpl"), TEMPLATE).expect("write template");
    dir
}

fn ggen(cwd: &Path) -> AssertCommand {
    let mut cmd = AssertCommand::new(ggen_bin());
    cmd.current_dir(cwd);
    cmd
}

fn output_written(dir: &Path) -> bool {
    dir.join("out").join("names.txt").exists()
}

fn receipt_written(dir: &Path) -> bool {
    dir.join(".ggen-v2").join("receipt.json").exists()
}

/// Case 1: bare `ggen sync` defaults to the `run` verb — it executes the
/// pipeline (exits 0, writes outputs on a valid fixture), it does not print
/// usage and it does not require the literal `run` subcommand.
#[test]
fn bare_sync_defaults_to_run() {
    let dir = scaffold();
    ggen(dir.path())
        .args(["sync"])
        .assert()
        .success()
        .stdout(predicates::str::contains("graph_hash_hex"));
    assert!(output_written(dir.path()), "bare sync must write outputs");
    assert!(receipt_written(dir.path()), "bare sync must write receipt");
}

/// Case 1b: bare `ggen sync` in a manifest-less dir fails with the run verb's
/// config error (FM-CONFIG-001), proving the default routes to run — not a
/// clap usage error, not a silent no-op.
#[test]
fn bare_sync_without_manifest_is_run_verb_config_error() {
    let dir = TempDir::new().expect("tempdir");
    ggen(dir.path())
        .args(["sync"])
        .assert()
        .failure()
        .stderr(predicates::str::contains("FM-CONFIG-001"));
}

/// Case 2: `--dry-run` exits 0, plans the write in JSON, but writes nothing
/// to disk: no template outputs, no `.ggen-v2/` receipt.
#[test]
fn dry_run_exits_zero_and_writes_nothing() {
    let dir = scaffold();
    ggen(dir.path())
        .args(["sync", "run", "--dry-run"])
        .assert()
        .success()
        .stdout(predicates::str::contains("planned: write (dry-run)"));
    assert!(
        !output_written(dir.path()),
        "dry-run must not write outputs"
    );
    assert!(
        !receipt_written(dir.path()),
        "dry-run must not write receipt"
    );
}

/// Case 3: a real run writes template outputs and the `.ggen-v2/receipt.json`.
#[test]
fn real_run_writes_outputs_and_receipt() {
    let dir = scaffold();
    ggen(dir.path())
        .args(["sync", "run"])
        .assert()
        .success()
        .stdout(predicates::str::contains("graph_hash_hex"));
    assert!(output_written(dir.path()));
    assert!(receipt_written(dir.path()));
}

/// Case 4: `--watch` starts and stays alive (it is a long-running loop, not a
/// one-shot). Spawn with a try_wait poll: after 3s it must still be running
/// with no instant error — then we kill it. Documented pin: watch is
/// long-lived; this asserts "started successfully and did not exit early",
/// not the full watch semantics.
#[test]
fn watch_starts_and_runs_until_killed() {
    let dir = scaffold();
    let mut child = Command::new(ggen_bin())
        .args(["sync", "run", "--watch"])
        .current_dir(dir.path())
        .stdout(Stdio::null())
        .stderr(Stdio::null())
        .spawn()
        .expect("spawn ggen sync run --watch");

    let deadline = Instant::now() + Duration::from_secs(3);
    while Instant::now() < deadline {
        match child.try_wait().expect("poll watch child") {
            Some(status) => {
                panic!("--watch exited early with {status}; it must be a long-running loop");
            }
            None => std::thread::sleep(Duration::from_millis(100)),
        }
    }
    // Reaching here without an early exit is the pin: watch survived 3s.
    child.kill().expect("kill watch child");
    child.wait().expect("reap watch child");
}

/// Case 5: the nonexistent `--audit` flag is refused by clap, with the flag
/// named in the error. Regression guard for the justfile bug class where
/// `ggen sync --audit true` / `--dry_run true` were invoked for months and
/// failed with exactly this clap error.
#[test]
fn unknown_audit_flag_is_refused_by_clap() {
    let dir = scaffold();
    ggen(dir.path())
        .args(["sync", "run", "--audit"])
        .assert()
        .failure()
        .stderr(predicates::str::contains("--audit"));
}
