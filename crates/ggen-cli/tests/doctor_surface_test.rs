#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: real-IO tests
//! Chicago-style integration tests for the `ggen doctor run` surface.
//!
//! Pins the real, observed behavior of `ggen doctor run` (the
//! non-actuating diagnostics verb, `crates/ggen-cli/src/cmds/doctor.rs`)
//! against real fixture projects on disk, running the real compiled `ggen`
//! binary from each fixture's cwd. Output format observed and pinned
//! 2026-10-10 at v26.10.10:
//!
//! - healthy project   -> exit 0, stdout = pretty JSON with `healthy: true`
//!                        and `checks.{lockfile_drift,orphaned_artifacts,receipt_staleness}`
//!                        (`lockfile_drift` is a documented `skip` on the
//!                        declarative-rules schema)
//! - missing ontology  -> exit 1, stderr names the missing file
//!                        (`[FM-CONFIG-003] ... Ontology source not found: ...`)
//! - malformed TOML    -> exit 1, typed `[FM-CONFIG-103]` parse error
//! - empty directory   -> exit 1, graceful typed `[FM-CONFIG-001]`
//!                        "ggen.toml not found" (no panic)
//!
//! No mocks, no stubs: real binary, real files, state-based assertions.

use assert_cmd::Command;
use std::fs;
use std::path::{Path, PathBuf};

/// Resolve the real `ggen` binary the same way the other integration tests do:
/// `CARGO_BIN_EXE_ggen` (set by `cargo test -p ggen-cli-lib`), then the
/// workspace `target/{debug,release}/ggen`, then `PATH`.
fn ggen_bin() -> PathBuf {
    if let Ok(path) = std::env::var("CARGO_BIN_EXE_ggen") {
        let p = PathBuf::from(path);
        if p.exists() {
            return p;
        }
    }

    let target_root = std::env::var_os("CARGO_TARGET_DIR")
        .map(PathBuf::from)
        .or_else(|| {
            let manifest_dir = std::env::var_os("CARGO_MANIFEST_DIR").map(PathBuf::from)?;
            let mut dir: &Path = manifest_dir.as_path();
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
            for name in ["ggen", "ggen.exe"] {
                let candidate = target.join(profile).join(name);
                if candidate.is_file() {
                    return candidate;
                }
            }
        }
    }

    panic!(
        "could not resolve the `ggen` binary: CARGO_BIN_EXE_ggen unset and no \
         target/debug/ggen found; build it with `cargo build -p ggen-cli-lib --bin ggen`"
    );
}

/// The exact minimal valid manifest pinned by observation: a healthy
/// declarative-rules project (project+version/ontology/generation) that
/// `doctor run` exits 0 on.
fn write_healthy_project(dir: &Path) {
    fs::create_dir_all(dir.join("templates")).unwrap();
    fs::write(
        dir.join("ggen.toml"),
        r#"[project]
name = "healthy"
version = "0.1.0"

[ontology]
source = "ontology.ttl"

[generation]
output_dir = "src/gen"

[[generation.rules]]
name = "noop"
query = { inline = "SELECT ?s WHERE { ?s ?p ?o } ORDER BY ?s" }
template = { inline = "{{ s }}" }
output_file = "src/gen/noop.txt"
"#,
    )
    .unwrap();
    fs::write(
        dir.join("ontology.ttl"),
        "@prefix : <http://example.com/> .\n:a :b :c .\n",
    )
    .unwrap();
}

/// Run `ggen doctor run` with cwd = `dir`, returning (exit_code, stdout, stderr).
fn doctor_run(dir: &Path) -> (i32, String, String) {
    let output = Command::new(ggen_bin())
        .args(["doctor", "run"])
        .current_dir(dir)
        .env("TMPDIR", "/tmp")
        .output()
        .expect("failed to spawn the real ggen binary");
    (
        output.status.code().unwrap_or(-1),
        String::from_utf8_lossy(&output.stdout).into_owned(),
        String::from_utf8_lossy(&output.stderr).into_owned(),
    )
}

/// Case 1: healthy minimal project -> exit 0, pretty-JSON stdout with
/// `healthy: true` and the three pinned checks present.
#[test]
fn doctor_run_healthy_project_exits_zero_with_healthy_json() {
    let tmp = tempfile::tempdir_in("/tmp").unwrap();
    write_healthy_project(tmp.path());

    let (code, stdout, stderr) = doctor_run(tmp.path());
    assert_eq!(code, 0, "healthy project must exit 0; stderr: {stderr}");

    let json: serde_json::Value = serde_json::from_str(&stdout)
        .unwrap_or_else(|e| panic!("stdout must be valid JSON ({e}): {stdout}"));
    assert_eq!(json["healthy"], serde_json::json!(true));
    for check in ["lockfile_drift", "orphaned_artifacts", "receipt_staleness"] {
        assert!(
            json["checks"][check].is_object(),
            "missing checks.{check} in: {stdout}"
        );
    }
    // The two checks that have real subjects here pass on a fresh project.
    assert_eq!(json["checks"]["orphaned_artifacts"]["status"], "pass");
    assert_eq!(json["checks"]["receipt_staleness"]["status"], "pass");
}

/// Case 2: manifest points at an ontology that does not exist -> exit 1 and
/// the stderr names the missing piece (the ontology file path).
#[test]
fn doctor_run_missing_ontology_exits_nonzero_naming_missing_file() {
    let tmp = tempfile::tempdir_in("/tmp").unwrap();
    write_healthy_project(tmp.path());
    fs::remove_file(tmp.path().join("ontology.ttl")).unwrap();

    let (code, stdout, stderr) = doctor_run(tmp.path());
    assert_ne!(code, 0, "missing ontology must not exit 0");
    assert!(
        stderr.contains("Ontology source not found"),
        "stderr must name the missing ontology; got: {stderr}"
    );
    assert!(
        stderr.contains("ontology.ttl"),
        "stderr must carry the missing path; got: {stderr}"
    );
    assert!(
        stdout.trim().is_empty(),
        "no JSON body on failure: {stdout}"
    );
}

/// Case 3: malformed ggen.toml (bad TOML) -> exit 1 with the typed
/// `[FM-CONFIG-103]` parse error carrying line/column diagnostics.
#[test]
fn doctor_run_malformed_toml_exits_nonzero_with_typed_error() {
    let tmp = tempfile::tempdir_in("/tmp").unwrap();
    fs::write(tmp.path().join("ggen.toml"), "this is [not toml\n").unwrap();

    let (code, _stdout, stderr) = doctor_run(tmp.path());
    assert_ne!(code, 0, "malformed TOML must not exit 0");
    assert!(
        stderr.contains("[FM-CONFIG-103]"),
        "expected typed FM-CONFIG-103 error; got: {stderr}"
    );
    assert!(
        stderr.contains("TOML parse error"),
        "expected parse diagnostics; got: {stderr}"
    );
}

/// Case 4: empty directory -> graceful typed failure (`[FM-CONFIG-001]`
/// "ggen.toml not found"), never a panic.
#[test]
fn doctor_run_empty_directory_is_graceful_not_panic() {
    let tmp = tempfile::tempdir_in("/tmp").unwrap();

    let (code, _stdout, stderr) = doctor_run(tmp.path());
    assert_ne!(code, 0, "empty dir must not exit 0");
    assert!(
        stderr.contains("[FM-CONFIG-001]"),
        "expected typed FM-CONFIG-001 not-found error; got: {stderr}"
    );
    assert!(
        stderr.contains("ggen.toml not found"),
        "expected not-found message; got: {stderr}"
    );
    assert!(
        !stderr.contains("panicked"),
        "empty dir must not panic; got: {stderr}"
    );
}

/// Case 5: determinism — two `doctor run` invocations over the same healthy
/// project emit byte-identical stdout.
#[test]
fn doctor_run_is_deterministic_identical_stdout_bytes() {
    let tmp = tempfile::tempdir_in("/tmp").unwrap();
    write_healthy_project(tmp.path());

    let (code1, out1, err1) = doctor_run(tmp.path());
    let (code2, out2, err2) = doctor_run(tmp.path());
    assert_eq!(code1, 0, "first run must be healthy: {err1}");
    assert_eq!(code2, 0, "second run must be healthy: {err2}");
    assert_eq!(
        out1.as_bytes(),
        out2.as_bytes(),
        "doctor stdout must be byte-identical across runs"
    );
}
