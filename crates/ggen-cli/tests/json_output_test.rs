//! Chicago-TDD integration tests for the global `--json` flag (Lane Q, cli-json).
//!
//! `--json` is expanded by `ggen_cli_lib`'s argv preprocessor into
//! clap-noun-verb's native global `--format json` (compact machine-readable
//! output), covering every verb whose return value flows through
//! `OutputFormat::format` — including the ggen-engine linkme nouns
//! (`doctor`, `graph`, `law`, `sync`, `receipt`) and the `pack doctor` verb.
//!
//! Real collaborators only: each test spawns the actual `ggen` binary in a
//! `TempDir` project and asserts on its real stdout. No mocks.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)] // Chicago TDD: real-IO tests

use std::process::Command;
use tempfile::TempDir;

fn ggen_bin() -> &'static str {
    env!("CARGO_BIN_EXE_ggen")
}

/// `doctor run` requires a parseable `ggen.toml` plus a readable templates dir
/// ([FM-CONFIG-001]/[FM-CONFIG-003]/[FM-CONFIG-004] otherwise). Note: the
/// `ggen init` scaffold itself currently fails strict-mode parse
/// (FM-CONFIG-003, pre-existing, init lane's problem) — this hand-minimal
/// frontmatter manifest is the smallest config doctor run accepts.
fn temp_project() -> TempDir {
    let dir = TempDir::new().expect("temp project dir");
    std::fs::write(
        dir.path().join("ggen.toml"),
        "[project]\nname = \"probe\"\n[ontology]\nsource = \"schema/domain.ttl\"\n[templates]\ndir = \"templates\"\n",
    )
    .expect("write minimal ggen.toml");
    std::fs::create_dir(dir.path().join("templates")).expect("create templates dir");
    dir
}

/// `ggen --json doctor run` prints compact JSON on stdout that parses as a
/// `serde_json::Value` object with the doctor report's expected keys.
#[test]
fn json_flag_doctor_run_stdout_is_compact_json_object() {
    let dir = temp_project();
    let output = Command::new(ggen_bin())
        .args(["--json", "doctor", "run"])
        .current_dir(dir.path())
        .env("TMPDIR", "/tmp")
        .output()
        .expect("spawn ggen");

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    assert!(
        output.status.success(),
        "ggen --json doctor run failed (exit {:?}); stderr: {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stderr)
    );

    let value: serde_json::Value = serde_json::from_str(stdout.trim())
        .unwrap_or_else(|e| panic!("stdout is not JSON: {e}\nstdout: {stdout}"));
    let obj = value
        .as_object()
        .unwrap_or_else(|| panic!("stdout JSON is not an object: {stdout}"));

    // doctor run reports health checks; assert its report shape, not prose.
    for key in ["healthy", "checks"] {
        assert!(
            obj.contains_key(key),
            "doctor report missing key `{key}`; keys: {:?}",
            obj.keys().collect::<Vec<_>>()
        );
    }

    // Compact (machine-readable), not the default pretty rendering: the whole
    // document is a single line.
    assert_eq!(
        stdout.trim().lines().count(),
        1,
        "--json output must be compact single-line JSON, got:\n{stdout}"
    );
}

/// Plain mode (no `--json`) keeps the pre-existing default rendering:
/// pretty-printed JSON (clap-noun-verb's `OutputFormat::JsonPretty` default),
/// multi-line, parseable — byte-shape unchanged, zero drift.
#[test]
fn plain_mode_doctor_run_stays_pretty_multiline_json() {
    let dir = temp_project();
    let output = Command::new(ggen_bin())
        .args(["doctor", "run"])
        .current_dir(dir.path())
        .env("TMPDIR", "/tmp")
        .output()
        .expect("spawn ggen");

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    assert!(
        output.status.success(),
        "plain ggen doctor run failed (exit {:?}); stderr: {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stderr)
    );

    // Stable substrings of the default (pretty) rendering.
    assert!(
        stdout.contains("\"healthy\""),
        "plain output missing `\"healthy\"` key:\n{stdout}"
    );
    assert!(
        stdout.contains('\n'),
        "plain output must remain multi-line pretty JSON, got:\n{stdout}"
    );
    let value: serde_json::Value = serde_json::from_str(stdout.trim())
        .unwrap_or_else(|e| panic!("plain stdout is not JSON: {e}\nstdout: {stdout}"));
    assert!(value.is_object(), "plain stdout is not a JSON object");
}

/// `pack doctor` under `--json` also yields a compact parseable JSON object
/// with the pack-health keys.
#[test]
fn json_flag_pack_doctor_stdout_is_compact_json_object() {
    let dir = temp_project();
    let output = Command::new(ggen_bin())
        .args(["--json", "pack", "doctor"])
        .current_dir(dir.path())
        .env("TMPDIR", "/tmp")
        .output()
        .expect("spawn ggen");

    let stdout = String::from_utf8_lossy(&output.stdout).to_string();
    assert!(
        output.status.success(),
        "ggen --json pack doctor failed (exit {:?}); stderr: {}",
        output.status.code(),
        String::from_utf8_lossy(&output.stderr)
    );

    let value: serde_json::Value = serde_json::from_str(stdout.trim())
        .unwrap_or_else(|e| panic!("stdout is not JSON: {e}\nstdout: {stdout}"));
    let obj = value
        .as_object()
        .unwrap_or_else(|| panic!("stdout JSON is not an object: {stdout}"));
    for key in ["healthy", "cache_dir", "pack_count", "checks"] {
        assert!(
            obj.contains_key(key),
            "pack doctor report missing key `{key}`; keys: {:?}",
            obj.keys().collect::<Vec<_>>()
        );
    }
    assert_eq!(
        stdout.trim().lines().count(),
        1,
        "must be compact:\n{stdout}"
    );
}
