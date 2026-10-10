//! OCEL/receipt determinism court (Lane X, ocel-determinism).
//!
//! Sync output is a function of (inputs, decisions), never of (machine, clock).
//! OCEL evidence must satisfy the same law. Real binary, real TempDir, no mocks.
//!
//! Root causes found while landing this court:
//!
//! 1. CliHarness::cargo_bin falls back to a PATH search, which resolves to an
//!    ambient ggen (26.9.28) predating the telemetry redirect d2a92c400. Engine
//!    tests that believed they ran this workspace were running the installed
//!    binary, which still writes .clap-noun-verb/{ocel.json,receipts.jsonl} into
//!    the consumer tree with wall-clock event ids (dep ocel.rs:206 generate_event_id),
//!    Utc::now() times (ocel.rs:202), PID+nanos process ids (ocel.rs:215) and
//!    per-invocation duration_ms (ocel.rs:1170). That is the whole flake class.
//!    This court resolves the workspace binary explicitly (building if absent).
//! 2. With the workspace binary the redirect
//!    (crates/ggen-cli/src/lib.rs:125-137) holds: telemetry goes to
//!    /tmp/ggen-cli-telemetry, not the project. .ggen-v2/ is append-only chain
//!    history by design (sync.rs:276-278, 4020-4036; ts_ns pinned 0, sync.rs:15-19),
//!    so it is excluded from byte-identity like book_gap_closure_e2e::tree_digest,
//!    and instead asserted clock-free directly.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};
use std::sync::OnceLock;

use tempfile::TempDir;

const GGEN_TOML: &str = r#"

[project]
name = "ocel-determinism"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
ex:bob ex:name "bob" .
"#;

const TEMPLATE: &str = "---\nto: run/names.txt\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name\n---\n{% for row in results %}{{ row.name }}\n{% endfor %}";

/// Resolve (building if needed) the workspace `ggen` binary. Never a PATH
/// lookup: an ambient install would silently test the wrong tree.
fn workspace_ggen() -> PathBuf {
    static BIN: OnceLock<PathBuf> = OnceLock::new();
    BIN.get_or_init(|| {
        if let Some(exe) = option_env!("CARGO_BIN_EXE_ggen") {
            return PathBuf::from(exe);
        }
        let manifest_dir = Path::new(env!("CARGO_MANIFEST_DIR"));
        let workspace_root = manifest_dir
            .parent()
            .and_then(|p| p.parent())
            .expect("crate lives at <workspace>/crates/ggen-engine");
        let target_dir = std::env::var("CARGO_TARGET_DIR")
            .map(PathBuf::from)
            .unwrap_or_else(|_| workspace_root.join("target"));
        let exe = target_dir.join("debug").join("ggen");
        if !exe.exists() {
            let cargo = std::env::var("CARGO").unwrap_or_else(|_| "cargo".into());
            let out = std::process::Command::new(cargo)
                .args(["build", "-p", "ggen-cli-lib", "--bin", "ggen"])
                .current_dir(workspace_root)
                .output()
                .expect("spawn cargo build for workspace ggen binary");
            assert!(
                out.status.success(),
                "cargo build -p ggen-cli-lib --bin ggen failed: {}",
                String::from_utf8_lossy(&out.stderr)
            );
        }
        assert!(
            exe.exists(),
            "workspace ggen binary not found at {} after build",
            exe.display()
        );
        exe
    })
    .clone()
}
fn run_ggen(project: &Path, args: &[&str]) -> std::process::Output {
    std::process::Command::new(workspace_ggen())
        .args(args)
        .current_dir(project)
        .env_remove("CLAP_NOUN_VERB_OCEL_PATH")
        .env_remove("CLAP_NOUN_VERB_RECEIPT_PATH")
        .env("TMPDIR", "/tmp")
        .output()
        .expect("spawn ggen")
}

fn assert_success(output: &std::process::Output, context: &str) {
    assert!(
        output.status.success(),
        "{context} failed: {}",
        String::from_utf8_lossy(&output.stderr)
    );
}

fn fresh_project() -> TempDir {
    let dir = TempDir::new().expect("tempdir");
    std::fs::write(dir.path().join("ggen.toml"), GGEN_TOML).expect("write manifest");
    std::fs::write(dir.path().join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(dir.path().join("templates")).expect("mkdir templates");
    std::fs::write(dir.path().join("templates/names.tmpl"), TEMPLATE).expect("write template");
    dir
}

/// Content fingerprint (relative path -> blake3 hex), recursively.
fn content_fingerprint(root: &Path, skip_receipts: bool) -> BTreeMap<PathBuf, String> {
    fn walk(
        dir: &Path,
        root: &Path,
        skip_receipts: bool,
        out: &mut BTreeMap<PathBuf, String>,
    ) {
        for entry in std::fs::read_dir(dir).expect("read_dir") {
            let path = entry.expect("dir entry").path();
            let rel = path.strip_prefix(root).unwrap_or(&path).to_path_buf();
            if path.is_dir() {
                walk(&path, root, skip_receipts, out);
            } else {
                if skip_receipts && rel.starts_with(".ggen-v2") {
                    continue;
                }
                let bytes = std::fs::read(&path).expect("read file");
                out.insert(rel, blake3::hash(&bytes).to_hex().to_string());
            }
        }
    }
    let mut out = BTreeMap::new();
    walk(root, root, skip_receipts, &mut out);
    out
}
/// Two syncs on an unchanged project: generated tree is content-identical,
/// and no per-invocation telemetry lands in the consumer tree.
#[test]
fn two_syncs_on_unchanged_project_are_content_identical() {
    let dir = fresh_project();
    let root = dir.path();

    let out = run_ggen(root, &["sync", "run"]);
    assert_success(&out, "first sync run");
    let before = content_fingerprint(root, true);

    let out = run_ggen(root, &["sync", "run"]);
    assert_success(&out, "second sync run");
    let after = content_fingerprint(root, true);

    assert_eq!(before, after, "second sync changed generated outputs");

    assert!(
        !root.join(".clap-noun-verb").exists(),
        ".clap-noun-verb telemetry leaked into the consumer tree on sync"
    );
}

/// The sync receipt must be a function of (inputs, decisions): no wall clock.
/// ts_ns is pinned to 0 (sync.rs:15-19) and no timestamp-ish field may appear.
#[test]
fn sync_receipt_is_clock_free() {
    let dir = fresh_project();
    let root = dir.path();

    let out = run_ggen(root, &["sync", "run"]);
    assert_success(&out, "sync run");

    let receipt_text =
        std::fs::read_to_string(root.join(".ggen-v2/receipt.json")).expect("read receipt");
    let receipt: serde_json::Value = serde_json::from_str(&receipt_text).expect("parse receipt");

    let ts = receipt["record"]["ts_ns"]
        .as_u64()
        .expect("record.ts_ns present and numeric");
    assert_eq!(ts, 0, "record.ts_ns must be pinned to 0, got {ts}");

    for banned in ["duration_ms", "wall_clock", "elapsed"] {
        assert!(
            !receipt_text.contains(banned),
            "receipt contains clock-derived field `{banned}`"
        );
    }
}

/// Read-only commands must not create .clap-noun-verb telemetry in the
/// consumer tree (the cli_read_only_invariant_matrix contract, end to end).
#[test]
fn read_only_commands_do_not_emit_ocel_into_the_project_tree() {
    let dir = fresh_project();
    let root = dir.path();

    // Seed one sync so `receipt verify` has a receipt to inspect (exit code is
    // irrelevant to the read-only contract; the invariant matrix tolerates
    // nonzero exits — only the disk must not change).
    let out = run_ggen(root, &["sync", "run"]);
    assert_success(&out, "seed sync run");

    for args in [
        vec!["graph", "validate"],
        vec!["receipt", "verify"],
        vec!["doctor", "run"],
    ] {
        let out = run_ggen(root, &args);
        assert!(
            out.status.success(),
            "{args:?} failed: {}",
            String::from_utf8_lossy(&out.stderr)
        );
    }

    assert!(
        !root.join(".clap-noun-verb").exists(),
        ".clap-noun-verb telemetry leaked into the consumer tree on read-only commands"
    );
}
