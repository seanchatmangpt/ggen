//! Dedicated lock for `[law].gates` inputs in `SyncReport::closure` — the
//! surface the closure-golden fixture declared only as a single
//! `gates/allow.rq` entry. Pins, with real fixtures and the real sync
//! pipeline (Chicago; no mocks):
//!
//! 1. multiple gate files all join the closure with live BLAKE3 hashes;
//! 2. gate drift: mutating a gate file between syncs makes the closure
//!    value track the new bytes;
//! 3. a gate file that does not exist is a typed `[FM-LAW-012]` refusal
//!    (the closure's `MISSING` marker is unreachable for gates — the file
//!    is read before it is hashed, fail-closed);
//! 4. a literal gate path that escapes the project root (`../evil.rq`) is
//!    refused by manifest semantic validation — a typed `[FM-CONFIG-003]`
//!    path-traversal refusal before the sync pipeline runs, so nothing is
//!    evaluated and no closure is produced (not even for the in-root
//!    gates).
//! 5. the closure is byte-identical across project directories even with
//!    multiple gates (no absolute paths, no filesystem order leakage).

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::sync::{sync, SyncOptions};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "gates-closure"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"

[law]
gates = ["gates/allow-a.rq", "gates/allow-b.rq"]
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:rex a ex:Dog ; ex:name "Rex" .
"#;

// ASK (true = violation); the graph holds neither forbidden individual, so
// both gates pass — but their bytes still join the closure as governing
// inputs.
const GATE_A: &str = r#"
ASK WHERE { ?s a <http://example.org/ForbiddenA> . }
"#;

const GATE_B: &str = r#"
ASK WHERE { ?s a <http://example.org/ForbiddenB> . }
"#;

const TEMPLATE: &str = "---\nto: out.txt\n---\ndog: rex\n";

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::create_dir_all(root.join("gates")).expect("mkdir gates");
    std::fs::write(root.join("gates/allow-a.rq"), GATE_A).expect("write gate a");
    std::fs::write(root.join("gates/allow-b.rq"), GATE_B).expect("write gate b");
    std::fs::write(root.join("templates/dog.tmpl"), TEMPLATE).expect("write template");
}

fn run_sync(root: &Path) -> ggen_engine::sync::SyncReport {
    sync(
        root,
        SyncOptions {
            consumer_mode: Default::default(),
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync must succeed: fixture is gate-conformant")
}

fn blake3_of(root: &Path, rel: &str) -> String {
    let bytes = std::fs::read(root.join(rel)).expect("read gate file");
    blake3::hash(&bytes).to_hex().to_string()
}

#[test]
fn every_declared_gate_joins_closure_with_live_blake3() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    let closure = run_sync(dir.path()).closure;

    for rel in ["gates/allow-a.rq", "gates/allow-b.rq"] {
        let recorded = closure
            .get(rel)
            .unwrap_or_else(|| panic!("closure must contain gate `{rel}`; had {closure:?}"));
        assert_eq!(
            recorded,
            &blake3_of(dir.path(), rel),
            "closure hash for gate `{rel}` must equal live blake3 of on-disk bytes"
        );
    }
}

#[test]
fn gate_drift_tracks_new_bytes_across_syncs() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    let before = run_sync(dir.path()).closure;
    let before_b = before
        .get("gates/allow-b.rq")
        .expect("gate b in closure before drift")
        .clone();

    // Drift: mutate gate b after the first sync, re-run. The closure value
    // must track the new bytes (live hash, never a cached one).
    let drifted = format!("{GATE_B}\n# drifted audit-line\n");
    std::fs::write(dir.path().join("gates/allow-b.rq"), drifted).expect("drift gate b");

    let after = run_sync(dir.path()).closure;
    let after_b = after
        .get("gates/allow-b.rq")
        .expect("gate b in closure after drift");

    assert_ne!(
        &before_b, after_b,
        "closure value must change when the gate file's bytes change"
    );
    assert_eq!(
        after_b,
        &blake3_of(dir.path(), "gates/allow-b.rq"),
        "post-drift closure value must equal the new on-disk bytes' blake3"
    );
    // The untouched sibling gate keeps its original hash.
    assert_eq!(
        after.get("gates/allow-a.rq").expect("gate a stable"),
        &blake3_of(dir.path(), "gates/allow-a.rq")
    );
}

#[test]
fn missing_gate_file_is_typed_fm_law_refusal() {
    let dir = TempDir::new().expect("tempdir");
    scaffold(dir.path());
    std::fs::remove_file(dir.path().join("gates/allow-b.rq")).expect("remove gate b");

    let err = sync(
        dir.path(),
        SyncOptions {
            consumer_mode: Default::default(),
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("a declared gate file that cannot be read must refuse, not pass zero gates");

    let msg = err.to_string();
    assert!(
        msg.contains("[FM-LAW-012]"),
        "missing gate must be a typed FM-LAW-012 refusal; got: {msg}"
    );
    assert!(
        msg.contains("unreadable"),
        "refusal must name the gate-read failure; got: {msg}"
    );
    // PINNED ACTUAL BEHAVIOR: the closure's `MISSING` marker is unreachable
    // for [law].gates — `sync.rs` reads the gate to a string (refusing on
    // error) BEFORE `hash_file_or_missing` runs, so a missing gate is a
    // fail-closed refusal, not a MISSING closure entry.
}

#[test]
fn gate_path_escaping_project_root_is_typed_fm_config_refusal() {
    // A `../…` gate entry is refused at manifest semantic validation, before
    // the pipeline runs: typed `[FM-CONFIG-003]` path-traversal refusal, so
    // the out-of-root file is never read, never evaluated, and never enters
    // a closure (the whole sync aborts — in-root gates included).
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root);

    let escape_rel = "../evil.rq";
    let manifest = GGEN_TOML.replace(
        "[\"gates/allow-a.rq\", \"gates/allow-b.rq\"]",
        &format!("[\"gates/allow-a.rq\", \"gates/allow-b.rq\", \"{escape_rel}\"]"),
    );
    std::fs::write(root.join("ggen.toml"), manifest).expect("rewrite manifest");
    // The out-of-root file exists and is readable — the refusal must not
    // depend on the target missing.
    std::fs::write(
        root.parent().expect("tempdir has a parent").join("evil.rq"),
        GATE_A,
    )
    .expect("write out-of-root gate");

    let err = sync(
        root,
        SyncOptions {
            consumer_mode: Default::default(),
            dry_run: false,
            ..Default::default()
        },
    )
    .expect_err("a gate path escaping the project root must be refused");

    let msg = err.to_string();
    assert!(
        msg.contains("[FM-CONFIG-003]"),
        "escape must be a typed FM-CONFIG-003 refusal; got: {msg}"
    );
    assert!(
        msg.contains("path traversal") && msg.contains("law.gates"),
        "refusal must name the traversal and the law.gates field; got: {msg}"
    );
}

#[test]
fn gates_closure_is_byte_stable_across_project_directories() {
    let a = TempDir::new().expect("tempdir a");
    let b = TempDir::new().expect("tempdir b");
    scaffold(a.path());
    scaffold(b.path());

    let closure_a = run_sync(a.path()).closure;
    let closure_b = run_sync(b.path()).closure;

    assert_eq!(
        closure_a, closure_b,
        "closure must be a pure function of project bytes, not of project location \
         (multiple gates included)"
    );
}
