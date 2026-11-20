//! Golden lock for the G2 sync-hardening item 2 per-subject graph diff:
//! `.ggen-v2/graph-digest.jsonl` (per-subject BLAKE3 ledger),
//! `SyncReport::graph_diff` / `ReceiptPayload::graph_diff`, and
//! `.ggen-v2/sync-diff.md`. Pins the three documented behaviors:
//!
//! 1. baseline run — first sync writes the ledger, reports no diff;
//! 2. term-changed run — an ontology edit surfaces as `added`/`changed`
//!    subjects attributed to the changed source doc;
//! 3. emission-neutral run — source bytes changed, terms did not → the
//!    loud note, both in the report and the persisted receipt payload.
//!
//! Chicago: real filesystem fixture in a fresh `TempDir` per run, real sync
//! pipeline via the library API, ledger/summary/payload read back off disk.
//! No mocks.

#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::path::Path;

use ggen_engine::sync::{sync, GraphDiff, ReceiptPayload, SyncOptions, SyncReceipt, SyncReport};
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "graph-diff-golden"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY_V1: &str = r#"
@prefix ex: <http://example.org/> .
ex:rex a ex:Dog ; ex:name "Rex" .
"#;

const ONTOLOGY_V2: &str = r#"
@prefix ex: <http://example.org/> .
ex:rex a ex:Dog ; ex:name "Rex" ; ex:age "5" .
"#;

const TEMPLATE_V1: &str = "---\nto: out.txt\nforce: true\n---\nhello v1\n";
const TEMPLATE_V2: &str = "---\nto: out.txt\nforce: true\n---\nhello v2\n";

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY_V1).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/dog.tmpl"), TEMPLATE_V1).expect("write template");
}

fn run_sync(root: &Path) -> SyncReport {
    sync(
        root,
        SyncOptions {
            // FM-LAW-015 EA-admission opt-out (fixture declares no EA graph)
            skip_ea_admission: true,
            dry_run: false,
            ..Default::default()
        },
    )
    .expect("sync must succeed: fixture is conformant")
}

fn read_payload(root: &Path) -> ReceiptPayload {
    let raw = std::fs::read_to_string(root.join(".ggen-v2/receipt.json")).expect("receipt");
    serde_json::from_str::<SyncReceipt>(&raw)
        .expect("receipt.json deserializes")
        .payload
}

#[test]
fn baseline_run_writes_ledger_and_reports_no_diff() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root);

    let report = run_sync(root);

    // Baseline: first sync reports no diff (there is no previous ledger).
    assert!(report.graph_diff.is_none(), "baseline must report no diff");

    // The ledger IS written, one JSON line per subject, and covers the
    // ontology's two subjects.
    let ledger = std::fs::read_to_string(root.join(".ggen-v2/graph-digest.jsonl"))
        .expect("ledger must exist after baseline run");
    let subjects: Vec<String> = ledger
        .lines()
        .filter(|l| !l.trim().is_empty())
        .map(|l| {
            let v: serde_json::Value = serde_json::from_str(l).expect("ledger line parses");
            v["subject"]
                .as_str()
                .expect("subject is a string")
                .to_string()
        })
        .collect();
    assert!(
        subjects.iter().any(|s| s.contains("example.org/rex")),
        "ledger must cover ex:rex; had {subjects:?}"
    );
    assert_eq!(subjects.len(), 1, "one subject line, sorted, deduped");

    // Baseline summary says so, and the payload carries no graph_diff.
    let summary = std::fs::read_to_string(root.join(".ggen-v2/sync-diff.md")).expect("summary");
    assert!(summary.contains("Baseline run"), "summary: {summary}");
    assert!(read_payload(root).graph_diff.is_none());

    // Second identical run: empty diff, no note.
    let report2 = run_sync(root);
    let diff = report2.graph_diff.as_ref().expect("diff present on run 2");
    assert!(diff.added.is_empty() && diff.removed.is_empty() && diff.changed.is_empty());
    assert!(diff.note.is_none(), "no diagnostic on a clean re-sync");
    // Ledger byte-stable across identical runs.
    let ledger2 =
        std::fs::read_to_string(root.join(".ggen-v2/graph-digest.jsonl")).expect("ledger2");
    assert_eq!(
        ledger, ledger2,
        "ledger must be byte-stable across identical runs"
    );
}

#[test]
fn term_changed_run_reports_added_subject_attributed_to_source() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root);
    run_sync(root);

    // Change the ontology: adds a triple on an existing subject AND a new
    // subject, removes none.
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY_V2).expect("rewrite ontology");
    let report = run_sync(root);

    let diff = report.graph_diff.as_ref().expect("diff present");
    assert!(
        diff.changed.iter().any(|s| s.contains("example.org/rex")),
        "ex:rex changed; diff = {diff:?}"
    );
    assert!(diff.added.is_empty() && diff.removed.is_empty());
    assert!(diff.note.is_none());

    // Attribution: single changed source doc → named per moved subject.
    let attr = diff
        .per_source
        .get("http://example.org/rex")
        .expect("rex attributed");
    assert!(attr.contains("ontology.ttl"), "attribution: {attr}");

    // Persisted receipt payload carries the same diff (serde round trip).
    let payload = read_payload(root);
    let pd = payload.graph_diff.as_ref().expect("payload graph_diff");
    assert_eq!(pd, diff, "payload diff equals report diff");

    // Human summary names the change and the source.
    let summary = std::fs::read_to_string(root.join(".ggen-v2/sync-diff.md")).expect("summary");
    assert!(
        summary.contains("changed subjects: 1"),
        "summary: {summary}"
    );
    assert!(summary.contains("ontology.ttl"), "summary: {summary}");
}

#[test]
fn emission_neutral_run_fires_the_loud_note() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root);
    run_sync(root);

    // Emission-neutral patch: template bytes change, graph terms do not.
    std::fs::write(root.join("templates/dog.tmpl"), TEMPLATE_V2).expect("rewrite template");
    let report = run_sync(root);

    let diff = report.graph_diff.as_ref().expect("diff present");
    assert!(diff.added.is_empty() && diff.removed.is_empty() && diff.changed.is_empty());
    let note = diff.note.as_ref().expect("emission-neutral note must fire");
    assert!(note.contains("emission-neutral"), "note: {note}");
    assert!(note.contains("templates/dog.tmpl"), "note: {note}");

    // Persisted payload carries the note through the receipt.
    let payload = read_payload(root);
    let note = payload
        .graph_diff
        .as_ref()
        .and_then(|d| d.note.as_ref())
        .expect("payload carries the emission-neutral note");
    assert!(note.contains("emission-neutral"));

    // Summary carries the note too.
    let summary = std::fs::read_to_string(root.join(".ggen-v2/sync-diff.md")).expect("summary");
    assert!(summary.contains("NOTE"), "summary: {summary}");
}

#[test]
fn dry_run_computes_nothing_and_writes_nothing() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root);

    let report = sync(
        root,
        SyncOptions {
            skip_ea_admission: true,
            dry_run: true,
            ..Default::default()
        },
    )
    .expect("dry run succeeds");

    assert!(
        report.graph_diff.is_none(),
        "dry run over no ledger = baseline"
    );
    assert!(
        !root.join(".ggen-v2/graph-digest.jsonl").exists(),
        "no ledger on dry run"
    );
    assert!(
        !root.join(".ggen-v2/sync-diff.md").exists(),
        "no summary on dry run"
    );
}

// Keep the GraphDiff import referenced even if assertions above change.
#[allow(dead_code)]
fn _type_witness(_d: &GraphDiff) {}
