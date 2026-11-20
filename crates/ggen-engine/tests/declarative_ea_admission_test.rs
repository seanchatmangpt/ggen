//! DEF-K6-2 (2026-11-03) — the declarative-rules (`[[generation.rules]]`)
//! `ggen.toml` path previously had ZERO Stage 2c wiring: repo-root EA
//! admission never executed even with a clean receipt chain. After the hoist
//! (`sync::run_ea_graph_admission`, called from `sync()`'s Stage 0
//! declarative branch before `generation_rules::run`), a repo-root
//! declarative manifest runs the SAME admission as the frontmatter path.
//!
//! Chicago: real files, real kernel, real pipeline; assertions on final
//! state; no mocks.
//!
//! Owned surface: crates/ggen-engine/src/sync.rs (declarative branch +
//! hoisted `run_ea_graph_admission`); G1's landed Stage 2c region is called,
//! never edited.

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::similar_names
)]

use ggen_engine::sync::{sync, SyncOptions};
use std::path::Path;
use tempfile::TempDir;

/// Minimal valid declarative-rules `ggen.toml` (schema shape per
/// `config_schema_dispatch_e2e.rs`'s accepted fixture).
const GGEN_TOML: &str = r#"
[project]
name = "declarative-ea"
version = "1.0.0"

[ontology]
source = "ontology.ttl"

[[generation.rules]]
name = "names"
query = { inline = "SELECT ?name WHERE { ?s <http://example.org/name> ?name } ORDER BY ?name" }
template = { inline = "{% for row in results %}{{ row.name }}\n{% endfor %}" }
output_file = "out/names.txt"
"#;

const ONTOLOGY: &str = r#"
@prefix ex: <http://example.org/> .
ex:alice ex:name "alice" .
"#;

fn scaffold(root: &Path) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
}

fn write_ea_graph(root: &Path, g: &ggen_abb_sbb::EaGraph) {
    let ea_json = serde_json::to_string_pretty(g).expect("serialize EA graph");
    std::fs::write(root.join("ea.graph.json"), ea_json).expect("write ea.graph.json");
}

// ── 1. valid graph → sync proceeds past Stage 2c, real output lands ────

#[test]
fn declarative_manifest_valid_graph_admits_and_writes() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root);
    // Real kernel fixture: 1 SBB, 8 artifacts ≤ kernel DEFAULT_MAX_ARTIFACTS
    // (declarative path passes `None` caps → kernel default ceiling 64).
    write_ea_graph(root, &ggen_abb_sbb::synthetic_graph(1, 8));

    let report = sync(root, SyncOptions::default())
        .expect("declarative sync with a valid EA graph must admit and proceed");
    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("out/names.txt")]
    );
    let content = std::fs::read_to_string(root.join("out/names.txt")).expect("read output");
    assert_eq!(content, "alice\n");
}

// ── 2. unqualified SBB → FM-LAW-015 refusal, nothing written ───────────

#[test]
fn declarative_manifest_unqualified_sbb_refuses_fm_law_015() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root);
    // Candidate SBB with NO Qualified qualification → kernel SBB_UNQUALIFIED
    // at admit (validate() passes — it only checks dangling references).
    let mut g = ggen_abb_sbb::synthetic_graph(1, 8);
    g.qualifications.clear();
    write_ea_graph(root, &g);

    let err = sync(root, SyncOptions::default())
        .expect_err("unqualified SBB must refuse the declarative sync");
    let msg = err.to_string();
    assert!(
        msg.contains("FM-LAW-015"),
        "expected FM-LAW-015 in refusal, got: {msg}"
    );
    assert!(
        msg.contains("SBB_UNQUALIFIED") || msg.contains("sbb:ingest-0000"),
        "refusal must name the kernel refusal or the SBB, got: {msg}"
    );
    assert!(
        !root.join("out/names.txt").exists(),
        "refused sync must write nothing"
    );
}

// ── 3. absent graph, no opt-out → fail-closed refusal ──────────────────

#[test]
fn declarative_manifest_missing_graph_refuses_by_default() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root);

    let err = sync(root, SyncOptions::default())
        .expect_err("missing ea.graph.json must refuse the declarative sync by default");
    let msg = err.to_string();
    assert!(
        msg.contains("ea.graph.json") && msg.contains("--skip-ea-admission"),
        "refusal must name the absent file and the opt-out flag, got: {msg}"
    );
    assert!(
        !root.join("out/names.txt").exists(),
        "refused sync must write nothing"
    );
}

// ── 4. skip flag → skips admission, sync proceeds ──────────────────────

#[test]
fn declarative_manifest_skip_flag_admits() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root);

    let report = sync(
        root,
        SyncOptions {
            skip_ea_admission: true,
            ..SyncOptions::default()
        },
    )
    .expect("explicit opt-out must admit on the declarative path");
    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("out/names.txt")]
    );
}
