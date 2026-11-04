//! WO-01 Stage 2c wiring e2e (2026-11-03) — fail-closed EA gate, envelope
//! caps translation, digest pinning. Chicago: real files, real kernel, real
//! sync pipeline; assertions on final state; no mocks.
//!
//! Owned surface: crates/ggen-engine/src/sync.rs Stage 2c region +
//! crates/ggen-engine/src/envelope.rs (abb_envelope_caps).

#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::similar_names
)]

use ggen_engine::envelope::abb_envelope_caps;
use ggen_engine::sync::{sync, SyncOptions};
use std::path::Path;
use tempfile::TempDir;

const GGEN_TOML: &str = r#"
[project]
name = "stage2c"

[ontology]
source = "ontology.ttl"

[templates]
dir = "templates"
"#;

const ONTOLOGY: &str = r"
@prefix ex: <http://example.org/> .
ex:subject a ex:Item .
";

// `envelope: http://ggen.org/envelope#tight` binds this template's run to the envelope grant
// declared in .specify/envelope-shapes.ttl.
const TEMPLATE_TIGHT: &str = "---\nto: out/result.txt\nenvelope: http://ggen.org/envelope#tight\n---\nhello\n";

fn scaffold(root: &Path, template: &str) {
    std::fs::write(root.join("ggen.toml"), GGEN_TOML).expect("write ggen.toml");
    std::fs::write(root.join("ontology.ttl"), ONTOLOGY).expect("write ontology");
    std::fs::create_dir_all(root.join("templates")).expect("mkdir templates");
    std::fs::write(root.join("templates/main.tmpl"), template).expect("write template");
}

fn write_ea_graph(root: &Path, g: &ggen_abb_sbb::EaGraph) {
    let ea_json = serde_json::to_string_pretty(g).expect("serialize EA graph");
    std::fs::write(root.join("ea.graph.json"), ea_json).expect("write ea.graph.json");
}

fn reticle(artifacts: u64) -> String {
    format!(
        "@prefix res: <http://ggen.org/envelope#> .\n\
         <http://ggen.org/envelope#tight> a res:Envelope ;\n\
         \x20 res:maxArtifacts {artifacts} .\n"
    )
}

fn write_reticle(root: &Path, artifacts: u64) {
    std::fs::create_dir_all(root.join(".specify")).expect("mkdir .specify");
    std::fs::write(
        root.join(".specify/envelope-shapes.ttl"),
        reticle(artifacts),
    )
    .expect("write reticle");
}

// ── Test 1: envelope cap tighter than contract → EXCEEDS_CONTRACT ──────

#[test]
fn envelope_cap_tighter_than_contract_refuses_admission() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root, TEMPLATE_TIGHT);
    // Contract allows 8 artifacts; envelope tightens the kernel ceiling to
    // 2, so the effective ceiling is min(8, 2) = 2 < 8 → EXCEEDS_CONTRACT.
    write_ea_graph(root, &ggen_abb_sbb::synthetic_graph(1, 8));
    write_reticle(root, 2);

    let err = sync(root, SyncOptions::default()).expect_err("tight envelope must refuse");
    let msg = err.to_string();
    assert!(
        msg.contains("EXCEEDS_CONTRACT"),
        "expected EXCEEDS_CONTRACT in refusal, got: {msg}"
    );
    assert!(
        !root.join("out/result.txt").exists(),
        "refused sync must write nothing"
    );
}

#[test]
fn envelope_cap_within_contract_admits() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root, TEMPLATE_TIGHT);
    // Envelope ceiling 16 vs contract 8 → effective min(16→clamped to
    // kernel default 64, 8) = 8 = SBB artifact count → admits.
    write_ea_graph(root, &ggen_abb_sbb::synthetic_graph(1, 8));
    write_reticle(root, 16);

    let report = sync(root, SyncOptions::default()).expect("must admit within caps");
    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("out/result.txt")]
    );
}

#[test]
fn translation_maps_max_artifacts_literal() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root, TEMPLATE_TIGHT);
    write_reticle(root, 2);

    let caps = abb_envelope_caps(root, Some("http://ggen.org/envelope#tight")).expect("grant must translate");
    assert_eq!(caps.max_artifacts, 2);
}

#[test]
fn translation_absent_reticle_yields_none() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root, TEMPLATE_TIGHT);

    assert!(abb_envelope_caps(root, Some("http://ggen.org/envelope#tight")).is_none());
    assert!(abb_envelope_caps(root, None).is_none());
}

// ── Test 2: missing ea.graph.json refuses without opt-out ──────────────

#[test]
fn missing_ea_graph_refuses_by_default() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root, TEMPLATE_TIGHT);

    let err = sync(root, SyncOptions::default())
        .expect_err("missing ea.graph.json must refuse by default");
    let msg = err.to_string();
    assert!(
        msg.contains("ea.graph.json") && msg.contains("--skip-ea-admission"),
        "refusal must name the absent file and the opt-out flag, got: {msg}"
    );
    assert!(
        !root.join("out/result.txt").exists(),
        "refused sync must write nothing"
    );
}

#[test]
fn missing_ea_graph_admits_with_opt_out_flag() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root, TEMPLATE_TIGHT);

    let report = sync(
        root,
        SyncOptions {
            skip_ea_admission: true,
            ..SyncOptions::default()
        },
    )
    .expect("explicit opt-out must admit");
    assert_eq!(
        report.written,
        vec![std::path::PathBuf::from("out/result.txt")]
    );
}

// ── Test 3: digest pinning refuses a mutated subject ───────────────────

#[test]
fn pinned_digest_refuses_mutated_graph() {
    // Pipeline-level pinning binds the Request to the exact parsed subject
    // (expected_graph_digest: Some(ea_graph.digest()) at the Stage 2c call
    // site), so a mutation between parse and admit refuses. This test
    // witnesses the gate on the exact kernel call shape the pipeline now
    // makes: real graph parsed from disk bytes, digest computed, subject
    // mutated, re-admitted through the same admit_with_caps seam.
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    scaffold(root, TEMPLATE_TIGHT);
    let g = ggen_abb_sbb::synthetic_graph(1, 1);
    write_ea_graph(root, &g);

    let content = std::fs::read_to_string(root.join("ea.graph.json")).expect("read back");
    let parsed = ggen_abb_sbb::parse_graph(&content).expect("parse");
    let digest = parsed.digest();

    // Mutate the graph after the digest is taken (the "mutate between
    // parse and admit" subject) and pin the stale digest.
    let mut mutated = parsed.clone();
    mutated.qualifications.clear();

    let req = ggen_abb_sbb::Request {
        abb: "abb:event-ingest".into(),
        sbb: "sbb:ingest-0000".into(),
        requested_authority: ggen_abb_sbb::Authority::Construct,
        expected_graph_digest: Some(digest.clone()),
        receipt_generation: 0,
        artifact_baseline_generation: 0,
    };
    let refusal = ggen_abb_sbb::admit_with_caps(&mutated, &req, None)
        .expect_err("stale pinned digest must refuse");
    assert!(
        refusal.to_string().contains("GRAPH_DIGEST_MISMATCH"),
        "expected GRAPH_DIGEST_MISMATCH, got: {refusal}"
    );

    // Positive control: the un-mutated subject with the same pinned digest
    // admits through the identical call shape.
    let admitted = ggen_abb_sbb::admit_with_caps(&parsed, &req, None)
        .expect("unmutated subject with pinned digest must admit");
    assert_eq!(admitted.graph_digest(), digest);
}


