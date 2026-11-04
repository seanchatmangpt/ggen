//! WO-04 deliverable 3 court: the Turtle -> ggen.ea.graph.v1 projection.
//!
//! Law under test: a graph authored as Turtle (examples/ea_projection/
//! schema/demo-graph.ttl, geag: vocabulary from ontologies/shapes/
//! ea-graph.shacl.ttl) and projected by the single generation rule in
//! examples/ea_projection/ggen.toml must land byte-identical to the
//! checked-in kernel fixture fixtures/marketplace-ea-graph.json, pass the
//! kernel's own parse_graph + validate, be deterministic across repeated
//! syncs, and the per-node-type SELECTs inlined in the template must stay
//! pinned to queries/ea_graph/*.rq (the .rq files are the single source; the
//! template inlines them because the engine's sparql() Tera function accepts
//! query text, not file paths).
//!
//! Real collaborators throughout: the ggen CLI binary runs a real sync
//! against the real manifest; the kernel is exercised in-process via its
//! public API. No mocks. Assert on final state (bytes on disk, kernel
//! verdicts).

// Chicago TDD (.claude/rules/rust/testing.md): unwrap/expect/panic allowed in test code.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

use std::fs;
use std::path::{Path, PathBuf};
use std::process::Command;

const GOLDEN: &str = "fixtures/marketplace-ea-graph.json";

/// Serializes the sync-dependent tests: they share the one ea.graph.json
/// output file, so parallel deletion/regeneration would race.
static SYNC_LOCK: std::sync::Mutex<()> = std::sync::Mutex::new(());

fn repo_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("../..")
}

fn example_dir() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR")).join("examples/ea_projection")
}

/// Locate (or build) the ggen CLI binary.
fn ggen_bin() -> PathBuf {
    if let Ok(path) = std::env::var("GGEN_BIN") {
        return PathBuf::from(path);
    }
    let bin = repo_root().join("target/debug/ggen");
    if !bin.exists() {
        let status = Command::new("cargo")
            .args(["build", "-p", "ggen-cli-lib", "--bin", "ggen"])
            .current_dir(repo_root())
            .status()
            .expect("spawn cargo build");
        assert!(status.success(), "cargo build --bin ggen failed");
    }
    bin
}

/// One real `ggen sync run` over the ea_projection manifest.
fn run_sync(skip_admission: bool) {
    let mut cmd = Command::new(ggen_bin());
    cmd.args(["sync", "run", "--format", "quiet"]);
    if skip_admission {
        // Bootstrap only: the Stage 2c gate requires ea.graph.json to exist
        // BEFORE it can admit it, so the first sync after deleting the output
        // uses the documented opt-out; the admitted run that follows exercises
        // the gate for real.
        cmd.arg("--skip-ea-admission");
    }
    cmd.current_dir(example_dir());
    let out = cmd.output().expect("spawn ggen sync run");
    let stdout = String::from_utf8_lossy(&out.stdout);
    let stderr = String::from_utf8_lossy(&out.stderr);
    assert!(
        out.status.success(),
        "sync run (skip_admission={skip_admission}) failed:\n{stdout}\n{stderr}"
    );
}

#[test]
fn projection_is_byte_identical_to_kernel_fixture() {
    let _guard = SYNC_LOCK.lock().expect("test fixture: constructed value cannot fail");
    fs::remove_file(example_dir().join("ea.graph.json")).ok();
    run_sync(true);
    run_sync(false);
    let got = fs::read(example_dir().join("ea.graph.json")).expect("read projected graph");
    let golden =
        fs::read(Path::new(env!("CARGO_MANIFEST_DIR")).join(GOLDEN)).expect("read golden fixture");
    assert_eq!(
        got, golden,
        "projected ea.graph.json must be byte-identical to the kernel fixture"
    );
}

#[test]
fn projection_passes_kernel_parse_and_validate() {
    let _guard = SYNC_LOCK.lock().expect("test fixture: constructed value cannot fail");
    if !example_dir().join("ea.graph.json").exists() {
        run_sync(true);
        run_sync(false);
    }
    let json = fs::read_to_string(example_dir().join("ea.graph.json")).expect("projected graph");
    let g = match ggen_abb_sbb::parse_graph(&json) {
        Ok(g) => g,
        Err(r) => panic!("parse_graph refused the projected graph: {r:?}"),
    };
    g.validate()
        .unwrap_or_else(|r| panic!("kernel validate refused the projected graph: {r:?}"));
}

#[test]
fn projection_is_deterministic_across_syncs() {
    let _guard = SYNC_LOCK.lock().expect("test fixture: constructed value cannot fail");
    if !example_dir().join("ea.graph.json").exists() {
        run_sync(true);
    }
    run_sync(false);
    let first = fs::read(example_dir().join("ea.graph.json")).expect("read after first sync");
    run_sync(false);
    let second = fs::read(example_dir().join("ea.graph.json")).expect("read after second sync");
    assert_eq!(first, second, "two admitted syncs must be byte-identical");
}

// ---- Text court: template-inlined queries pinned to queries/ea_graph/*.rq --
//
// The template inlines the per-node-type SELECTs (engine constraint: sparql()
// takes query text). This court makes the .rq files the single source by
// demanding whitespace-normalized equality between each .rq file and its
// inline copy; any drift fails loudly here.

const QUERY_MAP: &[(&str, &str)] = &[
    // (template set-name, .rq file stem); capability etc.
    ("caps", "capability"),
    ("abbs", "abb"),
    ("contracts", "contract"),
    ("sbbs", "candidate_sbb"),
    ("quals", "qualification"),
    ("packs", "pack"),
];

fn normalize(s: &str) -> String {
    s.split_whitespace().collect::<Vec<_>>().join(" ")
}

#[test]
fn inline_template_queries_match_rq_files() {
    let tpl = fs::read_to_string(example_dir().join("templates/ea_graph.json.tera"))
        .expect("read template");
    for (set_name, stem) in QUERY_MAP {
        let file_text =
            fs::read_to_string(example_dir().join(format!("queries/ea_graph/{stem}.rq")))
                .unwrap_or_else(|e| panic!("read queries/ea_graph/{stem}.rq: {e}"));
        let needle = format!("set {} = sparql(query='", set_name);
        let start = tpl
            .find(&needle)
            .unwrap_or_else(|| panic!("template must inline query for set {set_name}"));
        let rest = &tpl[start + needle.len()..];
        let end = rest.find('\'').expect("unterminated inline query");
        let inline = &rest[..end];
        assert_eq!(
            normalize(inline),
            normalize(&file_text),
            "inline template query for {set_name} has drifted from queries/ea_graph/{stem}.rq"
        );
        // Determinism law: every per-node-type SELECT carries ORDER BY ?id.
        assert!(
            file_text.contains("ORDER BY ?id"),
            "queries/ea_graph/{stem}.rq must carry ORDER BY ?id (determinism law)"
        );
    }
}

#[test]
fn manifest_rule_binds_strategy_query_and_kernel_output() {
    let manifest = fs::read_to_string(example_dir().join("ggen.toml")).expect("read ggen.toml");
    assert!(
        manifest.contains(r#"query = { file = "queries/ea_graph/strategy.rq" }"#),
        "rule-level query must bind queries/ea_graph/strategy.rq"
    );
    assert!(
        manifest.contains(r#"output_file = "ea.graph.json""#),
        "rule output must be ea.graph.json (the Stage 2c gate's admission subject)"
    );
    assert!(
        manifest.contains(r#"imports = ["../../../../ontologies/shapes/ea-graph.shacl.ttl"]"#),
        "manifest must import ontologies/shapes/ea-graph.shacl.ttl"
    );
}
