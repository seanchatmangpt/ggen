//! Chicago: agent completes dry_run → apply → verify without HITL (confirm in args).

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::path::Path;

use ggen_lsp::a2a_mcp::lifecycle::{apply, dry_run, receipt_verify, LifecycleApplyParams, LifecycleDryRunParams};

const EA_GRAPH_FIXTURE: &str = include_str!("../../ggen-abb-sbb/fixtures/ea-graph.json");

fn scaffold(root: &Path) {
    std::fs::write(
        root.join("ggen.toml"),
        "[project]\nname = \"a2a-lifecycle\"\n\n[ontology]\nsource = \"ontology.ttl\"\n\n[templates]\ndir = \"templates\"\n",
    )
    .unwrap();
    std::fs::write(
        root.join("ontology.ttl"),
        "@prefix ex: <http://example.org/> .\nex:alice ex:hasName \"alice\" .\n",
    )
    .unwrap();
    std::fs::create_dir_all(root.join("templates")).unwrap();
    std::fs::write(
        root.join("templates/names.tmpl"),
        "---\nto: out/names.txt\nsparql:\n  people: SELECT ?name WHERE { ?s <http://example.org/hasName> ?name } ORDER BY ?name\n---\n{% for row in people %}{{ row.name }}\n{% endfor %}",
    )
    .unwrap();
    std::fs::write(root.join("ea.graph.json"), EA_GRAPH_FIXTURE).unwrap();
}

#[test]
fn apply_refuses_without_confirm() {
    let dir = tempfile::tempdir().unwrap();
    scaffold(dir.path());
    let err = apply(&LifecycleApplyParams {
        root: dir.path().display().to_string(),
        confirm: false,
        expected_graph_hash: "x".into(),
    })
    .unwrap_err();
    assert!(err.to_string().contains("confirm"));
}

#[test]
fn lifecycle_dry_run_apply_verify_without_hitl() {
    let dir = tempfile::tempdir().unwrap();
    let root = dir.path();
    scaffold(root);
    let root_s = root.display().to_string();

    let dry = dry_run(&LifecycleDryRunParams {
        root: root_s.clone(),
    })
    .expect("dry_run");
    assert!(!dry.graph_hash.is_empty());
    assert!(!dry.decisions.is_empty());

    let applied = apply(&LifecycleApplyParams {
        root: root_s.clone(),
        confirm: true,
        expected_graph_hash: dry.graph_hash,
    })
    .expect("apply");
    assert!(applied.write_count > 0);
    assert!(root.join(".ggen-v2/receipt.json").exists());
    assert!(receipt_verify(&root_s).expect("verify"));
}