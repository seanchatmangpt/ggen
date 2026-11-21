//! Chicago: agent completes dry_run → apply → verify without HITL (confirm in args).

#![allow(clippy::unwrap_used, clippy::expect_used)]

use std::path::Path;

use ggen_lsp::a2a_mcp::lifecycle::{apply, dry_run, receipt_verify, LifecycleApplyParams, LifecycleDryRunParams};
use ggen_lsp::a2a_mcp::{
    dispatch_lifecycle_tool, ea_admit_result, ea_plan_result, LifecycleEaAdmitParams,
    LifecycleEaPlanParams,
};
use serde_json::json;

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
fn apply_refuses_without_expected_graph_hash() {
    let dir = tempfile::tempdir().unwrap();
    scaffold(dir.path());
    let err = apply(&LifecycleApplyParams {
        root: dir.path().display().to_string(),
        confirm: true,
        expected_graph_hash: String::new(),
    })
    .unwrap_err();
    assert!(err.to_string().contains("expected_graph_hash"));
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

#[test]
fn dispatch_lifecycle_wire_matches_direct_handlers() {
    let dir = tempfile::tempdir().unwrap();
    scaffold(dir.path());
    let root = dir.path().display().to_string();

    let dry = dispatch_lifecycle_tool(
        "ggen.lifecycle.dry_run",
        &json!({ "root": root }),
    )
    .expect("dispatch dry_run");
    let hash = dry["graph_hash"].as_str().expect("hash");

    dispatch_lifecycle_tool(
        "ggen.lifecycle.apply",
        &json!({ "root": root, "confirm": true, "expected_graph_hash": hash }),
    )
    .expect("dispatch apply");

    let verify = dispatch_lifecycle_tool("ggen.lifecycle.receipt_verify", &json!({ "root": root }))
        .expect("dispatch verify");
    assert_eq!(verify["valid"], json!(true));

    let plan = ea_plan_result(LifecycleEaPlanParams {
        graph: Some(dir.path().join("ea.graph.json").display().to_string()),
        abb: None,
        authority: Some("SELECT".to_string()),
    })
    .expect("ea_plan");
    assert_eq!(plan["ok"], json!(true));

    let admit = ea_admit_result(LifecycleEaAdmitParams {
        graph: Some(dir.path().join("ea.graph.json").display().to_string()),
        abb: None,
        sbb: Some("sbb:ingest-0000".to_string()),
        authority: Some("CONSTRUCT".to_string()),
        expected_graph_digest: None,
        generation: None,
        baseline_generation: None,
    })
    .expect("ea_admit");
    assert_eq!(admit["ok"], json!(true));
}