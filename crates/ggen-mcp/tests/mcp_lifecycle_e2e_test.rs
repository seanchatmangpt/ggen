//! In-process μ-sync MCP lifecycle (no CLI/HITL):
//! classify → check → (ea_plan/admit when ea.graph.json present) →
//! sync_dry_run → write_apply(confirm + hash) → receipt_verify.
//!
//! ## WIP inventory (receipt — no TODO markers in production sources)
//!
//! - `a2a_mcp` / `signal_dispatch` / `unattended_dispatch` / `write_apply`:
//!   no `TODO`/`FIXME`/`unimplemented!`; CP39 whole-project-clean limitation
//!   documented in `signal_dispatch_e2e_test.rs`.
//! - A2A `ggen.construct` / `ggen.lifecycle.*` live on `ggen-lsp` feature `a2a`.
//! - This test exercises the stdio `ggen-mcp` tool handlers directly.
#![allow(clippy::unwrap_used, clippy::expect_used, clippy::panic)]

mod common;

use std::path::Path;

use common::write_frontmatter_project;

use ggen_mcp::tools::check_project::{check_project, CheckProjectParams};
use ggen_mcp::tools::config_classify::{config_classify, ConfigClassifyParams};
use ggen_mcp::tools::ea_admit::{ea_admit, EaAdmitParams};
use ggen_mcp::tools::ea_plan::{ea_plan, EaPlanParams};
use ggen_mcp::tools::receipt_verify::{receipt_verify, ReceiptVerifyParams};
use ggen_mcp::tools::sync_dry_run::{sync_dry_run, SyncDryRunParams};
use ggen_mcp::tools::write_apply::{write_apply, WriteApplyParams};
use serde_json::json;
use tempfile::TempDir;

const EA_GRAPH_FIXTURE: &str = include_str!("../../ggen-abb-sbb/fixtures/ea-graph.json");

fn root_str(tmp: &TempDir) -> String {
    tmp.path().display().to_string()
}

#[test]
fn full_mcp_tool_lifecycle_in_process() {
    let tmp = TempDir::new().expect("tempdir");
    write_frontmatter_project(tmp.path());
    std::fs::write(tmp.path().join("ea.graph.json"), EA_GRAPH_FIXTURE).expect("ea graph");

    let classify = config_classify(&ConfigClassifyParams {
        root: root_str(&tmp),
    })
    .expect("classify");
    assert_eq!(classify.schema, "frontmatter");

    let check = check_project(&CheckProjectParams {
        root: root_str(&tmp),
        paths: None,
        with_routes: false,
    })
    .expect("check");
    assert!(check.ok);
    assert!(!check.has_errors, "errors: {:?}", check.report);
    assert_eq!(check.error_count, 0);
    assert!(check.files_checked > 0);

    let root = root_str(&tmp);
    let plan = ea_plan(&EaPlanParams {
        root: root.clone(),
        graph_path: None,
        abb: None,
        authority: Some("SELECT".to_string()),
    })
    .expect("ea_plan");
    assert!(plan.ok);
    let decision = plan
        .decision
        .expect("admitted plan must carry decision JSON");
    assert!(decision.get("sbb").is_some() || decision.get("abb").is_some());

    let admit = ea_admit(&EaAdmitParams {
        root: root.clone(),
        graph_path: None,
        abb: None,
        sbb: Some("sbb:ingest-0000".to_string()),
        authority: Some("CONSTRUCT".to_string()),
        expected_graph_digest: None,
        generation: None,
        baseline_generation: None,
    })
    .expect("ea_admit");
    assert!(admit.ok);
    let admitted = admit
        .admitted
        .expect("admit ok must carry admitted payload");
    assert_eq!(admitted["sbb"], json!("sbb:ingest-0000"));
    assert!(admitted["graph_digest"]
        .as_str()
        .is_some_and(|d| !d.is_empty()));

    let dry = sync_dry_run(&SyncDryRunParams { root: root.clone() }).expect("dry run");
    assert!(!dry.graph_hash.is_empty());
    assert!(dry.write_count >= 1);
    assert!(
        dry.would_write
            .iter()
            .any(|w| w.path.replace('\\', "/").ends_with("out/names.txt")),
        "dry run must plan names output: {:?}",
        dry.would_write
    );

    let apply = write_apply(&WriteApplyParams::new(
        root.clone(),
        true,
        dry.graph_hash.clone(),
    ))
    .expect("write_apply");
    assert!(apply.ok);
    let out = tmp.path().join("out/names.txt");
    assert!(Path::new(&out).exists());
    let body = std::fs::read_to_string(&out).expect("read names output");
    assert!(body.contains("alice"));
    assert!(body.contains("bob"));

    let receipt =
        receipt_verify(&ReceiptVerifyParams { root: root.clone() }).expect("receipt_verify");
    assert!(receipt.valid, "{receipt:?}");
    assert!(Path::new(&tmp.path().join(".ggen-v2/receipt.json")).exists());
}

#[test]
fn write_apply_refuses_without_confirm_or_hash_on_same_fixture() {
    let tmp = TempDir::new().expect("tempdir");
    write_frontmatter_project(tmp.path());
    let root = root_str(&tmp);
    let dry = sync_dry_run(&SyncDryRunParams { root: root.clone() }).expect("dry");

    let no_confirm = write_apply(&WriteApplyParams::new(
        root.clone(),
        false,
        dry.graph_hash.clone(),
    ))
    .expect_err("confirm:false must refuse");
    assert!(
        no_confirm.message.contains("confirm"),
        "refusal must name confirm: {:?}",
        no_confirm
    );

    let no_hash = write_apply(&WriteApplyParams::new(root, true, String::new()))
        .expect_err("empty hash must refuse");
    assert!(
        no_hash.message.contains("expected_graph_hash"),
        "refusal must name expected_graph_hash: {:?}",
        no_hash
    );
}
