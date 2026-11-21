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

    let root = root_str(&tmp);
    let plan = ea_plan(&EaPlanParams {
        root: root.clone(),
        graph_path: None,
        abb: None,
        authority: Some("SELECT".to_string()),
    })
    .expect("ea_plan");
    assert!(plan.ok);

    let admit = ea_admit(&EaAdmitParams {
        root,
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

    let dry = sync_dry_run(&SyncDryRunParams {
        root: root_str(&tmp),
    })
    .expect("dry run");
    assert!(!dry.graph_hash.is_empty());

    let apply = write_apply(&WriteApplyParams::new(root_str(&tmp), true, dry.graph_hash))
        .expect("write_apply");
    assert!(apply.ok);
    assert!(tmp.path().join("out/names.txt").exists());

    let receipt = receipt_verify(&ReceiptVerifyParams {
        root: root_str(&tmp),
    })
    .expect("receipt_verify");
    assert!(receipt.valid, "{receipt:?}");
    assert!(Path::new(&tmp.path().join(".ggen-v2/receipt.json")).exists());
}
