#![allow(clippy::unwrap_used, clippy::expect_used)]
use ggen_abb_sbb::synthetic_graph;
use ggen_mcp::tools::ea_manufacture::{ea_manufacture, EaManufactureParams};
use ggen_mcp::tools::ea_plan::{ea_plan, EaPlanParams};
use ggen_mcp::tools::ea_replay::{ea_replay, EaReplayParams};
use ggen_mcp::tools::ea_verify_receipt::{ea_verify_receipt, EaVerifyReceiptParams};
use tempfile::TempDir;

#[test]
fn lifecycle_plan_manufacture_verify_replay() {
    let dir = TempDir::new().unwrap();
    let g = synthetic_graph(2, 2);
    std::fs::write(dir.path().join("ea.graph.json"), serde_json::to_string_pretty(&g).unwrap()).unwrap();
    let root = dir.path().to_string_lossy().into_owned();
    assert!(ea_plan(&EaPlanParams { root: root.clone(), graph_path: None, abb: None, authority: Some("SELECT".into()) }).unwrap().ok);
    let mfg = ea_manufacture(&EaManufactureParams { root: root.clone(), graph_path: None, abb: None, sbb: None, authority: Some("CONSTRUCT".into()), expected_graph_digest: None, generation: None, baseline_generation: None, generator_id: Some("ggen-abb-sbb".into()), generator_version: Some("26.9.26".into()) }).unwrap();
    assert!(mfg.ok);
    let receipt = mfg.receipt.unwrap();
    assert!(ea_verify_receipt(&EaVerifyReceiptParams { receipt_path: None, receipt: Some(receipt.clone()) }).unwrap().ok);
    assert!(ea_replay(&EaReplayParams { root, graph_path: None, receipt, generator_id: Some("ggen-abb-sbb".into()), generator_version: Some("26.9.26".into()) }).unwrap().ok);
}
