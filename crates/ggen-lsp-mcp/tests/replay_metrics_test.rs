//! MCP-REPLAY-1 — MCP agents can replay and measure, not just route. The MCP
//! replay/metrics tools return exactly what the direct ggen-lsp calls return
//! (route + proof + replay parity).

use std::fs;
use std::path::Path;

use ggen_lsp::{check_files_in_root, compute_metrics, mine, verify_promotion};
use ggen_lsp_mcp::{metrics_result, replay_case_result};
use tempfile::TempDir;

fn args(json: serde_json::Value) -> Option<serde_json::Map<String, serde_json::Value>> {
    json.as_object().cloned()
}

/// Drive a real cycle so there is OCEL evidence + a promotion to replay/measure.
fn seed_cycle(root: &Path) {
    let rq = root.join("q.rq");
    fs::write(&rq, "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n").expect("write");
    check_files_in_root(root, &[rq], true).capture(root);
    mine(root).expect("mine");
}

#[test]
fn mcp_metrics_matches_direct_compute() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    seed_cycle(root);

    let via_mcp = metrics_result(args(serde_json::json!({ "root": root.to_str().unwrap() })))
        .expect("metrics result");
    let direct = serde_json::to_value(compute_metrics(root)).expect("ser metrics");
    assert_eq!(via_mcp, direct, "MCP metrics == direct compute_metrics");
}

#[test]
fn mcp_replay_verifies_promotion_binding() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    seed_cycle(root);

    // No case_id → verify the promotion binding (tamper check).
    let via_mcp = replay_case_result(args(serde_json::json!({ "root": root.to_str().unwrap() })))
        .expect("replay result");
    let direct = serde_json::to_value(verify_promotion(root)).expect("ser replay");
    assert_eq!(via_mcp, direct, "MCP replay == direct verify_promotion");
    assert_eq!(via_mcp["matches"], serde_json::json!(true), "fresh promotion replays");
}

#[test]
fn mcp_replay_reconstructs_a_specific_case() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    seed_cycle(root);

    // A nonexistent case id → found:false, but a valid (non-error) reconstruction.
    let v = replay_case_result(args(serde_json::json!({
        "root": root.to_str().unwrap(),
        "case_id": "deadbeef"
    })))
    .expect("replay result");
    assert_eq!(v["found"], serde_json::json!(false));
    let direct = serde_json::to_value(ggen_lsp::replay_case(root, "deadbeef")).expect("ser");
    assert_eq!(v, direct, "MCP replay_case == direct replay_case");
}
