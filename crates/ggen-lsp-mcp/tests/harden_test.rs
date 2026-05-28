//! MCP-HARDEN-1 — the MCP route server is a production-grade non-editor surface:
//! root-aware, input-bounded, structured refusals, never panics.

use ggen_lsp_mcp::{build_repair_routes_in, repair_route_result};

fn args(json: serde_json::Value) -> Option<serde_json::Map<String, serde_json::Value>> {
    json.as_object().cloned()
}

#[test]
fn missing_arguments_is_a_structured_refusal() {
    assert!(repair_route_result(None).is_err(), "no args → invalid_params, not panic");
}

#[test]
fn missing_required_field_is_a_structured_refusal() {
    let r = repair_route_result(args(serde_json::json!({ "file_path": "q.rq" })));
    assert!(r.is_err(), "missing file_content → invalid_params");
}

#[test]
fn empty_file_path_is_refused() {
    let r = repair_route_result(args(serde_json::json!({
        "file_path": "",
        "file_content": "x"
    })));
    assert!(r.is_err(), "empty file_path → invalid_params");
}

#[test]
fn oversized_content_is_refused() {
    let big = "a".repeat((1 << 20) + 1);
    let r = repair_route_result(args(serde_json::json!({
        "file_path": "q.rq",
        "file_content": big
    })));
    assert!(r.is_err(), "oversized content → invalid_params, not OOM/panic");
}

#[test]
fn non_law_surface_is_not_an_error() {
    let v = repair_route_result(args(serde_json::json!({
        "file_path": "notes.md",
        "file_content": "# hi"
    })))
    .expect("non-law-surface is a valid result, not an error");
    assert_eq!(v["is_law_surface"], serde_json::json!(false));
}

#[test]
fn valid_input_returns_envelopes() {
    let v = repair_route_result(args(serde_json::json!({
        "file_path": "q.rq",
        "file_content": "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n"
    })))
    .expect("valid input");
    assert_eq!(v["is_law_surface"], serde_json::json!(true));
    assert!(!v["envelopes"].as_array().expect("envelopes").is_empty());
}

#[test]
fn root_authority_is_explicit_not_cwd_magic() {
    // An empty/nonexistent root simply yields seed routes (no pack) — no panic,
    // and route authority is bound to the passed root, not the process cwd.
    let v = build_repair_routes_in(
        Some("/nonexistent-root-xyz"),
        "q.rq",
        "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n",
    );
    assert_eq!(v["is_law_surface"], serde_json::json!(true));
    // Seed route selected (no pack at that root).
    assert_eq!(v["envelopes"][0]["route_source"], serde_json::json!("seed"));
}
