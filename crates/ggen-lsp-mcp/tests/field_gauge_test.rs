//! FIELD-GAUGE-1 (MCP side) — a route request with a real root leaves attributed
//! field evidence (transport=mcp); pure projection (no root) stays side-effect-free.

use ggen_lsp::IntelLog;
use ggen_lsp_mcp::{repair_route_captured, repair_route_result};
use tempfile::TempDir;

const E0011: &str = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n";

fn args(json: serde_json::Value) -> Option<serde_json::Map<String, serde_json::Value>> {
    json.as_object().cloned()
}

#[test]
fn request_with_root_leaves_mcp_evidence() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    repair_route_captured(args(serde_json::json!({
        "file_path": "q.rq",
        "file_content": E0011,
        "root": root.to_str().unwrap()
    })))
    .expect("captured route");

    let log = IntelLog::at_root(root).read();
    assert!(
        !log.events.is_empty(),
        "field evidence captured under the given root"
    );
    assert!(
        log.events
            .iter()
            .all(|e| e.attributes.get("transport").map(String::as_str) == Some("mcp")),
        "every captured event is tagged transport=mcp"
    );
}

#[test]
fn request_without_root_is_pure_projection() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    // No root → the capturing variant leaves no trace under this root.
    repair_route_captured(args(serde_json::json!({
        "file_path": "q.rq",
        "file_content": E0011
    })))
    .expect("ok");
    assert!(
        IntelLog::at_root(root).read().events.is_empty(),
        "no root ⇒ pure projection, no field evidence"
    );

    // The pure result fn never captures, even with a root.
    repair_route_result(args(serde_json::json!({
        "file_path": "q.rq",
        "file_content": E0011,
        "root": root.to_str().unwrap()
    })))
    .expect("ok");
    assert!(
        IntelLog::at_root(root).read().events.is_empty(),
        "repair_route_result stays pure (parity-safe) even with a root"
    );
}
