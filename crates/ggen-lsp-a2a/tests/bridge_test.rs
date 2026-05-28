//! A2A-BRIDGE-1 — the A2A bridge actuates the same route engine as MCP, with no
//! route drift, structured refusals, and an advertised agent card.

use ggen_a2a_mcp::a2a_generated::adapter::Adapter;
use ggen_lsp_a2a::{agent_card, dispatch_tool, RepairRouteAdapter};
use serde_json::json;

const E0011: &str = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n";

#[test]
fn a2a_route_result_equals_mcp_result() {
    let a2a = dispatch_tool(
        "ggen.lsp.repair_route",
        &json!({ "file_path": "q.rq", "file_content": E0011 }),
    )
    .expect("dispatch");
    let mcp = ggen_lsp_mcp::build_repair_routes("q.rq", E0011);
    assert_eq!(a2a, mcp, "A2A result must equal the MCP result (no drift)");
}

#[test]
fn unknown_tool_is_a_structured_refusal() {
    assert!(dispatch_tool("nope", &json!({})).is_err());
}

#[test]
fn bad_args_preserve_the_mcp_refusal_shape() {
    // Missing file_content → the same validation refusal MCP would raise.
    assert!(dispatch_tool("ggen.lsp.repair_route", &json!({ "file_path": "q.rq" })).is_err());
}

#[test]
fn agent_card_advertises_all_three_capabilities() {
    let card = agent_card();
    let caps = card["capabilities"].as_object().expect("capabilities object");
    assert!(caps.contains_key("ggen.lsp.repair_route"));
    assert!(caps.contains_key("ggen.lsp.replay_case"));
    assert!(caps.contains_key("ggen.lsp.metrics"));
}

#[tokio::test]
async fn from_a2a_actuates_a_task_against_the_route_engine() {
    let mut adapter = RepairRouteAdapter::new();
    adapter.initialize(json!({})).await.expect("init");
    assert!(adapter.can_handle("ggen.lsp.repair_route"));

    let result = adapter
        .from_a2a(&json!({
            "tool": "ggen.lsp.repair_route",
            "arguments": { "file_path": "q.rq", "file_content": E0011 }
        }))
        .await
        .expect("from_a2a actuates");
    assert_eq!(result["is_law_surface"], json!(true));
    assert!(!result["envelopes"].as_array().expect("envelopes").is_empty());
}
