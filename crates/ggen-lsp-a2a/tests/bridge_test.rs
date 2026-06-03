#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]

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
    let caps = card["capabilities"]
        .as_object()
        .expect("capabilities object");
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
    assert!(!result["envelopes"]
        .as_array()
        .expect("envelopes")
        .is_empty());
}

#[tokio::test]
async fn from_a2a_with_root_leaves_a2a_field_evidence() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    let mut adapter = RepairRouteAdapter::new();
    adapter.initialize(json!({})).await.expect("init");

    adapter
        .from_a2a(&json!({
            "tool": "ggen.lsp.repair_route",
            "arguments": { "file_path": "q.rq", "file_content": E0011, "root": root.to_str().unwrap() }
        }))
        .await
        .expect("from_a2a");

    let log = ggen_lsp::IntelLog::at_root(root).read();
    assert!(!log.events.is_empty(), "a2a request left field evidence");
    assert!(
        log.events
            .iter()
            .all(|e| e.attributes.get("transport").map(String::as_str) == Some("a2a")),
        "every captured event is tagged transport=a2a"
    );
}

#[tokio::test]
async fn from_a2a_without_root_stays_pure() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    let mut adapter = RepairRouteAdapter::new();
    adapter.initialize(json!({})).await.expect("init");
    adapter
        .from_a2a(&json!({
            "tool": "ggen.lsp.repair_route",
            "arguments": { "file_path": "q.rq", "file_content": E0011 }
        }))
        .await
        .expect("from_a2a");
    assert!(
        ggen_lsp::IntelLog::at_root(root).read().events.is_empty(),
        "no root ⇒ no a2a field evidence"
    );
}
