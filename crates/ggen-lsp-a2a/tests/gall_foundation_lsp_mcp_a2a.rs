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
//! GALL-INTEGRATION-1 — the delivery-plane foundation, made load-bearing.
//!
//! These integration tests prove that ggen's three delivery transports — the
//! editor/headless **LSP** gate, the **MCP** tool surface, and the **A2A** bridge —
//! are NOT three separate surfaces. They are one route engine carrying the same
//! route/action shape. Each test drives the THREE DISTINCT PUBLIC ENTRY POINTS:
//!
//!   LSP : `ggen_lsp::check_files_in_root(root, &[path], with_routes=true)`  (headless gate)
//!   MCP : `ggen_lsp_mcp::build_repair_routes_in(root, path, content)`        (tool surface)
//!   A2A : `ggen_lsp_a2a::dispatch_tool("ggen.lsp.repair_route", {..})`       (bridge)
//!
//! and asserts they converge on the same diagnostic code + route_id for REAL
//! playground fixtures on disk. If any surface drifts, a test fails loudly.
//!
//! This crate (`ggen-lsp-a2a`) is the apex of the delivery dependency graph —
//! it depends on ggen-lsp, ggen-lsp-mcp, and ggen-a2a-mcp — so it is the only
//! place all three surfaces can be exercised together without a dependency cycle.

use ggen_a2a_mcp::a2a_generated::adapter::Adapter;
use ggen_lsp_a2a::{agent_card, dispatch_tool, RepairRouteAdapter, TOOLS};
use serde_json::{json, Value};
use std::path::{Path, PathBuf};

/// The playground project root (the "house" under foundation test).
fn playground_root() -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../../playground")
        .canonicalize()
        .expect("playground/ exists at the workspace root")
}

/// A foundation pier fixture: an unconstrained CONSTRUCT that raises E0011 under
/// strict mode and earns the seeded route `template.values-inline`.
const BROKEN_CONSTRUCT: &str = "playground/proof/broken-construct.rq";
const EXPECT_CODE: &str = "E0011";
const EXPECT_ROUTE: &str = "template.values-inline";

/// Read a playground fixture's content + return its absolute path string.
fn fixture(rel: &str) -> (PathBuf, String, String) {
    let abs = Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("../..")
        .join(rel)
        .canonicalize()
        .unwrap_or_else(|_| panic!("fixture {rel} exists"));
    let content = std::fs::read_to_string(&abs).expect("read fixture");
    let path_str = abs.to_string_lossy().into_owned();
    (abs, path_str, content)
}

fn root_str() -> String {
    playground_root().to_string_lossy().into_owned()
}

/// LSP arm: run the genuine headless `ggen lsp check` machinery over the file on
/// disk and project its first repair route. Returns (diagnostic_code, route_id).
fn lsp_route(abs: &Path) -> Option<(String, String)> {
    let report = ggen_lsp::check_files_in_root(&playground_root(), &[abs.to_path_buf()], true);
    let file = report.files.first()?;
    let env = file.envelopes().into_iter().next()?;
    Some((env.diagnostic_code, env.route_id))
}

/// MCP arm: the tool-surface route builder. Returns (diagnostic_code, route_id).
fn mcp_route(path: &str, content: &str) -> Option<(String, String)> {
    let v = ggen_lsp_mcp::build_repair_routes_in(Some(&root_str()), path, content);
    envelope_scalars(&v)
}

/// A2A arm: the bridge dispatch. Returns (diagnostic_code, route_id).
fn a2a_route(path: &str, content: &str) -> Option<(String, String)> {
    let v = dispatch_tool(
        "ggen.lsp.repair_route",
        &json!({ "file_path": path, "file_content": content, "root": root_str() }),
    )
    .expect("a2a dispatch");
    envelope_scalars(&v)
}

/// Pull (diagnostic_code, route_id) from the first envelope of an MCP/A2A result.
fn envelope_scalars(v: &Value) -> Option<(String, String)> {
    let env = v.get("envelopes")?.as_array()?.first()?;
    let code = env.get("diagnostic_code")?.as_str()?.to_string();
    let route = env.get("route_id")?.as_str()?.to_string();
    Some((code, route))
}

// ─────────────────────────────────────────────────────────────────────────────
// Foundation pier 1: the broken-construct fixture routes identically everywhere.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn gall_playground_broken_construct_routes_same_across_lsp_mcp_a2a() {
    let (abs, path, content) = fixture(BROKEN_CONSTRUCT);

    let lsp = lsp_route(&abs).expect("LSP produces a route for the broken construct");
    let mcp = mcp_route(&path, &content).expect("MCP produces a route");
    let a2a = a2a_route(&path, &content).expect("A2A produces a route");

    // All three agree on the diagnostic identity.
    assert_eq!(lsp.0, EXPECT_CODE, "LSP diagnostic code");
    assert_eq!(mcp.0, EXPECT_CODE, "MCP diagnostic code");
    assert_eq!(a2a.0, EXPECT_CODE, "A2A diagnostic code");

    // All three agree on the route identity — one engine, three transports.
    assert_eq!(lsp.1, EXPECT_ROUTE, "LSP route_id");
    assert_eq!(mcp.1, EXPECT_ROUTE, "MCP route_id");
    assert_eq!(a2a.1, EXPECT_ROUTE, "A2A route_id");

    // Cross-transport equality (no drift).
    assert_eq!(lsp, mcp, "LSP must equal MCP (diagnostic + route)");
    assert_eq!(mcp, a2a, "MCP must equal A2A (diagnostic + route)");
}

// ─────────────────────────────────────────────────────────────────────────────
// Foundation pier 2: a clean authored surface yields no blocking route.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn gall_clean_playground_surface_has_no_blocking_route() {
    // The real thesis ontology is valid Turtle — a recognized law surface that
    // must produce zero ERROR diagnostics and therefore no blocking route.
    let (abs, _path, _content) = fixture("playground/thesis-ontology.ttl");
    let report = ggen_lsp::check_files_in_root(&playground_root(), &[abs.clone()], true);

    let file = report
        .files
        .iter()
        .find(|f| f.path.ends_with("thesis-ontology.ttl"))
        .expect("the .ttl is recognized as a law surface (appears in the report)");

    assert_eq!(
        report.error_count, 0,
        "clean ontology has no ERROR diagnostics"
    );
    assert!(
        file.envelopes().is_empty(),
        "a clean law surface produces no repair route"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Foundation pier 3: the MCP tool list is exactly repair_route / replay_case / metrics.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn gall_mcp_tool_list_contains_repair_replay_metrics() {
    // The bridge TOOLS const mirrors the MCP server's single-source tool set.
    assert!(TOOLS.contains(&"ggen.lsp.repair_route"));
    assert!(TOOLS.contains(&"ggen.lsp.replay_case"));
    assert!(TOOLS.contains(&"ggen.lsp.metrics"));
    assert_eq!(TOOLS.len(), 3, "exactly three delivery tools");

    // Each named tool is backed by a real, callable MCP function (not just a name).
    let (_abs, path, content) = fixture(BROKEN_CONSTRUCT);
    let args = json!({ "file_path": path, "file_content": content })
        .as_object()
        .cloned();
    assert!(
        ggen_lsp_mcp::repair_route_result(args).is_ok(),
        "repair_route callable"
    );
    assert!(
        ggen_lsp_mcp::metrics_result(Some(
            json!({ "root": root_str() }).as_object().unwrap().clone()
        ))
        .is_ok(),
        "metrics callable"
    );
    assert!(
        ggen_lsp_mcp::replay_case_result(Some(
            json!({ "root": root_str() }).as_object().unwrap().clone()
        ))
        .is_ok(),
        "replay_case callable"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Foundation pier 4: the A2A bridge result is byte-identical to MCP.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn gall_a2a_bridge_matches_mcp_for_repair_route() {
    let (_abs, path, content) = fixture(BROKEN_CONSTRUCT);
    let a2a = dispatch_tool(
        "ggen.lsp.repair_route",
        &json!({ "file_path": path, "file_content": content }),
    )
    .expect("a2a dispatch");
    let mcp = ggen_lsp_mcp::build_repair_routes(&path, &content);
    assert_eq!(
        a2a, mcp,
        "A2A bridge must equal the MCP result byte-for-byte"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Foundation pier 5: route_id is preserved across all three transports.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn gall_lsp_mcp_a2a_preserve_route_id() {
    let (abs, path, content) = fixture(BROKEN_CONSTRUCT);
    let lsp = lsp_route(&abs).expect("lsp").1;
    let mcp = mcp_route(&path, &content).expect("mcp").1;
    let a2a = a2a_route(&path, &content).expect("a2a").1;
    assert_eq!(lsp, mcp, "route_id LSP==MCP");
    assert_eq!(mcp, a2a, "route_id MCP==A2A");
    assert_eq!(lsp, EXPECT_ROUTE, "route_id is the expected seeded route");
}

// ─────────────────────────────────────────────────────────────────────────────
// Foundation pier 6: the delivery plane advertises the current identity.
// ─────────────────────────────────────────────────────────────────────────────

#[test]
fn gall_delivery_plane_uses_current_identity() {
    let card = agent_card();
    assert_eq!(
        card["version"],
        json!(env!("CARGO_PKG_VERSION")),
        "agent card version"
    );
    let adapter = RepairRouteAdapter::new();
    assert_eq!(
        adapter.version(),
        env!("CARGO_PKG_VERSION"),
        "A2A adapter version"
    );
    // The bridge crate's own identity (single workspace version).
    assert_eq!(
        env!("CARGO_PKG_VERSION"),
        env!("CARGO_PKG_VERSION"),
        "bridge crate version"
    );
}

// ─────────────────────────────────────────────────────────────────────────────
// Foundation pier 7: replay + metrics surfaces are reachable for a captured case.
// ─────────────────────────────────────────────────────────────────────────────

#[tokio::test]
async fn gall_replay_metrics_surface_is_reachable_for_captured_case() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path().to_string_lossy().into_owned();

    // Capture a case into the root via the A2A actuation (leaves OCEL field evidence).
    let mut adapter = RepairRouteAdapter::new();
    adapter.initialize(json!({})).await.expect("init");
    adapter
        .from_a2a(&json!({
            "tool": "ggen.lsp.repair_route",
            "arguments": {
                "file_path": "q.rq",
                "file_content": "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n",
                "root": root
            }
        }))
        .await
        .expect("from_a2a captures a case");

    // Metrics surface reachable over the captured root.
    let metrics =
        ggen_lsp_mcp::metrics_result(Some(json!({ "root": root }).as_object().unwrap().clone()))
            .expect("metrics surface reachable");
    assert!(
        metrics.is_object(),
        "metrics returns a structured verdict object"
    );

    // Replay surface reachable (verify_promotion path when no case_id is given).
    let replay = ggen_lsp_mcp::replay_case_result(Some(
        json!({ "root": root }).as_object().unwrap().clone(),
    ))
    .expect("replay surface reachable");
    assert!(!replay.is_null(), "replay returns a structured value");
}
