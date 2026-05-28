//! TRIAD-CONTRACT-1 (MCP side) — the MCP tool emits the byte-equivalent canonical
//! `RouteEnvelope`, proving MCP is a transport projection of the one route engine.

use std::path::Path;

use ggen_lsp_mcp::build_repair_routes;

const E0011_SRC: &str = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n";

#[test]
fn mcp_envelope_equals_canonical_ggen_lsp_envelope() {
    // MCP channel output.
    let v = build_repair_routes("q.rq", E0011_SRC);
    assert_eq!(v["is_law_surface"], serde_json::json!(true));
    let mcp_env = &v["envelopes"][0];
    assert!(mcp_env.is_object(), "MCP emits an envelope");

    // Canonical projection via ggen-lsp directly, with the SAME registry the MCP
    // tool builds (seeded + cwd pack).
    let registry = ggen_lsp::RouteRegistry::seeded()
        .with_pack_routes(&ggen_lsp::route::default_pack_routes_path(Path::new(".")));
    let diagnostics = ggen_lsp::check_content("q.rq", E0011_SRC)
        .expect("rq law surface")
        .diagnostics;
    let canonical =
        ggen_lsp::envelope_for_diagnostic(&registry, &diagnostics[0], E0011_SRC, "q.rq")
            .expect("canonical envelope");

    assert_eq!(
        *mcp_env,
        serde_json::to_value(&canonical).expect("ser canonical"),
        "MCP tool output must carry the byte-equivalent canonical RouteEnvelope"
    );
}
