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
//! TRIAD-CONTRACT-1 (MCP side) — the MCP tool emits the byte-equivalent canonical
//! `RouteEnvelope`, proving MCP is a transport projection of the one route engine.

use std::path::Path;

use ggen_lsp::mcp::build_repair_routes;

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
