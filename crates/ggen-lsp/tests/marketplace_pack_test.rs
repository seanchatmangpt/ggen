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
//! MARKETPLACE-PACK-1 — one command installs the LSP/MCP/A2A route surface, and
//! the full loop (install → route → apply → receipt) works against it.

use ggen_lsp::intel::events::activity;
use ggen_lsp::route::default_pack_routes_path;
use ggen_lsp::state::ServerState;
use ggen_lsp::{check_content, envelope_for_diagnostic, init_project, IntelLog, RouteRegistry};
use lsp_max::lsp_types::Url;
use tempfile::TempDir;

fn url_from_path(path: impl AsRef<std::path::Path>) -> Url {
    url::Url::from_file_path(path.as_ref())
        .expect("absolute path")
        .to_string()
        .parse::<Url>()
        .expect("valid uri")
}

#[tokio::test]
async fn install_then_route_apply_receipt_end_to_end() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();

    // --- Install: one command writes MCP + editor LSP + hooks + advertised pack. ---
    let report = init_project(root, &[], &["generic".to_string()]).expect("init");
    assert!(
        root.join(".mcp.json").is_file(),
        "MCP server registration emitted"
    );
    assert!(
        root.join(".helix/languages.toml").is_file(),
        "editor LSP config emitted"
    );
    assert!(
        root.join(".agent-admissibility/hooks/generic/pre-edit.sh")
            .is_file(),
        "agent hooks emitted"
    );
    assert!(
        root.join(".agent-admissibility/pack-manifest.json")
            .is_file(),
        "advertised pack manifest emitted"
    );
    assert!(!report.files_written.is_empty());

    // --- Route: a real diagnostic gets the canonical envelope (same as MCP/A2A). ---
    let src = "[logging]\nlevel = \"verbose\"\n";
    let broken = check_content("ggen.toml", src).expect("toml law surface");
    let registry = RouteRegistry::seeded().with_pack_routes(&default_pack_routes_path(root));
    let env = envelope_for_diagnostic(&registry, &broken.diagnostics[0], src, "ggen.toml")
        .expect("a route envelope");
    assert_eq!(env.diagnostic_code, "E0023");

    // --- Apply: the editor applies the fix → rework closure → receipt. ---
    let state = ServerState::with_root(root);
    let uri = url_from_path(root.join("ggen.toml"));
    state.observe_diagnostics(&uri, &broken.diagnostics).await;
    let fixed = check_content("ggen.toml", "[logging]\nlevel = \"info\"\n").expect("toml");
    state.observe_diagnostics(&uri, &fixed.diagnostics).await;

    // --- Receipt: the OCEL log under the installed root records the closed episode. ---
    let log = IntelLog::at_root(root).read();
    assert!(
        log.events
            .iter()
            .any(|e| e.activity == activity::REPAIR_APPLIED),
        "applied repair observed end-to-end"
    );
    assert!(
        log.events
            .iter()
            .any(|e| e.activity == activity::RECEIPT_EMITTED),
        "receipt emitted end-to-end"
    );
}
