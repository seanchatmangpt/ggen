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
//! Manifest-contract test — the Claude Code plugin manifest
//! (`.claude-plugin/marketplace.json`) must stay in lockstep with the server's REAL
//! recognized law-surface file types. Drift either way is a silent integration bug:
//! a recognized surface left unmapped means Claude Code never launches the server for
//! it; a phantom extension mapped means the manifest advertises a surface the server
//! ignores. This test anchors the manifest to `FileType::from_path` (the single source
//! of truth), not to a hand-copied list.

use ggen_lsp::state::FileType;
use serde_json::Value;
use std::path::Path;

/// Load + parse the plugin marketplace manifest (resolved from the crate dir, so the
/// test passes regardless of the cwd `cargo test` is invoked from).
fn manifest() -> Value {
    let path = Path::new(env!("CARGO_MANIFEST_DIR")).join("../../.claude-plugin/marketplace.json");
    let text =
        std::fs::read_to_string(&path).unwrap_or_else(|e| panic!("read {}: {e}", path.display()));
    serde_json::from_str(&text).expect("marketplace.json must be valid JSON")
}

#[test]
fn manifest_declares_the_ggen_lsp_server() {
    let m = manifest();
    let plugin = &m["plugins"][0];
    assert_eq!(plugin["name"], "ggen-lsp", "first plugin is ggen-lsp");
    let server = &plugin["lspServers"]["ggen"];
    assert_eq!(
        server["command"], "ggen-lsp",
        "LSP server command must be the dedicated ggen-lsp binary (rust-analyzer convention)"
    );
    // Version drift guard: the manifest's plugin version tracks the crate version.
    assert_eq!(
        plugin["version"],
        env!("CARGO_PKG_VERSION"),
        "manifest plugin version must match the ggen-lsp crate version"
    );
}

#[test]
fn manifest_extensions_cover_exactly_the_recognized_law_surfaces() {
    let m = manifest();
    let ext_map = m["plugins"][0]["lspServers"]["ggen"]["extensionToLanguage"]
        .as_object()
        .expect("extensionToLanguage must be a JSON object");

    // (1) No phantom mappings: every manifest extension classifies to a real
    //     (non-Unknown) FileType, so the server actually serves what's advertised.
    for ext in ext_map.keys() {
        let probe = format!("x{ext}"); // e.g. ".ttl" -> "x.ttl"
        assert_ne!(
            FileType::from_path(&probe),
            FileType::Unknown,
            "manifest maps extension {ext:?} but FileType::from_path does not recognize it"
        );
    }

    // (2) No unmapped surfaces: every law surface the server recognizes is mapped.
    //     Anchored to the classifier itself, not just this literal list.
    let recognized = [".ttl", ".nt", ".nq", ".rq", ".sparql", ".tera", ".toml"];
    for ext in recognized {
        assert_ne!(
            FileType::from_path(&format!("x{ext}")),
            FileType::Unknown,
            "the recognized-set in this test drifted from FileType::from_path for {ext:?}"
        );
        assert!(
            ext_map.contains_key(ext),
            "law surface {ext:?} is recognized by the server but NOT mapped in the manifest"
        );
    }
}
