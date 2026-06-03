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
//! TRIAD-CONTRACT-1 — one route envelope across all channels.
//!
//! The headless gate, the LSP CodeAction `data`, and the MCP tool all project a
//! route through ONE function (`envelope_for_diagnostic`). This proves the
//! headless projection is byte-equivalent to that canonical function (which the
//! editor's `code_action` and the MCP `build_repair_routes` both call), so every
//! channel emits the identical `RouteEnvelope` for the same diagnostic.

use std::fs;

use ggen_lsp::route::{default_pack_routes_path, DiagnosticRef, RouteRefusal};
use ggen_lsp::{check_files_in_root, envelope_for_diagnostic, RouteRegistry};
use tempfile::TempDir;

const E0011_SRC: &str = "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }\n";

#[test]
fn headless_and_canonical_projection_are_byte_equivalent() {
    let dir = TempDir::new().expect("tempdir");
    let root = dir.path();
    let rq = root.join("q.rq");
    fs::write(&rq, E0011_SRC).expect("write");

    // Headless channel: FileReport::envelopes().
    let report = check_files_in_root(root, &[rq], true);
    let file = &report.files[0];
    let headless = file.envelopes();
    assert_eq!(headless.len(), 1, "one routed diagnostic (E0011)");

    // The canonical projection that code_action's `data` and the MCP tool both
    // call (same registry the headless gate built: seeded + root pack).
    let registry = RouteRegistry::seeded().with_pack_routes(&default_pack_routes_path(root));
    let canonical = envelope_for_diagnostic(&registry, &file.diagnostics[0], E0011_SRC, &file.path)
        .expect("canonical envelope");

    // Byte-equivalent across channels — transport differs, the route object does not.
    assert_eq!(
        serde_json::to_value(&headless[0]).expect("ser headless"),
        serde_json::to_value(&canonical).expect("ser canonical"),
        "headless and canonical envelope must be byte-equivalent"
    );

    // The envelope carries the full canonical contract.
    let env = &canonical;
    assert!(env.case_id.starts_with("c:"), "stable site case_id");
    assert_eq!(env.diagnostic_code, "E0011");
    assert_eq!(env.route_source, "seed"); // no pack promoted in this dir
    assert!(!env.span.is_empty());
    assert_eq!(
        env.case_id, env.compact_trace.case_id,
        "envelope and compact agree on case_id"
    );
}

#[test]
fn refusal_is_one_shared_shape() {
    // The refusal shape is channel-agnostic: a diagnostic with no admissible route
    // collapses to the same RouteRefusal regardless of transport. (Real analyzer
    // diagnostics all map to seeded families, so this is the defensive anti-fail-open
    // path; we assert its shape directly.)
    let target = DiagnosticRef {
        code: "E9999".to_string(),
        message: "hypothetical unrouted diagnostic".to_string(),
        range: tower_lsp::lsp_types::Range::default(),
        severity: "error".to_string(),
    };
    let refusal = RouteRefusal::from_target(&target);
    assert_eq!(refusal.diagnostic_code, "E9999");
    assert!(!refusal.reason.is_empty());
    assert!(serde_json::to_value(&refusal).is_ok(), "refusal serializes");
}
