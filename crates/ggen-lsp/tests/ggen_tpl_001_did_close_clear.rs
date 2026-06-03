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
//! GGEN-TPL-001 — proactive living-clear on `did_close` (the alive-vs-fake-live
//! proof for the close path).
//!
//! The cross-surface detector ([`ProjectIndex::from_root`] → `detect_tpl_001`) is
//! disk-based and buffer-agnostic: closing a buffer alone never changes detection
//! (the file persists on disk, so its flags persist — see the regression guard
//! below, which proves a still-on-disk surface keeps its peer flagged). The lawful
//! close-clear therefore fires when the close coincides with the closed surface
//! disappearing from disk — the realistic editor event of DELETING a project
//! manifest, which both removes `ggen.toml` and closes its buffer.
//!
//! When `ggen.toml` is gone, `ProjectIndex::from_root` errors, `detect_tpl_001_for`
//! returns no groups, and every template that manifest's rules sustained falls out
//! of the freshly re-detected `current` set — clearing through the SAME keyed
//! subtraction + residual-preservation path the edit flow uses:
//!
//! ```text
//! analyze rule surface (.rq) → peer template flagged (GGEN-TPL-001) → pending repair
//! → DELETE + CLOSE ggen.toml (the project manifest disappears from disk)
//! → re-detect finds no project → the template falls out of `current`
//! → the TEMPLATE URI is cleared THROUGH observe_diagnostics
//! → disappearance observed → RepairApplied → GatePassed → ReceiptEmitted
//! ```
//!
//! Before this fix, `did_close` only dropped the closed URI's own state and did a
//! blunt empty publish for it — the stale squiggle on the *peer* template (and the
//! repair lifecycle event) never happened ("fake-live"). The gate is read from the
//! EXTERNAL OCEL intel log on disk, not a return value — unforgeable evidence.
//!
//! This drives the Client-free core [`ServerState::close_document`] (the exact code
//! `server.rs::did_close` calls, minus the tower-lsp `Client` publish, which is
//! irrelevant to the receipt chain). Chicago TDD: real files, real `ProjectIndex`,
//! real on-disk intel log. No mocks.

use std::fs;
use std::path::Path;

use ggen_lsp::ServerState;
use tempfile::TempDir;
use tower_lsp::lsp_types::Url;

/// Write a minimal but valid ggen project: one rule binding `queries/items.rq`
/// to `templates/item.tera` with output `out.txt`. Identical shape to the
/// stale-clear fixture.
fn write_project(dir: &Path, query: &str, template: &str) {
    fs::create_dir_all(dir.join("schema")).expect("schema dir");
    fs::create_dir_all(dir.join("queries")).expect("queries dir");
    fs::create_dir_all(dir.join("templates")).expect("templates dir");
    fs::write(
        dir.join("schema/domain.ttl"),
        "@prefix schema: <https://schema.org/> .\n",
    )
    .expect("ttl");
    fs::write(dir.join("queries/items.rq"), query).expect("rq");
    fs::write(dir.join("templates/item.tera"), template).expect("tera");
    fs::write(
        dir.join("ggen.toml"),
        r#"[project]
name = "did-close-fixture"
version = "0.1.0"

[ontology]
source = "schema/domain.ttl"

[generation]
output_dir = "."

[[generation.rules]]
name = "items"
query = { file = "queries/items.rq" }
template = { file = "templates/item.tera" }
output_file = "out.txt"
"#,
    )
    .expect("ggen.toml");
}

/// Append a SECOND rule + query that ALSO binds the same template, used by the
/// regression guard: a surviving query keeps the template flagged after one
/// query closes. Both queries lack `?title`, so the template stays unlawful.
fn add_second_query_rule(dir: &Path) {
    fs::write(
        dir.join("queries/items2.rq"),
        "SELECT ?name WHERE { ?s <https://schema.org/name> ?name }",
    )
    .expect("rq2");
    fs::write(
        dir.join("ggen.toml"),
        r#"[project]
name = "did-close-fixture"
version = "0.1.0"

[ontology]
source = "schema/domain.ttl"

[generation]
output_dir = "."

[[generation.rules]]
name = "items"
query = { file = "queries/items.rq" }
template = { file = "templates/item.tera" }
output_file = "out.txt"

[[generation.rules]]
name = "items2"
query = { file = "queries/items2.rq" }
template = { file = "templates/item.tera" }
output_file = "out2.txt"
"#,
    )
    .expect("ggen.toml (two rules)");
}

/// Read the EXTERNAL OCEL intel log written under
/// `<root>/.ggen/ocel/agent-edit-events.ocel.jsonl` — the unforgeable surface.
fn read_log_lines(root: &Path) -> Vec<String> {
    let path = root
        .join(".ggen")
        .join("ocel")
        .join("agent-edit-events.ocel.jsonl");
    fs::read_to_string(path)
        .unwrap_or_default()
        .lines()
        .map(str::to_string)
        .collect()
}

/// True if some JSONL event line names this `activity`, the GGEN-TPL-001 `code`,
/// and the template file — one event per line.
fn has_template_event(lines: &[String], activity: &str) -> bool {
    lines.iter().any(|l| {
        l.contains(&format!("\"activity\":\"{activity}\""))
            && l.contains("item.tera")
            && l.contains("GGEN-TPL-001")
    })
}

#[tokio::test]
async fn closing_query_surface_clears_template_through_living_loop() {
    // ── Arrange: invalid project (query SELECTs ?name; template wants title).
    let tmp = TempDir::new().expect("tempdir");
    let root = tmp.path();
    write_project(
        root,
        "SELECT ?name WHERE { ?s <https://schema.org/name> ?name }",
        r#"{{ row["title"] }}"#,
    );
    let state = ServerState::with_root(root);
    let rq_uri = Url::from_file_path(root.join("queries/items.rq")).expect("rq url");
    let manifest_uri = Url::from_file_path(root.join("ggen.toml")).expect("manifest url");
    let template_uri = Url::from_file_path(root.join("templates/item.tera")).expect("template url");

    // ── Act 1 — RAISE: open/analyze the QUERY surface through the real edit path.
    // The cross-surface detector flags the PEER template (GGEN-TPL-001), and
    // `tpl_flagged` records it exactly as runtime would.
    let rq_content = fs::read_to_string(root.join("queries/items.rq")).expect("read rq");
    state.set_document(rq_uri.clone(), rq_content.clone()).await;
    let raised = state.analyze_and_observe(&rq_uri, &rq_content).await;
    assert!(
        raised
            .iter()
            .any(|(u, ds)| *u == template_uri && !ds.is_empty()),
        "precondition: analyzing the query must flag the peer template"
    );

    // ── Act 2 — DELETE + CLOSE the project manifest. Removing `ggen.toml` from disk
    // means re-detection finds no project, so the template that manifest's rule
    // sustained falls out and must clear THROUGH observe_diagnostics.
    fs::remove_file(root.join("ggen.toml")).expect("delete manifest");
    let published = state.close_document(&manifest_uri).await;

    // The reconciliation republished the now-lawful template (residual = empty,
    // single-file Tera analyzer yields no GGEN-TPL-001).
    assert!(
        published.iter().any(|(u, _)| *u == template_uri),
        "close must republish the cleared peer template, got: {:?}",
        published.iter().map(|(u, _)| u.path()).collect::<Vec<_>>()
    );
    // The closed URI's own empty clear is present (LSP conformance).
    assert!(
        published
            .iter()
            .any(|(u, ds)| *u == manifest_uri && ds.is_empty()),
        "close must publish an empty clear for the closed URI itself"
    );

    // ── Assert: the EXTERNAL OCEL log proves the full clear chain on the template.
    let lines = read_log_lines(root);
    assert!(
        has_template_event(&lines, "DiagnosticRaised"),
        "link 1 (raise): DiagnosticRaised for GGEN-TPL-001 on the template missing.\nlog:\n{}",
        lines.join("\n")
    );
    assert!(
        has_template_event(&lines, "RepairApplied"),
        "link 2 (close-clear-through-lifecycle): closing the query did NOT clear the \
         peer through observe_diagnostics (fake-live: a blunt empty publish for only \
         the closed URI would stop here).\nlog:\n{}",
        lines.join("\n")
    );
    assert!(
        has_template_event(&lines, "ReceiptEmitted"),
        "link 3 (receipt): ReceiptEmitted for the cross-surface close-clear missing.\nlog:\n{}",
        lines.join("\n")
    );

    // ── And: the close path materialized no emitted artifact.
    assert!(
        !root.join("out.txt").exists(),
        "close must never write the rule's output_file"
    );
}

/// Regression guard: closing ONE of two queries that both bind the same template
/// must NOT clear the template — the surviving query keeps it flagged. Re-detection
/// (not a blunt empty publish) is load-bearing: `current` stays non-empty, so
/// `tpl_clears_for` returns nothing and no spurious `RepairApplied` is observed.
#[tokio::test]
async fn closing_query_with_surviving_peer_keeps_flag() {
    let tmp = TempDir::new().expect("tempdir");
    let root = tmp.path();
    // Both rules bind the template; neither query SELECTs ?title → template unlawful.
    write_project(
        root,
        "SELECT ?name WHERE { ?s <https://schema.org/name> ?name }",
        r#"{{ row["title"] }}"#,
    );
    add_second_query_rule(root);
    let state = ServerState::with_root(root);
    let rq1_uri = Url::from_file_path(root.join("queries/items.rq")).expect("rq1 url");
    let template_uri = Url::from_file_path(root.join("templates/item.tera")).expect("template url");

    // RAISE through the first query: the peer template is flagged, `tpl_flagged`
    // records it.
    let rq1_content = fs::read_to_string(root.join("queries/items.rq")).expect("read rq1");
    state
        .set_document(rq1_uri.clone(), rq1_content.clone())
        .await;
    let raised = state.analyze_and_observe(&rq1_uri, &rq1_content).await;
    assert!(
        raised
            .iter()
            .any(|(u, ds)| *u == template_uri && !ds.is_empty()),
        "precondition: template flagged by the first query"
    );

    // CLOSE the first query. The SECOND query still binds the template (also lacking
    // ?title), so re-detection keeps it in `current` → no clear.
    let published = state.close_document(&rq1_uri).await;
    assert!(
        !published.iter().any(|(u, _)| *u == template_uri),
        "closing one query must NOT clear a template still flagged by a surviving query"
    );

    // No RepairApplied for the template (it was never repaired — still unlawful).
    let lines = read_log_lines(root);
    assert!(
        has_template_event(&lines, "DiagnosticRaised"),
        "raise must be recorded"
    );
    assert!(
        !has_template_event(&lines, "RepairApplied"),
        "no repair: the template is still lawfully flagged by the surviving query.\nlog:\n{}",
        lines.join("\n")
    );
}
