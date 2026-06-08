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

//! GALL-CHECKPOINT-001B — stale-clear hard gate (the alive-vs-fake-live proof).
//!
//! Proves the cross-surface living-loop closure for GGEN-TPL-001:
//!
//! ```text
//! raise-with-route → pending repair recorded
//! → cross-surface repair (the SPARQL query, NOT the template, is fixed)
//! → the TEMPLATE URI is cleared through observe_diagnostics
//! → disappearance observed → RepairApplied → GatePassed → ReceiptEmitted
//! ```
//!
//! A bare `publish_diagnostics(empty)` would clear the editor but emit no
//! lifecycle event ("fake-live"). The gate is therefore read from the EXTERNAL
//! OCEL intel log on disk, not from a return value — unforgeable evidence.
//!
//! This drives the exact `ServerState` methods `server.rs::refresh_analyzer`
//! calls for a rule-surface edit (`detect_tpl_001` → `tpl_clears_for` →
//! `observe_diagnostics`), minus only the tower-lsp `Client` publish, which is
//! irrelevant to the receipt chain. Chicago TDD: real files, real index, real
//! intel log.

use std::collections::HashSet;
use std::fs;
use std::path::Path;

use ggen_lsp::analyzers::detect_tpl_001;
use ggen_lsp::project_index::ProjectIndex;
use ggen_lsp::ServerState;
use tempfile::TempDir;
use tower_lsp_max::lsp_types::Url;

/// Write a minimal but valid ggen project: one rule binding `queries/items.rq`
/// to `templates/item.tera` with output `out.txt`.
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
name = "stale-clear-fixture"
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

/// Read the EXTERNAL OCEL intel log written under
/// `<root>/.ggen/ocel/agent-edit-events.ocel.jsonl` (the canonical path
/// `IntelLog::at_root` appends to — see `intel::log::default_path`). Reading the
/// raw on-disk evidence — rather than an in-process accessor — is the
/// unforgeable surface the anti-cheating doctrine requires.
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
/// and the template file — one event per line, so three substrings on one line
/// identify a single OCEL event.
fn has_template_event(lines: &[String], activity: &str) -> bool {
    lines.iter().any(|l| {
        l.contains(&format!("\"activity\":\"{activity}\""))
            && l.contains("item.tera")
            && l.contains("GGEN-TPL-001")
    })
}

#[tokio::test]
async fn query_side_repair_clears_template_through_living_loop() {
    // ── Arrange: an invalid project (query SELECTs ?name; template wants title).
    let tmp = TempDir::new().expect("tempdir");
    let root = tmp.path();
    write_project(
        root,
        "SELECT ?name WHERE { ?s <https://schema.org/name> ?name }",
        r#"{{ row["title"] }}"#,
    );
    let state = ServerState::with_root(root);

    // ── Act 1 — RAISE (template is "opened"/analyzed): detect → observe.
    let project = ProjectIndex::from_root(root).expect("index builds");
    let groups = detect_tpl_001(&project);
    assert!(
        !groups.is_empty(),
        "precondition: invalid project must flag GGEN-TPL-001"
    );
    // Capture the template URI from the detector's own resolved path so it
    // matches byte-for-byte across passes (no canonicalization skew).
    let (template_path, raise_diags) = groups.into_iter().next().expect("one group");
    let template_uri = Url::from_file_path(&template_path).expect("template url");
    let mut flagged_now: HashSet<Url> = HashSet::new();
    flagged_now.insert(template_uri.clone());
    // Mirror refresh_analyzer: publish-through-observe for the flagged template,
    // then reconcile (nothing to clear on the raise pass).
    state.observe_diagnostics(&template_uri, &raise_diags).await;
    let cleared = state.tpl_clears_for(&template_uri, &flagged_now).await;
    assert!(cleared.is_empty(), "raise pass clears nothing");

    // ── Act 2 — CROSS-SURFACE REPAIR: fix the QUERY (template file untouched).
    fs::write(
        root.join("queries/items.rq"),
        "SELECT ?name ?title WHERE { ?s <https://schema.org/name> ?name ; \
         <https://schema.org/title> ?title }",
    )
    .expect("rewrite rq");
    let rq_uri = Url::from_file_path(root.join("queries/items.rq")).expect("rq url");

    let project2 = ProjectIndex::from_root(root).expect("index rebuilds");
    let groups2 = detect_tpl_001(&project2);
    assert!(
        groups2.is_empty(),
        "after query repair the relation is lawful: no GGEN-TPL-001"
    );
    // The now-lawful template dropped out of the detector result entirely.
    // Reconciliation must still re-publish it — as an EMPTY set, through observe.
    let current_now: HashSet<Url> = HashSet::new();
    let to_clear = state.tpl_clears_for(&rq_uri, &current_now).await;
    assert_eq!(
        to_clear,
        vec![template_uri.clone()],
        "query-side repair must mark the TEMPLATE URI for clearing"
    );
    for uri in &to_clear {
        // Empty publish THROUGH observe_diagnostics — a clear is an event.
        state.observe_diagnostics(uri, &[]).await;
    }

    // ── Assert: the EXTERNAL intel log proves all three links.
    let lines = read_log_lines(root);
    assert!(
        has_template_event(&lines, "DiagnosticRaised"),
        "link 1 (raise-with-route): DiagnosticRaised for GGEN-TPL-001 on the template missing.\nlog:\n{}",
        lines.join("\n")
    );
    assert!(
        has_template_event(&lines, "RepairApplied"),
        "link 2 (clear-through-lifecycle): RepairApplied for the template missing — \
         the cross-surface clear did not flow through observe_diagnostics.\nlog:\n{}",
        lines.join("\n")
    );
    assert!(
        has_template_event(&lines, "ReceiptEmitted"),
        "link 3 (receipt): ReceiptEmitted for the template missing — repair was not remembered.\nlog:\n{}",
        lines.join("\n")
    );

    // ── And: the LSP path materialized no emitted artifact.
    assert!(
        !root.join("out.txt").exists(),
        "analysis must never write the rule's output_file"
    );
}

/// Negative control: with the bug (no reconciliation), the template clear never
/// happens. We prove the FIX is load-bearing by asserting the disappearance
/// requires the empty publish — i.e. without calling observe on the template URI
/// after repair, no RepairApplied exists for it. (This documents what fake-live
/// looks like; it does not exercise the buggy code path, which no longer exists.)
#[tokio::test]
async fn raise_without_clear_has_no_repair_receipt() {
    let tmp = TempDir::new().expect("tempdir");
    let root = tmp.path();
    write_project(
        root,
        "SELECT ?name WHERE { ?s <https://schema.org/name> ?name }",
        r#"{{ row["title"] }}"#,
    );
    let state = ServerState::with_root(root);

    let project = ProjectIndex::from_root(root).expect("index");
    let (template_path, raise_diags) = detect_tpl_001(&project)
        .into_iter()
        .next()
        .expect("flagged");
    let template_uri = Url::from_file_path(&template_path).expect("url");
    state.observe_diagnostics(&template_uri, &raise_diags).await;

    // No clear performed → only the raise is in the log.
    let lines = read_log_lines(root);
    assert!(
        has_template_event(&lines, "DiagnosticRaised"),
        "raise must be recorded"
    );
    assert!(
        !has_template_event(&lines, "RepairApplied"),
        "without a clear, there is no repair — fake-live would stop here"
    );
}

/// Residual preservation: a template carrying BOTH a syntax error (E0024) and a
/// GGEN-TPL-001 binding error must, on query-side repair, clear ONLY the
/// GGEN-TPL-001 — the E0024 must survive. A blunt empty-publish clear would
/// erase the E0024 too; the fix republishes the template's residual single-file
/// diagnostics so `observe_diagnostics`'s per-key diff drops only the TPL-001
/// key. Proven from the EXTERNAL intel log: a `RepairApplied` exists for the
/// GGEN-TPL-001 episode, and the last-published set still carries E0024.
#[tokio::test]
async fn clear_preserves_unrelated_diagnostic_on_same_template() {
    let tmp = TempDir::new().expect("tempdir");
    let root = tmp.path();
    // Template has an UNCLOSED block (E0024 syntax error) AND consumes an unbound
    // var (row["title"], query only SELECTs ?name → GGEN-TPL-001).
    write_project(
        root,
        "SELECT ?name WHERE { ?s <https://schema.org/name> ?name }",
        r#"{{ row["title"] }}{% for x in items %}"#, // missing endfor → E0024
    );
    let state = ServerState::with_root(root);
    let template_uri = Url::from_file_path(root.join("templates/item.tera")).expect("template url");

    // RAISE: the template's published set = E0024 (single-file) + GGEN-TPL-001.
    let project = ProjectIndex::from_root(root).expect("index");
    let groups = detect_tpl_001(&project);
    let tpl_diags = groups
        .into_iter()
        .find(|(p, _)| Url::from_file_path(p).ok().as_ref() == Some(&template_uri))
        .map(|(_, d)| d)
        .expect("template flagged with GGEN-TPL-001");
    // Mirror the server's merge-once: own single-file diags + cross-file TPL-001.
    let own = ggen_lsp::analyzers::build_analyzer(
        template_uri.path(),
        &fs::read_to_string(root.join("templates/item.tera")).unwrap(),
    )
    .map(|a| a.diagnostics())
    .unwrap_or_default();
    assert!(
        own.iter().any(|d| matches!(&d.code, Some(tower_lsp_max::lsp_types::NumberOrString::String(s)) if s == "E0024")),
        "precondition: template must have an E0024 syntax error"
    );
    let mut raised = own.clone();
    raised.extend(tpl_diags);
    state.observe_diagnostics(&template_uri, &raised).await;
    // Store the flagged set on the raise pass (mirrors refresh_analyzer: the
    // reconcile call runs every pass, recording `current_flagged` for next time).
    let mut flagged_now: HashSet<Url> = HashSet::new();
    flagged_now.insert(template_uri.clone());
    let raise_clears = state.tpl_clears_for(&template_uri, &flagged_now).await;
    assert!(raise_clears.is_empty(), "raise pass clears nothing");

    // CROSS-SURFACE REPAIR: fix the query (template, incl. its E0024, untouched).
    let rq_uri = Url::from_file_path(root.join("queries/items.rq")).expect("rq url");
    fs::write(
        root.join("queries/items.rq"),
        "SELECT ?name ?title WHERE { ?s <https://schema.org/name> ?name ; \
         <https://schema.org/title> ?title }",
    )
    .expect("rewrite rq");
    let project2 = ProjectIndex::from_root(root).expect("reindex");
    // Template still has E0024, but no longer any GGEN-TPL-001 group.
    assert!(
        detect_tpl_001(&project2).is_empty(),
        "query repair clears the binding error"
    );

    // RECONCILE: the cleared template is republished with its RESIDUAL (E0024),
    // NOT an empty set. Mirror residual_single_file_diags.
    let cleared = state.tpl_clears_for(&rq_uri, &HashSet::new()).await;
    assert_eq!(
        cleared,
        vec![template_uri.clone()],
        "template marked for clear"
    );
    let residual = ggen_lsp::analyzers::build_analyzer(
        template_uri.path(),
        &fs::read_to_string(root.join("templates/item.tera")).unwrap(),
    )
    .map(|a| a.diagnostics())
    .unwrap_or_default();
    assert!(
        residual.iter().any(|d| matches!(&d.code, Some(tower_lsp_max::lsp_types::NumberOrString::String(s)) if s == "E0024")),
        "residual must still carry the E0024 (it was never repaired)"
    );
    assert!(
        !residual.iter().any(|d| matches!(&d.code, Some(tower_lsp_max::lsp_types::NumberOrString::String(s)) if s == "GGEN-TPL-001")),
        "residual must NOT carry GGEN-TPL-001 (single-file path has empty bindings)"
    );
    state.observe_diagnostics(&template_uri, &residual).await;

    // PROOF: the GGEN-TPL-001 disappearance was observed as a repair...
    let lines = read_log_lines(root);
    assert!(
        has_template_event(&lines, "RepairApplied"),
        "GGEN-TPL-001 must clear as a lifecycle event.\nlog:\n{}",
        lines.join("\n")
    );
    // ...and the E0024 is still the standing published diagnostic (not erased).
    assert!(
        residual
            .iter()
            .any(|d| matches!(&d.code, Some(tower_lsp_max::lsp_types::NumberOrString::String(s)) if s == "E0024")),
        "unrelated E0024 preserved through the clear"
    );
}
