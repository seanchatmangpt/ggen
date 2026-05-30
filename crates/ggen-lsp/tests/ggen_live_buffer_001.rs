//! LIVE-BUFFER-001 — the cross-surface living loop LIVE ON THE OPEN BUFFER.
//!
//! The existing GGEN-TPL-001 living-loop proof
//! (`ggen_tpl_001_living_loop.rs::analyze_and_observe_records_live_receipt_chain`)
//! repairs the SPARQL query by WRITING it to DISK before re-analyzing. That
//! on-disk write masks whether the loop is buffer-live: it cannot distinguish a
//! clear driven by the editor's unsaved buffer from one driven by the saved file.
//!
//! This proof closes that gap. It applies the query repair **BUFFER-ONLY** via
//! `analyze_and_observe(&rq_uri, repaired_rq)` while leaving the disk `.rq`
//! UNCHANGED (still broken). The consumer template's GGEN-TPL-001 must clear, the
//! external on-disk OCEL log must show the full 6-link chain, AND a read-back of
//! the disk `.rq` must confirm it STILL holds the broken `SELECT ?name`-only
//! query — proving the clear came from the BUFFER, not disk. No artifact is
//! written.
//!
//! Chicago TDD: a real project tree on disk, the real `ServerState` orchestration,
//! the real `RouteRegistry`, and the real `IntelLog` read back from its external
//! JSONL surface. No mocks, no test doubles, no fabricated diagnostics or events.

use std::path::Path;

use tower_lsp::lsp_types::{Diagnostic, NumberOrString, Url};

use ggen_lsp::ServerState;

/// True if a `Diagnostic.code` renders as exactly `GGEN-TPL-001`.
fn is_tpl_001(d: &Diagnostic) -> bool {
    matches!(
        &d.code,
        Some(NumberOrString::String(s)) if s == "GGEN-TPL-001"
    )
}

/// Write a minimal valid ggen project: one rule binding `queries/items.rq` to
/// `templates/item.tera` with output `out.txt`. Mirrors the disk-driven living-loop
/// fixture so this buffer-only proof exercises the same cross-surface relation.
fn write_project(dir: &Path, query: &str, template: &str) {
    std::fs::create_dir_all(dir.join("schema")).expect("schema dir");
    std::fs::create_dir_all(dir.join("queries")).expect("queries dir");
    std::fs::create_dir_all(dir.join("templates")).expect("templates dir");
    std::fs::write(
        dir.join("schema/domain.ttl"),
        "@prefix schema: <https://schema.org/> .\n",
    )
    .expect("ttl");
    std::fs::write(dir.join("queries/items.rq"), query).expect("rq");
    std::fs::write(dir.join("templates/item.tera"), template).expect("tera");
    std::fs::write(
        dir.join("ggen.toml"),
        r#"[project]
name = "live-buffer-fixture"
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
/// `<root>/.ggen/ocel/agent-edit-events.ocel.jsonl` — the unforgeable on-disk
/// surface the anti-cheating doctrine requires (not an in-process accessor).
fn read_log_lines(root: &Path) -> Vec<String> {
    let path = root
        .join(".ggen")
        .join("ocel")
        .join("agent-edit-events.ocel.jsonl");
    std::fs::read_to_string(path)
        .unwrap_or_default()
        .lines()
        .map(str::to_string)
        .collect()
}

/// True if some JSONL event line names this `activity`, the GGEN-TPL-001 `code`,
/// and the template file — three substrings on one line identify a single event.
fn has_template_event(lines: &[String], activity: &str) -> bool {
    lines.iter().any(|l| {
        l.contains(&format!("\"activity\":\"{activity}\""))
            && l.contains("item.tera")
            && l.contains("GGEN-TPL-001")
    })
}

// ---------------------------------------------------------------------------
// BUFFER-LIVE CLEAR: a query repair held ONLY in the open buffer (NOT written to
// disk) clears the consumer template's GGEN-TPL-001 through the SAME
// `observe_diagnostics` the editor uses — with the disk `.rq` still broken.
// ---------------------------------------------------------------------------

#[tokio::test]
async fn buffer_only_query_repair_clears_template_tpl_001_with_disk_still_broken() {
    // ── Arrange: invalid project (query SELECTs ?name; template wants title).
    let tmp = tempfile::tempdir().expect("tempdir");
    let root = tmp.path();
    let broken_query = "SELECT ?name WHERE { ?s <https://schema.org/name> ?name }";
    write_project(root, broken_query, r#"{{ row["title"] }}"#);
    let state = ServerState::with_root(root);

    let tera_path = root.join("templates/item.tera");
    let tera_uri = Url::from_file_path(&tera_path).expect("tera url");
    let rq_path = root.join("queries/items.rq");
    let rq_uri = Url::from_file_path(&rq_path).expect("rq url");

    // ── Act 1 — RAISE: analyze the template through the real orchestration.
    // The query is still broken (both on disk AND in any buffer), so the template
    // consumes the unbound `title` and GGEN-TPL-001 fires.
    let tera_src = std::fs::read_to_string(&tera_path).expect("read tera");
    let raised = state.analyze_and_observe(&tera_uri, &tera_src).await;
    assert!(
        raised
            .iter()
            .any(|(u, diags)| u == &tera_uri && diags.iter().any(is_tpl_001)),
        "analyze_and_observe must raise GGEN-TPL-001 on the template. published: {raised:?}"
    );

    // ── Act 2 — BUFFER-ONLY CROSS-SURFACE REPAIR. Repair the QUERY purely in the
    // editor buffer: pass the repaired query as `content` to `analyze_and_observe`
    // for the `.rq` URI, and DO NOT write it to disk. The disk `.rq` stays broken.
    // The overlay built inside `analyze_and_observe` splices in this `(rq_uri,
    // repaired_rq)` pair, so the cross-surface ProjectIndex sees the repaired query
    // and the consumer template becomes lawful.
    let repaired_rq = "SELECT ?name ?title WHERE { ?s <https://schema.org/name> ?name ; \
                       <https://schema.org/title> ?title }";
    let cleared = state.analyze_and_observe(&rq_uri, repaired_rq).await;
    assert!(
        cleared.iter().any(|(u, _)| u == &tera_uri),
        "the BUFFER-ONLY query repair must re-publish (clear) the template URI through \
         the orchestration. published: {cleared:?}"
    );
    // The re-published template set must carry NO GGEN-TPL-001 (it cleared).
    let tera_after: Vec<&Diagnostic> = cleared
        .iter()
        .filter(|(u, _)| u == &tera_uri)
        .flat_map(|(_, d)| d.iter())
        .filter(|d| is_tpl_001(d))
        .collect();
    assert!(
        tera_after.is_empty(),
        "after the buffer-only query repair, the template must carry NO GGEN-TPL-001. \
         residual TPL diags: {tera_after:?}"
    );

    // ── PROOF that the clear is BUFFER-driven, not disk-driven: the disk `.rq`
    // STILL holds the broken `SELECT ?name`-only query. If a future change wrote the
    // repair to disk (or read disk instead of the buffer), this assertion fails loud.
    let disk_rq = std::fs::read_to_string(&rq_path).expect("read disk rq");
    assert_eq!(
        disk_rq, broken_query,
        "the disk `.rq` MUST still hold the broken query — the clear must come from the \
         OPEN BUFFER, not disk. If this differs, the repair leaked to disk."
    );
    assert!(
        !disk_rq.contains("title"),
        "the disk query must NOT project `title` (it is still broken on disk)"
    );

    // ── Assert: the EXTERNAL intel log proves the full live 6-link chain on the
    // template — raised then cleared, all from the buffer-only edit.
    let lines = read_log_lines(root);
    for activity in [
        "DiagnosticRaised",
        "RouteSelected",
        "RepairSuggested",
        "RepairApplied",
        "GatePassed",
        "ReceiptEmitted",
    ] {
        assert!(
            has_template_event(&lines, activity),
            "buffer-live chain link {activity:?} for GGEN-TPL-001 on the template missing.\nlog:\n{}",
            lines.join("\n")
        );
    }

    // ── And: the buffer-live analysis path materialized no emitted artifact.
    assert!(
        !root.join("out.txt").exists(),
        "analyze_and_observe must never write the rule's output_file"
    );
}
