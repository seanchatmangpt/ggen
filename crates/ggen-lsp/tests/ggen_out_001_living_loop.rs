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
//! GGEN-OUT-001 — LIVING LSP LOOP proof (GALL-OUT-001).
//!
//! The dual of `ggen_tpl_001_living_loop.rs` on the `ggen.toml`/SPARQL surfaces.
//! GGEN-OUT-001 (`unbound_output_path`) fires when a rule's dynamic `output_file`
//! Tera pattern consumes a variable the rule's SPARQL `SELECT` does not project.
//! `ggen_core::codegen::pipeline` renders `output_file` per result row against the
//! row's bindings (`tera.render_str`), so an unbound `{{ var }}` in the output
//! path is a real cross-surface dangling reference that fails at `ggen sync` time.
//!
//! This file proves OUT-001 enters the live / headless wired loop:
//!   1. it is RAISED through the headless gate (`check_files_in_root`), anchored on
//!      `ggen.toml`, bumping `error_count`;
//!   2. it CLEARS when the source law is repaired (the SELECT projects the var);
//!   3. its repair route is source-law only (never emitted output);
//!   4. analysis never materializes an output artifact;
//!   5. the full 6-link OCEL chain (`DiagnosticRaised → RouteSelected →
//!      RepairSuggested → RepairApplied → GatePassed → ReceiptEmitted`) is written
//!      to the EXTERNAL on-disk intel log and proven by reading it back;
//!   6. OUT-001 is INDEPENDENT of TPL-001 — the invalid fixture's template body is
//!      lawful, so it raises ZERO GGEN-TPL-001 (the unbound var lives only in the
//!      output path).
//!
//! Chicago TDD: every test loads a *real* fixture project tree from disk, runs the
//! *real* headless gate / *real* `RouteRegistry`, and reads the *real* `IntelLog`.
//! No mocks, no test doubles, no fabricated diagnostics or events.

use std::path::{Path, PathBuf};

use lsp_max::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range, DocumentUri};

fn url_from_path(path: impl AsRef<std::path::Path>) -> DocumentUri {
    url::Url::from_file_path(path.as_ref())
        .expect("absolute path")
        .to_string()
        .parse::<DocumentUri>()
        .expect("valid uri")
}

use ggen_lsp::check::{check_files_in_root, discover_law_surfaces, CheckReport};
use ggen_lsp::route::{Provenance, RouteRegistry};
use ggen_lsp::ServerState;

/// Absolute path to a fixture project root under this crate's `tests/fixtures`.
fn fixture_root(name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join("ggen_out_001_living_loop")
        .join(name)
}

fn is_code(d: &Diagnostic, code: &str) -> bool {
    matches!(&d.code, Some(NumberOrString::String(s)) if s == code)
}

fn count_code_in_report(report: &CheckReport, code: &str) -> usize {
    report
        .files
        .iter()
        .flat_map(|f| f.diagnostics.iter())
        .filter(|d| is_code(d, code))
        .count()
}

fn has_out_001_error_in_report(report: &CheckReport) -> bool {
    report
        .files
        .iter()
        .flat_map(|f| f.diagnostics.iter())
        .any(|d| is_code(d, "GGEN-OUT-001") && d.severity == Some(DiagnosticSeverity::ERROR))
}

/// The canonical key the live server + headless gate use to look a route up
/// (`select_for_diagnostic` keys on the code). NOT a fabricated detection: tests
/// #1/#2 drive real detection through the gate.
fn out_001_diag() -> Diagnostic {
    Diagnostic {
        range: Range {
            start: Position {
                line: 0,
                character: 0,
            },
            end: Position {
                line: 0,
                character: 20,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(NumberOrString::String("GGEN-OUT-001".to_string())),
        code_description: None,
        source: Some("ggen-lsp".to_string()),
        message: "unbound output path: `slug`".to_string(),
        related_information: None,
        tags: None,
        data: None,
    }
}

/// Recursively copy a directory tree using only std (+ tempfile). Real I/O.
fn copy_tree(src: &Path, dst: &Path) -> std::io::Result<()> {
    std::fs::create_dir_all(dst)?;
    for entry in std::fs::read_dir(src)? {
        let entry = entry?;
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if entry.file_type()?.is_dir() {
            copy_tree(&from, &to)?;
        } else {
            std::fs::copy(&from, &to)?;
        }
    }
    Ok(())
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

/// True if some JSONL event line names this `activity`, the GGEN-OUT-001 `code`,
/// and the `ggen.toml` manifest — three substrings on one line identify a single
/// event.
fn has_manifest_event(lines: &[String], activity: &str) -> bool {
    lines.iter().any(|l| {
        l.contains(&format!("\"activity\":\"{activity}\""))
            && l.contains("ggen.toml")
            && l.contains("GGEN-OUT-001")
    })
}

// ---------------------------------------------------------------------------
// 1. LIVE RAISE: unbound output_file raises GGEN-OUT-001 through the gate.
// ---------------------------------------------------------------------------

#[test]
fn invalid_output_path_raises_out_001_through_headless_gate() {
    let root = fixture_root("invalid_project");
    let paths = discover_law_surfaces(&root);
    assert!(
        !paths.is_empty(),
        "fixture must contain discoverable law surfaces (ggen.toml/.rq/.tera/.ttl)"
    );

    let report = check_files_in_root(&root, &paths, true);

    assert!(
        count_code_in_report(&report, "GGEN-OUT-001") >= 1,
        "the project binds only `name` but output_file consumes `{{{{ slug }}}}`; the \
         headless gate must surface GGEN-OUT-001. report.files: {:?}",
        report.files
    );
    assert!(
        has_out_001_error_in_report(&report),
        "GGEN-OUT-001 must be reported at ERROR severity. report.files: {:?}",
        report.files
    );
    assert!(
        report.error_count >= 1,
        "a GGEN-OUT-001 ERROR must increment CheckReport.error_count. error_count={}",
        report.error_count
    );
    assert!(report.has_errors(), "the gate must fail on GGEN-OUT-001");

    // The diagnostic must anchor on the ggen.toml manifest (declaration surface),
    // NOT the template or an emitted output.
    let manifest_report = report
        .files
        .iter()
        .find(|f| f.path.ends_with("ggen.toml"))
        .expect("ggen.toml report present");
    assert!(
        manifest_report
            .diagnostics
            .iter()
            .any(|d| is_code(d, "GGEN-OUT-001")),
        "GGEN-OUT-001 must anchor on ggen.toml. report: {manifest_report:?}"
    );
}

// ---------------------------------------------------------------------------
// 1b. INDEPENDENCE: the OUT invalid fixture raises ZERO GGEN-TPL-001.
// ---------------------------------------------------------------------------

#[test]
fn invalid_output_path_raises_zero_tpl_001() {
    // The template body consumes only the bound `name`, so TPL-001 must stay
    // silent: the defect lives only in the output_file pattern. Proves OUT-001 is
    // a distinct species, not a relabelling of TPL-001.
    let root = fixture_root("invalid_project");
    let paths = discover_law_surfaces(&root);
    let report = check_files_in_root(&root, &paths, true);

    assert_eq!(
        count_code_in_report(&report, "GGEN-TPL-001"),
        0,
        "the template body is lawful (`name` only); GGEN-TPL-001 must NOT fire on an \
         output-path-only unbound var. report.files: {:?}",
        report.files
    );
    // Sanity: OUT-001 IS present (so the zero-TPL above is not vacuous).
    assert!(
        count_code_in_report(&report, "GGEN-OUT-001") >= 1,
        "OUT-001 must still fire (else the zero-TPL assertion is vacuous)"
    );
}

// ---------------------------------------------------------------------------
// 2. LIVE CLEAR: repairing the SPARQL SELECT clears GGEN-OUT-001.
// ---------------------------------------------------------------------------

#[test]
fn repaired_select_clears_out_001_through_headless_gate() {
    let src = fixture_root("invalid_project");
    let temp = tempfile::tempdir().expect("create temp dir");
    let dst = temp.path().join("project");
    copy_tree(&src, &dst).expect("copy fixture tree");

    // Pre-repair: the copy must still raise GGEN-OUT-001.
    let before_paths = discover_law_surfaces(&dst);
    let before = check_files_in_root(&dst, &before_paths, true);
    assert!(
        count_code_in_report(&before, "GGEN-OUT-001") >= 1,
        "pre-repair copy must raise GGEN-OUT-001 (else the clear is vacuous). \
         report.files: {:?}",
        before.files
    );

    // Repair: rewrite the SPARQL query to PROJECT `slug` so the output_file binds.
    let rq_path = dst.join("queries").join("items.rq");
    std::fs::write(
        &rq_path,
        "PREFIX ex: <http://example.org/out001#>\n\
         SELECT ?name ?slug WHERE {\n\
         \x20\x20\x20\x20?s a ex:Item .\n\
         \x20\x20\x20\x20?s ex:name ?name .\n\
         \x20\x20\x20\x20?s ex:slug ?slug .\n\
         }\n",
    )
    .expect("rewrite query to project slug");

    // Post-repair: GGEN-OUT-001 must clear.
    let after_paths = discover_law_surfaces(&dst);
    let after = check_files_in_root(&dst, &after_paths, true);
    assert_eq!(
        count_code_in_report(&after, "GGEN-OUT-001"),
        0,
        "after projecting `slug` in the SELECT, GGEN-OUT-001 must clear. \
         report.files: {:?}",
        after.files
    );
}

// ---------------------------------------------------------------------------
// 3. ROUTE METADATA: GGEN-OUT-001's repair route is source-law only.
// ---------------------------------------------------------------------------

#[test]
fn out_001_route_is_source_law_only() {
    let registry = RouteRegistry::seeded();
    let diag = out_001_diag();

    let route = registry
        .select_for_diagnostic(&diag)
        .expect("GGEN-OUT-001 must resolve to a seeded repair route");

    assert_eq!(
        route.id.0, "source-law.bind-output-path",
        "GGEN-OUT-001 must select the output-path source-law route"
    );
    assert_eq!(
        route.provenance,
        Provenance::Seeded,
        "the GGEN-OUT-001 route must be a seeded (cold-start, doctrine) route"
    );

    // No step may reference an EMITTED-output marker; every step must name a
    // source-law surface (SPARQL SELECT or the ggen.toml `output_file` field).
    // NOTE: "output_file" is the literal ggen.toml field name (source law), so it
    // is NOT in the forbidden list — only emitted-output PATH fragments are.
    const FORBIDDEN_OUTPUT: &[&str] = &["out/", "output/", "dist/", "gen/", "emitted"];
    assert!(
        !route.steps.nodes.is_empty(),
        "the source-law route must offer at least one step"
    );
    for step in &route.steps.nodes {
        let title = step.title.to_lowercase();
        for forbidden in FORBIDDEN_OUTPUT {
            assert!(
                !title.contains(forbidden),
                "GGEN-OUT-001 route step {:?} references forbidden emitted-output \
                 marker {:?} — repairs are source-law only",
                step.title,
                forbidden
            );
        }
        assert!(
            title.contains("sparql")
                || title.contains("ggen.toml")
                || title.contains("output_file"),
            "GGEN-OUT-001 route step {:?} must reference a source-law surface \
             (SPARQL SELECT / ggen.toml output_file)",
            step.title
        );
    }
}

// ---------------------------------------------------------------------------
// 4. NO ARTIFACT: running the gate never materializes the declared output_file.
// ---------------------------------------------------------------------------

#[test]
fn headless_gate_never_materializes_output_dir() {
    let src = fixture_root("invalid_project");
    let temp = tempfile::tempdir().expect("create temp dir");
    let dst = temp.path().join("project");
    copy_tree(&src, &dst).expect("copy fixture tree");

    let out_dir = dst.join("out");
    assert!(
        !out_dir.exists(),
        "fixture copy unexpectedly already contains the emitted-output dir"
    );

    let paths = discover_law_surfaces(&dst);
    let report = check_files_in_root(&dst, &paths, true);

    assert!(
        report.has_errors(),
        "the gate must observe the GGEN-OUT-001 ERROR (sanity it ran)"
    );
    assert!(
        !out_dir.exists(),
        "the headless gate must be read-only and must NOT materialize the emitted \
         output dir ({}). A live/analysis path that emits artifacts conflates \
         refusal with execution.",
        out_dir.display()
    );
}

// ---------------------------------------------------------------------------
// 5. OCEL CHAIN: analyze_and_observe records the full live lifecycle.
// ---------------------------------------------------------------------------

/// Drives the REAL `ServerState::analyze_and_observe` orchestration (the
/// Client-free core of the editor publish path): raise GGEN-OUT-001 by analyzing
/// the project, then repair the SPARQL SELECT (cross-surface) and re-analyze so
/// the now-lawful manifest is cleared through the SAME `observe_diagnostics`.
/// The full 6-link chain is proven by reading the EXTERNAL on-disk OCEL log.
#[tokio::test]
async fn analyze_and_observe_records_live_out_001_receipt_chain() {
    // ── Arrange: invalid project copied into a hermetic TempDir.
    let src = fixture_root("invalid_project");
    let tmp = tempfile::tempdir().expect("tempdir");
    let root = tmp.path().join("project");
    copy_tree(&src, &root).expect("copy fixture tree");
    let state = ServerState::with_root(&root);

    let manifest_path = root.join("ggen.toml");
    let manifest_uri = url_from_path(&manifest_path);
    let rq_path = root.join("queries/items.rq");
    let rq_uri = url_from_path(&rq_path);

    // ── Act 1 — RAISE: analyze the SPARQL query (a TPL/OUT trigger surface) so the
    // orchestration recomputes the project graph and flags the ggen.toml manifest.
    let rq_src = std::fs::read_to_string(&rq_path).expect("read rq");
    let raised = state.analyze_and_observe(&rq_uri, &rq_src).await;
    assert!(
        raised.iter().any(|(u, diags)| u == &manifest_uri
            && diags.iter().any(|d| is_code(&d.lsp, "GGEN-OUT-001"))),
        "analyze_and_observe must raise GGEN-OUT-001 on the ggen.toml manifest. \
         published: {raised:?}"
    );

    // ── Act 2 — CROSS-SURFACE REPAIR: project `slug` in the SELECT, then analyze
    // the query URI so the orchestration reconciles the now-lawful manifest.
    let repaired_rq = "PREFIX ex: <http://example.org/out001#>\n\
                       SELECT ?name ?slug WHERE { ?s a ex:Item . ?s ex:name ?name . \
                       ?s ex:slug ?slug . }\n";
    std::fs::write(&rq_path, repaired_rq).expect("rewrite rq");
    let cleared = state.analyze_and_observe(&rq_uri, repaired_rq).await;
    assert!(
        cleared.iter().any(|(u, _)| u == &manifest_uri),
        "the query-side repair must re-publish (clear) the ggen.toml URI through the \
         orchestration. published: {cleared:?}"
    );

    // ── Assert: the EXTERNAL intel log proves the full live chain on the manifest.
    let lines = read_log_lines(&root);
    for activity in [
        "DiagnosticRaised",
        "RouteSelected",
        "RepairSuggested",
        "RepairApplied",
        "GatePassed",
        "ReceiptEmitted",
    ] {
        assert!(
            has_manifest_event(&lines, activity),
            "live chain link {activity:?} for GGEN-OUT-001 on the ggen.toml manifest \
             missing.\nlog:\n{}",
            lines.join("\n")
        );
    }

    // ── And: the live analysis path materialized no emitted artifact.
    assert!(
        !root.join("out").exists(),
        "analyze_and_observe must never materialize the rule's output_file dir"
    );
}

// ---------------------------------------------------------------------------
// 6. VALID FIXTURE: a bound output_file pattern raises zero GGEN-OUT-001.
// ---------------------------------------------------------------------------

#[test]
fn valid_output_path_stays_clean() {
    let root = fixture_root("valid_project");
    let paths = discover_law_surfaces(&root);
    let report = check_files_in_root(&root, &paths, true);

    assert_eq!(
        count_code_in_report(&report, "GGEN-OUT-001"),
        0,
        "the output_file binds the projected `name`; no GGEN-OUT-001 expected. \
         report.files: {:?}",
        report.files
    );
    assert!(
        !report.has_errors(),
        "the valid fixture must pass the gate. report.files: {:?}",
        report.files
    );
}
