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
//! GGEN-TPL-001 — LIVING LSP LOOP proof (Checkpoint-001B, Agent 3).
//!
//! Checkpoint-001 / Agent 4 already prove the *pure* detector
//! (`analyzers::detect_tpl_001` over a `ProjectIndex`). This file proves a
//! different thing: that GGEN-TPL-001 enters the **live / headless wired loop** —
//! it is *raised through* the deterministically-drivable live path (the headless
//! gate `check_files_in_root`), it *clears* when the source law is repaired, its
//! repair route is **source-law only** (never emitted output), and the analysis
//! never materializes an output artifact. The OCEL `observe_diagnostics` chain is
//! attempted and, where the public test surface cannot drive it without editing
//! `src/`, documented as an exact pending seam (test #5).
//!
//! Chicago TDD: every test loads a *real* fixture project tree from disk, runs the
//! *real* headless gate / *real* `RouteRegistry`, and (where possible) reads the
//! *real* `IntelLog`. No mocks, no test doubles, no fabricated diagnostics or
//! events.
//!
//! ## Shared API contract this file depends on (binding)
//!
//! * **Agent 2 — `ggen_lsp::check::check_files_in_root(root: &Path, paths: &[PathBuf],
//!   with_routes: bool) -> CheckReport`** MUST fold project-level GGEN-TPL-001
//!   diagnostics into the per-file `CheckReport` so that a project whose template
//!   consumes an unbound projection variable yields a `GGEN-TPL-001` diagnostic at
//!   `DiagnosticSeverity::ERROR` somewhere in `report.files[*].diagnostics`, and
//!   `report.error_count >= 1`. Until that wiring lands, tests #1 and #2 are
//!   EXPECTED RED (the gate currently only runs single-file analyzers, which never
//!   see the cross-surface SPARQL↔template binding). See the Agent 3 handoff.
//! * **Source-law route (already landed)** — `ggen_lsp::RouteRegistry::seeded()
//!   .select_for_diagnostic(&Diagnostic{code:"GGEN-TPL-001",..})` resolves to a
//!   `RepairRoute` whose `id` is `source-law.bind-projection`, whose `provenance`
//!   is `Seeded`, and whose every step is advisory and references only a
//!   source-law surface (SPARQL / Tera template / ggen.toml) — never an emitted
//!   output marker. Test #3 asserts this through the *same* registry path the live
//!   server (`state.rs` `observe_diagnostics`) and the headless gate use.

use std::path::{Path, PathBuf};

use lsp_max::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString, Position, Range, Url};

fn url_from_path(path: impl AsRef<std::path::Path>) -> Url {
    url::Url::from_file_path(path.as_ref())
        .expect("absolute path")
        .to_string()
        .parse::<Url>()
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
        .join("ggen_tpl_001_living_loop")
        .join(name)
}

/// True if a `Diagnostic.code` renders as exactly `GGEN-TPL-001`.
fn is_tpl_001(d: &Diagnostic) -> bool {
    matches!(
        &d.code,
        Some(NumberOrString::String(s)) if s == "GGEN-TPL-001"
    )
}

/// Count GGEN-TPL-001 diagnostics across every file report in a `CheckReport`.
fn count_tpl_001_in_report(report: &CheckReport) -> usize {
    report
        .files
        .iter()
        .flat_map(|f| f.diagnostics.iter())
        .filter(|d| is_tpl_001(d))
        .count()
}

/// True if any GGEN-TPL-001 diagnostic in the report is at ERROR severity.
fn has_tpl_001_error_in_report(report: &CheckReport) -> bool {
    report
        .files
        .iter()
        .flat_map(|f| f.diagnostics.iter())
        .any(|d| is_tpl_001(d) && d.severity == Some(DiagnosticSeverity::ERROR))
}

/// Build a synthetic GGEN-TPL-001 diagnostic for the *route-selection* test.
/// This is NOT fabricating a detection (tests #1/#2 drive real detection through
/// the gate) — it is the canonical key the live server and headless gate both use
/// to look a route up in the registry (`select_for_diagnostic` keys on the code).
fn tpl_001_diag() -> Diagnostic {
    Diagnostic {
        range: Range {
            start: Position {
                line: 2,
                character: 0,
            },
            end: Position {
                line: 2,
                character: 20,
            },
        },
        severity: Some(DiagnosticSeverity::ERROR),
        code: Some(NumberOrString::String("GGEN-TPL-001".to_string())),
        code_description: None,
        source: Some("ggen-lsp".to_string()),
        message: "unbound projection variable: `title`".to_string(),
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
        let file_type = entry.file_type()?;
        let from = entry.path();
        let to = dst.join(entry.file_name());
        if file_type.is_dir() {
            copy_tree(&from, &to)?;
        } else {
            std::fs::copy(&from, &to)?;
        }
    }
    Ok(())
}

/// Write a minimal valid ggen project: one rule binding `queries/items.rq` to
/// `templates/item.tera` with output `out.txt`. Mirrors the proven stale-clear
/// scenario so the seam-driven test exercises the same cross-surface relation.
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
name = "living-loop-seam-fixture"
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
// 1. LIVE RAISE: invalid template raises GGEN-TPL-001 through the headless gate.
// ---------------------------------------------------------------------------

/// The headless gate (`check_files_in_root`) is the deterministically-drivable
/// "live" path: the same analyzers + project-index fold that the editor publish
/// path runs, but over files on disk with a serializable report. A project whose
/// query binds only `?name` while its template consumes `row["title"]` must
/// surface a GGEN-TPL-001 ERROR in the gate's report and bump `error_count`.
///
/// EXPECTED RED until Agent 2 folds `detect_tpl_001(ProjectIndex::from_root(root))`
/// into `check_files_in_root`. The single-file analyzers alone cannot see the
/// cross-surface SPARQL↔template binding, so the gate reports zero TPL-001 today.
#[test]
fn invalid_template_raises_tpl_001_through_headless_gate() {
    let root = fixture_root("invalid_project");
    let paths = discover_law_surfaces(&root);
    assert!(
        !paths.is_empty(),
        "fixture must contain discoverable law surfaces (ggen.toml/.rq/.tera/.ttl)"
    );

    let report = check_files_in_root(&root, &paths, true);

    assert!(
        count_tpl_001_in_report(&report) >= 1,
        "the project binds only `name` but the template consumes `title`; the \
         headless gate must surface GGEN-TPL-001. (EXPECTED RED until Agent 2 \
         folds detect_tpl_001 into check_files_in_root.) report.files: {:?}",
        report.files
    );
    assert!(
        has_tpl_001_error_in_report(&report),
        "GGEN-TPL-001 must be reported at ERROR severity in the gate report. \
         report.files: {:?}",
        report.files
    );
    assert!(
        report.error_count >= 1,
        "a GGEN-TPL-001 ERROR must increment CheckReport.error_count (the hook \
         refusal signal). error_count={}",
        report.error_count
    );
    assert!(
        report.has_errors(),
        "CheckReport::has_errors() must be true when GGEN-TPL-001 fires"
    );
}

// ---------------------------------------------------------------------------
// 2. LIVE CLEAR: repairing the source law clears GGEN-TPL-001.
// ---------------------------------------------------------------------------

/// Copy the invalid fixture to a TempDir, rewrite the `.tera` so it consumes the
/// *bound* variable (`row["name"]`), re-run the headless gate, and assert
/// GGEN-TPL-001 has cleared. This is the "publish → clear" half of the living
/// loop expressed through the deterministic gate: the diagnostic disappears
/// because the source law was repaired, not because detection was disabled.
///
/// EXPECTED RED until Agent 2 lands (same dependency as #1): if the gate never
/// raises TPL-001, "it cleared" is vacuously true and the *raise* sub-assertion
/// below fails first, which is the honest signal.
#[test]
fn repaired_template_clears_tpl_001_through_headless_gate() {
    let src = fixture_root("invalid_project");
    let temp = tempfile::tempdir().expect("create temp dir");
    let dst = temp.path().join("project");
    copy_tree(&src, &dst).expect("copy fixture tree");

    // Pre-repair: the copy must still raise GGEN-TPL-001 (sanity that the copy is
    // faithful and the gate sees the defect). EXPECTED RED until Agent 2 lands.
    let before_paths = discover_law_surfaces(&dst);
    let before = check_files_in_root(&dst, &before_paths, true);
    assert!(
        count_tpl_001_in_report(&before) >= 1,
        "pre-repair copy must raise GGEN-TPL-001 (else the clear is vacuous). \
         (EXPECTED RED until Agent 2 folds detect_tpl_001 into the gate.) \
         report.files: {:?}",
        before.files
    );

    // Repair: rewrite the template to consume the BOUND projection variable.
    let tera_path = dst.join("templates").join("item.tera");
    std::fs::write(
        &tera_path,
        "{# Repaired: `name` IS bound by the query. #}\nItem name: {{ row[\"name\"] }}\n",
    )
    .expect("rewrite template to bound variable");

    // Post-repair: the gate must no longer raise GGEN-TPL-001 (diagnostic clears).
    let after_paths = discover_law_surfaces(&dst);
    let after = check_files_in_root(&dst, &after_paths, true);
    assert_eq!(
        count_tpl_001_in_report(&after),
        0,
        "after repairing the template to consume the bound `name`, GGEN-TPL-001 \
         must clear. report.files: {:?}",
        after.files
    );
}

// ---------------------------------------------------------------------------
// 3. ROUTE METADATA: GGEN-TPL-001's repair route is source-law only.
// ---------------------------------------------------------------------------

/// The live server (`state.rs` `observe_diagnostics`) and the headless gate both
/// select a repair route via `RouteRegistry::select_for_diagnostic`. For
/// GGEN-TPL-001 that route MUST be `source-law.bind-projection`, seeded, and must
/// reference NO emitted-output marker — repairing an unbound projection lives only
/// in source law (SPARQL SELECT / Tera template / ggen.toml rule), never in
/// generated output. This proves the *route the live loop would offer* is lawful,
/// independent of whether Agent 2's detection wiring has landed yet.
#[test]
fn tpl_001_route_is_source_law_only() {
    let registry = RouteRegistry::seeded();
    let diag = tpl_001_diag();

    let route = registry
        .select_for_diagnostic(&diag)
        .expect("GGEN-TPL-001 must resolve to a seeded repair route");

    assert_eq!(
        route.id.0, "source-law.bind-projection",
        "GGEN-TPL-001 must select the source-law route, not a TemplateFailure seed"
    );
    assert_eq!(
        route.provenance,
        Provenance::Seeded,
        "the GGEN-TPL-001 route must be a seeded (cold-start, doctrine) route"
    );

    // No step may reference an emitted-output marker; every step must name a
    // source-law surface.
    const FORBIDDEN_OUTPUT: &[&str] = &["out/", "output/", "dist/", "gen/", "emitted", "out.txt"];
    assert!(
        !route.steps.nodes.is_empty(),
        "the source-law route must offer at least one step"
    );
    for step in &route.steps.nodes {
        let title = step.title.to_lowercase();
        for forbidden in FORBIDDEN_OUTPUT {
            assert!(
                !title.contains(forbidden),
                "GGEN-TPL-001 route step {:?} references forbidden emitted-output \
                 marker {:?} — repairs are source-law only",
                step.title,
                forbidden
            );
        }
        assert!(
            title.contains("sparql") || title.contains("template") || title.contains("ggen.toml"),
            "GGEN-TPL-001 route step {:?} must reference a source-law surface \
             (SPARQL / Tera template / ggen.toml)",
            step.title
        );
    }
    assert!(
        !route.description.to_lowercase().contains("out.txt"),
        "the route description must not name an emitted-output file"
    );
}

// ---------------------------------------------------------------------------
// 4. NO ARTIFACT: running the gate never materializes the declared output_file.
// ---------------------------------------------------------------------------

/// The whole detection path is read-only: running `check_files_in_root` over a
/// TempDir copy of the invalid project must NEVER write the declared
/// `output_file` ("out.txt"). A live loop that materializes output during
/// *analysis* would conflate refusal with execution — the defining failure the
/// source-law-only doctrine forbids.
#[test]
fn headless_gate_never_materializes_output_file() {
    let src = fixture_root("invalid_project");
    let temp = tempfile::tempdir().expect("create temp dir");
    let dst = temp.path().join("project");
    copy_tree(&src, &dst).expect("copy fixture tree");

    let declared_output = dst.join("out.txt");
    assert!(
        !declared_output.exists(),
        "fixture copy unexpectedly already contains the declared output file"
    );

    let paths = discover_law_surfaces(&dst);
    let _ = check_files_in_root(&dst, &paths, true);

    assert!(
        !declared_output.exists(),
        "the headless gate must be read-only and must NOT write the declared \
         output_file ({}). A live/analysis path that emits artifacts conflates \
         refusal with execution.",
        declared_output.display()
    );
}

// ---------------------------------------------------------------------------
// 5. OCEL CHAIN: observe_diagnostics records the editor-flow lifecycle.
// ---------------------------------------------------------------------------

/// PENDING SEAM — documents the exact missing public test surface needed to prove
/// the live editor loop records the OCEL chain
/// `DiagnosticRaised → RouteSelected → RepairSuggested` (and on repair
/// `RepairApplied → GatePassed → ReceiptEmitted`) via the `IntelLog` the server
/// writes under the project root.
///
/// ## Why this cannot run today WITHOUT editing `src/`
///
/// The only public entry to the live editor loop is the tower-lsp trait impl on
/// `ggen_lsp::GgenLanguageServer` (`did_open`/`did_change` → `refresh_analyzer`
/// → `ServerState::observe_diagnostics` → `IntelLog::at_root(root).append(...)`).
/// Driving that loop from an integration test requires BOTH of:
///
///   1. **A constructible `GgenLanguageServer` with an injectable root.**
///      `GgenLanguageServer::new(client: Client)` (server.rs:13) is the ONLY
///      public constructor. It (a) requires a `lsp_max::Client`, which is
///      produced solely by `LspService::new(...)` inside `run_stdio` and is not
///      independently constructible in a unit test, and (b) hard-codes
///      `ServerState::default()` (server.rs:15) whose `root` is `current_dir()`
///      — so even if a `Client` existed, the OCEL log would land under the test
///      process CWD, not a hermetic TempDir. `ServerState::with_root(root)` is
///      public (state.rs:96) but `GgenLanguageServer` exposes no constructor that
///      accepts a pre-built `ServerState`/root, and its `state` field is private.
///
///   2. **A way to invoke `observe_diagnostics` with an injected root through a
///      public API.** `ServerState::observe_diagnostics` (state.rs:127) IS public
///      and IS the loop that writes the chain, but it is a method on a
///      `ServerState` that the live `did_open`/`did_change` path only ever builds
///      via the private `state` field of a `GgenLanguageServer`.
///
/// ### Exact seam that would make this runnable (orchestrator request — NOT done
/// here; this agent does not edit `src/`):
///
/// EITHER of the following public, test-only seams would suffice (smallest first):
///
///   * **`GgenLanguageServer::with_state(client: Client, state: Arc<ServerState>)`**
///     (or `GgenLanguageServer::new_in_root(client, root)`) so a test can pin the
///     OCEL root to a TempDir; combined with a tower-lsp test harness that yields
///     a `Client` (e.g. `LspService::new` + an in-memory duplex), `did_open` could
///     then be driven and the `IntelLog::at_root(temp)` read back.
///
///   * **A public, root-injected façade** that runs the *same* publish path as
///     `refresh_analyzer` without a `Client`, e.g.
///     `ServerState::analyze_and_observe(&self, uri: &Url, content: &str)` that
///     builds the analyzer, calls `observe_diagnostics`, and returns the
///     diagnostics (no `publish_diagnostics`). A test would then:
///       1. `let state = ServerState::with_root(temp);`
///       2. `state.analyze_and_observe(&tera_uri, invalid_src).await;` // raise
///       3. `state.analyze_and_observe(&tera_uri, repaired_src).await;` // clear
///       4. `let log = IntelLog::at_root(temp.path()).read();`
///       5. assert the activity sequence on `log.events` contains
///          `DiagnosticRaised`, `RouteSelected`, `RepairSuggested`, then on clear
///          `RepairApplied`, `GatePassed`, `ReceiptEmitted` for the same episode.
///     NOTE: this façade must additionally route the GGEN-TPL-001 diagnostic
///     (cross-surface) into `observe_diagnostics`; today `refresh_analyzer` only
///     observes single-file analyzer diagnostics (the same Agent 1 wiring gap that
///     makes the editor path raise TPL-001 at all).
///
/// ### What IS already proven without this seam
///
/// * The OCEL builders + `IntelLog` round-trip (read what was appended) are unit-
///   tested in `src/intel/log.rs` and `src/intel/events.rs`.
/// * `observe_diagnostics` (state.rs:127) is the real, public method that appends
///   exactly the `DiagnosticRaised → RouteSelected → RepairSuggested` /
///   `RepairApplied → GatePassed → ReceiptEmitted` chain — verifiable by reading
///   its body; what is UNOBSERVABLE from a black-box test is the *live driving* of
///   it through the tower-lsp server with a hermetic root.
/// * The route the chain would select (`RouteSelected`) is proven lawful by test
///   #3 above (same registry path `observe_diagnostics` uses at state.rs:165).
///
/// SEAM LANDED (GALL-CHECKPOINT-001C). The orchestrator added option (b):
/// `ServerState::analyze_and_observe(&self, uri, content)` — the Client-free core
/// `server::refresh_analyzer` now calls (the wrapper adds only
/// `Client::publish_diagnostics`). The test below drives that REAL orchestration,
/// so the live `ReceiptEmitted` chain is now observable from the on-disk OCEL log
/// without a `lsp_max::Client`. The proof exercises production code, not a
/// test-local reconstruction of it.
///
/// raise (analyze the template) → cross-surface repair (fix the SPARQL query) →
/// analyze the query → the now-lawful template is cleared through the SAME
/// `observe_diagnostics` the editor uses. Proven from the EXTERNAL on-disk OCEL
/// log — the unforgeable surface the anti-cheating doctrine requires.
#[tokio::test]
async fn analyze_and_observe_records_live_receipt_chain() {
    // ── Arrange: invalid project (query SELECTs ?name; template wants title).
    let tmp = tempfile::tempdir().expect("tempdir");
    let root = tmp.path();
    write_project(
        root,
        "SELECT ?name WHERE { ?s <https://schema.org/name> ?name }",
        r#"{{ row["title"] }}"#,
    );
    let state = ServerState::with_root(root);

    let tera_path = root.join("templates/item.tera");
    let tera_uri = url_from_path(&tera_path);
    let rq_path = root.join("queries/items.rq");
    let rq_uri = url_from_path(&rq_path);

    // ── Act 1 — RAISE: analyze the template through the real orchestration.
    let tera_src = std::fs::read_to_string(&tera_path).expect("read tera");
    let raised = state.analyze_and_observe(&tera_uri, &tera_src).await;
    assert!(
        raised
            .iter()
            .any(|(u, diags)| u == &tera_uri && diags.iter().any(|d| is_tpl_001(&d.lsp))),
        "analyze_and_observe must raise GGEN-TPL-001 on the template. published: {raised:?}"
    );

    // ── Act 2 — CROSS-SURFACE REPAIR: fix the QUERY (template untouched), then
    // analyze the query URI so the orchestration recomputes the project graph and
    // reconciles the now-lawful template's stale diagnostic.
    let repaired_rq = "SELECT ?name ?title WHERE { ?s <https://schema.org/name> ?name ; \
                       <https://schema.org/title> ?title }";
    std::fs::write(&rq_path, repaired_rq).expect("rewrite rq");
    let cleared = state.analyze_and_observe(&rq_uri, repaired_rq).await;
    assert!(
        cleared.iter().any(|(u, _)| u == &tera_uri),
        "the query-side repair must re-publish (clear) the template URI through the \
         orchestration. published: {cleared:?}"
    );

    // ── Assert: the EXTERNAL intel log proves the full live chain on the template.
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
            "live chain link {activity:?} for GGEN-TPL-001 on the template missing.\nlog:\n{}",
            lines.join("\n")
        );
    }

    // ── And: the live analysis path materialized no emitted artifact.
    assert!(
        !root.join("out.txt").exists(),
        "analyze_and_observe must never write the rule's output_file"
    );
}
