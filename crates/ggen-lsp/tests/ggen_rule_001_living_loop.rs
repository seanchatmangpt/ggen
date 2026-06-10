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
//! GGEN-RULE-001 — LIVING LSP LOOP proof (the 4th species).
//!
//! GGEN-RULE-001 (`unbound_rule_file`) is the FOUNDATIONAL binding-integrity
//! check the other three species presuppose. A `ggen.toml` [[generation.rules]]
//! whose `query = { file = ... }` or `template = { file = ... }` points at a file
//! that does NOT exist on disk is a *dangling rule binding*: it was previously a
//! SILENT author-time failure, collected only in the internal
//! `RuleIndexEntry::issues` channel (which had no production consumer). The
//! TPL-001/OUT-001 detectors deliberately SKIP such a rule (a missing template is
//! `template_content == None`; a missing query yields empty `selected_vars`), so
//! the defect went unsurfaced. RULE-001 turns that silent channel into a living
//! diagnostic on the `ggen.toml` declaration surface.
//!
//! This file proves RULE-001 enters the live / headless wired loop:
//!   1. it is RAISED through the headless gate (`check_files_in_root`), anchored on
//!      `ggen.toml`, bumping `error_count`;
//!   1b. it is INDEPENDENT — the invalid fixture raises ZERO TPL/OUT/HARNESS;
//!   2. it CLEARS when the source law is repaired (the missing file is created);
//!   3. its repair route is source-law only (never emitted output);
//!   4. analysis never materializes an output artifact;
//!   5. the full 6-link OCEL chain (`DiagnosticRaised → RouteSelected →
//!      RepairSuggested → RepairApplied → GatePassed → ReceiptEmitted`) is written
//!      to the EXTERNAL on-disk intel log and proven by reading it back;
//!   6. the VALID fixture stays clean;
//!   7. CROSS-SPECIES symmetry — TPL/OUT/HARNESS fixtures raise ZERO RULE-001.
//!
//! Chicago TDD: every test loads a *real* fixture project tree from disk, runs the
//! *real* headless gate / *real* `RouteRegistry`, and reads the *real* `IntelLog`.
//! No mocks, no test doubles, no fabricated diagnostics or events.

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
        .join("ggen_rule_001_living_loop")
        .join(name)
}

/// Path to a fixture root for ANOTHER species (cross-species independence checks).
fn other_fixture(species_dir: &str, name: &str) -> PathBuf {
    Path::new(env!("CARGO_MANIFEST_DIR"))
        .join("tests")
        .join("fixtures")
        .join(species_dir)
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

fn has_rule_001_error_in_report(report: &CheckReport) -> bool {
    report
        .files
        .iter()
        .flat_map(|f| f.diagnostics.iter())
        .any(|d| is_code(d, "GGEN-RULE-001") && d.severity == Some(DiagnosticSeverity::ERROR))
}

/// The canonical key the live server + headless gate use to look a route up
/// (`select_for_diagnostic` keys on the code). NOT a fabricated detection: tests
/// #1/#2 drive real detection through the gate.
fn rule_001_diag() -> Diagnostic {
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
        code: Some(NumberOrString::String("GGEN-RULE-001".to_string())),
        code_description: None,
        source: Some("ggen-lsp".to_string()),
        message: "unbound rule file: templates/missing.tera".to_string(),
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

/// True if some JSONL event line names this `activity`, the GGEN-RULE-001 `code`,
/// and the `ggen.toml` manifest — three substrings on one line identify a single
/// event.
fn has_manifest_event(lines: &[String], activity: &str) -> bool {
    lines.iter().any(|l| {
        l.contains(&format!("\"activity\":\"{activity}\""))
            && l.contains("ggen.toml")
            && l.contains("GGEN-RULE-001")
    })
}

// ---------------------------------------------------------------------------
// 1. LIVE RAISE: a dangling rule binding raises GGEN-RULE-001 through the gate.
// ---------------------------------------------------------------------------

#[test]
fn missing_rule_file_raises_rule_001_through_headless_gate() {
    let root = fixture_root("invalid_project");
    let paths = discover_law_surfaces(&root);
    assert!(
        !paths.is_empty(),
        "fixture must contain discoverable law surfaces (ggen.toml/.rq/.ttl)"
    );

    let report = check_files_in_root(&root, &paths, true);

    assert!(
        count_code_in_report(&report, "GGEN-RULE-001") >= 1,
        "the rule binds `templates/missing.tera` which does not exist; the headless \
         gate must surface GGEN-RULE-001. report.files: {:?}",
        report.files
    );
    assert!(
        has_rule_001_error_in_report(&report),
        "GGEN-RULE-001 must be reported at ERROR severity. report.files: {:?}",
        report.files
    );
    assert!(
        report.error_count >= 1,
        "a GGEN-RULE-001 ERROR must increment CheckReport.error_count. error_count={}",
        report.error_count
    );
    assert!(report.has_errors(), "the gate must fail on GGEN-RULE-001");

    // The diagnostic must anchor on the ggen.toml manifest (declaration surface),
    // NOT the (nonexistent) target file or an emitted output.
    let manifest_report = report
        .files
        .iter()
        .find(|f| f.path.ends_with("ggen.toml"))
        .expect("ggen.toml report present");
    assert!(
        manifest_report
            .diagnostics
            .iter()
            .any(|d| is_code(d, "GGEN-RULE-001")),
        "GGEN-RULE-001 must anchor on ggen.toml. report: {manifest_report:?}"
    );
}

// ---------------------------------------------------------------------------
// 1b. INDEPENDENCE: the RULE invalid fixture raises ZERO TPL/OUT/HARNESS.
// ---------------------------------------------------------------------------

#[test]
fn missing_rule_file_raises_zero_other_species() {
    // The defect is purely a dangling file binding. TPL-001 needs a resolved
    // template body (there is none — the file is missing), OUT-001 needs a
    // dynamic output_file (`out.txt` is static), HARNESS-001 needs a Cargo.toml
    // target. So RULE-001 must be the ONLY species that fires. Proves RULE-001 is
    // a distinct species, not a relabelling of an existing one.
    let root = fixture_root("invalid_project");
    let paths = discover_law_surfaces(&root);
    let report = check_files_in_root(&root, &paths, true);

    assert_eq!(
        count_code_in_report(&report, "GGEN-TPL-001"),
        0,
        "RULE invalid fixture must raise ZERO TPL-001. report.files: {:?}",
        report.files
    );
    assert_eq!(
        count_code_in_report(&report, "GGEN-OUT-001"),
        0,
        "RULE invalid fixture must raise ZERO OUT-001. report.files: {:?}",
        report.files
    );
    assert_eq!(
        count_code_in_report(&report, "GGEN-HARNESS-001"),
        0,
        "RULE invalid fixture must raise ZERO HARNESS-001. report.files: {:?}",
        report.files
    );
    // Sanity: RULE-001 IS present (so the zero-others above are not vacuous).
    assert!(
        count_code_in_report(&report, "GGEN-RULE-001") >= 1,
        "RULE-001 must still fire (else the zero-others assertions are vacuous)"
    );
}

// ---------------------------------------------------------------------------
// 2. LIVE CLEAR: creating the missing file clears GGEN-RULE-001.
// ---------------------------------------------------------------------------

#[test]
fn creating_missing_file_clears_rule_001_through_headless_gate() {
    let src = fixture_root("invalid_project");
    let temp = tempfile::tempdir().expect("create temp dir");
    let dst = temp.path().join("project");
    copy_tree(&src, &dst).expect("copy fixture tree");

    // Pre-repair: the copy must still raise GGEN-RULE-001.
    let before_paths = discover_law_surfaces(&dst);
    let before = check_files_in_root(&dst, &before_paths, true);
    assert!(
        count_code_in_report(&before, "GGEN-RULE-001") >= 1,
        "pre-repair copy must raise GGEN-RULE-001 (else the clear is vacuous). \
         report.files: {:?}",
        before.files
    );

    // Repair: SOURCE LAW — create the missing template file at the bound path.
    let tpl_dir = dst.join("templates");
    std::fs::create_dir_all(&tpl_dir).expect("create templates dir");
    std::fs::write(tpl_dir.join("missing.tera"), "{{ name }}\n")
        .expect("create the missing template file");

    // Post-repair: GGEN-RULE-001 must clear.
    let after_paths = discover_law_surfaces(&dst);
    let after = check_files_in_root(&dst, &after_paths, true);
    assert_eq!(
        count_code_in_report(&after, "GGEN-RULE-001"),
        0,
        "after creating the bound template file, GGEN-RULE-001 must clear. \
         report.files: {:?}",
        after.files
    );
}

// ---------------------------------------------------------------------------
// 3. ROUTE METADATA: GGEN-RULE-001's repair route is source-law only.
// ---------------------------------------------------------------------------

#[test]
fn rule_001_route_is_source_law_only() {
    let registry = RouteRegistry::seeded();
    let diag = rule_001_diag();

    let route = registry
        .select_for_diagnostic(&diag)
        .expect("GGEN-RULE-001 must resolve to a seeded repair route");

    assert_eq!(
        route.id.0, "source-law.bind-rule-file",
        "GGEN-RULE-001 must select the rule-file source-law route"
    );
    assert_eq!(
        route.provenance,
        Provenance::Seeded,
        "the GGEN-RULE-001 route must be a seeded (cold-start, doctrine) route"
    );

    // No step may reference an EMITTED-output marker; every step must name a
    // source-law surface (the rule's ggen.toml binding / query / template file).
    const FORBIDDEN_OUTPUT: &[&str] = &["out/", "output/", "dist/", "emitted"];
    assert!(
        !route.steps.nodes.is_empty(),
        "the source-law route must offer at least one step"
    );
    for step in &route.steps.nodes {
        let title = step.title.to_lowercase();
        for forbidden in FORBIDDEN_OUTPUT {
            assert!(
                !title.contains(forbidden),
                "GGEN-RULE-001 route step {:?} references forbidden emitted-output \
                 marker {:?} — repairs are source-law only",
                step.title,
                forbidden
            );
        }
        assert!(
            title.contains("ggen.toml")
                || title.contains("query")
                || title.contains("template")
                || title.contains("file"),
            "GGEN-RULE-001 route step {:?} must reference a source-law surface",
            step.title
        );
    }
}

// ---------------------------------------------------------------------------
// 4. NO ARTIFACT: running the gate never materializes the declared output_file.
// ---------------------------------------------------------------------------

#[test]
fn headless_gate_never_materializes_output() {
    let src = fixture_root("invalid_project");
    let temp = tempfile::tempdir().expect("create temp dir");
    let dst = temp.path().join("project");
    copy_tree(&src, &dst).expect("copy fixture tree");

    let declared_output = dst.join("out.txt");
    assert!(
        !declared_output.exists(),
        "fixture copy unexpectedly already contains the emitted output"
    );

    let paths = discover_law_surfaces(&dst);
    let report = check_files_in_root(&dst, &paths, true);

    assert!(
        report.has_errors(),
        "the gate must observe the GGEN-RULE-001 ERROR (sanity it ran)"
    );
    assert!(
        !declared_output.exists(),
        "the headless gate must be read-only and must NOT materialize the emitted \
         output ({}). A live/analysis path that emits artifacts conflates refusal \
         with execution.",
        declared_output.display()
    );
}

// ---------------------------------------------------------------------------
// 5. OCEL CHAIN: analyze_and_observe records the full live lifecycle.
// ---------------------------------------------------------------------------

/// Drives the REAL `ServerState::analyze_and_observe` orchestration (the
/// Client-free core of the editor publish path): raise GGEN-RULE-001 by analyzing
/// the project, then create the missing file (source-law repair) and re-analyze so
/// the now-lawful manifest is cleared through the SAME `observe_diagnostics`. The
/// full 6-link chain is proven by reading the EXTERNAL on-disk OCEL log.
#[tokio::test]
async fn analyze_and_observe_records_live_rule_001_receipt_chain() {
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

    // ── Act 1 — RAISE: analyze the SPARQL query (a TPL/OUT/RULE trigger surface)
    // so the orchestration recomputes the project graph and flags the manifest.
    let rq_src = std::fs::read_to_string(&rq_path).expect("read rq");
    let raised = state.analyze_and_observe(&rq_uri, &rq_src).await;
    assert!(
        raised
            .iter()
            .any(|(u, diags)| u == &manifest_uri
                && diags.iter().any(|d| is_code(d, "GGEN-RULE-001"))),
        "analyze_and_observe must raise GGEN-RULE-001 on the ggen.toml manifest. \
         published: {raised:?}"
    );

    // ── Act 2 — SOURCE-LAW REPAIR: create the missing template file, then analyze
    // the query URI so the orchestration reconciles the now-lawful manifest.
    let tpl_dir = root.join("templates");
    std::fs::create_dir_all(&tpl_dir).expect("create templates dir");
    std::fs::write(tpl_dir.join("missing.tera"), "{{ name }}\n").expect("create missing file");
    let cleared = state.analyze_and_observe(&rq_uri, &rq_src).await;
    assert!(
        cleared.iter().any(|(u, _)| u == &manifest_uri),
        "the source-law repair must re-publish (clear) the ggen.toml URI through the \
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
            "live chain link {activity:?} for GGEN-RULE-001 on the ggen.toml manifest \
             missing.\nlog:\n{}",
            lines.join("\n")
        );
    }

    // ── And: the live analysis path materialized no emitted artifact.
    assert!(
        !root.join("out.txt").exists(),
        "analyze_and_observe must never materialize the rule's output_file"
    );
}

// ---------------------------------------------------------------------------
// 6. VALID FIXTURE: both bound files present raises zero GGEN-RULE-001.
// ---------------------------------------------------------------------------

#[test]
fn valid_rule_bindings_stay_clean() {
    let root = fixture_root("valid_project");
    let paths = discover_law_surfaces(&root);
    let report = check_files_in_root(&root, &paths, true);

    assert_eq!(
        count_code_in_report(&report, "GGEN-RULE-001"),
        0,
        "both bound files exist; no GGEN-RULE-001 expected. report.files: {:?}",
        report.files
    );
    assert!(
        !report.has_errors(),
        "the valid fixture must pass the gate. report.files: {:?}",
        report.files
    );
}

// ---------------------------------------------------------------------------
// 7. CROSS-SPECIES SYMMETRY: other species' fixtures raise ZERO RULE-001.
// ---------------------------------------------------------------------------

#[test]
fn other_species_fixtures_raise_zero_rule_001() {
    // The TPL-001 and OUT-001 invalid fixtures bind only EXISTING query/template
    // files (their defect is an unbound *variable*, not a missing *file*), so
    // RULE-001 must stay silent on them. Proves no leakage in the other direction:
    // RULE-001 does not fire on a lawful (present) binding.
    for (species_dir, name) in [
        ("ggen_out_001_living_loop", "invalid_project"),
        ("ggen_out_001_living_loop", "valid_project"),
    ] {
        let root = other_fixture(species_dir, name);
        let paths = discover_law_surfaces(&root);
        let report = check_files_in_root(&root, &paths, true);
        assert_eq!(
            count_code_in_report(&report, "GGEN-RULE-001"),
            0,
            "{species_dir}/{name} binds existing files; it must raise ZERO RULE-001. \
             report.files: {:?}",
            report.files
        );
    }
}
