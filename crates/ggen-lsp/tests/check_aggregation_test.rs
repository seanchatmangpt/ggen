//! Aggregation-layer tests for the headless check gate
//! (`ggen_lsp::check::check_files_in_root`).
//!
//! Covers the AGGREGATION semantics, not per-code detection (the per-code
//! analyzers have their own suites): multiple analyzers feeding one report,
//! deterministic ordering, severity rollup (`error_count`/`warning_count` →
//! `has_errors`/`exit_code`), the empty-project case, and the malformed-.rq
//! case. Chicago style: real fixture projects on disk via `TempDir`, asserts on
//! the returned `CheckReport` state.

use std::fs;
use std::path::{Path, PathBuf};

use ggen_lsp::check::check_files_in_root;

const BASE_MANIFEST_TAIL: &str = r#"
[project]
name = "agg-demo"
version = "0.1.0"

[ontology]
source = "model.ttl"
"#;

/// Project triggering findings from BOTH analyzer families in one run:
/// - GGEN-TPL-001 (cross-surface fold, ERROR): template consumes a var the
///   rule's SELECT never binds;
/// - E0013 (sparql single-file analyzer, ERROR): a .rq SELECT missing ORDER BY;
/// - GGEN-QUERY-002 (cross-surface fold, WARNING): a rule whose query is
///   `SELECT *`.
fn write_multi_analyzer_project(root: &Path) -> Vec<PathBuf> {
    // Rule 1 binds ?name but the template consumes `title` → GGEN-TPL-001.
    // Rule 2 uses SELECT * → GGEN-QUERY-002 (and its query file exists so it
    // is not reclassified as GGEN-RULE-001).
    let manifest = format!(
        r#"{BASE_MANIFEST_TAIL}
[[generation.rules]]
name = "people"
output_file = "people.rs"
query = {{ inline = "SELECT ?name WHERE {{ ?p :name ?name }}" }}
template = {{ file = "row.tera" }}

[[generation.rules]]
name = "starred"
output_file = "starred.rs"
query = {{ file = "star.rq" }}
template = {{ inline = "static text" }}
"#
    );
    fs::write(root.join("ggen.toml"), manifest).expect("write manifest");
    fs::write(root.join("row.tera"), r#"{{ row["title"] }}"#).expect("write template");
    fs::write(root.join("star.rq"), "SELECT * WHERE { ?s ?p ?o }").expect("write star.rq");
    fs::write(root.join("unordered.rq"), "SELECT ?s WHERE { ?s ?p ?o }")
        .expect("write unordered.rq");

    vec![
        root.join("row.tera"),
        root.join("unordered.rq"),
        root.join("ggen.toml"),
    ]
}

fn codes_of(report: &ggen_lsp::check::CheckReport) -> Vec<String> {
    report
        .files
        .iter()
        .flat_map(|f| f.diagnostics.iter())
        .filter_map(|d| match &d.code {
            Some(lsp_max::lsp_types::NumberOrString::String(s)) => Some(s.clone()),
            Some(lsp_max::lsp_types::NumberOrString::Number(n)) => Some(n.to_string()),
            None => None,
        })
        .collect()
}

fn has_code(report: &ggen_lsp::check::CheckReport, code: &str) -> bool {
    codes_of(report).iter().any(|c| c == code)
}

fn code_string(d: &lsp_max::lsp_types::Diagnostic) -> String {
    match &d.code {
        Some(lsp_max::lsp_types::NumberOrString::String(s)) => s.clone(),
        Some(lsp_max::lsp_types::NumberOrString::Number(n)) => n.to_string(),
        None => String::new(),
    }
}

/// (1) Findings from BOTH the sparql single-file analyzer and the cross-surface
/// folds land in ONE aggregated report, counted by severity.
#[test]
fn multi_analyzer_project_aggregates_all_families() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    let paths = write_multi_analyzer_project(root);

    let report = check_files_in_root(root, &paths, false);

    // Tera cross-surface law (fold_tpl_001) — anchored on row.tera.
    assert!(
        has_code(&report, "GGEN-TPL-001"),
        "GGEN-TPL-001 must appear; got codes {:?}",
        codes_of(&report)
    );
    // SPARQL single-file law (sparql analyzer) — on unordered.rq.
    assert!(
        has_code(&report, "E0013"),
        "E0013 (SELECT without ORDER BY) must appear; got codes {:?}",
        codes_of(&report)
    );
    // SELECT * advisory (fold_query_002) — anchored on the manifest.
    assert!(
        has_code(&report, "GGEN-QUERY-002"),
        "GGEN-QUERY-002 must appear; got codes {:?}",
        codes_of(&report)
    );

    // Severity rollup is exact for these three pinned findings.
    assert_eq!(report.error_count, 2, "TPL-001 + E0013 are the ERRORs");
    assert_eq!(report.warning_count, 1, "QUERY-002 is the sole WARNING");
    assert!(report.has_errors());
    assert_eq!(report.exit_code(), 1);
}

/// (2) Ordering determinism: two identical runs produce byte-identical
/// aggregation (same file order, same diagnostic order per file). The gate's
/// documented ordering key is: input path order for the single-file pass, then
/// fold order (TPL → HARNESS → OUT → RULE → YIELD* → QUERY-002 → PACK → SRC)
/// appended per anchor — deterministic, no map iteration.
#[test]
fn two_runs_produce_identical_ordering() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    let paths = write_multi_analyzer_project(root);

    let a = check_files_in_root(root, &paths, false);
    let b = check_files_in_root(root, &paths, false);

    let fmt = |r: &ggen_lsp::check::CheckReport| {
        r.files
            .iter()
            .map(|f| {
                (
                    f.path.clone(),
                    f.diagnostics
                        .iter()
                        .map(|d| {
                            (
                                code_string(d),
                                format!("{}:{}", d.range.start.line, d.range.start.character),
                            )
                        })
                        .collect::<Vec<_>>(),
                )
            })
            .collect::<Vec<_>>()
    };
    assert_eq!(fmt(&a), fmt(&b), "aggregation order must be deterministic");
}

/// (3) Severity rollup: a warnings-only project (GGEN-QUERY-002 alone) passes
/// the gate; the same project with one ERROR fails it.
#[test]
fn severity_rollup_distinguishes_warning_only_from_error() {
    // Warnings-only: a single rule with SELECT *, template inline (no unbound
    // projection — inline templates are skipped by detect_tpl_001's
    // file-template path).
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    let manifest = format!(
        r#"{BASE_MANIFEST_TAIL}
[[generation.rules]]
name = "starred"
output_file = "starred.rs"
query = {{ file = "star.rq" }}
template = {{ inline = "static text" }}
"#
    );
    fs::write(root.join("ggen.toml"), manifest).expect("write manifest");
    fs::write(root.join("star.rq"), "SELECT * WHERE { ?s ?p ?o }").expect("write star.rq");

    let warn_only = check_files_in_root(root, &[root.join("ggen.toml")], false);
    assert!(
        has_code(&warn_only, "GGEN-QUERY-002"),
        "precondition: the SELECT * rule must raise the GGEN-QUERY-002 advisory; \
         got codes {:?}",
        codes_of(&warn_only)
    );

    // Error project: same shape but the template file consumes an unbound var.
    let dir2 = tempfile::TempDir::new().expect("tempdir");
    let root2 = dir2.path();
    let manifest2 = format!(
        r#"{BASE_MANIFEST_TAIL}
[[generation.rules]]
name = "people"
output_file = "people.rs"
query = {{ inline = "SELECT ?name WHERE {{ ?p :name ?name }}" }}
template = {{ file = "row.tera" }}
"#
    );
    fs::write(root2.join("ggen.toml"), manifest2).expect("write manifest");
    fs::write(root2.join("row.tera"), r#"{{ row["title"] }}"#).expect("write template");

    let with_error = check_files_in_root(root2, &[root2.join("row.tera")], false);

    // The rollup fields distinguish the two runs.
    assert_eq!(warn_only.error_count, 0, "warnings-only: no errors");
    assert!(
        warn_only.warning_count >= 1,
        "warnings-only: advisory counted"
    );
    assert_eq!(warn_only.exit_code(), 0, "warnings-only passes the gate");
    assert!(with_error.error_count >= 1);
    assert_eq!(with_error.exit_code(), 1);
}

/// (4) Empty project / empty path list → empty-but-successful report, no panic.
#[test]
fn empty_project_yields_empty_successful_report() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();

    let report = check_files_in_root(root, &[], false);

    assert!(report.files.is_empty());
    assert_eq!(report.error_count, 0);
    assert_eq!(report.warning_count, 0);
    assert!(!report.has_errors());
    assert_eq!(report.exit_code(), 0);
    assert!(report.route_summary.is_none());
}

/// (5) A malformed .rq (syntactically invalid SPARQL) is surfaced as an ERROR
/// diagnostic on that file — never silently dropped and never a panic.
#[test]
fn malformed_rq_is_surfaced_not_dropped() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    fs::write(root.join("broken.rq"), "SELEC ?x WHERE { ").expect("write broken.rq");

    let report = check_files_in_root(root, &[root.join("broken.rq")], false);

    let broken = report
        .files
        .iter()
        .find(|f| f.path.ends_with("broken.rq"))
        .expect("malformed .rq must still get a FileReport, not be dropped");
    assert!(
        !broken.diagnostics.is_empty(),
        "malformed .rq must produce at least one diagnostic"
    );
    assert!(
        broken
            .diagnostics
            .iter()
            .any(|d| d.severity == Some(lsp_max::lsp_types::DiagnosticSeverity::ERROR)),
        "malformed .rq must surface an ERROR; got {:?}",
        broken.diagnostics
    );
    assert!(report.has_errors());
    assert_eq!(report.exit_code(), 1);
}
