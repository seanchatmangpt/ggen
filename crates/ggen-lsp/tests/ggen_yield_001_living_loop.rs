#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::literal_string_with_formatting_args
)]
//! GGEN-YIELD-001 — living LSP loop proof.
//!
//! Proves that `check_files_in_root` surfaces GGEN-YIELD-001 when a rule's
//! `output_file` resolves outside the project root after Tera-var stripping,
//! and that a lawful `output_file` produces no YIELD-001 diagnostic.
//!
//! Chicago TDD: real TempDir, real ggen.toml, real headless gate. No mocks.

use std::path::Path;

use lsp_max::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString};

use ggen_lsp::check::{check_files_in_root, discover_law_surfaces, CheckReport};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn is_yield_001(d: &Diagnostic) -> bool {
    matches!(
        &d.code,
        Some(NumberOrString::String(s)) if s == "GGEN-YIELD-001"
    )
}

fn count_yield_001(report: &CheckReport) -> usize {
    report
        .files
        .iter()
        .flat_map(|f| f.diagnostics.iter())
        .filter(|d| is_yield_001(d))
        .count()
}

fn has_yield_001_error(report: &CheckReport) -> bool {
    report
        .files
        .iter()
        .flat_map(|f| f.diagnostics.iter())
        .any(|d| is_yield_001(d) && d.severity == Some(DiagnosticSeverity::ERROR))
}

/// Write a minimal ggen project to `dir` with the given `output_file` value.
///
/// All other fields are constant across tests; only the `output_file` rule
/// attribute varies, which is exactly the law surface YIELD-001 guards.
fn write_project(dir: &Path, output_file: &str) {
    std::fs::create_dir_all(dir.join("schema")).expect("schema dir");
    std::fs::create_dir_all(dir.join("queries")).expect("queries dir");
    std::fs::create_dir_all(dir.join("templates")).expect("templates dir");

    std::fs::write(
        dir.join("schema/domain.ttl"),
        "@prefix schema: <https://schema.org/> .\n",
    )
    .expect("write ttl");

    std::fs::write(
        dir.join("queries/items.rq"),
        "SELECT ?name WHERE { ?s <https://schema.org/name> ?name }",
    )
    .expect("write rq");

    std::fs::write(dir.join("templates/item.tera"), "Item: {{ name }}\n").expect("write tera");

    let toml = format!(
        r#"[project]
name = "yield-001-fixture"
version = "0.1.0"

[ontology]
source = "schema/domain.ttl"

[generation]
output_dir = "."

[[generation.rules]]
name = "items"
query = {{ file = "queries/items.rq" }}
template = {{ file = "templates/item.tera" }}
output_file = "{output_file}"
"#
    );
    std::fs::write(dir.join("ggen.toml"), toml).expect("write ggen.toml");
}

// ---------------------------------------------------------------------------
// Test 1 — escaping output_file raises GGEN-YIELD-001 through the headless gate
// ---------------------------------------------------------------------------

/// A rule whose `output_file` uses `../../` path traversal (after Tera-var
/// stripping the path resolves outside the project root) must surface
/// GGEN-YIELD-001 at ERROR severity in the headless gate's report.
///
/// This exercises the PURE path (`yield_001_diagnostics`) through the REAL
/// wired headless gate (`check_files_in_root`) — not through unit-test calls.
#[test]
fn escaping_output_file_raises_yield_001_through_headless_gate() {
    let tmp = tempfile::tempdir().expect("create tempdir");
    let root = tmp.path();

    // `../../escaped/{{ name }}.rs` → after strip_tera_vars → `../../escaped/.rs`
    // → after lexical_clean relative to manifest dir → resolves outside `root`.
    write_project(root, "../../escaped/{{ name }}.rs");

    let paths = discover_law_surfaces(root);
    assert!(
        !paths.is_empty(),
        "fixture must have discoverable law surfaces (.toml/.ttl/.rq/.tera)"
    );

    let report = check_files_in_root(root, &paths, false);

    assert!(
        count_yield_001(&report) >= 1,
        "escaping output_file must raise GGEN-YIELD-001 through the headless gate. \
         report.files: {:#?}",
        report.files
    );
    assert!(
        has_yield_001_error(&report),
        "GGEN-YIELD-001 must be reported at ERROR severity. report.files: {:#?}",
        report.files
    );
    assert!(
        report.error_count >= 1,
        "error_count must be >= 1 when GGEN-YIELD-001 fires. error_count={}",
        report.error_count
    );
    assert!(
        report.has_errors(),
        "CheckReport::has_errors() must be true when GGEN-YIELD-001 fires"
    );
}

// ---------------------------------------------------------------------------
// Test 2 — lawful output_file inside the project produces no YIELD-001
// ---------------------------------------------------------------------------

/// A rule whose `output_file` is a safe in-project path (`src/{{ name }}.rs`)
/// must NOT raise GGEN-YIELD-001. This is the passing baseline: normal project
/// configurations are silent on this law.
#[test]
fn in_project_output_file_produces_no_yield_001() {
    let tmp = tempfile::tempdir().expect("create tempdir");
    let root = tmp.path();

    write_project(root, "src/{{ name }}.rs");

    let paths = discover_law_surfaces(root);
    assert!(
        !paths.is_empty(),
        "fixture must have discoverable law surfaces"
    );

    let report = check_files_in_root(root, &paths, false);

    assert_eq!(
        count_yield_001(&report),
        0,
        "a lawful in-project output_file must not raise GGEN-YIELD-001. \
         report.files: {:#?}",
        report.files
    );
}

// ---------------------------------------------------------------------------
// Test 3 — absolute path outside project raises GGEN-YIELD-001
// ---------------------------------------------------------------------------

/// An absolute `output_file` pointing outside the project root is also a
/// LAYER_VIOLATION.  `/tmp/hax/{{ name }}.rs` must fire YIELD-001.
#[test]
fn absolute_escaping_output_file_raises_yield_001() {
    let tmp = tempfile::tempdir().expect("create tempdir");
    let root = tmp.path();

    // Use an absolute path that is definitely not under `root`.
    write_project(root, "/tmp/hax/{{ name }}.rs");

    let paths = discover_law_surfaces(root);
    let report = check_files_in_root(root, &paths, false);

    assert!(
        count_yield_001(&report) >= 1,
        "an absolute out-of-root output_file must raise GGEN-YIELD-001. \
         report.files: {:#?}",
        report.files
    );
    assert!(
        has_yield_001_error(&report),
        "GGEN-YIELD-001 must be at ERROR severity for absolute out-of-root path"
    );
}

// ---------------------------------------------------------------------------
// Test 4 — gate never materializes the declared output_file during analysis
// ---------------------------------------------------------------------------

/// `check_files_in_root` must be read-only. Even when an escaping `output_file`
/// fires YIELD-001, the declared file must never be written to disk.
/// Writing output during analysis conflates refusal with execution.
#[test]
fn headless_gate_never_materializes_output_file() {
    let tmp = tempfile::tempdir().expect("create tempdir");
    let root = tmp.path();

    write_project(root, "src/{{ name }}.rs");

    // The declared output path (skeleton after var-strip, relative to root).
    let declared = root.join("src").join(".rs");

    let paths = discover_law_surfaces(root);
    let _ = check_files_in_root(root, &paths, false);

    assert!(
        !declared.exists(),
        "the headless gate must never write the rule's declared output_file. \
         Found unexpected file: {}",
        declared.display()
    );
}
