//! Chicago court for the five GGEN-* tera_analyzer law codes, run through the
//! REAL headless gate (`check_files_in_root`) against REAL fixture projects in
//! TempDirs (ggen.toml + templates + inline SPARQL). No mocks, no doubles:
//! every assertion reads real `CheckReport` output produced by the same
//! analyzers/folds the editor and pre-commit hook run.

use std::fs;
use std::path::Path;

use ggen_lsp::check::check_files_in_root;
use lsp_max::lsp_types::{DiagnosticSeverity, NumberOrString};

fn code(d: &lsp_max::lsp_types::Diagnostic) -> Option<String> {
    match &d.code {
        Some(NumberOrString::String(s)) => Some(s.clone()),
        _ => None,
    }
}

fn count_code(report: &ggen_lsp::check::CheckReport, target: &str) -> usize {
    report
        .files
        .iter()
        .flat_map(|f| &f.diagnostics)
        .filter(|d| code(d).as_deref() == Some(target))
        .count()
}

fn diags_with_code<'a>(
    report: &'a ggen_lsp::check::CheckReport, target: &str,
) -> Vec<&'a lsp_max::lsp_types::Diagnostic> {
    report
        .files
        .iter()
        .flat_map(|f| &f.diagnostics)
        .filter(|d| code(d).as_deref() == Some(target))
        .collect()
}

/// The manifest preamble every fixture uses (mirrors the existing check.rs
/// integration tests' known-good shape).
const MANIFEST_HEAD: &str = r#"
[project]
name = "demo"
version = "0.1.0"

[ontology]
source = "model.ttl"
"#;

fn write_manifest(root: &Path, body: &str) {
    fs::write(root.join("ggen.toml"), format!("{MANIFEST_HEAD}{body}")).expect("write manifest");
}

// ─── 1. GGEN-TPL-001: template consumes a var the SELECT does not produce ───

#[test]
fn tpl_001_detected_on_unbound_template_var() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    fs::write(root.join("row.tera"), "{{ row[\"title\"] }}").expect("write template");
    write_manifest(
        root,
        r#"
[[generation.rules]]
name = "people"
output_file = "people.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }
"#,
    );

    let report = check_files_in_root(
        root,
        &[root.join("row.tera"), root.join("ggen.toml")],
        false,
    );

    assert_eq!(count_code(&report, "GGEN-TPL-001"), 1, "{report:?}");
    let d = &diags_with_code(&report, "GGEN-TPL-001")[0];
    assert_eq!(d.severity, Some(DiagnosticSeverity::ERROR));
    assert!(d.message.contains("title"), "{}", d.message);
}

// ─── 2. GGEN-OUT-001: output_file consumes an unbound var ───────────────────

#[test]
fn out_001_detected_on_unbound_output_path_var() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    fs::write(root.join("row.tera"), "{{ name }}").expect("write template");
    write_manifest(
        root,
        r#"
[[generation.rules]]
name = "people"
output_file = "out/{{missing}}.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }
"#,
    );

    let report = check_files_in_root(
        root,
        &[root.join("row.tera"), root.join("ggen.toml")],
        false,
    );

    assert_eq!(count_code(&report, "GGEN-OUT-001"), 1, "{report:?}");
    let d = &diags_with_code(&report, "GGEN-OUT-001")[0];
    assert_eq!(d.severity, Some(DiagnosticSeverity::ERROR));
    assert!(d.message.contains("missing"), "{}", d.message);
    // Anchored on the ggen.toml declaration surface, never the emitted output.
    let manifest_report = report
        .files
        .iter()
        .find(|f| f.path.ends_with("ggen.toml"))
        .expect("manifest report");
    assert!(
        manifest_report
            .diagnostics
            .iter()
            .any(|d| code(d).as_deref() == Some("GGEN-OUT-001")),
        "OUT-001 must anchor on ggen.toml"
    );
}

// ─── 3. GGEN-YIELD-001 boundary: escapes fires, in-root clean ───────────────

#[test]
fn yield_001_fires_on_path_escape() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    fs::write(root.join("row.tera"), "{{ name }}").expect("write template");
    write_manifest(
        root,
        r#"
[[generation.rules]]
name = "escape"
output_file = "out/../../escape.txt"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }
"#,
    );

    let report = check_files_in_root(
        root,
        &[root.join("row.tera"), root.join("ggen.toml")],
        false,
    );

    assert_eq!(count_code(&report, "GGEN-YIELD-001"), 1, "{report:?}");
    let d = &diags_with_code(&report, "GGEN-YIELD-001")[0];
    assert_eq!(d.severity, Some(DiagnosticSeverity::ERROR));
    assert!(d.message.contains("escape.txt"), "{}", d.message);
}

#[test]
fn yield_001_clean_on_in_root_path() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    fs::write(root.join("row.tera"), "{{ name }}").expect("write template");
    write_manifest(
        root,
        r#"
[[generation.rules]]
name = "inside"
output_file = "out/x.txt"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }
"#,
    );

    let report = check_files_in_root(
        root,
        &[root.join("row.tera"), root.join("ggen.toml")],
        false,
    );

    assert_eq!(count_code(&report, "GGEN-YIELD-001"), 0, "{report:?}");
    assert!(!report.has_errors(), "in-root path must pass the gate");
}

// ─── 4. GGEN-RULE-001: {file=} binding points at a missing file ─────────────

#[test]
fn rule_001_detected_on_missing_template_file() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    write_manifest(
        root,
        r#"
[[generation.rules]]
name = "dangling"
output_file = "people.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "nope.tera" }
"#,
    );

    let report = check_files_in_root(root, &[root.join("ggen.toml")], false);

    assert_eq!(count_code(&report, "GGEN-RULE-001"), 1, "{report:?}");
    let d = &diags_with_code(&report, "GGEN-RULE-001")[0];
    assert_eq!(d.severity, Some(DiagnosticSeverity::ERROR));
    assert!(d.message.contains("template file missing"), "{}", d.message);
}

// ─── 5. GGEN-QUERY-002: SELECT * advisory ────────────────────────────────────

#[test]
fn query_002_detected_on_select_star() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    fs::write(root.join("row.tera"), "{{ name }}").expect("write template");
    write_manifest(
        root,
        r#"
[[generation.rules]]
name = "starred"
output_file = "people.rs"
query = { inline = "SELECT * WHERE { ?p :name ?name }" }
template = { file = "row.tera" }
"#,
    );

    let report = check_files_in_root(
        root,
        &[root.join("row.tera"), root.join("ggen.toml")],
        false,
    );

    assert_eq!(count_code(&report, "GGEN-QUERY-002"), 1, "{report:?}");
    let d = &diags_with_code(&report, "GGEN-QUERY-002")[0];
    assert_eq!(
        d.severity,
        Some(DiagnosticSeverity::WARNING),
        "QUERY-002 is the WARNING advisory"
    );
    assert!(d.message.contains("starred"), "{}", d.message);
    // Corrected semantics: with SELECT *, the projection set is unknowable, so
    // unboundness cannot be proven — TPL-001 must be SUPPRESSED for this rule
    // (QUERY-002 is the sole advisory), even though the template consumes `name`.
    assert_eq!(
        count_code(&report, "GGEN-TPL-001"),
        0,
        "SELECT * must suppress TPL-001 (unsound without provision knowledge): {report:?}"
    );
    assert_eq!(
        report.error_count, 0,
        "the SELECT * fixture is warnings-only: {report:?}"
    );
}

// ─── 6. Negative: a fully clean fixture reports zero diagnostics ────────────

#[test]
fn clean_fixture_reports_zero_diagnostics() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    fs::write(root.join("row.tera"), "Hello {{ name }}").expect("write template");
    write_manifest(
        root,
        r#"
[[generation.rules]]
name = "clean"
output_file = "out/x.txt"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }
"#,
    );

    let report = check_files_in_root(
        root,
        &[root.join("row.tera"), root.join("ggen.toml")],
        false,
    );

    assert_eq!(
        report.error_count, 0,
        "clean fixture must be error-free: {report:?}"
    );
    assert_eq!(report.warning_count, 0, "{report:?}");
    for f in &report.files {
        assert!(f.diagnostics.is_empty(), "{}: {:?}", f.path, f.diagnostics);
    }
    assert!(!report.has_errors());
}

// ─── 7. Severity pinning: QUERY-002 Warning, all four law codes Error ───────

#[test]
fn severity_pinning_warning_vs_error() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    // One fixture that carries all five codes at once:
    // - rule "starred": SELECT * → QUERY-002 (Warning)
    // - rule "bad": template consumes `title` not in SELECT → TPL-001 (Error)
    //   AND dangling template file binding is not needed here; use a second
    //   rule for RULE-001, a third for OUT-001, a fourth for YIELD-001.
    fs::write(root.join("row.tera"), "{{ row[\"title\"] }}").expect("write template");
    write_manifest(
        root,
        r#"
[[generation.rules]]
name = "starred"
output_file = "a.rs"
query = { inline = "SELECT * WHERE { ?p :name ?name }" }
template = { file = "row.tera" }

[[generation.rules]]
name = "badtpl"
output_file = "b.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }

[[generation.rules]]
name = "dangling"
output_file = "c.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "nope.tera" }

[[generation.rules]]
name = "badout"
output_file = "out/{{missing}}.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }

[[generation.rules]]
name = "escape"
output_file = "out/../../escape.txt"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "row.tera" }
"#,
    );

    let report = check_files_in_root(
        root,
        &[root.join("row.tera"), root.join("ggen.toml")],
        false,
    );

    assert_eq!(count_code(&report, "GGEN-QUERY-002"), 1, "{report:?}");
    for code_name in [
        "GGEN-TPL-001",
        "GGEN-OUT-001",
        "GGEN-RULE-001",
        "GGEN-YIELD-001",
    ] {
        let ds = diags_with_code(&report, code_name);
        assert!(!ds.is_empty(), "{code_name} missing: {report:?}");
        for d in &ds {
            assert_eq!(
                d.severity,
                Some(DiagnosticSeverity::ERROR),
                "{code_name} must be Error: {}",
                d.message
            );
        }
    }
    // Warning/Error tallies consistent with the pinned severities.
    assert_eq!(report.warning_count, 1, "{report:?}");
    assert!(report.error_count >= 4, "{report:?}");
}

// ─── 8. Combined fixture: two codes at once, correct distinct locations ─────

#[test]
fn combined_fixture_reports_two_codes_at_correct_locations() {
    let dir = tempfile::TempDir::new().expect("tempdir");
    let root = dir.path();
    // TPL-001 lives on the template surface; OUT-001 lives on the manifest.
    // Each code gets its own rule/template so the counts are exactly 1 (the
    // TPL-001 detector aggregates per template path across rules).
    fs::write(root.join("bad.tera"), "{{ row[\"title\"] }}").expect("write template");
    fs::write(root.join("ok.tera"), "{{ name }}").expect("write template");
    write_manifest(
        root,
        r#"
[[generation.rules]]
name = "badtpl"
output_file = "b.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "bad.tera" }

[[generation.rules]]
name = "badout"
output_file = "out/{{missing}}.rs"
query = { inline = "SELECT ?name WHERE { ?p :name ?name }" }
template = { file = "ok.tera" }
"#,
    );

    let report = check_files_in_root(
        root,
        &[
            root.join("bad.tera"),
            root.join("ok.tera"),
            root.join("ggen.toml"),
        ],
        false,
    );

    assert_eq!(count_code(&report, "GGEN-TPL-001"), 1, "{report:?}");
    assert_eq!(count_code(&report, "GGEN-OUT-001"), 1, "{report:?}");

    let tera = report
        .files
        .iter()
        .find(|f| f.path.ends_with("bad.tera"))
        .expect("template report");
    assert!(
        tera.diagnostics
            .iter()
            .any(|d| code(d).as_deref() == Some("GGEN-TPL-001")),
        "TPL-001 must be located on the template file: {tera:?}"
    );
    let manifest = report
        .files
        .iter()
        .find(|f| f.path.ends_with("ggen.toml"))
        .expect("manifest report");
    assert!(
        manifest
            .diagnostics
            .iter()
            .any(|d| code(d).as_deref() == Some("GGEN-OUT-001")),
        "OUT-001 must be located on the manifest: {manifest:?}"
    );
    // And the locations are genuinely distinct surfaces.
    assert!(!tera.path.ends_with("ggen.toml"));
    assert!(!manifest.path.ends_with("bad.tera"));
}
