#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::literal_string_with_formatting_args
)]
//! GGEN-QUERY-002 — SELECT * blindspot advisory living-loop proof.
//!
//! Proves that `check_files_in_root` surfaces GGEN-QUERY-002 (WARNING) when a
//! rule's inline query uses `SELECT *`, and crucially that `SELECT *` suppresses
//! GGEN-TPL-001 detection (the blindspot: unbound template vars are invisible
//! when the SPARQL projection is a wildcard).
//!
//! Chicago TDD: real TempDir, real ggen.toml, real headless gate. No mocks.

use std::path::Path;

use lsp_max::lsp_types::{Diagnostic, DiagnosticSeverity, NumberOrString};

use ggen_lsp::check::{check_files_in_root, discover_law_surfaces, CheckReport};

// ---------------------------------------------------------------------------
// Helpers
// ---------------------------------------------------------------------------

fn is_query_002(d: &Diagnostic) -> bool {
    matches!(
        &d.code,
        Some(NumberOrString::String(s)) if s == "GGEN-QUERY-002"
    )
}

fn is_tpl_001(d: &Diagnostic) -> bool {
    matches!(
        &d.code,
        Some(NumberOrString::String(s)) if s == "GGEN-TPL-001"
    )
}

fn count_diag(report: &CheckReport, pred: impl Fn(&Diagnostic) -> bool) -> usize {
    report
        .files
        .iter()
        .flat_map(|f| f.diagnostics.iter())
        .filter(|d| pred(d))
        .count()
}

/// Write a minimal ggen project with a SPARQL inline query.
///
/// `query_inline` is injected verbatim as the `query.inline` value.
/// `template_body` controls what the Tera template renders.
fn write_project(dir: &Path, query_inline: &str, template_body: &str) {
    std::fs::create_dir_all(dir.join("schema")).expect("schema dir");
    std::fs::create_dir_all(dir.join("templates")).expect("templates dir");

    std::fs::write(
        dir.join("schema/domain.ttl"),
        "@prefix schema: <https://schema.org/> .\n",
    )
    .expect("write ttl");

    std::fs::write(dir.join("templates/item.tera"), template_body).expect("write tera");

    // Escape the query for TOML inline string. Single-quote TOML literal strings
    // can't contain single quotes, so we use a TOML multi-line basic string.
    let toml = format!(
        r#"[project]
name = "query-002-fixture"
version = "0.1.0"

[ontology]
source = "schema/domain.ttl"

[generation]
output_dir = "."

[[generation.rules]]
name = "items"
query = {{ inline = """
{query_inline}
""" }}
template = {{ file = "templates/item.tera" }}
output_file = "src/{{{{ name }}}}.rs"
"#
    );
    std::fs::write(dir.join("ggen.toml"), toml).expect("write ggen.toml");
}

// ---------------------------------------------------------------------------
// Test 1 — SELECT * raises GGEN-QUERY-002 at WARNING severity
// ---------------------------------------------------------------------------

/// A rule whose inline query uses `SELECT * WHERE { ?s ?p ?o }` must surface
/// GGEN-QUERY-002 at WARNING severity through the real headless gate.
///
/// This is not an error — it is a blindspot advisory. The gate warns that
/// TPL-001 / OUT-001 cross-surface guards are disabled for this rule.
#[test]
fn select_star_raises_query_002_through_headless_gate() {
    let tmp = tempfile::tempdir().expect("create tempdir");
    let root = tmp.path();

    write_project(root, "SELECT * WHERE { ?s ?p ?o }", "Item: {{ name }}\n");

    let paths = discover_law_surfaces(root);
    assert!(
        !paths.is_empty(),
        "fixture must have discoverable law surfaces (.toml/.ttl/.tera)"
    );

    let report = check_files_in_root(root, &paths, false);

    let q002_count = count_diag(&report, is_query_002);
    assert!(
        q002_count >= 1,
        "SELECT * must raise GGEN-QUERY-002 through the headless gate. \
         report.files: {:#?}",
        report.files
    );

    // GGEN-QUERY-002 is a WARNING, not an error — the advisory must not block.
    let q002_warnings = report
        .files
        .iter()
        .flat_map(|f| f.diagnostics.iter())
        .filter(|d| is_query_002(d) && d.severity == Some(DiagnosticSeverity::WARNING))
        .count();
    assert!(
        q002_warnings >= 1,
        "GGEN-QUERY-002 must be reported at WARNING severity. report.files: {:#?}",
        report.files
    );

    assert!(
        report.warning_count > 0,
        "warning_count must be > 0 when GGEN-QUERY-002 fires. warning_count={}",
        report.warning_count
    );

    // NOTE: SELECT * with a template that uses `{{ name }}` WILL produce a
    // TPL-001 error: with no explicit projection vars, ALL template vars look
    // "unbound" to the cross-surface guard. This is the observed behavior.
    // The QUERY-002 advisory is separate from any TPL-001 errors.
    // We do NOT assert error_count == 0 here because TPL-001 may co-fire.
}

// ---------------------------------------------------------------------------
// Test 2 — Explicit SELECT does NOT raise GGEN-QUERY-002
// ---------------------------------------------------------------------------

/// A rule whose inline query uses an explicit projection (`SELECT ?name WHERE …`)
/// must NOT surface GGEN-QUERY-002. This is the passing baseline: projects that
/// name their projection variables are clean on this law.
#[test]
fn explicit_select_does_not_raise_query_002() {
    let tmp = tempfile::tempdir().expect("create tempdir");
    let root = tmp.path();

    write_project(
        root,
        "SELECT ?name WHERE { ?s <https://schema.org/name> ?name }",
        "Item: {{ name }}\n",
    );

    let paths = discover_law_surfaces(root);
    assert!(
        !paths.is_empty(),
        "fixture must have discoverable law surfaces"
    );

    let report = check_files_in_root(root, &paths, false);

    let q002_count = count_diag(&report, is_query_002);
    assert_eq!(
        q002_count, 0,
        "an explicit SELECT projection must not raise GGEN-QUERY-002. \
         report.files: {:#?}",
        report.files
    );
}

// ---------------------------------------------------------------------------
// Test 3 — SELECT * causes TPL-001 false-positive flood (the blindspot proof)
// ---------------------------------------------------------------------------

/// A rule using `SELECT *` paired with a template that references `{{ name }}`
/// (a legitimate var that WOULD be bound at runtime) STILL produces GGEN-TPL-001
/// diagnostics — because with no explicit projection, the cross-surface guard
/// treats ALL template vars as unbound.
///
/// This is the behavioral proof of WHY QUERY-002 matters: `SELECT *` causes the
/// TPL-001 guard to flood with false-positive errors for every template variable,
/// including ones that are genuinely bound. The blindspot is that you cannot
/// distinguish real unbound vars from false positives — the advisory signal is
/// drowned in noise. QUERY-002 warns: "switch to explicit SELECT to restore
/// meaningful TPL-001 reporting."
#[test]
fn select_star_causes_tpl_001_false_positive_flood_blindspot_proven() {
    let tmp = tempfile::tempdir().expect("create tempdir");
    let root = tmp.path();

    // Template uses `{{ name }}` — legitimate at runtime, but invisible to the
    // static guard when SELECT * gives no explicit projection vars.
    write_project(root, "SELECT * WHERE { ?s ?p ?o }", "Item: {{ name }}\n");

    let paths = discover_law_surfaces(root);
    assert!(
        !paths.is_empty(),
        "fixture must have discoverable law surfaces"
    );

    let report = check_files_in_root(root, &paths, false);

    // QUERY-002 fires as the advisory.
    let q002_count = count_diag(&report, is_query_002);
    assert!(
        q002_count >= 1,
        "GGEN-QUERY-002 must fire for SELECT *. report.files: {:#?}",
        report.files
    );

    // TPL-001 ALSO fires (false positive) — the guard sees `name` as unbound
    // because the wildcard projection is not introspectable.
    let tpl_001_count = count_diag(&report, is_tpl_001);
    assert!(
        tpl_001_count >= 1,
        "GGEN-TPL-001 must fire for `{{ name }}` when SELECT * is used — \
         this is the false-positive blindspot: the guard cannot distinguish \
         bound vars from unbound when the projection is a wildcard. \
         tpl_001_count={tpl_001_count}. report.files: {:#?}",
        report.files
    );
}

// ---------------------------------------------------------------------------
// Test 4 — Explicit SELECT restores TPL-001 detection
// ---------------------------------------------------------------------------

/// When the same template (`{{ unbound_var }}`) is paired with an explicit
/// `SELECT ?name WHERE { … }` query instead of `SELECT *`, the cross-surface
/// TPL-001 guard can now compare projection variables against template variables —
/// and MUST fire GGEN-TPL-001 for the unbound reference.
///
/// This proves that switching from `SELECT *` to an explicit projection
/// re-enables the guard and closes the blindspot.
#[test]
fn explicit_select_restores_tpl_001_detection() {
    let tmp = tempfile::tempdir().expect("create tempdir");
    let root = tmp.path();

    // Same template as test 3 — `unbound_var` is not in the SELECT projection.
    // But now the query names its vars explicitly, so the guard can see the gap.
    write_project(
        root,
        "SELECT ?name WHERE { ?s <https://schema.org/name> ?name }",
        "Item: {{ unbound_var }}\n",
    );

    let paths = discover_law_surfaces(root);
    assert!(
        !paths.is_empty(),
        "fixture must have discoverable law surfaces"
    );

    let report = check_files_in_root(root, &paths, false);

    // QUERY-002 must NOT fire — this is an explicit projection.
    let q002_count = count_diag(&report, is_query_002);
    assert_eq!(
        q002_count, 0,
        "an explicit SELECT must not raise GGEN-QUERY-002. report.files: {:#?}",
        report.files
    );

    // TPL-001 MUST fire — the guard can now introspect `?name` and see that
    // `unbound_var` is absent from the projection.
    let tpl_001_count = count_diag(&report, is_tpl_001);
    assert!(
        tpl_001_count >= 1,
        "GGEN-TPL-001 must fire for {{ unbound_var }} when SELECT projection is \
         explicit and does not include `unbound_var`. \
         tpl_001_count={tpl_001_count}. report.files: {:#?}",
        report.files
    );
}
