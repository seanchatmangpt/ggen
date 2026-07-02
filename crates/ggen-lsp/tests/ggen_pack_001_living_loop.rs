//! Living-loop integration tests for GGEN-PACK-001.
//!
//! BUG-011: When a rule's query or template is declared with `{ pack = "...",
//! output = "...", file = "..." }`, ggen-lsp cannot resolve the content at
//! author time. GGEN-TPL-001 and GGEN-OUT-001 checks are vacuous for that rule.
//!
//! These tests verify that `check_files_in_root()` surfaces a GGEN-PACK-001
//! WARNING on the ggen.toml when a pack-sourced query or template is declared,
//! and that non-pack sources (inline, file) do NOT trigger the advisory.

#![allow(clippy::expect_used)]

use ggen_lsp::check::{check_files_in_root, discover_law_surfaces};
use lsp_max::lsp_types;
use std::fs;
use tempfile::TempDir;

fn minimal_ontology(dir: &std::path::Path) {
    fs::write(
        dir.join("onto.ttl"),
        "@prefix owl: <http://www.w3.org/2002/07/owl#> .\n<urn:test> a owl:Ontology .\n",
    )
    .expect("write onto.ttl");
}

fn ggen_toml_with_pack_query(dir: &std::path::Path) {
    fs::write(
        dir.join("ggen.toml"),
        r#"[project]
name = "pack-query-fixture"
version = "0.1.0"

[ontology]
source = "onto.ttl"

[[generation.rules]]
name = "pack-rule"
query = { pack = "nonexistent-pack", output = "queries", file = "foo.rq" }
template = { inline = "{{ name }}" }
output_file = "out/{{ name }}.rs"
"#,
    )
    .expect("write ggen.toml");
}

fn ggen_toml_with_pack_template(dir: &std::path::Path) {
    fs::write(
        dir.join("ggen.toml"),
        r#"[project]
name = "pack-template-fixture"
version = "0.1.0"

[ontology]
source = "onto.ttl"

[[generation.rules]]
name = "pack-tpl-rule"
query = { inline = "SELECT ?name WHERE { ?name a <urn:test:Thing> }" }
template = { pack = "nonexistent-pack", output = "templates", file = "foo.tera" }
output_file = "out/{{ name }}.rs"
"#,
    )
    .expect("write ggen.toml");
}

fn ggen_toml_with_inline_query(dir: &std::path::Path) {
    fs::create_dir_all(dir.join("templates")).expect("create templates dir");
    fs::write(dir.join("templates").join("row.tera"), "{{ name }}\n").expect("write row.tera");
    fs::write(
        dir.join("ggen.toml"),
        r#"[project]
name = "inline-query-fixture"
version = "0.1.0"

[ontology]
source = "onto.ttl"

[[generation.rules]]
name = "inline-rule"
query = { inline = "SELECT ?name WHERE { ?name a <urn:test:Thing> }" }
template = { inline = "{{ name }}" }
output_file = "out/{{ name }}.rs"
"#,
    )
    .expect("write ggen.toml");
}

#[test]
fn pack_001_warns_when_query_is_pack_sourced() {
    // Arrange — pack-sourced query that cannot be resolved at author time.
    let dir = TempDir::new().expect("create temp dir");
    minimal_ontology(dir.path());
    ggen_toml_with_pack_query(dir.path());

    // Act
    let paths = discover_law_surfaces(dir.path());
    let report = check_files_in_root(dir.path(), &paths, false);

    // Assert — GGEN-PACK-001 WARNING present in the report.
    let all_codes: Vec<String> = report
        .files
        .iter()
        .flat_map(|f| &f.diagnostics)
        .filter_map(|d| match &d.code {
            Some(lsp_types::NumberOrString::String(s)) => Some(s.clone()),
            _ => None,
        })
        .collect();

    assert!(
        all_codes.iter().any(|c| c == "GGEN-PACK-001"),
        "expected GGEN-PACK-001 in diagnostics, got: {:?}",
        all_codes
    );
    // Advisory is WARNING severity — must not count as an error.
    // (error_count is only incremented for ERROR-severity diagnostics)
    // We cannot assert error_count == 0 because other rules may fire ERRORs,
    // but we can assert the warning_count is at least 1.
    assert!(
        report.warning_count >= 1,
        "expected at least 1 warning (GGEN-PACK-001), got warning_count={}",
        report.warning_count
    );
}

#[test]
fn pack_001_warns_when_template_is_pack_sourced() {
    // Arrange — pack-sourced template that cannot be resolved at author time.
    let dir = TempDir::new().expect("create temp dir");
    minimal_ontology(dir.path());
    ggen_toml_with_pack_template(dir.path());

    // Act
    let paths = discover_law_surfaces(dir.path());
    let report = check_files_in_root(dir.path(), &paths, false);

    // Assert
    let all_codes: Vec<String> = report
        .files
        .iter()
        .flat_map(|f| &f.diagnostics)
        .filter_map(|d| match &d.code {
            Some(lsp_types::NumberOrString::String(s)) => Some(s.clone()),
            _ => None,
        })
        .collect();

    assert!(
        all_codes.iter().any(|c| c == "GGEN-PACK-001"),
        "expected GGEN-PACK-001 for pack-sourced template, got: {:?}",
        all_codes
    );
}

#[test]
fn pack_001_not_raised_for_inline_query() {
    // Arrange — inline query/template, no pack source.
    let dir = TempDir::new().expect("create temp dir");
    minimal_ontology(dir.path());
    ggen_toml_with_inline_query(dir.path());

    // Act
    let paths = discover_law_surfaces(dir.path());
    let report = check_files_in_root(dir.path(), &paths, false);

    // Assert — GGEN-PACK-001 must NOT appear.
    let pack_diags: Vec<_> = report
        .files
        .iter()
        .flat_map(|f| &f.diagnostics)
        .filter(|d| matches!(&d.code, Some(lsp_types::NumberOrString::String(s)) if s == "GGEN-PACK-001"))
        .collect();

    assert!(
        pack_diags.is_empty(),
        "GGEN-PACK-001 must not fire for inline query/template, got: {:?}",
        pack_diags
    );
}

#[test]
fn pack_001_message_contains_rule_id_and_pack_info() {
    // Arrange
    let dir = TempDir::new().expect("create temp dir");
    minimal_ontology(dir.path());
    ggen_toml_with_pack_query(dir.path());

    // Act
    let paths = discover_law_surfaces(dir.path());
    let report = check_files_in_root(dir.path(), &paths, false);

    // Assert — diagnostic message body contains the pack source info.
    let pack_msgs: Vec<String> = report
        .files
        .iter()
        .flat_map(|f| &f.diagnostics)
        .filter(|d| matches!(&d.code, Some(lsp_types::NumberOrString::String(s)) if s == "GGEN-PACK-001"))
        .map(|d| d.message.clone())
        .collect();

    assert!(
        !pack_msgs.is_empty(),
        "expected at least one GGEN-PACK-001 diagnostic"
    );
    let msg = &pack_msgs[0];
    assert!(
        msg.contains("GGEN-PACK-001"),
        "message should contain code: {msg}"
    );
    assert!(
        msg.contains("pack source resolved at generation time"),
        "message should explain the advisory: {msg}"
    );
    assert!(
        msg.contains("GGEN-TPL-001") && msg.contains("GGEN-OUT-001"),
        "message should mention which checks are disabled: {msg}"
    );
}
