//! Living-loop integration tests for GGEN-YIELD-003, GGEN-YIELD-004, GGEN-YIELD-005.
//!
//! These three σ-violation diagnostics had pure detector functions in tera_analyzer.rs
//! but were never wired into `check_files_in_root()`. These tests verify the full
//! gate path: fixture → check_files_in_root → diagnostic present.

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

fn has_code(dir: &std::path::Path, code: &str) -> bool {
    let paths = discover_law_surfaces(dir);
    let report = check_files_in_root(dir, &paths, false);
    report
        .files
        .iter()
        .flat_map(|f| &f.diagnostics)
        .any(|d| matches!(&d.code, Some(lsp_types::NumberOrString::String(s)) if s == code))
}

// ── GGEN-YIELD-003 ────────────────────────────────────────────────────────────

#[test]
fn yield_003_fires_for_orphaned_output() {
    let dir = TempDir::new().expect("create temp dir");
    minimal_ontology(dir.path());
    fs::write(
        dir.path().join("ggen.toml"),
        r#"[project]
name = "yield-003-fixture"
version = "0.1.0"

[ontology]
source = "onto.ttl"

[[generation.rules]]
name = "orphan-rule"
query = { inline = "SELECT ?name WHERE { ?name a <urn:test:Thing> }" }
template = { inline = "{{ name }}" }
output_file = "{{ name }}"
"#,
    )
    .expect("write ggen.toml");

    assert!(
        has_code(dir.path(), "GGEN-YIELD-003"),
        "expected GGEN-YIELD-003 for output_file with no static base"
    );
}

#[test]
fn yield_003_not_raised_for_valid_output() {
    let dir = TempDir::new().expect("create temp dir");
    minimal_ontology(dir.path());
    fs::write(
        dir.path().join("ggen.toml"),
        r#"[project]
name = "yield-003-ok-fixture"
version = "0.1.0"

[ontology]
source = "onto.ttl"

[[generation.rules]]
name = "good-rule"
query = { inline = "SELECT ?name WHERE { ?name a <urn:test:Thing> }" }
template = { inline = "{{ name }}" }
output_file = "out/{{ name }}.rs"
"#,
    )
    .expect("write ggen.toml");

    assert!(
        !has_code(dir.path(), "GGEN-YIELD-003"),
        "GGEN-YIELD-003 must not fire for a valid output_file with static base"
    );
}

// ── GGEN-YIELD-004 ────────────────────────────────────────────────────────────

#[test]
fn yield_004_fires_for_competing_rules() {
    let dir = TempDir::new().expect("create temp dir");
    minimal_ontology(dir.path());
    fs::write(
        dir.path().join("ggen.toml"),
        r#"[project]
name = "yield-004-fixture"
version = "0.1.0"

[ontology]
source = "onto.ttl"

[[generation.rules]]
name = "rule-a"
query = { inline = "SELECT ?name WHERE { ?name a <urn:test:Thing> }" }
template = { inline = "{{ name }}" }
output_file = "out/shared.rs"

[[generation.rules]]
name = "rule-b"
query = { inline = "SELECT ?name WHERE { ?name a <urn:test:Thing> }" }
template = { inline = "{{ name }}" }
output_file = "out/shared.rs"
"#,
    )
    .expect("write ggen.toml");

    assert!(
        has_code(dir.path(), "GGEN-YIELD-004"),
        "expected GGEN-YIELD-004 when two rules target the same output_file"
    );
}

#[test]
fn yield_004_not_raised_for_unique_outputs() {
    let dir = TempDir::new().expect("create temp dir");
    minimal_ontology(dir.path());
    fs::write(
        dir.path().join("ggen.toml"),
        r#"[project]
name = "yield-004-ok-fixture"
version = "0.1.0"

[ontology]
source = "onto.ttl"

[[generation.rules]]
name = "rule-a"
query = { inline = "SELECT ?name WHERE { ?name a <urn:test:Thing> }" }
template = { inline = "{{ name }}" }
output_file = "out/a.rs"

[[generation.rules]]
name = "rule-b"
query = { inline = "SELECT ?name WHERE { ?name a <urn:test:Thing> }" }
template = { inline = "{{ name }}" }
output_file = "out/b.rs"
"#,
    )
    .expect("write ggen.toml");

    assert!(
        !has_code(dir.path(), "GGEN-YIELD-004"),
        "GGEN-YIELD-004 must not fire when all rules target distinct output files"
    );
}

// ── GGEN-YIELD-005 ────────────────────────────────────────────────────────────

#[test]
fn yield_005_fires_for_remote_url() {
    let dir = TempDir::new().expect("create temp dir");
    minimal_ontology(dir.path());
    fs::write(
        dir.path().join("ggen.toml"),
        r#"[project]
name = "yield-005-fixture"
version = "0.1.0"

[ontology]
source = "onto.ttl"

[[generation.rules]]
name = "remote-rule"
query = { inline = "SELECT ?name WHERE { ?name a <urn:test:Thing> }" }
template = { inline = "{{ name }}" }
output_file = "https://example.com/out.rs"
"#,
    )
    .expect("write ggen.toml");

    assert!(
        has_code(dir.path(), "GGEN-YIELD-005"),
        "expected GGEN-YIELD-005 for remote https:// output_file"
    );
}

#[test]
fn yield_005_not_raised_for_local_path() {
    let dir = TempDir::new().expect("create temp dir");
    minimal_ontology(dir.path());
    fs::write(
        dir.path().join("ggen.toml"),
        r#"[project]
name = "yield-005-ok-fixture"
version = "0.1.0"

[ontology]
source = "onto.ttl"

[[generation.rules]]
name = "local-rule"
query = { inline = "SELECT ?name WHERE { ?name a <urn:test:Thing> }" }
template = { inline = "{{ name }}" }
output_file = "out/local.rs"
"#,
    )
    .expect("write ggen.toml");

    assert!(
        !has_code(dir.path(), "GGEN-YIELD-005"),
        "GGEN-YIELD-005 must not fire for a local filesystem output_file"
    );
}
