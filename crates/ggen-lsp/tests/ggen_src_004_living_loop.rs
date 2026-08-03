//! Living-loop integration coverage for the ggen × ggen-legacy × lsp-max source law.

#![allow(clippy::expect_used)]

use std::fs;
use std::path::Path;

use ggen_lsp::check::{check_files_in_root, discover_law_surfaces};
use lsp_max::lsp_types::{DiagnosticSeverity, NumberOrString};
use tempfile::TempDir;

fn write_project(root: &Path, child_rule: bool) {
    fs::create_dir_all(root.join("src")).expect("create source directory");
    fs::write(root.join("src/lib.rs"), "pub mod capabilities;\n").expect("write root source");
    fs::write(root.join("src/capabilities.rs"), "pub struct Capability;\n")
        .expect("write child source");
    let child = if child_rule {
        r#"
[[generation.rules]]
name = "capabilities"
output_file = "src/capabilities.rs"
query = { inline = "SELECT ?name WHERE { ?s ?p ?name }" }
template = { inline = "pub struct Capability;" }
"#
    } else {
        ""
    };
    fs::write(
        root.join("ggen.toml"),
        format!(
            r#"[project]
name = "src-004"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "root"
output_file = "src/lib.rs"
query = {{ inline = "SELECT ?name WHERE {{ ?s ?p ?name }}" }}
template = {{ inline = "pub mod capabilities;" }}
{child}
"#
        ),
    )
    .expect("write manifest");
}

fn src_004_diagnostics(report: &ggen_lsp::CheckReport) -> Vec<&lsp_max::lsp_types::Diagnostic> {
    report
        .files
        .iter()
        .flat_map(|file| &file.diagnostics)
        .filter(|diagnostic| {
            diagnostic.severity == Some(DiagnosticSeverity::ERROR)
                && matches!(
                    &diagnostic.code,
                    Some(NumberOrString::String(code)) if code == "GGEN-SRC-004"
                )
        })
        .collect()
}

#[test]
fn missing_generated_module_rule_fails_with_bound_lineage() {
    let temp = TempDir::new().expect("tempdir");
    write_project(temp.path(), false);
    let surfaces = discover_law_surfaces(temp.path());
    let report = check_files_in_root(temp.path(), &surfaces, false);

    let diagnostics = src_004_diagnostics(&report);
    assert_eq!(diagnostics.len(), 1);
    assert!(report.has_errors());
    let provenance = ggen_lsp::legacy_contract::provenance(diagnostics[0])
        .expect("ggen-legacy/lsp-max provenance");
    assert_eq!(provenance["authority"]["product"], "ggen");
    assert_eq!(
        provenance["authority"]["legacy_evidence_repository"],
        "seanchatmangpt/ggen-legacy"
    );
    assert_eq!(provenance["runtime"]["package"], "lsp-max");
}

#[test]
fn generated_child_rule_closes_src_004() {
    let temp = TempDir::new().expect("tempdir");
    write_project(temp.path(), true);
    let surfaces = discover_law_surfaces(temp.path());
    let report = check_files_in_root(temp.path(), &surfaces, false);

    assert!(src_004_diagnostics(&report).is_empty());
}

#[test]
fn inline_module_needs_no_separate_generation_rule() {
    let temp = TempDir::new().expect("tempdir");
    write_project(temp.path(), false);
    fs::write(
        temp.path().join("src/lib.rs"),
        "mod capabilities { pub struct Capability; }\n",
    )
    .expect("write inline module");
    let surfaces = discover_law_surfaces(temp.path());
    let report = check_files_in_root(temp.path(), &surfaces, false);

    assert!(src_004_diagnostics(&report).is_empty());
}

#[test]
fn nested_inline_scope_resolves_owned_child() {
    let temp = TempDir::new().expect("tempdir");
    fs::create_dir_all(temp.path().join("src/api")).expect("create nested source");
    fs::write(
        temp.path().join("src/lib.rs"),
        "mod api { pub mod model; }\n",
    )
    .expect("write root source");
    fs::write(
        temp.path().join("src/api/model.rs"),
        "pub struct Model;\n",
    )
    .expect("write model source");
    fs::write(
        temp.path().join("ggen.toml"),
        r#"[project]
name = "nested-src-004"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "root"
output_file = "src/lib.rs"
query = { inline = "SELECT ?name WHERE { ?s ?p ?name }" }
template = { inline = "mod api { pub mod model; }" }

[[generation.rules]]
name = "model"
output_file = "src/api/model.rs"
query = { inline = "SELECT ?name WHERE { ?s ?p ?name }" }
template = { inline = "pub struct Model;" }
"#,
    )
    .expect("write manifest");

    let surfaces = discover_law_surfaces(temp.path());
    let report = check_files_in_root(temp.path(), &surfaces, false);
    assert!(src_004_diagnostics(&report).is_empty());
}
