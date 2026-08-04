//! Living-loop integration tests for GGEN-SRC-004.
//!
//! The exact same project-graph detector must be visible through the headless
//! checker and the editor protocol.

#![allow(clippy::expect_used)]

use ggen_lsp::check::{check_files_in_root, discover_law_surfaces};
use lsp_max::lsp_types::{DiagnosticSeverity, NumberOrString};
use std::fs;
use tempfile::TempDir;

fn write_project(root: &std::path::Path, lib_source: &str, include_child_rule: bool) {
    fs::create_dir_all(root.join("src")).expect("src");
    fs::write(
        root.join("model.ttl"),
        "@prefix ex: <urn:example:> .\nex:s ex:p ex:o .\n",
    )
    .expect("ontology");
    fs::write(root.join("src/lib.rs"), lib_source).expect("lib.rs");
    if include_child_rule {
        fs::write(root.join("src/capabilities.rs"), "pub struct Capabilities;\n")
            .expect("capabilities.rs");
    }

    let child_rule = if include_child_rule {
        r#"
[[generation.rules]]
name = "capabilities"
query = { inline = "SELECT ?name WHERE { ?name ?p ?o }" }
template = { inline = "{{ name }}" }
output_file = "src/capabilities.rs"
"#
    } else {
        ""
    };
    fs::write(
        root.join("ggen.toml"),
        format!(
            r#"[project]
name = "src-004-fixture"
version = "0.1.0"

[ontology]
source = "model.ttl"

[[generation.rules]]
name = "lib"
query = {{ inline = "SELECT ?name WHERE {{ ?name ?p ?o }}" }}
template = {{ inline = "{{{{ name }}}}" }}
output_file = "src/lib.rs"
{child_rule}"#
        ),
    )
    .expect("ggen.toml");
}

fn src_004_count(report: &ggen_lsp::CheckReport) -> usize {
    report
        .files
        .iter()
        .flat_map(|file| &file.diagnostics)
        .filter(|diagnostic| {
            diagnostic.severity == Some(DiagnosticSeverity::ERROR)
                && diagnostic.code
                    == Some(NumberOrString::String("GGEN-SRC-004".to_string()))
        })
        .count()
}

#[test]
fn headless_gate_refuses_unowned_generated_module() {
    let temp = TempDir::new().expect("tempdir");
    write_project(temp.path(), "pub mod capabilities;\n", false);

    let surfaces = discover_law_surfaces(temp.path());
    let report = check_files_in_root(temp.path(), &surfaces, true);

    assert_eq!(src_004_count(&report), 1, "report: {report:?}");
    assert!(report.has_errors());
    assert_eq!(report.exit_code(), 1);
}

#[test]
fn headless_gate_accepts_owned_generated_module() {
    let temp = TempDir::new().expect("tempdir");
    write_project(temp.path(), "pub mod capabilities;\n", true);

    let surfaces = discover_law_surfaces(temp.path());
    let report = check_files_in_root(temp.path(), &surfaces, false);

    assert_eq!(src_004_count(&report), 0, "report: {report:?}");
}

#[test]
fn headless_gate_ignores_inline_and_path_overridden_modules() {
    let temp = TempDir::new().expect("tempdir");
    write_project(
        temp.path(),
        r#"
pub mod inline { pub struct Present; }
#[path = "external.rs"]
pub mod external;
"#,
        false,
    );

    let surfaces = discover_law_surfaces(temp.path());
    let report = check_files_in_root(temp.path(), &surfaces, false);

    assert_eq!(src_004_count(&report), 0, "report: {report:?}");
}
