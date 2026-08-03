//! Living-loop integration coverage for generated Rust module authority.

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

fn has_src_004(report: &ggen_lsp::CheckReport) -> bool {
    report.files.iter().flat_map(|file| &file.diagnostics).any(|diagnostic| {
        diagnostic.severity == Some(DiagnosticSeverity::ERROR)
            && matches!(
                &diagnostic.code,
                Some(NumberOrString::String(code)) if code == "GGEN-SRC-004"
            )
    })
}

#[test]
fn missing_generated_module_rule_fails_the_gate() {
    let temp = TempDir::new().expect("tempdir");
    write_project(temp.path(), false);
    let surfaces = discover_law_surfaces(temp.path());
    let report = check_files_in_root(temp.path(), &surfaces, false);

    assert!(has_src_004(&report));
    assert!(report.has_errors());
    assert!(report
        .files
        .iter()
        .any(|file| file.path.ends_with("src/lib.rs")));
}

#[test]
fn generated_child_rule_closes_src_004() {
    let temp = TempDir::new().expect("tempdir");
    write_project(temp.path(), true);
    let surfaces = discover_law_surfaces(temp.path());
    let report = check_files_in_root(temp.path(), &surfaces, false);

    assert!(!has_src_004(&report));
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

    assert!(!has_src_004(&report));
}
