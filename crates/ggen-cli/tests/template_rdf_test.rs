//! Integration tests for template generation with RDF/SPARQL support
//!
//! These tests verify that `ggen template generate` correctly routes to
//! `render_with_rdf()` when SPARQL frontmatter is detected.
//!
//! Chicago TDD: Tests verify observable state changes (files created, metrics reported)

use ggen_domain::template::{render_with_rdf, RenderWithRdfOptions};
use std::fs;
use tempfile::TempDir;

/// Test that template generation with SPARQL frontmatter executes queries
///
/// **Verifies**: FR-001 (SPARQL Detection), FR-002 (Query Execution)
/// **Expected**: sparql_queries_executed > 0 when template has sparql: field
#[test]
fn test_template_generate_with_sparql_executes_queries() {
    // Arrange: Create temp directory and template with inline RDF + SPARQL
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let template_path = temp_dir.path().join("template.tmpl");
    let output_path = temp_dir.path().join("output.txt");

    // Template with inline RDF and SPARQL frontmatter
    // Use EXACT same format as domain test_render_with_inline_rdf
    let template_content = r#"---
to: "output.txt"
prefixes: { ex: "http://example.org/" }
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:Alice a ex:Person ."
sparql:
  people: "SELECT ?person WHERE { ?person a ex:Person }"
---
Found {{ sparql_results.people | length }} person(s)"#;

    fs::write(&template_path, template_content).expect("Failed to write template");

    // Act: Render template with RDF support
    // Construct options exactly like the domain test does
    let options = RenderWithRdfOptions::new(template_path.clone(), output_path.clone());

    let result = render_with_rdf(&options).expect("Failed to render template");

    // Assert: SPARQL queries were executed
    assert!(
        result.sparql_queries_executed > 0,
        "Expected sparql_queries_executed > 0, got {}. Template: {}, Output path: {}",
        result.sparql_queries_executed,
        template_path.display(),
        output_path.display()
    );

    // Assert: Output file was created
    assert!(output_path.exists(), "Output file should exist");

    // Assert: Output contains expected content (1 person)
    let output_content = fs::read_to_string(&output_path).expect("Failed to read output");
    assert!(
        output_content.contains("Found 1 person(s)"),
        "Output should contain 'Found 1 person(s)' but got: {}",
        output_content
    );
}

/// Test that template generation without RDF uses fast path
///
/// **Verifies**: FR-003 (Backward Compatibility)
/// **Expected**: Simple templates without RDF still work
#[test]
fn test_template_generate_without_rdf_uses_fast_path() {
    // Arrange: Create temp directory and simple template (no RDF)
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let template_path = temp_dir.path().join("simple.tmpl");
    let output_path = temp_dir.path().join("output.txt");

    // Simple template without SPARQL/RDF
    let template_content = r#"---
to: "output.txt"
---
Hello, {{ name | default(value="World") }}!
"#;

    fs::write(&template_path, template_content).expect("Failed to write template");

    // Act: Render template (should use fast path, no RDF loading)
    let options = RenderWithRdfOptions::new(template_path.clone(), output_path.clone())
        .with_var("name", "Test")
        .force();

    let result = render_with_rdf(&options).expect("Failed to render template");

    // Assert: No SPARQL queries executed (fast path)
    assert_eq!(
        result.sparql_queries_executed, 0,
        "Expected sparql_queries_executed = 0 for non-RDF template"
    );

    // Assert: Output file was created
    assert!(output_path.exists(), "Output file should exist");

    // Assert: Output contains expected content
    let output_content = fs::read_to_string(&output_path).expect("Failed to read output");
    assert!(
        output_content.contains("Hello, Test!"),
        "Output should contain rendered content: {}",
        output_content
    );
}

/// Test multi-file output with FILE markers
///
/// **Verifies**: FR-004 (Multi-file Output)
/// **Expected**: Templates with {# FILE: path #} markers create multiple files
///
/// Note: Currently render_with_rdf expects output_path to be a FILE, not a directory.
/// Multi-file output with directories requires using generate_tree or project generate.
/// This test is ignored until that flow is updated.
#[test]
#[ignore = "Multi-file with directory output requires generate_tree integration"]
fn test_template_generate_multifile_output() {
    // Arrange: Create temp directory and multi-file template
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let template_path = temp_dir.path().join("multifile.tmpl");
    let output_path = temp_dir.path().join("output");

    // Create output directory
    fs::create_dir_all(&output_path).expect("Failed to create output dir");

    // Multi-file template with inline RDF and SPARQL (matching domain test format)
    fs::write(
        &template_path,
        r#"---
prefixes: { ex: "http://example.org/" }
rdf_inline:
  - "@prefix ex: <http://example.org/> . ex:Item1 a ex:Item . ex:Item2 a ex:Item ."
sparql:
  items: "SELECT ?item WHERE { ?item a ex:Item }"
---
{# FILE: README.md #}
# Generated Project

Items: {{ sparql_results.items | length }}

{# FILE: lib.rs #}
//! Generated library

pub fn items_count() -> usize { {{ sparql_results.items | length }} }
"#,
    )
    .expect("Failed to write template");

    // Act: Render template with RDF support
    let options = RenderWithRdfOptions::new(template_path.clone(), output_path.clone()).force();

    let result = render_with_rdf(&options).expect("Failed to render multi-file template");

    // Assert: Multiple files created
    assert!(
        result.files_created >= 2,
        "Expected at least 2 files created, got {}",
        result.files_created
    );

    // Assert: SPARQL queries were executed
    assert!(
        result.sparql_queries_executed > 0,
        "Expected SPARQL queries executed for multi-file template"
    );

    // Assert: Files were actually created
    let readme = output_path.join("README.md");
    let lib_rs = output_path.join("lib.rs");
    assert!(readme.exists(), "README.md should exist");
    assert!(lib_rs.exists(), "lib.rs should exist");
}
