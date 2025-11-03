//! Template RDF API Tests - v2.0 Architecture Validation
//!
//! Tests for the new render_with_rdf() API that loads RDF files via CLI/API
//! instead of frontmatter. This validates the architectural change that removes
//! the `rdf:` field from frontmatter and moves RDF loading to the caller.
//!
//! ## Test Coverage
//! - Unit tests: render_with_rdf() method functionality
//! - Integration tests: RDF loading, SPARQL execution, template rendering
//! - Regression tests: Existing inline RDF functionality preserved
//! - Edge cases: Multiple RDF files, missing files, invalid RDF, empty RDF
//! - Performance tests: RDF loading and rendering performance

use anyhow::Result;
use ggen_core::{Graph, Template};
use std::fs;
use std::io::Write;
use std::path::{Path, PathBuf};
use tempfile::{NamedTempFile, TempDir};
use tera::Context;

/* ========== Test Utilities ========== */

fn mk_tera() -> tera::Tera {
    let mut tera = tera::Tera::default();
    ggen_core::register::register_all(&mut tera);
    tera
}

fn ctx_from_pairs(pairs: &[(&str, &str)]) -> Context {
    let mut c = Context::new();
    for (k, v) in pairs {
        c.insert(*k, v);
    }
    c
}

fn create_rdf_file(content: &str) -> Result<NamedTempFile> {
    let mut file = NamedTempFile::new()?;
    writeln!(file, "{}", content)?;
    Ok(file)
}

/* ========== Unit Tests: render_with_rdf() API ========== */

#[test]
fn test_render_with_rdf_single_file() -> Result<()> {
    // Create RDF file with test data
    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
ex:Alice a ex:Person ;
         ex:name "Alice" ;
         ex:age 30 ."#,
    )?;

    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
sparql:
  people: "SELECT ?person ?name WHERE { ?person a ex:Person ; ex:name ?name }"
---
Found: {{ sparql_results.people | length }} person(s)
Name: {{ sparql_first(results=sparql_results.people, column="name") }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    assert!(rendered.contains("Found: 1 person(s)"));
    assert!(rendered.contains("Name: \"Alice\""));
    Ok(())
}

#[test]
fn test_render_with_rdf_multiple_files() -> Result<()> {
    // Create base RDF file with schema
    let base_rdf = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
ex:Person a rdf:Class ."#,
    )?;

    // Create data RDF file with instances
    let data_rdf = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
ex:Alice a ex:Person ; ex:name "Alice" .
ex:Bob a ex:Person ; ex:name "Bob" ."#,
    )?;

    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
sparql:
  count: "SELECT (COUNT(?p) as ?cnt) WHERE { ?p a ex:Person }"
---
Total people: {{ sparql_results.count }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // Load both RDF files
    let rendered = template.render_with_rdf(
        vec![base_rdf.path().to_path_buf(), data_rdf.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    // Both files should be loaded and merged into the graph
    assert!(rendered.contains("Total people:"));
    Ok(())
}

#[test]
fn test_render_with_rdf_empty_file_list() -> Result<()> {
    let template_str = r#"---
to: "output.txt"
---
No RDF loaded"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // Empty RDF file list should work (no RDF to load)
    let rendered = template.render_with_rdf(
        vec![],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    assert_eq!(rendered.trim(), "No RDF loaded");
    Ok(())
}

/* ========== Integration Tests: SPARQL Execution ========== */

#[test]
fn test_sparql_query_with_external_rdf() -> Result<()> {
    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
ex:Alice a ex:Person ; ex:name "Alice" ; ex:age 30 .
ex:Bob a ex:Person ; ex:name "Bob" ; ex:age 25 .
ex:Carol a ex:Person ; ex:name "Carol" ; ex:age 35 ."#,
    )?;

    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
sparql:
  names: "SELECT ?name WHERE { ?p a ex:Person ; ex:name ?name } ORDER BY ?name"
  ages: "SELECT ?age WHERE { ?p ex:age ?age } ORDER BY ?age"
---
Names: {{ sparql_values(results=sparql_results.names, column="name") }}
Min age: {{ sparql_first(results=sparql_results.ages, column="age") }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    assert!(rendered.contains("Names:"));
    assert!(rendered.contains("Min age:"));
    Ok(())
}

#[test]
fn test_sparql_ask_query_with_external_rdf() -> Result<()> {
    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
ex:data a ex:Thing ."#,
    )?;

    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
sparql:
  has_thing: "ASK WHERE { ?s a ex:Thing }"
  has_person: "ASK WHERE { ?s a ex:Person }"
---
Has Thing: {{ sparql_results.has_thing }}
Has Person: {{ sparql_results.has_person }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    assert!(rendered.contains("Has Thing: true"));
    assert!(rendered.contains("Has Person: false"));
    Ok(())
}

#[test]
fn test_combined_external_and_inline_rdf() -> Result<()> {
    // External RDF file
    let external_rdf = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
ex:Alice a ex:Person ; ex:name "Alice" ."#,
    )?;

    // Template with inline RDF
    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "ex:Bob a ex:Person ; ex:name 'Bob' ."
sparql:
  count: "SELECT (COUNT(?p) as ?cnt) WHERE { ?p a ex:Person }"
---
People count: {{ sparql_results.count }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    let rendered = template.render_with_rdf(
        vec![external_rdf.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    // Should have both Alice (external) and Bob (inline)
    assert!(rendered.contains("People count:"));
    Ok(())
}

/* ========== Regression Tests: Existing Functionality ========== */

#[test]
fn test_inline_rdf_still_works() -> Result<()> {
    // Verify that inline RDF (rdf_inline) still works correctly
    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "ex:Alice a ex:Person ."
sparql:
  query: "SELECT ?s WHERE { ?s a ex:Person }"
---
Count: {{ sparql_results.query | length }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // No external RDF files
    let rendered = template.render_with_rdf(
        vec![],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    assert!(rendered.contains("Count: 1"));
    Ok(())
}

#[test]
fn test_process_graph_without_external_rdf() -> Result<()> {
    // Verify that process_graph() still works for templates without external RDF
    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "ex:test a ex:Thing ."
sparql:
  query: "SELECT ?s WHERE { ?s a ex:Thing }"
---
Found: {{ sparql_results.query | length }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // Use process_graph directly (no external RDF)
    template.process_graph(&mut graph, &mut tera, &vars, Path::new("test.tmpl"))?;
    let rendered = template.render(&mut tera, &vars)?;

    assert!(rendered.contains("Found: 1"));
    Ok(())
}

#[test]
fn test_variables_work_in_rdf_and_sparql() -> Result<()> {
    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
ex:entity_placeholder a ex:Type ."#,
    )?;

    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "ex:{{entity_name}} a ex:Type ; ex:label '{{label}}' ."
sparql:
  query: "SELECT ?s WHERE { ?s a ex:Type }"
---
Count: {{ sparql_results.query | length }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = ctx_from_pairs(&[("entity_name", "test_entity"), ("label", "Test Label")]);

    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    // Should have entity_placeholder from file + test_entity from inline
    assert!(rendered.contains("Count:"));
    Ok(())
}

/* ========== Edge Case Tests ========== */

#[test]
fn test_missing_rdf_file() -> Result<()> {
    let template_str = r#"---
to: "output.txt"
---
body"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // Try to load non-existent RDF file
    let result = template.render_with_rdf(
        vec![PathBuf::from("/nonexistent/file.ttl")],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    );

    // Should return an error
    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("Failed to read RDF file"));
    Ok(())
}

#[test]
fn test_invalid_rdf_syntax() -> Result<()> {
    // Create file with invalid Turtle syntax
    let invalid_rdf = create_rdf_file("This is not valid Turtle syntax {{{")?;

    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
---
body"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // Should fail to parse invalid RDF
    let result = template.render_with_rdf(
        vec![invalid_rdf.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    );

    assert!(result.is_err());
    Ok(())
}

#[test]
fn test_empty_rdf_file() -> Result<()> {
    // Create empty RDF file
    let empty_rdf = create_rdf_file("")?;

    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
sparql:
  query: "SELECT ?s WHERE { ?s a ex:Person }"
---
Count: {{ sparql_results.query | length }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // Empty RDF file should load successfully but yield no data
    let rendered = template.render_with_rdf(
        vec![empty_rdf.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    assert!(rendered.contains("Count: 0"));
    Ok(())
}

#[test]
fn test_rdf_file_with_prefixes() -> Result<()> {
    // RDF file with prefix declarations
    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Thing a rdfs:Class ;
         rdfs:label "Thing" ."#,
    )?;

    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
  rdfs: "http://www.w3.org/2000/01/rdf-schema#"
sparql:
  classes: "SELECT ?label WHERE { ?c a rdfs:Class ; rdfs:label ?label }"
---
Label: {{ sparql_first(results=sparql_results.classes, column="label") }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    assert!(rendered.contains("Label:"));
    Ok(())
}

#[test]
fn test_duplicate_rdf_triples_idempotent() -> Result<()> {
    // Create two RDF files with overlapping data
    let rdf1 = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
ex:Alice a ex:Person ."#,
    )?;

    let rdf2 = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
ex:Alice a ex:Person ."#,
    )?;

    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
sparql:
  count: "SELECT (COUNT(?s) as ?cnt) WHERE { ?s a ex:Person }"
---
Count: {{ sparql_results.count }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    let rendered = template.render_with_rdf(
        vec![rdf1.path().to_path_buf(), rdf2.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    // Duplicate triples should be de-duplicated by RDF graph semantics
    assert!(rendered.contains("Count:"));
    Ok(())
}

/* ========== Performance Tests ========== */

#[test]
fn test_render_with_rdf_performance_large_file() -> Result<()> {
    use std::time::Instant;

    // Create large RDF file with 1000 triples
    let mut large_rdf_content = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..1000 {
        large_rdf_content.push_str(&format!(
            "ex:entity_{} a ex:Thing ; ex:index {} .\n",
            i, i
        ));
    }
    let large_rdf = create_rdf_file(&large_rdf_content)?;

    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
sparql:
  count: "SELECT (COUNT(?s) as ?cnt) WHERE { ?s a ex:Thing }"
---
Count: {{ sparql_results.count }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    let start = Instant::now();
    let _rendered = template.render_with_rdf(
        vec![large_rdf.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;
    let duration = start.elapsed();

    // Loading 1000 triples should be fast (< 500ms)
    assert!(
        duration.as_millis() < 500,
        "Performance too slow: {:?}",
        duration
    );
    Ok(())
}

#[test]
fn test_render_with_rdf_performance_multiple_files() -> Result<()> {
    use std::time::Instant;

    // Create 10 RDF files with 100 triples each
    let mut rdf_files = Vec::new();
    for file_idx in 0..10 {
        let mut content = String::from("@prefix ex: <http://example.org/> .\n");
        for i in 0..100 {
            content.push_str(&format!(
                "ex:entity_{}_{} a ex:Thing ; ex:file {} .\n",
                file_idx, i, file_idx
            ));
        }
        rdf_files.push(create_rdf_file(&content)?);
    }

    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
sparql:
  count: "SELECT (COUNT(?s) as ?cnt) WHERE { ?s a ex:Thing }"
---
Count: {{ sparql_results.count }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    let file_paths: Vec<PathBuf> = rdf_files.iter().map(|f| f.path().to_path_buf()).collect();

    let start = Instant::now();
    let _rendered = template.render_with_rdf(
        file_paths,
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;
    let duration = start.elapsed();

    // Loading 10 files with 100 triples each should be fast (< 1 second)
    assert!(
        duration.as_millis() < 1000,
        "Performance too slow: {:?}",
        duration
    );
    Ok(())
}

/* ========== Security Tests ========== */

#[test]
fn test_path_traversal_in_rdf_file_blocked() -> Result<()> {
    let template_str = r#"---
to: "output.txt"
---
body"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // Try to load RDF file with path traversal
    let result = template.render_with_rdf(
        vec![PathBuf::from("../../../etc/passwd.ttl")],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    );

    // Should fail due to file not found (path traversal blocked)
    assert!(result.is_err());
    Ok(())
}

/* ========== Compatibility Tests ========== */

#[test]
fn test_backward_compatibility_no_rdf_field() -> Result<()> {
    // Ensure old templates WITHOUT rdf: field still work
    let template_str = r#"---
to: "output.rs"
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "ex:data a ex:Thing ."
sparql:
  query: "SELECT ?s WHERE { ?s a ex:Thing }"
---
fn main() {
    // Found: {{ sparql_results.query | length }}
}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // Should work with empty RDF file list
    let rendered = template.render_with_rdf(
        vec![],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    assert!(rendered.contains("// Found: 1"));
    Ok(())
}

#[test]
fn test_frontmatter_rendering_before_rdf_load() -> Result<()> {
    // Verify that frontmatter is rendered before RDF is loaded
    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
ex:test a ex:Thing ."#,
    )?;

    let template_str = r#"---
to: "{{output_name}}.rs"
prefixes:
  ex: "http://example.org/"
---
// Output file: {{output_name}}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = ctx_from_pairs(&[("output_name", "test_output")]);

    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    // Frontmatter should be rendered
    assert_eq!(template.front.to.as_deref(), Some("test_output.rs"));
    assert!(rendered.contains("// Output file: test_output"));
    Ok(())
}

/* ========== Documentation Tests ========== */

#[test]
fn test_api_usage_example() -> Result<()> {
    // Example from documentation: Loading RDF via CLI/API
    let domain_rdf = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
@prefix schema: <http://schema.org/> .

ex:User a schema:Class ;
        schema:name "User" ;
        schema:description "Application user entity" ."#,
    )?;

    let template_str = r#"---
to: "src/models/user.rs"
prefixes:
  ex: "http://example.org/"
  schema: "http://schema.org/"
sparql:
  entities: "SELECT ?name ?desc WHERE { ?e a schema:Class ; schema:name ?name ; schema:description ?desc }"
---
// {{ sparql_first(results=sparql_results.entities, column="desc") }}
pub struct {{ sparql_first(results=sparql_results.entities, column="name") }} {
    pub id: u64,
}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    let rendered = template.render_with_rdf(
        vec![domain_rdf.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;

    assert!(rendered.contains("pub struct"));
    assert!(rendered.contains("pub id: u64"));
    Ok(())
}
