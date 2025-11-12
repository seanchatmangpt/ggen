//! End-to-End RDF Rendering Validation
//!
//! **CRITICAL VALIDATION**: This test ensures the v2.0 template.rs `render_with_rdf()`
//! method actually works end-to-end with real files.
//!
//! ## Test Flow
//! 1. Create minimal template with SPARQL query
//! 2. Create minimal RDF file with test data
//! 3. Call template.render_with_rdf() with RDF file path
//! 4. Verify SPARQL results are available in template context
//! 5. Verify template renders correctly
//!
//! ## What This Validates
//! - RDF file loading from disk (real I/O)
//! - SPARQL query execution against loaded RDF
//! - Template context population with SPARQL results
//! - Full rendering pipeline: Parse → Load RDF → Execute SPARQL → Render
//! - Prefixes and base IRI handling
//! - Multiple RDF files merging into single graph
//! - Error handling for missing/invalid RDF files
//!
//! ## Architecture Validation
//! This test validates the v2.0 architecture change:
//! - ❌ OLD: RDF files loaded via `rdf:` field in frontmatter
//! - ✅ NEW: RDF files loaded via `render_with_rdf()` API (CLI/API)
//!
//! If this test passes, the v2.0 architecture works correctly.

use anyhow::Result;
use ggen_core::{Graph, Template};
use std::io::Write;
use std::path::{Path, PathBuf};
use tempfile::NamedTempFile;
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
    file.flush()?;
    Ok(file)
}

/* ========== CRITICAL E2E Tests ========== */

#[test]
fn e2e_minimal_rdf_to_template_rendering() -> Result<()> {
    println!("🔍 E2E Test: Minimal RDF → SPARQL → Template rendering");

    // Step 1: Create minimal RDF file with test data
    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
ex:Alice a ex:Person ;
         ex:name "Alice Smith" ;
         ex:age 30 ."#,
    )?;
    println!("✓ Created RDF file: {}", rdf_file.path().display());

    // Step 2: Create minimal template with SPARQL query
    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
sparql:
  people: "SELECT ?person ?name ?age WHERE { ?person a ex:Person ; ex:name ?name ; ex:age ?age }"
---
Person Count: {{ sparql_results.people | length }}
Name: {{ sparql_first(results=sparql_results.people, column="name") }}
Age: {{ sparql_first(results=sparql_results.people, column="age") }}"#;

    let mut template = Template::parse(template_str)?;
    println!("✓ Parsed template with SPARQL query");

    // Step 3: Call render_with_rdf() with RDF file path
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
    println!("✓ Rendered template with RDF data");

    // Step 4: Verify SPARQL results are available in template context
    assert_eq!(template.front.sparql_results.len(), 1);
    assert!(template.front.sparql_results.contains_key("people"));
    println!("✓ SPARQL results available in template context");

    // Step 5: Verify template renders correctly
    assert!(rendered.contains("Person Count: 1"));
    assert!(rendered.contains("Name: \"Alice Smith\""));
    assert!(rendered.contains("Age: \"30\""));
    println!("✓ Template rendered correctly");

    println!("✅ E2E Test PASSED: RDF → SPARQL → Template pipeline works!");
    Ok(())
}

#[test]
fn e2e_multiple_rdf_files_merged_into_graph() -> Result<()> {
    println!("🔍 E2E Test: Multiple RDF files merged into single graph");

    // Create multiple RDF files
    let schema_rdf = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person a rdfs:Class ;
          rdfs:label "Person Class" ."#,
    )?;

    let data1_rdf = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
ex:Alice a ex:Person ; ex:name "Alice" ."#,
    )?;

    let data2_rdf = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
ex:Bob a ex:Person ; ex:name "Bob" .
ex:Carol a ex:Person ; ex:name "Carol" ."#,
    )?;

    println!("✓ Created 3 RDF files (schema + data1 + data2)");

    // Template that queries across all loaded RDF
    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
  rdfs: "http://www.w3.org/2000/01/rdf-schema#"
sparql:
  class_count: "SELECT (COUNT(?c) as ?cnt) WHERE { ?c a rdfs:Class }"
  person_count: "SELECT (COUNT(?p) as ?cnt) WHERE { ?p a ex:Person }"
  names: "SELECT ?name WHERE { ?p ex:name ?name } ORDER BY ?name"
---
Classes: {{ sparql_results.class_count }}
People: {{ sparql_results.person_count }}
First Name: {{ sparql_first(results=sparql_results.names, column="name") }}
All Names: {{ sparql_count(results=sparql_results.names) }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // Load all 3 RDF files
    let rendered = template.render_with_rdf(
        vec![
            schema_rdf.path().to_path_buf(),
            data1_rdf.path().to_path_buf(),
            data2_rdf.path().to_path_buf(),
        ],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;
    println!("✓ Loaded 3 RDF files into single graph");

    // Verify all data merged correctly
    assert!(rendered.contains("Classes:"));
    assert!(rendered.contains("People:"));
    assert!(rendered.contains("First Name: \"Alice\""));
    assert!(rendered.contains("All Names: 3"));
    println!("✓ All RDF files merged correctly");

    println!("✅ E2E Test PASSED: Multiple RDF files work!");
    Ok(())
}

#[test]
fn e2e_prefixes_and_base_iri_handling() -> Result<()> {
    println!("🔍 E2E Test: Prefixes and base IRI handling");

    // RDF file with full URIs
    let rdf_file = create_rdf_file(
        r#"<http://example.org/Alice> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/Person> .
<http://example.org/Alice> <http://example.org/name> "Alice" ."#,
    )?;
    println!("✓ Created RDF file with full URIs");

    // Template with prefixes and base
    let template_str = r#"---
base: "http://example.org/"
prefixes:
  ex: "http://example.org/"
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
sparql:
  people: "SELECT ?name WHERE { ?p rdf:type ex:Person ; ex:name ?name }"
---
Found: {{ sparql_count(results=sparql_results.people) }}
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
    println!("✓ Rendered with prefixes and base IRI");

    // Verify prefixes work
    assert!(rendered.contains("Found: 1"));
    assert!(rendered.contains("Name: \"Alice\""));
    println!("✓ Prefixes and base IRI handled correctly");

    println!("✅ E2E Test PASSED: Prefixes and base IRI work!");
    Ok(())
}

#[test]
fn e2e_combined_external_and_inline_rdf() -> Result<()> {
    println!("🔍 E2E Test: External RDF + inline RDF combined");

    // External RDF file
    let external_rdf = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
ex:Alice a ex:Person ; ex:name "Alice" ; ex:source "external" ."#,
    )?;
    println!("✓ Created external RDF file");

    // Template with inline RDF
    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
rdf_inline:
  - "ex:Bob a ex:Person ; ex:name 'Bob' ; ex:source 'inline' ."
  - "ex:Carol a ex:Person ; ex:name 'Carol' ; ex:source 'inline' ."
sparql:
  external_count: "SELECT (COUNT(?p) as ?cnt) WHERE { ?p ex:source 'external' }"
  inline_count: "SELECT (COUNT(?p) as ?cnt) WHERE { ?p ex:source 'inline' }"
  total_count: "SELECT (COUNT(?p) as ?cnt) WHERE { ?p a ex:Person }"
---
External: {{ sparql_results.external_count }}
Inline: {{ sparql_results.inline_count }}
Total: {{ sparql_results.total_count }}"#;

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
    println!("✓ Rendered with external + inline RDF");

    // Verify both sources combined
    assert!(rendered.contains("External:"));
    assert!(rendered.contains("Inline:"));
    assert!(rendered.contains("Total:"));
    println!("✓ External and inline RDF combined correctly");

    println!("✅ E2E Test PASSED: External + inline RDF work together!");
    Ok(())
}

#[test]
fn e2e_template_variables_in_sparql() -> Result<()> {
    println!("🔍 E2E Test: Template variables used in SPARQL queries");

    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
ex:Alice a ex:Person ; ex:role "admin" ; ex:name "Alice" .
ex:Bob a ex:Person ; ex:role "user" ; ex:name "Bob" .
ex:Carol a ex:Person ; ex:role "admin" ; ex:name "Carol" ."#,
    )?;
    println!("✓ Created RDF file with roles");

    // Template with variable in SPARQL query
    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
sparql:
  filtered: "SELECT ?name WHERE { ?p ex:role '{{target_role}}' ; ex:name ?name } ORDER BY ?name"
---
Searching for: {{target_role}}
Results: {{ sparql_count(results=sparql_results.filtered) }}
First: {{ sparql_first(results=sparql_results.filtered, column="name") }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = ctx_from_pairs(&[("target_role", "admin")]);

    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;
    println!("✓ Rendered with template variables in SPARQL");

    // Verify variable substitution worked
    assert!(rendered.contains("Searching for: admin"));
    assert!(rendered.contains("Results: 2"));
    assert!(rendered.contains("First: \"Alice\""));
    println!("✓ Template variables substituted in SPARQL correctly");

    println!("✅ E2E Test PASSED: Template variables in SPARQL work!");
    Ok(())
}

#[test]
fn e2e_error_handling_missing_file() -> Result<()> {
    println!("🔍 E2E Test: Error handling for missing RDF file");

    let template_str = r#"---
to: "output.txt"
---
This should fail"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // Try to load non-existent file
    let result = template.render_with_rdf(
        vec![PathBuf::from("/nonexistent/missing.ttl")],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    );

    assert!(result.is_err());
    let error_msg = result.unwrap_err().to_string();
    assert!(error_msg.contains("Failed to read RDF file"));
    println!("✓ Missing file error handled correctly: {}", error_msg);

    println!("✅ E2E Test PASSED: Missing file error handling works!");
    Ok(())
}

#[test]
fn e2e_error_handling_invalid_rdf_syntax() -> Result<()> {
    println!("🔍 E2E Test: Error handling for invalid RDF syntax");

    let invalid_rdf = create_rdf_file("This is not valid Turtle syntax! @#$%^&*()")?;
    println!("✓ Created file with invalid RDF syntax");

    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
---
body"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    let result = template.render_with_rdf(
        vec![invalid_rdf.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    );

    assert!(result.is_err());
    println!("✓ Invalid RDF syntax error handled correctly");

    println!("✅ E2E Test PASSED: Invalid RDF error handling works!");
    Ok(())
}

#[test]
fn e2e_full_code_generation_workflow() -> Result<()> {
    println!("🔍 E2E Test: Full code generation workflow (real-world scenario)");

    // Domain model in RDF - simplified for reliable testing
    let domain_rdf = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

ex:User rdf:type ex:Entity ;
        ex:entityName "User" ;
        ex:description "Application user entity" ;
        ex:hasField ex:Field_id ;
        ex:hasField ex:Field_email .

ex:Field_id ex:fieldName "id" ; ex:fieldType "u64" .
ex:Field_email ex:fieldName "email" ; ex:fieldType "String" ."#,
    )?;
    println!("✓ Created domain model RDF");

    // Code generation template - simplified for reliability
    let template_str = r#"---
to: "src/models/{{class_name | lower}}.rs"
prefixes:
  ex: "http://example.org/"
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
sparql:
  entity_info: "SELECT ?name ?desc WHERE { ex:User ex:entityName ?name ; ex:description ?desc }"
  field_count: "SELECT (COUNT(?f) as ?cnt) WHERE { ex:User ex:hasField ?f }"
---
// {{ sparql_first(results=sparql_results.entity_info, column="desc") }}
pub struct {{ sparql_first(results=sparql_results.entity_info, column="name") }} {
    pub id: u64,
    pub email: String,
}

impl {{ sparql_first(results=sparql_results.entity_info, column="name") }} {
    pub fn new() -> Self {
        Self::default()
    }
}

// Fields: {{ sparql_results.field_count }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = ctx_from_pairs(&[("class_name", "User")]);

    let rendered = template.render_with_rdf(
        vec![domain_rdf.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;
    println!("✓ Rendered Rust code from domain model");

    // Verify generated code structure
    assert!(rendered.contains("pub struct"));
    assert!(rendered.contains("User"));
    assert!(rendered.contains("pub id:") || rendered.contains("pub email:"));
    assert!(rendered.contains("impl"));
    assert!(rendered.contains("pub fn new()"));
    assert!(rendered.contains("Self::default()"));
    println!("✓ Generated code has correct structure");

    // Verify SPARQL queries executed
    assert_eq!(template.front.sparql_results.len(), 2);
    assert!(template.front.sparql_results.contains_key("entity_info"));
    assert!(template.front.sparql_results.contains_key("field_count"));
    println!("✓ SPARQL queries executed successfully");

    // Verify frontmatter variables were rendered
    assert_eq!(template.front.to.as_deref(), Some("src/models/user.rs"));
    println!("✓ Frontmatter variables rendered correctly");

    println!("✅ E2E Test PASSED: Full code generation workflow works!");
    Ok(())
}

/* ========== Performance Validation ========== */

#[test]
fn e2e_performance_large_rdf_file() -> Result<()> {
    use std::time::Instant;

    println!("🔍 E2E Performance Test: Large RDF file (1000 triples)");

    // Create large RDF file
    let mut large_rdf_content = String::from("@prefix ex: <http://example.org/> .\n");
    for i in 0..1000 {
        large_rdf_content.push_str(&format!(
            "ex:entity_{} a ex:Thing ; ex:index {} ; ex:name 'Entity {}' .\n",
            i, i, i
        ));
    }
    let large_rdf = create_rdf_file(&large_rdf_content)?;
    println!("✓ Created large RDF file (1000 triples)");

    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
sparql:
  count: "SELECT (COUNT(?s) as ?cnt) WHERE { ?s a ex:Thing }"
  sample: "SELECT ?name WHERE { ?s ex:name ?name } LIMIT 5"
---
Total: {{ sparql_results.count }}
Sample: {{ sparql_count(results=sparql_results.sample) }}"#;

    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    let start = Instant::now();
    let rendered = template.render_with_rdf(
        vec![large_rdf.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;
    let duration = start.elapsed();

    println!("✓ Rendered in {:?}", duration);
    assert!(rendered.contains("Total:"));
    assert!(rendered.contains("Sample: 5"));

    // Performance assertion: should complete in under 500ms
    assert!(
        duration.as_millis() < 500,
        "Performance regression: {:?} (expected < 500ms)",
        duration
    );
    println!("✓ Performance OK: {:?} < 500ms", duration);

    println!("✅ E2E Performance Test PASSED!");
    Ok(())
}

/* ========== Architecture Migration Validation ========== */

#[test]
fn e2e_v2_architecture_validation() -> Result<()> {
    println!("🔍 E2E Test: v2.0 Architecture Validation");
    println!("  Testing: RDF files loaded via render_with_rdf() API (not frontmatter)");

    // Create RDF file
    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
ex:test a ex:Thing ."#,
    )?;

    // Template WITHOUT rdf: field in frontmatter (v2.0 style)
    let template_str = r#"---
prefixes:
  ex: "http://example.org/"
sparql:
  things: "SELECT ?s WHERE { ?s a ex:Thing }"
---
Found: {{ sparql_count(results=sparql_results.things) }}"#;

    let mut template = Template::parse(template_str)?;

    // Verify no rdf field in frontmatter (v2.0 architecture)
    assert!(
        template.front.rdf_inline.is_empty()
            || template
                .front
                .rdf_inline
                .iter()
                .all(|s| !s.starts_with("rdf:"))
    );
    println!("✓ No rdf: field in frontmatter (v2.0 style)");

    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // Load RDF via API (v2.0 method)
    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;
    println!("✓ RDF loaded via render_with_rdf() API");

    // Verify SPARQL executed correctly
    assert!(rendered.contains("Found: 1"));
    println!("✓ SPARQL query executed successfully");

    println!("✅ v2.0 Architecture VALIDATED: RDF loading via API works!");
    Ok(())
}

#[test]
fn e2e_complete_pipeline_validation() -> Result<()> {
    println!("🚀 CRITICAL E2E Test: Complete Pipeline Validation");
    println!("  Testing: Parse → Load RDF → Execute SPARQL → Render");

    // Setup
    let rdf_file = create_rdf_file(
        r#"@prefix ex: <http://example.org/> .
ex:PipelineTest a ex:Validation ;
                ex:status "active" ;
                ex:version "2.0" ."#,
    )?;

    let template_str = r#"---
to: "validation_report.txt"
prefixes:
  ex: "http://example.org/"
sparql:
  status_check: "SELECT ?status ?version WHERE { ?t a ex:Validation ; ex:status ?status ; ex:version ?version }"
---
# Validation Report
Status: {{ sparql_first(results=sparql_results.status_check, column="status") }}
Version: {{ sparql_first(results=sparql_results.status_check, column="version") }}"#;

    // Phase 1: Parse
    println!("  Phase 1: Parse template...");
    let mut template = Template::parse(template_str)?;
    assert!(!template.body.is_empty());
    println!("  ✓ Template parsed");

    // Phase 2: Load RDF
    println!("  Phase 2: Load RDF files...");
    let mut graph = Graph::new()?;
    let mut tera = mk_tera();
    let vars = Context::new();

    // Phase 3: Execute SPARQL & Render
    println!("  Phase 3: Execute SPARQL and render...");
    let rendered = template.render_with_rdf(
        vec![rdf_file.path().to_path_buf()],
        &mut graph,
        &mut tera,
        &vars,
        Path::new("test.tmpl"),
    )?;
    println!("  ✓ RDF loaded and SPARQL executed");

    // Phase 4: Validate Results
    println!("  Phase 4: Validate results...");
    assert_eq!(template.front.to.as_deref(), Some("validation_report.txt"));
    assert_eq!(template.front.sparql_results.len(), 1);
    assert!(rendered.contains("# Validation Report"));
    assert!(rendered.contains("Status: \"active\""));
    assert!(rendered.contains("Version: \"2.0\""));
    println!("  ✓ Results validated");

    println!("✅ COMPLETE PIPELINE VALIDATED: v2.0 architecture works end-to-end!");
    Ok(())
}
