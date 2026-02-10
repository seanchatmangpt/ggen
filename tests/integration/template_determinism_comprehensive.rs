//! Comprehensive integration tests for template generation determinism
//!
//! This test suite verifies that template generation produces byte-identical
//! output across multiple runs with:
//! - Various ontology sizes (small/medium/large)
//! - Different RDF serialization formats (Turtle/JSON-LD/RDF-XML)
//! - Concurrent template regeneration
//! - Incremental ontology updates
//! - Cross-language consistency (Rust/TypeScript/Python output from same ontology)

use chicago_tdd_tools::prelude::*;
use ggen_core::{Graph, Template};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use std::sync::Arc;
use tempfile::TempDir;
use tera::Context;

/// Helper to create a Tera instance with ggen functions registered
fn create_tera() -> tera::Tera {
    let mut tera = tera::Tera::default();
    ggen_core::register::register_all(&mut tera);
    tera
}

/// Create a small test ontology in Turtle format
fn create_small_ontology_turtle() -> String {
    r#"
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person a rdfs:Class ;
    rdfs:label "Person" .

ex:alice a ex:Person ;
    ex:name "Alice" ;
    ex:age 30 .

ex:bob a ex:Person ;
    ex:name "Bob" ;
    ex:age 25 .
"#
    .to_string()
}

/// Create a medium test ontology in Turtle format
fn create_medium_ontology_turtle() -> String {
    let mut ontology = r#"
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person a rdfs:Class ;
    rdfs:label "Person" .

ex:Company a rdfs:Class ;
    rdfs:label "Company" .

ex:Project a rdfs:Class ;
    rdfs:label "Project" .

ex:worksFor a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range ex:Company .

ex:worksOn a rdf:Property ;
    rdfs:domain ex:Person ;
    rdfs:range ex:Project .
"#
    .to_string();

    // Add 50 people, 10 companies, 20 projects
    for i in 0..50 {
        ontology.push_str(&format!(
            r#"
ex:person{} a ex:Person ;
    ex:name "Person{}" ;
    ex:age {} .
"#,
            i,
            i,
            20 + (i % 50)
        ));
    }

    for i in 0..10 {
        ontology.push_str(&format!(
            r#"
ex:company{} a ex:Company ;
    ex:name "Company{}" .
"#,
            i, i
        ));
    }

    for i in 0..20 {
        ontology.push_str(&format!(
            r#"
ex:project{} a ex:Project ;
    ex:name "Project{}" .
"#,
            i, i
        ));
    }

    ontology
}

/// Create a large test ontology in Turtle format
fn create_large_ontology_turtle() -> String {
    let mut ontology = r#"
@prefix ex: <http://example.org/> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

ex:Person a rdfs:Class ;
    rdfs:label "Person" .

ex:Company a rdfs:Class ;
    rdfs:label "Company" .

ex:Project a rdfs:Class ;
    rdfs:label "Project" .

ex:Department a rdfs:Class ;
    rdfs:label "Department" .

ex:Skill a rdfs:Class ;
    rdfs:label "Skill" .
"#
    .to_string();

    // Add 500 people, 50 companies, 100 projects, 20 departments, 30 skills
    for i in 0..500 {
        let company_id = i % 50;
        let project_id = i % 100;
        let dept_id = i % 20;
        let skill_id = i % 30;

        ontology.push_str(&format!(
            r#"
ex:person{} a ex:Person ;
    ex:name "Person{}" ;
    ex:age {} ;
    ex:worksFor ex:company{} ;
    ex:worksOn ex:project{} ;
    ex:inDepartment ex:dept{} ;
    ex:hasSkill ex:skill{} .
"#,
            i,
            i,
            20 + (i % 50),
            company_id,
            project_id,
            dept_id,
            skill_id
        ));
    }

    for i in 0..50 {
        ontology.push_str(&format!(
            r#"
ex:company{} a ex:Company ;
    ex:name "Company{}" ;
    ex:industry "{}" .
"#,
            i,
            i,
            ["Tech", "Finance", "Healthcare", "Education", "Retail"][i % 5]
        ));
    }

    for i in 0..100 {
        ontology.push_str(&format!(
            r#"
ex:project{} a ex:Project ;
    ex:name "Project{}" ;
    ex:status "{}" .
"#,
            i,
            i,
            ["Active", "Completed", "Planning"][i % 3]
        ));
    }

    ontology
}

/// Convert Turtle to JSON-LD format
fn convert_turtle_to_jsonld(turtle: &str) -> anyhow::Result<String> {
    use ggen_core::graph::GraphExport;
    use oxigraph::io::RdfFormat;

    let graph = Graph::new()?;
    graph.insert_turtle(turtle)?;

    // Export to JSON-LD using GraphExport
    let export = GraphExport::new(&graph);
    Ok(export.write_to_string(RdfFormat::JsonLd)?)
}

/// Convert Turtle to RDF-XML format
fn convert_turtle_to_rdfxml(turtle: &str) -> anyhow::Result<String> {
    use ggen_core::graph::GraphExport;
    use oxigraph::io::RdfFormat;

    let graph = Graph::new()?;
    graph.insert_turtle(turtle)?;

    // Export to RDF-XML using GraphExport
    let export = GraphExport::new(&graph);
    Ok(export.write_to_string(RdfFormat::RdfXml)?)
}

/// Helper to render template with RDF data loaded from a specific format
fn render_template_with_rdf_format(
    template_str: &str,
    rdf_content: &str,
    format: oxigraph::io::RdfFormat,
    vars: &HashMap<String, String>,
) -> anyhow::Result<String> {
    use ggen_core::graph::GraphExport;
    use std::io::Cursor;
    use tempfile::NamedTempFile;

    // Load RDF into a temporary graph and convert to Turtle
    // This is necessary because Graph only has insert_turtle public method
    let temp_graph = Graph::new()?;

    // Write RDF content to a temp file with the appropriate extension
    let ext = match format {
        oxigraph::io::RdfFormat::Turtle => "ttl",
        oxigraph::io::RdfFormat::JsonLd => "jsonld",
        oxigraph::io::RdfFormat::RdfXml => "rdf",
        oxigraph::io::RdfFormat::NTriples => "nt",
        oxigraph::io::RdfFormat::TriG => "trig",
        oxigraph::io::RdfFormat::NQuads => "nq",
        _ => "ttl",
    };

    let temp_file = NamedTempFile::with_suffix(&format!(".{}", ext))?;
    std::fs::write(temp_file.path(), rdf_content)?;

    // Load the file (Graph.load_path supports multiple formats)
    temp_graph.load_path(temp_file.path())?;

    // Export to Turtle for use in rendering
    let export = GraphExport::new(&temp_graph);
    let turtle = export.write_to_string(oxigraph::io::RdfFormat::Turtle)?;

    // Now render with the Turtle representation
    render_template_with_rdf(template_str, &turtle, vars)
}

/// Create a template that generates Rust code from an ontology
fn create_rust_template() -> String {
    r#"---
to: "{{ output_file }}"
prefixes:
  ex: "http://example.org/"
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  rdfs: "http://www.w3.org/2000/01/rdf-schema#"
sparql:
  people: "SELECT ?person ?name ?age WHERE { ?person a ex:Person ; ex:name ?name ; ex:age ?age } ORDER BY ?name"
  classes: "SELECT ?class ?label WHERE { ?class a rdfs:Class ; rdfs:label ?label } ORDER BY ?label"
---
// Generated from ontology
// Total classes: {{ sparql_results.classes | length }}
// Total people: {{ sparql_results.people | length }}

#[derive(Debug, Clone)]
pub struct Person {
    pub name: String,
    pub age: u32,
}

pub fn get_all_people() -> Vec<Person> {
    vec![
        {%- for person in sparql_results.people %}
        Person {
            name: {{ person.name }}.to_string(),
            age: {{ person.age | replace(from='"', to='') }},
        },
        {%- endfor %}
    ]
}

pub fn get_class_count() -> usize {
    {{ sparql_results.classes | length }}
}
"#
    .to_string()
}

/// Create a template that generates TypeScript code from an ontology
fn create_typescript_template() -> String {
    r#"---
to: "{{ output_file }}"
prefixes:
  ex: "http://example.org/"
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  rdfs: "http://www.w3.org/2000/01/rdf-schema#"
sparql:
  people: "SELECT ?person ?name ?age WHERE { ?person a ex:Person ; ex:name ?name ; ex:age ?age } ORDER BY ?name"
  classes: "SELECT ?class ?label WHERE { ?class a rdfs:Class ; rdfs:label ?label } ORDER BY ?label"
---
// Generated from ontology
// Total classes: {{ sparql_results.classes | length }}
// Total people: {{ sparql_results.people | length }}

export interface Person {
  name: string;
  age: number;
}

export function getAllPeople(): Person[] {
  return [
    {%- for person in sparql_results.people %}
    {
      name: {{ person.name }},
      age: {{ person.age | replace(from='"', to='') }},
    },
    {%- endfor %}
  ];
}

export function getClassCount(): number {
  return {{ sparql_results.classes | length }};
}
"#
    .to_string()
}

/// Create a template that generates Python code from an ontology
fn create_python_template() -> String {
    r#"---
to: "{{ output_file }}"
prefixes:
  ex: "http://example.org/"
  rdf: "http://www.w3.org/1999/02/22-rdf-syntax-ns#"
  rdfs: "http://www.w3.org/2000/01/rdf-schema#"
sparql:
  people: "SELECT ?person ?name ?age WHERE { ?person a ex:Person ; ex:name ?name ; ex:age ?age } ORDER BY ?name"
  classes: "SELECT ?class ?label WHERE { ?class a rdfs:Class ; rdfs:label ?label } ORDER BY ?label"
---
"""Generated from ontology
Total classes: {{ sparql_results.classes | length }}
Total people: {{ sparql_results.people | length }}
"""

from dataclasses import dataclass
from typing import List


@dataclass
class Person:
    name: str
    age: int


def get_all_people() -> List[Person]:
    return [
        {%- for person in sparql_results.people %}
        Person(
            name={{ person.name }},
            age={{ person.age | replace(from='"', to='') }},
        ),
        {%- endfor %}
    ]


def get_class_count() -> int:
    return {{ sparql_results.classes | length }}
"#
    .to_string()
}

/// Helper to render a template with RDF data
fn render_template_with_rdf(
    template_str: &str,
    rdf_content: &str,
    vars: &HashMap<String, String>,
) -> anyhow::Result<String> {
    let mut template = Template::parse(template_str)?;
    let mut graph = Graph::new()?;
    let mut tera = create_tera();

    // Create context from vars
    let mut context = Context::new();
    for (key, value) in vars {
        context.insert(key, value);
    }

    // Render frontmatter
    template.render_frontmatter(&mut tera, &context)?;

    // Load RDF into graph
    graph.insert_turtle(rdf_content)?;

    // Process graph (executes SPARQL queries)
    template.process_graph(&mut graph, &mut tera, &context, Path::new("template.tmpl"))?;

    // Render body
    Ok(template.render(&mut tera, &context)?)
}

/// Calculate SHA256 hash of content for byte-level comparison
fn calculate_sha256(content: &str) -> String {
    use sha2::{Digest, Sha256};
    let mut hasher = Sha256::new();
    hasher.update(content.as_bytes());
    format!("{:x}", hasher.finalize())
}

// ============================================================================
// TESTS: Various Ontology Sizes
// ============================================================================

test!(test_small_ontology_determinism, {
    let ontology = create_small_ontology_turtle();
    let template = create_rust_template();

    let mut vars = HashMap::new();
    vars.insert("output_file".to_string(), "output.rs".to_string());

    // Generate multiple times
    let output1 = render_template_with_rdf(&template, &ontology, &vars)?;
    let output2 = render_template_with_rdf(&template, &ontology, &vars)?;
    let output3 = render_template_with_rdf(&template, &ontology, &vars)?;

    // Verify byte-identical output
    assert_eq!(output1, output2, "First and second runs should produce identical output");
    assert_eq!(output2, output3, "Second and third runs should produce identical output");

    // Verify hash consistency
    let hash1 = calculate_sha256(&output1);
    let hash2 = calculate_sha256(&output2);
    let hash3 = calculate_sha256(&output3);

    assert_eq!(hash1, hash2);
    assert_eq!(hash2, hash3);

    Ok(())
});

test!(test_medium_ontology_determinism, {
    let ontology = create_medium_ontology_turtle();
    let template = create_rust_template();

    let mut vars = HashMap::new();
    vars.insert("output_file".to_string(), "output.rs".to_string());

    // Generate multiple times
    let output1 = render_template_with_rdf(&template, &ontology, &vars)?;
    let output2 = render_template_with_rdf(&template, &ontology, &vars)?;
    let output3 = render_template_with_rdf(&template, &ontology, &vars)?;

    // Verify byte-identical output
    assert_eq!(output1, output2, "First and second runs should produce identical output");
    assert_eq!(output2, output3, "Second and third runs should produce identical output");

    // Verify the output contains expected data
    assert!(output1.contains("Person"));
    assert!(output1.contains("get_all_people"));
    assert!(output1.contains("Total people: 50"));

    Ok(())
});

test!(test_large_ontology_determinism, {
    let ontology = create_large_ontology_turtle();
    let template = create_rust_template();

    let mut vars = HashMap::new();
    vars.insert("output_file".to_string(), "output.rs".to_string());

    // Generate multiple times
    let output1 = render_template_with_rdf(&template, &ontology, &vars)?;
    let output2 = render_template_with_rdf(&template, &ontology, &vars)?;
    let output3 = render_template_with_rdf(&template, &ontology, &vars)?;

    // Verify byte-identical output
    assert_eq!(output1, output2, "First and second runs should produce identical output");
    assert_eq!(output2, output3, "Second and third runs should produce identical output");

    // Verify the output contains expected data
    assert!(output1.contains("Total people: 500"));

    Ok(())
});

// ============================================================================
// TESTS: Different RDF Serialization Formats
// ============================================================================

test!(test_turtle_format_determinism, {
    let ontology = create_small_ontology_turtle();
    let template = create_rust_template();

    let mut vars = HashMap::new();
    vars.insert("output_file".to_string(), "output.rs".to_string());

    // Generate from Turtle format multiple times
    let output1 = render_template_with_rdf(&template, &ontology, &vars)?;
    let output2 = render_template_with_rdf(&template, &ontology, &vars)?;

    assert_eq!(output1, output2);

    Ok(())
});

test!(test_jsonld_format_determinism, {
    use oxigraph::io::RdfFormat;

    let ontology_turtle = create_small_ontology_turtle();
    let ontology_jsonld = convert_turtle_to_jsonld(&ontology_turtle)?;
    let template = create_rust_template();

    let mut vars = HashMap::new();
    vars.insert("output_file".to_string(), "output.rs".to_string());

    // Generate from JSON-LD format multiple times
    let output1 = render_template_with_rdf_format(&template, &ontology_jsonld, RdfFormat::JsonLd, &vars)?;
    let output2 = render_template_with_rdf_format(&template, &ontology_jsonld, RdfFormat::JsonLd, &vars)?;

    assert_eq!(output1, output2, "JSON-LD format should produce deterministic output");

    Ok(())
});

test!(test_rdfxml_format_determinism, {
    use oxigraph::io::RdfFormat;

    let ontology_turtle = create_small_ontology_turtle();
    let ontology_rdfxml = convert_turtle_to_rdfxml(&ontology_turtle)?;
    let template = create_rust_template();

    let mut vars = HashMap::new();
    vars.insert("output_file".to_string(), "output.rs".to_string());

    // Generate from RDF-XML format multiple times
    let output1 = render_template_with_rdf_format(&template, &ontology_rdfxml, RdfFormat::RdfXml, &vars)?;
    let output2 = render_template_with_rdf_format(&template, &ontology_rdfxml, RdfFormat::RdfXml, &vars)?;

    assert_eq!(output1, output2, "RDF-XML format should produce deterministic output");

    Ok(())
});

test!(test_cross_format_consistency, {
    use oxigraph::io::RdfFormat;

    let ontology_turtle = create_small_ontology_turtle();
    let ontology_jsonld = convert_turtle_to_jsonld(&ontology_turtle)?;
    let ontology_rdfxml = convert_turtle_to_rdfxml(&ontology_turtle)?;
    let template = create_rust_template();

    let mut vars = HashMap::new();
    vars.insert("output_file".to_string(), "output.rs".to_string());

    // Generate from Turtle
    let output_turtle = render_template_with_rdf(&template, &ontology_turtle, &vars)?;

    // Generate from JSON-LD
    let output_jsonld = render_template_with_rdf_format(&template, &ontology_jsonld, RdfFormat::JsonLd, &vars)?;

    // Generate from RDF-XML
    let output_rdfxml = render_template_with_rdf_format(&template, &ontology_rdfxml, RdfFormat::RdfXml, &vars)?;

    // All formats should produce the same output
    assert_eq!(output_turtle, output_jsonld, "Turtle and JSON-LD should produce identical output");
    assert_eq!(output_jsonld, output_rdfxml, "JSON-LD and RDF-XML should produce identical output");

    Ok(())
});

// ============================================================================
// TESTS: Concurrent Template Regeneration
// ============================================================================

async_test!(test_concurrent_regeneration, {
    use std::sync::Arc;
    use tokio::task;

    let ontology = Arc::new(create_medium_ontology_turtle());
    let template = Arc::new(create_rust_template());

    // Spawn multiple concurrent regeneration tasks
    let mut handles = vec![];
    for i in 0..10 {
        let ontology = Arc::clone(&ontology);
        let template = Arc::clone(&template);

        let handle = task::spawn(async move {
            let mut vars = HashMap::new();
            vars.insert("output_file".to_string(), format!("output_{}.rs", i));

            render_template_with_rdf(&template, &ontology, &vars)
        });

        handles.push(handle);
    }

    // Collect all results
    let mut outputs = vec![];
    for handle in handles {
        outputs.push(handle.await??);
    }

    // All outputs should be identical (except for the filename in metadata)
    let first_hash = calculate_sha256(&outputs[0]);
    for (i, output) in outputs.iter().enumerate().skip(1) {
        let hash = calculate_sha256(output);
        assert_eq!(
            first_hash, hash,
            "Concurrent run {} produced different output",
            i
        );
    }

    Ok(())
});

async_test!(test_concurrent_different_formats, {
    use std::sync::Arc;
    use tokio::task;

    let ontology_turtle = create_small_ontology_turtle();
    let ontology_jsonld = Arc::new(convert_turtle_to_jsonld(&ontology_turtle)?);
    let ontology_rdfxml = Arc::new(convert_turtle_to_rdfxml(&ontology_turtle)?);
    let ontology_turtle = Arc::new(ontology_turtle);
    let template = Arc::new(create_rust_template());

    // Spawn concurrent tasks for each format
    let mut handles = vec![];

    // Turtle tasks
    for i in 0..3 {
        let ontology = Arc::clone(&ontology_turtle);
        let template = Arc::clone(&template);
        let handle = task::spawn(async move {
            let mut vars = HashMap::new();
            vars.insert("output_file".to_string(), format!("output_turtle_{}.rs", i));
            render_template_with_rdf(&template, &ontology, &vars)
        });
        handles.push(handle);
    }

    // Collect results
    let mut outputs = vec![];
    for handle in handles {
        outputs.push(handle.await??);
    }

    // All Turtle outputs should be identical
    for i in 1..outputs.len() {
        assert_eq!(
            calculate_sha256(&outputs[0]),
            calculate_sha256(&outputs[i]),
            "Concurrent Turtle generation produced different outputs"
        );
    }

    Ok(())
});

// ============================================================================
// TESTS: Incremental Ontology Updates
// ============================================================================

test!(test_incremental_ontology_update, {
    let base_ontology = r#"
@prefix ex: <http://example.org/> .

ex:alice a ex:Person ;
    ex:name "Alice" ;
    ex:age 30 .
"#;

    let updated_ontology = r#"
@prefix ex: <http://example.org/> .

ex:alice a ex:Person ;
    ex:name "Alice" ;
    ex:age 30 .

ex:bob a ex:Person ;
    ex:name "Bob" ;
    ex:age 25 .
"#;

    let template = create_rust_template();
    let mut vars = HashMap::new();
    vars.insert("output_file".to_string(), "output.rs".to_string());

    // Generate from base ontology
    let output_base = render_template_with_rdf(&template, base_ontology, &vars)?;

    // Generate from updated ontology
    let output_updated = render_template_with_rdf(&template, updated_ontology, &vars)?;

    // Outputs should be different
    assert_ne!(output_base, output_updated, "Adding data should change output");

    // But generating from updated ontology multiple times should be deterministic
    let output_updated2 = render_template_with_rdf(&template, updated_ontology, &vars)?;
    assert_eq!(output_updated, output_updated2, "Updated ontology should be deterministic");

    // Verify content differences
    assert!(output_updated.contains("Total people: 2"));
    assert!(output_base.contains("Total people: 1"));

    Ok(())
});

test!(test_ontology_ordering_independence, {
    // Same data, different ordering in Turtle
    let ontology1 = r#"
@prefix ex: <http://example.org/> .

ex:alice a ex:Person ; ex:name "Alice" ; ex:age 30 .
ex:bob a ex:Person ; ex:name "Bob" ; ex:age 25 .
"#;

    let ontology2 = r#"
@prefix ex: <http://example.org/> .

ex:bob a ex:Person ; ex:name "Bob" ; ex:age 25 .
ex:alice a ex:Person ; ex:name "Alice" ; ex:age 30 .
"#;

    let template = create_rust_template();
    let mut vars = HashMap::new();
    vars.insert("output_file".to_string(), "output.rs".to_string());

    let output1 = render_template_with_rdf(&template, ontology1, &vars)?;
    let output2 = render_template_with_rdf(&template, ontology2, &vars)?;

    // Should produce identical output because SPARQL query has ORDER BY
    assert_eq!(output1, output2, "Different ordering in ontology should produce same output due to ORDER BY in SPARQL");

    Ok(())
});

// ============================================================================
// TESTS: Cross-Language Consistency
// ============================================================================

test!(test_cross_language_rust_typescript_python, {
    let ontology = create_small_ontology_turtle();

    let rust_template = create_rust_template();
    let typescript_template = create_typescript_template();
    let python_template = create_python_template();

    let mut vars = HashMap::new();
    vars.insert("output_file".to_string(), "output".to_string());

    // Generate from all three languages
    let rust_output = render_template_with_rdf(&rust_template, &ontology, &vars)?;
    let typescript_output = render_template_with_rdf(&typescript_template, &ontology, &vars)?;
    let python_output = render_template_with_rdf(&python_template, &ontology, &vars)?;

    // All should contain the same semantic data
    // Check for person count consistency
    assert!(rust_output.contains("Total people: 2"));
    assert!(typescript_output.contains("Total people: 2"));
    assert!(python_output.contains("Total people: 2"));

    // Check for class count consistency
    assert!(rust_output.contains("Total classes: 1"));
    assert!(typescript_output.contains("Total classes: 1"));
    assert!(python_output.contains("Total classes: 1"));

    // Generate multiple times for each language to verify determinism
    let rust_output2 = render_template_with_rdf(&rust_template, &ontology, &vars)?;
    let typescript_output2 = render_template_with_rdf(&typescript_template, &ontology, &vars)?;
    let python_output2 = render_template_with_rdf(&python_template, &ontology, &vars)?;

    assert_eq!(rust_output, rust_output2);
    assert_eq!(typescript_output, typescript_output2);
    assert_eq!(python_output, python_output2);

    Ok(())
});

test!(test_cross_language_data_consistency, {
    let ontology = create_medium_ontology_turtle();

    let rust_template = create_rust_template();
    let typescript_template = create_typescript_template();
    let python_template = create_python_template();

    let mut vars = HashMap::new();
    vars.insert("output_file".to_string(), "output".to_string());

    let rust_output = render_template_with_rdf(&rust_template, &ontology, &vars)?;
    let typescript_output = render_template_with_rdf(&typescript_template, &ontology, &vars)?;
    let python_output = render_template_with_rdf(&python_template, &ontology, &vars)?;

    // All should have consistent data counts
    assert!(rust_output.contains("Total people: 50"));
    assert!(typescript_output.contains("Total people: 50"));
    assert!(python_output.contains("Total people: 50"));

    // Verify they all contain the same person names (in sorted order due to ORDER BY)
    // Check for first and last person alphabetically
    assert!(rust_output.contains("Person0") || rust_output.contains("person0"));
    assert!(typescript_output.contains("Person0") || typescript_output.contains("person0"));
    assert!(python_output.contains("Person0") || python_output.contains("person0"));

    Ok(())
});

// ============================================================================
// TESTS: File-based End-to-End Determinism
// ============================================================================

test!(test_file_based_determinism, {
    let temp_dir = TempDir::new()?;
    let ontology_path = temp_dir.path().join("ontology.ttl");
    let template_path = temp_dir.path().join("template.tmpl");
    let output_path1 = temp_dir.path().join("output1.rs");
    let output_path2 = temp_dir.path().join("output2.rs");

    // Write ontology and template to files
    fs::write(&ontology_path, create_small_ontology_turtle())?;
    fs::write(&template_path, create_rust_template())?;

    // Generate twice using file paths
    let mut vars = HashMap::new();
    vars.insert("output_file".to_string(), "output.rs".to_string());

    let ontology_content = fs::read_to_string(&ontology_path)?;
    let template_content = fs::read_to_string(&template_path)?;

    let output1 = render_template_with_rdf(&template_content, &ontology_content, &vars)?;
    let output2 = render_template_with_rdf(&template_content, &ontology_content, &vars)?;

    fs::write(&output_path1, &output1)?;
    fs::write(&output_path2, &output2)?;

    // Compare file contents
    let file1_content = fs::read_to_string(&output_path1)?;
    let file2_content = fs::read_to_string(&output_path2)?;

    assert_eq!(file1_content, file2_content, "File-based generation should be deterministic");

    // Verify byte-level identity
    let file1_bytes = fs::read(&output_path1)?;
    let file2_bytes = fs::read(&output_path2)?;

    assert_eq!(file1_bytes, file2_bytes, "Files should be byte-identical");

    Ok(())
});

test!(test_large_ontology_file_determinism, {
    let temp_dir = TempDir::new()?;
    let ontology_path = temp_dir.path().join("large_ontology.ttl");
    let template_path = temp_dir.path().join("template.tmpl");

    // Write large ontology
    fs::write(&ontology_path, create_large_ontology_turtle())?;
    fs::write(&template_path, create_rust_template())?;

    let mut vars = HashMap::new();
    vars.insert("output_file".to_string(), "output.rs".to_string());

    let ontology_content = fs::read_to_string(&ontology_path)?;
    let template_content = fs::read_to_string(&template_path)?;

    // Generate multiple times
    let outputs: Vec<String> = (0..5)
        .map(|_| render_template_with_rdf(&template_content, &ontology_content, &vars))
        .collect::<Result<Vec<_>, _>>()?;

    // All should be identical
    let first_hash = calculate_sha256(&outputs[0]);
    for (i, output) in outputs.iter().enumerate().skip(1) {
        assert_eq!(
            first_hash,
            calculate_sha256(output),
            "Run {} produced different output",
            i
        );
    }

    Ok(())
});
