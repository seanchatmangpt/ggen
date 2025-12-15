//! Chicago TDD integration tests - load, query, export workflow
//!
//! Tests REAL end-to-end workflows with Oxigraph

use anyhow::Result;
use ggen_cli::domain::graph::{
    execute_sparql, export_graph, load_rdf, ExportFormat, ExportOptions, LoadOptions, QueryOptions,
    RdfFormat,
};
use std::fs;
use std::io::Write;
use tempfile::{tempdir, NamedTempFile};

#[test]
fn test_load_query_export_workflow() -> Result<()> {
    let temp_dir = tempdir()?;

    // Step 1: Create REAL RDF file
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .

        ex:alice a foaf:Person ;
            foaf:name "Alice" ;
            foaf:age "30" .

        ex:bob a foaf:Person ;
            foaf:name "Bob" ;
            foaf:age "25" .
    "#;

    let mut input_file = NamedTempFile::new()?;
    input_file.write_all(turtle.as_bytes())?;
    let input_path = input_file.path().to_string_lossy().to_string();

    // Step 2: Load REAL RDF data
    let load_options = LoadOptions {
        file_path: input_path.clone(),
        format: Some(RdfFormat::Turtle),
        base_iri: None,
        merge: false,
    };

    let load_stats = load_rdf(load_options)?;
    assert!(load_stats.total_triples > 0);

    // Step 3: Execute REAL SPARQL query
    let query_options = QueryOptions {
        query: r#"
            PREFIX foaf: <http://xmlns.com/foaf/0.1/>
            SELECT ?name
            WHERE {
                ?person foaf:name ?name .
            }
        "#
        .to_string(),
        graph_file: Some(input_path),
        output_format: "json".to_string(),
    };

    let query_result = execute_sparql(query_options)?;
    assert_eq!(query_result.result_count, 2);

    // Step 4: Export to different format
    let output_path = temp_dir.path().join("output.jsonld");
    let export_options = ExportOptions {
        output_path: output_path.to_string_lossy().to_string(),
        format: ExportFormat::JsonLd,
        pretty: true,
        graph: None,
    };

    export_graph(export_options)?;

    // Verify REAL export file
    assert!(output_path.exists());
    let exported = fs::read_to_string(&output_path)?;
    assert!(exported.contains("@context"));

    Ok(())
}

#[test]
fn test_filter_and_export() -> Result<()> {
    let temp_dir = tempdir()?;

    // Create data with ages
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ex:alice ex:age "30"^^xsd:integer .
        ex:bob ex:age "25"^^xsd:integer .
        ex:charlie ex:age "35"^^xsd:integer .
    "#;

    let mut temp_file = NamedTempFile::new()?;
    temp_file.write_all(turtle.as_bytes())?;
    let temp_path = temp_file.path().to_string_lossy().to_string();

    // Load data
    let load_options = LoadOptions {
        file_path: temp_path.clone(),
        format: Some(RdfFormat::Turtle),
        base_iri: None,
        merge: false,
    };

    load_rdf(load_options)?;

    // Query with FILTER
    let query_options = QueryOptions {
        query: r#"
            PREFIX ex: <http://example.org/>
            PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>
            SELECT ?person ?age
            WHERE {
                ?person ex:age ?age .
                FILTER(?age > "28"^^xsd:integer)
            }
        "#
        .to_string(),
        graph_file: Some(temp_path),
        output_format: "json".to_string(),
    };

    let result = execute_sparql(query_options)?;
    assert_eq!(result.result_count, 2); // alice and charlie

    // Export results
    let output_path = temp_dir.path().join("filtered.ttl");
    let export_options = ExportOptions {
        output_path: output_path.to_string_lossy().to_string(),
        format: ExportFormat::Turtle,
        pretty: true,
        graph: None,
    };

    export_graph(export_options)?;
    assert!(output_path.exists());

    Ok(())
}

#[test]
fn test_multiple_format_round_trip() -> Result<()> {
    let temp_dir = tempdir()?;

    // Original Turtle
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:subject ex:predicate "value" .
    "#;

    let mut input_file = NamedTempFile::new()?;
    input_file.write_all(turtle.as_bytes())?;
    let input_path = input_file.path().to_string_lossy().to_string();

    // Load Turtle
    load_rdf(LoadOptions {
        file_path: input_path,
        format: Some(RdfFormat::Turtle),
        base_iri: None,
        merge: false,
    })?;

    // Export to multiple formats
    let formats = vec![
        ExportFormat::NTriples,
        ExportFormat::RdfXml,
        ExportFormat::JsonLd,
    ];

    for format in formats {
        let output_path = temp_dir
            .path()
            .join(format!("output.{}", format.as_str()));

        export_graph(ExportOptions {
            output_path: output_path.to_string_lossy().to_string(),
            format,
            pretty: true,
            graph: None,
        })?;

        assert!(output_path.exists());
    }

    Ok(())
}
