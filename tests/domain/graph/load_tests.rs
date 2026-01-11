//! Chicago TDD tests for RDF loading
//!
//! Uses REAL file I/O and REAL graph state verification

use anyhow::Result;
use ggen_cli::domain::graph::{load_rdf, LoadOptions, RdfFormat};
use std::io::Write;
use tempfile::NamedTempFile;

#[test]
fn test_load_real_turtle_file() -> Result<()> {
    // Create REAL Turtle file
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .

        ex:alice a foaf:Person ;
            foaf:name "Alice" .

        ex:bob a foaf:Person ;
            foaf:name "Bob" .
    "#;

    let mut temp_file = NamedTempFile::new()?;
    temp_file.write_all(turtle.as_bytes())?;
    let temp_path = temp_file.path().to_string_lossy().to_string();

    // Load REAL RDF file
    let options = LoadOptions {
        file_path: temp_path.clone(),
        format: Some(RdfFormat::Turtle),
        base_iri: None,
        merge: false,
    };

    let stats = load_rdf(options)?;

    // Verify REAL triple count
    assert_eq!(stats.format, RdfFormat::Turtle);
    assert_eq!(stats.file_path, temp_path);
    assert!(stats.total_triples > 0);
    assert_eq!(stats.triples_loaded, stats.total_triples);

    Ok(())
}

#[test]
fn test_load_verifies_graph_state() -> Result<()> {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:triple1 ex:predicate1 "value1" .
        ex:triple2 ex:predicate2 "value2" .
        ex:triple3 ex:predicate3 "value3" .
    "#;

    let mut temp_file = NamedTempFile::new()?;
    temp_file.write_all(turtle.as_bytes())?;
    let temp_path = temp_file.path().to_string_lossy().to_string();

    let options = LoadOptions {
        file_path: temp_path,
        format: Some(RdfFormat::Turtle),
        base_iri: None,
        merge: false,
    };

    let stats = load_rdf(options)?;

    // Verify graph state has REAL triples
    assert!(stats.total_triples >= 3);

    Ok(())
}

#[test]
fn test_load_complex_rdf() -> Result<()> {
    let turtle = r#"
        @prefix ex: <http://example.org/> .
        @prefix foaf: <http://xmlns.com/foaf/0.1/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ex:person1 a foaf:Person ;
            foaf:name "John Doe" ;
            foaf:age "30"^^xsd:integer ;
            foaf:knows ex:person2 .

        ex:person2 a foaf:Person ;
            foaf:name "Jane Smith" ;
            foaf:age "28"^^xsd:integer .
    "#;

    let mut temp_file = NamedTempFile::new()?;
    temp_file.write_all(turtle.as_bytes())?;
    let temp_path = temp_file.path().to_string_lossy().to_string();

    let options = LoadOptions {
        file_path: temp_path,
        format: Some(RdfFormat::Turtle),
        base_iri: Some("http://example.org/base/".to_string()),
        merge: false,
    };

    let stats = load_rdf(options)?;

    // Verify complex RDF loaded
    assert!(stats.total_triples >= 6);

    Ok(())
}

#[test]
fn test_load_format_detection() {
    assert_eq!(RdfFormat::from_extension("data.ttl"), RdfFormat::Turtle);
    assert_eq!(RdfFormat::from_extension("data.nt"), RdfFormat::NTriples);
    assert_eq!(RdfFormat::from_extension("data.rdf"), RdfFormat::RdfXml);
    assert_eq!(RdfFormat::from_extension("data.jsonld"), RdfFormat::JsonLd);
    assert_eq!(RdfFormat::from_extension("data.n3"), RdfFormat::N3);
}

#[test]
fn test_load_nonexistent_file() {
    let options = LoadOptions {
        file_path: "/nonexistent/file.ttl".to_string(),
        format: Some(RdfFormat::Turtle),
        base_iri: None,
        merge: false,
    };

    let result = load_rdf(options);
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("not found"));
}
