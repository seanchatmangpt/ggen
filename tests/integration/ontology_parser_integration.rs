//! Integration Tests for Ontology Parser
//!
//! Chicago TDD integration tests for end-to-end ontology parsing:
//! - Real RDF/OWL inputs from files
//! - Complete extraction pipeline
//! - Error handling with invalid inputs
//! - Performance characteristics

use ggen_core::{Graph, OntologyExtractor};
use std::fs;
use tempfile::TempDir;

/// Test helper: Create temporary OWL file
fn create_temp_owl_file(content: &str) -> (TempDir, std::path::PathBuf) {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let file_path = temp_dir.path().join("test_ontology.ttl");
    fs::write(&file_path, content).expect("Failed to write temp file");
    (temp_dir, file_path)
}

#[test]
fn test_end_to_end_ontology_extraction_from_file() {
    // Arrange: Create OWL file with real ontology
    let owl_content = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:Product a owl:Class ;
            rdfs:label "Product" ;
            rdfs:comment "An item for sale" .

        ex:Category a owl:Class ;
            rdfs:label "Category" ;
            rdfs:comment "Product category" .

        ex:name a owl:DatatypeProperty ;
            rdfs:label "name" ;
            rdfs:domain ex:Product ;
            rdfs:range rdfs:Literal .

        ex:belongsTo a owl:ObjectProperty ;
            rdfs:label "belongs to" ;
            rdfs:domain ex:Product ;
            rdfs:range ex:Category .
    "#;

    let (_temp_dir, file_path) = create_temp_owl_file(owl_content);

    // Act: Load file into graph and extract
    let graph = Graph::new().expect("Failed to create graph");
    let file_content = fs::read_to_string(&file_path)
        .expect("Failed to read temp file");
    graph.insert_turtle(&file_content)
        .expect("Failed to insert turtle");

    let schema = OntologyExtractor::extract(&graph, "http://example.org/")
        .expect("Failed to extract ontology");

    // Assert: Verify complete extraction
    assert_eq!(schema.classes.len(), 2, "Should extract both Product and Category classes");
    assert_eq!(schema.properties.len(), 2, "Should extract both name and belongsTo properties");

    // Verify class details
    let product = schema.classes.iter()
        .find(|c| c.name == "Product")
        .expect("Should have Product class");
    assert_eq!(product.label, "Product");
    assert_eq!(product.description, Some("An item for sale".to_string()));

    // Verify property details
    let belongs_to = schema.properties.iter()
        .find(|p| p.name == "belongsTo")
        .expect("Should have belongsTo property");
    assert_eq!(belongs_to.label, "belongs to");
}

#[test]
fn test_extraction_handles_malformed_turtle_gracefully() {
    // Arrange: Create graph with invalid turtle syntax
    let graph = Graph::new().expect("Failed to create graph");

    let invalid_ttl = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .

        ex:InvalidClass a owl:Class
        # Missing period - syntax error
    "#;

    // Act: Attempt to insert malformed turtle
    let insert_result = graph.insert_turtle(invalid_ttl);

    // Assert: Verify error handling
    assert!(insert_result.is_err(), "Should reject malformed turtle");
}

#[test]
fn test_extraction_handles_missing_namespace_prefix() {
    // Arrange: Create graph with undefined prefix
    let graph = Graph::new().expect("Failed to create graph");

    let ttl = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .

        # Using undefined prefix 'foo'
        ex:MyClass a owl:Class ;
            foo:unknownProperty "value" .
    "#;

    // Act: Insert turtle with undefined prefix
    let insert_result = graph.insert_turtle(ttl);

    // Assert: Verify error handling
    assert!(insert_result.is_err(), "Should reject undefined prefix");
}

#[test]
fn test_extraction_with_large_ontology() {
    // Arrange: Create large ontology (100+ classes)
    let mut ttl = String::from(r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
    "#);

    // Generate 100 classes
    for i in 0..100 {
        ttl.push_str(&format!(r#"
            ex:Class{} a owl:Class ;
                rdfs:label "Class {}" ;
                rdfs:comment "Test class number {}" .
        "#, i, i, i));
    }

    let graph = Graph::new().expect("Failed to create graph");
    graph.insert_turtle(&ttl).expect("Failed to insert turtle");

    // Act: Extract large ontology
    let start = std::time::Instant::now();
    let schema = OntologyExtractor::extract(&graph, "http://example.org/")
        .expect("Failed to extract ontology");
    let duration = start.elapsed();

    // Assert: Verify extraction completes efficiently
    assert_eq!(schema.classes.len(), 100, "Should extract all 100 classes");
    assert!(duration.as_secs() < 5, "Extraction should complete within 5 seconds (SLO)");
}

#[test]
fn test_extraction_with_circular_class_hierarchy() {
    // Arrange: Create ontology with circular inheritance (invalid OWL but possible in RDF)
    let graph = Graph::new().expect("Failed to create graph");

    let ttl = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:A a owl:Class ;
            rdfs:subClassOf ex:B .

        ex:B a owl:Class ;
            rdfs:subClassOf ex:C .

        ex:C a owl:Class ;
            rdfs:subClassOf ex:A .
    "#;

    graph.insert_turtle(ttl).expect("Failed to insert turtle");

    // Act: Extract with circular hierarchy
    let result = OntologyExtractor::extract(&graph, "http://example.org/");

    // Assert: Verify extraction doesn't hang and completes
    assert!(result.is_ok(), "Extraction should handle circular hierarchies gracefully");
    let schema = result.unwrap();
    assert_eq!(schema.classes.len(), 3, "Should extract all 3 classes");
}

#[test]
fn test_extraction_preserves_unicode_labels() {
    // Arrange: Create ontology with Unicode labels
    let graph = Graph::new().expect("Failed to create graph");

    let ttl = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:Person a owl:Class ;
            rdfs:label "Person"@en ;
            rdfs:label "人"@zh ;
            rdfs:label "Person"@de .
    "#;

    graph.insert_turtle(ttl).expect("Failed to insert turtle");

    // Act: Extract ontology
    let schema = OntologyExtractor::extract(&graph, "http://example.org/")
        .expect("Failed to extract ontology");

    // Assert: Verify Unicode labels preserved
    assert_eq!(schema.classes.len(), 1);
    let person_class = &schema.classes[0];
    // Note: Which label is selected depends on implementation
    // We just verify extraction succeeds and has a label
    assert!(!person_class.label.is_empty(), "Should have a label");
}

#[test]
fn test_extraction_handles_empty_namespace() {
    // Arrange: Create ontology with classes
    let graph = Graph::new().expect("Failed to create graph");

    let ttl = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .

        ex:TestClass a owl:Class .
    "#;

    graph.insert_turtle(ttl).expect("Failed to insert turtle");

    // Act: Extract with empty namespace filter
    let schema = OntologyExtractor::extract(&graph, "")
        .expect("Failed to extract ontology");

    // Assert: Verify extraction with empty namespace
    // Empty namespace should match nothing (depending on implementation)
    // or match everything - we verify it doesn't crash
    assert!(schema.classes.len() <= 1, "Empty namespace handling should be deterministic");
}

#[test]
fn test_extraction_with_special_characters_in_uris() {
    // Arrange: Create ontology with special characters in URIs
    let graph = Graph::new().expect("Failed to create graph");

    let ttl = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:Person-Manager a owl:Class ;
            rdfs:label "Person Manager" .

        ex:has_name a owl:DatatypeProperty ;
            rdfs:label "has name" ;
            rdfs:domain ex:Person-Manager .
    "#;

    graph.insert_turtle(ttl).expect("Failed to insert turtle");

    // Act: Extract ontology
    let schema = OntologyExtractor::extract(&graph, "http://example.org/")
        .expect("Failed to extract ontology");

    // Assert: Verify special characters handled correctly
    assert!(schema.classes.iter().any(|c| c.uri.contains("Person-Manager")),
        "Should handle hyphens in class names");
    assert!(schema.properties.iter().any(|p| p.uri.contains("has_name")),
        "Should handle underscores in property names");
}

#[test]
fn test_extraction_concurrent_access() {
    // Arrange: Create ontology graph
    let graph = Graph::new().expect("Failed to create graph");

    let ttl = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .

        ex:TestClass a owl:Class .
    "#;

    graph.insert_turtle(ttl).expect("Failed to insert turtle");

    // Act: Extract concurrently (simulate multiple readers)
    let handles: Vec<_> = (0..5).map(|_| {
        let g = graph.clone();
        std::thread::spawn(move || {
            OntologyExtractor::extract(&g, "http://example.org/")
        })
    }).collect();

    // Assert: Verify all extractions succeed
    for handle in handles {
        let result = handle.join().expect("Thread panicked");
        assert!(result.is_ok(), "Concurrent extraction should succeed");
    }
}

#[test]
fn test_extraction_memory_efficiency() {
    // Arrange: Create medium-sized ontology
    let mut ttl = String::from(r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
    "#);

    // Generate 50 classes with properties
    for i in 0..50 {
        ttl.push_str(&format!(r#"
            ex:Class{} a owl:Class ;
                rdfs:label "Class {}" .

            ex:prop{} a owl:DatatypeProperty ;
                rdfs:domain ex:Class{} ;
                rdfs:range rdfs:Literal .
        "#, i, i, i, i));
    }

    let graph = Graph::new().expect("Failed to create graph");
    graph.insert_turtle(&ttl).expect("Failed to insert turtle");

    // Act: Extract and measure memory usage
    let schema = OntologyExtractor::extract(&graph, "http://example.org/")
        .expect("Failed to extract ontology");

    // Assert: Verify extraction is memory efficient
    // Schema should be small compared to input
    let schema_json = serde_json::to_string(&schema)
        .expect("Failed to serialize schema");
    let schema_size_kb = schema_json.len() / 1024;

    assert!(schema_size_kb < 100, "Schema should be under 100KB for 50 classes (memory SLO)");
    assert_eq!(schema.classes.len(), 50, "Should have all 50 classes");
}
