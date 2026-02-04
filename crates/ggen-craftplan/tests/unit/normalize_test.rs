//! μ₁ (Normalize) Unit Tests
//!
//! Chicago TDD: AAA pattern, real collaborators, state-based verification

use std::fs;
use tempfile::TempDir;
use ggen_craftplan::normalize::Normalizer;
use chicago_tdd_tools::prelude::*;

test!(test_normalize_creates_empty_store, {
    // Arrange
    let normalizer = Normalizer::new().expect("Failed to create normalizer");

    // Act
    let store = normalizer.store();

    // Assert
    assert_eq!(store.len(), 0, "Empty store should have 0 triples");
});

test!(test_normalize_validate_empty_store_fails, {
    // Arrange
    let normalizer = Normalizer::new().expect("Failed to create normalizer");

    // Act
    let result = normalizer.validate();

    // Assert
    assert!(result.is_err(), "Validation should fail for empty store");
    let err = result.unwrap_err();
    let err_msg = err.to_string();
    assert!(err_msg.contains("empty"), "Error should mention empty store");
});

test!(test_normalize_load_minimal_rdf, {
    // Arrange
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let rdf_path = temp_dir.path().join("test.ttl");
    fs::write(
        &rdf_path,
        r#"
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
            @prefix craft: <http://craftplan.org/ontology/> .

            craft:Product a rdfs:Class ;
                rdfs:label "Product" .
        "#
    ).expect("Failed to write RDF file");

    let mut normalizer = Normalizer::new().expect("Failed to create normalizer");

    // Act
    let result = normalizer.load_rdf(&rdf_path);

    // Assert
    assert!(result.is_ok(), "Should load minimal RDF successfully");
});

test!(test_normalize_load_nonexistent_file_fails, {
    // Arrange
    let mut normalizer = Normalizer::new().expect("Failed to create normalizer");
    let nonexistent_path = "/tmp/this_file_does_not_exist_12345.ttl";

    // Act
    let result = normalizer.load_rdf(nonexistent_path);

    // Assert
    assert!(result.is_err(), "Should fail to load nonexistent file");
});

test!(test_normalize_load_invalid_rdf_syntax, {
    // Arrange
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let rdf_path = temp_dir.path().join("invalid.ttl");
    fs::write(
        &rdf_path,
        r#"
            This is not valid Turtle RDF syntax
            at all!!!!!!!!
        "#
    ).expect("Failed to write invalid RDF file");

    let mut normalizer = Normalizer::new().expect("Failed to create normalizer");

    // Act
    let result = normalizer.load_rdf(&rdf_path);

    // Assert
    assert!(result.is_err(), "Should fail to parse invalid RDF");
});

test!(test_normalize_validate_returns_triple_count, {
    // Arrange
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let rdf_path = temp_dir.path().join("test.ttl");
    fs::write(
        &rdf_path,
        r#"
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
            @prefix craft: <http://craftplan.org/ontology/> .

            craft:Product a rdfs:Class .
            craft:Order a rdfs:Class .
            craft:Customer a rdfs:Class .
        "#
    ).expect("Failed to write RDF file");

    let mut normalizer = Normalizer::new().expect("Failed to create normalizer");
    normalizer.load_rdf(&rdf_path).expect("Failed to load RDF");

    // Act
    let count = normalizer.validate().expect("Validation should succeed");

    // Assert
    assert!(count > 0, "Should have loaded some triples");
});

test!(test_normalize_resolve_dependencies, {
    // Arrange
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let rdf_path = temp_dir.path().join("test.ttl");
    fs::write(
        &rdf_path,
        r#"
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
            @prefix craft: <http://craftplan.org/ontology/> .

            craft:Product a rdfs:Class ;
                rdfs:label "Product" .

            craft:Order a rdfs:Class ;
                rdfs:label "Order" .
        "#
    ).expect("Failed to write RDF file");

    let mut normalizer = Normalizer::new().expect("Failed to create normalizer");
    normalizer.load_rdf(&rdf_path).expect("Failed to load RDF");

    // Act
    let deps = normalizer.resolve_dependencies().expect("Should resolve dependencies");

    // Assert
    assert!(!deps.is_empty(), "Should find at least one entity");
});

test!(test_normalize_default_trait_works, {
    // Arrange & Act
    let normalizer = Normalizer::default();

    // Assert
    let store = normalizer.store();
    assert_eq!(store.len(), 0, "Default normalizer should have empty store");
});
