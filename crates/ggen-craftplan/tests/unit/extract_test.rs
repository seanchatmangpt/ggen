//! μ₂ (Extract) Unit Tests
//!
//! Chicago TDD: AAA pattern, real collaborators, state-based verification

use std::fs;
use tempfile::TempDir;
use ggen_craftplan::extract::{Entity, Extractor};
use ggen_craftplan::normalize::Normalizer;
use chicago_tdd_tools::prelude::*;

test!(test_extractor_creation, {
    // Arrange
    let normalizer = Normalizer::new().expect("Failed to create normalizer");

    // Act
    let extractor = Extractor::new(&normalizer);

    // Assert
    assert!(extractor.is_ok(), "Extractor should be created successfully");
});

test!(test_extract_entities_from_empty_store, {
    // Arrange
    let normalizer = Normalizer::new().expect("Failed to create normalizer");
    let store = normalizer.store();
    let mut extractor = Extractor::new(&normalizer).expect("Failed to create extractor");

    // Act
    let entities = extractor.extract_entities(store);

    // Assert
    assert!(entities.is_ok(), "Extraction should not error");
    let result = entities.unwrap();
    assert_eq!(result.len(), 0, "Should have no entities from empty store");
});

test!(test_extract_single_entity, {
    // Arrange
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let rdf_path = temp_dir.path().join("test.ttl");
    fs::write(
        &rdf_path,
        r#"
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
            @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
            @prefix craft: <http://craftplan.org/ontology/> .

            craft:ProductEntity a craft:Entity ;
                craft:name "Product" ;
                craft:pluralName "Products" .
        "#
    ).expect("Failed to write RDF file");

    let mut normalizer = Normalizer::new().expect("Failed to create normalizer");
    normalizer.load_rdf(&rdf_path).expect("Failed to load RDF");
    let store = normalizer.store();
    let mut extractor = Extractor::new(&normalizer).expect("Failed to create extractor");

    // Act
    let entities = extractor.extract_entities(store).expect("Failed to extract entities");

    // Assert
    assert_eq!(entities.len(), 1, "Should extract exactly one entity");
    assert_eq!(entities[0].name, "Product", "Entity name should match");
    assert_eq!(entities[0].plural, Some("Products".to_string()), "Plural should match");
});

test!(test_extract_multiple_entities, {
    // Arrange
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let rdf_path = temp_dir.path().join("test.ttl");
    fs::write(
        &rdf_path,
        r#"
            @prefix craft: <http://craftplan.org/ontology/> .

            craft:ProductEntity a craft:Entity ;
                craft:name "Product" ;
                craft:pluralName "Products" .

            craft:OrderEntity a craft:Entity ;
                craft:name "Order" ;
                craft:pluralName "Orders" .

            craft:CustomerEntity a craft:Entity ;
                craft:name "Customer" ;
                craft:pluralName "Customers" .
        "#
    ).expect("Failed to write RDF file");

    let mut normalizer = Normalizer::new().expect("Failed to create normalizer");
    normalizer.load_rdf(&rdf_path).expect("Failed to load RDF");
    let store = normalizer.store();
    let mut extractor = Extractor::new(&normalizer).expect("Failed to create extractor");

    // Act
    let entities = extractor.extract_entities(store).expect("Failed to extract entities");

    // Assert
    assert_eq!(entities.len(), 3, "Should extract exactly three entities");
    assert_eq!(entities[0].name, "Product", "First entity should be Product");
    assert_eq!(entities[1].name, "Order", "Second entity should be Order");
    assert_eq!(entities[2].name, "Customer", "Third entity should be Customer");
});

test!(test_extract_attributes_for_entity, {
    // Arrange
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let rdf_path = temp_dir.path().join("test.ttl");
    fs::write(
        &rdf_path,
        r#"
            @prefix craft: <http://craftplan.org/ontology/> .
            @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

            craft:ProductEntity a craft:Entity ;
                craft:name "Product" ;
                craft:hasAttribute craft:productName ,
                                 craft:productPrice ,
                                 craft:inStock .

            craft:productName a craft:Attribute ;
                craft:name "name" ;
                craft:type "string" ;
                craft:required "true"^^xsd:boolean ;
                craft:documentation "The product name" .

            craft:productPrice a craft:Attribute ;
                craft:name "price" ;
                craft:type "decimal" ;
                craft:required "false"^^xsd:boolean .

            craft:inStock a craft:Attribute ;
                craft:name "in_stock" ;
                craft:type "boolean" ;
                craft:required "true"^^xsd:boolean .
        "#
    ).expect("Failed to write RDF file");

    let mut normalizer = Normalizer::new().expect("Failed to create normalizer");
    normalizer.load_rdf(&rdf_path).expect("Failed to load RDF");
    let store = normalizer.store();
    let extractor = Extractor::new(&normalizer).expect("Failed to create extractor");

    // Act
    let attrs = extractor
        .extract_attributes(store, "Product")
        .expect("Failed to extract attributes");

    // Assert
    assert_eq!(attrs.len(), 3, "Should extract exactly three attributes");
    assert_eq!(attrs[0].name, "name", "First attribute should be name");
    assert_eq!(attrs[0].type_, "string", "First attribute type should be string");
    assert!(attrs[0].required, "First attribute should be required");
    assert_eq!(attrs[0].doc, Some("The product name".to_string()), "Should have doc");
});

test!(test_extract_attributes_empty_for_nonexistent_entity, {
    // Arrange
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let rdf_path = temp_dir.path().join("test.ttl");
    fs::write(
        &rdf_path,
        r#"
            @prefix craft: <http://craftplan.org/ontology/> .
            craft:ProductEntity a craft:Entity ;
                craft:name "Product" .
        "#
    ).expect("Failed to write RDF file");

    let mut normalizer = Normalizer::new().expect("Failed to create normalizer");
    normalizer.load_rdf(&rdf_path).expect("Failed to load RDF");
    let store = normalizer.store();
    let extractor = Extractor::new(&normalizer).expect("Failed to create extractor");

    // Act
    let attrs = extractor
        .extract_attributes(store, "NonExistent")
        .expect("Failed to extract attributes");

    // Assert
    assert_eq!(attrs.len(), 0, "Should have no attributes for nonexistent entity");
});

test!(test_entity_struct_has_empty_relationships_initially, {
    // Arrange & Act
    let entity = Entity {
        name: "Test".to_string(),
        plural: None,
        attributes: vec![],
        relationships: vec![],
    };

    // Assert
    assert_eq!(entity.relationships.len(), 0, "New entity should have no relationships");
    assert_eq!(entity.name, "Test", "Entity name should match");
});
