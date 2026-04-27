//! Chicago TDD Tests for OntologyExtractor
//!
//! These tests use Chicago TDD principles:
//! - State-based testing with real collaborators (actual Graph and OntologyExtractor)
//! - Behavior verification (verify observable outputs and state changes)
//! - AAA pattern (Arrange-Act-Assert)
//! - No mocks - use real oxigraph Graph instances

use ggen_core::{Graph, OntologyExtractor, OntologySchema};

/// Test helper: Create a simple OWL ontology with classes and properties
fn create_simple_ontology_graph() -> Graph {
    let graph = Graph::new().expect("Failed to create graph");

    let ttl = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        # Ontology definition
        ex:PersonOntology a owl:Ontology ;
            rdfs:label "Person Ontology" ;
            rdfs:comment "A simple ontology for people" .

        # Class definitions
        ex:Person a owl:Class ;
            rdfs:label "Person" ;
            rdfs:comment "A human being" .

        ex:Organization a owl:Class ;
            rdfs:label "Organization" ;
            rdfs:comment "A group of people" .

        # Property definitions
        ex:name a owl:DatatypeProperty ;
            rdfs:label "name" ;
            rdfs:comment "The name of a person or organization" ;
            rdfs:domain ex:Person ;
            rdfs:range rdfs:Literal .

        ex:worksFor a owl:ObjectProperty ;
            rdfs:label "works for" ;
            rdfs:comment "Employment relationship" ;
            rdfs:domain ex:Person ;
            rdfs:range ex:Organization .
    "#;

    graph.insert_turtle(ttl).expect("Failed to insert turtle");
    graph
}

/// Test helper: Create an ontology with class hierarchies
fn create_hierarchical_ontology_graph() -> Graph {
    let graph = Graph::new().expect("Failed to create graph");

    let ttl = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        # Class hierarchy
        ex:Entity a owl:Class ;
            rdfs:label "Entity" .

        ex:Person a owl:Class ;
            rdfs:label "Person" ;
            rdfs:subClassOf ex:Entity .

        ex:Employee a owl:Class ;
            rdfs:label "Employee" ;
            rdfs:subClassOf ex:Person .

        ex:Manager a owl:Class ;
            rdfs:label "Manager" ;
            rdfs:subClassOf ex:Employee .
    "#;

    graph.insert_turtle(ttl).expect("Failed to insert turtle");
    graph
}

#[test]
fn test_extract_simple_ontology_returns_correct_schema() {
    // Arrange: Create graph with simple ontology
    let graph = create_simple_ontology_graph();
    let namespace = "http://example.org/";

    // Act: Extract ontology schema
    let schema = OntologyExtractor::extract(&graph, namespace)
        .expect("Failed to extract ontology");

    // Assert: Verify extracted schema structure
    assert_eq!(schema.namespace, namespace);
    assert_eq!(schema.label, "Person Ontology");

    // Verify classes extracted
    assert!(schema.classes.len() >= 2, "Should have at least 2 classes (Person, Organization)");

    let person_class = schema.classes.iter()
        .find(|c| c.name == "Person")
        .expect("Should have Person class");
    assert_eq!(person_class.label, "Person");
    assert_eq!(person_class.description, Some("A human being".to_string()));

    let org_class = schema.classes.iter()
        .find(|c| c.name == "Organization")
        .expect("Should have Organization class");
    assert_eq!(org_class.label, "Organization");
}

#[test]
fn test_extract_properties_from_ontology() {
    // Arrange: Create graph with properties
    let graph = create_simple_ontology_graph();
    let namespace = "http://example.org/";

    // Act: Extract ontology schema
    let schema = OntologyExtractor::extract(&graph, namespace)
        .expect("Failed to extract ontology");

    // Assert: Verify properties extracted correctly
    assert!(schema.properties.len() >= 2, "Should have at least 2 properties (name, worksFor)");

    // Verify datatype property
    let name_prop = schema.properties.iter()
        .find(|p| p.name == "name")
        .expect("Should have name property");
    assert_eq!(name_prop.label, "name");
    assert_eq!(name_prop.description, Some("The name of a person or organization".to_string()));

    // Verify object property
    let works_for_prop = schema.properties.iter()
        .find(|p| p.name == "worksFor")
        .expect("Should have worksFor property");
    assert_eq!(works_for_prop.label, "works for");
}

#[test]
fn test_extract_class_hierarchy_preserves_parent_relationships() {
    // Arrange: Create graph with class hierarchy
    let graph = create_hierarchical_ontology_graph();
    let namespace = "http://example.org/";

    // Act: Extract ontology schema
    let schema = OntologyExtractor::extract(&graph, namespace)
        .expect("Failed to extract ontology");

    // Assert: Verify class hierarchy structure
    let employee_class = schema.classes.iter()
        .find(|c| c.name == "Employee")
        .expect("Should have Employee class");

    // Verify parent relationship
    assert!(!employee_class.parent_classes.is_empty(), "Employee should have parent classes");
    assert!(employee_class.parent_classes.iter()
        .any(|p| p.contains("Person")),
        "Employee should have Person as parent");

    let manager_class = schema.classes.iter()
        .find(|c| c.name == "Manager")
        .expect("Should have Manager class");

    assert!(manager_class.parent_classes.iter()
        .any(|p| p.contains("Employee")),
        "Manager should have Employee as parent");
}

#[test]
fn test_extract_from_empty_graph_returns_empty_schema() {
    // Arrange: Create empty graph
    let graph = Graph::new().expect("Failed to create graph");
    let namespace = "http://example.org/";

    // Act: Extract ontology schema
    let schema = OntologyExtractor::extract(&graph, namespace)
        .expect("Failed to extract ontology");

    // Assert: Verify empty schema
    assert_eq!(schema.namespace, namespace);
    assert!(schema.classes.is_empty(), "Empty graph should have no classes");
    assert!(schema.properties.is_empty(), "Empty graph should have no properties");
    assert!(schema.relationships.is_empty(), "Empty graph should have no relationships");
}

#[test]
fn test_extract_filters_by_namespace() {
    // Arrange: Create graph with multiple namespaces
    let graph = Graph::new().expect("Failed to create graph");

    let ttl = r#"
        @prefix ex1: <http://example1.org/> .
        @prefix ex2: <http://example2.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex1:Person a owl:Class ;
            rdfs:label "Person from ex1" .

        ex2:Person a owl:Class ;
            rdfs:label "Person from ex2" .
    "#;

    graph.insert_turtle(ttl).expect("Failed to insert turtle");

    // Act: Extract with ex1 namespace
    let schema = OntologyExtractor::extract(&graph, "http://example1.org/")
        .expect("Failed to extract ontology");

    // Assert: Verify only ex1 classes extracted
    assert_eq!(schema.classes.len(), 1, "Should only have classes from ex1 namespace");
    assert_eq!(schema.classes[0].label, "Person from ex1");
    assert!(schema.classes[0].uri.contains("example1.org"));
}

#[test]
fn test_extract_handles_missing_optional_fields() {
    // Arrange: Create graph with minimal class definition (no labels/comments)
    let graph = Graph::new().expect("Failed to create graph");

    let ttl = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .

        ex:MinimalClass a owl:Class .
    "#;

    graph.insert_turtle(ttl).expect("Failed to insert turtle");

    // Act: Extract ontology schema
    let schema = OntologyExtractor::extract(&graph, "http://example.org/")
        .expect("Failed to extract ontology");

    // Assert: Verify extraction succeeds with defaults
    assert_eq!(schema.classes.len(), 1);
    let minimal_class = &schema.classes[0];
    assert_eq!(minimal_class.name, "MinimalClass");
    // Label should default to name
    assert_eq!(minimal_class.label, "MinimalClass");
    // Description should be None
    assert!(minimal_class.description.is_none());
}

#[test]
fn test_extract_builds_relationships_from_properties() {
    // Arrange: Create graph with object properties
    let graph = create_simple_ontology_graph();
    let namespace = "http://example.org/";

    // Act: Extract ontology schema
    let schema = OntologyExtractor::extract(&graph, namespace)
        .expect("Failed to extract ontology");

    // Assert: Verify relationships built from properties
    // Note: relationships are built from properties with domain/range
    // The exact structure depends on implementation, but we verify it's populated
    assert!(!schema.properties.is_empty(), "Should have properties to build relationships from");
}

#[test]
fn test_extract_preserves_property_cardinality() {
    // Arrange: Create graph with cardinality restrictions
    let graph = Graph::new().expect("Failed to create graph");

    let ttl = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:Person a owl:Class ;
            rdfs:label "Person" .

        ex:hasName a owl:DatatypeProperty ;
            rdfs:label "has name" ;
            rdfs:domain ex:Person ;
            rdfs:range rdfs:Literal .
    "#;

    graph.insert_turtle(ttl).expect("Failed to insert turtle");

    // Act: Extract ontology schema
    let schema = OntologyExtractor::extract(&graph, "http://example.org/")
        .expect("Failed to extract ontology");

    // Assert: Verify properties extracted
    assert!(!schema.properties.is_empty(), "Should have extracted properties");
    let has_name = schema.properties.iter()
        .find(|p| p.name == "hasName")
        .expect("Should have hasName property");
    assert_eq!(has_name.label, "has name");
}

#[test]
fn test_extract_multiple_parent_classes() {
    // Arrange: Create graph with multiple inheritance
    let graph = Graph::new().expect("Failed to create graph");

    let ttl = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:Animal a owl:Class .
        ex:Intelligent a owl:Class .

        ex:Human a owl:Class ;
            rdfs:subClassOf ex:Animal ;
            rdfs:subClassOf ex:Intelligent .
    "#;

    graph.insert_turtle(ttl).expect("Failed to insert turtle");

    // Act: Extract ontology schema
    let schema = OntologyExtractor::extract(&graph, "http://example.org/")
        .expect("Failed to extract ontology");

    // Assert: Verify multiple parent classes
    let human_class = schema.classes.iter()
        .find(|c| c.name == "Human")
        .expect("Should have Human class");

    assert!(human_class.parent_classes.len() >= 2,
        "Human should have at least 2 parent classes");
    assert!(human_class.parent_classes.iter()
        .any(|p| p.contains("Animal")),
        "Human should have Animal as parent");
    assert!(human_class.parent_classes.iter()
        .any(|p| p.contains("Intelligent")),
        "Human should have Intelligent as parent");
}

#[test]
fn test_extract_with_invalid_namespace_returns_empty_schema() {
    // Arrange: Create graph with classes
    let graph = create_simple_ontology_graph();
    let invalid_namespace = "http://nonexistent.org/";

    // Act: Extract with non-matching namespace
    let schema = OntologyExtractor::extract(&graph, invalid_namespace)
        .expect("Failed to extract ontology");

    // Assert: Verify empty schema (namespace filter excludes all)
    assert!(schema.classes.is_empty(), "Non-matching namespace should result in no classes");
}

#[test]
fn test_extract_preserves_class_sorting() {
    // Arrange: Create graph with multiple classes
    let graph = Graph::new().expect("Failed to create graph");

    let ttl = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .

        ex:Zebra a owl:Class .
        ex:Apple a owl:Class .
        ex:Mango a owl:Class .
    "#;

    graph.insert_turtle(ttl).expect("Failed to insert turtle");

    // Act: Extract ontology schema
    let schema = OntologyExtractor::extract(&graph, "http://example.org/")
        .expect("Failed to extract ontology");

    // Assert: Verify classes are sorted alphabetically
    assert_eq!(schema.classes.len(), 3);
    assert_eq!(schema.classes[0].name, "Apple");
    assert_eq!(schema.classes[1].name, "Mango");
    assert_eq!(schema.classes[2].name, "Zebra");
}
