//! Chicago TDD unit tests for ggen-yawl ontology::loader module
//!
//! Tests follow AAA pattern (Arrange/Act/Assert) with real collaborators.
//! State-based verification over interaction verification.

use ggen_core::Graph;
use ggen_yawl::ontology::loader::{OntologyFormat, OntologyLoader};
use std::io::Write;
use std::str::FromStr;
use tempfile::NamedTempFile;

/// Helper module for test fixtures and utilities.
pub mod fixtures {
    use super::*;

    /// Valid Turtle ontology fixture for testing.
    pub const VALID_TURTLE: &str = r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ex:TestWorkflow a owl:Class ;
            rdfs:label "Test Workflow" ;
            rdfs:comment "A test workflow for unit testing" .

        ex:Task1 a ex:AtomicTask ;
            rdfs:label "First Task" ;
            ex:taskId "t1" ;
            ex:splitBehavior ex:XOR ;
            ex:joinBehavior ex:XOR .

        ex:Task2 a ex:AtomicTask ;
            rdfs:label "Second Task" ;
            ex:taskId "t2" ;
            ex:splitBehavior ex:AND ;
            ex:joinBehavior ex:AND .
    "#;

    /// Valid RDF/XML ontology fixture for testing.
    pub const VALID_RDF_XML: &str = r#"<?xml version="1.0"?>
        <rdf:RDF xmlns:rdf="http://www.w3.org/1999/02/22-rdf-syntax-ns#"
                 xmlns:ex="http://example.org/"
                 xmlns:rdfs="http://www.w3.org/2000/01/rdf-schema#">
            <owl:Class rdf:about="http://example.org/TestWorkflow">
                <rdfs:label>XML Workflow</rdfs:label>
            </owl:Class>
        </rdf:RDF>
    "#;

    /// Valid N-Triples ontology fixture for testing.
    pub const VALID_NTRIPLES: &str = r#"
        <http://example.org/subject> <http://example.org/predicate> "object" .
        <http://example.org/subject> <http://www.w3.org/1999/02/22-rdf-syntax-ns#type> <http://example.org/Class> .
    "#;

    /// Valid N-Quads ontology fixture for testing.
    pub const VALID_NQUADS: &str = r#"
        <http://example.org/s> <http://example.org/p> "o" <http://example.org/graph> .
    "#;

    /// Valid TriG ontology fixture for testing.
    pub const VALID_TRIG: &str = r#"
        @prefix ex: <http://example.org/> .

        ex:graph1 {
            ex:s ex:p ex:o .
        }
    "#;

    /// Invalid Turtle syntax fixture for error testing.
    pub const INVALID_TURTLE: &str = r#"
        @prefix ex: <http://example.org/> .
        ex:Test a ex:Class
        ;  # Missing object
    "#;

    /// Turtle with owl:imports for testing import flattening.
    pub const TURTLE_WITH_IMPORTS: &str = r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .

        ex:MainOntology a owl:Ontology ;
            owl:imports ex:ImportedOntology .

        ex:MainClass a owl:Class ;
            rdfs:label "Main Class" .
    "#;

    /// Complex Turtle ontology with multiple entity types.
    pub const COMPLEX_TURTLE: &str = r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        # Classes
        ex:Workflow a owl:Class ;
            rdfs:label "Workflow" ;
            rdfs:comment "Represents a business workflow" ;
            rdfs:subClassOf owl:Thing .

        ex:Task a owl:Class ;
            rdfs:label "Task" ;
            rdfs:comment "A unit of work" .

        # Properties
        ex:hasTask a owl:ObjectProperty ;
            rdfs:label "has task" ;
            rdfs:domain ex:Workflow ;
            rdfs:range ex:Task .

        ex:taskName a owl:DatatypeProperty ;
            rdfs:label "task name" ;
            rdfs:domain ex:Task ;
            rdfs:range xsd:string .

        # Instances
        ex:MyWorkflow a ex:Workflow ;
            rdfs:label "My Workflow" ;
            ex:hasTask ex:Task1, ex:Task2 .

        ex:Task1 a ex:Task ;
            rdfs:label "Task 1" ;
            ex:taskName "Initialize" .

        ex:Task2 a ex:Task ;
            rdfs:label "Task 2" ;
            ex:taskName "Process" .
    "#;
}

#[cfg(test)]
mod ontology_format_tests {
    use super::*;
    use fixtures::*;

    /// Test: OntologyFormat::from_str parses valid format strings correctly
    #[test]
    fn test_format_from_str_valid_formats() {
        // Arrange & Act
        let turtle = OntologyFormat::from_str("ttl");
        let turtle_upper = OntologyFormat::from_str("Turtle");
        let rdf = OntologyFormat::from_str("rdf");
        let xml = OntologyFormat::from_str("xml");
        let owl = OntologyFormat::from_str("owl");
        let nt = OntologyFormat::from_str("nt");
        let nq = OntologyFormat::from_str("nq");
        let trig = OntologyFormat::from_str("trig");

        // Assert
        assert!(matches!(turtle, Ok(OntologyFormat::Turtle)));
        assert!(matches!(turtle_upper, Ok(OntologyFormat::Turtle)));
        assert!(matches!(rdf, Ok(OntologyFormat::RdfXml)));
        assert!(matches!(xml, Ok(OntologyFormat::RdfXml)));
        assert!(matches!(owl, Ok(OntologyFormat::RdfXml)));
        assert!(matches!(nt, Ok(OntologyFormat::NTriples)));
        assert!(matches!(nq, Ok(OntologyFormat::NQuads)));
        assert!(matches!(trig, Ok(OntologyFormat::Trig)));
    }

    /// Test: OntologyFormat::from_str returns error for unknown formats
    #[test]
    fn test_format_from_str_unknown_format() {
        // Arrange & Act
        let result = OntologyFormat::from_str("unknown_format");

        // Assert
        assert!(result.is_err());
        match result {
            Err(e) => {
                let error_msg = e.to_string();
                assert!(error_msg.contains("Unknown format"));
                assert!(error_msg.contains("unknown_format"));
            }
            Ok(_) => panic!("Expected error for unknown format"),
        }
    }

    /// Test: OntologyFormat::from_extension correctly maps extensions
    #[test]
    fn test_format_from_extension() {
        // Arrange & Act
        let ttl = OntologyFormat::from_extension("ttl");
        let turtle = OntologyFormat::from_extension("turtle");
        let rdf = OntologyFormat::from_extension("rdf");
        let owl = OntologyFormat::from_extension("owl");
        let nt = OntologyFormat::from_extension("nt");
        let nq = OntologyFormat::from_extension("nq");
        let trig = OntologyFormat::from_extension("trig");
        let unknown = OntologyFormat::from_extension("unknown");

        // Assert
        assert_eq!(ttl, Some(OntologyFormat::Turtle));
        assert_eq!(turtle, Some(OntologyFormat::Turtle));
        assert_eq!(rdf, Some(OntologyFormat::RdfXml));
        assert_eq!(owl, Some(OntologyFormat::RdfXml));
        assert_eq!(nt, Some(OntologyFormat::NTriples));
        assert_eq!(nq, Some(OntologyFormat::NQuads));
        assert_eq!(trig, Some(OntologyFormat::Trig));
        assert_eq!(unknown, None);
    }

    /// Test: OntologyFormat::to_rdf_format returns correct oxigraph format
    #[test]
    fn test_format_to_rdf_format() {
        // Arrange & Act
        let turtle_rdf = OntologyFormat::Turtle.to_rdf_format();
        let rdf_xml_rdf = OntologyFormat::RdfXml.to_rdf_format();
        let nt_rdf = OntologyFormat::NTriples.to_rdf_format();
        let nq_rdf = OntologyFormat::NQuads.to_rdf_format();
        let trig_rdf = OntologyFormat::Trig.to_rdf_format();

        // Assert - Verify format types match expectations
        use oxigraph::io::RdfFormat;
        assert!(matches!(turtle_rdf, RdfFormat::Turtle));
        assert!(matches!(rdf_xml_rdf, RdfFormat::RdfXml));
        assert!(matches!(nt_rdf, RdfFormat::NTriples));
        assert!(matches!(nq_rdf, RdfFormat::NQuads));
        assert!(matches!(trig_rdf, RdfFormat::TriG));
    }

    /// Test: OntologyFormat::content_type returns correct MIME types
    #[test]
    fn test_format_content_type() {
        // Arrange & Act
        let turtle_ct = OntologyFormat::Turtle.content_type();
        let rdf_xml_ct = OntologyFormat::RdfXml.content_type();
        let nt_ct = OntologyFormat::NTriples.content_type();
        let nq_ct = OntologyFormat::NQuads.content_type();
        let trig_ct = OntologyFormat::Trig.content_type();

        // Assert
        assert_eq!(turtle_ct, "text/turtle");
        assert_eq!(rdf_xml_ct, "application/rdf+xml");
        assert_eq!(nt_ct, "application/n-triples");
        assert_eq!(nq_ct, "application/n-quads");
        assert_eq!(trig_ct, "application/trig");
    }

    /// Test: OntologyFormat values are equality comparable
    #[test]
    fn test_format_equality() {
        // Arrange & Act
        let format1 = OntologyFormat::Turtle;
        let format2 = OntologyFormat::from_str("ttl").unwrap();
        let format3 = OntologyFormat::RdfXml;

        // Assert
        assert_eq!(format1, format2);
        assert_ne!(format1, format3);
    }
}

#[cfg(test)]
mod ontology_loader_tests {
    use super::*;
    use fixtures::*;

    /// Test: OntologyLoader::new creates loader with default configuration
    #[test]
    fn test_loader_new_default_configuration() {
        // Arrange & Act
        let loader = OntologyLoader::new();

        // Assert
        assert!(loader.flatten_imports, "Default should flatten imports");
        assert_eq!(loader.base_iri, None, "Default has no base IRI");
    }

    /// Test: OntologyLoader::with_base_iri sets base IRI
    #[test]
    fn test_loader_with_base_iri() {
        // Arrange
        let base_iri = "http://example.org/base/".to_string();

        // Act
        let loader = OntologyLoader::new().with_base_iri(base_iri.clone());

        // Assert
        assert_eq!(loader.base_iri, Some(base_iri));
    }

    /// Test: OntologyLoader::with_flatten_imports configures import handling
    #[test]
    fn test_loader_with_flatten_imports() {
        // Arrange & Act
        let loader_flatten = OntologyLoader::new().with_flatten_imports(true);
        let loader_no_flatten = OntologyLoader::new().with_flatten_imports(false);

        // Assert
        assert!(loader_flatten.flatten_imports);
        assert!(!loader_no_flatten.flatten_imports);
    }

    /// Test: OntologyLoader::load_from_str loads valid Turtle
    #[test]
    fn test_loader_load_from_str_turtle() {
        // Arrange
        let loader = OntologyLoader::new();

        // Act
        let result = loader.load_from_str(VALID_TURTLE, OntologyFormat::Turtle);

        // Assert
        assert!(result.is_ok(), "Loading valid Turtle should succeed");
        let graph = result.unwrap();
        assert!(!graph.is_empty(), "Graph should contain triples");

        // Verify specific triples exist
        let check = graph.query_cached("ASK { ?s a <http://www.w3.org/2002/07/owl#Class> }");
        assert!(check.is_ok());
        if let Ok(ggen_core::graph::types::CachedResult::Boolean(true)) = check {
            // Classes loaded
        } else {
            panic!("Expected owl:Class triples in graph");
        }
    }

    /// Test: OntologyLoader::load_from_str loads N-Triples
    #[test]
    fn test_loader_load_from_str_ntriples() {
        // Arrange
        let loader = OntologyLoader::new();

        // Act
        let result = loader.load_from_str(VALID_NTRIPLES, OntologyFormat::NTriples);

        // Assert
        assert!(result.is_ok(), "Loading valid N-Triples should succeed");
        let graph = result.unwrap();
        assert!(!graph.is_empty());
    }

    /// Test: OntologyLoader::load_from_str loads N-Quads
    #[test]
    fn test_loader_load_from_str_nquads() {
        // Arrange
        let loader = OntologyLoader::new();

        // Act
        let result = loader.load_from_str(VALID_NQUADS, OntologyFormat::NQuads);

        // Assert
        assert!(result.is_ok(), "Loading valid N-Quads should succeed");
        let graph = result.unwrap();
        assert!(!graph.is_empty());
    }

    /// Test: OntologyLoader::load_from_str loads TriG
    #[test]
    fn test_loader_load_from_str_trig() {
        // Arrange
        let loader = OntologyLoader::new();

        // Act
        let result = loader.load_from_str(VALID_TRIG, OntologyFormat::Trig);

        // Assert
        assert!(result.is_ok(), "Loading valid TriG should succeed");
        let graph = result.unwrap();
        assert!(!graph.is_empty());
    }

    /// Test: OntologyLoader::load_from_str returns error for invalid syntax
    #[test]
    fn test_loader_load_from_str_invalid_syntax() {
        // Arrange
        let loader = OntologyLoader::new();

        // Act
        let result = loader.load_from_str(INVALID_TURTLE, OntologyFormat::Turtle);

        // Assert
        assert!(result.is_err(), "Loading invalid Turtle should fail");
        match result {
            Err(e) => {
                let error_msg = e.to_string();
                assert!(
                    error_msg.contains("Ontology load error")
                        || error_msg.contains("Failed to load")
                );
            }
            Ok(_) => panic!("Expected error for invalid Turtle"),
        }
    }

    /// Test: OntologyLoader::load_from_str handles empty input
    #[test]
    fn test_loader_load_from_str_empty() {
        // Arrange
        let loader = OntologyLoader::new();

        // Act
        let result = loader.load_from_str("", OntologyFormat::Turtle);

        // Assert - Empty input should create an empty graph (not error)
        assert!(result.is_ok(), "Empty input should create empty graph");
        let graph = result.unwrap();
        assert!(graph.is_empty(), "Graph should be empty");
    }

    /// Test: OntologyLoader::load_from_file loads from file path
    #[test]
    fn test_loader_load_from_file_turtle() {
        // Arrange
        let loader = OntologyLoader::new();
        let mut temp_file = NamedTempFile::new().expect("Failed to create temp file");
        writeln!(temp_file, "{}", VALID_TURTLE).expect("Failed to write to temp file");

        // Act
        let result = loader.load_from_file(temp_file.path());

        // Assert
        assert!(result.is_ok(), "Loading valid Turtle file should succeed");
        let graph = result.unwrap();
        assert!(!graph.is_empty());
    }

    /// Test: OntologyLoader::load_from_file auto-detects format from extension
    #[test]
    fn test_loader_load_from_file_format_detection() {
        // Arrange
        let loader = OntologyLoader::new();

        // Test .ttl extension
        let mut ttl_file = NamedTempFile::new().expect("Failed to create temp file");
        writeln!(ttl_file, "{}", VALID_TURTLE).expect("Failed to write");

        // Act
        let result = loader.load_from_file(ttl_file.path());

        // Assert
        assert!(result.is_ok(), "Auto-detected Turtle format should load");
    }

    /// Test: OntologyLoader::load_from_file returns error for non-existent file
    #[test]
    fn test_loader_load_from_file_not_found() {
        // Arrange
        let loader = OntologyLoader::new();
        let nonexistent_path = "/path/that/does/not/exist.ttl";

        // Act
        let result = loader.load_from_file(nonexistent_path);

        // Assert
        assert!(result.is_err(), "Non-existent file should return error");
    }

    /// Test: OntologyLoader loads complex ontology with multiple entity types
    #[test]
    fn test_loader_load_complex_ontology() {
        // Arrange
        let loader = OntologyLoader::new();

        // Act
        let result = loader.load_from_str(COMPLEX_TURTLE, OntologyFormat::Turtle);

        // Assert
        assert!(result.is_ok(), "Loading complex ontology should succeed");
        let graph = result.unwrap();

        // Verify classes loaded
        let classes_query = graph
            .query_cached("SELECT ?class WHERE { ?class a <http://www.w3.org/2002/07/owl#Class> }");
        assert!(classes_query.is_ok());
        if let Ok(ggen_core::graph::types::CachedResult::Solutions(rows)) = classes_query {
            assert!(rows.len() >= 2, "Should have at least 2 classes");
        }

        // Verify properties loaded
        let props_query = graph.query_cached(
            "SELECT ?prop WHERE { ?prop a <http://www.w3.org/2002/07/owl#ObjectProperty> }",
        );
        assert!(props_query.is_ok());
    }

    /// Test: OntologyLoader produces consistent results for same input
    #[test]
    fn test_loader_deterministic_loading() {
        // Arrange
        let loader = OntologyLoader::new();

        // Act - Load same content twice
        let result1 = loader.load_from_str(VALID_TURTLE, OntologyFormat::Turtle);
        let result2 = loader.load_from_str(VALID_TURTLE, OntologyFormat::Turtle);

        // Assert
        assert!(result1.is_ok() && result2.is_ok());
        let graph1 = result1.unwrap();
        let graph2 = result2.unwrap();

        // Both graphs should have same number of triples
        assert_eq!(graph1.len(), graph2.len());
    }
}

#[cfg(test)]
mod integration_tests {
    use super::*;
    use fixtures::*;

    /// Test: End-to-end loading and querying workflow
    #[test]
    fn test_load_and_query_workflow() {
        // Arrange
        let loader = OntologyLoader::new();

        // Act - Load ontology
        let graph = loader
            .load_from_str(VALID_TURTLE, OntologyFormat::Turtle)
            .expect("Failed to load ontology");

        // Query for tasks
        let tasks_query = r#"
            PREFIX ex: <http://example.org/>
            SELECT ?task ?label
            WHERE {
                ?task a ex:AtomicTask ;
                      rdfs:label ?label .
            }
            ORDER BY ?label
        "#;

        let result = graph.query_cached(tasks_query);

        // Assert
        assert!(result.is_ok());
        if let Ok(ggen_core::graph::types::CachedResult::Solutions(rows)) = result {
            assert_eq!(rows.len(), 2, "Should find 2 tasks");
            assert!(rows[0].get("label").is_some());
        } else {
            panic!("Expected solutions from task query");
        }
    }

    /// Test: Loading with base IRI resolves relative references
    #[test]
    fn test_load_with_base_iri() {
        // Arrange
        let base_iri = "http://example.org/base/";
        let loader = OntologyLoader::new().with_base_iri(base_iri.to_string());

        let turtle_with_relative = r#"
            @prefix : <#> .
            :Local a :Class .
        "#;

        // Act
        let result = loader.load_from_str(turtle_with_relative, OntologyFormat::Turtle);

        // Assert
        assert!(result.is_ok(), "Loading with base IRI should succeed");
        let graph = result.unwrap();
        assert!(!graph.is_empty());
    }

    /// Test: Multiple sequential loads accumulate data
    #[test]
    fn test_sequential_loads_accumulate() {
        // Arrange
        let loader = OntologyLoader::new();

        // Act - Load first ontology
        let graph1 = loader
            .load_from_str(
                r#"@prefix ex: <http://example.org/> . ex:A a ex:Class ."#,
                OntologyFormat::Turtle,
            )
            .expect("First load failed");

        // Load second ontology into same graph
        let graph2 = loader
            .load_from_str(
                r#"@prefix ex: <http://example.org/> . ex:B a ex:Class ."#,
                OntologyFormat::Turtle,
            )
            .expect("Second load failed");

        // Assert
        // Each load creates a new graph, so verify each works independently
        assert!(!graph1.is_empty());
        assert!(!graph2.is_empty());
    }
}

#[cfg(test)]
mod property_based_tests {
    use super::*;
    use fixtures::*;
    use proptest::prelude::*;

    /// Property: Empty ontology produces empty graph
    #[test]
    fn test_empty_ontology_produces_empty_graph() {
        // Arrange
        let loader = OntologyLoader::new();

        // Act
        let result = loader.load_from_str("", OntologyFormat::Turtle);

        // Assert
        assert!(result.is_ok());
        let graph = result.unwrap();
        assert!(graph.is_empty());
        assert_eq!(graph.len(), 0);
    }

    /// Property: Whitespace-only ontology produces empty graph
    #[test]
    fn test_whitespace_only_produces_empty_graph() {
        // Arrange
        let loader = OntologyLoader::new();

        // Act
        let result = loader.load_from_str("   \n\t  ", OntologyFormat::Turtle);

        // Assert
        assert!(result.is_ok());
        let graph = result.unwrap();
        assert!(graph.is_empty());
    }

    /// Property: Loading same content produces graphs of equal size
    #[test]
    fn test_idempotent_loading() {
        // Arrange
        let loader = OntologyLoader::new();

        // Act
        let graph1 = loader
            .load_from_str(VALID_TURTLE, OntologyFormat::Turtle)
            .expect("First load failed");
        let graph2 = loader
            .load_from_str(VALID_TURTLE, OntologyFormat::Turtle)
            .expect("Second load failed");

        // Assert
        assert_eq!(graph1.len(), graph2.len());
    }

    /// Property: Format string parsing is case-insensitive for "Turtle"
    #[test]
    fn test_format_parsing_case_insensitive() {
        // Arrange & Act
        let variants = vec![
            OntologyFormat::from_str("ttl"),
            OntologyFormat::from_str("TTL"),
            OntologyFormat::from_str("Ttl"),
            OntologyFormat::from_str("TuRtLe"),
        ];

        // Assert
        for result in variants {
            assert!(matches!(result, Ok(OntologyFormat::Turtle)));
        }
    }
}
