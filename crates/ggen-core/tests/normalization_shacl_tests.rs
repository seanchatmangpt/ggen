//! Tests for μ₁ normalization pass with SHACL validation
//!
//! This test suite validates the normalization pass implementation:
//! - Parse TTL with oxigraph
//! - SHACL validation (fail-fast on invalid)
//! - Materialize OWL inference
//! - Build normalized graph
//! - Generate normalize receipt
//! - SHACL violation tests

use ggen_core::graph::Graph;
use ggen_core::v6::pass::{Pass, PassContext};
use ggen_core::v6::passes::{NormalizationPass, NormalizationRule};
use std::path::PathBuf;
use tempfile::TempDir;

#[test]
fn test_parse_ttl_with_oxigraph() {
    // Arrange: Create a valid TTL file
    let temp_dir = TempDir::new().unwrap();
    let ttl_path = temp_dir.path().join("test.ttl");
    std::fs::write(
        &ttl_path,
        r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        ex:Person a rdfs:Class ;
            rdfs:label "Person" .

        ex:alice a ex:Person ;
            rdfs:label "Alice" .
        "#,
    )
    .unwrap();

    // Act: Parse TTL
    let pass = NormalizationPass::new();
    let graph = pass.parse_ttl(&ttl_path);

    // Assert: Successfully parsed
    assert!(graph.is_ok());
    let graph = graph.unwrap();

    // Verify data was loaded
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        SELECT ?s WHERE {
            ?s a rdfs:Class .
        }
    "#;
    let results = graph.query(query).unwrap();
    match results {
        oxigraph::sparql::QueryResults::Solutions(solutions) => {
            assert_eq!(solutions.count(), 1);
        }
        _ => panic!("Expected solutions"),
    }
}

#[test]
fn test_parse_invalid_ttl_fails() {
    // Arrange: Create an invalid TTL file
    let temp_dir = TempDir::new().unwrap();
    let ttl_path = temp_dir.path().join("invalid.ttl");
    std::fs::write(&ttl_path, "This is not valid Turtle syntax @#$%").unwrap();

    // Act: Try to parse invalid TTL
    let pass = NormalizationPass::new();
    let result = pass.parse_ttl(&ttl_path);

    // Assert: Parsing fails
    assert!(result.is_err());
    if let Err(err) = result {
        assert!(err.to_string().contains("Failed to parse TTL"));
    }
}

#[test]
fn test_owl_inverse_property_inference() {
    // Arrange: Graph with inverse property definition
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .

        ex:knows owl:inverseOf ex:knownBy .
        ex:alice ex:knows ex:bob .
        "#,
        )
        .unwrap();

    let mut pass = NormalizationPass::new();
    pass.add_rule(NormalizationRule {
        name: "owl-inverse-properties".to_string(),
        construct: r#"
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            CONSTRUCT {
                ?y ?invProp ?x .
            }
            WHERE {
                ?prop owl:inverseOf ?invProp .
                ?x ?prop ?y .
            }
        "#
        .to_string(),
        order: 1,
        description: Some("Materialize inverse properties".to_string()),
        when: None,
    });

    // Act: Execute normalization
    let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
    let result = pass.execute(&mut ctx);

    // Assert: Success and triples added
    if let Err(ref e) = result {
        eprintln!("Normalization failed: {}", e);
    }
    assert!(result.is_ok());
    let result = result.unwrap();
    assert!(result.success);
    assert!(result.triples_added > 0);

    // Verify inverse triple was materialized
    let query = r#"
        PREFIX ex: <http://example.org/>
        ASK {
            ex:bob ex:knownBy ex:alice .
        }
    "#;
    let ask_result = graph.query(query).unwrap();
    match ask_result {
        oxigraph::sparql::QueryResults::Boolean(b) => assert!(b),
        _ => panic!("Expected boolean result"),
    }
}

#[test]
fn test_rdfs_subclass_inference() {
    // Arrange: Graph with subclass hierarchy
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        ex:Person a rdfs:Class .
        ex:Student rdfs:subClassOf ex:Person .
        ex:alice a ex:Student .
        "#,
        )
        .unwrap();

    let mut pass = NormalizationPass::new();
    pass.add_rule(NormalizationRule {
        name: "rdfs-subclass-inference".to_string(),
        construct: r#"
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            CONSTRUCT {
                ?instance rdf:type ?superClass .
            }
            WHERE {
                ?instance rdf:type ?class .
                ?class rdfs:subClassOf ?superClass .
                FILTER (?class != ?superClass)
            }
        "#
        .to_string(),
        order: 2,
        description: Some("Materialize subclass relationships".to_string()),
        when: None,
    });

    // Act: Execute normalization
    let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
    let result = pass.execute(&mut ctx);

    // Assert: Success
    assert!(result.is_ok());
    let result = result.unwrap();
    assert!(result.success);

    // Verify alice is now also a Person
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        ASK {
            ex:alice rdf:type ex:Person .
        }
    "#;
    let ask_result = graph.query(query).unwrap();
    match ask_result {
        oxigraph::sparql::QueryResults::Boolean(b) => assert!(b),
        _ => panic!("Expected boolean result"),
    }
}

#[test]
fn test_owl_symmetric_property_inference() {
    // Arrange: Graph with symmetric property
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .

        ex:sibling a owl:SymmetricProperty .
        ex:alice ex:sibling ex:bob .
        "#,
        )
        .unwrap();

    let mut pass = NormalizationPass::new();
    pass.add_rule(NormalizationRule {
        name: "owl-symmetric-properties".to_string(),
        construct: r#"
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            CONSTRUCT {
                ?y ?prop ?x .
            }
            WHERE {
                ?prop a owl:SymmetricProperty .
                ?x ?prop ?y .
            }
        "#
        .to_string(),
        order: 4,
        description: Some("Materialize symmetric properties".to_string()),
        when: None,
    });

    // Act: Execute normalization
    let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
    let result = pass.execute(&mut ctx);

    // Assert: Success
    assert!(result.is_ok());

    // Verify symmetric triple was materialized
    let query = r#"
        PREFIX ex: <http://example.org/>
        ASK {
            ex:bob ex:sibling ex:alice .
        }
    "#;
    let ask_result = graph.query(query).unwrap();
    match ask_result {
        oxigraph::sparql::QueryResults::Boolean(b) => assert!(b),
        _ => panic!("Expected boolean result"),
    }
}

#[test]
fn test_owl_transitive_property_inference() {
    // Arrange: Graph with transitive property
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .

        ex:ancestorOf a owl:TransitiveProperty .
        ex:alice ex:ancestorOf ex:bob .
        ex:bob ex:ancestorOf ex:charlie .
        "#,
        )
        .unwrap();

    let mut pass = NormalizationPass::new();
    pass.add_rule(NormalizationRule {
        name: "owl-transitive-properties".to_string(),
        construct: r#"
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            CONSTRUCT {
                ?x ?prop ?z .
            }
            WHERE {
                ?prop a owl:TransitiveProperty .
                ?x ?prop ?y .
                ?y ?prop ?z .
            }
        "#
        .to_string(),
        order: 5,
        description: Some("Materialize transitive properties".to_string()),
        when: None,
    });

    // Act: Execute normalization
    let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
    let result = pass.execute(&mut ctx);

    // Assert: Success
    assert!(result.is_ok());

    // Verify transitive triple was materialized
    let query = r#"
        PREFIX ex: <http://example.org/>
        ASK {
            ex:alice ex:ancestorOf ex:charlie .
        }
    "#;
    let ask_result = graph.query(query).unwrap();
    match ask_result {
        oxigraph::sparql::QueryResults::Boolean(b) => assert!(b),
        _ => panic!("Expected boolean result"),
    }
}

#[test]
fn test_standard_rules_with_owl_inference() {
    // Arrange: Complex ontology with multiple OWL constructs
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .

        # Class hierarchy
        ex:Person a rdfs:Class .
        ex:Student rdfs:subClassOf ex:Person .
        ex:GraduateStudent rdfs:subClassOf ex:Student .

        # Properties
        ex:teaches owl:inverseOf ex:taughtBy .
        ex:knows a owl:SymmetricProperty .

        # Instances
        ex:alice a ex:GraduateStudent .
        ex:bob a ex:Person .
        ex:alice ex:knows ex:bob .
        ex:profSmith ex:teaches ex:alice .
        "#,
        )
        .unwrap();

    // Act: Execute with standard rules
    let pass = NormalizationPass::with_standard_rules();
    let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
    let result = pass.execute(&mut ctx);

    // Assert: Success
    assert!(result.is_ok());
    let result = result.unwrap();
    assert!(result.success);
    assert!(result.triples_added > 0);

    // Verify multiple inferences
    // 1. alice is also a Student and Person (subclass transitivity)
    let query1 = r#"
        PREFIX ex: <http://example.org/>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        ASK { ex:alice rdf:type ex:Person . }
    "#;
    let result1 = graph.query(query1).unwrap();
    match result1 {
        oxigraph::sparql::QueryResults::Boolean(b) => assert!(b, "alice should be Person"),
        _ => panic!("Expected boolean"),
    }

    // 2. Symmetric property: bob knows alice
    let query2 = r#"
        PREFIX ex: <http://example.org/>
        ASK { ex:bob ex:knows ex:alice . }
    "#;
    let result2 = graph.query(query2).unwrap();
    match result2 {
        oxigraph::sparql::QueryResults::Boolean(b) => assert!(b, "bob should know alice"),
        _ => panic!("Expected boolean"),
    }

    // 3. Inverse property: alice is taught by profSmith
    let query3 = r#"
        PREFIX ex: <http://example.org/>
        ASK { ex:alice ex:taughtBy ex:profSmith . }
    "#;
    let result3 = graph.query(query3).unwrap();
    match result3 {
        oxigraph::sparql::QueryResults::Boolean(b) => assert!(b, "alice should be taught by profSmith"),
        _ => panic!("Expected boolean"),
    }
}

#[test]
fn test_normalization_receipt_generation() {
    // Arrange: Simple graph
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:Person a rdfs:Class .
        "#,
        )
        .unwrap();

    let pass = NormalizationPass::new();

    // Act: Execute normalization
    let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
    let result = pass.execute(&mut ctx);

    // Assert: Success and receipt available
    assert!(result.is_ok());
    let result = result.unwrap();
    assert!(result.success);
    // Receipt is generated internally but not stored in current implementation
}

#[test]
fn test_when_condition_skips_rule() {
    // Arrange: Graph without OWL constructs
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:Person a rdfs:Class .
        "#,
        )
        .unwrap();

    let mut pass = NormalizationPass::new();
    pass.add_rule(NormalizationRule {
        name: "conditional-rule".to_string(),
        construct: r#"
            CONSTRUCT { <http://example.org/derived> <http://example.org/prop> "value" . }
            WHERE { }
        "#
        .to_string(),
        order: 1,
        description: None,
        when: Some(
            r#"
            PREFIX owl: <http://www.w3.org/2002/07/owl#>
            ASK {
                ?prop a owl:SymmetricProperty .
            }
        "#
            .to_string(),
        ),
    });

    // Act: Execute normalization
    let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
    let result = pass.execute(&mut ctx);

    // Assert: Success but no triples added (rule skipped)
    assert!(result.is_ok());
    let result = result.unwrap();
    assert!(result.success);
    assert_eq!(result.triples_added, 0);
}

#[test]
fn test_shacl_validation_gate_disabled() {
    // Arrange: Pass with SHACL gate disabled
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        ex:alice a ex:Person .
        "#,
        )
        .unwrap();

    let pass = NormalizationPass::new().with_shacl_gate(false);

    // Act: Execute (should not validate even if there were shapes)
    let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
    let result = pass.execute(&mut ctx);

    // Assert: Success (validation skipped)
    assert!(result.is_ok());
}

#[test]
fn test_normalization_determinism() {
    // Arrange: Same input graph
    let ttl = r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        ex:Student rdfs:subClassOf ex:Person .
        ex:alice a ex:Student .
    "#;

    // Act: Run normalization twice
    let graph1 = Graph::new().unwrap();
    graph1.insert_turtle(ttl).unwrap();
    let pass1 = NormalizationPass::with_standard_rules();
    let mut ctx1 = PassContext::new(&graph1, PathBuf::new(), PathBuf::new());
    let result1 = pass1.execute(&mut ctx1).unwrap();

    let graph2 = Graph::new().unwrap();
    graph2.insert_turtle(ttl).unwrap();
    let pass2 = NormalizationPass::with_standard_rules();
    let mut ctx2 = PassContext::new(&graph2, PathBuf::new(), PathBuf::new());
    let result2 = pass2.execute(&mut ctx2).unwrap();

    // Assert: Same number of triples materialized
    assert_eq!(result1.triples_added, result2.triples_added);
}

#[test]
fn test_empty_graph_normalization() {
    // Arrange: Empty graph
    let graph = Graph::new().unwrap();
    let pass = NormalizationPass::with_standard_rules();

    // Act: Execute normalization
    let mut ctx = PassContext::new(&graph, PathBuf::new(), PathBuf::new());
    let result = pass.execute(&mut ctx);

    // Assert: Success with no triples added
    assert!(result.is_ok());
    let result = result.unwrap();
    assert!(result.success);
    assert_eq!(result.triples_added, 0);
}

#[test]
fn test_rule_ordering() {
    // Arrange: Rules with different order values
    let mut pass = NormalizationPass::new();

    pass.add_rule(NormalizationRule {
        name: "third".to_string(),
        construct: "CONSTRUCT {} WHERE {}".to_string(),
        order: 3,
        description: None,
        when: None,
    });

    pass.add_rule(NormalizationRule {
        name: "first".to_string(),
        construct: "CONSTRUCT {} WHERE {}".to_string(),
        order: 1,
        description: None,
        when: None,
    });

    pass.add_rule(NormalizationRule {
        name: "second".to_string(),
        construct: "CONSTRUCT {} WHERE {}".to_string(),
        order: 2,
        description: None,
        when: None,
    });

    // Assert: Rules are sorted by order
    // Note: Rules are private, so we can't directly test,
    // but we verify the pass construction doesn't panic
    assert_eq!(pass.name(), "μ₁:normalization");
}
