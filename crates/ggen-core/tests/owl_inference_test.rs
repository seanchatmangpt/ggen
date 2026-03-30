//! OWL/RDFS inference rule tests for the NormalizationPass standard rules.
//!
//! Tests the 6 OWL/RDFS inference rules defined in
//! `NormalizationPass::with_standard_rules()` by extracting the CONSTRUCT queries
//! and executing them directly via `ConstructExecutor::execute_and_materialize()`.
//!
//! Chicago TDD: Black-box behavior verification with real Graph instances.
//! Each test is independent (creates its own graph) and verifies observable
//! behavior via ASK/SELECT queries after materialization.

use ggen_core::graph::{CachedResult, ConstructExecutor, Graph};
use std::collections::BTreeMap;

/// Helper: run an ASK query and return the boolean result.
fn ask(graph: &Graph, sparql: &str) -> bool {
    match graph.query_cached(sparql).expect("ASK query should succeed") {
        CachedResult::Boolean(b) => b,
        other => panic!(
            "Expected CachedResult::Boolean from ASK, got {:?}",
            std::mem::discriminant(&other)
        ),
    }
}

/// Helper: run a SELECT query and return the solution rows.
fn select(graph: &Graph, sparql: &str) -> Vec<BTreeMap<String, String>> {
    match graph.query_cached(sparql).expect("SELECT query should succeed") {
        CachedResult::Solutions(rows) => rows,
        other => panic!(
            "Expected CachedResult::Solutions from SELECT, got {:?}",
            std::mem::discriminant(&other)
        ),
    }
}

// ---------------------------------------------------------------------------
// CONSTRUCT query strings extracted from NormalizationPass::with_standard_rules()
// ---------------------------------------------------------------------------

const RULE_INVERSE_PROPERTIES: &str = r#"
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    CONSTRUCT {
        ?y ?invProp ?x .
    }
    WHERE {
        ?prop owl:inverseOf ?invProp .
        ?x ?prop ?y .
    }
"#;

const RULE_SUBCLASS_INFERENCE: &str = r#"
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
"#;

const RULE_DOMAIN_RANGE_INFERENCE: &str = r#"
    PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    CONSTRUCT {
        ?subject rdf:type ?domainClass .
        ?object rdf:type ?rangeClass .
    }
    WHERE {
        ?subject ?prop ?object .
        OPTIONAL { ?prop rdfs:domain ?domainClass . }
        OPTIONAL { ?prop rdfs:range ?rangeClass . }
        FILTER (isIRI(?object))
    }
"#;

const RULE_SYMMETRIC_PROPERTIES: &str = r#"
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    CONSTRUCT {
        ?y ?prop ?x .
    }
    WHERE {
        ?prop a owl:SymmetricProperty .
        ?x ?prop ?y .
    }
"#;

const RULE_TRANSITIVE_PROPERTIES: &str = r#"
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    CONSTRUCT {
        ?x ?prop ?z .
    }
    WHERE {
        ?prop a owl:TransitiveProperty .
        ?x ?prop ?y .
        ?y ?prop ?z .
    }
"#;

const RULE_EQUIVALENT_CLASSES: &str = r#"
    PREFIX owl: <http://www.w3.org/2002/07/owl#>
    PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
    CONSTRUCT {
        ?instance rdf:type ?equivClass .
    }
    WHERE {
        ?instance rdf:type ?class .
        ?class owl:equivalentClass ?equivClass .
        FILTER (?class != ?equivClass)
    }
"#;

// ---------------------------------------------------------------------------
// Test 1: Subclass inference materializes transitive types
// ---------------------------------------------------------------------------

#[test]
fn test_subclass_inference_materializes_transitive_types() {
    // Arrange: alice is a Student, Student is a subClassOf Person
    let graph = Graph::new().expect("Graph::new should succeed");
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        ex:Student rdfs:subClassOf ex:Person .
        ex:alice rdf:type ex:Student .
    "#,
        )
        .expect("insert_turtle should succeed");
    let executor = ConstructExecutor::new(&graph);

    // Act: run the subclass inference rule
    let count = executor
        .execute_and_materialize(RULE_SUBCLASS_INFERENCE)
        .expect("subclass rule should succeed");

    // Assert: exactly 1 triple inferred (alice rdf:type Person)
    assert_eq!(
        count, 1,
        "Subclass inference should materialize exactly 1 triple, got {}",
        count
    );

    // Verify via ASK that alice is now typed as Person
    let has_person_type = ask(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        ASK { ex:alice rdf:type ex:Person }
        "#,
    );
    assert!(
        has_person_type,
        "alice should be inferred as rdf:type ex:Person"
    );
}

// ---------------------------------------------------------------------------
// Test 2: Inverse properties produce both directions
// ---------------------------------------------------------------------------

#[test]
fn test_inverse_properties_produce_both_directions() {
    // Arrange: partOf is inverseOf hasPart; wheel partOf car
    let graph = Graph::new().expect("Graph::new should succeed");
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .

        ex:partOf owl:inverseOf ex:hasPart .
        ex:wheel ex:partOf ex:car .
    "#,
        )
        .expect("insert_turtle should succeed");
    let executor = ConstructExecutor::new(&graph);

    // Act: run the inverse properties rule
    let count = executor
        .execute_and_materialize(RULE_INVERSE_PROPERTIES)
        .expect("inverse rule should succeed");

    // Assert: 1 triple inferred (car hasPart wheel)
    assert_eq!(
        count, 1,
        "Inverse property rule should materialize exactly 1 triple, got {}",
        count
    );

    // Verify the inverse direction is materialized
    let has_inverse = ask(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        ASK { ex:car ex:hasPart ex:wheel }
        "#,
    );
    assert!(
        has_inverse,
        "ex:car ex:hasPart ex:wheel should be materialized via inverseOf"
    );
}

// ---------------------------------------------------------------------------
// Test 3: Symmetric properties work both ways
// ---------------------------------------------------------------------------

#[test]
fn test_symmetric_properties_work_both_ways() {
    // Arrange: knows is SymmetricProperty; alice knows bob
    let graph = Graph::new().expect("Graph::new should succeed");
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        ex:knows rdf:type owl:SymmetricProperty .
        ex:alice ex:knows ex:bob .
    "#,
        )
        .expect("insert_turtle should succeed");
    let executor = ConstructExecutor::new(&graph);

    // Act: run the symmetric properties rule
    let count = executor
        .execute_and_materialize(RULE_SYMMETRIC_PROPERTIES)
        .expect("symmetric rule should succeed");

    // Assert: 1 triple inferred (bob knows alice)
    assert_eq!(
        count, 1,
        "Symmetric rule should materialize exactly 1 triple, got {}",
        count
    );

    // Verify the reverse direction is materialized
    let has_reverse = ask(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        ASK { ex:bob ex:knows ex:alice }
        "#,
    );
    assert!(
        has_reverse,
        "ex:bob ex:knows ex:alice should be materialized via SymmetricProperty"
    );
}

// ---------------------------------------------------------------------------
// Test 4: Transitive properties chain through multiple hops
// ---------------------------------------------------------------------------

#[test]
fn test_transitive_properties_chain_through_multiple_hops() {
    // Arrange: locatedIn is TransitiveProperty; office locatedIn building locatedIn campus
    let graph = Graph::new().expect("Graph::new should succeed");
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        ex:locatedIn rdf:type owl:TransitiveProperty .
        ex:office ex:locatedIn ex:building .
        ex:building ex:locatedIn ex:campus .
    "#,
        )
        .expect("insert_turtle should succeed");
    let executor = ConstructExecutor::new(&graph);

    // Act: run the transitive properties rule
    let count = executor
        .execute_and_materialize(RULE_TRANSITIVE_PROPERTIES)
        .expect("transitive rule should succeed");

    // Assert: 1 triple inferred (office locatedIn campus)
    assert_eq!(
        count, 1,
        "Transitive rule should materialize exactly 1 triple, got {}",
        count
    );

    // Verify the transitive link is materialized
    let has_transitive = ask(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        ASK { ex:office ex:locatedIn ex:campus }
        "#,
    );
    assert!(
        has_transitive,
        "ex:office ex:locatedIn ex:campus should be materialized via TransitiveProperty"
    );
}

// ---------------------------------------------------------------------------
// Test 5: Domain inference from property usage
// ---------------------------------------------------------------------------

#[test]
fn test_domain_inference_from_property_usage() {
    // Arrange: hasMother has domain Person; alice hasMother bob
    let graph = Graph::new().expect("Graph::new should succeed");
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:hasMother rdfs:domain ex:Person .
        ex:alice ex:hasMother ex:bob .
    "#,
        )
        .expect("insert_turtle should succeed");
    let executor = ConstructExecutor::new(&graph);

    // Act: run the domain/range inference rule
    let count = executor
        .execute_and_materialize(RULE_DOMAIN_RANGE_INFERENCE)
        .expect("domain/range rule should succeed");

    // Assert: at least 1 triple inferred (alice rdf:type Person from domain)
    assert!(
        count >= 1,
        "Domain/range rule should materialize at least 1 triple, got {}",
        count
    );

    // Verify domain inference: alice should be typed as Person
    let has_domain_type = ask(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        ASK { ex:alice rdf:type ex:Person }
        "#,
    );
    assert!(
        has_domain_type,
        "ex:alice should be inferred as rdf:type ex:Person via rdfs:domain"
    );
}

// ---------------------------------------------------------------------------
// Test 6: Range inference from property usage
// ---------------------------------------------------------------------------

#[test]
fn test_range_inference_from_property_usage() {
    // Arrange: hasMother has range Person; alice hasMother bob
    let graph = Graph::new().expect("Graph::new should succeed");
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:hasMother rdfs:range ex:Person .
        ex:alice ex:hasMother ex:bob .
    "#,
        )
        .expect("insert_turtle should succeed");
    let executor = ConstructExecutor::new(&graph);

    // Act: run the domain/range inference rule
    let count = executor
        .execute_and_materialize(RULE_DOMAIN_RANGE_INFERENCE)
        .expect("domain/range rule should succeed");

    // Assert: at least 1 triple inferred (bob rdf:type Person from range)
    assert!(
        count >= 1,
        "Domain/range rule should materialize at least 1 triple, got {}",
        count
    );

    // Verify range inference: bob should be typed as Person
    let has_range_type = ask(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        ASK { ex:bob rdf:type ex:Person }
        "#,
    );
    assert!(
        has_range_type,
        "ex:bob should be inferred as rdf:type ex:Person via rdfs:range"
    );
}

// ---------------------------------------------------------------------------
// Test 7: Equivalent classes propagate membership
// ---------------------------------------------------------------------------

#[test]
fn test_equivalent_classes_propagate_membership() {
    // Arrange: Human is equivalentClass of Person; alice is Human
    let graph = Graph::new().expect("Graph::new should succeed");
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        ex:Human owl:equivalentClass ex:Person .
        ex:alice rdf:type ex:Human .
    "#,
        )
        .expect("insert_turtle should succeed");
    let executor = ConstructExecutor::new(&graph);

    // Act: run the equivalent classes rule
    let count = executor
        .execute_and_materialize(RULE_EQUIVALENT_CLASSES)
        .expect("equivalent classes rule should succeed");

    // Assert: exactly 1 triple inferred (alice rdf:type Person)
    assert_eq!(
        count, 1,
        "Equivalent classes rule should materialize exactly 1 triple, got {}",
        count
    );

    // Verify alice is now typed as Person
    let has_equiv_type = ask(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        ASK { ex:alice rdf:type ex:Person }
        "#,
    );
    assert!(
        has_equiv_type,
        "ex:alice should be inferred as rdf:type ex:Person via owl:equivalentClass"
    );
}

// ---------------------------------------------------------------------------
// Test 8: No inference on empty graph
// ---------------------------------------------------------------------------

#[test]
fn test_no_inference_on_empty_graph() {
    // Arrange: empty graph
    let graph = Graph::new().expect("Graph::new should succeed");
    assert!(graph.is_empty(), "Graph should start empty");
    let executor = ConstructExecutor::new(&graph);

    let all_rules = [
        RULE_INVERSE_PROPERTIES,
        RULE_SUBCLASS_INFERENCE,
        RULE_DOMAIN_RANGE_INFERENCE,
        RULE_SYMMETRIC_PROPERTIES,
        RULE_TRANSITIVE_PROPERTIES,
        RULE_EQUIVALENT_CLASSES,
    ];

    // Act: run every rule against the empty graph
    let mut total_inferred = 0;
    for rule in &all_rules {
        let count = executor
            .execute_and_materialize(rule)
            .expect("Rule should not error on empty graph");
        assert_eq!(
            count, 0,
            "Rule should produce 0 triples on empty graph, got {}",
            count
        );
        total_inferred += count;
    }

    // Assert: zero triples added across all rules
    assert_eq!(
        total_inferred, 0,
        "No triples should be inferred from an empty graph"
    );
    assert!(
        graph.is_empty(),
        "Graph should remain empty after running all inference rules"
    );
}

// ---------------------------------------------------------------------------
// Test 9: Subclass inference with multi-level hierarchy
// ---------------------------------------------------------------------------

#[test]
fn test_subclass_inference_multi_level_hierarchy() {
    // Arrange: GradStudent subClassOf Student subClassOf Person
    let graph = Graph::new().expect("Graph::new should succeed");
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        ex:GradStudent rdfs:subClassOf ex:Student .
        ex:Student rdfs:subClassOf ex:Person .
        ex:alice rdf:type ex:GradStudent .
    "#,
        )
        .expect("insert_turtle should succeed");
    let executor = ConstructExecutor::new(&graph);

    // Act: first pass materializes GradStudent -> Student, Student -> Person
    let count1 = executor
        .execute_and_materialize(RULE_SUBCLASS_INFERENCE)
        .expect("subclass rule pass 1 should succeed");

    // After pass 1: alice should have type Student (direct subclass link)
    assert!(
        count1 >= 1,
        "Pass 1 should infer at least 1 triple, got {}",
        count1
    );

    // Second pass: now alice has type Student, Student subClassOf Person
    // should materialize alice rdf:type Person
    let count2 = executor
        .execute_and_materialize(RULE_SUBCLASS_INFERENCE)
        .expect("subclass rule pass 2 should succeed");

    assert!(
        count2 >= 1,
        "Pass 2 should infer at least 1 triple (transitive), got {}",
        count2
    );

    // Verify alice has both Student and Person types
    let rows = select(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        SELECT ?type WHERE { ex:alice rdf:type ?type }
        "#,
    );

    let types: Vec<&str> = rows
        .iter()
        .filter_map(|r| r.get("type").map(|s| s.as_str()))
        .collect();
    assert!(
        types.iter().any(|t| t.contains("Student")),
        "alice should have type Student after inference. Got: {:?}",
        types
    );
    assert!(
        types.iter().any(|t| t.contains("Person")),
        "alice should have type Person after inference. Got: {:?}",
        types
    );
}

// ---------------------------------------------------------------------------
// Test 10: Inverse properties with multiple instances
// ---------------------------------------------------------------------------

#[test]
fn test_inverse_properties_with_multiple_instances() {
    // Arrange: partOf inverseOf hasPart; multiple partOf triples
    let graph = Graph::new().expect("Graph::new should succeed");
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .

        ex:partOf owl:inverseOf ex:hasPart .
        ex:wheel ex:partOf ex:car .
        ex:engine ex:partOf ex:car .
    "#,
        )
        .expect("insert_turtle should succeed");
    let executor = ConstructExecutor::new(&graph);

    // Act
    let count = executor
        .execute_and_materialize(RULE_INVERSE_PROPERTIES)
        .expect("inverse rule should succeed");

    // Assert: 2 triples inferred (car hasPart wheel, car hasPart engine)
    assert_eq!(
        count, 2,
        "Inverse rule should materialize 2 triples, got {}",
        count
    );

    // Verify both inverse directions
    assert!(ask(
        &graph,
        r#"PREFIX ex: <http://example.org/> ASK { ex:car ex:hasPart ex:wheel }"#
    ));
    assert!(ask(
        &graph,
        r#"PREFIX ex: <http://example.org/> ASK { ex:car ex:hasPart ex:engine }"#
    ));
}

// ---------------------------------------------------------------------------
// Test 11: Symmetric property is idempotent (re-running does not duplicate)
// ---------------------------------------------------------------------------

#[test]
fn test_symmetric_property_idempotent() {
    // Arrange
    let graph = Graph::new().expect("Graph::new should succeed");
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        ex:knows rdf:type owl:SymmetricProperty .
        ex:alice ex:knows ex:bob .
    "#,
        )
        .expect("insert_turtle should succeed");
    let executor = ConstructExecutor::new(&graph);

    let initial_len = graph.len();

    // Act: run twice
    let count1 = executor
        .execute_and_materialize(RULE_SYMMETRIC_PROPERTIES)
        .expect("symmetric pass 1 should succeed");
    assert!(count1 >= 1, "First pass should produce at least 1 triple");

    let _count2 = executor
        .execute_and_materialize(RULE_SYMMETRIC_PROPERTIES)
        .expect("symmetric pass 2 should succeed");

    // The CONSTRUCT query may produce triples that already exist (Oxigraph deduplicates
    // on insert). The graph should not grow on the second pass.
    let len_after_second = graph.len();
    assert_eq!(
        len_after_second, initial_len + 1,
        "Graph should grow by exactly 1 total after idempotent rule (initial_len={}, after={})",
        initial_len, len_after_second
    );
}

// ---------------------------------------------------------------------------
// Test 12: Domain/range inference with literal objects is filtered out
// ---------------------------------------------------------------------------

#[test]
fn test_domain_range_skips_literal_objects() {
    // Arrange: age has domain Person and range xsd:integer; alice age "30"
    // The domain/range rule has FILTER(isIRI(?object)), so literals should be skipped
    let graph = Graph::new().expect("Graph::new should succeed");
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ex:age rdfs:domain ex:Person .
        ex:age rdfs:range xsd:integer .
        ex:alice ex:age "30"^^xsd:integer .
    "#,
        )
        .expect("insert_turtle should succeed");
    let executor = ConstructExecutor::new(&graph);

    // Act: the rule should NOT fire because "30"^^xsd:integer is a literal, not an IRI
    let count = executor
        .execute_and_materialize(RULE_DOMAIN_RANGE_INFERENCE)
        .expect("domain/range rule should succeed");

    // Assert: the rule's FILTER(isIRI(?object)) excludes literal objects
    // So alice does NOT get rdf:type Person via this triple
    // (Note: whether count is 0 depends on the OPTIONAL semantics --
    //  the CONSTRUCT template only emits if the OPTIONALs bind. With literal objects
    //  filtered out, there are no matching rows at all.)
    assert_eq!(
        count, 0,
        "Domain/range rule should produce 0 triples when object is a literal (FILTER isIRI)",
    );

    // Verify alice is NOT typed as Person
    let has_type = ask(
        &graph,
        r#"
        PREFIX ex: <http://example.org/>
        PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
        ASK { ex:alice rdf:type ex:Person }
        "#,
    );
    assert!(
        !has_type,
        "alice should NOT be inferred as Person when the property object is a literal"
    );
}

// ---------------------------------------------------------------------------
// Test 13: Equivalent classes bidirectional propagation
// ---------------------------------------------------------------------------

#[test]
fn test_equivalent_classes_bidirectional() {
    // Arrange: Human equivalentClass Person; alice is Human, bob is Person
    let graph = Graph::new().expect("Graph::new should succeed");
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        ex:Human owl:equivalentClass ex:Person .
        ex:alice rdf:type ex:Human .
        ex:bob rdf:type ex:Person .
    "#,
        )
        .expect("insert_turtle should succeed");
    let executor = ConstructExecutor::new(&graph);

    // Act: Pass 1 — matches Human equivalentClass Person (one direction)
    let count1 = executor
        .execute_and_materialize(RULE_EQUIVALENT_CLASSES)
        .expect("equivalent classes pass 1 should succeed");
    assert_eq!(
        count1, 1,
        "Pass 1 should infer alice rdf:type Person (Human equiv Person), got {}",
        count1
    );

    // Act: Pass 2 — CONSTRUCT re-produces alice->Person (already in graph).
    // The graph should not grow since Oxigraph deduplicates on insert.
    let len_after_1 = graph.len();
    let _count2 = executor
        .execute_and_materialize(RULE_EQUIVALENT_CLASSES)
        .expect("equivalent classes pass 2 should succeed");

    let len_after_2 = graph.len();
    assert_eq!(
        len_after_2, len_after_1,
        "Graph should not grow on pass 2 (deduplication). before={}, after={}",
        len_after_1, len_after_2
    );

    // alice should have type Person (from pass 1)
    assert!(ask(&graph, r#"PREFIX ex: <http://example.org/> PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           ASK { ex:alice rdf:type ex:Person }"#));

    // bob should NOT have type Human (owl:equivalentClass is not symmetric in the rule)
    // The rule only matches ?class owl:equivalentClass ?equivClass (directional).
    // To propagate bob->Human, the ontology would need ex:Person owl:equivalentClass ex:Human .
    let bob_has_human = ask(&graph, r#"PREFIX ex: <http://example.org/> PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
           ASK { ex:bob rdf:type ex:Human }"#);
    assert!(
        !bob_has_human,
        "bob should NOT be inferred as Human because owl:equivalentClass is not \
         symmetric in the CONSTRUCT rule (only ?class owl:equivalentClass ?equivClass, \
         not the reverse)"
    );
}

// ---------------------------------------------------------------------------
// Test 14: Transitive property with three-hop chain (two materialization passes)
// ---------------------------------------------------------------------------

#[test]
fn test_transitive_property_three_hop_chain() {
    // Arrange: a -> b -> c -> d
    let graph = Graph::new().expect("Graph::new should succeed");
    graph
        .insert_turtle(
            r#"
        @prefix ex: <http://example.org/> .
        @prefix owl: <http://www.w3.org/2002/07/owl#> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        ex:locatedIn rdf:type owl:TransitiveProperty .
        ex:a ex:locatedIn ex:b .
        ex:b ex:locatedIn ex:c .
        ex:c ex:locatedIn ex:d .
    "#,
        )
        .expect("insert_turtle should succeed");
    let executor = ConstructExecutor::new(&graph);

    // Pass 1: materializes a->c, b->d
    let count1 = executor
        .execute_and_materialize(RULE_TRANSITIVE_PROPERTIES)
        .expect("transitive pass 1 should succeed");
    assert_eq!(
        count1, 2,
        "Pass 1 should infer a->c and b->d, got {}",
        count1
    );

    let len_after_1 = graph.len();

    // Pass 2: CONSTRUCT re-discovers a->c, b->d (already in graph) and finds new a->d.
    // CONSTRUCT produces 3 triples (a->c, a->d, b->d) but only a->d is actually new.
    let count2 = executor
        .execute_and_materialize(RULE_TRANSITIVE_PROPERTIES)
        .expect("transitive pass 2 should succeed");
    assert!(count2 >= 1, "Pass 2 should produce at least a->d, got {}", count2);

    let len_after_2 = graph.len();
    assert_eq!(
        len_after_2, len_after_1 + 1,
        "Graph should grow by exactly 1 (a->d). before={}, after={}",
        len_after_1, len_after_2
    );

    // Pass 3: fully idempotent -- graph should not grow
    let _count3 = executor
        .execute_and_materialize(RULE_TRANSITIVE_PROPERTIES)
        .expect("transitive pass 3 should succeed");
    let len_after_3 = graph.len();
    assert_eq!(
        len_after_3, len_after_2,
        "Graph should not grow on pass 3 (full closure reached). before={}, after={}",
        len_after_2, len_after_3
    );

    // Verify the full transitive closure
    assert!(ask(
        &graph,
        r#"PREFIX ex: <http://example.org/> ASK { ex:a ex:locatedIn ex:c }"#
    ));
    assert!(ask(
        &graph,
        r#"PREFIX ex: <http://example.org/> ASK { ex:b ex:locatedIn ex:d }"#
    ));
    assert!(ask(
        &graph,
        r#"PREFIX ex: <http://example.org/> ASK { ex:a ex:locatedIn ex:d }"#
    ));
}
