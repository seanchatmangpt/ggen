//! SPARQL CONSTRUCT query validation tests
//!
//! Verifies that CONSTRUCT queries produce valid RDF triples that can be
//! re-inserted into the graph. This is critical because ggen uses CONSTRUCT
//! as its inference rule mechanism.
//!
//! Chicago TDD: Black-box behavior verification with real Graph instances.

use ggen_core::graph::{CachedResult, ConstructExecutor, Graph};

// ---------------------------------------------------------------------------
// Test 1: CONSTRUCT produces valid N-Triples
// ---------------------------------------------------------------------------

#[test]
fn test_construct_query_produces_valid_triples() {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    graph
        .insert_turtle(
            r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:knows ex:bob .
            ex:alice ex:knows ex:carol .
        "#,
        )
        .expect("Failed to insert turtle");

    let executor = ConstructExecutor::new(&graph);

    // Act
    let triples = executor
        .execute(
            r#"
            PREFIX ex: <http://example.org/>
            CONSTRUCT { ?s ex:related ?o }
            WHERE { ?s ex:knows ?o }
        "#,
        )
        .expect("CONSTRUCT query should succeed");

    // Assert: Should produce exactly 2 inferred triples
    assert_eq!(
        triples.len(),
        2,
        "Expected 2 inferred triples, got {}",
        triples.len()
    );

    // Each triple must match the N-Triples structural pattern:
    // <subject> <predicate> <object>  (N-Quads format without trailing dot)
    let ntriples_pattern =
        regex::Regex::new(r#"^<[^>]+>\s+<[^>]+>\s+(<[^>]+>|"[^"]*").*$"#).unwrap();
    for triple in &triples {
        assert!(
            ntriples_pattern.is_match(triple),
            "Triple does not match N-Triples pattern: {}",
            triple
        );
    }

    // Also verify via query_cached returns CachedResult::Graph variant
    let cached = graph
        .query_cached(
            r#"
            PREFIX ex: <http://example.org/>
            CONSTRUCT { ?s ex:related ?o }
            WHERE { ?s ex:knows ?o }
        "#,
        )
        .expect("Cached CONSTRUCT should succeed");

    match cached {
        CachedResult::Graph(triples_from_cache) => {
            assert_eq!(triples_from_cache.len(), 2);
        }
        other => panic!(
            "Expected CachedResult::Graph, got {:?}",
            std::mem::discriminant(&other)
        ),
    }
}

// ---------------------------------------------------------------------------
// Test 2: CONSTRUCT triples can be re-inserted into a new graph
// ---------------------------------------------------------------------------

#[test]
fn test_construct_triples_can_be_reinserted() {
    // Arrange
    let source_graph = Graph::new().expect("Failed to create graph");
    source_graph
        .insert_turtle(
            r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:knows ex:bob .
        "#,
        )
        .expect("Failed to insert turtle");

    let executor = ConstructExecutor::new(&source_graph);

    // Act: Execute CONSTRUCT to produce inferred triples
    let triples = executor
        .execute(
            r#"
            PREFIX ex: <http://example.org/>
            CONSTRUCT { ?s ex:related ?o }
            WHERE { ?s ex:knows ?o }
        "#,
        )
        .expect("CONSTRUCT query should succeed");

    assert!(
        !triples.is_empty(),
        "Should have produced at least one triple"
    );

    // Build N-Triples string with trailing dots for Turtle parser
    let ntriples: String = triples
        .iter()
        .map(|t| format!("{} .", t))
        .collect::<Vec<_>>()
        .join("\n");

    // Act: Load into a brand-new graph
    let target_graph = Graph::new().expect("Failed to create target graph");
    target_graph
        .insert_turtle(&ntriples)
        .expect("Re-inserted triples should parse without error");

    // Assert: The new graph contains the inferred triple
    assert!(
        !target_graph.is_empty(),
        "Target graph should not be empty after re-insert"
    );
    assert_eq!(
        target_graph.len(),
        1,
        "Target graph should have exactly 1 triple"
    );

    // Verify the inferred triple is queryable in the target graph
    let check = target_graph
        .query_cached(
            r#"
            PREFIX ex: <http://example.org/>
            ASK { ?s ex:related ?o }
        "#,
        )
        .expect("ASK query should succeed");

    match check {
        CachedResult::Boolean(true) => {} // pass
        other => panic!("Expected ASK true for re-inserted triple, got {:?}", other),
    }
}

// ---------------------------------------------------------------------------
// Test 3: CONSTRUCT with OPTIONAL produces partial results (no error)
// ---------------------------------------------------------------------------

#[test]
fn test_construct_with_optional_produces_partial_results() {
    // Arrange: alice has a name but no email; bob has both
    let graph = Graph::new().expect("Failed to create graph");
    graph
        .insert_turtle(
            r#"
            @prefix ex: <http://example.org/> .
            ex:alice a ex:Person ;
                     ex:name "Alice" .
            ex:bob   a ex:Person ;
                     ex:name "Bob" ;
                     ex:email "bob@example.org" .
        "#,
        )
        .expect("Failed to insert turtle");

    let executor = ConstructExecutor::new(&graph);

    // Act: CONSTRUCT with OPTIONAL — only matches for persons with an email
    let triples = executor
        .execute(
            r#"
            PREFIX ex: <http://example.org/>
            CONSTRUCT { ?person ex:contact ?email }
            WHERE {
                ?person a ex:Person ;
                        ex:name ?name .
                OPTIONAL { ?person ex:email ?email }
                FILTER(BOUND(?email))
            }
        "#,
        )
        .expect("CONSTRUCT with OPTIONAL should not error");

    // Assert: Only bob has an email, so only 1 triple
    assert_eq!(
        triples.len(),
        1,
        "Expected exactly 1 triple (bob), got {}",
        triples.len()
    );

    // The triple must reference bob
    let triple_str = &triples[0];
    assert!(
        triple_str.contains("bob")
            || triple_str.contains("Bob")
            || triple_str.contains("http://example.org/bob"),
        "Triple should reference bob: {}",
        triple_str
    );
}

// ---------------------------------------------------------------------------
// Test 4: Chain two CONSTRUCT rules — rule 2 depends on rule 1 output
// ---------------------------------------------------------------------------

#[test]
fn test_construct_chain_two_rules() {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    graph
        .insert_turtle(
            r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:knows ex:bob .
            ex:bob   ex:knows ex:carol .
        "#,
        )
        .expect("Failed to insert turtle");

    let executor = ConstructExecutor::new(&graph);

    // Act — Rule 1: infer ex:related from ex:knows
    let rule1_count = executor
        .execute_and_materialize(
            r#"
            PREFIX ex: <http://example.org/>
            CONSTRUCT { ?s ex:related ?o }
            WHERE { ?s ex:knows ?o }
        "#,
        )
        .expect("Rule 1 materialization should succeed");

    assert_eq!(rule1_count, 2, "Rule 1 should infer 2 triples");

    // Act — Rule 2: infer ex:indirectConnection from ex:related (depends on rule 1)
    let rule2_count = executor
        .execute_and_materialize(
            r#"
            PREFIX ex: <http://example.org/>
            CONSTRUCT { ?s ex:indirectConnection ?o }
            WHERE { ?s ex:related ?o }
        "#,
        )
        .expect("Rule 2 materialization should succeed");

    assert_eq!(
        rule2_count, 2,
        "Rule 2 should find 2 inferred triples from rule 1"
    );

    // Assert: Total graph should now have original (2) + rule1 (2) + rule2 (2) = 6 triples
    // (note: oxigraph deduplicates, but these are all distinct triples)
    let total = graph.len();
    assert!(
        total >= 6,
        "Graph should have at least 6 triples after chaining, got {}",
        total
    );

    // Verify rule 2 output is queryable
    let check = graph
        .query_cached(
            r#"
            PREFIX ex: <http://example.org/>
            ASK { ?s ex:indirectConnection ?o }
        "#,
        )
        .expect("ASK query should succeed");

    match check {
        CachedResult::Boolean(true) => {} // pass
        other => panic!("Expected ASK true for rule 2 output, got {:?}", other),
    }
}

// ---------------------------------------------------------------------------
// Test 5: CONSTRUCT on empty graph returns empty (no error)
// ---------------------------------------------------------------------------

#[test]
fn test_construct_empty_graph_returns_empty() {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    assert!(graph.is_empty(), "Graph should start empty");

    let executor = ConstructExecutor::new(&graph);

    // Act
    let triples = executor
        .execute(
            r#"
            PREFIX ex: <http://example.org/>
            CONSTRUCT { ?s ex:related ?o }
            WHERE { ?s ex:knows ?o }
        "#,
        )
        .expect("CONSTRUCT on empty graph should not error");

    // Assert
    assert!(
        triples.is_empty(),
        "CONSTRUCT on empty graph should return zero triples, got {}",
        triples.len()
    );

    // Also verify via query_cached
    let cached = graph
        .query_cached(
            r#"
            PREFIX ex: <http://example.org/>
            CONSTRUCT { ?s ex:related ?o }
            WHERE { ?s ex:knows ?o }
        "#,
        )
        .expect("Cached CONSTRUCT on empty graph should not error");

    match cached {
        CachedResult::Graph(t) => {
            assert!(t.is_empty(), "Cached result should be empty");
        }
        other => panic!(
            "Expected CachedResult::Graph, got {:?}",
            std::mem::discriminant(&other)
        ),
    }
}
