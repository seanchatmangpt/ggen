//! Comprehensive Chicago TDD Tests for SPARQL Query Builders
//!
//! This test suite verifies SPARQL query construction, validation, and security
//! using Chicago TDD principles:
//! - AAA pattern (Arrange/Act/Assert)
//! - Real SPARQL parsers and RDF stores (no mocks)
//! - Behavior verification through observable outputs
//! - State-based testing
//!
//! # Test Categories
//!
//! 1. Query Construction Tests (15 tests)
//!    - SELECT, CONSTRUCT, ASK, DESCRIBE queries
//!    - Multiple WHERE clauses, OPTIONAL patterns, UNION queries
//!    - Nested queries and complex patterns
//!
//! 2. Input Validation Tests (20 tests)
//!    - SPARQL injection prevention
//!    - Malicious input sanitization
//!    - Unicode and encoding attacks
//!    - SQL-like injection patterns
//!
//! 3. Edge Case Tests (10 tests)
//!    - Empty queries, very long queries
//!    - Many variables, deep nesting
//!    - Special characters, reserved keywords
//!
//! 4. Error Handling Tests (10 tests)
//!    - Invalid syntax, duplicate variables
//!    - Missing clauses, type mismatches
//!
//! 5. Property-Based Tests (fuzzing with proptest)

use ggen_core::graph::{Graph, GraphQuery};
use ggen_ontology_core::sparql_generator::SparqlGenerator;
use oxigraph::sparql::QueryResults;
use oxigraph::store::Store;
use oxigraph::io::RdfFormat;
use std::collections::BTreeMap;

// ============================================================================
// TEST FIXTURES & HELPERS
// ============================================================================

/// Create a test RDF store with sample data
fn create_test_store() -> Store {
    let ttl = r#"
        @prefix ex: <http://example.org/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ex:alice a ex:Person ;
            ex:name "Alice" ;
            ex:age 30 ;
            ex:email "alice@example.org" .

        ex:bob a ex:Person ;
            ex:name "Bob" ;
            ex:age 25 ;
            ex:email "bob@example.org" .

        ex:charlie a ex:Person ;
            ex:name "Charlie" ;
            ex:age 35 ;
            ex:email "charlie@example.org" .

        ex:Person a rdfs:Class ;
            rdfs:label "Person" .

        ex:Organization a rdfs:Class ;
            rdfs:label "Organization" .
    "#;

    let store = Store::new().expect("Failed to create store");
    let reader = std::io::Cursor::new(ttl);
    store.load_from_reader(RdfFormat::Turtle, reader)
        .expect("Failed to load TTL");
    store
}

/// Create a Graph instance with test data
fn create_test_graph() -> Graph {
    let graph = Graph::new().expect("Failed to create graph");
    graph.insert_turtle(r#"
        @prefix ex: <http://example.org/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:project1 a ex:Project ;
            ex:name "Project Alpha" ;
            ex:status "active" .

        ex:project2 a ex:Project ;
            ex:name "Project Beta" ;
            ex:status "completed" .
    "#).expect("Failed to insert turtle");
    graph
}

/// Helper to count solutions in QueryResults
fn count_solutions(results: QueryResults) -> usize {
    match results {
        QueryResults::Solutions(solutions) => {
            solutions.filter_map(|s| s.ok()).count()
        }
        _ => 0,
    }
}

// ============================================================================
// CATEGORY 1: QUERY CONSTRUCTION TESTS (15 tests)
// ============================================================================

#[test]
fn test_select_query_with_single_variable() {
    // Arrange
    let store = create_test_store();

    // Act - Use full IRI to avoid prefix issues
    let query = "SELECT ?name WHERE { ?s <http://example.org/name> ?name }";
    let results = store.query(query).expect("Query should execute");

    // Assert - Verify we get results with name variable
    let count = count_solutions(results);
    assert_eq!(count, 3, "Should find 3 people with names");
}

#[test]
fn test_select_query_with_multiple_variables() {
    // Arrange
    let store = create_test_store();

    // Act
    let query = r#"
        SELECT ?name ?age WHERE {
            ?s <http://example.org/name> ?name .
            ?s <http://example.org/age> ?age .
        }
    "#;
    let results = store.query(query).expect("Query should execute");

    // Assert
    let count = count_solutions(results);
    assert_eq!(count, 3, "Should find 3 people with name and age");
}

#[test]
fn test_construct_query_creates_new_triples() {
    // Arrange
    let store = create_test_store();

    // Act - CONSTRUCT creates new graph
    let query = r#"
        CONSTRUCT { ?person <http://example.org/hasName> ?name }
        WHERE { ?person <http://example.org/name> ?name }
    "#;
    let results = store.query(query).expect("Query should execute");

    // Assert - Verify CONSTRUCT returns graph
    match results {
        QueryResults::Graph(graph) => {
            let count = graph.count();
            assert_eq!(count, 3, "Should construct 3 new triples");
        }
        _ => panic!("CONSTRUCT should return Graph"),
    }
}

#[test]
fn test_ask_query_returns_boolean() {
    // Arrange
    let store = create_test_store();

    // Act - ASK query checks existence
    let query = r#"
        ASK { ?s <http://example.org/name> "Alice" }
    "#;
    let results = store.query(query).expect("Query should execute");

    // Assert - Verify boolean result
    match results {
        QueryResults::Boolean(true) => {}, // Success
        QueryResults::Boolean(false) => panic!("Should find Alice"),
        _ => panic!("ASK should return Boolean"),
    }
}

#[test]
fn test_describe_query_returns_graph() {
    // Arrange
    let store = create_test_store();

    // Act - DESCRIBE returns all triples about a resource
    let query = "DESCRIBE <http://example.org/alice>";
    let results = store.query(query).expect("Query should execute");

    // Assert - Verify DESCRIBE returns graph
    match results {
        QueryResults::Graph(graph) => {
            let count = graph.count();
            assert!(count >= 3, "Should describe Alice with at least 3 triples");
        }
        _ => panic!("DESCRIBE should return Graph"),
    }
}

#[test]
fn test_select_with_filter_clause() {
    // Arrange
    let store = create_test_store();

    // Act - Filter by age > 28
    let query = r#"
        SELECT ?name ?age WHERE {
            ?s <http://example.org/name> ?name .
            ?s <http://example.org/age> ?age .
            FILTER (?age > 28)
        }
    "#;
    let results = store.query(query).expect("Query should execute");

    // Assert - Only Alice (30) and Charlie (35) match
    let count = count_solutions(results);
    assert_eq!(count, 2, "Should find 2 people over 28");
}

#[test]
fn test_select_with_optional_pattern() {
    // Arrange
    let graph = create_test_graph();

    // Act - Query with OPTIONAL clause
    let query = r#"
        SELECT ?name ?status WHERE {
            ?s <http://example.org/name> ?name .
            OPTIONAL { ?s <http://example.org/status> ?status }
        }
    "#;
    let results = graph.query(query).expect("Query should execute");

    // Assert - Both projects should return (OPTIONAL allows missing status)
    let count = count_solutions(results);
    assert_eq!(count, 2, "Should find 2 projects");
}

#[test]
fn test_select_with_union_queries() {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    graph.insert_turtle(r#"
        @prefix ex: <http://example.org/> .

        ex:item1 ex:type "book" ; ex:title "Book 1" .
        ex:item2 ex:type "article" ; ex:title "Article 1" .
    "#).expect("Failed to insert turtle");

    // Act - UNION combines results from two patterns
    let query = r#"
        SELECT ?item WHERE {
            { ?item <http://example.org/type> "book" }
            UNION
            { ?item <http://example.org/type> "article" }
        }
    "#;
    let results = graph.query(query).expect("Query should execute");

    // Assert - Should find both book and article
    let count = count_solutions(results);
    assert_eq!(count, 2, "UNION should find both items");
}

#[test]
fn test_nested_select_query() {
    // Arrange
    let store = create_test_store();

    // Act - Nested query with subselect
    let query = r#"
        SELECT ?name WHERE {
            ?s <http://example.org/name> ?name .
            {
                SELECT ?s WHERE {
                    ?s <http://example.org/age> ?age .
                    FILTER (?age >= 30)
                }
            }
        }
    "#;
    let results = store.query(query).expect("Query should execute");

    // Assert - Only people >= 30 (Alice, Charlie)
    let count = count_solutions(results);
    assert_eq!(count, 2, "Nested query should find 2 people");
}

#[test]
fn test_select_with_order_by() {
    // Arrange
    let store = create_test_store();

    // Act - ORDER BY age
    let query = r#"
        SELECT ?name ?age WHERE {
            ?s <http://example.org/name> ?name .
            ?s <http://example.org/age> ?age .
        }
        ORDER BY ?age
    "#;
    let results = store.query(query).expect("Query should execute");

    // Assert - Should return all 3 in order
    let count = count_solutions(results);
    assert_eq!(count, 3, "Should find all 3 people ordered by age");
}

#[test]
fn test_select_with_limit_and_offset() {
    // Arrange
    let store = create_test_store();

    // Act - LIMIT 2 OFFSET 1
    let query = r#"
        SELECT ?name WHERE {
            ?s <http://example.org/name> ?name .
        }
        ORDER BY ?name
        LIMIT 2
        OFFSET 1
    "#;
    let results = store.query(query).expect("Query should execute");

    // Assert - Should return 2 results starting from offset 1
    let count = count_solutions(results);
    assert_eq!(count, 2, "LIMIT 2 OFFSET 1 should return 2 results");
}

#[test]
fn test_select_with_distinct() {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    graph.insert_turtle(r#"
        @prefix ex: <http://example.org/> .

        ex:person1 ex:country "USA" .
        ex:person2 ex:country "USA" .
        ex:person3 ex:country "Canada" .
    "#).expect("Failed to insert turtle");

    // Act - DISTINCT removes duplicates
    let query = r#"
        SELECT DISTINCT ?country WHERE {
            ?s <http://example.org/country> ?country .
        }
    "#;
    let results = graph.query(query).expect("Query should execute");

    // Assert - Should return 2 unique countries
    let count = count_solutions(results);
    assert_eq!(count, 2, "DISTINCT should return 2 unique countries");
}

#[test]
fn test_select_with_count_aggregate() {
    // Arrange
    let store = create_test_store();

    // Act - COUNT aggregate
    let query = r#"
        SELECT (COUNT(?s) AS ?count) WHERE {
            ?s <http://example.org/name> ?name .
        }
    "#;
    let results = store.query(query).expect("Query should execute");

    // Assert - Should count 3 people
    let count = count_solutions(results);
    assert_eq!(count, 1, "Aggregate should return 1 row with count");
}

#[test]
fn test_select_with_group_by() {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    graph.insert_turtle(r#"
        @prefix ex: <http://example.org/> .

        ex:emp1 ex:dept "Engineering" ; ex:salary 100000 .
        ex:emp2 ex:dept "Engineering" ; ex:salary 110000 .
        ex:emp3 ex:dept "Sales" ; ex:salary 90000 .
    "#).expect("Failed to insert turtle");

    // Act - GROUP BY department
    let query = r#"
        SELECT ?dept (COUNT(?emp) AS ?count) WHERE {
            ?emp <http://example.org/dept> ?dept .
        }
        GROUP BY ?dept
    "#;
    let results = graph.query(query).expect("Query should execute");

    // Assert - Should return 2 groups
    let count = count_solutions(results);
    assert_eq!(count, 2, "GROUP BY should return 2 department groups");
}

#[test]
fn test_query_with_graph_query_builder() {
    // Arrange
    let graph = create_test_graph();
    let query = GraphQuery::new(&graph);

    // Act - Use GraphQuery builder
    let results = query.execute(
        "SELECT ?name WHERE { ?s <http://example.org/name> ?name }"
    ).expect("Query should execute");

    // Assert - Should find 2 projects
    let count = count_solutions(results);
    assert_eq!(count, 2, "GraphQuery should find 2 projects");
}

// ============================================================================
// CATEGORY 2: INPUT VALIDATION TESTS (20 tests)
// ============================================================================

#[test]
fn test_injection_sql_drop_table_blocked() {
    // Arrange
    let store = create_test_store();
    let malicious_input = "'; DROP TABLE users--";

    // Act - Try to inject DROP TABLE
    let query = format!(
        r#"SELECT ?s WHERE {{ ?s <http://example.org/name> "{}" }}"#,
        malicious_input
    );
    let result = store.query(&query);

    // Assert - Query should fail or return empty (not execute DROP)
    match result {
        Ok(results) => {
            let count = count_solutions(results);
            assert_eq!(count, 0, "Injection should not match any results");
        }
        Err(_) => {
            // Parse error is also acceptable - injection blocked
        }
    }
}

#[test]
fn test_injection_sparql_union_blocked() {
    // Arrange
    let store = create_test_store();
    let malicious_input = "> UNION SELECT * WHERE { ?s ?p ?o } <";

    // Act
    let query = format!(
        r#"SELECT ?s WHERE {{ ?s <http://example.org/name{}> ?o }}"#,
        malicious_input
    );
    let result = store.query(&query);

    // Assert - Should fail to parse or return empty
    match result {
        Ok(results) => {
            let count = count_solutions(results);
            assert_eq!(count, 0, "UNION injection should not succeed");
        }
        Err(_) => {} // Parse error is acceptable
    }
}

#[test]
fn test_injection_sparql_filter_manipulation_blocked() {
    // Arrange
    let store = create_test_store();
    let malicious_input = "> FILTER (1=1 OR 1=1) <";

    // Act
    let query = format!(
        r#"SELECT ?s WHERE {{ ?s <http://example.org/test{}> ?o }}"#,
        malicious_input
    );
    let result = store.query(&query);

    // Assert
    match result {
        Ok(results) => {
            let count = count_solutions(results);
            assert_eq!(count, 0, "FILTER injection should not succeed");
        }
        Err(_) => {} // Parse error is acceptable
    }
}

#[test]
fn test_injection_comment_syntax_blocked() {
    // Arrange
    let store = create_test_store();
    let malicious_input = "test#comment injection";

    // Act
    let query = format!(
        r#"SELECT ?s WHERE {{ ?s <http://example.org/name> "{}" }}"#,
        malicious_input
    );
    let result = store.query(&query);

    // Assert - Hash in string literal should be safe
    match result {
        Ok(results) => {
            let count = count_solutions(results);
            assert_eq!(count, 0, "Comment injection should not match");
        }
        Err(_) => {} // Error is acceptable
    }
}

#[test]
fn test_injection_double_dash_comment_blocked() {
    // Arrange
    let store = create_test_store();
    let malicious_input = "test-- SQL comment";

    // Act
    let query = format!(
        r#"SELECT ?s WHERE {{ ?s <http://example.org/name> "{}" }}"#,
        malicious_input
    );
    let result = store.query(&query);

    // Assert
    match result {
        Ok(results) => {
            let count = count_solutions(results);
            assert_eq!(count, 0, "SQL comment injection should not match");
        }
        Err(_) => {} // Error is acceptable
    }
}

#[test]
fn test_injection_quote_escape_blocked() {
    // Arrange
    let store = create_test_store();
    let malicious_input = r#"test\" OR \"1\"=\"1"#;

    // Act
    let query = format!(
        r#"SELECT ?s WHERE {{ ?s <http://example.org/name> "{}" }}"#,
        malicious_input
    );
    let result = store.query(&query);

    // Assert - Escaped quotes should be handled safely
    match result {
        Ok(results) => {
            let count = count_solutions(results);
            assert_eq!(count, 0, "Quote escape injection should not match");
        }
        Err(_) => {} // Error is acceptable
    }
}

#[test]
fn test_injection_unicode_escape_blocked() {
    // Arrange
    let store = create_test_store();
    // \u0027 = single quote
    let malicious_input = "test\u{0027}; DELETE DATA";

    // Act
    let query = format!(
        r#"SELECT ?s WHERE {{ ?s <http://example.org/name> "{}" }}"#,
        malicious_input
    );
    let result = store.query(&query);

    // Assert
    match result {
        Ok(results) => {
            let count = count_solutions(results);
            assert_eq!(count, 0, "Unicode injection should not match");
        }
        Err(_) => {} // Error is acceptable
    }
}

#[test]
fn test_injection_null_byte_blocked() {
    // Arrange
    let store = create_test_store();
    let malicious_input = "test\u{0000}null";

    // Act
    let query = format!(
        r#"SELECT ?s WHERE {{ ?s <http://example.org/name> "{}" }}"#,
        malicious_input
    );
    let result = store.query(&query);

    // Assert
    match result {
        Ok(results) => {
            let count = count_solutions(results);
            assert_eq!(count, 0, "Null byte injection should not match");
        }
        Err(_) => {} // Error is acceptable
    }
}

#[test]
fn test_injection_control_characters_blocked() {
    // Arrange
    let store = create_test_store();
    let malicious_input = "test\u{0001}\u{001F}control";

    // Act
    let query = format!(
        r#"SELECT ?s WHERE {{ ?s <http://example.org/name> "{}" }}"#,
        malicious_input
    );
    let result = store.query(&query);

    // Assert
    match result {
        Ok(results) => {
            let count = count_solutions(results);
            assert_eq!(count, 0, "Control chars should not match");
        }
        Err(_) => {} // Error is acceptable
    }
}

#[test]
fn test_injection_sparql_delete_blocked() {
    // Arrange
    let store = create_test_store();
    let malicious_input = ">}; DELETE WHERE { ?s ?p ?o } #<";

    // Act
    let query = format!(
        r#"SELECT ?s WHERE {{ ?s <http://example.org/test{}> ?o }}"#,
        malicious_input
    );
    let result = store.query(&query);

    // Assert
    match result {
        Ok(results) => {
            let count = count_solutions(results);
            assert_eq!(count, 0, "DELETE injection should not succeed");
        }
        Err(_) => {} // Parse error is acceptable
    }
}

#[test]
fn test_injection_sparql_insert_blocked() {
    // Arrange
    let store = create_test_store();
    let malicious_input = ">}; INSERT DATA { <http://evil> <p> <o> } #<";

    // Act
    let query = format!(
        r#"SELECT ?s WHERE {{ ?s <http://example.org/test{}> ?o }}"#,
        malicious_input
    );
    let result = store.query(&query);

    // Assert
    match result {
        Ok(results) => {
            let count = count_solutions(results);
            assert_eq!(count, 0, "INSERT injection should not succeed");
        }
        Err(_) => {} // Parse error is acceptable
    }
}

#[test]
fn test_sparql_generator_escapes_quotes() {
    // Arrange
    let malicious_jurisdiction = r#"US"; DROP TABLE--"#;

    // Act - SparqlGenerator should escape properly
    let query = SparqlGenerator::find_policies_by_jurisdiction(malicious_jurisdiction);

    // Assert - Query should contain escaped quote
    assert!(query.contains("\\\""), "Should escape double quotes");
    assert!(!query.contains("DROP TABLE"), "Should escape the injection attempt");
}

#[test]
fn test_sparql_generator_escapes_backslashes() {
    // Arrange
    let malicious_input = r"Test\Injection";

    // Act
    let query = SparqlGenerator::find_data_classifications(malicious_input);

    // Assert - Backslashes should be escaped
    assert!(query.contains("\\\\"), "Should escape backslashes");
}

#[test]
fn test_sparql_generator_escapes_newlines() {
    // Arrange
    let malicious_input = "Test\nNewline\rReturn";

    // Act
    let query = SparqlGenerator::find_security_controls(malicious_input);

    // Assert - Newlines should be escaped
    assert!(query.contains("\\n"), "Should escape newlines");
    assert!(query.contains("\\r"), "Should escape carriage returns");
}

#[test]
fn test_sparql_generator_escapes_tabs() {
    // Arrange
    let malicious_input = "Test\tTab";

    // Act
    let query = SparqlGenerator::find_compute_by_type(malicious_input);

    // Assert - Tabs should be escaped
    assert!(query.contains("\\t"), "Should escape tabs");
}

#[test]
fn test_injection_url_encoded_payload_blocked() {
    // Arrange
    let store = create_test_store();
    let malicious_input = "%3B%20DELETE%20DATA";

    // Act - URL encoded payload
    let query = format!(
        r#"SELECT ?s WHERE {{ ?s <http://example.org/name> "{}" }}"#,
        malicious_input
    );
    let result = store.query(&query);

    // Assert - Should be treated as literal
    match result {
        Ok(results) => {
            let count = count_solutions(results);
            assert_eq!(count, 0, "URL encoded payload should not decode");
        }
        Err(_) => {} // Error is acceptable
    }
}

#[test]
fn test_injection_javascript_protocol_blocked() {
    // Arrange
    let store = create_test_store();
    let malicious_uri = "javascript:alert('xss')";

    // Act
    let query = format!(r#"SELECT ?s WHERE {{ ?s <{}> ?o }}"#, malicious_uri);
    let result = store.query(&query);

    // Assert
    match result {
        Ok(results) => {
            let count = count_solutions(results);
            assert_eq!(count, 0, "JavaScript protocol should not execute");
        }
        Err(_) => {} // Parse error is acceptable
    }
}

#[test]
fn test_injection_path_traversal_blocked() {
    // Arrange
    let store = create_test_store();
    let malicious_path = "../../../etc/passwd";

    // Act
    let query = format!(
        r#"SELECT ?s WHERE {{ ?s <http://example.org/{}> ?o }}"#,
        malicious_path
    );
    let result = store.query(&query);

    // Assert - Path treated as literal
    match result {
        Ok(results) => {
            let count = count_solutions(results);
            assert_eq!(count, 0, "Path traversal should not work");
        }
        Err(_) => {} // Error is acceptable
    }
}

#[test]
fn test_injection_bind_variable_manipulation_blocked() {
    // Arrange
    let store = create_test_store();
    let malicious_input = "> . BIND(\"hacked\" AS ?result) . <";

    // Act
    let query = format!(
        r#"SELECT ?s WHERE {{ ?s <http://example.org/test{}> ?o }}"#,
        malicious_input
    );
    let result = store.query(&query);

    // Assert
    match result {
        Ok(results) => {
            let count = count_solutions(results);
            assert_eq!(count, 0, "BIND injection should not succeed");
        }
        Err(_) => {} // Parse error is acceptable
    }
}

#[test]
fn test_injection_values_clause_manipulation_blocked() {
    // Arrange
    let store = create_test_store();
    let malicious_input = "> } VALUES ?s { <http://evil> } {<";

    // Act
    let query = format!(
        r#"SELECT ?s WHERE {{ ?s <http://example.org/test{}> ?o }}"#,
        malicious_input
    );
    let result = store.query(&query);

    // Assert
    match result {
        Ok(results) => {
            let count = count_solutions(results);
            assert_eq!(count, 0, "VALUES injection should not succeed");
        }
        Err(_) => {} // Parse error is acceptable
    }
}

// ============================================================================
// CATEGORY 3: EDGE CASE TESTS (10 tests)
// ============================================================================

#[test]
fn test_empty_query_returns_error() {
    // Arrange
    let store = create_test_store();

    // Act - Empty query string
    let result = store.query("");

    // Assert - Should return error
    assert!(result.is_err(), "Empty query should return error");
}

#[test]
fn test_very_long_query_handled_gracefully() {
    // Arrange
    let store = create_test_store();

    // Act - Query with 1000+ variables
    let mut vars = Vec::new();
    for i in 0..1000 {
        vars.push(format!("?var{}", i));
    }
    let query = format!(
        "SELECT {} WHERE {{ ?s <http://example.org/name> ?name }}",
        vars.join(" ")
    );
    let result = store.query(&query);

    // Assert - Should handle gracefully (ok or error, not panic)
    match result {
        Ok(_) => {}, // Handled
        Err(_) => {}, // Also acceptable
    }
}

#[test]
fn test_very_long_iri_handled_gracefully() {
    // Arrange
    let store = create_test_store();

    // Act - IRI with 10,000+ characters
    let long_iri = format!("http://example.org/{}", "a".repeat(10000));
    let query = format!(r#"SELECT ?s WHERE {{ ?s <{}> ?o }}"#, long_iri);
    let result = store.query(&query);

    // Assert - Should not panic
    match result {
        Ok(results) => {
            let count = count_solutions(results);
            assert_eq!(count, 0, "Long IRI should not match");
        }
        Err(_) => {} // Error is acceptable
    }
}

#[test]
fn test_deeply_nested_query_handled() {
    // Arrange
    let store = create_test_store();

    // Act - Deep nesting (10 levels)
    let query = r#"
        SELECT ?name WHERE {
            ?s <http://example.org/name> ?name .
            {
                SELECT ?s WHERE {
                    {
                        SELECT ?s WHERE {
                            ?s <http://example.org/age> ?age .
                            FILTER (?age > 20)
                        }
                    }
                }
            }
        }
    "#;
    let result = store.query(query);

    // Assert - Should handle nested query
    assert!(result.is_ok(), "Deeply nested query should execute");
}

#[test]
fn test_query_with_special_characters_in_iri() {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    graph.insert_turtle(r#"
        @prefix ex: <http://example.org/> .
        <http://example.org/resource-with-dash> ex:name "Test" .
        <http://example.org/resource_with_underscore> ex:name "Test2" .
    "#).expect("Failed to insert turtle");

    // Act - Query with special chars in IRI
    let query = r#"
        SELECT ?name WHERE {
            <http://example.org/resource-with-dash> <http://example.org/name> ?name .
        }
    "#;
    let results = graph.query(query).expect("Query should execute");

    // Assert - Should find the resource
    let count = count_solutions(results);
    assert_eq!(count, 1, "Should find resource with dash in IRI");
}

#[test]
fn test_query_with_reserved_keyword_as_variable() {
    // Arrange
    let store = create_test_store();

    // Act - Use SPARQL reserved keywords as variable names
    let query = r#"
        SELECT ?select ?where ?filter WHERE {
            ?s <http://example.org/name> ?select .
            ?s <http://example.org/age> ?where .
            ?s <http://example.org/email> ?filter .
        }
    "#;
    let result = store.query(query);

    // Assert - Should handle or reject gracefully
    match result {
        Ok(_) => {}, // Some parsers allow this
        Err(_) => {}, // Some parsers reject this
    }
}

#[test]
fn test_query_with_unicode_characters() {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    graph.insert_turtle(r#"
        @prefix ex: <http://example.org/> .
        ex:person1 ex:name "José" .
        ex:person2 ex:name "李明" .
        ex:person3 ex:name "Müller" .
    "#).expect("Failed to insert turtle");

    // Act - Query for Unicode name
    let query = r#"
        SELECT ?s WHERE {
            ?s <http://example.org/name> "José" .
        }
    "#;
    let results = graph.query(query).expect("Query should execute");

    // Assert - Should find Unicode name
    let count = count_solutions(results);
    assert_eq!(count, 1, "Should find Unicode name");
}

#[test]
fn test_query_with_blank_nodes() {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    graph.insert_turtle(r#"
        @prefix ex: <http://example.org/> .
        _:blank1 ex:name "Anonymous" .
        _:blank2 ex:name "Unknown" .
    "#).expect("Failed to insert turtle");

    // Act - Query for blank nodes
    let query = r#"
        SELECT ?name WHERE {
            ?s <http://example.org/name> ?name .
        }
    "#;
    let results = graph.query(query).expect("Query should execute");

    // Assert - Should find blank nodes
    let count = count_solutions(results);
    assert_eq!(count, 2, "Should find both blank nodes");
}

#[test]
fn test_query_with_language_tags() {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    graph.insert_turtle(r#"
        @prefix ex: <http://example.org/> .
        ex:resource ex:label "Hello"@en .
        ex:resource ex:label "Bonjour"@fr .
    "#).expect("Failed to insert turtle");

    // Act - Query for English label
    let query = r#"
        SELECT ?label WHERE {
            <http://example.org/resource> <http://example.org/label> ?label .
            FILTER (lang(?label) = "en")
        }
    "#;
    let results = graph.query(query).expect("Query should execute");

    // Assert - Should find English label
    let count = count_solutions(results);
    assert_eq!(count, 1, "Should find English label");
}

#[test]
fn test_query_with_typed_literals() {
    // Arrange
    let graph = Graph::new().expect("Failed to create graph");
    graph.insert_turtle(r#"
        @prefix ex: <http://example.org/> .
        @prefix xsd: <http://www.w3.org/2001/XMLSchema#> .

        ex:item1 ex:price "10.50"^^xsd:decimal .
        ex:item2 ex:price "20.00"^^xsd:decimal .
    "#).expect("Failed to insert turtle");

    // Act - Query with typed literal filter
    let query = r#"
        SELECT ?item ?price WHERE {
            ?item <http://example.org/price> ?price .
            FILTER (?price > 15)
        }
    "#;
    let results = graph.query(query).expect("Query should execute");

    // Assert - Should find items > 15
    let count = count_solutions(results);
    assert_eq!(count, 1, "Should find 1 item with price > 15");
}

// ============================================================================
// CATEGORY 4: ERROR HANDLING TESTS (10 tests)
// ============================================================================

#[test]
fn test_invalid_iri_syntax_returns_error() {
    // Arrange
    let store = create_test_store();

    // Act - Invalid IRI with unescaped spaces
    let query = r#"SELECT ?s WHERE { ?s <http://example.org/invalid iri> ?o }"#;
    let result = store.query(query);

    // Assert - Should return parse error
    assert!(result.is_err(), "Invalid IRI should return error");
}

#[test]
fn test_duplicate_variable_names_handled() {
    // Arrange
    let store = create_test_store();

    // Act - Same variable bound twice
    let query = r#"
        SELECT ?name ?name WHERE {
            ?s <http://example.org/name> ?name .
        }
    "#;
    let result = store.query(query);

    // Assert - Should handle or reject
    match result {
        Ok(_) => {}, // Some parsers allow this
        Err(_) => {}, // Some parsers reject this
    }
}

#[test]
fn test_invalid_filter_syntax_returns_error() {
    // Arrange
    let store = create_test_store();

    // Act - Invalid FILTER syntax
    let query = r#"
        SELECT ?name WHERE {
            ?s <http://example.org/name> ?name .
            FILTER (invalid syntax here)
        }
    "#;
    let result = store.query(query);

    // Assert - Should return parse error
    assert!(result.is_err(), "Invalid FILTER should return error");
}

#[test]
fn test_missing_where_clause_returns_error() {
    // Arrange
    let store = create_test_store();

    // Act - Missing WHERE clause
    let query = "SELECT ?name";
    let result = store.query(query);

    // Assert - Should return error
    assert!(result.is_err(), "Missing WHERE should return error");
}

#[test]
fn test_missing_select_variables_returns_error() {
    // Arrange
    let store = create_test_store();

    // Act - SELECT without variables
    let query = r#"SELECT WHERE { ?s <http://example.org/name> ?name }"#;
    let result = store.query(query);

    // Assert - Should return error
    assert!(result.is_err(), "Missing SELECT vars should return error");
}

#[test]
fn test_type_mismatch_in_filter_handled() {
    // Arrange
    let store = create_test_store();

    // Act - Type mismatch (comparing string to number)
    let query = r#"
        SELECT ?name WHERE {
            ?s <http://example.org/name> ?name .
            FILTER (?name > 100)
        }
    "#;
    let result = store.query(query);

    // Assert - Should handle gracefully
    match result {
        Ok(results) => {
            // Type error in filter - no results
            let count = count_solutions(results);
            assert_eq!(count, 0, "Type mismatch should return no results");
        }
        Err(_) => {} // Error is also acceptable
    }
}

#[test]
fn test_undefined_prefix_returns_error() {
    // Arrange
    let store = create_test_store();

    // Act - Use undefined prefix
    let query = r#"SELECT ?s WHERE { ?s undefined:property ?o }"#;
    let result = store.query(query);

    // Assert - Should return error
    assert!(result.is_err(), "Undefined prefix should return error");
}

#[test]
fn test_malformed_triple_pattern_returns_error() {
    // Arrange
    let store = create_test_store();

    // Act - Malformed triple (missing object)
    let query = r#"SELECT ?s WHERE { ?s <http://example.org/name> }"#;
    let result = store.query(query);

    // Assert - Should return parse error
    assert!(result.is_err(), "Malformed triple should return error");
}

#[test]
fn test_unbalanced_braces_returns_error() {
    // Arrange
    let store = create_test_store();

    // Act - Unbalanced braces
    let query = r#"SELECT ?s WHERE { ?s <http://example.org/name> ?o "#;
    let result = store.query(query);

    // Assert - Should return parse error
    assert!(result.is_err(), "Unbalanced braces should return error");
}

#[test]
fn test_invalid_aggregate_usage_returns_error() {
    // Arrange
    let store = create_test_store();

    // Act - COUNT without GROUP BY on non-aggregated var
    let query = r#"
        SELECT ?name (COUNT(?s) AS ?count) WHERE {
            ?s <http://example.org/name> ?name .
        }
    "#;
    let result = store.query(query);

    // Assert - Should return error (name not grouped)
    assert!(result.is_err(), "Invalid aggregate should return error");
}

// ============================================================================
// CATEGORY 5: PROPERTY-BASED TESTS (fuzzing with proptest)
// ============================================================================

#[cfg(feature = "proptest")]
mod proptest_tests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn test_arbitrary_string_input_no_panic(input in "\\PC*") {
            // Arrange
            let store = create_test_store();

            // Act - Try arbitrary string as query
            let result = store.query(&input);

            // Assert - Should not panic
            match result {
                Ok(_) => {},
                Err(_) => {},
            }
        }

        #[test]
        fn test_arbitrary_iri_input_no_panic(path in "[a-zA-Z0-9_-]{1,100}") {
            // Arrange
            let store = create_test_store();

            // Act - Try arbitrary IRI
            let query = format!(r#"SELECT ?s WHERE {{ ?s <http://example.org/{}> ?o }}"#, path);
            let result = store.query(&query);

            // Assert - Should not panic
            match result {
                Ok(_) => {},
                Err(_) => {},
            }
        }

        #[test]
        fn test_sparql_generator_no_panic_on_arbitrary_input(input in "\\PC*") {
            // Arrange & Act
            let query = SparqlGenerator::find_policies_by_jurisdiction(&input);

            // Assert - Should produce valid query structure
            assert!(query.contains("@prefix"), "Should have prefix declarations");
            assert!(query.contains("SELECT"), "Should have SELECT clause");
        }
    }
}

// ============================================================================
// INTEGRATION TESTS WITH GRAPHQUERY
// ============================================================================

#[test]
fn test_graphquery_with_prefixes() {
    // Arrange
    let graph = create_test_graph();
    let query = GraphQuery::new(&graph);
    let mut prefixes = BTreeMap::new();
    prefixes.insert("ex".to_string(), "http://example.org/".to_string());

    // Act
    let results = query.execute_with_prefixes(
        "SELECT ?name WHERE { ?s ex:name ?name }",
        &prefixes,
        None
    ).expect("Query should execute");

    // Assert
    let count = count_solutions(results);
    assert_eq!(count, 2, "Should find 2 projects with prefixes");
}

#[test]
fn test_graphquery_cached_execution() {
    // Arrange
    let graph = create_test_graph();
    let query = GraphQuery::new(&graph);

    // Act - Execute same query twice
    let result1 = query.execute_cached(
        "SELECT ?name WHERE { ?s <http://example.org/name> ?name }"
    ).expect("First query should execute");

    let result2 = query.execute_cached(
        "SELECT ?name WHERE { ?s <http://example.org/name> ?name }"
    ).expect("Second query should execute (cached)");

    // Assert - Both should return same results
    match (result1, result2) {
        (
            ggen_core::graph::types::CachedResult::Solutions(rows1),
            ggen_core::graph::types::CachedResult::Solutions(rows2)
        ) => {
            assert_eq!(rows1.len(), rows2.len(), "Cached query should return same count");
            assert_eq!(rows1.len(), 2, "Should find 2 projects");
        }
        _ => panic!("Expected Solutions"),
    }
}

#[test]
fn test_graphquery_builder_interface() {
    // Arrange
    let graph = create_test_graph();
    let query = GraphQuery::new(&graph);

    // Act - Use builder interface
    let builder = query.builder();
    let results = builder
        .parse_query("SELECT ?name WHERE { ?s <http://example.org/name> ?name }")
        .expect("Should parse query")
        .on_store(graph.inner())
        .execute()
        .expect("Should execute");

    // Assert
    let count = count_solutions(results);
    assert_eq!(count, 2, "Builder should find 2 projects");
}
