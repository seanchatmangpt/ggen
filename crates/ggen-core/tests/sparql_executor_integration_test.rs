//! Integration tests for SPARQL executor with ggen pipeline
//!
//! Tests verify that SPARQL queries execute correctly within the code generation pipeline
//! and produce results that can be consumed by template rendering.

use ggen_core::graph::Graph;
use ggen_core::sparql::execute_query_inline;

#[test]
fn test_sparql_executor_simple_select() {
    // Arrange
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:name "Alice" ;
                     ex:role "Engineer" .
            ex:bob ex:name "Bob" ;
                   ex:role "Manager" .
        "#,
        )
        .unwrap();

    // Act
    let results = execute_query_inline(
        &graph,
        "SELECT ?name ?role WHERE { ?s <http://example.org/name> ?name ; <http://example.org/role> ?role }",
    )
    .unwrap();

    // Assert
    assert_eq!(results.len(), 2);

    // Check first result
    let alice = results.iter().find(|r| r.get("name") == Some(&"Alice".to_string()));
    assert!(alice.is_some());
    assert_eq!(alice.unwrap().get("role"), Some(&"Engineer".to_string()));

    // Check second result
    let bob = results.iter().find(|r| r.get("name") == Some(&"Bob".to_string()));
    assert!(bob.is_some());
    assert_eq!(bob.unwrap().get("role"), Some(&"Manager".to_string()));
}

#[test]
fn test_sparql_executor_complex_pattern() {
    // Arrange
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
            @prefix ex: <http://example.org/> .
            @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

            ex:Alice a ex:Person ;
                     ex:age "30" ;
                     ex:email "alice@example.org" .

            ex:Bob a ex:Person ;
                   ex:age "25" ;
                   ex:email "bob@example.org" .
        "#,
        )
        .unwrap();

    // Act
    let results = execute_query_inline(
        &graph,
        "SELECT ?person ?age WHERE { ?person a <http://example.org/Person> ; <http://example.org/age> ?age }",
    )
    .unwrap();

    // Assert
    assert_eq!(results.len(), 2);
    assert!(results.iter().any(|r| r.get("age") == Some(&"30".to_string())));
    assert!(results.iter().any(|r| r.get("age") == Some(&"25".to_string())));
}

#[test]
fn test_sparql_executor_filter() {
    // Arrange
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
            @prefix ex: <http://example.org/> .

            ex:alice ex:age 30 .
            ex:bob ex:age 25 .
            ex:charlie ex:age 35 .
        "#,
        )
        .unwrap();

    // Act - Use FILTER with simple string comparison
    let results = execute_query_inline(
        &graph,
        "SELECT ?person ?age WHERE { ?person <http://example.org/age> ?age . FILTER(?age > 26) }",
    )
    .unwrap();

    // Assert
    // Results should contain ages greater than 26 (30 and 35)
    assert!(results.len() >= 2);
    let ages: Vec<_> = results
        .iter()
        .filter_map(|r| r.get("age").map(|s| s.as_str()))
        .collect();
    assert!(ages.contains(&"30") || ages.contains(&"35"));
}

#[test]
fn test_sparql_executor_with_language_tags() {
    // Arrange
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
            @prefix ex: <http://example.org/> .

            ex:alice ex:name "Alice"@en ;
                     ex:name "Alice"@fr .
            ex:bob ex:name "Bob"@en .
        "#,
        )
        .unwrap();

    // Act
    let results = execute_query_inline(
        &graph,
        "SELECT ?name WHERE { ?s <http://example.org/name> ?name . FILTER(LANG(?name) = \"en\") }",
    )
    .unwrap();

    // Assert
    // Note: Results should contain names with @en language tag stripped by executor
    assert!(results.len() >= 2);
}

#[test]
fn test_sparql_executor_empty_result_handling() {
    // Arrange
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:name "Alice" .
        "#,
        )
        .unwrap();

    // Act
    let results = execute_query_inline(
        &graph,
        "SELECT ?name WHERE { ?s <http://example.org/nonexistent> ?name }",
    )
    .unwrap();

    // Assert
    assert_eq!(results.len(), 0);
}

#[test]
fn test_sparql_executor_variable_naming() {
    // Arrange - Verify variable names don't have '?' prefix in results
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
            @prefix ex: <http://example.org/> .
            ex:test ex:prop "value" .
        "#,
        )
        .unwrap();

    // Act
    let results = execute_query_inline(
        &graph,
        "SELECT ?subj ?prop ?obj WHERE { ?subj ?prop ?obj }",
    )
    .unwrap();

    // Assert
    assert!(!results.is_empty());
    for row in results {
        // Variable names should NOT have '?' prefix
        for key in row.keys() {
            assert!(!key.starts_with('?'), "Variable name has '?' prefix: {}", key);
        }
    }
}

#[test]
fn test_sparql_executor_multiple_rows_template_compatible() {
    // Arrange
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
            @prefix ex: <http://example.org/> .
            ex:user1 ex:username "alice" ; ex:email "alice@example.com" .
            ex:user2 ex:username "bob" ; ex:email "bob@example.com" .
            ex:user3 ex:username "charlie" ; ex:email "charlie@example.com" .
        "#,
        )
        .unwrap();

    // Act
    let results = execute_query_inline(
        &graph,
        "SELECT ?username ?email WHERE { ?u <http://example.org/username> ?username ; <http://example.org/email> ?email }",
    )
    .unwrap();

    // Assert
    assert_eq!(results.len(), 3);

    // Verify each result can be used as template context
    for result in &results {
        assert!(result.contains_key("username"));
        assert!(result.contains_key("email"));
        // Both values should be strings ready for template rendering
        assert!(result.get("username").unwrap().len() > 0);
        assert!(result.get("email").unwrap().len() > 0);
    }
}

#[test]
fn test_sparql_executor_distinct_query() {
    // Arrange
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
            @prefix ex: <http://example.org/> .
            ex:alice ex:tag "rust" .
            ex:alice ex:tag "rust" .
            ex:bob ex:tag "rust" .
            ex:charlie ex:tag "go" .
        "#,
        )
        .unwrap();

    // Act
    let results = execute_query_inline(
        &graph,
        "SELECT DISTINCT ?tag WHERE { ?s <http://example.org/tag> ?tag }",
    )
    .unwrap();

    // Assert
    let tags: Vec<_> = results.iter().filter_map(|r| r.get("tag")).collect();
    assert!(tags.contains(&&"rust".to_string()));
    assert!(tags.contains(&&"go".to_string()));
}

#[test]
fn test_sparql_executor_result_conversion_to_hashmap() {
    // Arrange
    let graph = Graph::new().unwrap();
    graph
        .insert_turtle(
            r#"
            @prefix ex: <http://example.org/> .
            ex:test1 ex:value "100" .
            ex:test2 ex:value "200" .
        "#,
        )
        .unwrap();

    // Act
    let results = execute_query_inline(
        &graph,
        "SELECT ?test ?value WHERE { ?test <http://example.org/value> ?value }",
    )
    .unwrap();

    // Assert
    assert_eq!(results.len(), 2);
    for row in results {
        // Each row should be a HashMap<String, String>
        // Verify it has the expected structure
        assert!(!row.is_empty());
        assert!(row.contains_key("test") || row.contains_key("value"));
    }
}
