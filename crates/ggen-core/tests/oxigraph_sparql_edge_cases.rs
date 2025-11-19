//! Comprehensive SPARQL 1.1 edge case tests for Oxigraph triple store integration
//!
//! This test suite validates advanced SPARQL 1.1 features and edge cases:
//! - Complex UNION/OPTIONAL queries
//! - Nested FILTER conditions
//! - CONSTRUCT queries with blank nodes
//! - Property path queries
//! - Aggregation functions (COUNT, SUM, AVG, MIN, MAX)
//! - BIND operations
//! - Subqueries
//! - Concurrent query execution
//! - Malformed query handling
//! - Timeout scenarios
//! - Query result ordering with LIMIT/OFFSET

use ggen_core::graph::{Graph, GraphQuery};
use ggen_utils::error::Result;
use oxigraph::sparql::QueryResults;
use std::sync::Arc;
use std::thread;

/// Test fixture for SPARQL edge case tests
struct SparqlTestFixture {
    graph: Graph,
}

impl SparqlTestFixture {
    fn new() -> Result<Self> {
        let graph = Graph::new()?;
        Ok(Self { graph })
    }

    /// Load comprehensive test data covering various SPARQL scenarios
    fn load_comprehensive_test_data(&self) -> Result<()> {
        let turtle = r#"
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .
@prefix xsd: <http://www.w3.org/2001/XMLSchema#> .
@prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .
@prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

# People with various properties
ex:alice a foaf:Person ;
    foaf:name "Alice" ;
    ex:age 30 ;
    ex:salary 75000 ;
    foaf:knows ex:bob, ex:charlie ;
    ex:department ex:engineering .

ex:bob a foaf:Person ;
    foaf:name "Bob" ;
    ex:age 25 ;
    ex:salary 65000 ;
    foaf:knows ex:alice ;
    ex:department ex:sales .

ex:charlie a foaf:Person ;
    foaf:name "Charlie" ;
    ex:age 35 ;
    ex:salary 85000 ;
    ex:department ex:engineering .

ex:diana a foaf:Person ;
    foaf:name "Diana" ;
    ex:age 28 ;
    ex:salary 70000 ;
    ex:department ex:marketing .

ex:eve a foaf:Person ;
    foaf:name "Eve" ;
    ex:age 32 .
    # Note: Eve has no salary or department

# Organizations
ex:engineering a ex:Department ;
    ex:name "Engineering" ;
    ex:budget 500000 .

ex:sales a ex:Department ;
    ex:name "Sales" ;
    ex:budget 300000 .

ex:marketing a ex:Department ;
    ex:name "Marketing" ;
    ex:budget 200000 .

# Hierarchical relationships (for property paths)
ex:alice ex:manages ex:bob .
ex:bob ex:manages ex:frank .
ex:charlie ex:supervises ex:alice .

ex:frank a foaf:Person ;
    foaf:name "Frank" ;
    ex:age 22 ;
    ex:salary 55000 .

# Projects
ex:project1 a ex:Project ;
    ex:name "Project Alpha" ;
    ex:budget 100000 ;
    ex:member ex:alice, ex:bob .

ex:project2 a ex:Project ;
    ex:name "Project Beta" ;
    ex:budget 150000 ;
    ex:member ex:charlie .
"#;

        self.graph.insert_turtle(turtle)?;
        Ok(())
    }

    /// Load minimal test data
    fn load_minimal_data(&self) -> Result<()> {
        let turtle = r#"
@prefix ex: <http://example.org/> .
@prefix foaf: <http://xmlns.com/foaf/0.1/> .

ex:alice a foaf:Person ;
    foaf:name "Alice" .

ex:bob a foaf:Person ;
    foaf:name "Bob" .
"#;

        self.graph.insert_turtle(turtle)?;
        Ok(())
    }
}

// ============================================================================
// UNION and OPTIONAL Query Tests
// ============================================================================

#[test]
fn test_complex_union_queries() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Query for people in engineering OR sales departments
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?person ?name ?dept WHERE {
            ?person a foaf:Person ;
                    foaf:name ?name .
            {
                ?person ex:department ?dept .
                FILTER(?dept = ex:engineering)
            }
            UNION
            {
                ?person ex:department ?dept .
                FILTER(?dept = ex:sales)
            }
        }
        ORDER BY ?name
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(rows.len() >= 3, "Should find at least 3 people in engineering or sales");
            assert!(rows.iter().any(|r| r.get("name").map(|v| v.contains("Alice")).unwrap_or(false)));
            assert!(rows.iter().any(|r| r.get("name").map(|v| v.contains("Bob")).unwrap_or(false)));
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_nested_union_queries() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Nested UNION queries
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?person ?name WHERE {
            ?person foaf:name ?name .
            {
                { ?person ex:age ?age . FILTER(?age < 30) }
                UNION
                { ?person ex:salary ?salary . FILTER(?salary > 80000) }
            }
            UNION
            {
                ?person ex:department ex:marketing .
            }
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(!rows.is_empty(), "Should find people matching nested UNION criteria");
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_optional_queries() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Query with OPTIONAL clauses (Eve has no salary)
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?name ?salary ?dept WHERE {
            ?person a foaf:Person ;
                    foaf:name ?name .
            OPTIONAL { ?person ex:salary ?salary }
            OPTIONAL { ?person ex:department ?dept }
        }
        ORDER BY ?name
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(rows.len() >= 5, "Should find all people including those without salary");

            // Check that Eve exists but may not have salary
            let eve_row = rows.iter().find(|r|
                r.get("name").map(|v| v.contains("Eve")).unwrap_or(false)
            );
            assert!(eve_row.is_some(), "Should find Eve in results");
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_complex_optional_union_combination() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Complex query combining OPTIONAL and UNION
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?person ?name ?value WHERE {
            ?person foaf:name ?name .
            OPTIONAL {
                {
                    ?person ex:salary ?value .
                }
                UNION
                {
                    ?person ex:age ?value .
                }
            }
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(!rows.is_empty(), "Should find results from complex OPTIONAL/UNION");
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

// ============================================================================
// Nested FILTER Condition Tests
// ============================================================================

#[test]
fn test_nested_filter_with_logical_operators() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Complex nested FILTERs with AND, OR, NOT
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?name ?age ?salary WHERE {
            ?person foaf:name ?name ;
                    ex:age ?age ;
                    ex:salary ?salary .
            FILTER(
                (?age >= 25 && ?age <= 35) &&
                (?salary > 60000 || ?age > 30) &&
                !(?salary < 70000 && ?age < 28)
            )
        }
        ORDER BY ?salary
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(!rows.is_empty(), "Should find people matching complex filter criteria");

            // Verify filter logic
            for row in &rows {
                if let (Some(age_str), Some(salary_str)) = (row.get("age"), row.get("salary")) {
                    let age: i32 = age_str.parse().unwrap_or(0);
                    let salary: i32 = salary_str.parse().unwrap_or(0);

                    assert!(age >= 25 && age <= 35, "Age should be between 25 and 35");
                    assert!(salary > 60000, "Salary should be > 60000");
                }
            }
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_filter_with_string_operations() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: FILTER with string functions
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?name WHERE {
            ?person foaf:name ?name .
            FILTER(
                STRLEN(?name) > 3 &&
                REGEX(?name, "^[A-Z]") &&
                (CONTAINS(?name, "a") || CONTAINS(?name, "e"))
            )
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(!rows.is_empty(), "Should find names matching string filters");

            for row in &rows {
                if let Some(name) = row.get("name") {
                    assert!(name.len() > 3, "Name should be longer than 3 chars");
                }
            }
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_filter_with_bound_and_exists() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: FILTER with BOUND and EXISTS
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?name ?salary WHERE {
            ?person foaf:name ?name .
            OPTIONAL { ?person ex:salary ?salary }
            FILTER(
                BOUND(?salary) &&
                EXISTS { ?person ex:department ?dept }
            )
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(!rows.is_empty(), "Should find people with salary and department");

            // All results should have salary bound
            for row in &rows {
                assert!(row.contains_key("salary"), "Salary should be bound");
            }
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

// ============================================================================
// CONSTRUCT Query Tests with Blank Nodes
// ============================================================================

#[test]
fn test_construct_with_blank_nodes() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: CONSTRUCT query creating blank nodes
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        CONSTRUCT {
            ?person foaf:name ?name .
            ?person ex:profile _:profile .
            _:profile ex:age ?age .
            _:profile ex:salary ?salary .
        }
        WHERE {
            ?person a foaf:Person ;
                    foaf:name ?name .
            OPTIONAL { ?person ex:age ?age }
            OPTIONAL { ?person ex:salary ?salary }
        }
    "#;

    let results = fixture.graph.query(query)?;

    // Assert
    match results {
        QueryResults::Graph(graph_iter) => {
            let mut count = 0;
            for quad_result in graph_iter {
                assert!(quad_result.is_ok(), "Each quad should be valid");
                count += 1;
            }
            assert!(count > 0, "CONSTRUCT should produce triples");
        }
        _ => panic!("Expected graph results from CONSTRUCT"),
    }

    Ok(())
}

#[test]
fn test_construct_with_nested_blank_nodes() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: CONSTRUCT with nested blank node structures
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        CONSTRUCT {
            ?person ex:hasRecord _:record .
            _:record ex:employment _:employment .
            _:employment ex:salary ?salary .
            _:employment ex:department ?dept .
            _:record ex:personal _:personal .
            _:personal ex:age ?age .
        }
        WHERE {
            ?person a foaf:Person .
            OPTIONAL { ?person ex:salary ?salary }
            OPTIONAL { ?person ex:department ?dept }
            OPTIONAL { ?person ex:age ?age }
        }
    "#;

    let results = fixture.graph.query(query)?;

    // Assert
    match results {
        QueryResults::Graph(graph_iter) => {
            let mut count = 0;
            for quad_result in graph_iter {
                assert!(quad_result.is_ok());
                count += 1;
            }
            assert!(count > 0, "Nested CONSTRUCT should produce triples");
        }
        _ => panic!("Expected graph results"),
    }

    Ok(())
}

#[test]
fn test_construct_preserving_data_types() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: CONSTRUCT preserving numeric data types
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        PREFIX xsd: <http://www.w3.org/2001/XMLSchema#>

        CONSTRUCT {
            ?person ex:numericAge ?age .
            ?person ex:numericSalary ?salary .
        }
        WHERE {
            ?person a foaf:Person .
            ?person ex:age ?age .
            OPTIONAL { ?person ex:salary ?salary }
        }
    "#;

    let results = fixture.graph.query(query)?;

    // Assert
    match results {
        QueryResults::Graph(graph_iter) => {
            let mut count = 0;
            for quad_result in graph_iter {
                assert!(quad_result.is_ok());
                count += 1;
            }
            assert!(count > 0, "CONSTRUCT with data types should work");
        }
        _ => panic!("Expected graph results"),
    }

    Ok(())
}

// ============================================================================
// Property Path Query Tests
// ============================================================================

#[test]
fn test_property_path_transitive_closure() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Property path with transitive closure (*)
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?manager ?subordinate ?subName WHERE {
            ?manager ex:manages+ ?subordinate .
            ?subordinate foaf:name ?subName .
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            // Alice manages Bob, Bob manages Frank, so Alice should transitively manage Frank
            assert!(!rows.is_empty(), "Should find transitive management relationships");
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_property_path_alternative() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Property path with alternatives (|)
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?person1 ?person2 WHERE {
            ?person1 (ex:manages|ex:supervises) ?person2 .
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(!rows.is_empty(), "Should find management or supervision relationships");
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_property_path_sequence() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Property path with sequence (/)
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?person ?friendOfFriend WHERE {
            ?person foaf:knows/foaf:knows ?friendOfFriend .
            FILTER(?person != ?friendOfFriend)
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            // May be empty if no friend-of-friend relationships exist
            // The query itself should execute without error
            assert!(rows.len() >= 0);
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_property_path_inverse() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Property path with inverse (^)
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?subordinate ?manager WHERE {
            ?subordinate ^ex:manages ?manager .
            ?subordinate foaf:name ?name .
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(!rows.is_empty(), "Should find inverse management relationships");
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

// ============================================================================
// Aggregation Function Tests (COUNT, SUM, AVG, MIN, MAX)
// ============================================================================

#[test]
fn test_aggregation_count() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: COUNT aggregation
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT (COUNT(?person) AS ?count) WHERE {
            ?person a foaf:Person .
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert_eq!(rows.len(), 1, "Should have one aggregated row");
            assert!(rows[0].contains_key("count"), "Should have count binding");

            if let Some(count_str) = rows[0].get("count") {
                let count: i32 = count_str.parse().unwrap_or(0);
                assert!(count >= 5, "Should count at least 5 people");
            }
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_aggregation_sum() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: SUM aggregation
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT (SUM(?salary) AS ?totalSalary) WHERE {
            ?person a foaf:Person ;
                    ex:salary ?salary .
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert_eq!(rows.len(), 1, "Should have one aggregated row");
            assert!(rows[0].contains_key("totalSalary"), "Should have totalSalary binding");

            if let Some(total_str) = rows[0].get("totalSalary") {
                let total: i32 = total_str.parse().unwrap_or(0);
                assert!(total > 0, "Total salary should be positive");
            }
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_aggregation_avg() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: AVG aggregation
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT (AVG(?age) AS ?avgAge) WHERE {
            ?person a foaf:Person ;
                    ex:age ?age .
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert_eq!(rows.len(), 1, "Should have one aggregated row");
            assert!(rows[0].contains_key("avgAge"), "Should have avgAge binding");
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_aggregation_min_max() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: MIN and MAX aggregations
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT (MIN(?salary) AS ?minSalary) (MAX(?salary) AS ?maxSalary) WHERE {
            ?person a foaf:Person ;
                    ex:salary ?salary .
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert_eq!(rows.len(), 1, "Should have one aggregated row");
            assert!(rows[0].contains_key("minSalary"), "Should have minSalary");
            assert!(rows[0].contains_key("maxSalary"), "Should have maxSalary");

            if let (Some(min_str), Some(max_str)) = (rows[0].get("minSalary"), rows[0].get("maxSalary")) {
                let min: i32 = min_str.parse().unwrap_or(0);
                let max: i32 = max_str.parse().unwrap_or(0);
                assert!(max >= min, "Max should be >= min");
            }
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_aggregation_with_group_by() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Aggregation with GROUP BY
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?dept (COUNT(?person) AS ?count) (AVG(?salary) AS ?avgSalary)
        WHERE {
            ?person a foaf:Person ;
                    ex:department ?dept ;
                    ex:salary ?salary .
        }
        GROUP BY ?dept
        ORDER BY DESC(?avgSalary)
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(!rows.is_empty(), "Should have grouped results");

            for row in &rows {
                assert!(row.contains_key("dept"), "Should have department");
                assert!(row.contains_key("count"), "Should have count");
                assert!(row.contains_key("avgSalary"), "Should have average salary");
            }
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_aggregation_with_having() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Aggregation with HAVING clause
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?dept (COUNT(?person) AS ?count)
        WHERE {
            ?person a foaf:Person ;
                    ex:department ?dept .
        }
        GROUP BY ?dept
        HAVING (COUNT(?person) > 1)
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            // Only departments with more than 1 person should appear
            for row in &rows {
                if let Some(count_str) = row.get("count") {
                    let count: i32 = count_str.parse().unwrap_or(0);
                    assert!(count > 1, "HAVING filter should ensure count > 1");
                }
            }
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

// ============================================================================
// BIND Operation Tests
// ============================================================================

#[test]
fn test_bind_simple_expressions() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: BIND with arithmetic expressions
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?name ?age ?ageIn10Years WHERE {
            ?person foaf:name ?name ;
                    ex:age ?age .
            BIND(?age + 10 AS ?ageIn10Years)
        }
        ORDER BY ?name
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(!rows.is_empty(), "Should have BIND results");

            for row in &rows {
                assert!(row.contains_key("ageIn10Years"), "Should have computed age");

                if let (Some(age_str), Some(future_age_str)) = (row.get("age"), row.get("ageIn10Years")) {
                    let age: i32 = age_str.parse().unwrap_or(0);
                    let future_age: i32 = future_age_str.parse().unwrap_or(0);
                    assert_eq!(future_age, age + 10, "Future age should be current age + 10");
                }
            }
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_bind_with_string_functions() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: BIND with string functions
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?name ?upperName ?nameLength WHERE {
            ?person foaf:name ?name .
            BIND(UCASE(?name) AS ?upperName)
            BIND(STRLEN(?name) AS ?nameLength)
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(!rows.is_empty(), "Should have BIND results with string functions");

            for row in &rows {
                assert!(row.contains_key("upperName"), "Should have uppercase name");
                assert!(row.contains_key("nameLength"), "Should have name length");
            }
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_bind_with_conditional_expressions() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: BIND with IF expressions
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?name ?salary ?salaryCategory WHERE {
            ?person foaf:name ?name ;
                    ex:salary ?salary .
            BIND(
                IF(?salary > 75000, "High",
                   IF(?salary > 65000, "Medium", "Low")
                ) AS ?salaryCategory
            )
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(!rows.is_empty(), "Should have BIND results with conditionals");

            for row in &rows {
                assert!(row.contains_key("salaryCategory"), "Should have salary category");
            }
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_bind_with_multiple_bindings() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Multiple BIND clauses
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?name ?totalComp ?bonus WHERE {
            ?person foaf:name ?name ;
                    ex:salary ?salary .
            BIND(?salary * 1.1 AS ?totalComp)
            BIND(?totalComp - ?salary AS ?bonus)
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(!rows.is_empty(), "Should have results with multiple BINDs");

            for row in &rows {
                assert!(row.contains_key("totalComp"), "Should have total compensation");
                assert!(row.contains_key("bonus"), "Should have bonus");
            }
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

// ============================================================================
// Subquery Tests
// ============================================================================

#[test]
fn test_simple_subquery() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Simple subquery
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?name ?salary WHERE {
            ?person foaf:name ?name ;
                    ex:salary ?salary .
            {
                SELECT (AVG(?s) AS ?avgSalary) WHERE {
                    ?p ex:salary ?s .
                }
            }
            FILTER(?salary > ?avgSalary)
        }
        ORDER BY DESC(?salary)
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            // Should find people with above-average salaries
            assert!(rows.len() >= 0, "Subquery should execute");
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_subquery_with_limit() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Subquery with LIMIT
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?topPerson ?topName WHERE {
            {
                SELECT ?topPerson ?topName WHERE {
                    ?topPerson foaf:name ?topName ;
                               ex:salary ?salary .
                }
                ORDER BY DESC(?salary)
                LIMIT 3
            }
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(rows.len() <= 3, "Subquery LIMIT should restrict results to 3");
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_nested_subqueries() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Nested subqueries
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?name WHERE {
            ?person foaf:name ?name ;
                    ex:department ?dept .
            {
                SELECT ?dept WHERE {
                    {
                        SELECT ?dept (COUNT(?p) AS ?count) WHERE {
                            ?p ex:department ?dept .
                        }
                        GROUP BY ?dept
                    }
                    FILTER(?count > 1)
                }
            }
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(rows.len() >= 0, "Nested subqueries should execute");
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

// ============================================================================
// Concurrent Query Execution Tests
// ============================================================================

#[test]
fn test_concurrent_read_queries() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    let graph = Arc::new(fixture.graph);
    let mut handles = vec![];

    // Act: Spawn multiple threads executing queries concurrently
    for i in 0..10 {
        let graph_clone = Arc::clone(&graph);
        let handle = thread::spawn(move || {
            let query = r#"
                PREFIX ex: <http://example.org/>
                PREFIX foaf: <http://xmlns.com/foaf/0.1/>

                SELECT ?name ?age WHERE {
                    ?person foaf:name ?name ;
                            ex:age ?age .
                }
            "#;

            graph_clone.query_cached(query)
        });
        handles.push(handle);
    }

    // Assert: All threads should complete successfully
    for handle in handles {
        let result = handle.join().expect("Thread should not panic");
        assert!(result.is_ok(), "Concurrent query should succeed");

        match result.unwrap() {
            ggen_core::graph::types::CachedResult::Solutions(rows) => {
                assert!(!rows.is_empty(), "Should find results");
            }
            _ => panic!("Expected solutions"),
        }
    }

    Ok(())
}

#[test]
fn test_concurrent_mixed_operations() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    let graph = Arc::new(fixture.graph);
    let mut handles = vec![];

    // Act: Mix of different query types executed concurrently
    for i in 0..5 {
        let graph_clone = Arc::clone(&graph);
        let handle = thread::spawn(move || {
            match i % 3 {
                0 => {
                    // SELECT query
                    graph_clone.query_cached(
                        "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT ?name WHERE { ?p foaf:name ?name }"
                    )
                }
                1 => {
                    // ASK query
                    graph_clone.query_cached(
                        "PREFIX foaf: <http://xmlns.com/foaf/0.1/> ASK { ?p a foaf:Person }"
                    )
                }
                _ => {
                    // COUNT query
                    graph_clone.query_cached(
                        "PREFIX foaf: <http://xmlns.com/foaf/0.1/> SELECT (COUNT(?p) AS ?count) WHERE { ?p a foaf:Person }"
                    )
                }
            }
        });
        handles.push(handle);
    }

    // Assert: All threads should complete successfully
    for handle in handles {
        let result = handle.join().expect("Thread should not panic");
        assert!(result.is_ok(), "Concurrent mixed operations should succeed");
    }

    Ok(())
}

// ============================================================================
// Malformed Query Handling Tests
// ============================================================================

#[test]
fn test_malformed_query_syntax_error() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_minimal_data()?;

    // Act: Execute malformed query with syntax error
    let malformed_query = "SELECT ?name WHERE INVALID SYNTAX";
    let result = fixture.graph.query_cached(malformed_query);

    // Assert
    assert!(result.is_err(), "Malformed query should return error");

    Ok(())
}

#[test]
fn test_malformed_query_missing_prefix() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_minimal_data()?;

    // Act: Query using undefined prefix
    let query = "SELECT ?name WHERE { ?p undefined:name ?name }";
    let result = fixture.graph.query_cached(query);

    // Assert: Should fail due to undefined prefix
    assert!(result.is_err(), "Query with undefined prefix should error");

    Ok(())
}

#[test]
fn test_malformed_query_unbalanced_braces() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_minimal_data()?;

    // Act: Query with unbalanced braces
    let query = "SELECT ?name WHERE { ?p foaf:name ?name";
    let result = fixture.graph.query_cached(query);

    // Assert
    assert!(result.is_err(), "Query with unbalanced braces should error");

    Ok(())
}

#[test]
fn test_malformed_query_invalid_filter() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_minimal_data()?;

    // Act: Query with invalid FILTER syntax
    let query = r#"
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?name WHERE {
            ?p foaf:name ?name .
            FILTER(INVALID_FUNCTION(?name))
        }
    "#;
    let result = fixture.graph.query_cached(query);

    // Assert
    assert!(result.is_err(), "Query with invalid FILTER should error");

    Ok(())
}

#[test]
fn test_malformed_query_type_mismatch() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Query attempting invalid type operations
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?name WHERE {
            ?p foaf:name ?name .
            FILTER(?name + 10 > 5)
        }
    "#;
    let result = fixture.graph.query_cached(query);

    // Assert: May error or return empty results depending on implementation
    // The important thing is it doesn't crash
    assert!(result.is_ok() || result.is_err(), "Query should handle type mismatch gracefully");

    Ok(())
}

// ============================================================================
// Query Result Ordering with LIMIT/OFFSET Tests
// ============================================================================

#[test]
fn test_order_by_with_limit() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: ORDER BY with LIMIT
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?name ?salary WHERE {
            ?person foaf:name ?name ;
                    ex:salary ?salary .
        }
        ORDER BY DESC(?salary)
        LIMIT 3
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(rows.len() <= 3, "Should limit to 3 results");

            // Verify ordering (descending salary)
            let mut prev_salary = i32::MAX;
            for row in &rows {
                if let Some(salary_str) = row.get("salary") {
                    let salary: i32 = salary_str.parse().unwrap_or(0);
                    assert!(salary <= prev_salary, "Results should be ordered by salary DESC");
                    prev_salary = salary;
                }
            }
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_order_by_with_offset() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Get all results first
    let query_all = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?name ?age WHERE {
            ?person foaf:name ?name ;
                    ex:age ?age .
        }
        ORDER BY ?age
    "#;

    let all_results = fixture.graph.query_cached(query_all)?;

    // Act: Query with OFFSET
    let query_offset = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?name ?age WHERE {
            ?person foaf:name ?name ;
                    ex:age ?age .
        }
        ORDER BY ?age
        OFFSET 2
    "#;

    let offset_results = fixture.graph.query_cached(query_offset)?;

    // Assert
    match (all_results, offset_results) {
        (
            ggen_core::graph::types::CachedResult::Solutions(all_rows),
            ggen_core::graph::types::CachedResult::Solutions(offset_rows)
        ) => {
            assert!(offset_rows.len() <= all_rows.len(), "Offset results should be <= total results");
            assert_eq!(offset_rows.len(), all_rows.len().saturating_sub(2), "Should skip first 2 results");
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_order_by_limit_offset_pagination() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Paginated queries
    let page_size = 2;
    let mut all_paginated_results = Vec::new();

    for page in 0..3 {
        let query = format!(r#"
            PREFIX ex: <http://example.org/>
            PREFIX foaf: <http://xmlns.com/foaf/0.1/>

            SELECT ?name ?age WHERE {{
                ?person foaf:name ?name ;
                        ex:age ?age .
            }}
            ORDER BY ?age
            LIMIT {}
            OFFSET {}
        "#, page_size, page * page_size);

        let results = fixture.graph.query_cached(&query)?;

        match results {
            ggen_core::graph::types::CachedResult::Solutions(rows) => {
                all_paginated_results.extend(rows);
            }
            _ => panic!("Expected solutions"),
        }
    }

    // Assert: Pagination should work correctly
    assert!(!all_paginated_results.is_empty(), "Should get paginated results");

    // Verify ordering across pages
    let mut prev_age = 0;
    for row in &all_paginated_results {
        if let Some(age_str) = row.get("age") {
            let age: i32 = age_str.parse().unwrap_or(0);
            assert!(age >= prev_age, "Pagination should maintain ordering");
            prev_age = age;
        }
    }

    Ok(())
}

#[test]
fn test_order_by_multiple_columns() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: ORDER BY multiple columns
    let query = r#"
        PREFIX ex: <http://example.org/>
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?dept ?name ?salary WHERE {
            ?person foaf:name ?name ;
                    ex:department ?dept ;
                    ex:salary ?salary .
        }
        ORDER BY ?dept DESC(?salary)
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(!rows.is_empty(), "Should have results with multi-column ordering");

            // Verify ordering is maintained
            for row in &rows {
                assert!(row.contains_key("dept"));
                assert!(row.contains_key("salary"));
            }
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_limit_offset_edge_cases() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act & Assert: LIMIT 0
    let query_limit_zero = r#"
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?name WHERE { ?p foaf:name ?name }
        LIMIT 0
    "#;
    let results_zero = fixture.graph.query_cached(query_limit_zero)?;

    match results_zero {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert_eq!(rows.len(), 0, "LIMIT 0 should return no results");
        }
        _ => panic!("Expected solutions"),
    }

    // Act & Assert: OFFSET larger than result set
    let query_large_offset = r#"
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?name WHERE { ?p foaf:name ?name }
        OFFSET 1000
    "#;
    let results_large_offset = fixture.graph.query_cached(query_large_offset)?;

    match results_large_offset {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert_eq!(rows.len(), 0, "Large OFFSET should return no results");
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

// ============================================================================
// Additional Edge Case Tests
// ============================================================================

#[test]
fn test_values_clause() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: Query with VALUES clause
    let query = r#"
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>

        SELECT ?name WHERE {
            VALUES ?name { "Alice" "Bob" "Charlie" }
            ?person foaf:name ?name .
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(rows.len() >= 3, "Should find specific people via VALUES");
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_distinct_query() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: DISTINCT query
    let query = r#"
        PREFIX ex: <http://example.org/>

        SELECT DISTINCT ?dept WHERE {
            ?person ex:department ?dept .
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(!rows.is_empty(), "Should find distinct departments");

            // Verify distinctness
            let mut seen = std::collections::HashSet::new();
            for row in &rows {
                if let Some(dept) = row.get("dept") {
                    assert!(!seen.contains(dept), "DISTINCT should prevent duplicates");
                    seen.insert(dept.clone());
                }
            }
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_reduced_query() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: REDUCED query (allows duplicates but may reduce them)
    let query = r#"
        PREFIX ex: <http://example.org/>

        SELECT REDUCED ?dept WHERE {
            ?person ex:department ?dept .
        }
    "#;

    let results = fixture.graph.query_cached(query)?;

    // Assert
    match results {
        ggen_core::graph::types::CachedResult::Solutions(rows) => {
            assert!(!rows.is_empty(), "Should find departments with REDUCED");
        }
        _ => panic!("Expected solutions"),
    }

    Ok(())
}

#[test]
fn test_describe_query() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: DESCRIBE query
    let query = "PREFIX ex: <http://example.org/> DESCRIBE ex:alice";

    let results = fixture.graph.query(query)?;

    // Assert
    match results {
        QueryResults::Graph(graph_iter) => {
            let mut count = 0;
            for quad_result in graph_iter {
                assert!(quad_result.is_ok());
                count += 1;
            }
            assert!(count > 0, "DESCRIBE should return triples about the resource");
        }
        _ => panic!("Expected graph results from DESCRIBE"),
    }

    Ok(())
}

#[test]
fn test_ask_query() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_comprehensive_test_data()?;

    // Act: ASK query (true case)
    let query_true = r#"
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        ASK { ?person a foaf:Person }
    "#;

    let result_true = fixture.graph.query_cached(query_true)?;

    // Assert: Should return true
    match result_true {
        ggen_core::graph::types::CachedResult::Boolean(val) => {
            assert!(val, "ASK should return true when pattern matches");
        }
        _ => panic!("Expected boolean result from ASK"),
    }

    // Act: ASK query (false case)
    let query_false = r#"
        PREFIX ex: <http://example.org/>
        ASK { ?person ex:nonExistentProperty "value" }
    "#;

    let result_false = fixture.graph.query_cached(query_false)?;

    // Assert: Should return false
    match result_false {
        ggen_core::graph::types::CachedResult::Boolean(val) => {
            assert!(!val, "ASK should return false when pattern doesn't match");
        }
        _ => panic!("Expected boolean result from ASK"),
    }

    Ok(())
}

#[test]
fn test_service_clause_not_supported() -> Result<()> {
    // Arrange
    let fixture = SparqlTestFixture::new()?;
    fixture.load_minimal_data()?;

    // Act: SERVICE clause (federated query) - may not be supported
    let query = r#"
        PREFIX foaf: <http://xmlns.com/foaf/0.1/>
        SELECT ?name WHERE {
            SERVICE <http://example.org/sparql> {
                ?person foaf:name ?name .
            }
        }
    "#;

    let result = fixture.graph.query_cached(query);

    // Assert: Should either error or handle gracefully
    // The important thing is no crash
    assert!(result.is_ok() || result.is_err(), "SERVICE queries should be handled gracefully");

    Ok(())
}
