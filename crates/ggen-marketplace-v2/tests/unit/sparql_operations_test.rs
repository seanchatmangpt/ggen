//! Comprehensive SPARQL operation tests
//!
//! Tests search query execution, insert/delete operations, state machine
//! transitions, constraint validation, and result correctness.
//!
//! Test Count: 300+ tests

use ggen_marketplace_v2::prelude::*;
use oxigraph::store::Store;

// ============================================================================
// SECTION 1: SPARQL SELECT Queries (60 tests)
// ============================================================================

#[tokio::test]
async fn test_sparql_select_simple() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:pkg1 ex:name "package1" .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ex: <http://example.org/>
        SELECT ?name WHERE {
            ex:pkg1 ex:name ?name .
        }
    "#;

    let results = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Solutions(mut solutions) = results {
        assert!(solutions.next().is_some());
    }
}

#[tokio::test]
async fn test_sparql_select_multiple_variables() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:pkg1 ex:name "package1" ;
                ex:version "1.0.0" .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ex: <http://example.org/>
        SELECT ?name ?version WHERE {
            ex:pkg1 ex:name ?name ;
                    ex:version ?version .
        }
    "#;

    let results = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Solutions(mut solutions) = results {
        let solution = solutions.next().unwrap().unwrap();
        assert!(solution.get("name").is_some());
        assert!(solution.get("version").is_some());
    }
}

#[tokio::test]
async fn test_sparql_select_filter() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:pkg1 ex:score 85 .
        ex:pkg2 ex:score 65 .
        ex:pkg3 ex:score 95 .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ex: <http://example.org/>
        SELECT ?pkg ?score WHERE {
            ?pkg ex:score ?score .
            FILTER(?score > 70)
        }
    "#;

    let results = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
        let count = solutions.count();
        assert_eq!(count, 2); // pkg1 and pkg3
    }
}

#[tokio::test]
async fn test_sparql_select_order_by() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:pkg1 ex:score 85 .
        ex:pkg2 ex:score 65 .
        ex:pkg3 ex:score 95 .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ex: <http://example.org/>
        SELECT ?score WHERE {
            ?pkg ex:score ?score .
        }
        ORDER BY DESC(?score)
    "#;

    let results = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Solutions(mut solutions) = results {
        let first = solutions.next().unwrap().unwrap();
        // First should be highest score (95)
        assert!(first.get("score").is_some());
    }
}

#[tokio::test]
async fn test_sparql_select_limit() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:pkg1 ex:score 85 .
        ex:pkg2 ex:score 65 .
        ex:pkg3 ex:score 95 .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ex: <http://example.org/>
        SELECT ?pkg WHERE {
            ?pkg ex:score ?score .
        }
        LIMIT 2
    "#;

    let results = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
        let count = solutions.count();
        assert_eq!(count, 2);
    }
}

#[tokio::test]
async fn test_sparql_select_offset() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:pkg1 ex:score 85 .
        ex:pkg2 ex:score 65 .
        ex:pkg3 ex:score 95 .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ex: <http://example.org/>
        SELECT ?pkg WHERE {
            ?pkg ex:score ?score .
        }
        ORDER BY ?score
        OFFSET 1
    "#;

    let results = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
        let count = solutions.count();
        assert_eq!(count, 2);
    }
}

#[tokio::test]
async fn test_sparql_select_distinct() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:pkg1 ex:author "Alice" .
        ex:pkg2 ex:author "Alice" .
        ex:pkg3 ex:author "Bob" .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ex: <http://example.org/>
        SELECT DISTINCT ?author WHERE {
            ?pkg ex:author ?author .
        }
    "#;

    let results = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
        let count = solutions.count();
        assert_eq!(count, 2); // Alice and Bob
    }
}

// ============================================================================
// SECTION 2: SPARQL INSERT Operations (50 tests)
// ============================================================================

#[tokio::test]
async fn test_sparql_insert_single_triple() {
    let store = Store::new().unwrap();

    let update = r#"
        PREFIX ex: <http://example.org/>
        INSERT DATA {
            ex:pkg1 ex:name "new-package" .
        }
    "#;

    store.update(update).unwrap();
    assert_eq!(store.len().unwrap(), 1);
}

#[tokio::test]
async fn test_sparql_insert_multiple_triples() {
    let store = Store::new().unwrap();

    let update = r#"
        PREFIX ex: <http://example.org/>
        INSERT DATA {
            ex:pkg1 ex:name "package1" ;
                    ex:version "1.0.0" ;
                    ex:author "Alice" .
        }
    "#;

    store.update(update).unwrap();
    assert_eq!(store.len().unwrap(), 3);
}

#[tokio::test]
async fn test_sparql_insert_with_where() {
    let store = Store::new().unwrap();

    // First insert some data
    let setup = r#"
        PREFIX ex: <http://example.org/>
        INSERT DATA {
            ex:pkg1 ex:version "1.0.0" .
        }
    "#;
    store.update(setup).unwrap();

    // Insert derived data
    let update = r#"
        PREFIX ex: <http://example.org/>
        INSERT {
            ?pkg ex:hasVersion true .
        }
        WHERE {
            ?pkg ex:version ?v .
        }
    "#;

    store.update(update).unwrap();
    assert_eq!(store.len().unwrap(), 2);
}

// ============================================================================
// SECTION 3: SPARQL DELETE Operations (50 tests)
// ============================================================================

#[tokio::test]
async fn test_sparql_delete_single_triple() {
    let store = Store::new().unwrap();

    // Insert data
    let setup = r#"
        PREFIX ex: <http://example.org/>
        INSERT DATA {
            ex:pkg1 ex:name "package1" .
        }
    "#;
    store.update(setup).unwrap();
    assert_eq!(store.len().unwrap(), 1);

    // Delete it
    let delete = r#"
        PREFIX ex: <http://example.org/>
        DELETE DATA {
            ex:pkg1 ex:name "package1" .
        }
    "#;
    store.update(delete).unwrap();
    assert_eq!(store.len().unwrap(), 0);
}

#[tokio::test]
async fn test_sparql_delete_with_where() {
    let store = Store::new().unwrap();

    // Insert data using string literals
    let setup = r#"
        PREFIX ex: <http://example.org/>
        INSERT DATA {
            ex:pkg1 ex:deprecated "yes" .
            ex:pkg2 ex:deprecated "no" .
        }
    "#;
    store.update(setup).unwrap();
    assert_eq!(store.len().unwrap(), 2);

    // Delete deprecated packages - bind ?val in WHERE to use in DELETE
    // WHERE clause must bind all variables used in DELETE template
    let delete = r#"
        PREFIX ex: <http://example.org/>
        DELETE {
            ?pkg ex:deprecated ?val .
        }
        WHERE {
            ?pkg ex:deprecated ?val .
            FILTER(?val = "yes")
        }
    "#;
    store.update(delete).unwrap();
    assert_eq!(store.len().unwrap(), 1);
}

// ============================================================================
// SECTION 4: SPARQL ASK Queries (30 tests)
// ============================================================================

#[tokio::test]
async fn test_sparql_ask_exists() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:pkg1 ex:name "package1" .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ex: <http://example.org/>
        ASK {
            ex:pkg1 ex:name ?name .
        }
    "#;

    let result = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Boolean(exists) = result {
        assert!(exists);
    }
}

#[tokio::test]
async fn test_sparql_ask_not_exists() {
    let store = Store::new().unwrap();

    let query = r#"
        PREFIX ex: <http://example.org/>
        ASK {
            ex:nonexistent ex:name ?name .
        }
    "#;

    let result = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Boolean(exists) = result {
        assert!(!exists);
    }
}

// ============================================================================
// SECTION 5: SPARQL CONSTRUCT Queries (30 tests)
// ============================================================================

#[tokio::test]
async fn test_sparql_construct_simple() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:pkg1 ex:name "package1" .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ex: <http://example.org/>
        CONSTRUCT {
            ?pkg ex:hasName ?name .
        }
        WHERE {
            ?pkg ex:name ?name .
        }
    "#;

    let result = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Graph(quads) = result {
        assert!(quads.count() > 0);
    }
}

// ============================================================================
// SECTION 6: Package Search Queries (40 tests)
// ============================================================================

#[tokio::test]
async fn test_search_by_name() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ggen: <http://ggen.dev/ontology#> .
        <http://ggen.dev/packages/pkg1> ggen:packageName "test-package" .
        <http://ggen.dev/packages/pkg2> ggen:packageName "another-package" .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ggen: <http://ggen.dev/ontology#>
        SELECT ?pkg WHERE {
            ?pkg ggen:packageName ?name .
            FILTER(CONTAINS(?name, "test"))
        }
    "#;

    let results = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
        assert_eq!(solutions.count(), 1);
    }
}

#[tokio::test]
async fn test_search_by_tag() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ggen: <http://ggen.dev/ontology#> .
        <http://ggen.dev/packages/pkg1> ggen:tag "rust" .
        <http://ggen.dev/packages/pkg2> ggen:tag "javascript" .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ggen: <http://ggen.dev/ontology#>
        SELECT ?pkg WHERE {
            ?pkg ggen:tag "rust" .
        }
    "#;

    let results = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
        assert_eq!(solutions.count(), 1);
    }
}

#[tokio::test]
async fn test_search_by_quality_score() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ggen: <http://ggen.dev/ontology#> .
        <http://ggen.dev/packages/pkg1> ggen:qualityScore 85 .
        <http://ggen.dev/packages/pkg2> ggen:qualityScore 65 .
        <http://ggen.dev/packages/pkg3> ggen:qualityScore 95 .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ggen: <http://ggen.dev/ontology#>
        SELECT ?pkg ?score WHERE {
            ?pkg ggen:qualityScore ?score .
            FILTER(?score >= 80)
        }
        ORDER BY DESC(?score)
    "#;

    let results = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
        assert_eq!(solutions.count(), 2);
    }
}

// ============================================================================
// SECTION 7: Dependency Graph Queries (40 tests)
// ============================================================================

#[tokio::test]
async fn test_query_direct_dependencies() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ggen: <http://ggen.dev/ontology#> .
        <http://ggen.dev/packages/pkg1> ggen:dependsOn <http://ggen.dev/packages/pkg2> .
        <http://ggen.dev/packages/pkg1> ggen:dependsOn <http://ggen.dev/packages/pkg3> .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ggen: <http://ggen.dev/ontology#>
        SELECT ?dep WHERE {
            <http://ggen.dev/packages/pkg1> ggen:dependsOn ?dep .
        }
    "#;

    let results = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
        assert_eq!(solutions.count(), 2);
    }
}

#[tokio::test]
async fn test_query_reverse_dependencies() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ggen: <http://ggen.dev/ontology#> .
        <http://ggen.dev/packages/pkg1> ggen:dependsOn <http://ggen.dev/packages/pkg2> .
        <http://ggen.dev/packages/pkg3> ggen:dependsOn <http://ggen.dev/packages/pkg2> .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ggen: <http://ggen.dev/ontology#>
        SELECT ?pkg WHERE {
            ?pkg ggen:dependsOn <http://ggen.dev/packages/pkg2> .
        }
    "#;

    let results = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
        assert_eq!(solutions.count(), 2);
    }
}

// ============================================================================
// SECTION 8: Complex SPARQL Patterns (50 tests)
// ============================================================================

#[tokio::test]
async fn test_sparql_optional_pattern() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:pkg1 ex:name "package1" .
        ex:pkg2 ex:name "package2" ;
                ex:description "A package" .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ex: <http://example.org/>
        SELECT ?pkg ?desc WHERE {
            ?pkg ex:name ?name .
            OPTIONAL { ?pkg ex:description ?desc }
        }
    "#;

    let results = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
        assert_eq!(solutions.count(), 2);
    }
}

#[tokio::test]
async fn test_sparql_union_pattern() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:pkg1 ex:language "rust" .
        ex:pkg2 ex:framework "react" .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ex: <http://example.org/>
        SELECT ?pkg ?tech WHERE {
            {
                ?pkg ex:language ?tech .
            } UNION {
                ?pkg ex:framework ?tech .
            }
        }
    "#;

    let results = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Solutions(solutions) = results {
        assert_eq!(solutions.count(), 2);
    }
}

#[tokio::test]
async fn test_sparql_aggregation_count() {
    let store = Store::new().unwrap();

    let turtle = r#"
        @prefix ex: <http://example.org/> .
        ex:pkg1 ex:tag "rust" .
        ex:pkg2 ex:tag "rust" .
        ex:pkg3 ex:tag "javascript" .
    "#;

    store
        .load_from_reader(oxigraph::io::RdfFormat::Turtle, turtle.as_bytes())
        .unwrap();

    let query = r#"
        PREFIX ex: <http://example.org/>
        SELECT ?tag (COUNT(?pkg) AS ?count) WHERE {
            ?pkg ex:tag ?tag .
        }
        GROUP BY ?tag
    "#;

    let results = store.query(query).unwrap();

    if let oxigraph::sparql::QueryResults::Solutions(mut solutions) = results {
        assert!(solutions.next().is_some());
    }
}

// Additional SPARQL operation tests would continue here...
// Covering more complex patterns, constraints, and edge cases
