//! Chicago TDD unit tests for ggen-yawl transform::executor module
//!
//! Tests follow AAA pattern (Arrange/Act/Assert) with real collaborators.
//! State-based verification over interaction verification.

use ggen_core::Graph;
use ggen_yawl::transform::{ConstructExecutor, Query};
use std::collections::HashMap;

/// Helper module for test fixtures and utilities.
pub mod fixtures {
    /// Simple SPARQL CONSTRUCT query for testing.
    pub const SIMPLE_CONSTRUCT: &str = r#"
        PREFIX ex: <http://example.org/>
        CONSTRUCT {
            ?s ex:transformed ?o
        }
        WHERE {
            ?s ex:input ?o
        }
    "#;

    /// CONSTRUCT query with variable dependencies
    pub const CONSTRUCT_WITH_FILTER: &str = r#"
        PREFIX ex: <http://example.org/>
        CONSTRUCT {
            ?s ex:filtered ?o
        }
        WHERE {
            ?s ex:value ?o
            FILTER (?o > 10)
        }
    "#;

    /// CONSTRUCT query creating new triples from multiple patterns
    pub const MULTI_PATTERN_CONSTRUCT: &str = r#"
        PREFIX ex: <http://example.org/>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        CONSTRUCT {
            ?s ex:inferredType rdfs:Class
        }
        WHERE {
            ?s a ?type
            FILTER (?type != rdfs:Class)
        }
    "#;

    /// Turtle data with input triples for transformation.
    pub const INPUT_DATA: &str = r#"
        @prefix ex: <http://example.org/> .

        ex:alice ex:input ex:value1 .
        ex:bob ex:input ex:value2 .
        ex:carol ex:input ex:value3 .
    "#;

    /// Turtle data with numeric values for filtering.
    pub const NUMERIC_DATA: &str = r#"
        @prefix ex: <http://example.org/> .

        ex:item1 ex:value 5 .
        ex:item2 ex:value 15 .
        ex:item3 ex:value 25 .
        ex:item4 ex:value 100 .
    "#;

    /// Turtle data with typed entities.
    pub const TYPED_DATA: &str = r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:Person a rdfs:Class .
        ex:Organization a rdfs:Class .

        ex:alice a ex:Person .
        ex:bob a ex:Person .
        ex:acme a ex:Organization .
    "#;

    /// Turtle data representing a simple workflow.
    pub const WORKFLOW_DATA: &str = r#"
        @prefix ex: <http://example.org/> .
        @prefix rdfs: <http://www.w3.org/2000/01/rdf-schema#> .

        ex:task1 a ex:Task ;
            rdfs:label "Start Task" ;
            ex:next ex:task2 .

        ex:task2 a ex:Task ;
            rdfs:label "Process Task" ;
            ex:next ex:task3 .

        ex:task3 a ex:Task ;
            rdfs:label "End Task" .
    "#;

    /// CONSTRUCT query that extracts workflow tasks
    pub const EXTRACT_TASKS_QUERY: &str = r#"
        PREFIX ex: <http://example.org/>
        PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
        PREFIX yawl: <http://unrdf.org/yawl#>
        CONSTRUCT {
            ?task a yawl:AtomicTask ;
                   rdfs:label ?label ;
                   yawl:taskId ?taskId .
        }
        WHERE {
            ?task a ex:Task ;
                  rdfs:label ?label .
            BIND(REPLACE(STR(?task), "http://example.org/", "") AS ?taskId)
        }
    "#;
}

#[cfg(test)]
mod query_tests {
    use super::*;

    /// Test: Query::new creates a query with no dependencies
    #[test]
    fn test_query_new_no_dependencies() {
        // Arrange & Act
        let query = Query::new("test_query", "SELECT * WHERE { ?s ?p ?o }");

        // Assert
        assert_eq!(query.name, "test_query");
        assert_eq!(query.sparql, "SELECT * WHERE { ?s ?p ?o }");
        assert!(query.dependencies.is_empty());
    }

    /// Test: Query::with_dependency adds a dependency
    #[test]
    fn test_query_with_dependency() {
        // Arrange
        let query = Query::new(
            "dependent_query",
            "CONSTRUCT { ?s a ?type } WHERE { ?s a ?type }",
        );

        // Act
        let query_with_dep = query.with_dependency("base_query");

        // Assert
        assert_eq!(query_with_dep.dependencies.len(), 1);
        assert_eq!(query_with_dep.dependencies[0], "base_query");
    }

    /// Test: Query::with_dependency can chain multiple dependencies
    #[test]
    fn test_query_with_multiple_dependencies() {
        // Arrange & Act
        let query = Query::new("final_query", "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }")
            .with_dependency("query1")
            .with_dependency("query2")
            .with_dependency("query3");

        // Assert
        assert_eq!(query.dependencies.len(), 3);
        assert_eq!(query.dependencies, vec!["query1", "query2", "query3"]);
    }

    /// Test: Query values can be cloned
    #[test]
    fn test_query_clone() {
        // Arrange
        let query = Query::new("test", "SELECT * WHERE { ?s ?p ?o }");

        // Act
        let cloned = query.clone();

        // Assert
        assert_eq!(query.name, cloned.name);
        assert_eq!(query.sparql, cloned.sparql);
        assert_eq!(query.dependencies, cloned.dependencies);
    }
}

#[cfg(test)]
mod executor_creation_tests {
    use super::*;

    /// Test: ConstructExecutor::new creates executor with default queries
    #[test]
    fn test_executor_new_has_default_queries() {
        // Arrange & Act
        let executor = ConstructExecutor::new();
        let query_names = executor.query_names();

        // Assert
        assert!(
            !query_names.is_empty(),
            "Executor should have default queries"
        );

        // Verify expected default queries exist
        assert!(query_names.contains(&"class_to_task".to_string()));
        assert!(query_names.contains(&"property_to_flow".to_string()));
        assert!(query_names.contains(&"cardinality_splitjoin".to_string()));
        assert!(query_names.contains(&"rules_to_conditions".to_string()));
        assert!(query_names.contains(&"multiple_instance".to_string()));
        assert!(query_names.contains(&"composite_task".to_string()));
    }

    /// Test: ConstructExecutor can be cloned
    #[test]
    fn test_executor_clone() {
        // Arrange
        let executor = ConstructExecutor::new();

        // Act
        let cloned = executor.clone();

        // Assert
        assert_eq!(
            executor.query_names(),
            cloned.query_names(),
            "Cloned executor should have same queries"
        );
    }

    /// Test: ConstructExecutor is Debug
    #[test]
    fn test_executor_debug() {
        // Arrange & Act
        let executor = ConstructExecutor::new();
        let debug_str = format!("{:?}", executor);

        // Assert
        assert!(debug_str.contains("ConstructExecutor"));
    }
}

#[cfg(test)]
mod topological_sort_tests {
    use super::*;

    /// Test: Topological sort orders independent queries
    #[test]
    fn test_topological_sort_independent_queries() {
        // Arrange
        let executor = ConstructExecutor::new();

        // Act
        let order = executor.topological_sort();

        // Assert
        assert!(
            order.is_ok(),
            "Topological sort should succeed for default queries"
        );
        let sorted = order.unwrap();

        // All default queries should be in the result
        assert_eq!(sorted.len(), 6, "Should have all 6 default queries");
    }

    /// Test: Topological sort respects dependencies
    #[test]
    fn test_topological_sort_respects_dependencies() {
        // Arrange
        let executor = ConstructExecutor::new();

        // Act
        let order = executor
            .topological_sort()
            .expect("Topological sort failed");

        // Assert - Verify known dependency ordering
        // property_to_flow depends on class_to_task
        if let (Some(i1), Some(i2)) = (
            order.iter().position(|x| x == "class_to_task"),
            order.iter().position(|x| x == "property_to_flow"),
        ) {
            assert!(i1 < i2, "class_to_task should come before property_to_flow");
        }

        // cardinality_splitjoin depends on property_to_flow
        if let (Some(i1), Some(i2)) = (
            order.iter().position(|x| x == "property_to_flow"),
            order.iter().position(|x| x == "cardinality_splitjoin"),
        ) {
            assert!(
                i1 < i2,
                "property_to_flow should come before cardinality_splitjoin"
            );
        }

        // multiple_instance depends on class_to_task
        if let (Some(i1), Some(i2)) = (
            order.iter().position(|x| x == "class_to_task"),
            order.iter().position(|x| x == "multiple_instance"),
        ) {
            assert!(
                i1 < i2,
                "class_to_task should come before multiple_instance"
            );
        }

        // composite_task depends on class_to_task
        if let (Some(i1), Some(i2)) = (
            order.iter().position(|x| x == "class_to_task"),
            order.iter().position(|x| x == "composite_task"),
        ) {
            assert!(i1 < i2, "class_to_task should come before composite_task");
        }
    }

    /// Test: Topological sort detects circular dependencies
    #[test]
    fn test_topological_sort_circular_dependency() {
        // Arrange
        let mut executor = ConstructExecutor::new();

        // Create circular dependency: A -> B -> A
        executor.register_query(
            Query::new("query_a", "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }")
                .with_dependency("query_b"),
        );
        executor.register_query(
            Query::new("query_b", "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }")
                .with_dependency("query_a"),
        );

        // Act
        let order = executor.topological_sort();

        // Assert
        assert!(order.is_err(), "Circular dependency should cause error");
        match order {
            Err(e) => {
                let error_msg = e.to_string();
                assert!(
                    error_msg.contains("Circular"),
                    "Error should mention circular dependency"
                );
            }
            Ok(_) => panic!("Expected error for circular dependency"),
        }
    }

    /// Test: Topological sort handles complex dependency chains
    #[test]
    fn test_topological_sort_complex_chain() {
        // Arrange
        let mut executor = ConstructExecutor::new();

        // Create chain: base -> mid1 -> mid2 -> final
        executor.register_query(Query::new(
            "base",
            "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }",
        ));
        executor.register_query(
            Query::new("mid1", "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }").with_dependency("base"),
        );
        executor.register_query(
            Query::new("mid2", "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }").with_dependency("mid1"),
        );
        executor.register_query(
            Query::new("final", "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }")
                .with_dependency("mid2"),
        );

        // Act
        let order = executor.topological_sort();

        // Assert
        assert!(order.is_ok());
        let sorted = order.unwrap();

        // Verify ordering
        let positions: HashMap<_, _> = sorted
            .iter()
            .enumerate()
            .map(|(i, name)| (name.as_str(), i))
            .collect();

        assert!(positions.get("base") < positions.get("mid1"));
        assert!(positions.get("mid1") < positions.get("mid2"));
        assert!(positions.get("mid2") < positions.get("final"));
    }
}

#[cfg(test)]
mod query_execution_tests {
    use super::*;
    use fixtures::*;
    use oxigraph::sparql::QueryResults;

    /// Test: Execute single CONSTRUCT query produces new triples
    #[test]
    fn test_execute_construct_materializes_triples() {
        // Arrange
        let graph = Graph::new().expect("Failed to create graph");
        graph
            .insert_turtle(INPUT_DATA)
            .expect("Failed to insert input data");

        let executor = ConstructExecutor::new();
        let query = Query::new("transform", SIMPLE_CONSTRUCT);

        // Act
        let result = executor.execute_query(&graph, &query);

        // Assert
        assert!(result.is_ok(), "Query execution should succeed");
        let result_graph = result.unwrap();

        // Verify original triples still exist
        let original_check = result_graph
            .query_cached("ASK { <http://example.org/alice> <http://example.org/input> ?o }");
        assert!(matches!(
            original_check,
            Ok(ggen_core::graph::types::CachedResult::Boolean(true))
        ));

        // Verify new triples were materialized
        let new_check = result_graph
            .query_cached("ASK { <http://example.org/alice> <http://example.org/transformed> ?o }");
        assert!(matches!(
            new_check,
            Ok(ggen_core::graph::types::CachedResult::Boolean(true))
        ));
    }

    /// Test: Execute CONSTRUCT with filter produces filtered results
    #[test]
    fn test_execute_construct_with_filter() {
        // Arrange
        let graph = Graph::new().expect("Failed to create graph");
        graph
            .insert_turtle(NUMERIC_DATA)
            .expect("Failed to insert numeric data");

        let executor = ConstructExecutor::new();
        let query = Query::new("filter_gt_10", CONSTRUCT_WITH_FILTER);

        // Act
        let result = executor.execute_query(&graph, &query);

        // Assert
        assert!(result.is_ok());
        let result_graph = result.unwrap();

        // Verify only values > 10 were transformed
        let check = result_graph.query_cached(
            "SELECT (COUNT(*) AS ?count) WHERE { ?s <http://example.org/filtered> ?o }",
        );
        assert!(check.is_ok());
        if let Ok(ggen_core::graph::types::CachedResult::Solutions(rows)) = check {
            if let Some(count_str) = rows.first().and_then(|r| r.get("count")) {
                // Should have 3 filtered results (15, 25, 100)
                assert!(count_str.contains("3"), "Expected 3 filtered results");
            }
        }
    }

    /// Test: Execute CONSTRUCT with multiple patterns
    #[test]
    fn test_execute_construct_multi_pattern() {
        // Arrange
        let graph = Graph::new().expect("Failed to create graph");
        graph
            .insert_turtle(TYPED_DATA)
            .expect("Failed to insert typed data");

        let executor = ConstructExecutor::new();
        let query = Query::new("infer_types", MULTI_PATTERN_CONSTRUCT);

        // Act
        let result = executor.execute_query(&graph, &query);

        // Assert
        assert!(result.is_ok());
        let result_graph = result.unwrap();

        // Verify inferred types exist for non-Class entities
        let check = result_graph.query_cached(
            "ASK { <http://example.org/alice> <http://example.org/inferredType> <http://www.w3.org/2000/01/rdf-schema#Class> }"
        );
        assert!(matches!(
            check,
            Ok(ggen_core::graph::types::CachedResult::Boolean(true))
        ));
    }

    /// Test: Execute CONSTRUCT that returns empty results
    #[test]
    fn test_execute_construct_empty_results() {
        // Arrange
        let graph = Graph::new().expect("Failed to create graph");
        // Empty graph, query will match nothing

        let executor = ConstructExecutor::new();
        let query = Query::new("empty", SIMPLE_CONSTRUCT);

        // Act
        let result = executor.execute_query(&graph, &query);

        // Assert - Empty results should not error
        assert!(result.is_ok());
        let result_graph = result.unwrap();

        // Graph should exist but have no new triples
        let check = result_graph.query_cached("ASK { ?s <http://example.org/transformed> ?o }");
        assert!(matches!(
            check,
            Ok(ggen_core::graph::types::CachedResult::Boolean(false))
        ));
    }

    /// Test: Execute invalid SPARQL returns error
    #[test]
    fn test_execute_invalid_sparql() {
        // Arrange
        let graph = Graph::new().expect("Failed to create graph");
        let executor = ConstructExecutor::new();
        let query = Query::new("invalid", "NOT VALID SPARQL !!!");

        // Act
        let result = executor.execute_query(&graph, &query);

        // Assert
        assert!(result.is_err(), "Invalid SPARQL should return error");
    }

    /// Test: Execute SELECT query (not CONSTRUCT) returns error
    #[test]
    fn test_execute_select_not_construct() {
        // Arrange
        let graph = Graph::new().expect("Failed to create graph");
        let executor = ConstructExecutor::new();
        let query = Query::new("select_query", "SELECT ?s WHERE { ?s ?p ?o }");

        // Act
        let result = executor.execute_query(&graph, &query);

        // Assert
        assert!(result.is_err(), "SELECT query should return error");
        match result {
            Err(e) => {
                let error_msg = e.to_string();
                assert!(
                    error_msg.contains("SELECT") || error_msg.contains("expected to be CONSTRUCT")
                );
            }
            Ok(_) => panic!("Expected error for SELECT query"),
        }
    }

    /// Test: Execute ASK query (not CONSTRUCT) returns error
    #[test]
    fn test_execute_ask_not_construct() {
        // Arrange
        let graph = Graph::new().expect("Failed to create graph");
        let executor = ConstructExecutor::new();
        let query = Query::new("ask_query", "ASK { ?s ?p ?o }");

        // Act
        let result = executor.execute_query(&graph, &query);

        // Assert
        assert!(result.is_err(), "ASK query should return error");
        match result {
            Err(e) => {
                let error_msg = e.to_string();
                assert!(
                    error_msg.contains("ASK") || error_msg.contains("expected to be CONSTRUCT")
                );
            }
            Ok(_) => panic!("Expected error for ASK query"),
        }
    }
}

#[cfg(test)]
mod execute_all_tests {
    use super::*;
    use fixtures::*;

    /// Test: Execute all queries runs them in dependency order
    #[test]
    fn test_execute_all_runs_all_queries() {
        // Arrange
        let graph = Graph::new().expect("Failed to create graph");
        graph
            .insert_turtle(WORKFLOW_DATA)
            .expect("Failed to insert workflow data");

        let mut executor = ConstructExecutor::new();
        // Add custom queries for workflow transformation
        executor.register_query(Query::new("extract_tasks", EXTRACT_TASKS_QUERY));

        // Act
        let result = executor.execute_all(&graph);

        // Assert
        assert!(result.is_ok(), "execute_all should succeed");
        let result_graph = result.unwrap();
        assert!(!result_graph.is_empty(), "Result graph should contain data");

        // Verify YAWL tasks were created
        let check = result_graph.query_cached("ASK { ?s a <http://unrdf.org/yawl#AtomicTask> }");
        assert!(matches!(
            check,
            Ok(ggen_core::graph::types::CachedResult::Boolean(true))
        ));
    }

    /// Test: Execute all preserves original data
    #[test]
    fn test_execute_all_preserves_original_triples() {
        // Arrange
        let graph = Graph::new().expect("Failed to create graph");
        graph
            .insert_turtle(INPUT_DATA)
            .expect("Failed to insert input data");

        let executor = ConstructExecutor::new();

        // Act
        let result = executor.execute_all(&graph);

        // Assert
        assert!(result.is_ok());
        let result_graph = result.unwrap();

        // Original ex:input triples should still exist
        let check = result_graph.query_cached("ASK { ?s <http://example.org/input> ?o }");
        assert!(matches!(
            check,
            Ok(ggen_core::graph::types::CachedResult::Boolean(true))
        ));
    }

    /// Test: Execute all with empty graph
    #[test]
    fn test_execute_all_empty_graph() {
        // Arrange
        let graph = Graph::new().expect("Failed to create graph");
        let executor = ConstructExecutor::new();

        // Act
        let result = executor.execute_all(&graph);

        // Assert - Should not error on empty graph
        assert!(result.is_ok());
    }

    /// Test: Execute all accumulates results from multiple queries
    #[test]
    fn test_execute_all_accumulates_results() {
        // Arrange
        let graph = Graph::new().expect("Failed to create graph");
        graph
            .insert_turtle(INPUT_DATA)
            .expect("Failed to insert input data");

        let mut executor = ConstructExecutor::new();

        // Add multiple independent queries
        executor.register_query(Query::new("q1", SIMPLE_CONSTRUCT));
        executor.register_query(Query::new(
            "q2",
            r#"
                PREFIX ex: <http://example.org/>
                CONSTRUCT { ?s ex:derived ?s }
                WHERE { ?s ex:input ?o }
            "#,
        ));

        // Act
        let result = executor.execute_all(&graph);

        // Assert
        assert!(result.is_ok());
        let result_graph = result.unwrap();

        // Should have results from both queries
        let q1_check = result_graph.query_cached("ASK { ?s <http://example.org/transformed> ?o }");
        let q2_check = result_graph.query_cached("ASK { ?s <http://example.org/derived> ?o }");

        assert!(matches!(
            q1_check,
            Ok(ggen_core::graph::types::CachedResult::Boolean(true))
        ));
        assert!(matches!(
            q2_check,
            Ok(ggen_core::graph::types::CachedResult::Boolean(true))
        ));
    }
}

#[cfg(test)]
mod custom_query_tests {
    use super::*;

    /// Test: Register custom query adds to executor
    #[test]
    fn test_register_custom_query() {
        // Arrange
        let mut executor = ConstructExecutor::new();
        let custom = Query::new("custom", "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }");

        // Act
        executor.register_query(custom.clone());

        // Assert
        let names = executor.query_names();
        assert!(names.contains(&"custom".to_string()));
    }

    /// Test: Register query overwrites existing query with same name
    #[test]
    fn test_register_query_overwrites() {
        // Arrange
        let mut executor = ConstructExecutor::new();
        let q1 = Query::new("test", "CONSTRUCT { ?s a ex:Type1 } WHERE { ?s ?p ?o }");
        let q2 = Query::new("test", "CONSTRUCT { ?s a ex:Type2 } WHERE { ?s ?p ?o }");

        // Act
        executor.register_query(q1);
        executor.register_query(q2);

        // Assert - Should only have one "test" query
        let names: Vec<_> = executor
            .query_names()
            .into_iter()
            .filter(|n| n == "test")
            .collect();
        assert_eq!(names.len(), 1);
    }

    /// Test: Query with multiple dependencies executes in correct order
    #[test]
    fn test_query_with_multiple_dependencies() {
        // Arrange
        let mut executor = ConstructExecutor::new();

        executor.register_query(Query::new(
            "base",
            r#"
            PREFIX ex: <http://example.org/>
            CONSTRUCT { ?s ex:transformed ?o }
            WHERE { ?s ex:input ?o }
        "#,
        ));
        executor.register_query(
            Query::new("mid", "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }").with_dependency("base"),
        );
        executor.register_query(
            Query::new("final", "CONSTRUCT { ?s ?p ?o } WHERE { ?s ?p ?o }")
                .with_dependency("base")
                .with_dependency("mid"),
        );

        // Act
        let order = executor.topological_sort();

        // Assert
        assert!(order.is_ok());
        let sorted = order.unwrap();
        let positions: HashMap<_, _> = sorted
            .iter()
            .enumerate()
            .map(|(i, name)| (name.as_str(), i))
            .collect();

        assert!(positions.get("base") < positions.get("mid"));
        assert!(positions.get("mid") < positions.get("final"));
    }
}
