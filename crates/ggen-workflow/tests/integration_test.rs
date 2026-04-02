//! Integration tests for SPARQL condition evaluation in workflow engine
//!
//! This test file verifies that SPARQL queries can be evaluated as workflow conditions
//! using the RDF triple store from ggen_ontology_core.

use ggen_ontology_core::TripleStore;
use ggen_workflow::engine::ConditionEvaluator;
use std::collections::HashMap;
use std::io::Write;
use tempfile::NamedTempFile;

/// Create a test Turtle file with sample RDF data
fn create_test_turtle() -> WorkflowTestResult<(NamedTempFile, TripleStore)> {
    let mut file = NamedTempFile::new()
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e.to_string()))?;

    let turtle_content = r#"
@prefix ex: <http://example.com/> .
@prefix workflow: <http://workflow.example/> .

# Task status data
workflow:task1 workflow:status "completed" .
workflow:task1 workflow:priority 5 .

workflow:task2 workflow:status "pending" .
workflow:task2 workflow:priority 3 .

workflow:task3 workflow:status "failed" .
workflow:task3 workflow:priority 1 .

# Conditional data
ex:condition1 ex:value true .
ex:condition2 ex:value false .

# User permissions
ex:user1 ex:hasPermission ex:readPermission .
ex:user1 ex:hasPermission ex:writePermission .
ex:user2 ex:hasPermission ex:readPermission .
"#;

    file.write_all(turtle_content.as_bytes())
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e.to_string()))?;
    file.flush()
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e.to_string()))?;

    let store = TripleStore::new()
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e.to_string()))?;

    store
        .load_turtle(file.path())
        .map_err(|e| std::io::Error::new(std::io::ErrorKind::Other, e.to_string()))?;

    Ok((file, store))
}

type WorkflowTestResult<T> = Result<T, Box<dyn std::error::Error>>;

#[test]
fn test_sparql_ask_query_true() -> WorkflowTestResult<()> {
    let (_file, store) = create_test_turtle()?;

    let context = HashMap::new();
    let evaluator = ConditionEvaluator::with_triple_store(context, store);

    // Test ASK query that returns true
    let query = r#"
        SPARQL:ASK {
            workflow:task1 workflow:status "completed" .
        }
    "#;

    let result = evaluator.evaluate(query)?;
    assert!(result, "ASK query should return true for completed task");

    Ok(())
}

#[test]
fn test_sparql_ask_query_false() -> WorkflowTestResult<()> {
    let (_file, store) = create_test_turtle()?;

    let context = HashMap::new();
    let evaluator = ConditionEvaluator::with_triple_store(context, store);

    // Test ASK query that returns false
    let query = r#"
        SPARQL:ASK {
            workflow:task999 workflow:status "completed" .
        }
    "#;

    let result = evaluator.evaluate(query)?;
    assert!(!result, "ASK query should return false for non-existent task");

    Ok(())
}

#[test]
fn test_sparql_select_query_with_results() -> WorkflowTestResult<()> {
    let (_file, store) = create_test_turtle()?;

    let context = HashMap::new();
    let evaluator = ConditionEvaluator::with_triple_store(context, store);

    // Test SELECT query that returns results
    let query = r#"
        SPARQL:SELECT ?task WHERE {
            ?task workflow:status "completed" .
        }
    "#;

    let result = evaluator.evaluate(query)?;
    assert!(
        result,
        "SELECT query should return true when results are found"
    );

    Ok(())
}

#[test]
fn test_sparql_select_query_no_results() -> WorkflowTestResult<()> {
    let (_file, store) = create_test_turtle()?;

    let context = HashMap::new();
    let evaluator = ConditionEvaluator::with_triple_store(context, store);

    // Test SELECT query that returns no results
    let query = r#"
        SPARQL:SELECT ?task WHERE {
            ?task workflow:status "non_existent_status" .
        }
    "#;

    let result = evaluator.evaluate(query)?;
    assert!(
        !result,
        "SELECT query should return false when no results are found"
    );

    Ok(())
}

#[test]
fn test_sparql_complex_condition() -> WorkflowTestResult<()> {
    let (_file, store) = create_test_turtle()?;

    let context = HashMap::new();
    let evaluator = ConditionEvaluator::with_triple_store(context, store);

    // Test complex SPARQL condition with filters
    let query = r#"
        SPARQL:ASK {
            ?task workflow:status "completed" ;
                  workflow:priority ?priority .
            FILTER(?priority > 3)
        }
    "#;

    let result = evaluator.evaluate(query)?;
    assert!(
        result,
        "Complex ASK query should return true for completed high-priority task"
    );

    Ok(())
}

#[test]
fn test_sparql_permission_check() -> WorkflowTestResult<()> {
    let (_file, store) = create_test_turtle()?;

    let context = HashMap::new();
    let evaluator = ConditionEvaluator::with_triple_store(context, store);

    // Test permission check
    let query = r#"
        SPARQL:ASK {
            ex:user1 ex:hasPermission ex:writePermission .
        }
    "#;

    let result = evaluator.evaluate(query)?;
    assert!(result, "User should have write permission");

    Ok(())
}

#[test]
fn test_sparql_permission_check_negative() -> WorkflowTestResult<()> {
    let (_file, store) = create_test_turtle()?;

    let context = HashMap::new();
    let evaluator = ConditionEvaluator::with_triple_store(context, store);

    // Test permission check that should fail
    let query = r#"
        SPARQL:ASK {
            ex:user2 ex:hasPermission ex:writePermission .
        }
    "#;

    let result = evaluator.evaluate(query)?;
    assert!(!result, "User2 should not have write permission");

    Ok(())
}

#[test]
fn test_sparql_aggregation_count() -> WorkflowTestResult<()> {
    let (_file, store) = create_test_turtle()?;

    let context = HashMap::new();
    let evaluator = ConditionEvaluator::with_triple_store(context, store);

    // Test COUNT aggregation - should return results (non-empty)
    let query = r#"
        SPARQL:SELECT (COUNT(?task) AS ?count) WHERE {
            ?task workflow:status ?status .
        }
    "#;

    let result = evaluator.evaluate(query)?;
    assert!(
        result,
        "COUNT query should return true (has results)"
    );

    Ok(())
}

#[test]
fn test_sparql_without_triple_store_errors() -> WorkflowTestResult<()> {
    let context = HashMap::new();
    let evaluator = ConditionEvaluator::new(context);

    // Test that SPARQL query without triple store returns error
    let query = r#"
        SPARQL:ASK {
            ?s ?p ?o .
        }
    "#;

    let result = evaluator.evaluate(query);
    assert!(
        result.is_err(),
        "SPARQL query without triple store should return error"
    );

    let err = result.unwrap_err();
    let err_msg = err.to_string();
    assert!(
        err_msg.contains("triple store"),
        "Error message should mention triple store requirement"
    );

    Ok(())
}

#[test]
fn test_sparql_invalid_query_syntax() -> WorkflowTestResult<()> {
    let (_file, store) = create_test_turtle()?;

    let context = HashMap::new();
    let evaluator = ConditionEvaluator::with_triple_store(context, store);

    // Test invalid SPARQL syntax
    let query = "SPARQL:INVALID SYNTAX HERE !!!";

    let result = evaluator.evaluate(query);
    assert!(
        result.is_err(),
        "Invalid SPARQL syntax should return error"
    );

    let err = result.unwrap_err();
    let err_msg = err.to_string();
    assert!(
        err_msg.contains("SPARQL") || err_msg.contains("query"),
        "Error message should mention SPARQL or query"
    );

    Ok(())
}

#[test]
fn test_sparql_optional_pattern() -> WorkflowTestResult<()> {
    let (_file, store) = create_test_turtle()?;

    let context = HashMap::new();
    let evaluator = ConditionEvaluator::with_triple_store(context, store);

    // Test OPTIONAL pattern - should return results
    let query = r#"
        SPARQL:SELECT ?task WHERE {
            ?task workflow:status "completed" .
            OPTIONAL {
                ?task workflow:priority ?priority .
            }
        }
    "#;

    let result = evaluator.evaluate(query)?;
    assert!(
        result,
        "OPTIONAL pattern query should return true when results found"
    );

    Ok(())
}

#[test]
fn test_sparql_union_pattern() -> WorkflowTestResult<()> {
    let (_file, store) = create_test_turtle()?;

    let context = HashMap::new();
    let evaluator = ConditionEvaluator::with_triple_store(context, store);

    // Test UNION pattern - should match either condition
    let query = r#"
        SPARQL:ASK {
            {
                workflow:task1 workflow:status "completed" .
            } UNION {
                workflow:task999 workflow:status "completed" .
            }
        }
    "#;

    let result = evaluator.evaluate(query)?;
    assert!(
        result,
        "UNION query should return true when either side matches"
    );

    Ok(())
}

#[test]
fn test_context_variables_still_work() -> WorkflowTestResult<()> {
    let (_file, store) = create_test_turtle()?;

    let mut context = HashMap::new();
    context.insert("x".to_string(), serde_json::json!(10));
    context.insert("y".to_string(), serde_json::json!(20));

    let evaluator = ConditionEvaluator::with_triple_store(context, store);

    // Test that non-SPARQL conditions still work
    assert!(evaluator.evaluate("x < y")?);
    assert!(evaluator.evaluate("true")?);
    assert!(!evaluator.evaluate("false")?);

    Ok(())
}

#[test]
fn test_mixed_sparql_and_boolean_conditions() -> WorkflowTestResult<()> {
    let (_file, store) = create_test_turtle()?;

    let mut context = HashMap::new();
    context.insert("has_permission".to_string(), serde_json::json!(true));

    let evaluator = ConditionEvaluator::with_triple_store(context, store);

    // Test SPARQL condition
    let sparql_result = evaluator.evaluate(
        r#"
        SPARQL:ASK {
            workflow:task1 workflow:status "completed" .
        }
    "#,
    )?;
    assert!(sparql_result);

    // Test context variable condition
    let context_result = evaluator.evaluate("has_permission")?;
    assert!(context_result);

    // Both should work independently
    Ok(())
}
