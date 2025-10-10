// Integration tests for critical ggen-mcp tools
// Testing priority: project_gen > market_search > graph_query (80/20 rule)
//
// This test suite directly tests the tool implementations without going through
// the MCP protocol layer, focusing on business logic correctness.

use serde_json::{json, Value};

// Import the tool modules directly
use ggen_mcp::tools::{project, market, graph};
use ggen_mcp::error::{GgenMcpError, Result};

// Helper to validate success response structure
fn assert_success_response(result: &Value) {
    // Check for "success" field which can be either true boolean or "success" string
    let has_success = result.get("success").and_then(|v| v.as_bool()).unwrap_or(false)
        || result.get("status").and_then(|v| v.as_str()) == Some("success");

    assert!(has_success, "Expected success response, got: {:?}", result);
    assert!(result.get("data").is_some(), "Missing data field in response");
}

// Helper to validate error response
fn assert_error_response<T>(result: Result<T>) {
    assert!(result.is_err(), "Expected error, but got Ok");
}

// ============================================================================
// PROJECT_GEN TESTS (HIGHEST PRIORITY - Core functionality)
// ============================================================================

#[tokio::test]
async fn test_project_gen_with_valid_params() {
    let params = json!({
        "template": "test-template",
        "output_dir": "/tmp/test-output",
        "vars": {
            "name": "test-project",
            "version": "1.0.0"
        }
    });

    let result = project::gen(params).await;

    assert!(result.is_ok(), "project_gen should succeed with valid params");
    let response = result.unwrap();

    assert_success_response(&response);

    let data = response.get("data").unwrap();
    assert!(data.get("files_created").is_some(), "Should return files_created");
    assert!(data.get("execution_time_ms").is_some(), "Should return execution time");
}

#[tokio::test]
async fn test_project_gen_with_variables_substitution() {
    let params = json!({
        "template": "rust-api",
        "output_dir": "/tmp/rust-project",
        "vars": {
            "project_name": "my_api",
            "author": "Test User",
            "license": "MIT"
        }
    });

    let result = project::gen(params).await;
    assert!(result.is_ok());

    let response = result.unwrap();
    let data = response.get("data").unwrap();

    // Verify variables were recorded
    let vars_used = data.get("variables_used").and_then(|v| v.as_array());
    assert!(vars_used.is_some());
    assert!(vars_used.unwrap().len() > 0, "Should track used variables");
}

#[tokio::test]
async fn test_project_gen_missing_template_param() {
    let params = json!({
        "output_dir": "/tmp/output",
        "vars": {}
    });

    let result = project::gen(params).await;
    assert_error_response(result);
}

#[tokio::test]
async fn test_project_gen_with_dry_run() {
    let params = json!({
        "template": "test-template",
        "output_dir": "/tmp/dry-run",
        "dry_run": true,
        "vars": {}
    });

    let result = project::gen(params).await;
    assert!(result.is_ok());

    let response = result.unwrap();
    let data = response.get("data").unwrap();

    assert_eq!(
        data.get("dry_run").and_then(|v| v.as_bool()),
        Some(true),
        "Should respect dry_run flag"
    );
}

#[tokio::test]
async fn test_project_gen_with_force_flag() {
    let params = json!({
        "template": "test-template",
        "output_dir": "/tmp/force-test",
        "force": true,
        "vars": {}
    });

    let result = project::gen(params).await;
    assert!(result.is_ok());

    let response = result.unwrap();
    let data = response.get("data").unwrap();

    assert_eq!(
        data.get("force").and_then(|v| v.as_bool()),
        Some(true),
        "Should respect force flag"
    );
}

#[tokio::test]
async fn test_project_gen_response_format() {
    let params = json!({
        "template": "test",
        "vars": {}
    });

    let result = project::gen(params).await.unwrap();
    let data = result.get("data").unwrap();

    // Verify response contains expected fields
    assert!(data.get("template").is_some());
    assert!(data.get("output_dir").is_some());
    assert!(data.get("files_created").is_some());
    assert!(data.get("files_modified").is_some());
    assert!(data.get("variables_used").is_some());
    assert!(data.get("execution_time_ms").is_some());
}

// ============================================================================
// MARKET_SEARCH TESTS (HIGH PRIORITY - Discovery functionality)
// ============================================================================

#[tokio::test]
async fn test_market_search_basic_query() {
    let params = json!({
        "query": "rust api"
    });

    let result = market::search(params).await;

    assert!(result.is_ok(), "market_search should succeed with basic query");
    let response = result.unwrap();

    assert_success_response(&response);

    let data = response.get("data").unwrap();
    assert!(data.get("results").is_some());
    assert!(data.get("total").is_some());
}

#[tokio::test]
async fn test_market_search_with_filters() {
    let params = json!({
        "query": "api",
        "category": "backend",
        "min_stars": 10,
        "min_downloads": 100,
        "license": "MIT"
    });

    let result = market::search(params).await;
    assert!(result.is_ok());

    let response = result.unwrap();
    let data = response.get("data").unwrap();

    // Verify filters are recorded
    let filters = data.get("filters").unwrap();
    assert!(filters.get("category").is_some());
    assert!(filters.get("min_stars").is_some());
    assert!(filters.get("min_downloads").is_some());
}

#[tokio::test]
async fn test_market_search_with_author_filter() {
    let params = json!({
        "query": "template",
        "author": "ggen-team"
    });

    let result = market::search(params).await;
    assert!(result.is_ok());

    let response = result.unwrap();
    let data = response.get("data").unwrap();
    let filters = data.get("filters").unwrap();

    assert_eq!(
        filters.get("author").and_then(|v| v.as_str()),
        Some("ggen-team")
    );
}

#[tokio::test]
async fn test_market_search_missing_query_param() {
    let params = json!({
        "category": "api"
    });

    let result = market::search(params).await;
    assert_error_response(result);
}

#[tokio::test]
async fn test_market_search_with_fuzzy_mode() {
    let params = json!({
        "query": "apii",
        "fuzzy": "true"
    });

    let result = market::search(params).await;
    assert!(result.is_ok());

    let response = result.unwrap();
    let data = response.get("data").unwrap();

    assert_eq!(
        data.get("fuzzy").and_then(|v| v.as_bool()),
        Some(true)
    );
}

#[tokio::test]
async fn test_market_search_response_format() {
    let params = json!({
        "query": "test"
    });

    let result = market::search(params).await.unwrap();
    let data = result.get("data").unwrap();

    // Verify response structure
    assert!(data.get("results").is_some());
    assert!(data.get("total").is_some());
    assert!(data.get("query").is_some());
    assert!(data.get("filters").is_some());
}

// ============================================================================
// GRAPH_QUERY TESTS (MEDIUM PRIORITY - Advanced functionality)
// ============================================================================

#[tokio::test]
async fn test_graph_query_simple_select() {
    let params = json!({
        "sparql": "SELECT * WHERE { ?s ?p ?o }"
    });

    let result = graph::query(params).await;

    assert!(result.is_ok(), "graph_query should succeed with valid SPARQL");
    let response = result.unwrap();

    assert_success_response(&response);

    let data = response.get("data").unwrap();
    assert!(data.get("bindings").is_some());
    assert!(data.get("count").is_some());
}

#[tokio::test]
async fn test_graph_query_with_named_graph() {
    let params = json!({
        "sparql": "SELECT * WHERE { ?s ?p ?o }",
        "graph": "http://example.org/graph1"
    });

    let result = graph::query(params).await;
    assert!(result.is_ok());

    let response = result.unwrap();
    let data = response.get("data").unwrap();

    assert_eq!(
        data.get("graph").and_then(|v| v.as_str()),
        Some("http://example.org/graph1")
    );
}

#[tokio::test]
async fn test_graph_query_missing_sparql_param() {
    let params = json!({
        "graph": "http://example.org/graph1"
    });

    let result = graph::query(params).await;
    assert_error_response(result);
}

#[tokio::test]
async fn test_graph_query_complex_query() {
    let params = json!({
        "sparql": r#"
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            SELECT ?subject ?label
            WHERE {
                ?subject rdf:type ?type .
                ?subject rdfs:label ?label .
            }
            LIMIT 10
        "#
    });

    let result = graph::query(params).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_graph_query_response_format() {
    let params = json!({
        "sparql": "SELECT * WHERE { ?s ?p ?o } LIMIT 1"
    });

    let result = graph::query(params).await.unwrap();
    let data = result.get("data").unwrap();

    // Verify response contains expected fields
    assert!(data.get("query").is_some());
    assert!(data.get("bindings").is_some());
    assert!(data.get("count").is_some());
    assert!(data.get("execution_time_ms").is_some());
}

// ============================================================================
// CONCURRENT EXECUTION TESTS (Performance & Reliability)
// ============================================================================

#[tokio::test]
async fn test_concurrent_project_gen_calls() {
    let mut handles = vec![];

    for i in 0..5 {
        let params = json!({
            "template": format!("template-{}", i),
            "output_dir": format!("/tmp/concurrent-{}", i),
            "vars": {}
        });

        let handle = tokio::spawn(async move {
            project::gen(params).await
        });

        handles.push(handle);
    }

    // Wait for all tasks to complete
    let results = futures::future::join_all(handles).await;

    // Verify all succeeded
    for result in results {
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());
    }
}

#[tokio::test]
async fn test_concurrent_market_search_calls() {
    let queries = vec!["rust", "api", "frontend", "backend", "template"];
    let mut handles = vec![];

    for query in queries {
        let params = json!({
            "query": query
        });

        let handle = tokio::spawn(async move {
            market::search(params).await
        });

        handles.push(handle);
    }

    let results = futures::future::join_all(handles).await;

    // All searches should succeed
    for result in results {
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());
    }
}

#[tokio::test]
async fn test_concurrent_mixed_tool_calls() {
    let mut handles = vec![];

    // Spawn different tool calls concurrently
    handles.push(tokio::spawn(async {
        project::gen(json!({
            "template": "test",
            "vars": {}
        })).await
    }));

    handles.push(tokio::spawn(async {
        market::search(json!({
            "query": "rust"
        })).await
    }));

    handles.push(tokio::spawn(async {
        graph::query(json!({
            "sparql": "SELECT * WHERE { ?s ?p ?o } LIMIT 1"
        })).await
    }));

    let results = futures::future::join_all(handles).await;

    // All should succeed
    for result in results {
        assert!(result.is_ok());
        assert!(result.unwrap().is_ok());
    }
}

// ============================================================================
// ADDITIONAL TOOLS TESTS (Supporting functionality)
// ============================================================================

#[tokio::test]
async fn test_market_list_with_filters() {
    let params = json!({
        "category": "api",
        "limit": 10,
        "sort": "downloads",
        "order": "desc"
    });

    let result = market::list(params).await;
    assert!(result.is_ok());

    let response = result.unwrap();
    assert_success_response(&response);
}

#[tokio::test]
async fn test_market_info_for_package() {
    let params = json!({
        "package_id": "rust-api-template",
        "examples": "true",
        "dependencies": "true",
        "health": "true"
    });

    let result = market::info(params).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_project_plan_generation() {
    let params = json!({
        "template": "test-template",
        "vars": {"name": "test"}
    });

    let result = project::plan(params).await;
    assert!(result.is_ok());

    let response = result.unwrap();
    let data = response.get("data").unwrap();

    assert!(data.get("estimated_files").is_some());
    assert!(data.get("estimated_size_kb").is_some());
}

#[tokio::test]
async fn test_graph_load_rdf_file() {
    let params = json!({
        "file": "/tmp/test.ttl",
        "format": "turtle"
    });

    let result = graph::load(params).await;
    assert!(result.is_ok());
}

#[tokio::test]
async fn test_graph_export_to_file() {
    let params = json!({
        "output": "/tmp/export.ttl",
        "format": "turtle"
    });

    let result = graph::export(params).await;
    assert!(result.is_ok());
}

// ============================================================================
// ERROR HANDLING & EDGE CASES
// ============================================================================

#[tokio::test]
async fn test_empty_parameters() {
    // Tools that require parameters should fail with empty params
    let result = project::gen(json!({})).await;
    assert!(result.is_err());
}

#[tokio::test]
async fn test_malformed_json_parameters() {
    // Test with invalid nested structure
    let params = json!({
        "template": ["invalid", "array", "value"]
    });

    let _result = project::gen(params).await;
    // Should handle gracefully (either succeed with defaults or fail cleanly)
    // The specific behavior depends on implementation
}

#[tokio::test]
async fn test_market_search_with_limit() {
    let params = json!({
        "query": "template",
        "limit": 5
    });

    let result = market::search(params).await;
    assert!(result.is_ok());

    let response = result.unwrap();
    let data = response.get("data").unwrap();

    assert_eq!(data.get("limit").and_then(|v| v.as_u64()), Some(5));
}

// ============================================================================
// PERFORMANCE & TIMEOUT TESTS
// ============================================================================

#[tokio::test]
async fn test_project_gen_execution_time_tracking() {
    let params = json!({
        "template": "test",
        "vars": {}
    });

    let start = std::time::Instant::now();
    let result = project::gen(params).await.unwrap();
    let elapsed = start.elapsed();

    let data = result.get("data").unwrap();
    let execution_time = data.get("execution_time_ms")
        .and_then(|v| v.as_u64())
        .expect("Should have execution time");

    // Execution time should be reasonable
    assert!(execution_time < 5000, "Execution should complete in under 5 seconds");
    // The actual elapsed time should be close to the reported execution time
    // Allow some tolerance for measurement differences
    assert!(elapsed.as_millis() as u64 <= execution_time + 100,
            "Elapsed time {} should be close to reported time {}",
            elapsed.as_millis(), execution_time);
}

#[tokio::test]
async fn test_rapid_sequential_calls() {
    for i in 0..10 {
        let params = json!({
            "query": format!("test-{}", i)
        });

        let result = market::search(params).await;
        assert!(result.is_ok(), "Sequential call {} should succeed", i);
    }
}
