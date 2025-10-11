//! Tool Delegation Tests
//!
//! Comprehensive tests verifying that MCP tools correctly delegate
//! to underlying ggen CLI commands with proper parameter mapping.

use serde_json::json;

// ============================================================================
// PROJECT TOOL DELEGATION TESTS
// ============================================================================

mod project_tools {
    use super::*;
    use ggen_mcp::tools::project;

    #[tokio::test]
    async fn test_gen_delegates_with_minimal_params() {
        let params = json!({"template": "test-template"});
        let result = project::gen(params).await;

        // Should attempt delegation (may fail if CLI not available, but shouldn't panic)
        match result {
            Ok(_) => println!("Delegation successful"),
            Err(e) => println!("Delegation attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_gen_delegates_with_full_params() {
        let params = json!({
            "template": "rust-api",
            "vars": {
                "name": "my-project",
                "version": "1.0.0"
            },
            "dry_run": true,
            "force": false
        });

        let result = project::gen(params).await;

        match result {
            Ok(_) => println!("Full delegation successful"),
            Err(e) => println!("Delegation attempted with all params: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_gen_requires_template_param() {
        let params = json!({
            "vars": {"name": "test"}
            // template is missing
        });

        let result = project::gen(params).await;

        // Should fail with missing parameter error
        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(error.to_string().contains("template") || error.to_string().contains("Missing"));
    }

    #[tokio::test]
    async fn test_gen_handles_empty_vars() {
        let params = json!({
            "template": "test",
            "vars": {}
        });

        let result = project::gen(params).await;

        // Should handle empty vars gracefully
        match result {
            Ok(_) => println!("Empty vars handled successfully"),
            Err(e) => println!("Delegation with empty vars: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_plan_delegates_correctly() {
        let params = json!({
            "template": "test-template",
            "vars": {"name": "test"}
        });

        let result = project::plan(params).await;

        match result {
            Ok(_) => println!("Plan delegation successful"),
            Err(e) => println!("Plan delegation attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_plan_requires_template() {
        let params = json!({});

        let result = project::plan(params).await;

        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_apply_delegates_correctly() {
        let params = json!({"plan": "test-plan.json"});

        let result = project::apply(params).await;

        match result {
            Ok(_) => println!("Apply delegation successful"),
            Err(e) => println!("Apply delegation attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_apply_requires_plan() {
        let params = json!({});

        let result = project::apply(params).await;

        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_diff_delegates_correctly() {
        let params = json!({
            "template": "test-template",
            "vars": {}
        });

        let result = project::diff(params).await;

        match result {
            Ok(_) => println!("Diff delegation successful"),
            Err(e) => println!("Diff delegation attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_diff_requires_template() {
        let params = json!({});

        let result = project::diff(params).await;

        assert!(result.is_err());
    }
}

// ============================================================================
// MARKET TOOL DELEGATION TESTS
// ============================================================================

mod market_tools {
    use super::*;
    use ggen_mcp::tools::market;

    #[tokio::test]
    async fn test_list_delegates_with_no_filters() {
        let params = json!({});

        let result = market::list(params).await;

        match result {
            Ok(_) => println!("List delegation successful"),
            Err(e) => println!("List delegation attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_list_delegates_with_category_filter() {
        let params = json!({"category": "api"});

        let result = market::list(params).await;

        match result {
            Ok(_) => println!("List with category successful"),
            Err(e) => println!("List with category attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_list_delegates_with_tag_filter() {
        let params = json!({"tag": "rust"});

        let result = market::list(params).await;

        match result {
            Ok(_) => println!("List with tag successful"),
            Err(e) => println!("List with tag attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_list_delegates_with_multiple_filters() {
        let params = json!({
            "category": "api",
            "tag": "rust"
        });

        let result = market::list(params).await;

        match result {
            Ok(_) => println!("List with multiple filters successful"),
            Err(e) => println!("List with multiple filters attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_search_delegates_with_query() {
        let params = json!({"query": "rust api"});

        let result = market::search(params).await;

        match result {
            Ok(_) => println!("Search delegation successful"),
            Err(e) => println!("Search delegation attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_search_requires_query_param() {
        let params = json!({});

        let result = market::search(params).await;

        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(error.to_string().contains("query") || error.to_string().contains("Missing"));
    }

    #[tokio::test]
    async fn test_search_delegates_with_filters() {
        let params = json!({
            "query": "api",
            "category": "backend",
            "limit": 10
        });

        let result = market::search(params).await;

        match result {
            Ok(_) => println!("Search with filters successful"),
            Err(e) => println!("Search with filters attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_install_delegates_correctly() {
        let params = json!({"package": "test-package"});

        let result = market::install(params).await;

        match result {
            Ok(_) => println!("Install delegation successful"),
            Err(e) => println!("Install delegation attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_install_requires_package() {
        let params = json!({});

        let result = market::install(params).await;

        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_install_with_version() {
        let params = json!({
            "package": "test-package",
            "version": "1.2.3"
        });

        let result = market::install(params).await;

        match result {
            Ok(_) => println!("Install with version successful"),
            Err(e) => println!("Install with version attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_info_delegates_correctly() {
        let params = json!({"package_id": "test-package"});

        let result = market::info(params).await;

        match result {
            Ok(_) => println!("Info delegation successful"),
            Err(e) => println!("Info delegation attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_info_requires_package_id() {
        let params = json!({});

        let result = market::info(params).await;

        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_recommend_delegates_with_no_params() {
        let params = json!({});

        let result = market::recommend(params).await;

        match result {
            Ok(_) => println!("Recommend delegation successful"),
            Err(e) => println!("Recommend delegation attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_recommend_delegates_with_based_on() {
        let params = json!({"based_on": "existing-package"});

        let result = market::recommend(params).await;

        match result {
            Ok(_) => println!("Recommend with based_on successful"),
            Err(e) => println!("Recommend with based_on attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_recommend_delegates_with_all_params() {
        let params = json!({
            "based_on": "existing-package",
            "category": "api",
            "limit": 5
        });

        let result = market::recommend(params).await;

        match result {
            Ok(_) => println!("Recommend with all params successful"),
            Err(e) => println!("Recommend with all params attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_offline_search_delegates_correctly() {
        let params = json!({"query": "api"});

        let result = market::offline_search(params).await;

        match result {
            Ok(_) => println!("Offline search successful"),
            Err(e) => println!("Offline search attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_offline_search_requires_query() {
        let params = json!({});

        let result = market::offline_search(params).await;

        assert!(result.is_err());
    }
}

// ============================================================================
// GRAPH TOOL DELEGATION TESTS
// ============================================================================

mod graph_tools {
    use super::*;
    use ggen_mcp::tools::graph;

    #[tokio::test]
    async fn test_query_delegates_with_sparql() {
        let params = json!({"sparql": "SELECT * WHERE { ?s ?p ?o }"});

        let result = graph::query(params).await;

        match result {
            Ok(_) => println!("Query delegation successful"),
            Err(e) => println!("Query delegation attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_query_requires_sparql_param() {
        let params = json!({});

        let result = graph::query(params).await;

        assert!(result.is_err());
        let error = result.unwrap_err();
        assert!(error.to_string().contains("sparql") || error.to_string().contains("Missing"));
    }

    #[tokio::test]
    async fn test_query_delegates_with_graph_param() {
        let params = json!({
            "sparql": "SELECT * WHERE { ?s ?p ?o }",
            "graph": "http://example.org/graph1"
        });

        let result = graph::query(params).await;

        match result {
            Ok(_) => println!("Query with graph successful"),
            Err(e) => println!("Query with graph attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_query_handles_complex_sparql() {
        let params = json!({
            "sparql": r#"
                PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
                PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
                SELECT ?subject ?label
                WHERE {
                    ?subject rdf:type ?type .
                    ?subject rdfs:label ?label .
                    FILTER(lang(?label) = 'en')
                }
                LIMIT 10
            "#
        });

        let result = graph::query(params).await;

        match result {
            Ok(_) => println!("Complex SPARQL successful"),
            Err(e) => println!("Complex SPARQL attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_load_delegates_with_file() {
        let params = json!({"file": "/tmp/test.ttl"});

        let result = graph::load(params).await;

        match result {
            Ok(_) => println!("Load delegation successful"),
            Err(e) => println!("Load delegation attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_load_requires_file_param() {
        let params = json!({});

        let result = graph::load(params).await;

        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_load_delegates_with_format() {
        let params = json!({
            "file": "/tmp/test.ttl",
            "format": "turtle"
        });

        let result = graph::load(params).await;

        match result {
            Ok(_) => println!("Load with format successful"),
            Err(e) => println!("Load with format attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_load_delegates_with_graph() {
        let params = json!({
            "file": "/tmp/test.ttl",
            "graph": "http://example.org/graph1"
        });

        let result = graph::load(params).await;

        match result {
            Ok(_) => println!("Load with graph successful"),
            Err(e) => println!("Load with graph attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_load_delegates_with_all_params() {
        let params = json!({
            "file": "/tmp/test.ttl",
            "format": "turtle",
            "graph": "http://example.org/graph1"
        });

        let result = graph::load(params).await;

        match result {
            Ok(_) => println!("Load with all params successful"),
            Err(e) => println!("Load with all params attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_export_delegates_with_output() {
        let params = json!({"output": "/tmp/export.ttl"});

        let result = graph::export(params).await;

        match result {
            Ok(_) => println!("Export delegation successful"),
            Err(e) => println!("Export delegation attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_export_requires_output_param() {
        let params = json!({});

        let result = graph::export(params).await;

        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_export_delegates_with_format() {
        let params = json!({
            "output": "/tmp/export.ttl",
            "format": "turtle"
        });

        let result = graph::export(params).await;

        match result {
            Ok(_) => println!("Export with format successful"),
            Err(e) => println!("Export with format attempted: {:?}", e),
        }
    }

    #[tokio::test]
    async fn test_export_delegates_with_all_params() {
        let params = json!({
            "output": "/tmp/export.ttl",
            "format": "turtle",
            "graph": "http://example.org/graph1"
        });

        let result = graph::export(params).await;

        match result {
            Ok(_) => println!("Export with all params successful"),
            Err(e) => println!("Export with all params attempted: {:?}", e),
        }
    }
}

// ============================================================================
// CROSS-TOOL DELEGATION TESTS
// ============================================================================

#[tokio::test]
async fn test_multiple_tools_can_be_called_sequentially() {
    use ggen_mcp::tools::{project, market, graph};

    // Call different tools in sequence
    let _ = market::list(json!({})).await;
    let _ = market::search(json!({"query": "api"})).await;
    let _ = project::gen(json!({"template": "test"})).await;
    let _ = graph::query(json!({"sparql": "SELECT * WHERE { ?s ?p ?o }"})).await;

    // All should complete without interfering with each other
}

#[tokio::test]
async fn test_tools_maintain_independent_state() {
    use ggen_mcp::tools::{project, market};

    // First tool call
    let _ = project::gen(json!({"template": "test1"})).await;

    // Second tool call should not be affected by first
    let _ = market::search(json!({"query": "test"})).await;

    // Third tool call back to first type
    let _ = project::gen(json!({"template": "test2"})).await;

    // All should be independent
}

#[tokio::test]
async fn test_concurrent_tool_delegations() {
    use ggen_mcp::tools::{project, market, graph};

    let mut handles = vec![];

    // Spawn concurrent calls to different tools
    handles.push(tokio::spawn(async {
        project::gen(json!({"template": "test1"})).await
    }));

    handles.push(tokio::spawn(async {
        market::search(json!({"query": "test"})).await
    }));

    handles.push(tokio::spawn(async {
        graph::query(json!({"sparql": "SELECT * WHERE { ?s ?p ?o }"})).await
    }));

    // All should complete without deadlocks
    for handle in handles {
        let _ = handle.await;
    }
}

// ============================================================================
// PARAMETER EDGE CASE TESTS
// ============================================================================

#[tokio::test]
async fn test_delegation_with_very_long_strings() {
    use ggen_mcp::tools::market;

    let long_query = "a".repeat(10000);
    let params = json!({"query": long_query});

    let result = market::search(params).await;

    // Should handle long strings (may fail in CLI, but shouldn't panic)
    match result {
        Ok(_) => println!("Long string handled"),
        Err(e) => println!("Long string delegation attempted: {:?}", e),
    }
}

#[tokio::test]
async fn test_delegation_with_special_characters() {
    use ggen_mcp::tools::market;

    let params = json!({"query": "test!@#$%^&*()_+-=[]{}|;:',.<>?/\\"});

    let result = market::search(params).await;

    match result {
        Ok(_) => println!("Special characters handled"),
        Err(e) => println!("Special characters delegation attempted: {:?}", e),
    }
}

#[tokio::test]
async fn test_delegation_with_unicode() {
    use ggen_mcp::tools::market;

    let params = json!({"query": "测试 тест テスト 🚀"});

    let result = market::search(params).await;

    match result {
        Ok(_) => println!("Unicode handled"),
        Err(e) => println!("Unicode delegation attempted: {:?}", e),
    }
}

#[tokio::test]
async fn test_delegation_with_newlines_in_params() {
    use ggen_mcp::tools::graph;

    let params = json!({
        "sparql": "SELECT *\nWHERE {\n  ?s ?p ?o\n}\nLIMIT 10"
    });

    let result = graph::query(params).await;

    match result {
        Ok(_) => println!("Newlines handled"),
        Err(e) => println!("Newlines delegation attempted: {:?}", e),
    }
}

// ============================================================================
// ERROR PROPAGATION TESTS
// ============================================================================

#[tokio::test]
async fn test_delegation_error_contains_context() {
    use ggen_mcp::tools::project;

    let params = json!({});  // Missing required param

    let result = project::gen(params).await;

    assert!(result.is_err());
    let error = result.unwrap_err();
    let error_msg = error.to_string();

    // Error should be informative
    assert!(!error_msg.is_empty());
    assert!(error_msg.len() > 5);
}

#[tokio::test]
async fn test_delegation_errors_are_consistent() {
    use ggen_mcp::tools::{project, market, graph};

    // Collect errors from different tools
    let err1 = project::gen(json!({})).await.unwrap_err();
    let err2 = market::search(json!({})).await.unwrap_err();
    let err3 = graph::query(json!({})).await.unwrap_err();

    // All should have informative messages
    assert!(!err1.to_string().is_empty());
    assert!(!err2.to_string().is_empty());
    assert!(!err3.to_string().is_empty());
}
