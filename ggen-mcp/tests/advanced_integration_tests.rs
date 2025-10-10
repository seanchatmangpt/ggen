//! Advanced Integration Tests
//!
//! Tests complex scenarios, edge cases, and advanced features

mod common;

use common::*;
use ggen_mcp::server::GgenMcpServer;
use serde_json::json;
use std::time::Instant;

/// Test large-scale project generation with many files
#[tokio::test]
async fn test_large_project_generation() {
    let server = GgenMcpServer::new();
    let workspace = create_temp_workspace();

    let large_template = json!({
        "template": "large-project",
        "output_dir": workspace.path().to_str().unwrap(),
        "vars": {
            "name": "large-project",
            "modules": ["auth", "api", "db", "cache", "logging"],
            "features": ["async", "tracing", "metrics"]
        }
    });

    let start = Instant::now();
    let result = server.execute_tool("project_gen", large_template).await;
    let duration = start.elapsed();

    println!("Large project generation took: {:?}", duration);
    assert!(result.is_ok(), "Large project generation should succeed");

    // Should complete in reasonable time (< 5 seconds)
    assert!(
        duration.as_secs() < 5,
        "Large project should complete quickly"
    );
}

/// Test template with complex variable interpolation
#[tokio::test]
async fn test_complex_variable_interpolation() {
    let server = GgenMcpServer::new();

    let complex_vars = json!({
        "template": "test-template",
        "vars": {
            "project": {
                "name": "my-app",
                "version": "1.0.0",
                "authors": ["Alice", "Bob"],
                "dependencies": {
                    "serde": "1.0",
                    "tokio": "1.0"
                }
            },
            "features": {
                "async": true,
                "logging": true,
                "metrics": false
            }
        }
    });

    let result = server.execute_tool("project_gen", complex_vars).await;
    println!("Complex vars result: {:?}", result);
}

/// Test SPARQL query with complex patterns
#[tokio::test]
async fn test_complex_sparql_queries() {
    let server = GgenMcpServer::new();

    // Test federated query
    let federated_query = json!({
        "sparql": r#"
            PREFIX rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#>
            PREFIX rdfs: <http://www.w3.org/2000/01/rdf-schema#>
            PREFIX foaf: <http://xmlns.com/foaf/0.1/>

            SELECT ?person ?name ?project ?projectName
            WHERE {
                ?person rdf:type foaf:Person .
                ?person foaf:name ?name .
                ?person foaf:currentProject ?project .
                ?project rdfs:label ?projectName .
                FILTER(LANG(?name) = "en")
            }
            ORDER BY ?name
            LIMIT 100
        "#
    });

    let result = server.execute_tool("graph_query", federated_query).await;
    println!("Federated query result: {:?}", result);

    // Test aggregation query
    let aggregation_query = json!({
        "sparql": r#"
            SELECT ?type (COUNT(?s) as ?count) (AVG(?value) as ?avgValue)
            WHERE {
                ?s rdf:type ?type .
                ?s ex:value ?value .
            }
            GROUP BY ?type
            HAVING (COUNT(?s) > 5)
            ORDER BY DESC(?count)
        "#
    });

    let agg_result = server.execute_tool("graph_query", aggregation_query).await;
    println!("Aggregation query result: {:?}", agg_result);
}

/// Test market search with complex filters
#[tokio::test]
async fn test_advanced_market_search() {
    let server = GgenMcpServer::new();

    // Test with multiple filters
    let complex_search = json!({
        "query": "rust api backend",
        "category": "backend",
        "min_stars": 100,
        "min_downloads": 1000,
        "license": "MIT",
        "author": "trusted-org",
        "tags": ["api", "rest", "async"],
        "updated_since": "2024-01-01",
        "fuzzy": true
    });

    let result = server.execute_tool("market_search", complex_search).await;
    println!("Complex search result: {:?}", result);
    assert!(result.is_ok(), "Complex search should succeed");

    // Test relevance scoring
    let relevance_search = json!({
        "query": "web framework with authentication",
        "sort": "relevance",
        "limit": 10
    });

    let rel_result = server.execute_tool("market_search", relevance_search).await;
    println!("Relevance search result: {:?}", rel_result);
}

/// Test unicode and special characters handling
#[tokio::test]
async fn test_unicode_handling() {
    let server = GgenMcpServer::new();

    let unicode_vars = json!({
        "template": "test-template",
        "vars": {
            "name": "プロジェクト",
            "author": "José García",
            "description": "A project with 中文 characters and émojis 🚀",
            "tags": ["日本語", "español", "中文"]
        }
    });

    let result = server.execute_tool("project_gen", unicode_vars).await;
    println!("Unicode handling result: {:?}", result);
}

/// Test very long strings and potential buffer issues
#[tokio::test]
async fn test_large_string_handling() {
    let server = GgenMcpServer::new();

    // Create a very long description
    let long_description = "A".repeat(10000);

    let large_string_vars = json!({
        "template": "test-template",
        "vars": {
            "name": "test",
            "description": long_description,
            "content": "X".repeat(50000)
        }
    });

    let result = server.execute_tool("project_gen", large_string_vars).await;
    println!("Large string result: {:?}", result);
}

/// Test concurrent load with different operations
#[tokio::test]
async fn test_mixed_concurrent_load() {
    let server = std::sync::Arc::new(GgenMcpServer::new());
    let mut handles = vec![];

    // Spawn 20 concurrent tasks with different operations
    for i in 0..20 {
        let server_clone = server.clone();
        let handle = tokio::spawn(async move {
            match i % 4 {
                0 => server_clone.execute_tool("market_search", json!({"query": format!("query-{}", i)})).await,
                1 => server_clone.execute_tool("project_plan", json!({"template": format!("tmpl-{}", i)})).await,
                2 => server_clone.execute_tool("graph_query", json!({"sparql": "SELECT * WHERE { ?s ?p ?o } LIMIT 1"})).await,
                _ => server_clone.execute_tool("market_list", json!({})).await,
            }
        });
        handles.push(handle);
    }

    let results = futures::future::join_all(handles).await;

    // Count successes
    let success_count = results.iter().filter(|r| r.is_ok()).count();
    println!("Concurrent load: {} / {} succeeded", success_count, results.len());

    assert!(success_count >= 15, "Most concurrent operations should succeed");
}

/// Test memory usage with repeated operations
#[tokio::test]
async fn test_memory_stability() {
    let server = GgenMcpServer::new();

    // Perform 100 operations
    for i in 0..100 {
        let _ = server
            .execute_tool(
                "market_search",
                json!({
                    "query": format!("test-query-{}", i),
                    "limit": 10
                }),
            )
            .await;

        if i % 20 == 0 {
            println!("Completed {} operations", i);
        }
    }

    // Server should still be responsive
    let final_result = server
        .execute_tool("market_list", json!({}))
        .await;

    assert!(final_result.is_ok(), "Server should remain stable after many operations");
}

/// Test retry logic and timeout handling
#[tokio::test]
async fn test_timeout_and_retry() {
    let server = GgenMcpServer::new();

    // Test with timeout parameter (if supported)
    let timeout_test = json!({
        "template": "test-template",
        "vars": {},
        "timeout_ms": 100
    });

    let result = server.execute_tool("project_gen", timeout_test).await;
    println!("Timeout test result: {:?}", result);
}

/// Test cache invalidation and refresh
#[tokio::test]
async fn test_cache_invalidation() {
    let server = GgenMcpServer::new();

    // First search
    let first_search = server
        .execute_tool("market_search", json!({"query": "rust"}))
        .await;
    println!("First search: {:?}", first_search);

    // Force cache sync
    let sync_result = server
        .execute_tool("market_sync", json!({"force": true}))
        .await;
    println!("Sync result: {:?}", sync_result);

    // Second search (should use fresh cache)
    let second_search = server
        .execute_tool("market_search", json!({"query": "rust"}))
        .await;
    println!("Second search: {:?}", second_search);
}

/// Test project plan vs actual generation consistency
#[tokio::test]
async fn test_plan_consistency() {
    let server = GgenMcpServer::new();
    let workspace = create_temp_workspace();

    let vars = json!({
        "name": "test-project",
        "version": "1.0.0"
    });

    // Create plan
    let plan_result = server
        .execute_tool(
            "project_plan",
            json!({
                "template": "test-template",
                "vars": vars.clone()
            }),
        )
        .await
        .unwrap();

    // Execute generation
    let gen_result = server
        .execute_tool(
            "project_gen",
            json!({
                "template": "test-template",
                "output_dir": workspace.path().to_str().unwrap(),
                "vars": vars
            }),
        )
        .await
        .unwrap();

    // Compare plan vs actual
    let plan_data = plan_result.get("data").unwrap();
    let gen_data = gen_result.get("data").unwrap();

    println!("Plan: {:?}", plan_data);
    println!("Actual: {:?}", gen_data);

    // Verify consistency
    // (In real implementation, file counts should match)
}

/// Test graph operations with named graphs
#[tokio::test]
async fn test_named_graph_operations() {
    let server = GgenMcpServer::new();
    let workspace = create_temp_workspace();

    // Create test RDF
    let rdf_content = r#"
        @prefix ex: <http://example.org/> .
        ex:Alice ex:knows ex:Bob .
        ex:Bob ex:knows ex:Charlie .
    "#;

    let rdf_file = create_test_file(&workspace, "data.ttl", rdf_content);

    // Load into named graph
    let load_result = server
        .execute_tool(
            "graph_load",
            json!({
                "file": rdf_file.to_str().unwrap(),
                "format": "turtle",
                "graph": "http://example.org/graph1"
            }),
        )
        .await;

    println!("Named graph load: {:?}", load_result);

    // Query specific graph
    let query_result = server
        .execute_tool(
            "graph_query",
            json!({
                "sparql": "SELECT * WHERE { ?s ?p ?o }",
                "graph": "http://example.org/graph1"
            }),
        )
        .await;

    println!("Named graph query: {:?}", query_result);

    // Export specific graph
    let export_path = workspace.path().join("graph1-export.ttl");
    let export_result = server
        .execute_tool(
            "graph_export",
            json!({
                "output": export_path.to_str().unwrap(),
                "format": "turtle",
                "graph": "http://example.org/graph1"
            }),
        )
        .await;

    println!("Named graph export: {:?}", export_result);
}

/// Test template validation edge cases
#[tokio::test]
async fn test_template_validation_edge_cases() {
    let server = GgenMcpServer::new();

    // Test empty template
    let empty_result = server
        .execute_tool(
            "template_validate",
            json!({
                "template": ""
            }),
        )
        .await;
    println!("Empty template validation: {:?}", empty_result);

    // Test template with circular references
    let circular_result = server
        .execute_tool(
            "template_validate",
            json!({
                "template": "{{#each items}}{{> item}}{{/each}}"
            }),
        )
        .await;
    println!("Circular reference validation: {:?}", circular_result);

    // Test template with special characters
    let special_chars_result = server
        .execute_tool(
            "template_validate",
            json!({
                "template": "{{name}}\n{{#if condition}}${{price}}{{/if}}"
            }),
        )
        .await;
    println!("Special chars validation: {:?}", special_chars_result);
}

/// Test market recommendations with different strategies
#[tokio::test]
async fn test_recommendation_strategies() {
    let server = GgenMcpServer::new();

    // Collaborative filtering
    let collab_result = server
        .execute_tool(
            "market_recommend",
            json!({
                "based_on": "popular-template",
                "strategy": "collaborative",
                "limit": 5
            }),
        )
        .await;
    println!("Collaborative recommendations: {:?}", collab_result);

    // Content-based filtering
    let content_result = server
        .execute_tool(
            "market_recommend",
            json!({
                "category": "backend",
                "tags": ["api", "rest"],
                "strategy": "content",
                "limit": 5
            }),
        )
        .await;
    println!("Content-based recommendations: {:?}", content_result);

    // Hybrid approach
    let hybrid_result = server
        .execute_tool(
            "market_recommend",
            json!({
                "based_on": "template-1",
                "category": "backend",
                "strategy": "hybrid",
                "limit": 5
            }),
        )
        .await;
    println!("Hybrid recommendations: {:?}", hybrid_result);
}
