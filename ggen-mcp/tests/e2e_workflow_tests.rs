//! End-to-End Workflow Integration Tests
//!
//! This test suite validates complete workflows that users would perform,
//! testing multiple tools in sequence to ensure proper integration.

mod common;

use common::*;
use ggen_mcp::server::GgenMcpServer;
use serde_json::json;

/// Test complete project generation workflow:
/// 1. Search for template
/// 2. Get template info
/// 3. Generate project plan
/// 4. Execute generation
#[tokio::test]
async fn test_complete_project_generation_workflow() {
    setup_test_logging();
    let server = GgenMcpServer::new();
    let workspace = create_temp_workspace();

    // Step 1: Search for a template
    let search_result = server
        .execute_tool(
            "market_search",
            json!({
                "query": "rust api",
                "limit": 5
            }),
        )
        .await;

    assert!(search_result.is_ok(), "Market search should succeed");
    let search_data = search_result.unwrap();
    println!("Search result: {:?}", search_data);

    // Step 2: Get detailed info about first template
    let info_result = server
        .execute_tool(
            "market_info",
            json!({
                "package_id": "rust-api-template",
                "examples": true,
                "dependencies": true
            }),
        )
        .await;

    println!("Template info: {:?}", info_result);

    // Step 3: Create execution plan
    let plan_result = server
        .execute_tool(
            "project_plan",
            json!({
                "template": "rust-api-template",
                "vars": {
                    "name": "test-api",
                    "author": "Test User"
                }
            }),
        )
        .await;

    assert!(plan_result.is_ok(), "Project plan should succeed");
    let plan_data = plan_result.unwrap();
    println!("Project plan: {:?}", plan_data);

    // Step 4: Execute project generation
    let gen_result = server
        .execute_tool(
            "project_gen",
            json!({
                "template": "rust-api-template",
                "output_dir": workspace.path().to_str().unwrap(),
                "vars": {
                    "name": "test-api",
                    "author": "Test User"
                }
            }),
        )
        .await;

    assert!(gen_result.is_ok(), "Project generation should succeed");
    let gen_data = gen_result.unwrap();

    // Verify response structure
    assert!(gen_data.get("data").is_some());
    let data = gen_data.get("data").unwrap();
    assert!(data.get("files_created").is_some());
    assert!(data.get("execution_time_ms").is_some());
}

/// Test template validation and creation workflow
#[tokio::test]
async fn test_template_validation_workflow() {
    let server = GgenMcpServer::new();

    // Step 1: Create a new template
    let create_result = server
        .execute_tool(
            "template_create",
            json!({
                "name": "my-custom-template",
                "description": "A custom test template",
                "content": "{{project_name}}\n{{author}}",
                "tags": ["test", "custom"]
            }),
        )
        .await;

    println!("Template creation: {:?}", create_result);

    // Step 2: Validate the template
    let validate_result = server
        .execute_tool(
            "template_validate",
            json!({
                "template": "my-custom-template"
            }),
        )
        .await;

    assert!(
        validate_result.is_ok(),
        "Template validation should succeed"
    );
    let validate_data = validate_result.unwrap();

    // Check validation results
    assert!(validate_data.get("data").is_some());
    let data = validate_data.get("data").unwrap();
    assert!(data.get("valid").is_some());
    assert!(data.get("variables").is_some());
}

/// Test graph data workflow: load, query, export
#[tokio::test]
async fn test_graph_workflow() {
    let server = GgenMcpServer::new();
    let workspace = create_temp_workspace();

    // Create a test RDF file
    let rdf_content = r#"
        @prefix ex: <http://example.org/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        ex:Project1 rdf:type ex:SoftwareProject .
        ex:Project1 ex:name "Test Project" .
        ex:Project1 ex:language "Rust" .
    "#;

    let rdf_file = create_test_file(&workspace, "test-data.ttl", rdf_content);

    // Step 1: Load RDF data
    let load_result = server
        .execute_tool(
            "graph_load",
            json!({
                "file": rdf_file.to_str().unwrap(),
                "format": "turtle"
            }),
        )
        .await;

    println!("Graph load result: {:?}", load_result);

    // Step 2: Query the loaded data
    let query_result = server
        .execute_tool(
            "graph_query",
            json!({
                "sparql": "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10"
            }),
        )
        .await;

    assert!(query_result.is_ok(), "Graph query should succeed");
    let query_data = query_result.unwrap();
    assert!(query_data.get("data").is_some());

    // Step 3: Export to different format
    let export_path = workspace.path().join("export.ntriples");
    let export_result = server
        .execute_tool(
            "graph_export",
            json!({
                "output": export_path.to_str().unwrap(),
                "format": "ntriples"
            }),
        )
        .await;

    println!("Graph export result: {:?}", export_result);
}

/// Test market cache synchronization workflow
#[tokio::test]
async fn test_market_sync_workflow() {
    let server = GgenMcpServer::new();

    // Step 1: Check cache status
    let status_result = server.execute_tool("market_cache_status", json!({})).await;

    println!("Cache status: {:?}", status_result);

    // Step 2: Perform sync
    let sync_result = server
        .execute_tool(
            "market_sync",
            json!({
                "force": false,
                "dry_run": true
            }),
        )
        .await;

    println!("Sync result: {:?}", sync_result);

    // Step 3: Search using offline cache
    let offline_search = server
        .execute_tool(
            "market_offline_search",
            json!({
                "query": "template",
                "limit": 5
            }),
        )
        .await;

    println!("Offline search: {:?}", offline_search);
}

/// Test project diff workflow
#[tokio::test]
async fn test_project_diff_workflow() {
    let server = GgenMcpServer::new();
    let workspace = create_temp_workspace();

    // Step 1: Generate initial project
    let gen_result = server
        .execute_tool(
            "project_gen",
            json!({
                "template": "test-template",
                "output_dir": workspace.path().to_str().unwrap(),
                "vars": {
                    "name": "test-project",
                    "version": "1.0.0"
                }
            }),
        )
        .await;

    println!("Initial generation: {:?}", gen_result);

    // Step 2: Check diff with modified variables
    let diff_result = server
        .execute_tool(
            "project_diff",
            json!({
                "template": "test-template",
                "target": workspace.path().to_str().unwrap(),
                "vars": {
                    "name": "test-project-updated",
                    "version": "2.0.0"
                }
            }),
        )
        .await;

    assert!(diff_result.is_ok(), "Project diff should succeed");
    println!("Diff result: {:?}", diff_result);
}

/// Test hook registration workflow
#[tokio::test]
async fn test_hook_workflow() {
    let server = GgenMcpServer::new();

    // Register pre-generation hook
    let pre_hook = server
        .execute_tool(
            "hook_register",
            json!({
                "event": "pre_gen",
                "command": "echo 'Starting generation'",
                "name": "pre-gen-logger"
            }),
        )
        .await;

    println!("Pre-hook registration: {:?}", pre_hook);

    // Register post-generation hook
    let post_hook = server
        .execute_tool(
            "hook_register",
            json!({
                "event": "post_gen",
                "command": "echo 'Generation complete'",
                "name": "post-gen-logger"
            }),
        )
        .await;

    println!("Post-hook registration: {:?}", post_hook);

    // Now generate project with hooks
    let gen_result = server
        .execute_tool(
            "project_gen",
            json!({
                "template": "test-template",
                "vars": {}
            }),
        )
        .await;

    println!("Generation with hooks: {:?}", gen_result);
}

/// Test recommendation workflow
#[tokio::test]
async fn test_recommendation_workflow() {
    let server = GgenMcpServer::new();

    // Step 1: Install a package (simulated)
    let install_result = server
        .execute_tool(
            "market_install",
            json!({
                "package": "rust-api-template"
            }),
        )
        .await;

    println!("Install result: {:?}", install_result);

    // Step 2: Get recommendations based on installed packages
    let recommend_result = server
        .execute_tool(
            "market_recommend",
            json!({
                "based_on": "rust-api-template",
                "limit": 5
            }),
        )
        .await;

    assert!(recommend_result.is_ok(), "Recommendations should succeed");
    println!("Recommendations: {:?}", recommend_result);

    // Step 3: Get recommendations by category
    let category_recommend = server
        .execute_tool(
            "market_recommend",
            json!({
                "category": "backend",
                "limit": 3
            }),
        )
        .await;

    println!("Category recommendations: {:?}", category_recommend);
}

/// Test error recovery workflow
#[tokio::test]
async fn test_error_recovery_workflow() {
    let server = GgenMcpServer::new();

    // Step 1: Try to generate with missing template
    let fail_result = server
        .execute_tool(
            "project_gen",
            json!({
                "template": "nonexistent-template",
                "vars": {}
            }),
        )
        .await;

    println!("Expected failure: {:?}", fail_result);

    // Step 2: Search for available templates
    let search_result = server
        .execute_tool(
            "market_search",
            json!({
                "query": "api"
            }),
        )
        .await;

    assert!(search_result.is_ok(), "Search should work after failure");

    // Step 3: Retry with valid template
    let retry_result = server
        .execute_tool(
            "project_gen",
            json!({
                "template": "test-template",
                "vars": {}
            }),
        )
        .await;

    println!("Retry result: {:?}", retry_result);
}

/// Test concurrent workflow execution
#[tokio::test]
async fn test_concurrent_workflows() {
    let server = std::sync::Arc::new(GgenMcpServer::new());
    let mut handles = vec![];

    // Spawn 5 concurrent workflows
    for i in 0..5 {
        let server_clone = server.clone();
        let handle = tokio::spawn(async move {
            // Each workflow: search -> info -> plan
            let search = server_clone
                .execute_tool("market_search", json!({"query": format!("template-{}", i)}))
                .await;

            let info = server_clone
                .execute_tool("market_info", json!({"package_id": format!("pkg-{}", i)}))
                .await;

            let plan = server_clone
                .execute_tool("project_plan", json!({"template": format!("tmpl-{}", i)}))
                .await;

            (search, info, plan)
        });

        handles.push(handle);
    }

    // Wait for all workflows
    let results = futures::future::join_all(handles).await;

    // Verify all completed without panics
    for result in results {
        assert!(result.is_ok(), "Concurrent workflow should complete");
    }
}

/// Test resource cleanup workflow
#[tokio::test]
async fn test_resource_cleanup_workflow() {
    let server = GgenMcpServer::new();
    let workspace = create_temp_workspace();

    // Generate project
    let gen_result = server
        .execute_tool(
            "project_gen",
            json!({
                "template": "test-template",
                "output_dir": workspace.path().to_str().unwrap(),
                "vars": {}
            }),
        )
        .await;

    println!("Generation: {:?}", gen_result);

    // Workspace should exist
    assert!(workspace.path().exists());

    // Drop workspace to trigger cleanup
    drop(workspace);

    // Verify server still works after cleanup
    let search_result = server
        .execute_tool("market_search", json!({"query": "test"}))
        .await;

    assert!(search_result.is_ok(), "Server should work after cleanup");
}
