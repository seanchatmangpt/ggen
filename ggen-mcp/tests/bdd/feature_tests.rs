// BDD-Style Feature Tests
// Cucumber-style scenarios for ggen-mcp tools

use ggen_mcp::tools::{project, market, template, graph};
use serde_json::json;

// Feature: Project Generation
// As a developer, I want to generate projects from templates
// So that I can quickly scaffold new applications

#[tokio::test]
async fn scenario_generate_rust_library_from_template() {
    // Given: I have a rust library template
    let template_name = "rust-lib";

    // When: I generate a project with custom variables
    let result = project::gen(json!({
        "template": template_name,
        "vars": {
            "name": "awesome-lib",
            "author": "Dev Team",
            "license": "MIT"
        },
        "output": "/tmp/awesome-lib"
    })).await;

    // Then: The project should be generated successfully
    assert!(result.is_ok(), "Project generation should succeed");

    let response = result.unwrap();
    let data = response.get("data").unwrap();

    // And: Files should be created
    assert!(data.get("files_generated").is_some());

    // And: Status should be completed
    assert_eq!(data.get("status").and_then(|v| v.as_str()), Some("completed"));
}

#[tokio::test]
async fn scenario_preview_changes_before_generation() {
    // Given: I want to see what will be generated
    let template_name = "rust-web-api";

    // When: I create an execution plan
    let plan_result = project::plan(json!({
        "template": template_name,
        "vars": {
            "name": "my-api",
            "port": 8080
        }
    })).await;

    // Then: I should see planned actions
    assert!(plan_result.is_ok(), "Plan creation should succeed");

    let data = plan_result.unwrap().get("data").unwrap().clone();

    // And: Actions should be listed
    let actions = data.get("actions").unwrap().as_array().unwrap();
    assert!(!actions.is_empty(), "Plan should contain actions");

    // And: I should see file count estimate
    assert!(data.get("estimated_files").is_some());
}

// Feature: Marketplace Search
// As a developer, I want to search for templates
// So that I can find suitable starting points for my project

#[tokio::test]
async fn scenario_search_marketplace_by_keyword() {
    // Given: The marketplace contains templates

    // When: I search for "authentication" templates
    let result = market::search(json!({
        "query": "authentication",
        "limit": 10
    })).await;

    // Then: I should see matching templates
    assert!(result.is_ok(), "Search should succeed");

    let data = result.unwrap().get("data").unwrap().clone();

    // And: Results should be returned
    assert!(data.get("results").is_some());

    // And: Total matches should be reported
    assert!(data.get("total_matches").is_some());
}

#[tokio::test]
async fn scenario_filter_search_by_category() {
    // Given: I'm looking for web development templates

    // When: I search with a category filter
    let result = market::search(json!({
        "query": "api",
        "category": "web",
        "limit": 5
    })).await;

    // Then: Results should be filtered by category
    assert!(result.is_ok());

    let data = result.unwrap().get("data").unwrap().clone();
    let filters = data.get("filters_applied").unwrap();

    // And: Category filter should be applied
    assert_eq!(
        filters.get("category").and_then(|v| v.as_str()),
        Some("web")
    );
}

#[tokio::test]
async fn scenario_get_recommendations_for_project() {
    // Given: I'm working on a web project

    // When: I request recommendations
    let result = market::recommend(json!({
        "category": "web",
        "limit": 5,
        "explain": "true"
    })).await;

    // Then: I should receive personalized recommendations
    assert!(result.is_ok());

    let data = result.unwrap().get("data").unwrap().clone();
    let recommendations = data.get("recommendations").unwrap().as_array().unwrap();

    // And: Recommendations should have confidence scores
    if !recommendations.is_empty() {
        assert!(recommendations[0].get("confidence").is_some());
    }

    // And: Explanation should be provided
    assert!(data.get("explanation").is_some());
}

// Feature: Template Authoring
// As a template author, I want to create and validate templates
// So that I can share reusable project structures

#[tokio::test]
async fn scenario_create_custom_template() {
    // Given: I have template content
    let template_content = "# {{name}}\n\n{{description}}";

    // When: I create a new template
    let result = template::create(json!({
        "name": "my-template",
        "content": template_content,
        "description": "My custom template",
        "tags": ["custom", "test"]
    })).await;

    // Then: Template should be created
    assert!(result.is_ok(), "Template creation should succeed");

    let data = result.unwrap().get("data").unwrap().clone();

    // And: Template path should be returned
    assert!(data.get("path").is_some());

    // And: Status should be created
    assert_eq!(data.get("status").and_then(|v| v.as_str()), Some("created"));
}

#[tokio::test]
async fn scenario_validate_template_syntax() {
    // Given: I have a template to validate

    // When: I validate the template
    let result = template::validate(json!({
        "template": "rust-lib"
    })).await;

    // Then: Validation should complete
    assert!(result.is_ok(), "Validation should succeed");

    let data = result.unwrap().get("data").unwrap().clone();

    // And: Valid status should be returned
    assert!(data.get("valid").is_some());

    // And: Detected variables should be listed
    assert!(data.get("variables_detected").is_some());
}

// Feature: Graph Operations
// As a data engineer, I want to work with RDF graphs
// So that I can manage semantic data

#[tokio::test]
async fn scenario_load_rdf_data_into_graph() {
    // Given: I have an RDF file
    let file_path = "/path/to/ontology.ttl";

    // When: I load the file into a graph
    let result = graph::load(json!({
        "file": file_path,
        "format": "turtle",
        "graph": "http://example.org/ontology"
    })).await;

    // Then: Data should be loaded
    assert!(result.is_ok(), "RDF load should succeed");

    let data = result.unwrap().get("data").unwrap().clone();

    // And: Triple count should be reported
    assert!(data.get("triples_loaded").is_some());

    // And: Status should be success
    assert_eq!(data.get("status").and_then(|v| v.as_str()), Some("success"));
}

#[tokio::test]
async fn scenario_query_graph_with_sparql() {
    // Given: A graph contains data

    // When: I execute a SPARQL query
    let result = graph::query(json!({
        "sparql": "SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 10",
        "graph": "http://example.org/data"
    })).await;

    // Then: Query results should be returned
    assert!(result.is_ok(), "SPARQL query should succeed");

    let data = result.unwrap().get("data").unwrap().clone();

    // And: Bindings should be present
    assert!(data.get("bindings").is_some());

    // And: Result count should be provided
    assert!(data.get("count").is_some());

    // And: Execution time should be tracked
    assert!(data.get("execution_time_ms").is_some());
}

#[tokio::test]
async fn scenario_export_graph_data() {
    // Given: A graph contains processed data

    // When: I export the graph
    let result = graph::export(json!({
        "output": "/tmp/export.ttl",
        "format": "turtle",
        "graph": "http://example.org/data"
    })).await;

    // Then: Data should be exported
    assert!(result.is_ok(), "Export should succeed");

    let data = result.unwrap().get("data").unwrap().clone();

    // And: Export statistics should be provided
    assert!(data.get("triples_exported").is_some());
    assert!(data.get("file_size_bytes").is_some());
}

// Feature: Offline Mode
// As a developer, I want to work offline
// So that I can continue development without internet

#[tokio::test]
async fn scenario_search_marketplace_offline() {
    // Given: I'm working without internet connection

    // When: I search using cached data
    let result = market::offline_search(json!({
        "query": "rust",
        "limit": 5
    })).await;

    // Then: Search should use local cache
    assert!(result.is_ok(), "Offline search should succeed");

    let data = result.unwrap().get("data").unwrap().clone();

    // And: Offline mode should be indicated
    assert_eq!(
        data.get("offline_mode").and_then(|v| v.as_bool()),
        Some(true)
    );

    // And: Cache timestamp should be provided
    assert!(data.get("cache_timestamp").is_some());
}

#[tokio::test]
async fn scenario_check_cache_status() {
    // Given: I want to know cache freshness

    // When: I check cache status
    let result = market::cache_status(json!({})).await;

    // Then: Cache statistics should be returned
    assert!(result.is_ok());

    let data = result.unwrap().get("data").unwrap().clone();

    // And: Package count should be reported
    assert!(data.get("package_count").is_some());

    // And: Staleness indicator should be present
    assert!(data.get("is_stale").is_some());
}
