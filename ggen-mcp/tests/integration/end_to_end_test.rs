// Integration Tests - End-to-End Workflows
// London School: Test complete user journeys with mocked collaborators

use ggen_mcp::tools::{project, market, template, graph};
use serde_json::json;

#[tokio::test]
async fn should_complete_template_discovery_to_generation_workflow() {
    // Given: User wants to discover and use a template

    // Step 1: Search marketplace
    let search_result = market::search(json!({
        "query": "rust-web-api",
        "category": "web",
        "limit": 5
    })).await;

    assert!(search_result.is_ok(), "Search should succeed");

    // Step 2: Get package info
    let info_result = market::info(json!({
        "package_id": "io.ggen.rust.web-api",
        "examples": true
    })).await;

    assert!(info_result.is_ok(), "Package info should be retrieved");

    // Step 3: Create execution plan
    let plan_result = project::plan(json!({
        "template": "io.ggen.rust.web-api",
        "vars": {
            "name": "my-api",
            "port": 8080
        }
    })).await;

    assert!(plan_result.is_ok(), "Plan creation should succeed");

    // Step 4: Generate project
    let gen_result = project::gen(json!({
        "template": "io.ggen.rust.web-api",
        "vars": {
            "name": "my-api",
            "port": 8080
        },
        "output": "/tmp/my-api"
    })).await;

    assert!(gen_result.is_ok(), "Generation should succeed");
}

#[tokio::test]
async fn should_complete_template_authoring_workflow() {
    // Given: User wants to create and validate a template

    // Step 1: Create new template
    let create_result = template::create(json!({
        "name": "my-custom-template",
        "content": "# {{name}}\n\nA custom template",
        "description": "Custom template for testing",
        "tags": ["test", "custom"]
    })).await;

    assert!(create_result.is_ok(), "Template creation should succeed");

    // Step 2: Validate template syntax
    let validate_result = template::validate(json!({
        "template": "my-custom-template"
    })).await;

    assert!(validate_result.is_ok(), "Validation should succeed");

    let data = validate_result.unwrap().get("data").unwrap().clone();
    assert_eq!(data.get("valid").and_then(|v| v.as_bool()), Some(true));
}

#[tokio::test]
async fn should_complete_graph_data_processing_workflow() {
    // Given: User wants to work with RDF data

    // Step 1: Load RDF data
    let load_result = graph::load(json!({
        "file": "/path/to/ontology.ttl",
        "format": "turtle",
        "graph": "http://example.org/ontology"
    })).await;

    assert!(load_result.is_ok(), "RDF load should succeed");

    // Step 2: Query the data
    let query_result = graph::query(json!({
        "sparql": "SELECT ?class WHERE { ?class a owl:Class }",
        "graph": "http://example.org/ontology"
    })).await;

    assert!(query_result.is_ok(), "SPARQL query should succeed");

    // Step 3: Export results
    let export_result = graph::export(json!({
        "output": "/tmp/classes.ttl",
        "format": "turtle",
        "graph": "http://example.org/ontology"
    })).await;

    assert!(export_result.is_ok(), "Export should succeed");
}

#[tokio::test]
async fn should_handle_marketplace_offline_mode() {
    // Given: User in offline environment

    // Step 1: Check cache status
    let cache_status = market::cache_status(json!({})).await;
    assert!(cache_status.is_ok(), "Cache status should be available");

    // Step 2: Perform offline search
    let offline_search = market::offline_search(json!({
        "query": "rust",
        "limit": 5
    })).await;

    assert!(offline_search.is_ok(), "Offline search should work");

    let data = offline_search.unwrap().get("data").unwrap().clone();
    assert_eq!(
        data.get("offline_mode").and_then(|v| v.as_bool()),
        Some(true)
    );
}

#[tokio::test]
async fn should_support_dry_run_workflow() {
    // Given: User wants to preview changes

    // Step 1: Create plan (dry-run)
    let plan_result = project::plan(json!({
        "template": "rust-lib",
        "vars": { "name": "test-lib" }
    })).await;

    assert!(plan_result.is_ok());
    let plan_data = plan_result.unwrap().get("data").unwrap().clone();
    let plan_id = plan_data.get("plan_id").unwrap().as_str().unwrap();

    // Step 2: Review diff
    let diff_result = project::diff(json!({
        "template": "rust-lib",
        "target": ".",
        "vars": { "name": "test-lib" }
    })).await;

    assert!(diff_result.is_ok());

    // Step 3: Apply plan only if satisfied
    let apply_result = project::apply(json!({
        "plan": plan_id
    })).await;

    assert!(apply_result.is_ok());
}

#[tokio::test]
async fn should_complete_recommendation_to_install_workflow() {
    // Given: User wants recommendations

    // Step 1: Get recommendations
    let recommend_result = market::recommend(json!({
        "category": "web",
        "limit": 3,
        "explain": "true"
    })).await;

    assert!(recommend_result.is_ok());
    let data = recommend_result.unwrap().get("data").unwrap().clone();

    let recommendations = data.get("recommendations").unwrap().as_array().unwrap();
    assert!(!recommendations.is_empty());

    // Step 2: Install recommended package
    if !recommendations.is_empty() {
        let first_rec = &recommendations[0];
        let package_id = first_rec.get("package_id").unwrap().as_str().unwrap();

        let install_result = market::install(json!({
            "package": package_id,
            "version": "latest"
        })).await;

        assert!(install_result.is_ok());
    }
}

#[tokio::test]
async fn should_handle_multi_step_project_generation() {
    // Given: Complex project with multiple phases

    // Phase 1: Plan backend
    let backend_plan = project::plan(json!({
        "template": "rust-backend",
        "vars": { "name": "backend" }
    })).await;
    assert!(backend_plan.is_ok());

    // Phase 2: Plan frontend
    let frontend_plan = project::plan(json!({
        "template": "react-frontend",
        "vars": { "name": "frontend" }
    })).await;
    assert!(frontend_plan.is_ok());

    // Phase 3: Generate both
    let backend_gen = project::gen(json!({
        "template": "rust-backend",
        "vars": { "name": "backend" },
        "output": "/tmp/project/backend"
    })).await;
    assert!(backend_gen.is_ok());

    let frontend_gen = project::gen(json!({
        "template": "react-frontend",
        "vars": { "name": "frontend" },
        "output": "/tmp/project/frontend"
    })).await;
    assert!(frontend_gen.is_ok());
}

#[tokio::test]
async fn should_validate_before_generation() {
    // Given: Validation-first workflow

    // Step 1: Validate template
    let validate_result = template::validate(json!({
        "template": "rust-lib"
    })).await;

    assert!(validate_result.is_ok());
    let data = validate_result.unwrap().get("data").unwrap().clone();

    // Step 2: Only generate if valid
    if data.get("valid").and_then(|v| v.as_bool()) == Some(true) {
        let gen_result = project::gen(json!({
            "template": "rust-lib",
            "vars": { "name": "validated-lib" }
        })).await;

        assert!(gen_result.is_ok());
    }
}
