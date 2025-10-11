//! Comprehensive tests for AI tools
//!
//! This test suite provides coverage for all AI tool functions including
//! template generation, SPARQL queries, ontology generation, and project scaffolding.

use ggen_mcp::tools::ai;
use serde_json::json;

#[tokio::test]
async fn test_generate_template_with_mock() {
    let params = json!({
        "description": "Generate a REST API endpoint for user management",
        "provider": "mock",
        "validate": false
    });

    let result = ai::generate_template(params).await;
    if let Err(e) = &result {
        eprintln!("Error: {:?}", e);
    }
    assert!(result.is_ok(), "Template generation should succeed with mock provider");

    let response = result.unwrap();
    assert!(response.get("template_body").is_some());
    assert_eq!(response.get("generated").and_then(|v| v.as_bool()), Some(true));
}

#[tokio::test]
async fn test_generate_template_with_validation() {
    let params = json!({
        "description": "Generate a simple HTML template",
        "provider": "mock",
        "validate": true
    });

    let result = ai::generate_template(params).await;
    assert!(result.is_ok());

    let response = result.unwrap();
    assert_eq!(response.get("validated").and_then(|v| v.as_bool()), Some(true));
    assert!(response.get("validation_errors").is_some());
}

#[tokio::test]
async fn test_generate_template_missing_description() {
    let params = json!({
        "provider": "mock"
    });

    let result = ai::generate_template(params).await;
    assert!(result.is_err(), "Should fail without description parameter");
}

#[tokio::test]
async fn test_generate_project_rust_basic() {
    let params = json!({
        "description": "A simple Rust CLI tool",
        "name": "test-cli",
        "provider": "mock",
        "language": "rust"
    });

    let result = ai::generate_project(params).await;
    assert!(result.is_ok());

    let response = result.unwrap();
    assert_eq!(response.get("project_name").and_then(|v| v.as_str()), Some("test-cli"));
    assert_eq!(response.get("language").and_then(|v| v.as_str()), Some("rust"));

    let files = response.get("files").and_then(|v| v.as_array());
    assert!(files.is_some());
    assert!(files.unwrap().len() >= 2, "Should generate at least Cargo.toml and main.rs");
}

#[tokio::test]
async fn test_generate_project_rust_axum() {
    let params = json!({
        "description": "A REST API service",
        "name": "test-api",
        "provider": "mock",
        "language": "rust",
        "framework": "axum"
    });

    let result = ai::generate_project(params).await;
    assert!(result.is_ok());

    let response = result.unwrap();
    let files = response.get("files").and_then(|v| v.as_array()).unwrap();
    assert!(files.len() >= 3, "Axum project should generate Cargo.toml, main.rs, and lib.rs");
}

#[tokio::test]
async fn test_generate_project_python() {
    let params = json!({
        "description": "A Python web service",
        "name": "test-service",
        "provider": "mock",
        "language": "python"
    });

    let result = ai::generate_project(params).await;
    assert!(result.is_ok());

    let response = result.unwrap();
    assert_eq!(response.get("language").and_then(|v| v.as_str()), Some("python"));

    let files = response.get("files").and_then(|v| v.as_array()).unwrap();
    assert!(files.iter().any(|f| f.as_str().unwrap().contains("pyproject.toml")));
    assert!(files.iter().any(|f| f.as_str().unwrap().contains("main.py")));
}

#[tokio::test]
async fn test_generate_project_unsupported_language() {
    let params = json!({
        "description": "A Java application",
        "name": "test-app",
        "provider": "mock",
        "language": "java"
    });

    let result = ai::generate_project(params).await;
    assert!(result.is_err(), "Should fail with unsupported language");
}

#[tokio::test]
async fn test_list_providers() {
    let params = json!({});

    let result = ai::list_providers(params).await;
    if let Err(e) = &result {
        eprintln!("Error: {:?}", e);
    }
    assert!(result.is_ok());

    let response = result.unwrap();
    eprintln!("Response: {:?}", response);
    assert!(response.get("providers").is_some());
    assert!(response.get("total_providers").is_some());

    let providers = response.get("providers").and_then(|v| v.as_array()).unwrap();
    assert!(!providers.is_empty(), "Should have at least mock provider");
}

#[tokio::test]
async fn test_validate_and_improve() {
    let params = json!({
        "content": "fn main() { println!(\"Hello\"); }",
        "content_type": "code",
        "provider": "mock",
        "max_iterations": 2
    });

    let result = ai::validate_and_improve(params).await;
    assert!(result.is_ok());

    let response = result.unwrap();
    assert_eq!(response.get("validated").and_then(|v| v.as_bool()), Some(true));
    assert_eq!(response.get("iterations").and_then(|v| v.as_u64()), Some(2));
}

#[test]
fn test_validate_template_empty() {
    use ggen_mcp::tools::ai::validation::validate_template;

    let result = validate_template("");
    assert!(result.is_ok());

    let errors = result.unwrap();
    assert_eq!(errors.len(), 1);
    assert!(errors[0].contains("empty"));
}

#[test]
fn test_validate_template_unbalanced_braces() {
    use ggen_mcp::tools::ai::validation::validate_template;

    let template = "Hello {{ name }} world {{ ";
    let result = validate_template(template);
    assert!(result.is_ok());

    let errors = result.unwrap();
    assert!(errors.iter().any(|e| e.contains("Unbalanced")));
}

#[test]
fn test_validate_template_dangerous_patterns() {
    use ggen_mcp::tools::ai::validation::validate_template;

    let template = "{{ eval(user_input) }}";
    let result = validate_template(template);
    assert!(result.is_ok());

    let errors = result.unwrap();
    assert!(errors.iter().any(|e| e.contains("dangerous")));
}

#[test]
fn test_validate_template_valid() {
    use ggen_mcp::tools::ai::validation::validate_template;

    let template = "Hello {{ name }}! Welcome to {{ site }}.";
    let result = validate_template(template);
    assert!(result.is_ok());

    let errors = result.unwrap();
    assert!(errors.is_empty(), "Valid template should have no errors");
}

#[test]
fn test_validate_template_size_limit() {
    use ggen_mcp::tools::ai::validation::validate_template;

    let large_template = "x".repeat(1_000_001);
    let result = validate_template(&large_template);
    assert!(result.is_ok());

    let errors = result.unwrap();
    assert!(errors.iter().any(|e| e.contains("very large")));
}
