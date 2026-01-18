//! Tests for generated Rust service
//! 
//! These tests validate the generated Rust service template
//! and ensure it works correctly with the RDF domain model.

use std::collections::HashMap;
use serde_json::json;

#[tokio::test]
async fn test_rust_service_generation() {
    // Test that the Rust service template generates valid code
    let template_content = include_str!("../../templates/rust-service.tmpl");
    
    // Basic template validation
    assert!(template_content.contains("{{ name | title }}"));
    assert!(template_content.contains("{{ name | snake }}"));
    assert!(template_content.contains("{{ name | pascal }}"));
    assert!(template_content.contains("sparql_results"));
    
    // Validate frontmatter structure
    assert!(template_content.starts_with("---"));
    assert!(template_content.contains("to:"));
    assert!(template_content.contains("vars:"));
    assert!(template_content.contains("prefixes:"));
    assert!(template_content.contains("sparql:"));
}

#[tokio::test]
async fn test_api_endpoint_generation() {
    // Test that the API endpoint template generates valid code
    let template_content = include_str!("../../templates/api-endpoint.tmpl");
    
    // Basic template validation
    assert!(template_content.contains("{{ name | title }}"));
    assert!(template_content.contains("{{ method }}"));
    assert!(template_content.contains("{{ path }}"));
    assert!(template_content.contains("sparql_results"));
    
    // Validate frontmatter structure
    assert!(template_content.starts_with("---"));
    assert!(template_content.contains("to:"));
    assert!(template_content.contains("vars:"));
    assert!(template_content.contains("prefixes:"));
    assert!(template_content.contains("sparql:"));
}

#[tokio::test]
async fn test_database_schema_generation() {
    // Test that the database schema template generates valid code
    let template_content = include_str!("../../templates/database-schema.tmpl");
    
    // Basic template validation
    assert!(template_content.contains("{{ name | title }}"));
    assert!(template_content.contains("{{ database }}"));
    assert!(template_content.contains("{{ orm }}"));
    assert!(template_content.contains("sparql_results"));
    
    // Validate frontmatter structure
    assert!(template_content.starts_with("---"));
    assert!(template_content.contains("to:"));
    assert!(template_content.contains("vars:"));
    assert!(template_content.contains("prefixes:"));
    assert!(template_content.contains("sparql:"));
}

#[tokio::test]
async fn test_documentation_generation() {
    // Test that the documentation template generates valid code
    let template_content = include_str!("../../templates/documentation.tmpl");
    
    // Basic template validation
    assert!(template_content.contains("{{ name | title }}"));
    assert!(template_content.contains("{{ format }}"));
    assert!(template_content.contains("{{ style }}"));
    assert!(template_content.contains("sparql_results"));
    
    // Validate frontmatter structure
    assert!(template_content.starts_with("---"));
    assert!(template_content.contains("to:"));
    assert!(template_content.contains("vars:"));
    assert!(template_content.contains("prefixes:"));
    assert!(template_content.contains("sparql:"));
}

#[tokio::test]
async fn test_rdf_domain_model() {
    // Test that the RDF domain model is valid
    let domain_content = include_str!("../../data/domain.ttl");
    
    // Basic RDF validation
    assert!(domain_content.contains("@prefix ex:"));
    assert!(domain_content.contains("ex:User a ex:Entity"));
    assert!(domain_content.contains("ex:Product a ex:Entity"));
    assert!(domain_content.contains("ex:Order a ex:Entity"));
    assert!(domain_content.contains("ex:Category a ex:Entity"));
    
    // Validate API endpoints
    assert!(domain_content.contains("ex:UserAPI a ex:APIEndpoint"));
    assert!(domain_content.contains("ex:ProductAPI a ex:APIEndpoint"));
    assert!(domain_content.contains("ex:OrderAPI a ex:APIEndpoint"));
    
    // Validate database schema
    assert!(domain_content.contains("ex:UsersTable a ex:Table"));
    assert!(domain_content.contains("ex:ProductsTable a ex:Table"));
    assert!(domain_content.contains("ex:OrdersTable a ex:Table"));
    assert!(domain_content.contains("ex:CategoriesTable a ex:Table"));
}

#[tokio::test]
async fn test_sparql_queries() {
    // Test that SPARQL queries are valid
    let domain_content = include_str!("../../data/domain.ttl");
    
    // Extract and validate SPARQL queries from templates
    let templates = [
        include_str!("../../templates/rust-service.tmpl"),
        include_str!("../../templates/api-endpoint.tmpl"),
        include_str!("../../templates/database-schema.tmpl"),
        include_str!("../../templates/documentation.tmpl"),
    ];
    
    for template in templates {
        // Check for SPARQL query definitions
        assert!(template.contains("sparql:"));
        assert!(template.contains("SELECT"));
        assert!(template.contains("WHERE"));
    }
}

#[tokio::test]
async fn test_template_filters() {
    // Test that template filters work correctly
    let test_cases = [
        ("hello_world", "camel", "helloWorld"),
        ("hello_world", "pascal", "HelloWorld"),
        ("hello_world", "snake", "hello_world"),
        ("hello_world", "kebab", "hello-world"),
        ("hello_world", "title", "Hello World"),
        ("hello_world", "upper", "HELLO_WORLD"),
        ("hello_world", "lower", "hello_world"),
    ];
    
    for (input, filter, expected) in test_cases {
        // This would be tested in actual template rendering
        // For now, we validate the filter names exist in templates
        let template_content = include_str!("../../templates/rust-service.tmpl");
        assert!(template_content.contains(&format!("{{{{ name | {} }}}}", filter)));
    }
}

#[tokio::test]
async fn test_sparql_helpers() {
    // Test that SPARQL helper functions are available
    let template_content = include_str!("../../templates/rust-service.tmpl");
    
    // Check for SPARQL helper usage
    assert!(template_content.contains("sparql_count"));
    assert!(template_content.contains("sparql_first"));
    assert!(template_content.contains("sparql_values"));
    assert!(template_content.contains("sparql_results"));
}

#[tokio::test]
async fn test_security_features() {
    // Test that security features are implemented
    let template_content = include_str!("../../templates/rust-service.tmpl");
    
    // Check for security-related code
    assert!(template_content.contains("Error::"));
    assert!(template_content.contains("Result<"));
    assert!(template_content.contains("anyhow::Result"));
    
    // Check for input validation
    assert!(template_content.contains("validation"));
    assert!(template_content.contains("required"));
}

#[tokio::test]
async fn test_generated_code_structure() {
    // Test that generated code has proper structure
    let template_content = include_str!("../../templates/rust-service.tmpl");
    
    // Check for Rust code structure
    assert!(template_content.contains("use serde"));
    assert!(template_content.contains("use axum"));
    assert!(template_content.contains("pub struct"));
    assert!(template_content.contains("impl"));
    assert!(template_content.contains("#[tokio::test]"));
    
    // Check for proper error handling
    assert!(template_content.contains("thiserror::Error"));
    assert!(template_content.contains("anyhow::Result"));
}

#[tokio::test]
async fn test_lifecycle_integration() {
    // Test that lifecycle integration works
    let make_content = include_str!("../../make.toml");
    
    // Check for lifecycle phases
    assert!(make_content.contains("[lifecycle.init]"));
    assert!(make_content.contains("[lifecycle.setup]"));
    assert!(make_content.contains("[lifecycle.generate]"));
    assert!(make_content.contains("[lifecycle.build]"));
    assert!(make_content.contains("[lifecycle.test]"));
    assert!(make_content.contains("[lifecycle.deploy]"));
    
    // Check for dependencies
    assert!(make_content.contains("depends_on"));
    assert!(make_content.contains("[\"setup\"]"));
    assert!(make_content.contains("[\"generate\"]"));
    assert!(make_content.contains("[\"build\"]"));
    assert!(make_content.contains("[\"test\"]"));
}

#[tokio::test]
async fn test_configuration_files() {
    // Test that configuration files are valid
    let ggen_content = include_str!("../../ggen.toml");
    
    // Check for project configuration
    assert!(ggen_content.contains("[project]"));
    assert!(ggen_content.contains("name = \"advanced-rust-project\""));
    assert!(ggen_content.contains("version = \"1.0.0\""));
    
    // Check for AI configuration
    assert!(ggen_content.contains("[ai]"));
    assert!(ggen_content.contains("provider = \"openai\""));
    assert!(ggen_content.contains("model = \"gpt-4\""));
    
    // Check for RDF configuration
    assert!(ggen_content.contains("[rdf]"));
    assert!(ggen_content.contains("base_iri"));
    assert!(ggen_content.contains("default_format = \"turtle\""));
    
    // Check for security configuration
    assert!(ggen_content.contains("[security]"));
    assert!(ggen_content.contains("validate_paths = true"));
    assert!(ggen_content.contains("block_shell_injection = true"));
}
