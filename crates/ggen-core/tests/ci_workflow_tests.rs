//! Integration tests for CI workflow generation
//!
//! These tests verify the complete workflow generation pipeline from RDF
//! specifications to GitHub Actions YAML files using Chicago TDD patterns.

use ggen_core::ci::{WorkflowConfig, WorkflowGenerator};
use ggen_core::Graph;
use ggen_utils::error::Result;
use std::collections::HashMap;

/// Chicago TDD: Test workflow generation from RDF with real Graph
#[tokio::test]
async fn test_workflow_generation_from_rdf() -> Result<()> {
    // Arrange: Create RDF graph with test configuration
    let graph = Graph::new()?;
    graph.insert_turtle(
        r#"
        @prefix ggen: <http://ggen.example.org/> .
        @prefix test: <http://ggen.example.org/test/> .

        test:unit_tests a ggen:TestSuite ;
            ggen:testType "unit" ;
            ggen:timeout "10s" .

        test:integration_tests a ggen:TestSuite ;
            ggen:testType "integration" ;
            ggen:timeout "30s" ;
            ggen:requires "cache" .
    "#,
    )?;

    // Act: Extract configuration and generate workflow
    let config = WorkflowConfig::from_graph(&graph)?;
    let generator = WorkflowGenerator::new(config.clone());
    let yaml = generator.generate_workflow().await?;

    // Assert: Verify workflow structure
    assert_eq!(config.jobs.len(), 2, "Should have 2 jobs (unit + integration)");
    assert!(yaml.contains("name: Test with Containers"), "Should have workflow name");
    assert!(yaml.contains("unit_tests:"), "Should have unit test job");
    assert!(yaml.contains("integration_tests:"), "Should have integration test job");
    assert!(yaml.contains("cargo make test-unit"), "Should have unit test command");
    assert!(yaml.contains("cargo make test"), "Should have integration test command");
    assert!(yaml.contains("actions/cache@v4"), "Should have cache action for integration tests");

    // Verify YAML is valid
    WorkflowGenerator::validate_yaml(&yaml)?;

    Ok(())
}

/// Chicago TDD: Test job creation with timeout parsing
#[tokio::test]
async fn test_job_timeout_parsing() -> Result<()> {
    // Arrange: Create RDF with various timeout formats
    let graph = Graph::new()?;
    graph.insert_turtle(
        r#"
        @prefix ggen: <http://ggen.example.org/> .
        @prefix test: <http://ggen.example.org/test/> .

        test:quick_tests a ggen:TestSuite ;
            ggen:testType "unit" ;
            ggen:timeout "90s" .

        test:long_tests a ggen:TestSuite ;
            ggen:testType "integration" ;
            ggen:timeout "15m" .
    "#,
    )?;

    // Act: Extract configuration
    let config = WorkflowConfig::from_graph(&graph)?;

    // Assert: Verify timeout conversion
    assert_eq!(config.jobs.len(), 2);

    let quick_job = config.jobs.iter().find(|j| j.id == "unit_tests")
        .expect("Should have unit_tests job");
    assert_eq!(quick_job.timeout_minutes, 2, "90s should round up to 2 minutes");

    let long_job = config.jobs.iter().find(|j| j.id == "integration_tests")
        .expect("Should have integration_tests job");
    assert_eq!(long_job.timeout_minutes, 15, "15m should be 15 minutes");

    Ok(())
}

/// Chicago TDD: Test optimization settings extraction
#[tokio::test]
async fn test_optimization_extraction() -> Result<()> {
    // Arrange: Create RDF with optimization settings
    let graph = Graph::new()?;
    graph.insert_turtle(
        r#"
        @prefix ggen: <http://ggen.example.org/> .
        @prefix opt: <http://ggen.example.org/optimization/> .

        opt:config ggen:cacheDependencies "true"^^<http://www.w3.org/2001/XMLSchema#boolean> ;
            ggen:prepullImages "true"^^<http://www.w3.org/2001/XMLSchema#boolean> ;
            ggen:parallelJobs "true"^^<http://www.w3.org/2001/XMLSchema#boolean> ;
            ggen:maxParallel "4"^^<http://www.w3.org/2001/XMLSchema#integer> .
    "#,
    )?;

    // Act: Extract configuration
    let config = WorkflowConfig::from_graph(&graph)?;

    // Assert: Verify optimization settings
    assert!(config.optimization.cache_dependencies, "Should enable cache");
    assert!(config.optimization.prepull_images, "Should prepull images");
    assert!(config.optimization.parallel_jobs, "Should enable parallel jobs");
    assert_eq!(config.optimization.max_parallel, 4, "Should have max_parallel = 4");

    Ok(())
}

/// Chicago TDD: Test default optimization when not in RDF
#[tokio::test]
async fn test_default_optimization() -> Result<()> {
    // Arrange: Create RDF without optimization settings
    let graph = Graph::new()?;
    graph.insert_turtle(
        r#"
        @prefix ggen: <http://ggen.example.org/> .
        @prefix test: <http://ggen.example.org/test/> .

        test:basic_tests a ggen:TestSuite ;
            ggen:testType "unit" ;
            ggen:timeout "10s" .
    "#,
    )?;

    // Act: Extract configuration
    let config = WorkflowConfig::from_graph(&graph)?;

    // Assert: Verify default optimization settings
    assert!(config.optimization.cache_dependencies, "Should default to cache enabled");
    assert!(config.optimization.prepull_images, "Should default to prepull enabled");
    assert!(config.optimization.parallel_jobs, "Should default to parallel enabled");
    assert_eq!(config.optimization.max_parallel, 8, "Should default to max_parallel = 8");

    Ok(())
}

/// Chicago TDD: Test complete workflow generation with multiple jobs
#[tokio::test]
async fn test_multi_job_workflow() -> Result<()> {
    // Arrange: Create RDF with multiple test types
    let graph = Graph::new()?;
    graph.insert_turtle(
        r#"
        @prefix ggen: <http://ggen.example.org/> .
        @prefix test: <http://ggen.example.org/test/> .

        test:unit a ggen:TestSuite ;
            ggen:testType "unit" ;
            ggen:timeout "10s" .

        test:integration a ggen:TestSuite ;
            ggen:testType "integration" ;
            ggen:timeout "30s" .

        test:chaos a ggen:TestSuite ;
            ggen:testType "chaos" ;
            ggen:timeout "60s" .

        test:benchmark a ggen:TestSuite ;
            ggen:testType "benchmark" ;
            ggen:timeout "120s" .
    "#,
    )?;

    // Act: Generate workflow
    let config = WorkflowConfig::from_graph(&graph)?;
    let generator = WorkflowGenerator::new(config.clone());
    let yaml = generator.generate_workflow().await?;

    // Assert: Verify all jobs present
    assert_eq!(config.jobs.len(), 4, "Should have 4 jobs");
    assert!(yaml.contains("unit_tests:"), "Should have unit tests job");
    assert!(yaml.contains("integration_tests:"), "Should have integration tests job");
    assert!(yaml.contains("chaos_tests:"), "Should have chaos tests job");
    assert!(yaml.contains("benchmark_tests:"), "Should have benchmark tests job");

    // Verify correct commands
    assert!(yaml.contains("cargo make test-unit"), "Should have unit command");
    assert!(yaml.contains("cargo make test"), "Should have integration command");
    assert!(yaml.contains("cargo make test -- chaos"), "Should have chaos command");
    assert!(yaml.contains("cargo make bench"), "Should have benchmark command");

    WorkflowGenerator::validate_yaml(&yaml)?;

    Ok(())
}

/// Chicago TDD: Test workflow trigger generation
#[tokio::test]
async fn test_workflow_triggers() -> Result<()> {
    // Arrange: Create basic configuration
    let config = WorkflowConfig::new("Test Workflow".to_string());
    let generator = WorkflowGenerator::new(config);

    // Act: Generate workflow
    let yaml = generator.generate_workflow().await?;

    // Assert: Verify default triggers
    assert!(yaml.contains("on:"), "Should have on: section");
    assert!(yaml.contains("push:"), "Should have push trigger");
    assert!(yaml.contains("pull_request:"), "Should have PR trigger");
    assert!(yaml.contains("- main"), "Should trigger on main branch");
    assert!(yaml.contains("- develop"), "Should trigger on develop branch");

    WorkflowGenerator::validate_yaml(&yaml)?;

    Ok(())
}

/// Chicago TDD: Test badge URL generation
#[tokio::test]
async fn test_badge_generation() -> Result<()> {
    // Arrange: Create configuration
    let config = WorkflowConfig::new("CI Tests".to_string());
    let generator = WorkflowGenerator::new(config);

    // Act: Generate with badge
    let (yaml, badge) = generator.generate_with_badge("seanchatmangpt", "ggen").await?;

    // Assert: Verify badge URL format
    assert!(badge.starts_with("![CI Status]"), "Badge should start with markdown image syntax");
    assert!(badge.contains("github.com/seanchatmangpt/ggen"), "Badge should contain repo path");
    assert!(badge.contains("CI%20Tests"), "Badge should contain URL-encoded workflow name");
    assert!(badge.contains("badge.svg"), "Badge should end with badge.svg");

    // Verify YAML is valid
    WorkflowGenerator::validate_yaml(&yaml)?;

    Ok(())
}

/// Chicago TDD: Test YAML validation catches errors
#[test]
fn test_yaml_validation_error() {
    // Arrange: Create invalid YAML
    let invalid_yaml = r#"
name: Test
on: push
jobs:
  test:
    - this is invalid YAML syntax
    - because lists cannot be job values
"#;

    // Act: Attempt validation
    let result = WorkflowGenerator::validate_yaml(invalid_yaml);

    // Assert: Should detect invalid YAML
    assert!(result.is_err(), "Should reject invalid YAML");
    assert!(result.unwrap_err().to_string().contains("invalid"), "Error message should mention invalid YAML");
}

/// Chicago TDD: Test cache configuration for integration tests
#[tokio::test]
async fn test_cache_configuration() -> Result<()> {
    // Arrange: Create RDF with cache requirement
    let graph = Graph::new()?;
    graph.insert_turtle(
        r#"
        @prefix ggen: <http://ggen.example.org/> .
        @prefix test: <http://ggen.example.org/test/> .

        test:integration_tests a ggen:TestSuite ;
            ggen:testType "integration" ;
            ggen:timeout "30s" ;
            ggen:requires "cache" .
    "#,
    )?;

    // Act: Generate workflow
    let config = WorkflowConfig::from_graph(&graph)?;
    let generator = WorkflowGenerator::new(config);
    let yaml = generator.generate_workflow().await?;

    // Assert: Verify cache step present
    assert!(yaml.contains("Cache dependencies"), "Should have cache step");
    assert!(yaml.contains("actions/cache@v4"), "Should use cache action");
    assert!(yaml.contains("_build"), "Should cache _build directory");
    assert!(yaml.contains("deps"), "Should cache deps directory");

    WorkflowGenerator::validate_yaml(&yaml)?;

    Ok(())
}
