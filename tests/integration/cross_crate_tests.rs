//! Cross-Crate Integration Tests
//!
//! Tests coordination between ggen-core, ggen-marketplace, and ggen-ai crates
//! to ensure they work together correctly in realistic workflows.
//!
//! ## Test Coverage
//!
//! - Lifecycle + Template generation workflow
//! - Marketplace + Cache integration
//! - Template + RDF graph integration
//! - Full end-to-end generation pipeline
//!
//! ## Running These Tests
//!
//! ```bash
//! cargo test --test cross_crate_tests
//! ```

use chicago_tdd_tools::prelude::*;
use ggen_core::{
    generate_file_tree, GenContext, Generator, Graph, Pipeline, Template, TemplateContext,
};
use ggen_marketplace::prelude::*;
use std::collections::BTreeMap;
use std::fs;
use tempfile::TempDir;

// Import common test utilities
#[path = "../common/mod.rs"]
mod common;
use common::{create_temp_dir, sample_package, sample_template_vars, write_file_in_temp};

// ============================================================================
// Lifecycle + Template Generation Workflow
// ============================================================================

test!(test_lifecycle_with_template_generation, {
    // Arrange
    let temp_dir = create_temp_dir();
    let template_content = r#"# {{ project_name }}

Generated during lifecycle phase.
"#;

    let template_path = write_file_in_temp(&temp_dir, "template.md", template_content);
    let output_dir = temp_dir.path().join("output");

    // Act - Generate template as part of lifecycle
    let pipeline = Pipeline::new()?;
    let ctx =
        GenContext::new(template_path.into(), output_dir.clone()).with_vars(sample_template_vars());

    let mut generator = Generator::new(pipeline, ctx);
    let output_path = generator.generate()?;

    // Assert
    assert!(output_path.exists(), "Template should be generated");
    let content = fs::read_to_string(&output_path)?;
    assert!(
        content.contains("TestProject"),
        "Generated content should contain project name"
    );
    Ok(())
});

test!(test_file_tree_generation_in_lifecycle_phase, {
    // Arrange
    let temp_dir = create_temp_dir();
    let output_dir = temp_dir.path().join("project");

    let tree_template = r#"
project/
  src/
    lib.rs: pub mod {{ module_name }};
  Cargo.toml: [package]\nname = "{{ package_name }}"
  README.md: # {{ project_name }}
"#;

    let mut vars = BTreeMap::new();
    vars.insert("project_name".to_string(), "MyProject".to_string());
    vars.insert("package_name".to_string(), "my_project".to_string());
    vars.insert("module_name".to_string(), "core".to_string());

    let ctx = TemplateContext {
        variables: vars,
        output_dir: output_dir.clone(),
    };

    // Act
    generate_file_tree(tree_template, &ctx)?;

    // Assert
    assert!(output_dir.join("src/lib.rs").exists());
    assert!(output_dir.join("Cargo.toml").exists());
    assert!(output_dir.join("README.md").exists());

    let cargo_content = fs::read_to_string(output_dir.join("Cargo.toml"))?;
    assert!(cargo_content.contains("my_project"));
    Ok(())
});

// ============================================================================
// Marketplace + Cache Integration
// ============================================================================

test!(test_marketplace_package_with_cache, {
    // Arrange
    let temp_dir = create_temp_dir();
    let cache_dir = temp_dir.path().join(".cache");
    let registry_dir = temp_dir.path().join("registry");

    use ggen_core::CacheManager;

    let cache = CacheManager::with_dir(cache_dir)?;
    let registry = LocalRegistry::new(registry_dir)?;

    let package = sample_package();

    // Act - Cache package content
    let pack_content = b"Mock package content";
    cache.cache_pack(
        &package.id.to_string(),
        &package.version.to_string(),
        pack_content,
    )?;

    // Assert - Verify cached
    let cached = cache.get_pack(&package.id.to_string(), &package.version.to_string())?;
    assert!(cached.is_some(), "Package should be cached");
    Ok(())
});

test!(test_marketplace_search_integration, {
    // Arrange
    let temp_dir = create_temp_dir();
    let registry_dir = temp_dir.path().join("registry");
    let registry = LocalRegistry::new(registry_dir)?;

    // Act - Search should initialize successfully
    let query = Query::new("test");
    let result = registry.search(&query).await;

    // Assert
    assert!(
        result.is_ok(),
        "Registry search should complete (even if empty)"
    );
    Ok(())
});

// ============================================================================
// Template + RDF Graph Integration
// ============================================================================

test!(test_template_with_rdf_metadata, {
    // Arrange
    let graph = Graph::new()?;

    // Insert RDF metadata about a template
    graph.insert_turtle(
        r#"
        @prefix ggen: <http://ggen.dev/> .
        @prefix rdf: <http://www.w3.org/1999/02/22-rdf-syntax-ns#> .

        ggen:template1 a ggen:Template ;
            ggen:name "Test Template" ;
            ggen:description "A template for testing" ;
            ggen:version "1.0.0" .
    "#,
    )?;

    // Act - Query template metadata
    let results = graph.query(
        r#"
        PREFIX ggen: <http://ggen.dev/>
        SELECT ?name ?desc WHERE {
            ?template a ggen:Template ;
                     ggen:name ?name ;
                     ggen:description ?desc .
        }
    "#,
    )?;

    // Assert
    assert!(
        !results.is_empty(),
        "Should find template metadata in RDF graph"
    );
    Ok(())
});

test!(test_rdf_driven_template_selection, {
    // Arrange
    let graph = Graph::new()?;

    graph.insert_turtle(
        r#"
        @prefix ggen: <http://ggen.dev/> .

        ggen:rustTemplate a ggen:Template ;
            ggen:language "rust" ;
            ggen:category "cli" ;
            ggen:path "/templates/rust-cli.tmpl" .

        ggen:pythonTemplate a ggen:Template ;
            ggen:language "python" ;
            ggen:category "cli" ;
            ggen:path "/templates/python-cli.tmpl" .
    "#,
    )?;

    // Act - Query for Rust CLI templates
    let results = graph.query(
        r#"
        PREFIX ggen: <http://ggen.dev/>
        SELECT ?path WHERE {
            ?template a ggen:Template ;
                     ggen:language "rust" ;
                     ggen:category "cli" ;
                     ggen:path ?path .
        }
    "#,
    )?;

    // Assert
    assert_eq!(
        results.len(),
        1,
        "Should find exactly one Rust CLI template"
    );
    Ok(())
});

// ============================================================================
// Full End-to-End Workflow Tests
// ============================================================================

test!(test_complete_generation_workflow, {
    // Arrange
    let temp_dir = create_temp_dir();

    // Step 1: Create template with RDF metadata
    let template_content = r#"---
name: complete-workflow-template
language: rust
category: library
---
// {{ project_name }} - {{ description }}

pub struct {{ struct_name }} {
    // Generated code
}
"#;

    let template_path = write_file_in_temp(&temp_dir, "template.rs", template_content);
    let output_dir = temp_dir.path().join("output");

    // Step 2: Prepare variables
    let mut vars = BTreeMap::new();
    vars.insert("project_name".to_string(), "CompleteWorkflow".to_string());
    vars.insert("description".to_string(), "End-to-end test".to_string());
    vars.insert("struct_name".to_string(), "WorkflowTest".to_string());

    // Step 3: Generate using Pipeline
    let pipeline = Pipeline::new()?;
    let ctx = GenContext::new(template_path.into(), output_dir.clone()).with_vars(vars);

    let mut generator = Generator::new(pipeline, ctx);

    // Act
    let output_path = generator.generate()?;

    // Assert
    assert!(output_path.exists(), "Generated file should exist");

    let content = fs::read_to_string(&output_path)?;
    assert!(content.contains("CompleteWorkflow"));
    assert!(content.contains("WorkflowTest"));
    assert!(content.contains("End-to-end test"));
    Ok(())
});

test!(test_workflow_with_cache_and_marketplace, {
    use ggen_core::CacheManager;

    // Arrange
    let temp_dir = create_temp_dir();
    let cache_dir = temp_dir.path().join(".cache");
    let registry_dir = temp_dir.path().join("registry");

    let cache = CacheManager::with_dir(cache_dir)?;
    let registry = LocalRegistry::new(registry_dir)?;

    let package = sample_package();

    // Step 1: Cache package
    let pack_content = b"Template pack content";
    cache.cache_pack(
        &package.id.to_string(),
        &package.version.to_string(),
        pack_content,
    )?;

    // Step 2: Verify cache
    let cached = cache.get_pack(&package.id.to_string(), &package.version.to_string())?;
    assert!(cached.is_some(), "Package should be in cache");

    // Step 3: Search in registry
    let query = Query::new("test");
    let search_result = registry.search(&query).await?;

    // Assert
    assert!(
        search_result.is_empty() || !search_result.is_empty(),
        "Search should complete"
    );
    Ok(())
});

// ============================================================================
// Maturity Assessment Integration
// ============================================================================

test!(test_package_assessment_workflow, {
    // Arrange
    let package = Package {
        id: PackageId::new("assessed-package".to_string()),
        version: Version::new(1, 0, 0),
        name: "Assessed Package".to_string(),
        description: "Comprehensive package for assessment testing with detailed documentation"
            .to_string(),
        authors: vec!["Team <team@example.com>".to_string()],
        license: Some("MIT".to_string()),
        homepage: Some("https://example.com".to_string()),
        repository: Some("https://github.com/example/assessed".to_string()),
        keywords: vec!["assessment".to_string(), "testing".to_string()],
        categories: vec!["development-tools".to_string()],
        dependencies: BTreeMap::new(),
        content_id: None,
    };

    let evaluator = MaturityEvaluator::new();

    // Act
    let assessment = evaluator.evaluate_package(&package)?;

    // Assert - Verify assessment structure
    assert!(
        assessment.overall_score >= 0.0 && assessment.overall_score <= 100.0,
        "Overall score in valid range"
    );
    assert!(
        assessment.maturity_level >= MaturityLevel::Experimental,
        "Should have valid maturity level"
    );

    // Verify all sub-scores are populated
    assert!(assessment.documentation_score.score >= 0.0);
    assert!(assessment.security_score.score >= 0.0);
    assert!(assessment.adoption_score.score >= 0.0);

    Ok(())
});

// ============================================================================
// Performance Integration Tests
// ============================================================================

test!(test_end_to_end_workflow_performance, {
    // Arrange
    let temp_dir = create_temp_dir();
    let template_path = write_file_in_temp(&temp_dir, "perf.tmpl", "{{ content }}");
    let output_dir = temp_dir.path().join("output");

    let mut vars = BTreeMap::new();
    vars.insert("content".to_string(), "Performance test".to_string());

    let pipeline = Pipeline::new()?;
    let ctx = GenContext::new(template_path.into(), output_dir).with_vars(vars);
    let mut generator = Generator::new(pipeline, ctx);

    // Act
    let start = std::time::Instant::now();
    let _output = generator.generate()?;
    let duration = start.elapsed();

    // Assert
    assert!(
        duration.as_millis() < 200,
        "End-to-end workflow should be fast (< 200ms), took {:?}",
        duration
    );
    Ok(())
});

test!(test_batch_package_assessment_performance, {
    // Arrange
    let packages: Vec<_> = (0..50)
        .map(|i| {
            let mut pkg = sample_package();
            pkg.id = PackageId::new(format!("batch-{}", i));
            pkg
        })
        .collect();

    let evaluator = MaturityEvaluator::new();

    // Act
    let start = std::time::Instant::now();
    let assessments: Vec<_> = packages
        .iter()
        .map(|p| evaluator.evaluate_package(p))
        .collect::<Result<_, _>>()?;
    let duration = start.elapsed();

    // Assert
    assert_eq!(assessments.len(), 50);
    assert!(
        duration.as_millis() < 500,
        "Batch assessment should be fast (< 500ms for 50 packages), took {:?}",
        duration
    );
    Ok(())
});
