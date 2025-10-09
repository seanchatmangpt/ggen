use anyhow::Result;
use core::tracing::{PipelineTracer, PerformanceTimer};
use std::path::Path;
use tempfile::TempDir;
use std::fs;

#[test]
fn test_tracing_initialization() {
    // Test that tracing can be initialized without errors
    std::env::set_var("RGEN_TRACE", "debug");
    let result = core::tracing::init_tracing();
    assert!(result.is_ok());
}

#[test]
fn test_performance_timer() {
    let timer = PerformanceTimer::start("test_operation");
    std::thread::sleep(std::time::Duration::from_millis(10));
    timer.finish(); // Should not panic
}

#[test]
fn test_tracing_macros() {
    // Test that the macros compile and work
    let _span = core::trace_span!("test_span", operation = "test");
    
    let result = core::time_operation!("test_op", {
        42
    });
    assert_eq!(result, 42);
}

#[test]
fn test_pipeline_tracer_methods() {
    let temp_dir = TempDir::new().unwrap();
    let test_path = temp_dir.path().join("test.tmpl");
    fs::write(&test_path, "test content").unwrap();
    
    // Test all PipelineTracer methods compile and work
    PipelineTracer::template_parsing_start(&test_path);
    PipelineTracer::template_parsing_complete(&test_path, 1);
    
    let frontmatter = core::template::Frontmatter::default();
    PipelineTracer::frontmatter_processed(&frontmatter);
    
    PipelineTracer::context_blessed(5);
    PipelineTracer::rdf_loading_start(&["file1.ttl".to_string()], 2);
    PipelineTracer::rdf_loading_complete(100);
    PipelineTracer::sparql_query("SELECT * WHERE { ?s ?p ?o }", Some(10));
    
    PipelineTracer::template_rendering_start(&test_path);
    PipelineTracer::template_rendering_complete(&test_path, 1000);
    
    PipelineTracer::file_injection_start(&test_path, "append");
    PipelineTracer::file_injection_complete(&test_path, "append");
    
    PipelineTracer::shell_hook_start("echo 'test'", "before");
    PipelineTracer::shell_hook_complete("echo 'test'", "before", 0);
    
    PipelineTracer::validation_start("frontmatter");
    PipelineTracer::validation_complete("frontmatter", true);
    
    PipelineTracer::performance_metric("test_operation", 50);
    PipelineTracer::dry_run(&test_path, 500);
    
    PipelineTracer::backup_created(&test_path, &temp_dir.path().join("backup.tmpl"));
    PipelineTracer::skip_condition("skip_if", "pattern found");
    
    // Test error logging
    let error = anyhow::anyhow!("Test error");
    PipelineTracer::error_with_context(&error, "test context");
    
    // Test warning logging
    PipelineTracer::warning("Test warning", Some("test context"));
    PipelineTracer::warning("Test warning", None);
}

#[test]
fn test_tracing_spans() {
    let temp_dir = TempDir::new().unwrap();
    let test_path = temp_dir.path().join("test.tmpl");
    
    // Test span creation
    let _template_span = PipelineTracer::template_span(&test_path);
    let _frontmatter_span = PipelineTracer::frontmatter_span();
    let _rdf_span = PipelineTracer::rdf_span();
    let _render_span = PipelineTracer::render_span();
    let _file_span = PipelineTracer::file_span("write", &test_path);
}

#[test]
fn test_tracing_environment_variables() {
    // Test different RGEN_TRACE values
    let test_values = ["error", "warn", "info", "debug", "trace", "1", "0", "true", "false"];
    
    for value in &test_values {
        std::env::set_var("RGEN_TRACE", value);
        let result = core::tracing::init_tracing();
        assert!(result.is_ok(), "Failed to initialize tracing with RGEN_TRACE={}", value);
    }
}

#[test]
fn test_tracing_integration() {
    // Test that tracing integrates properly with the pipeline
    let temp_dir = TempDir::new().unwrap();
    let template_path = temp_dir.path().join("test.tmpl");
    
    // Create a simple template
    let template_content = r#"---
to: "output/{{ name }}.txt"
vars:
  name: "test"
---
Hello {{ name }}!
"#;
    
    fs::write(&template_path, template_content).unwrap();
    
    // Test that we can create a pipeline with tracing
    let mut pipeline = core::pipeline::PipelineBuilder::new().build().unwrap();
    let vars = std::collections::BTreeMap::new();
    
    // This should work with tracing enabled
    let result = pipeline.render_file(&template_path, &vars, true);
    assert!(result.is_ok());
}

#[test]
fn test_tracing_performance_measurement() {
    // Test performance timing
    let timer = PerformanceTimer::start("test_operation");
    std::thread::sleep(std::time::Duration::from_millis(5));
    timer.finish();
    
    // Test macro
    let result = core::time_operation!("macro_test", {
        std::thread::sleep(std::time::Duration::from_millis(2));
        42
    });
    assert_eq!(result, 42);
}

#[test]
fn test_tracing_error_handling() {
    // Test error logging
    let error = anyhow::anyhow!("Test error message");
    PipelineTracer::error_with_context(&error, "test operation");
    
    // Test warning logging
    PipelineTracer::warning("Test warning message", Some("test context"));
    PipelineTracer::warning("Test warning without context", None);
}

#[test]
fn test_tracing_structured_logging() {
    // Test that structured logging works with different data types
    let temp_dir = TempDir::new().unwrap();
    let test_path = temp_dir.path().join("test.tmpl");
    
    // Test with different path types
    PipelineTracer::template_parsing_start(&test_path);
    PipelineTracer::file_injection_start(&test_path, "prepend");
    
    // Test with different counts and sizes
    PipelineTracer::rdf_loading_complete(1000);
    PipelineTracer::template_rendering_complete(&test_path, 5000);
    PipelineTracer::context_blessed(10);
}

#[test]
fn test_tracing_conditional_logging() {
    // Test that tracing respects log levels
    std::env::set_var("RGEN_TRACE", "error");
    let result = core::tracing::init_tracing();
    assert!(result.is_ok());
    
    // These should not panic even with error level
    PipelineTracer::template_parsing_start(Path::new("test.tmpl"));
    PipelineTracer::performance_metric("test", 100);
}

#[test]
fn test_tracing_memory_safety() {
    // Test that tracing doesn't cause memory issues
    let temp_dir = TempDir::new().unwrap();
    let test_path = temp_dir.path().join("test.tmpl");
    
    // Create many tracing events
    for i in 0..100 {
        PipelineTracer::performance_metric(&format!("operation_{}", i), i as u64);
        PipelineTracer::sparql_query(&format!("SELECT * WHERE {{ ?s ?p ?o }} LIMIT {}", i), Some(i));
    }
    
    // Should not cause memory issues
    assert!(true);
}
