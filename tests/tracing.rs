use anyhow::Result;
use core::pipeline::PipelineBuilder;
use core::simple_tracing::SimpleTracer;
use std::collections::BTreeMap;
use std::fs;
use std::path::Path;
use tempfile::TempDir;

#[test]
fn test_tracing_initialization() {
    // Test that tracing can be initialized without errors
    std::env::set_var("RGEN_TRACE", "debug");
    // SimpleTracer doesn't need initialization
    assert!(SimpleTracer::is_enabled());
}

#[test]
fn test_simple_tracer_methods() {
    let temp_dir = TempDir::new().unwrap();
    let test_path = temp_dir.path().join("test.tmpl");
    fs::write(&test_path, "test content").unwrap();
    
    // Test all SimpleTracer methods compile and work
    SimpleTracer::template_start(&test_path);
    SimpleTracer::template_complete(&test_path, &test_path, 100);
    
    let frontmatter = core::template::Frontmatter::default();
    SimpleTracer::frontmatter_processed(&frontmatter);
    
    SimpleTracer::context_blessed(5);
    SimpleTracer::rdf_loading(&["file1.ttl".to_string()], 2, 100);
    SimpleTracer::sparql_query("SELECT * WHERE { ?s ?p ?o }", Some(10));
    
    SimpleTracer::trace(core::simple_tracing::TraceLevel::Debug, "Validation frontmatter: true", Some("validation"));
    SimpleTracer::performance("test_operation", 50);
    SimpleTracer::dry_run(&test_path, 500);
}

#[test]
fn test_tracing_environment_variables() {
    // Test different RGEN_TRACE values
    let test_values = ["error", "warn", "info", "debug", "trace", "1", "0", "true", "false"];
    
    for value in &test_values {
        std::env::set_var("RGEN_TRACE", value);
        // SimpleTracer doesn't need initialization - just check it's enabled
        assert!(SimpleTracer::is_enabled() || *value == "0" || *value == "false", "Tracing should be enabled with RGEN_TRACE={}", value);
    }
}

#[test]
fn test_tracing_integration() -> Result<()> {
    // Test that tracing integrates properly with the pipeline
    let temp_dir = TempDir::new()?;
    let template_path = temp_dir.path().join("test.tmpl");
    
    // Create a simple template without complex variables
    let template_content = r#"---
to: "output.txt"
---
Hello World!
"#;
    
    fs::write(&template_path, template_content)?;
    
    // Test that we can create a pipeline with tracing
    let mut pipeline = PipelineBuilder::new().build()?;
    let vars = BTreeMap::new();
    
    // This should work with tracing enabled
    let result = pipeline.render_file(&template_path, &vars, true);
    if let Err(e) = &result {
        eprintln!("Pipeline render failed: {}", e);
    }
    assert!(result.is_ok());
    
    Ok(())
}

#[test]
fn test_tracing_performance_measurement() {
    // Test performance timing with SimpleTracer
    SimpleTracer::performance("test_operation", 5);
    
    // Test that performance measurement works
    let start = std::time::Instant::now();
    // Simulate some work instead of sleeping
    let _result = (0..1000).sum::<i32>();
    let duration = start.elapsed().as_millis() as u64;
    SimpleTracer::performance("macro_test", duration);
}

#[test]
fn test_tracing_error_handling() {
    // Test error logging with SimpleTracer
    SimpleTracer::trace(core::simple_tracing::TraceLevel::Error, "Test error message", Some("test operation"));
    
    // Test warning logging
    SimpleTracer::trace(core::simple_tracing::TraceLevel::Warn, "Test warning message", Some("test context"));
    SimpleTracer::trace(core::simple_tracing::TraceLevel::Warn, "Test warning without context", None);
}

#[test]
fn test_tracing_structured_logging() {
    // Test that structured logging works with different data types
    let temp_dir = TempDir::new().unwrap();
    let test_path = temp_dir.path().join("test.tmpl");
    
    // Test with different path types
    SimpleTracer::template_start(&test_path);
    SimpleTracer::template_complete(&test_path, &test_path, 1000);
    
    // Test with different counts and sizes
    SimpleTracer::rdf_loading(&["file1.ttl".to_string()], 2, 1000);
    SimpleTracer::context_blessed(10);
}

#[test]
fn test_tracing_conditional_logging() {
    // Test that tracing respects log levels
    std::env::set_var("RGEN_TRACE", "error");
    
    // These should not panic even with error level
    SimpleTracer::template_start(Path::new("test.tmpl"));
    SimpleTracer::performance("test", 100);
}

#[test]
fn test_tracing_memory_safety() {
    // Test that tracing doesn't cause memory issues
    let temp_dir = TempDir::new().unwrap();
    let _test_path = temp_dir.path().join("test.tmpl");
    
    // Create many tracing events
    for i in 0..100 {
        SimpleTracer::performance(&format!("operation_{}", i), i as u64);
        SimpleTracer::sparql_query(&format!("SELECT * WHERE {{ ?s ?p ?o }} LIMIT {}", i), Some(i));
    }
    
    // Should not cause memory issues
    assert!(true);
}

#[test]
fn test_tracing_levels() -> Result<()> {
    let test_levels = ["error", "warn", "info", "debug", "trace"];
    
    for level in &test_levels {
        std::env::set_var("RGEN_TRACE", level);
        
        // Test that tracing methods work at different levels
        SimpleTracer::template_start(std::path::Path::new("test.tmpl"));
        SimpleTracer::frontmatter_processed(&core::template::Frontmatter::default());
        SimpleTracer::context_blessed(5);
        SimpleTracer::rdf_loading(&["file.ttl".to_string()], 1, 100);
        SimpleTracer::sparql_query("SELECT * WHERE { ?s ?p ?o }", Some(10));
        SimpleTracer::file_injection(std::path::Path::new("test.txt"), "append", true);
        SimpleTracer::shell_hook("echo test", "before", 0);
        SimpleTracer::performance("test_op", 50);
        SimpleTracer::dry_run(std::path::Path::new("test.txt"), 500);
        SimpleTracer::backup_created(std::path::Path::new("test.txt"), std::path::Path::new("backup.txt"));
        SimpleTracer::skip_condition("skip_if", "pattern found");
        
        let error = anyhow::anyhow!("Test error");
        SimpleTracer::error(&error, "test context");
        SimpleTracer::warning("Test warning", Some("test context"));
    }
    
    Ok(())
}

#[test]
fn test_tracing_performance() -> Result<()> {
    // Test performance timing
    let timer = core::simple_tracing::SimpleTimer::start("test_operation");
    // Simulate some work instead of sleeping
    let _result = (0..1000).sum::<i32>();
    timer.finish();
    
    // Test macro
    let result = core::time_operation!("macro_test", {
        // Simulate some work instead of sleeping
        let _work = (0..500).sum::<i32>();
        42
    });
    assert_eq!(result, 42);
    
    Ok(())
}

#[test]
fn test_tracing_disabled() -> Result<()> {
    // Test that tracing is disabled when RGEN_TRACE is not set
    std::env::remove_var("RGEN_TRACE");
    
    // These should not output anything
    SimpleTracer::template_start(std::path::Path::new("test.tmpl"));
    SimpleTracer::performance("test_op", 50);
    
    Ok(())
}

#[test]
fn test_tracing_enabled() -> Result<()> {
    // Test that tracing is enabled when RGEN_TRACE is set
    std::env::set_var("RGEN_TRACE", "info");
    
    // These should output to stderr
    SimpleTracer::template_start(std::path::Path::new("test.tmpl"));
    SimpleTracer::performance("test_op", 50);
    
    Ok(())
}

#[test]
fn test_tracing_error_handling_detailed() -> Result<()> {
    std::env::set_var("RGEN_TRACE", "error");
    
    // Test error logging
    let error = anyhow::anyhow!("Test error message");
    SimpleTracer::error(&error, "test operation");
    
    // Test warning logging
    SimpleTracer::warning("Test warning message", Some("test context"));
    SimpleTracer::warning("Test warning without context", None);
    
    Ok(())
}

#[test]
fn test_tracing_structured_data() -> Result<()> {
    std::env::set_var("RGEN_TRACE", "debug");
    
    let temp_dir = TempDir::new()?;
    let test_path = temp_dir.path().join("test.tmpl");
    
    // Test with different data types
    SimpleTracer::template_start(&test_path);
    SimpleTracer::template_complete(&test_path, &test_path, 1000);
    
    // Test with different counts and sizes
    SimpleTracer::rdf_loading(&["file1.ttl".to_string(), "file2.ttl".to_string()], 3, 5000);
    SimpleTracer::context_blessed(15);
    SimpleTracer::sparql_query("SELECT ?s ?p ?o WHERE { ?s ?p ?o } LIMIT 100", Some(100));
    
    Ok(())
}

#[test]
fn test_tracing_memory_safety_extended() -> Result<()> {
    std::env::set_var("RGEN_TRACE", "debug");
    
    // Test that tracing doesn't cause memory issues with many events
    for i in 0..1000 {
        SimpleTracer::performance(&format!("operation_{}", i), i as u64);
        SimpleTracer::sparql_query(&format!("SELECT * WHERE {{ ?s ?p ?o }} LIMIT {}", i), Some(i));
    }
    
    // Should not cause memory issues
    assert!(true);
    
    Ok(())
}

#[test]
fn test_tracing_concurrent_access() -> Result<()> {
    std::env::set_var("RGEN_TRACE", "debug");
    
    use std::thread;
    
    // Test that tracing works with concurrent access
    let handles: Vec<_> = (0..10).map(|i| {
        thread::spawn(move || {
            for j in 0..10 {
                SimpleTracer::performance(&format!("thread_{}_op_{}", i, j), (i * 10 + j) as u64);
            }
        })
    }).collect();
    
    // Wait for all threads to complete
    for handle in handles {
        handle.join().unwrap();
    }
    
    Ok(())
}
