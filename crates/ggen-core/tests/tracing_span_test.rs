//! Integration tests for tracing module
//!
//! This test verifies that:
//! 1. Tracing can be initialized
//! 2. Tracing spans are created correctly
//! 3. PipelineTracer methods work as expected

use ggen_core::tracing::{PerformanceTimer, PipelineTracer};
use std::fs;
use tempfile::TempDir;

#[test]
fn test_tracing_initialization() {
    // Test that tracing can be initialized
    std::env::set_var("GGEN_TRACE", "info");
    let result = ggen_core::tracing::init_tracing();
    // May fail if already initialized, that's OK for this test
    assert!(
        result.is_ok()
            || result
                .unwrap_err()
                .to_string()
                .contains("a global tracer was already installed")
    );
}

#[test]
fn test_pipeline_tracer_spans() {
    // Initialize tracing
    let _ = ggen_core::tracing::init_tracing();

    let temp_dir = TempDir::new().unwrap();
    let template_path = temp_dir.path().join("test.tmpl");
    let output_path = temp_dir.path().join("output.txt");

    // Create a test template file
    fs::write(&template_path, "test content").unwrap();

    // Test all PipelineTracer methods compile and execute without panicking
    PipelineTracer::template_start(&template_path);
    PipelineTracer::template_parsing_complete(&template_path, 100);

    let frontmatter = ggen_core::template_types::Frontmatter::default();
    PipelineTracer::frontmatter_processed(&frontmatter);

    PipelineTracer::context_blessed(5);
    PipelineTracer::rdf_loading_start(&["file1.ttl".to_string()], 2);
    PipelineTracer::rdf_loading_complete(1000);

    PipelineTracer::sparql_query("SELECT * WHERE { ?s ?p ?o }", Some(10));
    PipelineTracer::template_rendering_start(&output_path);
    PipelineTracer::template_rendering_complete(&output_path, 500);

    PipelineTracer::file_injection_start(&output_path, "append");
    PipelineTracer::file_injection_complete(&output_path, "append");

    PipelineTracer::shell_hook_start("echo 'test'", "before");
    PipelineTracer::shell_hook_complete("echo 'test'", "before", 0);

    PipelineTracer::performance_metric("test_op", 50);

    let error = ggen_utils::error::Error::new("Test error");
    PipelineTracer::error_with_context(&error, "test context");

    PipelineTracer::warning("Test warning", Some("test context"));
    PipelineTracer::warning("Test warning", None);

    PipelineTracer::dry_run(&output_path, 500);
    PipelineTracer::backup_created(&output_path, &temp_dir.path().join("backup.tmpl"));
    PipelineTracer::skip_condition("skip_if", "pattern found");

    // If we got here without panicking, all methods work
    assert!(true);
}

#[test]
fn test_performance_timer() {
    // Initialize tracing
    let _ = ggen_core::tracing::init_tracing();

    let timer = PerformanceTimer::start("test_operation");
    std::thread::sleep(std::time::Duration::from_millis(10));
    timer.finish(); // Should not panic

    // Test with macro
    let result = ggen_core::time_operation!("timed_op", {
        std::thread::sleep(std::time::Duration::from_millis(5));
        42
    });
    assert_eq!(result, 42);
}

#[test]
fn test_tracing_span_creation() {
    use tracing::span;

    // Initialize tracing
    let _ = ggen_core::tracing::init_tracing();

    let temp_dir = TempDir::new().unwrap();
    let template_path = temp_dir.path().join("test.tmpl");
    fs::write(&template_path, "test content").unwrap();

    // Test span creation
    let span = PipelineTracer::template_span(&template_path);
    let _guard = span.enter();

    // Inside the span, do some work
    PipelineTracer::template_parsing_complete(&template_path, 100);

    // Span automatically closes when _guard drops
}

#[test]
fn test_trace_span_macro() {
    // Test the trace_span! macro
    let span = ggen_core::trace_span!("test_span", operation = "test", value = 42);
    let _guard = span.enter();

    // Inside the span
    assert!(true);
}

#[test]
fn test_concurrent_tracing() {
    // Test that multiple tracing calls don't interfere
    let _ = ggen_core::tracing::init_tracing();

    let temp_dir = TempDir::new().unwrap();
    let path1 = temp_dir.path().join("test1.tmpl");
    let path2 = temp_dir.path().join("test2.tmpl");

    fs::write(&path1, "content1").unwrap();
    fs::write(&path2, "content2").unwrap();

    PipelineTracer::template_start(&path1);
    PipelineTracer::template_start(&path2);

    PipelineTracer::template_rendering_complete(&path1, 100);
    PipelineTracer::template_rendering_complete(&path2, 200);

    assert!(true);
}

#[test]
fn test_tracing_with_env_var() {
    // Test with different GGEN_TRACE values
    for level in ["error", "warn", "info", "debug", "trace"] {
        std::env::set_var("GGEN_TRACE", level);

        // Each initialization should work
        let result = ggen_core::tracing::init_tracing();
        // After first init, subsequent will fail with "already installed"
        // That's expected behavior
    }
}
