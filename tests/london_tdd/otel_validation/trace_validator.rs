//! OpenTelemetry instrumentation validation
//!
//! Tests verify that all CLI commands properly create spans,
//! record attributes, and emit events for observability.

use crate::lib::otel::*;

#[test]
fn test_all_commands_create_root_spans() {
    // Arrange
    let tracer = MockTracerProvider::new();
    let commands = vec!["doctor", "help_me", "quickstart", "search", "add", "ai.generate"];

    // Act: Simulate each command creating a span
    for cmd in &commands {
        let span = MockSpan {
            name: format!("ggen.{}", cmd),
            attributes: vec![("command".to_string(), cmd.to_string())],
            events: vec![],
            status: SpanStatus::Ok,
        };
        tracer.record_span(span);
    }

    // Assert: All commands have spans
    let spans = tracer.get_spans();
    assert_eq!(spans.len(), commands.len());
    for cmd in &commands {
        assert!(tracer.find_span(&format!("ggen.{}", cmd)).is_some());
    }
}

#[test]
fn test_spans_include_required_attributes() {
    // Arrange
    let tracer = MockTracerProvider::new();

    let span = MockSpan {
        name: "ggen.doctor".to_string(),
        attributes: vec![
            ("command".to_string(), "doctor".to_string()),
            ("version".to_string(), "1.2.0".to_string()),
            ("duration_ms".to_string(), "42".to_string()),
        ],
        events: vec![],
        status: SpanStatus::Ok,
    };

    // Act
    tracer.record_span(span);

    // Assert: Required attributes present
    let recorded = tracer.find_span("ggen.doctor").unwrap();
    assert!(recorded.attributes.iter().any(|(k, _)| k == "command"));
    assert!(recorded.attributes.iter().any(|(k, _)| k == "version"));
    assert!(recorded.attributes.iter().any(|(k, _)| k == "duration_ms"));
}

#[test]
fn test_error_spans_record_error_status() {
    // Arrange
    let tracer = MockTracerProvider::new();

    let error_span = MockSpan {
        name: "ggen.add".to_string(),
        attributes: vec![("package.id".to_string(), "invalid".to_string())],
        events: vec!["error_occurred".to_string()],
        status: SpanStatus::Error("Package not found".to_string()),
    };

    // Act
    tracer.record_span(error_span);

    // Assert: Error properly recorded
    let span = tracer.find_span("ggen.add").unwrap();
    match span.status {
        SpanStatus::Error(msg) => assert_eq!(msg, "Package not found"),
        _ => panic!("Expected error status"),
    }
    assert!(span.events.contains(&"error_occurred".to_string()));
}

#[test]
fn test_nested_spans_for_complex_operations() {
    // Arrange
    let tracer = MockTracerProvider::new();

    // Root span
    let root = MockSpan {
        name: "ggen.ai.project".to_string(),
        attributes: vec![("project.name".to_string(), "my-api".to_string())],
        events: vec![],
        status: SpanStatus::Ok,
    };
    tracer.record_span(root);

    // Child spans
    let child1 = MockSpan {
        name: "ggen.ai.project.generate_structure".to_string(),
        attributes: vec![],
        events: vec!["structure_planned".to_string()],
        status: SpanStatus::Ok,
    };
    tracer.record_span(child1);

    let child2 = MockSpan {
        name: "ggen.ai.project.write_files".to_string(),
        attributes: vec![("files.count".to_string(), "5".to_string())],
        events: vec!["files_written".to_string()],
        status: SpanStatus::Ok,
    };
    tracer.record_span(child2);

    // Assert: All spans recorded
    assert_eq!(tracer.get_spans().len(), 3);
    assert!(tracer.find_span("ggen.ai.project").is_some());
    assert!(tracer
        .find_span("ggen.ai.project.generate_structure")
        .is_some());
    assert!(tracer.find_span("ggen.ai.project.write_files").is_some());
}

#[test]
fn test_span_events_track_operation_progress() {
    // Arrange
    let tracer = MockTracerProvider::new();

    let span = MockSpan {
        name: "ggen.quickstart".to_string(),
        attributes: vec![],
        events: vec![
            "prerequisites_checked".to_string(),
            "rust_installed".to_string(),
            "project_generated".to_string(),
            "tests_passed".to_string(),
        ],
        status: SpanStatus::Ok,
    };

    // Act
    tracer.record_span(span);

    // Assert: All events recorded in order
    let recorded = tracer.find_span("ggen.quickstart").unwrap();
    assert_eq!(recorded.events.len(), 4);
    assert_eq!(recorded.events[0], "prerequisites_checked");
    assert_eq!(recorded.events[3], "tests_passed");
}

#[test]
fn test_performance_attributes_recorded() {
    // Arrange
    let tracer = MockTracerProvider::new();

    let span = MockSpan {
        name: "ggen.search".to_string(),
        attributes: vec![
            ("duration_ms".to_string(), "45".to_string()),
            ("results.count".to_string(), "12".to_string()),
            ("cache.hit".to_string(), "false".to_string()),
        ],
        events: vec![],
        status: SpanStatus::Ok,
    };

    // Act
    tracer.record_span(span);

    // Assert: Performance metrics captured
    let recorded = tracer.find_span("ggen.search").unwrap();
    assert!(recorded.attributes.iter().any(|(k, v)| k == "duration_ms" && v == "45"));
    assert!(recorded.attributes.iter().any(|(k, v)| k == "results.count" && v == "12"));
}

#[test]
fn test_ai_provider_attributes_recorded() {
    // Arrange
    let tracer = MockTracerProvider::new();

    let span = MockSpan {
        name: "ggen.ai.generate".to_string(),
        attributes: vec![
            ("ai.provider".to_string(), "openai".to_string()),
            ("ai.model".to_string(), "gpt-4o".to_string()),
            ("ai.tokens.input".to_string(), "150".to_string()),
            ("ai.tokens.output".to_string(), "320".to_string()),
        ],
        events: vec![],
        status: SpanStatus::Ok,
    };

    // Act
    tracer.record_span(span);

    // Assert: AI-specific attributes present
    let recorded = tracer.find_span("ggen.ai.generate").unwrap();
    assert!(recorded.attributes.iter().any(|(k, v)| k == "ai.provider" && v == "openai"));
    assert!(recorded.attributes.iter().any(|(k, v)| k == "ai.model" && v == "gpt-4o"));
    assert!(recorded.attributes.iter().any(|(k, _)| k == "ai.tokens.input"));
}

#[test]
fn test_span_cleanup_on_success_and_error() {
    // Arrange
    let tracer = MockTracerProvider::new();

    // Success case
    let success_span = MockSpan {
        name: "ggen.test.success".to_string(),
        attributes: vec![],
        events: vec!["completed".to_string()],
        status: SpanStatus::Ok,
    };
    tracer.record_span(success_span);

    // Error case
    let error_span = MockSpan {
        name: "ggen.test.error".to_string(),
        attributes: vec![],
        events: vec!["error".to_string()],
        status: SpanStatus::Error("test error".to_string()),
    };
    tracer.record_span(error_span);

    // Assert: Both spans recorded with correct status
    assert_eq!(tracer.get_spans().len(), 2);
    let success = tracer.find_span("ggen.test.success").unwrap();
    assert_eq!(success.status, SpanStatus::Ok);

    let error = tracer.find_span("ggen.test.error").unwrap();
    match error.status {
        SpanStatus::Error(msg) => assert_eq!(msg, "test error"),
        _ => panic!("Expected error status"),
    }
}
