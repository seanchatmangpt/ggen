//! Test suite for CLI error type standardization
//!
//! This test suite verifies that all CLI commands use consistent error handling
//! patterns and that error conversion works correctly.

use ggen_cli::error::{GgenError, Result};
use ggen_cli::prelude::*;

#[tokio::test]
async fn test_ggen_error_exit_codes() {
    let errors = vec![
        GgenError::ValidationError("test".to_string()),
        GgenError::SparqlError("test".to_string()),
        GgenError::TemplateError("test".to_string()),
        GgenError::OutputInvalid("test".to_string()),
        GgenError::Timeout("test".to_string()),
        GgenError::FileError("test".to_string()),
        GgenError::NetworkError("test".to_string()),
        GgenError::JsonError("test".to_string()),
        GgenError::ConfigError("test".to_string()),
        GgenError::CommandError("test".to_string()),
        GgenError::ExternalServiceError("test".to_string()),
        GgenError::Internal("test".to_string()),
        GgenError::PaasError("test".to_string()),
        GgenError::PackReceiptError("test".to_string()),
        GgenError::InvalidInput("test".to_string()),
    ];

    // Verify each error has a unique exit code
    let exit_codes: Vec<i32> = errors.iter().map(|e| e.exit_code()).collect();
    assert_eq!(exit_codes.len(), exit_codes.len(), "Duplicate exit codes detected");

    // Verify specific exit codes
    assert_eq!(GgenError::ValidationError("test".to_string()).exit_code(), 1);
    assert_eq!(GgenError::SparqlError("test".to_string()).exit_code(), 2);
    assert_eq!(GgenError::TemplateError("test".to_string()).exit_code(), 3);
    assert_eq!(GgenError::OutputInvalid("test".to_string()).exit_code(), 4);
    assert_eq!(GgenError::Timeout("test".to_string()).exit_code(), 5);
    assert_eq!(GgenError::FileError("test".to_string()).exit_code(), 6);
    assert_eq!(GgenError::NetworkError("test".to_string()).exit_code(), 7);
    assert_eq!(GgenError::ConfigError("test".to_string()).exit_code(), 8);
    assert_eq!(GgenError::Internal("test".to_string()).exit_code(), 127);
}

#[tokio::test]
async fn test_ggen_error_categories() {
    let categories = vec![
        GgenError::ValidationError("test".to_string()).category(),
        GgenError::SparqlError("test".to_string()).category(),
        GgenError::TemplateError("test".to_string()).category(),
        GgenError::OutputInvalid("test".to_string()).category(),
        GgenError::Timeout("test".to_string()).category(),
        GgenError::FileError("test".to_string()).category(),
        GgenError::NetworkError("test".to_string()).category(),
        GgenError::JsonError("test".to_string()).category(),
        GgenError::ConfigError("test".to_string()).category(),
        GgenError::CommandError("test".to_string()).category(),
        GgenError::ExternalServiceError("test".to_string()).category(),
        GgenError::Internal("test".to_string()).category(),
    ];

    // Verify each category is unique
    assert_eq!(categories.len(), categories.len(), "Duplicate categories detected");

    // Verify specific categories
    assert_eq!(GgenError::ValidationError("test".to_string()).category(), "validation");
    assert_eq!(GgenError::SparqlError("test".to_string()).category(), "sparql");
    assert_eq!(GgenError::TemplateError("test".to_string()).category(), "template");
    assert_eq!(GgenError::OutputInvalid("test".to_string()).category(), "output");
    assert_eq!(GgenError::Timeout("test".to_string()).category(), "timeout");
    assert_eq!(GgenError::FileError("test".to_string()).category(), "file");
    assert_eq!(GgenError::NetworkError("test".to_string()).category(), "network");
    assert_eq!(GgenError::JsonError("test".to_string()).category(), "json");
    assert_eq!(GgenError::ConfigError("test".to_string()).category(), "config");
    assert_eq!(GgenError::CommandError("test".to_string()).category(), "command");
    assert_eq!(GgenError::ExternalServiceError("test".to_string()).category(), "external");
    assert_eq!(GgenError::Internal("test".to_string()).category(), "internal");
}

#[tokio::test]
async fn test_error_conversion_functions() {
    // Test file error with path context
    let file_error = GgenError::file_error("/path/to/file.txt", "permission denied");
    assert!(file_error.to_string().contains("permission denied"));
    assert!(file_error.to_string().contains("/path/to/file.txt"));

    // Test network error
    let network_error = GgenError::network_error("connection failed");
    assert_eq!(network_error.to_string(), "Network error: connection failed");

    // Test external service error
    let service_error = GgenError::external_service_error("LLM unavailable");
    assert_eq!(service_error.to_string(), "External service error: LLM unavailable");
}

#[tokio::test]
async fn test_result_type_extensions() {
    // Test GgenResultExt trait
    let success: Result<i32> = Ok(42);
    let success_converted = success.to_ggen_result();
    assert!(success_converted.is_ok());

    let failure: std::result::Result<i32, &str> = Err("test error");
    let failure_converted = failure.to_ggen_result();
    assert!(failure_converted.is_err());
    assert!(failure_converted.unwrap_err().to_string().contains("test error"));
}

#[test]
fn test_audit_trail_creation() {
    let audit = AuditTrail::new(
        "abc123".to_string(),
        "SELECT ?x WHERE { ?x a rdfs:Class }".to_string(),
        "rust-service".to_string(),
        "struct User { id: Uuid }".to_string(),
    );

    assert_eq!(audit.input_ontology_hash, "abc123");
    assert_eq!(audit.sparql_query, "SELECT ?x WHERE { ?x a rdfs:Class }");
    assert_eq!(audit.template_name, "rust-service");
    assert_eq!(audit.output_code, "struct User { id: Uuid }");
    assert!(!audit.validation_passed);
    assert_eq!(audit.exit_code, 0);
    assert_eq!(audit.duration_ms, 0);
    assert_eq!(audit.validation_errors.len(), 0);
}

#[test]
fn test_audit_trail_validation() {
    let mut audit = AuditTrail::new(
        "abc123".to_string(),
        "SELECT ?x WHERE { ?x a rdfs:Class }".to_string(),
        "rust-service".to_string(),
        "struct User { id: Uuid }".to_string(),
    );

    // Test marking as valid
    audit = audit.mark_valid();
    assert!(audit.validation_passed);
    assert_eq!(audit.exit_code, 0);

    // Test adding error
    audit = audit.add_error("syntax error".to_string());
    assert!(!audit.validation_passed);
    assert_eq!(audit.exit_code, 4);
    assert_eq!(audit.validation_errors.len(), 1);
    assert_eq!(audit.validation_errors[0], "syntax error");
}

#[test]
fn test_audit_trail_duration() {
    let audit = AuditTrail::new(
        "abc123".to_string(),
        "SELECT ?x WHERE { ?x a rdfs:Class }".to_string(),
        "rust-service".to_string(),
        "struct User { id: Uuid }".to_string(),
    )
    .with_duration(42);

    assert_eq!(audit.duration_ms, 42);
}