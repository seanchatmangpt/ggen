// v0.4.0 validation tests
// Property-based and integration tests for release validation

use knhk_validation::*;

#[cfg(feature = "std")]
#[test]
fn test_cli_binary_exists() {
    let result = cli_validation::validate_cli_binary_exists();
    assert!(result.passed, "CLI binary should exist: {}", result.message);
}

#[cfg(feature = "std")]
#[test]
fn test_cli_help_command() {
    let result = cli_validation::validate_cli_command("--help", &[]);
    assert!(result.passed, "CLI --help should work: {}", result.message);
}

#[cfg(feature = "std")]
#[test]
fn test_http_client_exists() {
    let result = network_validation::validate_http_client_exists();
    assert!(result.passed, "HTTP client should exist: {}", result.message);
}

#[cfg(feature = "std")]
#[test]
fn test_otel_exporter_exists() {
    let result = network_validation::validate_otel_exporter_exists();
    assert!(result.passed, "OTEL exporter should exist: {}", result.message);
}

#[test]
fn test_receipt_merging_properties() {
    let result = property_validation::validate_receipt_merging_properties();
    assert!(result.passed, "Receipt merging properties should be valid: {}", result.message);
}

#[test]
fn test_iri_hashing_properties() {
    let result = property_validation::validate_iri_hashing_properties();
    assert!(result.passed, "IRI hashing properties should be valid: {}", result.message);
}

#[test]
fn test_guard_constraints() {
    let result = property_validation::validate_guard_constraints();
    assert!(result.passed, "Guard constraints should be valid: {}", result.message);
}

#[test]
fn test_hot_path_performance() {
    let result = performance_validation::validate_hot_path_performance();
    assert!(result.passed, "Hot path performance should be valid: {}", result.message);
}

#[cfg(feature = "std")]
#[test]
fn test_cli_latency() {
    let result = performance_validation::validate_cli_latency();
    assert!(result.passed, "CLI latency should be valid: {}", result.message);
}

#[cfg(feature = "std")]
#[test]
fn test_validation_report() {
    let mut report = ValidationReport::new();
    
    report.add_result(ValidationResult {
        passed: true,
        message: "Test 1".to_string(),
    });
    
    report.add_result(ValidationResult {
        passed: false,
        message: "Test 2".to_string(),
    });
    
    assert_eq!(report.total, 2);
    assert_eq!(report.passed, 1);
    assert_eq!(report.failed, 1);
    assert!(!report.is_success());
}

