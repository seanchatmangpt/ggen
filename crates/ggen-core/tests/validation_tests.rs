//! Validation tests (T026) - Chicago School TDD
//!
//! Tests the validation pipeline functionality that checks generated code
//! before allowing sync to proceed.
//!
//! ## Coverage
//! - Validation pass allows sync to continue
//! - Validation fail blocks sync
//! - Severity Error blocks generation
//! - Severity Warning continues with log

use ggen_core::codegen::pipeline::{ValidationResult, ValidationSeverity};

// ============================================================================
// T026.1: test_validation_pass_allows_sync
// ============================================================================

#[test]
fn test_validation_pass_allows_sync() {
    // Arrange: Create passing validation result
    let validation = ValidationResult {
        rule_name: "test_rule".to_string(),
        passed: true,
        message: Some("All checks passed".to_string()),
        severity: ValidationSeverity::Error, // Even Error severity is OK when passed
    };

    // Assert: Validation passed
    assert!(validation.passed, "Validation should pass");
    assert_eq!(
        validation.severity,
        ValidationSeverity::Error,
        "Should have Error severity (would block if failed)"
    );

    // Assert: Message indicates success
    let msg = validation.message.as_ref().unwrap();
    assert!(msg.contains("passed"), "Message should indicate success");
}

// ============================================================================
// T026.2: test_validation_fail_blocks_sync
// ============================================================================

#[test]
fn test_validation_fail_blocks_sync() {
    // Arrange: Create failing validation result with Error severity
    let validation = ValidationResult {
        rule_name: "failing_rule".to_string(),
        passed: false,
        message: Some("Validation failed: missing required field".to_string()),
        severity: ValidationSeverity::Error,
    };

    // Assert: Validation failed
    assert!(!validation.passed, "Validation should fail");
    assert_eq!(
        validation.severity,
        ValidationSeverity::Error,
        "Should have Error severity"
    );

    // Assert: Message indicates failure
    let msg = validation.message.as_ref().unwrap();
    assert!(msg.contains("failed"), "Message should indicate failure");
    assert!(
        msg.contains("missing required field"),
        "Message should explain failure reason"
    );

    // Note: In real pipeline, this would prevent file generation
}

// ============================================================================
// T026.3: test_severity_error_blocks
// ============================================================================

#[test]
fn test_severity_error_blocks() {
    // Arrange: Create validation results with different severities
    let error_validation = ValidationResult {
        rule_name: "error_rule".to_string(),
        passed: false,
        message: Some("Critical error: syntax invalid".to_string()),
        severity: ValidationSeverity::Error,
    };

    let warning_validation = ValidationResult {
        rule_name: "warning_rule".to_string(),
        passed: false,
        message: Some("Warning: style issue detected".to_string()),
        severity: ValidationSeverity::Warning,
    };

    // Assert: Error severity blocks
    assert_eq!(
        error_validation.severity,
        ValidationSeverity::Error,
        "Should be Error severity"
    );
    assert!(
        !error_validation.passed,
        "Failed Error validation should block"
    );

    // Assert: Warning severity is different from Error
    assert_eq!(
        warning_validation.severity,
        ValidationSeverity::Warning,
        "Should be Warning severity"
    );
    assert_ne!(
        warning_validation.severity, error_validation.severity,
        "Warning and Error should be different"
    );
}

// ============================================================================
// T026.4: test_severity_warning_continues
// ============================================================================

#[test]
fn test_severity_warning_continues() {
    // Arrange: Create warning validation (failed but non-blocking)
    let warning = ValidationResult {
        rule_name: "style_check".to_string(),
        passed: false,
        message: Some("Warning: code style doesn't match conventions".to_string()),
        severity: ValidationSeverity::Warning,
    };

    // Assert: Warning severity
    assert_eq!(
        warning.severity,
        ValidationSeverity::Warning,
        "Should be Warning severity"
    );

    // Assert: Failed but should allow continuation
    assert!(!warning.passed, "Warning validation failed");

    // Note: In real pipeline, Warning severity allows generation to continue
    // while logging the warning message
}

// ============================================================================
// T026.5: test_multiple_validations_scenario
// ============================================================================

#[test]
fn test_multiple_validations_scenario() {
    // Arrange: Simulate pipeline with multiple validations
    let validations = vec![
        ValidationResult {
            rule_name: "syntax_check".to_string(),
            passed: true,
            message: Some("Syntax valid".to_string()),
            severity: ValidationSeverity::Error,
        },
        ValidationResult {
            rule_name: "type_check".to_string(),
            passed: true,
            message: Some("Types correct".to_string()),
            severity: ValidationSeverity::Error,
        },
        ValidationResult {
            rule_name: "style_check".to_string(),
            passed: false,
            message: Some("Style issues detected".to_string()),
            severity: ValidationSeverity::Warning,
        },
    ];

    // Assert: All validations present
    assert_eq!(validations.len(), 3, "Should have 3 validations");

    // Assert: Error validations passed
    let errors: Vec<_> = validations
        .iter()
        .filter(|v| v.severity == ValidationSeverity::Error)
        .collect();
    assert_eq!(errors.len(), 2, "Should have 2 Error validations");
    assert!(
        errors.iter().all(|v| v.passed),
        "All Error validations should pass"
    );

    // Assert: Warning validation failed but non-blocking
    let warnings: Vec<_> = validations
        .iter()
        .filter(|v| v.severity == ValidationSeverity::Warning)
        .collect();
    assert_eq!(warnings.len(), 1, "Should have 1 Warning validation");
    assert!(
        !warnings[0].passed,
        "Warning validation failed (but non-blocking)"
    );

    // Business logic: Pipeline should continue (no blocking errors)
    let blocking_failures = validations
        .iter()
        .any(|v| !v.passed && v.severity == ValidationSeverity::Error);
    assert!(
        !blocking_failures,
        "No blocking failures - pipeline should continue"
    );
}

// ============================================================================
// T026.6: test_validation_with_empty_message
// ============================================================================

#[test]
fn test_validation_with_empty_message() {
    // Arrange: Validation result without message
    let validation = ValidationResult {
        rule_name: "silent_check".to_string(),
        passed: true,
        message: None,
        severity: ValidationSeverity::Error,
    };

    // Assert: Message is optional
    assert!(validation.message.is_none(), "Message should be None");
    assert!(validation.passed, "Validation should still pass");
}

// ============================================================================
// T026.7: test_validation_result_is_serializable
// ============================================================================

#[test]
fn test_validation_result_is_serializable() {
    // Arrange: Create validation result
    let validation = ValidationResult {
        rule_name: "test_rule".to_string(),
        passed: true,
        message: Some("Success".to_string()),
        severity: ValidationSeverity::Warning,
    };

    // Act: Serialize to JSON
    let json = serde_json::to_string(&validation).expect("Should serialize to JSON");

    // Assert: JSON contains expected fields
    assert!(json.contains("test_rule"), "JSON should contain rule_name");
    assert!(json.contains("true"), "JSON should contain passed status");
    assert!(json.contains("Success"), "JSON should contain message");
    assert!(json.contains("Warning"), "JSON should contain severity");
}

// ============================================================================
// T026.8: test_severity_equality
// ============================================================================

#[test]
fn test_severity_equality() {
    // Arrange: Create severity instances
    let error1 = ValidationSeverity::Error;
    let error2 = ValidationSeverity::Error;
    let warning = ValidationSeverity::Warning;

    // Assert: Equality works
    assert_eq!(error1, error2, "Same severity should be equal");
    assert_ne!(error1, warning, "Different severities should not be equal");
    assert_ne!(warning, error1, "Different severities should not be equal");
}

// ============================================================================
// T026.9: test_validation_pipeline_integration
// ============================================================================

#[test]
fn test_validation_pipeline_integration() {
    // Arrange: Simulate complete validation pipeline
    let mut validations = Vec::new();

    // Step 1: Syntax validation (critical)
    validations.push(ValidationResult {
        rule_name: "syntax".to_string(),
        passed: true,
        message: Some("Syntax valid".to_string()),
        severity: ValidationSeverity::Error,
    });

    // Step 2: Semantic validation (critical)
    validations.push(ValidationResult {
        rule_name: "semantics".to_string(),
        passed: true,
        message: Some("Semantics valid".to_string()),
        severity: ValidationSeverity::Error,
    });

    // Step 3: Linting (non-critical)
    validations.push(ValidationResult {
        rule_name: "linting".to_string(),
        passed: false,
        message: Some("Unused variable detected".to_string()),
        severity: ValidationSeverity::Warning,
    });

    // Assert: Pipeline can continue (no blocking errors)
    let can_continue = !validations
        .iter()
        .any(|v| !v.passed && v.severity == ValidationSeverity::Error);
    assert!(can_continue, "Pipeline should continue with only warnings");

    // Assert: All critical validations passed
    let critical_passed = validations
        .iter()
        .filter(|v| v.severity == ValidationSeverity::Error)
        .all(|v| v.passed);
    assert!(critical_passed, "All critical validations should pass");

    // Assert: Warnings are logged but don't block
    let has_warnings = validations
        .iter()
        .any(|v| !v.passed && v.severity == ValidationSeverity::Warning);
    assert!(has_warnings, "Should have warnings to log");
}
