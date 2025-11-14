//! Unit tests for RDF validation module
//!
//! Testing individual validation functions and business logic.

use chicago_tdd_tools::test;
use ggen_domain::rdf::metadata::{TemplateMetadata, TemplateVariable};
use ggen_domain::rdf::validation::*;

// ============================================================================
// Validator Construction Tests
// ============================================================================

test!(test_validator_new, {
    // Arrange & Act
    let validator = Validator::new();

    // Assert - validator should be created successfully
    // We can't directly test internal state, but we can test it's usable
    let metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    let result = validator.validate(&metadata);
    assert!(result.is_ok());
});

test!(test_validator_default, {
    // Arrange & Act
    let validator = Validator::default();

    // Assert - default should behave like new
    let metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    let result = validator.validate(&metadata);
    assert!(result.is_ok());
});

// ============================================================================
// ValidationReport Tests
// ============================================================================

test!(test_validation_report_new, {
    // Arrange & Act
    let report = ValidationReport::new("template123".to_string());

    // Assert
    assert_eq!(report.template_id, "template123");
    assert!(report.is_valid());
    assert_eq!(report.errors.len(), 0);
    assert_eq!(report.warnings.len(), 0);
    assert_eq!(report.info.len(), 0);
    assert_eq!(report.total_issues(), 0);
});

test!(test_validation_report_add_error, {
    // Arrange
    let mut report = ValidationReport::new("test".to_string());

    // Act
    report.add_error(
        "testPath".to_string(),
        "Test error message".to_string(),
        Some("badValue".to_string()),
    );

    // Assert
    assert!(!report.is_valid());
    assert_eq!(report.errors.len(), 1);
    assert_eq!(report.total_issues(), 1);

    let error = &report.errors[0];
    assert_eq!(error.path, "testPath");
    assert_eq!(error.message, "Test error message");
    assert_eq!(error.value, Some("badValue".to_string()));
    assert_eq!(error.severity, Severity::Error);
});

test!(test_validation_report_add_warning, {
    // Arrange
    let mut report = ValidationReport::new("test".to_string());

    // Act
    report.add_warning("testPath".to_string(), "Test warning".to_string(), None);

    // Assert
    assert!(report.is_valid()); // warnings don't invalidate
    assert_eq!(report.warnings.len(), 1);
    assert_eq!(report.total_issues(), 1);

    let warning = &report.warnings[0];
    assert_eq!(warning.severity, Severity::Warning);
});

test!(test_validation_report_add_info, {
    // Arrange
    let mut report = ValidationReport::new("test".to_string());

    // Act
    report.add_info("testPath".to_string(), "Test info".to_string(), None);

    // Assert
    assert!(report.is_valid()); // info doesn't invalidate
    assert_eq!(report.info.len(), 1);
    assert_eq!(report.total_issues(), 1);

    let info = &report.info[0];
    assert_eq!(info.severity, Severity::Info);
});

test!(test_validation_report_multiple_issues, {
    // Arrange
    let mut report = ValidationReport::new("test".to_string());

    // Act - add various issues
    report.add_error("path1".to_string(), "Error 1".to_string(), None);
    report.add_error("path2".to_string(), "Error 2".to_string(), None);
    report.add_warning("path3".to_string(), "Warning 1".to_string(), None);
    report.add_info("path4".to_string(), "Info 1".to_string(), None);

    // Assert
    assert!(!report.is_valid());
    assert_eq!(report.errors.len(), 2);
    assert_eq!(report.warnings.len(), 1);
    assert_eq!(report.info.len(), 1);
    assert_eq!(report.total_issues(), 4);
});

// ============================================================================
// Template Name Validation Tests
// ============================================================================

test!(test_validate_empty_template_name, {
    // Arrange
    let metadata = TemplateMetadata::new("test".to_string(), "".to_string());
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report.is_valid());
    assert!(report.errors.iter().any(|e| e.path == "templateName"));
    assert!(report.errors.iter().any(|e| e.message.contains("required")));
});

test!(test_validate_valid_template_name, {
    // Arrange
    let metadata = TemplateMetadata::new("test".to_string(), "Valid Name".to_string());
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report.errors.iter().any(|e| e.path == "templateName"));
});

// ============================================================================
// Version Validation Tests
// ============================================================================

test!(test_validate_valid_semantic_version, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.version = Some("1.0.0".to_string());
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report.warnings.iter().any(|w| w.path == "templateVersion"));
});

test!(test_validate_invalid_version_format, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.version = Some("v1.0".to_string());
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(report.warnings.iter().any(|w| w.path == "templateVersion"));
    assert!(report
        .warnings
        .iter()
        .any(|w| w.message.contains("semantic")));
});

test!(test_validate_various_invalid_versions, {
    // Arrange
    let invalid_versions = vec!["1.0", "1", "1.0.0.0", "v1.0.0", "invalid"];
    let validator = Validator::new();

    for version in invalid_versions {
        // Arrange
        let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
        metadata.version = Some(version.to_string());

        // Act
        let report = validator.validate(&metadata).unwrap();

        // Assert
        assert!(
            report.warnings.iter().any(|w| w.path == "templateVersion"),
            "Version '{}' should trigger warning",
            version
        );
    }
});

// ============================================================================
// Stability Validation Tests
// ============================================================================

test!(test_validate_valid_stability_experimental, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.stability = Some("experimental".to_string());
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report.errors.iter().any(|e| e.path == "stability"));
});

test!(test_validate_valid_stability_stable, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.stability = Some("stable".to_string());
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report.errors.iter().any(|e| e.path == "stability"));
});

test!(test_validate_valid_stability_deprecated, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.stability = Some("deprecated".to_string());
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report.errors.iter().any(|e| e.path == "stability"));
});

test!(test_validate_invalid_stability, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.stability = Some("beta".to_string());
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report.is_valid());
    assert!(report.errors.iter().any(|e| e.path == "stability"));
    assert!(report
        .errors
        .iter()
        .any(|e| e.message.contains("experimental")));
});

// ============================================================================
// Test Coverage Validation Tests
// ============================================================================

test!(test_validate_valid_test_coverage, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.test_coverage = Some(85.5);
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report.errors.iter().any(|e| e.path == "testCoverage"));
});

test!(test_validate_test_coverage_boundary_zero, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.test_coverage = Some(0.0);
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report.errors.iter().any(|e| e.path == "testCoverage"));
});

test!(test_validate_test_coverage_boundary_hundred, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.test_coverage = Some(100.0);
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report.errors.iter().any(|e| e.path == "testCoverage"));
});

test!(test_validate_test_coverage_negative, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.test_coverage = Some(-10.0);
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report.is_valid());
    assert!(report.errors.iter().any(|e| e.path == "testCoverage"));
});

test!(test_validate_test_coverage_over_hundred, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.test_coverage = Some(150.0);
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report.is_valid());
    assert!(report.errors.iter().any(|e| e.path == "testCoverage"));
});

// ============================================================================
// Variable Validation Tests
// ============================================================================

test!(test_validate_valid_variable_name, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.variables.push(TemplateVariable {
        name: "valid_name".to_string(),
        var_type: "string".to_string(),
        default_value: None,
        description: None,
        required: false,
    });
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report
        .errors
        .iter()
        .any(|e| e.path.contains("variableName")));
});

test!(test_validate_invalid_variable_name_starts_with_number, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.variables.push(TemplateVariable {
        name: "123invalid".to_string(),
        var_type: "string".to_string(),
        default_value: None,
        description: None,
        required: false,
    });
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report.is_valid());
    assert!(report
        .errors
        .iter()
        .any(|e| e.path.contains("variableName")));
});

test!(test_validate_invalid_variable_name_with_hyphen, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.variables.push(TemplateVariable {
        name: "invalid-name".to_string(),
        var_type: "string".to_string(),
        default_value: None,
        description: None,
        required: false,
    });
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report.is_valid());
    assert!(report
        .errors
        .iter()
        .any(|e| e.path.contains("variableName")));
});

test!(test_validate_valid_variable_types, {
    // Arrange
    let valid_types = vec!["string", "number", "boolean", "array", "object"];
    let validator = Validator::new();

    for var_type in valid_types {
        let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
        metadata.variables.push(TemplateVariable {
            name: "test_var".to_string(),
            var_type: var_type.to_string(),
            default_value: None,
            description: None,
            required: false,
        });

        // Act
        let report = validator.validate(&metadata).unwrap();

        // Assert
        assert!(
            !report
                .errors
                .iter()
                .any(|e| e.path.contains("variableType")),
            "Type '{}' should be valid",
            var_type
        );
    }
});

test!(test_validate_invalid_variable_type, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.variables.push(TemplateVariable {
        name: "test_var".to_string(),
        var_type: "invalid_type".to_string(),
        default_value: None,
        description: None,
        required: false,
    });
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report.is_valid());
    assert!(report
        .errors
        .iter()
        .any(|e| e.path.contains("variableType")));
});

test!(test_validate_required_variable_without_description, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.variables.push(TemplateVariable {
        name: "required_var".to_string(),
        var_type: "string".to_string(),
        default_value: None,
        description: None,
        required: true,
    });
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert - should have info message
    assert!(report.info.iter().any(|i| i.path.contains("description")));
});

// ============================================================================
// Generated Files Validation Tests
// ============================================================================

test!(test_validate_empty_generated_file_path, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.generated_files.push("".to_string());
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report.is_valid());
    assert!(report
        .errors
        .iter()
        .any(|e| e.path.contains("generatedFiles")));
});

test!(test_validate_valid_generated_files, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.generated_files.push("src/main.rs".to_string());
    metadata.generated_files.push("Cargo.toml".to_string());
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report
        .errors
        .iter()
        .any(|e| e.path.contains("generatedFiles")));
});

// ============================================================================
// Recommendation Tests (Info Level)
// ============================================================================

test!(test_validate_missing_description_recommendation, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.description = None;
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert - should have info recommendation
    assert!(report.info.iter().any(|i| i.path == "templateDescription"));
});

test!(test_validate_missing_category_recommendation, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.category = None;
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert - should have info recommendation
    assert!(report.info.iter().any(|i| i.path == "category"));
});

test!(test_validate_missing_tags_recommendation, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.tags = Vec::new();
    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert - should have info recommendation
    assert!(report.info.iter().any(|i| i.path == "tags"));
});

// ============================================================================
// Complex Validation Scenarios
// ============================================================================

test!(test_validate_complete_valid_template, {
    // Arrange
    let mut metadata = TemplateMetadata::new(
        "http://example.org/template1".to_string(),
        "Complete Template".to_string(),
    );
    metadata.version = Some("1.0.0".to_string());
    metadata.description = Some("A complete template".to_string());
    metadata.category = Some("testing".to_string());
    metadata.stability = Some("stable".to_string());
    metadata.test_coverage = Some(95.0);
    metadata.tags = vec!["test".to_string(), "complete".to_string()];
    metadata.variables.push(TemplateVariable {
        name: "project_name".to_string(),
        var_type: "string".to_string(),
        default_value: Some("my-project".to_string()),
        description: Some("Name of the project".to_string()),
        required: true,
    });
    metadata.generated_files.push("src/main.rs".to_string());

    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(report.is_valid());
    assert_eq!(report.errors.len(), 0);
    assert_eq!(report.total_issues(), 0);
});

test!(test_validate_template_with_multiple_errors, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "".to_string());
    metadata.stability = Some("invalid".to_string());
    metadata.test_coverage = Some(-10.0);
    metadata.variables.push(TemplateVariable {
        name: "123invalid".to_string(),
        var_type: "invalid_type".to_string(),
        default_value: None,
        description: None,
        required: false,
    });
    metadata.generated_files.push("".to_string());

    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert
    assert!(!report.is_valid());
    assert!(report.errors.len() >= 5); // Multiple errors expected
});
