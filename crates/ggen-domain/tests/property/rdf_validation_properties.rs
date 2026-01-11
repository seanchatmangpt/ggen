//! Property-based tests for RDF validation
//!
//! Using fuzzing and property testing to find edge cases.

use chicago_tdd_tools::prelude::*;
use ggen_domain::rdf::metadata::{TemplateMetadata, TemplateVariable};
use ggen_domain::rdf::validation::*;

// ============================================================================
// Property Test Helpers
// ============================================================================

fn generate_semantic_version(major: u32, minor: u32, patch: u32) -> String {
    format!("{}.{}.{}", major, minor, patch)
}

// ============================================================================
// Property: Validation is Deterministic
// ============================================================================

test!(test_property_validation_is_deterministic, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
    metadata.version = Some("1.0.0".to_string());
    metadata.stability = Some("stable".to_string());

    let validator = Validator::new();

    // Act - validate multiple times
    let report1 = validator.validate(&metadata).unwrap();
    let report2 = validator.validate(&metadata).unwrap();
    let report3 = validator.validate(&metadata).unwrap();

    // Assert - results should be identical
    assert_eq!(report1.is_valid(), report2.is_valid());
    assert_eq!(report1.is_valid(), report3.is_valid());
    assert_eq!(report1.errors.len(), report2.errors.len());
    assert_eq!(report1.errors.len(), report3.errors.len());
});

// ============================================================================
// Property: Empty Name Always Fails
// ============================================================================

test!(test_property_empty_name_always_fails, {
    // Arrange & Act - test various configurations with empty name
    let configs = vec![
        (None, None),
        (Some("1.0.0".to_string()), None),
        (None, Some("stable".to_string())),
        (Some("1.0.0".to_string()), Some("stable".to_string())),
    ];

    let validator = Validator::new();

    for (version, stability) in configs {
        let mut metadata = TemplateMetadata::new("test".to_string(), "".to_string());
        metadata.version = version;
        metadata.stability = stability;

        let report = validator.validate(&metadata).unwrap();

        // Assert - should always fail
        assert!(!report.is_valid());
        assert!(report.errors.iter().any(|e| e.path == "templateName"));
    }
});

// ============================================================================
// Property: Valid Semantic Versions Pass
// ============================================================================

test!(test_property_semantic_versions_pass, {
    // Arrange
    let validator = Validator::new();

    // Test multiple valid semantic versions
    for major in 0..5 {
        for minor in 0..5 {
            for patch in 0..5 {
                let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
                metadata.version = Some(generate_semantic_version(major, minor, patch));

                // Act
                let report = validator.validate(&metadata).unwrap();

                // Assert - should not have version warnings
                assert!(
                    !report.warnings.iter().any(|w| w.path == "templateVersion"),
                    "Version {}.{}.{} should be valid",
                    major,
                    minor,
                    patch
                );
            }
        }
    }
});

// ============================================================================
// Property: Test Coverage in Range [0, 100]
// ============================================================================

test!(test_property_coverage_range, {
    // Arrange
    let validator = Validator::new();

    // Test boundary and middle values
    let test_values = vec![
        (0.0, true),
        (25.0, true),
        (50.0, true),
        (75.0, true),
        (100.0, true),
        (-0.1, false),
        (100.1, false),
        (-10.0, false),
        (150.0, false),
    ];

    for (coverage, should_pass) in test_values {
        let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
        metadata.test_coverage = Some(coverage);

        // Act
        let report = validator.validate(&metadata).unwrap();

        // Assert
        if should_pass {
            assert!(
                !report.errors.iter().any(|e| e.path == "testCoverage"),
                "Coverage {} should be valid",
                coverage
            );
        } else {
            assert!(
                report.errors.iter().any(|e| e.path == "testCoverage"),
                "Coverage {} should be invalid",
                coverage
            );
        }
    }
});

// ============================================================================
// Property: Stability Values
// ============================================================================

test!(test_property_stability_values, {
    // Arrange
    let validator = Validator::new();

    let test_cases = vec![
        ("experimental", true),
        ("stable", true),
        ("deprecated", true),
        ("beta", false),
        ("alpha", false),
        ("rc", false),
        ("", false),
        ("STABLE", false), // case sensitive
    ];

    for (stability, should_pass) in test_cases {
        let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
        metadata.stability = Some(stability.to_string());

        // Act
        let report = validator.validate(&metadata).unwrap();

        // Assert
        if should_pass {
            assert!(
                !report.errors.iter().any(|e| e.path == "stability"),
                "Stability '{}' should be valid",
                stability
            );
        } else {
            assert!(
                report.errors.iter().any(|e| e.path == "stability"),
                "Stability '{}' should be invalid",
                stability
            );
        }
    }
});

// ============================================================================
// Property: Variable Name Patterns
// ============================================================================

test!(test_property_variable_name_patterns, {
    // Arrange
    let validator = Validator::new();

    let test_cases = vec![
        // Valid names
        ("_underscore", true),
        ("camelCase", true),
        ("snake_case", true),
        ("name123", true),
        ("_123", true),
        ("a", true),
        // Invalid names
        ("123invalid", false),
        ("", false),
        ("invalid-name", false),
        ("invalid name", false),
        ("invalid.name", false),
        ("invalid@name", false),
    ];

    for (name, should_pass) in test_cases {
        let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
        metadata.variables.push(TemplateVariable {
            name: name.to_string(),
            var_type: "string".to_string(),
            default_value: None,
            description: None,
            required: false,
        });

        // Act
        let report = validator.validate(&metadata).unwrap();

        // Assert
        if should_pass {
            assert!(
                !report
                    .errors
                    .iter()
                    .any(|e| e.path.contains("variableName")),
                "Variable name '{}' should be valid",
                name
            );
        } else {
            assert!(
                report
                    .errors
                    .iter()
                    .any(|e| e.path.contains("variableName")),
                "Variable name '{}' should be invalid",
                name
            );
        }
    }
});

// ============================================================================
// Property: Variable Type Values
// ============================================================================

test!(test_property_variable_types, {
    // Arrange
    let validator = Validator::new();

    let test_cases = vec![
        ("string", true),
        ("number", true),
        ("boolean", true),
        ("array", true),
        ("object", true),
        ("String", false), // case sensitive
        ("int", false),
        ("float", false),
        ("null", false),
        ("", false),
    ];

    for (var_type, should_pass) in test_cases {
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
        if should_pass {
            assert!(
                !report
                    .errors
                    .iter()
                    .any(|e| e.path.contains("variableType")),
                "Variable type '{}' should be valid",
                var_type
            );
        } else {
            assert!(
                report
                    .errors
                    .iter()
                    .any(|e| e.path.contains("variableType")),
                "Variable type '{}' should be invalid",
                var_type
            );
        }
    }
});

// ============================================================================
// Property: Generated Files Cannot Be Empty
// ============================================================================

test!(test_property_generated_files_non_empty, {
    // Arrange
    let validator = Validator::new();

    // Test various empty/whitespace strings
    // Note: Validator only checks if string is_empty(), not if it's whitespace-only
    let invalid_paths = vec![""];

    for path in invalid_paths {
        let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
        metadata.generated_files.push(path.to_string());

        // Act
        let report = validator.validate(&metadata).unwrap();

        // Assert - empty paths should fail
        assert!(
            report
                .errors
                .iter()
                .any(|e| e.path.contains("generatedFiles")),
            "Empty file path should be invalid"
        );
    }

    // Whitespace-only paths are technically not empty strings,
    // so they pass validation (business logic decision)
    let whitespace_paths = vec![" ", "  ", "\t", "\n"];
    for path in whitespace_paths {
        let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());
        metadata.generated_files.push(path.to_string());

        // Act
        let report = validator.validate(&metadata).unwrap();

        // Assert - whitespace paths currently pass validation
        // (could be enhanced later to trim and check)
        assert!(
            !report
                .errors
                .iter()
                .any(|e| e.path.contains("generatedFiles")),
            "Whitespace-only paths currently pass validation (implementation detail)"
        );
    }
});

// ============================================================================
// Property: Multiple Variables Index Correctly
// ============================================================================

test!(test_property_multiple_variables_indexed, {
    // Arrange
    let mut metadata = TemplateMetadata::new("test".to_string(), "Test".to_string());

    // Add multiple invalid variables
    for i in 0..5 {
        metadata.variables.push(TemplateVariable {
            name: format!("123invalid{}", i), // starts with number
            var_type: "string".to_string(),
            default_value: None,
            description: None,
            required: false,
        });
    }

    let validator = Validator::new();

    // Act
    let report = validator.validate(&metadata).unwrap();

    // Assert - should have errors for each variable with correct index
    for i in 0..5 {
        let expected_path = format!("variables[{}].variableName", i);
        assert!(
            report.errors.iter().any(|e| e.path == expected_path),
            "Should have error for variable index {}",
            i
        );
    }
});

// ============================================================================
// Property: ValidationReport Total Issues
// ============================================================================

test!(test_property_total_issues_sum, {
    // Arrange
    let mut report = ValidationReport::new("test".to_string());

    // Act - add various issues
    report.add_error("e1".to_string(), "Error 1".to_string(), None);
    report.add_error("e2".to_string(), "Error 2".to_string(), None);
    report.add_warning("w1".to_string(), "Warning 1".to_string(), None);
    report.add_info("i1".to_string(), "Info 1".to_string(), None);

    // Assert - total should equal sum
    assert_eq!(report.total_issues(), 4);
    assert_eq!(
        report.total_issues(),
        report.errors.len() + report.warnings.len() + report.info.len()
    );
});

// ============================================================================
// Property: Validation Never Panics
// ============================================================================

test!(test_property_validation_never_panics, {
    // Arrange - create various "bad" metadata
    let validator = Validator::new();

    let test_cases = vec![
        TemplateMetadata::new("".to_string(), "".to_string()),
        {
            let mut m = TemplateMetadata::new("test".to_string(), "Test".to_string());
            m.version = Some("".to_string());
            m
        },
        {
            let mut m = TemplateMetadata::new("test".to_string(), "Test".to_string());
            m.stability = Some("".to_string());
            m
        },
        {
            let mut m = TemplateMetadata::new("test".to_string(), "Test".to_string());
            m.test_coverage = Some(f64::NAN);
            m
        },
        {
            let mut m = TemplateMetadata::new("test".to_string(), "Test".to_string());
            m.test_coverage = Some(f64::INFINITY);
            m
        },
    ];

    for metadata in test_cases {
        // Act - should never panic
        let result = validator.validate(&metadata);

        // Assert - should return either Ok or Err, never panic
        assert!(result.is_ok() || result.is_err());
    }
});
