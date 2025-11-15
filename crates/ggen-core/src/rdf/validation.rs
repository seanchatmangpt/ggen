//! SHACL validation for template metadata
//!
//! This module provides validation rules for template metadata using SHACL shapes.
//! It ensures templates have required fields, valid relationships, and consistent structure.
//!
//! ## Features
//!
//! - **SHACL-based validation**: Uses W3C SHACL standard for validation rules
//! - **Template metadata validation**: Validates template structure and relationships
//! - **Rich error reporting**: Detailed validation errors with context
//! - **Extensible rules**: Easy to add new validation rules
//!
//! ## Examples
//!
//! ### Validating Template Metadata
//!
//! ```rust,no_run
//! use ggen_core::rdf::validation::Validator;
//! use ggen_core::rdf::template_metadata::TemplateMetadata;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let validator = Validator::new();
//! let metadata = TemplateMetadata::default();
//!
//! let report = validator.validate(&metadata)?;
//! if report.is_valid() {
//!     println!("Template metadata is valid");
//! } else {
//!     for error in &report.errors {
//!         println!("Validation error: {}", error.message);
//!     }
//! }
//! # Ok(())
//! # }
//! ```
//!
//! ### Getting Validation Report
//!
//! ```rust,no_run
//! use ggen_core::rdf::validation::Validator;
//! use ggen_core::rdf::template_metadata::TemplateMetadata;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! let validator = Validator::new();
//! let metadata = TemplateMetadata::default();
//!
//! let report = validator.validate(&metadata)?;
//! println!("Validation report: {} errors, {} warnings",
//!          report.errors.len(), report.warnings.len());
//! println!("Total issues: {}", report.total_issues());
//! # Ok(())
//! # }
//! ```

use ggen_utils::error::{Error, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use super::template_metadata::TemplateMetadata;

/// SHACL validation result
///
/// Represents the result of validating template metadata against SHACL shapes.
///
/// # Examples
///
/// ```rust
/// use ggen_core::rdf::validation::{ValidationResult, ValidationError, Severity};
///
/// # fn main() {
/// // Valid result
/// let valid = ValidationResult::Valid;
/// match valid {
///     ValidationResult::Valid => assert!(true),
///     ValidationResult::Invalid(_) => panic!("Should be valid"),
/// }
///
/// // Invalid result with errors
/// let errors = vec![ValidationError {
///     severity: Severity::Error,
///     path: "template.name".to_string(),
///     message: "Name is required".to_string(),
///     value: None,
/// }];
/// let invalid = ValidationResult::Invalid(errors);
/// match invalid {
///     ValidationResult::Valid => panic!("Should be invalid"),
///     ValidationResult::Invalid(errs) => assert_eq!(errs.len(), 1),
/// }
/// # }
/// ```
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ValidationResult {
    /// Validation passed - no errors found
    Valid,
    /// Validation failed - contains list of validation errors
    Invalid(Vec<ValidationError>),
}

/// Validation error with details
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub struct ValidationError {
    pub severity: Severity,
    pub path: String,
    pub message: String,
    pub value: Option<String>,
}

/// Error severity levels
///
/// # Examples
///
/// ```rust
/// use ggen_core::rdf::validation::Severity;
///
/// # fn main() {
/// let error = Severity::Error;
/// let warning = Severity::Warning;
/// let info = Severity::Info;
///
/// assert_ne!(error, warning);
/// assert_ne!(warning, info);
/// # }
/// ```
#[derive(Debug, Clone, Copy, Serialize, Deserialize, PartialEq)]
pub enum Severity {
    Error,
    Warning,
    Info,
}

/// Validation report with all findings
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationReport {
    pub template_id: String,
    pub result: ValidationResult,
    pub errors: Vec<ValidationError>,
    pub warnings: Vec<ValidationError>,
    pub info: Vec<ValidationError>,
}

impl ValidationReport {
    /// Create a new validation report for a template
    ///
    /// # Arguments
    ///
    /// * `template_id` - Identifier of the template being validated
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::validation::ValidationReport;
    ///
    /// # fn main() {
    /// let report = ValidationReport::new("template://example/test".to_string());
    /// assert_eq!(report.template_id, "template://example/test");
    /// assert!(report.is_valid());
    /// # }
    /// ```
    pub fn new(template_id: String) -> Self {
        Self {
            template_id,
            result: ValidationResult::Valid,
            errors: Vec::new(),
            warnings: Vec::new(),
            info: Vec::new(),
        }
    }

    pub fn add_error(&mut self, path: String, message: String, value: Option<String>) {
        self.errors.push(ValidationError {
            severity: Severity::Error,
            path,
            message,
            value,
        });
        self.result = ValidationResult::Invalid(self.errors.clone());
    }

    pub fn add_warning(&mut self, path: String, message: String, value: Option<String>) {
        self.warnings.push(ValidationError {
            severity: Severity::Warning,
            path,
            message,
            value,
        });
    }

    pub fn add_info(&mut self, path: String, message: String, value: Option<String>) {
        self.info.push(ValidationError {
            severity: Severity::Info,
            path,
            message,
            value,
        });
    }

    /// Check if the validation report indicates valid metadata
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::validation::ValidationReport;
    ///
    /// # fn main() {
    /// let mut report = ValidationReport::new("template://test".to_string());
    /// assert!(report.is_valid());
    ///
    /// report.add_error("path".to_string(), "Error message".to_string(), None);
    /// assert!(!report.is_valid());
    /// # }
    /// ```
    pub fn is_valid(&self) -> bool {
        matches!(self.result, ValidationResult::Valid)
    }

    /// Get the total number of validation issues (errors + warnings + info)
    ///
    /// # Examples
    ///
    /// ```rust
    /// use ggen_core::rdf::validation::ValidationReport;
    ///
    /// # fn main() {
    /// let mut report = ValidationReport::new("template://test".to_string());
    /// assert_eq!(report.total_issues(), 0);
    ///
    /// report.add_error("path1".to_string(), "Error".to_string(), None);
    /// report.add_warning("path2".to_string(), "Warning".to_string(), None);
    /// assert_eq!(report.total_issues(), 2);
    /// # }
    /// ```
    pub fn total_issues(&self) -> usize {
        self.errors.len() + self.warnings.len() + self.info.len()
    }
}

/// SHACL-based validator for template metadata
pub struct Validator {
    #[allow(dead_code)]
    shapes: HashMap<String, Shape>,
}

/// SHACL shape definition
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct Shape {
    target_class: String,
    properties: Vec<PropertyConstraint>,
}

/// Property constraint in SHACL shape
#[derive(Debug, Clone)]
#[allow(dead_code)]
struct PropertyConstraint {
    path: String,
    min_count: Option<usize>,
    max_count: Option<usize>,
    datatype: Option<String>,
    pattern: Option<String>,
    node_kind: Option<NodeKind>,
}

#[derive(Debug, Clone, PartialEq)]
#[allow(dead_code)]
enum NodeKind {
    Iri,
    Literal,
    BlankNode,
}

impl Validator {
    /// Create new validator with default shapes
    pub fn new() -> Self {
        let mut shapes = HashMap::new();

        // Template shape
        shapes.insert(
            "TemplateShape".to_string(),
            Shape {
                target_class: "http://ggen.dev/ontology#Template".to_string(),
                properties: vec![
                    PropertyConstraint {
                        path: "http://ggen.dev/ontology#templateName".to_string(),
                        min_count: Some(1),
                        max_count: Some(1),
                        datatype: Some("http://www.w3.org/2001/XMLSchema#string".to_string()),
                        pattern: None,
                        node_kind: Some(NodeKind::Literal),
                    },
                    PropertyConstraint {
                        path: "http://ggen.dev/ontology#templateVersion".to_string(),
                        min_count: None,
                        max_count: Some(1),
                        datatype: Some("http://www.w3.org/2001/XMLSchema#string".to_string()),
                        pattern: Some(r"^\d+\.\d+\.\d+$".to_string()), // Semantic versioning
                        node_kind: Some(NodeKind::Literal),
                    },
                    PropertyConstraint {
                        path: "http://ggen.dev/ontology#stability".to_string(),
                        min_count: None,
                        max_count: Some(1),
                        datatype: Some("http://www.w3.org/2001/XMLSchema#string".to_string()),
                        pattern: Some(r"^(experimental|stable|deprecated)$".to_string()),
                        node_kind: Some(NodeKind::Literal),
                    },
                ],
            },
        );

        // Variable shape
        shapes.insert(
            "VariableShape".to_string(),
            Shape {
                target_class: "http://ggen.dev/ontology#Variable".to_string(),
                properties: vec![
                    PropertyConstraint {
                        path: "http://ggen.dev/ontology#variableName".to_string(),
                        min_count: Some(1),
                        max_count: Some(1),
                        datatype: Some("http://www.w3.org/2001/XMLSchema#string".to_string()),
                        pattern: Some(r"^[a-zA-Z_][a-zA-Z0-9_]*$".to_string()), // Valid identifier
                        node_kind: Some(NodeKind::Literal),
                    },
                    PropertyConstraint {
                        path: "http://ggen.dev/ontology#variableType".to_string(),
                        min_count: Some(1),
                        max_count: Some(1),
                        datatype: Some("http://www.w3.org/2001/XMLSchema#string".to_string()),
                        pattern: Some(r"^(string|number|boolean|array|object)$".to_string()),
                        node_kind: Some(NodeKind::Literal),
                    },
                    PropertyConstraint {
                        path: "http://ggen.dev/ontology#isRequired".to_string(),
                        min_count: Some(1),
                        max_count: Some(1),
                        datatype: Some("http://www.w3.org/2001/XMLSchema#boolean".to_string()),
                        pattern: None,
                        node_kind: Some(NodeKind::Literal),
                    },
                ],
            },
        );

        Self { shapes }
    }

    /// Validate template metadata against SHACL shapes
    pub fn validate(&self, metadata: &TemplateMetadata) -> Result<ValidationReport> {
        let mut report = ValidationReport::new(metadata.id.clone());

        // Validate template name
        if metadata.name.is_empty() {
            report.add_error(
                "templateName".to_string(),
                "Template name is required and cannot be empty".to_string(),
                None,
            );
        }

        // Validate version format if present
        if let Some(version) = &metadata.version {
            if !is_semantic_version(version) {
                report.add_warning(
                    "templateVersion".to_string(),
                    "Version should follow semantic versioning (e.g., 1.0.0)".to_string(),
                    Some(version.clone()),
                );
            }
        }

        // Validate stability value
        if let Some(stability) = &metadata.stability {
            if !["experimental", "stable", "deprecated"].contains(&stability.as_str()) {
                report.add_error(
                    "stability".to_string(),
                    "Stability must be one of: experimental, stable, deprecated".to_string(),
                    Some(stability.clone()),
                );
            }
        }

        // Validate test coverage range
        if let Some(coverage) = metadata.test_coverage {
            if !(0.0..=100.0).contains(&coverage) {
                report.add_error(
                    "testCoverage".to_string(),
                    "Test coverage must be between 0 and 100".to_string(),
                    Some(coverage.to_string()),
                );
            }
        }

        // Validate variables
        for (i, var) in metadata.variables.iter().enumerate() {
            let var_path = format!("variables[{}]", i);

            // Validate variable name format
            if !is_valid_identifier(&var.name) {
                report.add_error(
                    format!("{}.variableName", var_path),
                    "Variable name must be a valid identifier (letters, numbers, underscores)"
                        .to_string(),
                    Some(var.name.clone()),
                );
            }

            // Validate variable type
            if !["string", "number", "boolean", "array", "object"].contains(&var.var_type.as_str())
            {
                report.add_error(
                    format!("{}.variableType", var_path),
                    "Variable type must be one of: string, number, boolean, array, object"
                        .to_string(),
                    Some(var.var_type.clone()),
                );
            }

            // Warn if required variable has no description
            if var.required && var.description.is_none() {
                report.add_info(
                    format!("{}.description", var_path),
                    "Required variables should have a description for better documentation"
                        .to_string(),
                    None,
                );
            }
        }

        // Validate generated files have proper paths
        for (i, file) in metadata.generated_files.iter().enumerate() {
            if file.is_empty() {
                report.add_error(
                    format!("generatedFiles[{}]", i),
                    "Generated file path cannot be empty".to_string(),
                    None,
                );
            }
        }

        // Info: Recommend adding description
        if metadata.description.is_none() {
            report.add_info(
                "templateDescription".to_string(),
                "Templates should have a description for better discoverability".to_string(),
                None,
            );
        }

        // Info: Recommend adding category
        if metadata.category.is_none() {
            report.add_info(
                "category".to_string(),
                "Templates should have a category for better organization".to_string(),
                None,
            );
        }

        // Info: Recommend adding tags
        if metadata.tags.is_empty() {
            report.add_info(
                "tags".to_string(),
                "Templates should have tags for better searchability".to_string(),
                None,
            );
        }

        Ok(report)
    }

    /// Validate Turtle RDF against shapes
    pub fn validate_turtle(&self, turtle: &str, template_id: &str) -> Result<ValidationReport> {
        let metadata = TemplateMetadata::from_turtle(turtle, template_id).map_err(|e| {
            Error::with_context("Failed to parse Turtle for validation", &e.to_string())
        })?;
        self.validate(&metadata)
    }
}

impl Default for Validator {
    fn default() -> Self {
        Self::new()
    }
}

/// Check if string is a valid semantic version
fn is_semantic_version(s: &str) -> bool {
    let parts: Vec<&str> = s.split('.').collect();
    if parts.len() != 3 {
        return false;
    }
    parts.iter().all(|p| p.parse::<u32>().is_ok())
}

/// Check if string is a valid identifier
fn is_valid_identifier(s: &str) -> bool {
    if s.is_empty() {
        return false;
    }
    let mut chars = s.chars();
    let Some(first) = chars.next() else {
        return false;
    };
    if !first.is_alphabetic() && first != '_' {
        return false;
    }
    chars.all(|c| c.is_alphanumeric() || c == '_')
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::rdf::template_metadata::TemplateVariable;
    use chicago_tdd_tools::prelude::*;

    test!(test_validate_valid_template, {
        let mut metadata = TemplateMetadata::new(
            "http://example.org/template1".to_string(),
            "Test Template".to_string(),
        );
        metadata.version = Some("1.0.0".to_string());
        metadata.description = Some("A valid template".to_string());
        metadata.category = Some("testing".to_string());
        metadata.stability = Some("stable".to_string());

        let validator = Validator::new();
        let report = validator.validate(&metadata).unwrap();

        assert!(report.is_valid());
        assert_eq!(report.errors.len(), 0);
    });

    test!(test_validate_empty_name, {
        let metadata =
            TemplateMetadata::new("http://example.org/template1".to_string(), "".to_string());

        let validator = Validator::new();
        let report = validator.validate(&metadata).unwrap();

        assert!(!report.is_valid());
        assert!(report.errors.iter().any(|e| e.path == "templateName"));
    });

    test!(test_validate_invalid_version, {
        let mut metadata = TemplateMetadata::new(
            "http://example.org/template1".to_string(),
            "Test".to_string(),
        );
        metadata.version = Some("invalid".to_string());

        let validator = Validator::new();
        let report = validator.validate(&metadata).unwrap();

        assert!(report.warnings.iter().any(|w| w.path == "templateVersion"));
    });

    test!(test_validate_invalid_stability, {
        let mut metadata = TemplateMetadata::new(
            "http://example.org/template1".to_string(),
            "Test".to_string(),
        );
        metadata.stability = Some("invalid".to_string());

        let validator = Validator::new();
        let report = validator.validate(&metadata).unwrap();

        assert!(!report.is_valid());
        assert!(report.errors.iter().any(|e| e.path == "stability"));
    });

    test!(test_validate_variable_name, {
        let mut metadata = TemplateMetadata::new(
            "http://example.org/template1".to_string(),
            "Test".to_string(),
        );
        metadata.variables.push(TemplateVariable {
            name: "123invalid".to_string(),
            var_type: "string".to_string(),
            default_value: None,
            description: None,
            required: false,
        });

        let validator = Validator::new();
        let report = validator.validate(&metadata).unwrap();

        assert!(!report.is_valid());
        assert!(report
            .errors
            .iter()
            .any(|e| e.path.contains("variableName")));
    });

    test!(test_validate_variable_type, {
        let mut metadata = TemplateMetadata::new(
            "http://example.org/template1".to_string(),
            "Test".to_string(),
        );
        metadata.variables.push(TemplateVariable {
            name: "valid_name".to_string(),
            var_type: "invalid_type".to_string(),
            default_value: None,
            description: None,
            required: false,
        });

        let validator = Validator::new();
        let report = validator.validate(&metadata).unwrap();

        assert!(!report.is_valid());
        assert!(report
            .errors
            .iter()
            .any(|e| e.path.contains("variableType")));
    });

    test!(test_is_semantic_version, {
        assert!(is_semantic_version("1.0.0"));
        assert!(is_semantic_version("1.2.3"));
        assert!(is_semantic_version("10.20.30"));

        assert!(!is_semantic_version("1.0"));
        assert!(!is_semantic_version("1"));
        assert!(!is_semantic_version("1.0.0.0"));
        assert!(!is_semantic_version("v1.0.0"));
        assert!(!is_semantic_version("invalid"));
    });

    test!(test_is_valid_identifier, {
        assert!(is_valid_identifier("valid_name"));
        assert!(is_valid_identifier("_underscore"));
        assert!(is_valid_identifier("camelCase"));
        assert!(is_valid_identifier("snake_case"));
        assert!(is_valid_identifier("name123"));

        assert!(!is_valid_identifier("123invalid"));
        assert!(!is_valid_identifier(""));
        assert!(!is_valid_identifier("invalid-name"));
        assert!(!is_valid_identifier("invalid name"));
    });

    test!(test_validation_report, {
        let mut report = ValidationReport::new("test".to_string());

        assert!(report.is_valid());
        assert_eq!(report.total_issues(), 0);

        report.add_error("path".to_string(), "Error".to_string(), None);
        assert!(!report.is_valid());
        assert_eq!(report.errors.len(), 1);

        report.add_warning("path".to_string(), "Warning".to_string(), None);
        assert_eq!(report.warnings.len(), 1);
        assert_eq!(report.total_issues(), 2);

        report.add_info("path".to_string(), "Info".to_string(), None);
        assert_eq!(report.info.len(), 1);
        assert_eq!(report.total_issues(), 3);
    });
}
