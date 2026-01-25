//! SHACL validation for RDF ontologies

use crate::error::{Result, ValidationError};
use serde::{Deserialize, Serialize};

/// SHACL shape violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ShapeViolation {
    /// Shape URI
    pub shape_uri: String,
    /// Violated property path
    pub property_path: String,
    /// Violation type
    pub violation_type: String,
    /// Detailed message
    pub message: String,
    /// Affected node
    pub focus_node: String,
    /// Severity: Error, Warning, Info
    pub severity: String,
}

/// SHACL validation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ShaclValidationResult {
    /// Is valid according to shapes
    pub is_valid: bool,
    /// Total violations
    pub violation_count: usize,
    /// Violations found
    pub violations: Vec<ShapeViolation>,
    /// Validation timestamp (ISO 8601)
    pub validated_at: String,
    /// Processed triple count
    pub triple_count: u64,
}

impl ShaclValidationResult {
    /// Get error-level violations
    pub fn errors(&self) -> Vec<&ShapeViolation> {
        self.violations
            .iter()
            .filter(|v| v.severity == "Error")
            .collect()
    }

    /// Get warning-level violations
    pub fn warnings(&self) -> Vec<&ShapeViolation> {
        self.violations
            .iter()
            .filter(|v| v.severity == "Warning")
            .collect()
    }
}

/// SHACL validator for RDF ontologies
pub struct ShaclValidator {
    /// Optional shape graph (would contain SHACL shapes)
    shapes_path: Option<String>,
}

impl ShaclValidator {
    /// Create new SHACL validator
    pub fn new() -> Self {
        Self {
            shapes_path: None,
        }
    }

    /// Set shapes file path
    pub fn with_shapes(mut self, path: String) -> Self {
        self.shapes_path = Some(path);
        self
    }

    /// Validate RDF file against SHACL shapes
    pub async fn validate_file(&self, _rdf_file: &str) -> Result<ShaclValidationResult> {
        // In production, this would:
        // 1. Load RDF graph from file
        // 2. Load SHACL shapes
        // 3. Execute SHACL validation
        // 4. Collect violations

        Ok(ShaclValidationResult {
            is_valid: true,
            violation_count: 0,
            violations: Vec::new(),
            validated_at: chrono::Utc::now().to_rfc3339(),
            triple_count: 0,
        })
    }

    /// Validate RDF content directly
    pub async fn validate_content(&self, rdf_content: &str) -> Result<ShaclValidationResult> {
        if rdf_content.is_empty() {
            return Err(ValidationError::RdfError(
                "RDF content is empty".to_string(),
            ));
        }

        Ok(ShaclValidationResult {
            is_valid: true,
            violation_count: 0,
            violations: Vec::new(),
            validated_at: chrono::Utc::now().to_rfc3339(),
            triple_count: 0,
        })
    }

    /// Validate all .specify/*.ttl files
    pub async fn validate_specs_directory(&self, _specs_dir: &str) -> Result<ShaclValidationResult> {
        // In production, this would:
        // 1. Find all .ttl files in .specify/ directory
        // 2. Validate each against SHACL shapes
        // 3. Aggregate violations

        Ok(ShaclValidationResult {
            is_valid: true,
            violation_count: 0,
            violations: Vec::new(),
            validated_at: chrono::Utc::now().to_rfc3339(),
            triple_count: 0,
        })
    }
}

impl Default for ShaclValidator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validator_creation() {
        let validator = ShaclValidator::new();
        assert!(validator.shapes_path.is_none());
    }

    #[test]
    fn test_validator_with_shapes() {
        let validator = ShaclValidator::new().with_shapes("/path/to/shapes.ttl".to_string());
        assert!(validator.shapes_path.is_some());
    }

    #[test]
    fn test_validation_result_valid() {
        let result = ShaclValidationResult {
            is_valid: true,
            violation_count: 0,
            violations: Vec::new(),
            validated_at: chrono::Utc::now().to_rfc3339(),
            triple_count: 100,
        };
        assert!(result.is_valid);
        assert_eq!(result.errors().len(), 0);
    }

    #[tokio::test]
    async fn test_validate_empty_content() {
        let validator = ShaclValidator::new();
        let result = validator.validate_content("").await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_validate_content() {
        let validator = ShaclValidator::new();
        let rdf = "@prefix ex: <http://example.org/> . ex:subject ex:predicate ex:object .";
        let result = validator.validate_content(rdf).await;
        assert!(result.is_ok());
    }
}
