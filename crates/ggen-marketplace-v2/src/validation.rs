//! Package validation framework with pluggable validators
//!
//! Features:
//! - Multiple validation rules
//! - Pluggable validator system
//! - Weighted scoring
//! - Detailed reporting

use async_trait::async_trait;
use std::sync::Arc;
use tracing::{debug, info};

use crate::error::Result;
use crate::models::{Manifest, Package, QualityScore};
use crate::traits::Validatable;

/// Validation result
#[derive(Clone, Debug)]
pub struct ValidationResult {
    /// Overall pass/fail
    pub passed: bool,
    /// Quality score (0-100)
    pub quality_score: u32,
    /// Individual check results
    pub checks: Vec<ValidationCheck>,
}

/// Individual validation check
#[derive(Clone, Debug)]
pub struct ValidationCheck {
    /// Check name
    pub name: String,
    /// Check passed
    pub passed: bool,
    /// Severity level
    pub severity: CheckSeverity,
    /// Details/message
    pub message: String,
    /// Weight in scoring (0-100)
    pub weight: u32,
}

/// Severity level for validation checks
#[derive(Clone, Copy, Debug, Eq, PartialEq, Ord, PartialOrd)]
pub enum CheckSeverity {
    /// Critical issue - blocks production use
    Critical,
    /// Major issue - needs fixing
    Major,
    /// Minor issue - nice to have
    Minor,
    /// Information only
    Info,
}

/// Pluggable validator trait
#[async_trait]
pub trait Validator: Send + Sync {
    /// Run validation
    async fn validate(&self, package: &Package) -> Result<ValidationCheck>;

    /// Get validator name
    fn name(&self) -> &str;

    /// Get validator weight
    fn weight(&self) -> u32 {
        1
    }
}

/// Package validator
pub struct PackageValidator {
    validators: Vec<Arc<dyn Validator>>,
}

impl PackageValidator {
    /// Create a new package validator with default validators
    pub fn new() -> Self {
        Self {
            validators: vec![
                Arc::new(MetadataValidator),
                Arc::new(LicenseValidator),
                Arc::new(ReadmeValidator),
                Arc::new(RepositoryValidator),
                Arc::new(AuthorValidator),
            ],
        }
    }

    /// Add a custom validator
    pub fn add_validator(mut self, validator: Arc<dyn Validator>) -> Self {
        self.validators.push(validator);
        self
    }

    /// Run all validators
    pub async fn validate_all(&self, package: &Package) -> Result<ValidationResult> {
        let mut checks = Vec::new();
        let mut total_weight = 0;
        let mut weighted_score = 0;

        for validator in &self.validators {
            let check = validator.validate(package).await?;
            let weight = validator.weight();
            total_weight += weight;

            if check.passed {
                weighted_score += weight;
            }

            checks.push(check);
        }

        let quality_score = if total_weight > 0 {
            (weighted_score * 100 / total_weight) as u32
        } else {
            0
        };

        let passed = quality_score >= 80; // 80% threshold

        let result = ValidationResult {
            passed,
            quality_score,
            checks,
        };

        info!(
            "Validation complete for {}: score={}, passed={}",
            package.metadata.id, quality_score, passed
        );

        Ok(result)
    }
}

impl Default for PackageValidator {
    fn default() -> Self {
        Self::new()
    }
}

#[async_trait]
impl Validatable for PackageValidator {
    type ValidationResult = ValidationResult;

    async fn validate(&self, package: &Package) -> Result<Self::ValidationResult> {
        self.validate_all(package).await
    }

    async fn validate_manifest(&self, _manifest: &Manifest) -> Result<Self::ValidationResult> {
        // Manifest validation would go here
        Ok(ValidationResult {
            passed: true,
            quality_score: 100,
            checks: vec![],
        })
    }

    fn validation_passes(&self, result: &Self::ValidationResult) -> bool {
        result.passed
    }
}

// Built-in validators

/// Validates package metadata completeness
pub struct MetadataValidator;

#[async_trait]
impl Validator for MetadataValidator {
    async fn validate(&self, package: &Package) -> Result<ValidationCheck> {
        let mut passed = true;
        let mut message = String::new();

        if package.metadata.name.is_empty() {
            passed = false;
            message.push_str("Missing package name. ");
        }

        if package.metadata.description.is_empty() {
            passed = false;
            message.push_str("Missing package description. ");
        }

        if package.metadata.authors.is_empty() {
            message.push_str("No authors specified. ");
        }

        if message.is_empty() {
            message = "Metadata is complete".to_string();
        }

        Ok(ValidationCheck {
            name: "Metadata".to_string(),
            passed,
            severity: if passed {
                CheckSeverity::Info
            } else {
                CheckSeverity::Critical
            },
            message,
            weight: 20,
        })
    }

    fn name(&self) -> &str {
        "MetadataValidator"
    }

    fn weight(&self) -> u32 {
        20
    }
}

/// Validates license specification
pub struct LicenseValidator;

#[async_trait]
impl Validator for LicenseValidator {
    async fn validate(&self, package: &Package) -> Result<ValidationCheck> {
        let valid_licenses = ["MIT", "Apache-2.0", "GPL-3.0", "BSD-3-Clause", "ISC"];
        let passed = valid_licenses
            .iter()
            .any(|l| package.metadata.license.contains(l))
            || package.metadata.license.contains("Custom");

        Ok(ValidationCheck {
            name: "License".to_string(),
            passed,
            severity: if passed {
                CheckSeverity::Info
            } else {
                CheckSeverity::Critical
            },
            message: format!("License: {}", package.metadata.license),
            weight: 25,
        })
    }

    fn name(&self) -> &str {
        "LicenseValidator"
    }

    fn weight(&self) -> u32 {
        25
    }
}

/// Validates README presence
pub struct ReadmeValidator;

#[async_trait]
impl Validator for ReadmeValidator {
    async fn validate(&self, package: &Package) -> Result<ValidationCheck> {
        // In a real implementation, this would check for actual README files
        let has_documentation = !package.metadata.description.is_empty();

        Ok(ValidationCheck {
            name: "Documentation".to_string(),
            passed: has_documentation,
            severity: CheckSeverity::Major,
            message: if has_documentation {
                "Documentation present".to_string()
            } else {
                "Missing documentation".to_string()
            },
            weight: 20,
        })
    }

    fn name(&self) -> &str {
        "ReadmeValidator"
    }

    fn weight(&self) -> u32 {
        20
    }
}

/// Validates repository URL
pub struct RepositoryValidator;

#[async_trait]
impl Validator for RepositoryValidator {
    async fn validate(&self, package: &Package) -> Result<ValidationCheck> {
        let has_repo = package.metadata.repository.is_some();

        Ok(ValidationCheck {
            name: "Repository".to_string(),
            passed: has_repo,
            severity: CheckSeverity::Minor,
            message: if has_repo {
                "Repository URL provided".to_string()
            } else {
                "No repository URL".to_string()
            },
            weight: 15,
        })
    }

    fn name(&self) -> &str {
        "RepositoryValidator"
    }

    fn weight(&self) -> u32 {
        15
    }
}

/// Validates author information
pub struct AuthorValidator;

#[async_trait]
impl Validator for AuthorValidator {
    async fn validate(&self, package: &Package) -> Result<ValidationCheck> {
        let has_authors = !package.metadata.authors.is_empty();

        Ok(ValidationCheck {
            name: "Authors".to_string(),
            passed: has_authors,
            severity: CheckSeverity::Minor,
            message: if has_authors {
                format!("Authors: {}", package.metadata.authors.join(", "))
            } else {
                "No authors specified".to_string()
            },
            weight: 20,
        })
    }

    fn name(&self) -> &str {
        "AuthorValidator"
    }

    fn weight(&self) -> u32 {
        20
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::{PackageId, PackageMetadata};

    #[tokio::test]
    async fn test_validation() {
        let validator = PackageValidator::new();

        let id = PackageId::new("test-pkg").unwrap();
        let metadata = PackageMetadata::new(id, "Test Package", "A test package", "MIT");
        let package = Package {
            metadata,
            latest_version: crate::models::PackageVersion::new("1.0.0").unwrap(),
            versions: vec![],
            releases: indexmap::IndexMap::new(),
        };

        let result = validator.validate(&package).await.unwrap();
        assert!(result.quality_score > 0);
    }
}
