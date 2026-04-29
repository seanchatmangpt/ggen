//! Package validation framework with pluggable validators
//!
//! Features:
//! - Multiple validation rules
//! - Pluggable validator system
//! - Weighted scoring
//! - Detailed reporting

use async_trait::async_trait;
use std::sync::Arc;
use tracing::info;
use semver::Version;

use crate::error::Result;
use crate::models::{Manifest, Package};
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
    ///
    /// # Errors
    ///
    /// Implementations may return:
    /// * [`Error::ValidationFailed`] - When validation fails
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
    #[must_use]
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
    #[must_use]
    pub fn add_validator(mut self, validator: Arc<dyn Validator>) -> Self {
        self.validators.push(validator);
        self
    }

    /// Run all validators
    ///
    /// # Errors
    ///
    /// * [`Error::ValidationFailed`] - When any validator fails with a critical error
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

        // u32 * 100 / u32 produces u32; cast is unnecessary
        let quality_score = if total_weight > 0 {
            weighted_score * 100 / total_weight
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

    async fn validate_manifest(&self, manifest: &Manifest) -> Result<Self::ValidationResult> {
        let mut checks = Vec::new();
        let mut passed_count = 0u32;
        let total_checks = 7u32;

        // Check 1: Non-empty ID
        let id_str = manifest.id.as_str();
        let id_check = if id_str.is_empty() {
            ValidationCheck {
                name: "Non-empty ID".to_string(),
                passed: false,
                severity: CheckSeverity::Critical,
                message: "Manifest ID cannot be empty".to_string(),
                weight: 100 / total_checks,
            }
        } else {
            passed_count += 1;
            ValidationCheck {
                name: "Non-empty ID".to_string(),
                passed: true,
                severity: CheckSeverity::Minor,
                message: "Manifest has valid ID".to_string(),
                weight: 100 / total_checks,
            }
        };
        checks.push(id_check);

        // Check 2: Valid semver version
        let version_check = match Version::parse(manifest.version.as_str()) {
            Ok(_) => {
                passed_count += 1;
                ValidationCheck {
                    name: "Valid semver version".to_string(),
                    passed: true,
                    severity: CheckSeverity::Minor,
                    message: format!("Version '{}' is valid semver", manifest.version),
                    weight: 100 / total_checks,
                }
            }
            Err(_) => ValidationCheck {
                name: "Valid semver version".to_string(),
                passed: false,
                severity: CheckSeverity::Critical,
                message: format!("Version '{}' is not valid semver", manifest.version),
                weight: 100 / total_checks,
            },
        };
        checks.push(version_check);

        // Check 3: Non-empty name (from metadata)
        let name_check = if manifest.metadata.name.is_empty() {
            ValidationCheck {
                name: "Non-empty name".to_string(),
                passed: false,
                severity: CheckSeverity::Critical,
                message: "Package name cannot be empty".to_string(),
                weight: 100 / total_checks,
            }
        } else {
            passed_count += 1;
            ValidationCheck {
                name: "Non-empty name".to_string(),
                passed: true,
                severity: CheckSeverity::Minor,
                message: "Package has valid name".to_string(),
                weight: 100 / total_checks,
            }
        };
        checks.push(name_check);

        // Check 4: Non-empty description (from metadata)
        let desc_check = if manifest.metadata.description.is_empty() {
            ValidationCheck {
                name: "Non-empty description".to_string(),
                passed: false,
                severity: CheckSeverity::Major,
                message: "Package description cannot be empty".to_string(),
                weight: 100 / total_checks,
            }
        } else {
            passed_count += 1;
            ValidationCheck {
                name: "Non-empty description".to_string(),
                passed: true,
                severity: CheckSeverity::Minor,
                message: "Package has valid description".to_string(),
                weight: 100 / total_checks,
            }
        };
        checks.push(desc_check);

        // Check 5: Max ID length (≤ 64 chars)
        let id_len_check = if id_str.len() > 64 {
            ValidationCheck {
                name: "ID max length (64)".to_string(),
                passed: false,
                severity: CheckSeverity::Major,
                message: format!("ID length {} exceeds maximum 64 chars", id_str.len()),
                weight: 100 / total_checks,
            }
        } else {
            passed_count += 1;
            ValidationCheck {
                name: "ID max length (64)".to_string(),
                passed: true,
                severity: CheckSeverity::Minor,
                message: "ID length is valid".to_string(),
                weight: 100 / total_checks,
            }
        };
        checks.push(id_len_check);

        // Check 6: Valid license string (non-empty, ASCII)
        let license_check = if manifest.metadata.license.is_empty() || !manifest.metadata.license.is_ascii() {
            ValidationCheck {
                name: "Valid license string".to_string(),
                passed: false,
                severity: CheckSeverity::Major,
                message: "License must be non-empty and ASCII".to_string(),
                weight: 100 / total_checks,
            }
        } else {
            passed_count += 1;
            ValidationCheck {
                name: "Valid license string".to_string(),
                passed: true,
                severity: CheckSeverity::Minor,
                message: "License is valid".to_string(),
                weight: 100 / total_checks,
            }
        };
        checks.push(license_check);

        // Check 7: At least 1 author
        let authors_check = if manifest.metadata.authors.is_empty() {
            ValidationCheck {
                name: "At least one author".to_string(),
                passed: false,
                severity: CheckSeverity::Major,
                message: "Package must have at least one author".to_string(),
                weight: 100 / total_checks,
            }
        } else {
            passed_count += 1;
            ValidationCheck {
                name: "At least one author".to_string(),
                passed: true,
                severity: CheckSeverity::Minor,
                message: format!("Package has {} author(s)", manifest.metadata.authors.len()),
                weight: 100 / total_checks,
            }
        };
        checks.push(authors_check);

        // Compute weighted quality score
        let quality_score = (passed_count * 100) / total_checks;
        let passed = quality_score >= 80;

        Ok(ValidationResult {
            passed,
            quality_score,
            checks,
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

    #[allow(clippy::unnecessary_literal_bound)]
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
            message: format!("License: {license}", license = package.metadata.license),
            weight: 25,
        })
    }

    #[allow(clippy::unnecessary_literal_bound)]
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

    #[allow(clippy::unnecessary_literal_bound)]
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

    #[allow(clippy::unnecessary_literal_bound)]
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
                let authors = package.metadata.authors.join(", ");
                format!("Authors: {authors}")
            } else {
                "No authors specified".to_string()
            },
            weight: 20,
        })
    }

    #[allow(clippy::unnecessary_literal_bound)]
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
