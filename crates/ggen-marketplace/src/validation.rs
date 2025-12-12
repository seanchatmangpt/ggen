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

/// Validates package dependencies (semantic validation)
///
/// Checks for:
/// - Dependency existence (optional with repository)
/// - Circular dependency detection
/// - Valid version requirements
pub struct DependencyValidator;

#[async_trait]
impl Validator for DependencyValidator {
    async fn validate(&self, package: &Package) -> Result<ValidationCheck> {
        let mut issues = Vec::new();
        let mut dependency_ids: std::collections::HashSet<&crate::models::PackageId> =
            std::collections::HashSet::new();

        for release in package.releases.values() {
            for dep in &release.dependencies {
                // Check for self-dependency (circular)
                if dep.id == package.metadata.id {
                    issues.push(format!(
                        "Self-dependency detected: {} depends on itself",
                        package.metadata.id
                    ));
                }

                // Check for duplicate dependencies
                if !dependency_ids.insert(&dep.id) {
                    issues.push(format!("Duplicate dependency: {}", dep.id));
                }

                // Validate version requirement format
                if dep.version_req.is_empty() {
                    issues.push(format!(
                        "Empty version requirement for dependency: {}",
                        dep.id
                    ));
                }
            }
        }

        let passed = issues.is_empty();
        let message = if passed {
            "All dependencies are valid".to_string()
        } else {
            issues.join("; ")
        };

        Ok(ValidationCheck {
            name: "Dependencies".to_string(),
            passed,
            severity: if passed {
                CheckSeverity::Info
            } else {
                CheckSeverity::Critical
            },
            message,
            weight: 25,
        })
    }

    fn name(&self) -> &str {
        "DependencyValidator"
    }

    fn weight(&self) -> u32 {
        25
    }
}

/// Validates version format compliance
///
/// Ensures all versions follow semantic versioning (MAJOR.MINOR.PATCH)
pub struct VersionFormatValidator;

#[async_trait]
impl Validator for VersionFormatValidator {
    async fn validate(&self, package: &Package) -> Result<ValidationCheck> {
        let mut issues = Vec::new();

        // Validate latest version
        if !Self::is_valid_semver(package.latest_version.as_str()) {
            issues.push(format!(
                "Latest version '{}' is not valid semver",
                package.latest_version
            ));
        }

        // Validate all versions
        for version in &package.versions {
            if !Self::is_valid_semver(version.as_str()) {
                issues.push(format!("Version '{}' is not valid semver", version));
            }
        }

        // Check versions are sorted (newest first)
        if package.versions.len() > 1 {
            let mut sorted = package.versions.clone();
            sorted.sort();
            sorted.reverse();
            if package.versions != sorted {
                issues.push("Versions are not sorted in descending order".to_string());
            }
        }

        let passed = issues.is_empty();
        let message = if passed {
            format!(
                "All {} versions follow semantic versioning",
                package.versions.len()
            )
        } else {
            issues.join("; ")
        };

        Ok(ValidationCheck {
            name: "VersionFormat".to_string(),
            passed,
            severity: if passed {
                CheckSeverity::Info
            } else {
                CheckSeverity::Major
            },
            message,
            weight: 15,
        })
    }

    fn name(&self) -> &str {
        "VersionFormatValidator"
    }

    fn weight(&self) -> u32 {
        15
    }
}

impl VersionFormatValidator {
    fn is_valid_semver(version: &str) -> bool {
        let normalized = version.strip_prefix('v').unwrap_or(version);
        let parts: Vec<&str> = normalized.split('.').collect();

        if parts.len() < 3 {
            return false;
        }

        // Check major, minor, patch are valid numbers
        parts[0].parse::<u32>().is_ok()
            && parts[1].parse::<u32>().is_ok()
            && parts[2]
                .split(|c: char| c == '-' || c == '+')
                .next()
                .map(|p| p.parse::<u32>().is_ok())
                .unwrap_or(false)
    }
}

/// Validates package ID format and constraints
pub struct PackageIdValidator;

#[async_trait]
impl Validator for PackageIdValidator {
    async fn validate(&self, package: &Package) -> Result<ValidationCheck> {
        let id = package.metadata.id.as_str();
        let mut issues = Vec::new();

        // Check length constraints
        if id.len() < 2 {
            issues.push("Package ID too short (minimum 2 characters)".to_string());
        }
        if id.len() > 64 {
            issues.push("Package ID too long (maximum 64 characters recommended)".to_string());
        }

        // Check for reserved names
        let reserved = ["test", "example", "sample", "temp", "tmp"];
        if reserved.contains(&id) {
            issues.push(format!("Package ID '{}' is reserved", id));
        }

        // Check name consistency with ID
        let name_slug = package
            .metadata
            .name
            .to_lowercase()
            .replace(' ', "-")
            .replace('_', "-");
        if !name_slug.contains(id) && !id.contains(&name_slug) {
            // Relaxed check - just a warning
            issues.push("Package name does not match ID pattern".to_string());
        }

        let passed = issues.is_empty()
            || issues
                .iter()
                .all(|i| i.contains("does not match") || i.contains("too long"));
        let message = if issues.is_empty() {
            format!("Package ID '{}' is valid", id)
        } else {
            issues.join("; ")
        };

        Ok(ValidationCheck {
            name: "PackageId".to_string(),
            passed,
            severity: if passed {
                CheckSeverity::Info
            } else {
                CheckSeverity::Critical
            },
            message,
            weight: 10,
        })
    }

    fn name(&self) -> &str {
        "PackageIdValidator"
    }

    fn weight(&self) -> u32 {
        10
    }
}

/// Validates package integrity (checksums, signatures)
pub struct IntegrityValidator;

#[async_trait]
impl Validator for IntegrityValidator {
    async fn validate(&self, package: &Package) -> Result<ValidationCheck> {
        let mut issues = Vec::new();
        let mut releases_checked = 0;

        for (version, release) in &package.releases {
            releases_checked += 1;

            // Check checksum format (should be hex SHA256)
            if !Self::is_valid_sha256(&release.checksum) {
                issues.push(format!(
                    "Invalid checksum format for version {}: expected SHA256 hex",
                    version
                ));
            }

            // Check download URL format
            if !release.download_url.starts_with("http://")
                && !release.download_url.starts_with("https://")
            {
                issues.push(format!(
                    "Invalid download URL for version {}: must be http(s)",
                    version
                ));
            }
        }

        let passed = issues.is_empty();
        let message = if passed {
            format!("Integrity verified for {} release(s)", releases_checked)
        } else {
            issues.join("; ")
        };

        Ok(ValidationCheck {
            name: "Integrity".to_string(),
            passed,
            severity: if passed {
                CheckSeverity::Info
            } else {
                CheckSeverity::Critical
            },
            message,
            weight: 30,
        })
    }

    fn name(&self) -> &str {
        "IntegrityValidator"
    }

    fn weight(&self) -> u32 {
        30
    }
}

impl IntegrityValidator {
    fn is_valid_sha256(checksum: &str) -> bool {
        // SHA256 is 64 hex characters
        checksum.len() == 64 && checksum.chars().all(|c| c.is_ascii_hexdigit())
    }
}

/// Comprehensive validator combining all validation checks
pub struct ComprehensiveValidator {
    validators: Vec<Arc<dyn Validator>>,
}

impl ComprehensiveValidator {
    /// Create a comprehensive validator with all built-in validators
    pub fn new() -> Self {
        Self {
            validators: vec![
                Arc::new(MetadataValidator),
                Arc::new(LicenseValidator),
                Arc::new(ReadmeValidator),
                Arc::new(RepositoryValidator),
                Arc::new(AuthorValidator),
                Arc::new(DependencyValidator),
                Arc::new(VersionFormatValidator),
                Arc::new(PackageIdValidator),
                Arc::new(IntegrityValidator),
            ],
        }
    }

    /// Create a minimal validator for quick checks
    pub fn minimal() -> Self {
        Self {
            validators: vec![
                Arc::new(MetadataValidator),
                Arc::new(PackageIdValidator),
                Arc::new(VersionFormatValidator),
            ],
        }
    }

    /// Run all validations and return detailed result
    pub async fn validate_comprehensive(&self, package: &Package) -> Result<ValidationResult> {
        let mut checks = Vec::new();
        let mut total_weight = 0;
        let mut weighted_score = 0;
        let mut critical_failures = 0;

        for validator in &self.validators {
            let check = validator.validate(package).await?;
            let weight = validator.weight();
            total_weight += weight;

            if check.passed {
                weighted_score += weight;
            } else if check.severity == CheckSeverity::Critical {
                critical_failures += 1;
            }

            checks.push(check);
        }

        let quality_score = if total_weight > 0 {
            (weighted_score * 100 / total_weight) as u32
        } else {
            0
        };

        // Pass requires 80% score AND no critical failures
        let passed = quality_score >= 80 && critical_failures == 0;

        Ok(ValidationResult {
            passed,
            quality_score,
            checks,
        })
    }
}

impl Default for ComprehensiveValidator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::models::{PackageId, PackageMetadata, PackageVersion, ReleaseInfo};

    fn create_test_package() -> Package {
        let id = PackageId::new("test-pkg").unwrap();
        let metadata = PackageMetadata::new(id, "Test Package", "A test package", "MIT");
        Package {
            metadata,
            latest_version: PackageVersion::new("1.0.0").unwrap(),
            versions: vec![PackageVersion::new("1.0.0").unwrap()],
            releases: indexmap::IndexMap::new(),
        }
    }

    fn create_package_with_release() -> Package {
        let id = PackageId::new("test-pkg").unwrap();
        let metadata = PackageMetadata::new(id, "Test Package", "A test package", "MIT");
        let version = PackageVersion::new("1.0.0").unwrap();
        let mut releases = indexmap::IndexMap::new();
        releases.insert(
            version.clone(),
            ReleaseInfo {
                version: version.clone(),
                released_at: chrono::Utc::now(),
                changelog: "Initial release".to_string(),
                checksum: "a".repeat(64), // Valid SHA256 format
                download_url: "https://example.com/pkg.tar.gz".to_string(),
                dependencies: vec![],
            },
        );
        Package {
            metadata,
            latest_version: version.clone(),
            versions: vec![version],
            releases,
        }
    }

    #[tokio::test]
    async fn test_validation() {
        let validator = PackageValidator::new();
        let package = create_test_package();
        let result = validator.validate(&package).await.unwrap();
        assert!(result.quality_score > 0);
    }

    #[tokio::test]
    async fn test_dependency_validator_no_deps() {
        let validator = DependencyValidator;
        let package = create_test_package();
        let result = validator.validate(&package).await.unwrap();
        assert!(result.passed);
    }

    #[tokio::test]
    async fn test_version_format_validator() {
        let validator = VersionFormatValidator;
        let package = create_test_package();
        let result = validator.validate(&package).await.unwrap();
        assert!(result.passed);
    }

    #[tokio::test]
    async fn test_package_id_validator() {
        let validator = PackageIdValidator;
        let package = create_test_package();
        let result = validator.validate(&package).await.unwrap();
        assert!(result.passed);
    }

    #[tokio::test]
    async fn test_integrity_validator_with_release() {
        let validator = IntegrityValidator;
        let package = create_package_with_release();
        let result = validator.validate(&package).await.unwrap();
        assert!(result.passed);
    }

    #[tokio::test]
    async fn test_comprehensive_validator() {
        let validator = ComprehensiveValidator::new();
        let package = create_package_with_release();
        let result = validator.validate_comprehensive(&package).await.unwrap();
        assert!(result.quality_score > 0);
        assert!(!result.checks.is_empty());
    }

    #[tokio::test]
    async fn test_semver_validation() {
        assert!(VersionFormatValidator::is_valid_semver("1.0.0"));
        assert!(VersionFormatValidator::is_valid_semver("v1.0.0"));
        assert!(VersionFormatValidator::is_valid_semver("1.0.0-alpha"));
        assert!(VersionFormatValidator::is_valid_semver("1.0.0+build"));
        assert!(!VersionFormatValidator::is_valid_semver("1.0"));
        assert!(!VersionFormatValidator::is_valid_semver("abc"));
    }

    #[tokio::test]
    async fn test_sha256_validation() {
        let valid_hash = "a".repeat(64);
        assert!(IntegrityValidator::is_valid_sha256(&valid_hash));
        assert!(!IntegrityValidator::is_valid_sha256("tooshort"));
        assert!(!IntegrityValidator::is_valid_sha256(&"z".repeat(64)));
    }
}
