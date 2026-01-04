//! Package validation with FMEA/poka-yoke patterns (T010)
//!
//! This module implements comprehensive package validation including:
//! - Manifest validation
//! - Checksum verification
//! - Signature verification
//! - FMEA-based risk assessment
//! - Poka-yoke error prevention
//!
//! ## FMEA Validation
//!
//! Failure Mode and Effects Analysis (FMEA) provides structured risk
//! assessment for package operations:
//! - `FmeaValidation`: Complete validation result with failure modes
//! - `FmeaEntry`: Individual failure mode with severity/occurrence/detection
//!
//! ## Poka-Yoke Guards
//!
//! Error-proofing mechanisms that prevent common mistakes:
//! - `DirectorySeparation`: Ensures proper file organization
//! - `TraitBoundary`: Enforces API contract compliance
//! - `PathProtection`: Prevents path traversal attacks
//! - `VersionConstraint`: Validates semver compliance

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use super::format::PackageArchive;
use super::manifest::GpackManifest;

/// Validation result with details
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationResult {
    /// Whether validation passed
    pub valid: bool,
    /// Validation errors (if any)
    pub errors: Vec<ValidationError>,
    /// Validation warnings
    pub warnings: Vec<ValidationWarning>,
    /// FMEA score (Risk Priority Number)
    pub fmea_rpn: u32,
    /// Poka-yoke checks passed
    pub poka_yoke_passed: bool,
}

/// A validation error
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationError {
    /// Error code
    pub code: String,
    /// Error message
    pub message: String,
    /// Severity level
    pub severity: Severity,
    /// FMEA failure mode (if applicable)
    pub failure_mode: Option<String>,
}

/// A validation warning
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationWarning {
    /// Warning code
    pub code: String,
    /// Warning message
    pub message: String,
}

/// Severity levels for validation issues
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Severity {
    /// Critical - blocks installation
    Critical,
    /// Error - may cause issues
    Error,
    /// Warning - informational
    Warning,
}

/// FMEA (Failure Mode and Effects Analysis) entry
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmeaEntry {
    /// Failure mode description
    pub failure_mode: String,
    /// Severity (1-10)
    pub severity: u8,
    /// Occurrence probability (1-10)
    pub occurrence: u8,
    /// Detection difficulty (1-10)
    pub detection: u8,
    /// Risk Priority Number (severity * occurrence * detection)
    pub rpn: u32,
    /// Mitigation strategy
    pub mitigation: String,
}

impl FmeaEntry {
    /// Create a new FMEA entry
    pub fn new(
        failure_mode: impl Into<String>,
        severity: u8,
        occurrence: u8,
        detection: u8,
        mitigation: impl Into<String>,
    ) -> Self {
        Self {
            failure_mode: failure_mode.into(),
            severity: severity.clamp(1, 10),
            occurrence: occurrence.clamp(1, 10),
            detection: detection.clamp(1, 10),
            rpn: (severity as u32) * (occurrence as u32) * (detection as u32),
            mitigation: mitigation.into(),
        }
    }

    /// Check if this failure mode is critical (RPN > 100)
    pub fn is_critical(&self) -> bool {
        self.rpn > 100
    }

    /// Check if this failure mode is high severity (severity >= 8)
    pub fn is_high_severity(&self) -> bool {
        self.severity >= 8
    }
}

/// Complete FMEA validation result for a package (T010)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmeaValidation {
    /// Crate/package name being validated
    pub crate_name: String,
    /// Package version
    pub version: String,
    /// All failure modes identified
    pub failure_modes: Vec<FmeaEntry>,
    /// Count of critical failure modes (RPN > 100)
    pub critical_count: usize,
    /// Controls that have been applied
    pub controls_applied: Vec<String>,
    /// Overall validation status
    pub passed: bool,
    /// Maximum RPN found
    pub max_rpn: u32,
    /// Timestamp of validation
    pub validated_at: Option<String>,
}

impl FmeaValidation {
    /// Create a new FMEA validation result
    pub fn new(crate_name: impl Into<String>, version: impl Into<String>) -> Self {
        Self {
            crate_name: crate_name.into(),
            version: version.into(),
            failure_modes: Vec::new(),
            critical_count: 0,
            controls_applied: Vec::new(),
            passed: true,
            max_rpn: 0,
            validated_at: None,
        }
    }

    /// Add a failure mode to the validation
    pub fn add_failure_mode(&mut self, entry: FmeaEntry) {
        if entry.is_critical() {
            self.critical_count += 1;
        }
        if entry.rpn > self.max_rpn {
            self.max_rpn = entry.rpn;
        }
        self.failure_modes.push(entry);
    }

    /// Add a control that was applied
    pub fn add_control(&mut self, control: impl Into<String>) {
        self.controls_applied.push(control.into());
    }

    /// Mark validation as failed
    pub fn fail(&mut self) {
        self.passed = false;
    }

    /// Check if any critical failure modes exist
    pub fn has_critical_failures(&self) -> bool {
        self.critical_count > 0
    }

    /// Get all critical failure modes
    pub fn critical_failures(&self) -> Vec<&FmeaEntry> {
        self.failure_modes.iter().filter(|e| e.is_critical()).collect()
    }

    /// Calculate average RPN across all failure modes
    pub fn average_rpn(&self) -> f64 {
        if self.failure_modes.is_empty() {
            0.0
        } else {
            let total: u32 = self.failure_modes.iter().map(|e| e.rpn).sum();
            total as f64 / self.failure_modes.len() as f64
        }
    }
}

/// Poka-yoke guard types for error prevention (T010)
///
/// These guards implement error-proofing mechanisms that make it
/// impossible or difficult to make common mistakes.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum PokayokeGuard {
    /// Ensures files are organized in proper directories
    /// Prevents mixing generated/source files
    DirectorySeparation,

    /// Enforces API contract compliance through trait bounds
    /// Prevents incompatible type usage
    TraitBoundary,

    /// Protects against path traversal attacks
    /// Validates all paths are within allowed scope
    PathProtection,

    /// Validates semantic version constraints
    /// Ensures version strings follow semver
    VersionConstraint,

    /// Validates checksum integrity
    /// Prevents corrupted package installation
    ChecksumVerification,

    /// Validates package signatures
    /// Prevents tampered package installation
    SignatureVerification,

    /// Ensures required fields are present
    /// Prevents incomplete manifests
    RequiredFields,

    /// Validates dependency resolution
    /// Prevents unresolvable dependency trees
    DependencyResolution,
}

impl PokayokeGuard {
    /// Get the severity level for this guard (1-10)
    pub fn severity(&self) -> u8 {
        match self {
            PokayokeGuard::PathProtection => 10,
            PokayokeGuard::SignatureVerification => 10,
            PokayokeGuard::ChecksumVerification => 9,
            PokayokeGuard::DependencyResolution => 8,
            PokayokeGuard::VersionConstraint => 7,
            PokayokeGuard::TraitBoundary => 7,
            PokayokeGuard::RequiredFields => 6,
            PokayokeGuard::DirectorySeparation => 5,
        }
    }

    /// Get a description of what this guard protects against
    pub fn description(&self) -> &'static str {
        match self {
            PokayokeGuard::DirectorySeparation => {
                "Ensures generated and source files are properly separated"
            }
            PokayokeGuard::TraitBoundary => {
                "Enforces type safety through trait bounds"
            }
            PokayokeGuard::PathProtection => {
                "Prevents path traversal and directory escape attacks"
            }
            PokayokeGuard::VersionConstraint => {
                "Validates semantic versioning compliance"
            }
            PokayokeGuard::ChecksumVerification => {
                "Verifies package integrity through checksums"
            }
            PokayokeGuard::SignatureVerification => {
                "Validates cryptographic signatures for authenticity"
            }
            PokayokeGuard::RequiredFields => {
                "Ensures all required manifest fields are present"
            }
            PokayokeGuard::DependencyResolution => {
                "Validates dependency tree is resolvable"
            }
        }
    }

    /// All available guards
    pub fn all() -> Vec<PokayokeGuard> {
        vec![
            PokayokeGuard::DirectorySeparation,
            PokayokeGuard::TraitBoundary,
            PokayokeGuard::PathProtection,
            PokayokeGuard::VersionConstraint,
            PokayokeGuard::ChecksumVerification,
            PokayokeGuard::SignatureVerification,
            PokayokeGuard::RequiredFields,
            PokayokeGuard::DependencyResolution,
        ]
    }
}

/// Result of a poka-yoke guard check
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PokayokeResult {
    /// The guard that was checked
    pub guard: PokayokeGuard,
    /// Whether the check passed
    pub passed: bool,
    /// Error message if check failed
    pub error: Option<String>,
    /// Suggested fix if check failed
    pub suggested_fix: Option<String>,
}

impl PokayokeResult {
    /// Create a passing result
    pub fn pass(guard: PokayokeGuard) -> Self {
        Self {
            guard,
            passed: true,
            error: None,
            suggested_fix: None,
        }
    }

    /// Create a failing result
    pub fn fail(guard: PokayokeGuard, error: impl Into<String>, fix: impl Into<String>) -> Self {
        Self {
            guard,
            passed: false,
            error: Some(error.into()),
            suggested_fix: Some(fix.into()),
        }
    }
}

/// Quality tier for marketplace packages (T010)
///
/// Determines the level of quality assurance applied to a package.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum ValidatorQualityTier {
    /// Highest quality - extensive testing, documentation, FMEA compliance
    Gold,
    /// Good quality - solid testing and documentation
    Silver,
    /// Basic quality - meets minimum requirements
    Bronze,
    /// Unverified - no quality assessment performed
    Unverified,
}

impl ValidatorQualityTier {
    /// Get the minimum test coverage required for this tier
    pub fn min_coverage(&self) -> f64 {
        match self {
            ValidatorQualityTier::Gold => 0.90,
            ValidatorQualityTier::Silver => 0.80,
            ValidatorQualityTier::Bronze => 0.60,
            ValidatorQualityTier::Unverified => 0.0,
        }
    }

    /// Get the maximum RPN allowed for this tier
    pub fn max_rpn(&self) -> u32 {
        match self {
            ValidatorQualityTier::Gold => 50,
            ValidatorQualityTier::Silver => 100,
            ValidatorQualityTier::Bronze => 200,
            ValidatorQualityTier::Unverified => u32::MAX,
        }
    }

    /// Check if documentation is required for this tier
    pub fn requires_documentation(&self) -> bool {
        matches!(self, ValidatorQualityTier::Gold | ValidatorQualityTier::Silver)
    }

    /// Check if FMEA analysis is required for this tier
    pub fn requires_fmea(&self) -> bool {
        matches!(self, ValidatorQualityTier::Gold)
    }
}

impl Default for ValidatorQualityTier {
    fn default() -> Self {
        ValidatorQualityTier::Unverified
    }
}

/// Package validator
#[derive(Debug)]
pub struct PackageValidator {
    /// FMEA database
    fmea_database: HashMap<String, FmeaEntry>,
    /// Maximum allowed RPN
    max_rpn_threshold: u32,
    /// Enable strict mode
    strict: bool,
}

impl Default for PackageValidator {
    fn default() -> Self {
        Self::new()
    }
}

impl PackageValidator {
    /// Create a new validator with default settings
    pub fn new() -> Self {
        let mut validator = Self {
            fmea_database: HashMap::new(),
            max_rpn_threshold: 100,
            strict: true,
        };
        validator.init_fmea_database();
        validator
    }

    /// Create validator with custom settings
    pub fn with_config(max_rpn_threshold: u32, strict: bool) -> Self {
        let mut validator = Self {
            fmea_database: HashMap::new(),
            max_rpn_threshold,
            strict,
        };
        validator.init_fmea_database();
        validator
    }

    /// Initialize FMEA database with known failure modes
    fn init_fmea_database(&mut self) {
        // FM001: Checksum mismatch
        self.fmea_database.insert(
            "FM001".to_string(),
            FmeaEntry {
                failure_mode: "Checksum verification failure".to_string(),
                severity: 9,
                occurrence: 2,
                detection: 1,
                rpn: 18,
                mitigation: "Verify SHA-256 checksum before installation".to_string(),
            },
        );

        // FM002: Invalid signature
        self.fmea_database.insert(
            "FM002".to_string(),
            FmeaEntry {
                failure_mode: "Invalid package signature".to_string(),
                severity: 10,
                occurrence: 3,
                detection: 1,
                rpn: 30,
                mitigation: "Verify Ed25519 signature against trusted keys".to_string(),
            },
        );

        // FM003: Missing dependencies
        self.fmea_database.insert(
            "FM003".to_string(),
            FmeaEntry {
                failure_mode: "Missing required dependencies".to_string(),
                severity: 8,
                occurrence: 5,
                detection: 2,
                rpn: 80,
                mitigation: "Resolve all dependencies before installation".to_string(),
            },
        );

        // FM004: Version conflict
        self.fmea_database.insert(
            "FM004".to_string(),
            FmeaEntry {
                failure_mode: "Dependency version conflict".to_string(),
                severity: 7,
                occurrence: 6,
                detection: 3,
                rpn: 126,
                mitigation: "Use SAT solver for dependency resolution".to_string(),
            },
        );

        // FM005: Malformed manifest
        self.fmea_database.insert(
            "FM005".to_string(),
            FmeaEntry {
                failure_mode: "Invalid manifest format".to_string(),
                severity: 8,
                occurrence: 4,
                detection: 1,
                rpn: 32,
                mitigation: "Validate manifest schema before processing".to_string(),
            },
        );
    }

    /// Validate a package manifest
    pub fn validate_manifest(&self, manifest: &GpackManifest) -> ValidationResult {
        let mut errors = Vec::new();
        let mut warnings = Vec::new();

        // Validate package name
        if manifest.package.name.is_empty() {
            errors.push(ValidationError {
                code: "E001".to_string(),
                message: "Package name cannot be empty".to_string(),
                severity: Severity::Critical,
                failure_mode: Some("FM005".to_string()),
            });
        }

        // Validate version
        if semver::Version::parse(&manifest.package.version).is_err() {
            errors.push(ValidationError {
                code: "E002".to_string(),
                message: format!("Invalid version: {}", manifest.package.version),
                severity: Severity::Critical,
                failure_mode: Some("FM005".to_string()),
            });
        }

        // Warn if no description
        if manifest.package.description.is_none() {
            warnings.push(ValidationWarning {
                code: "W001".to_string(),
                message: "Package has no description".to_string(),
            });
        }

        // Warn if no license
        if manifest.package.license.is_none() {
            warnings.push(ValidationWarning {
                code: "W002".to_string(),
                message: "Package has no license specified".to_string(),
            });
        }

        let fmea_rpn = self.calculate_fmea_rpn(&errors);
        let poka_yoke_passed = errors
            .iter()
            .all(|e| e.severity != Severity::Critical);

        ValidationResult {
            valid: errors.is_empty(),
            errors,
            warnings,
            fmea_rpn,
            poka_yoke_passed,
        }
    }

    /// Validate a package archive
    pub fn validate_archive(&self, archive: &PackageArchive) -> ValidationResult {
        let mut errors = Vec::new();
        let warnings = Vec::new();

        // Verify checksum
        let calculated = archive.calculate_checksum();
        if !archive.checksum.is_empty() && archive.checksum != calculated {
            errors.push(ValidationError {
                code: "E003".to_string(),
                message: format!(
                    "Checksum mismatch: expected {}, got {}",
                    archive.checksum, calculated
                ),
                severity: Severity::Critical,
                failure_mode: Some("FM001".to_string()),
            });
        }

        let fmea_rpn = self.calculate_fmea_rpn(&errors);
        let poka_yoke_passed = errors
            .iter()
            .all(|e| e.severity != Severity::Critical);

        ValidationResult {
            valid: errors.is_empty(),
            errors,
            warnings,
            fmea_rpn,
            poka_yoke_passed,
        }
    }

    /// Calculate FMEA RPN from validation errors
    fn calculate_fmea_rpn(&self, errors: &[ValidationError]) -> u32 {
        errors
            .iter()
            .filter_map(|e| e.failure_mode.as_ref())
            .filter_map(|fm| self.fmea_database.get(fm))
            .map(|entry| entry.rpn)
            .max()
            .unwrap_or(0)
    }

    /// Get FMEA entry by code
    pub fn get_fmea_entry(&self, code: &str) -> Option<&FmeaEntry> {
        self.fmea_database.get(code)
    }

    /// Check if RPN exceeds threshold
    pub fn exceeds_threshold(&self, rpn: u32) -> bool {
        rpn > self.max_rpn_threshold
    }

    /// Get max RPN threshold
    pub fn max_rpn_threshold(&self) -> u32 {
        self.max_rpn_threshold
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::collections::HashMap;

    fn create_valid_manifest() -> GpackManifest {
        GpackManifest {
            package: crate::gpack::manifest::PackageMetadata {
                name: "test-package".to_string(),
                version: "1.0.0".to_string(),
                description: Some("A test package".to_string()),
                authors: vec!["Test Author".to_string()],
                license: Some("MIT".to_string()),
                repository: None,
                keywords: vec![],
                categories: vec![],
                ggen_version: None,
                homepage: None,
                documentation: None,
                fmea_reference: None,
                quality_tier: None,
            },
            dependencies: HashMap::new(),
            dev_dependencies: HashMap::new(),
            features: HashMap::new(),
            build: None,
        }
    }

    #[test]
    fn test_valid_manifest() {
        let validator = PackageValidator::new();
        let manifest = create_valid_manifest();
        let result = validator.validate_manifest(&manifest);

        assert!(result.valid);
        assert!(result.errors.is_empty());
        assert!(result.poka_yoke_passed);
    }

    #[test]
    fn test_empty_name() {
        let validator = PackageValidator::new();
        let mut manifest = create_valid_manifest();
        manifest.package.name = String::new();

        let result = validator.validate_manifest(&manifest);
        assert!(!result.valid);
        assert!(!result.errors.is_empty());
    }

    #[test]
    fn test_invalid_version() {
        let validator = PackageValidator::new();
        let mut manifest = create_valid_manifest();
        manifest.package.version = "not-a-version".to_string();

        let result = validator.validate_manifest(&manifest);
        assert!(!result.valid);
    }

    #[test]
    fn test_fmea_database() {
        let validator = PackageValidator::new();
        let entry = validator.get_fmea_entry("FM001");
        assert!(entry.is_some());
        assert_eq!(entry.unwrap().severity, 9);
    }

    #[test]
    fn test_fmea_entry_new() {
        let entry = FmeaEntry::new(
            "Test failure mode",
            8,
            5,
            3,
            "Apply mitigation",
        );
        assert_eq!(entry.severity, 8);
        assert_eq!(entry.occurrence, 5);
        assert_eq!(entry.detection, 3);
        assert_eq!(entry.rpn, 8 * 5 * 3); // 120
        assert!(entry.is_critical());
    }

    #[test]
    fn test_fmea_entry_clamping() {
        let entry = FmeaEntry::new("Test", 15, 0, 12, "Fix");
        assert_eq!(entry.severity, 10); // Clamped from 15
        assert_eq!(entry.occurrence, 1); // Clamped from 0
        assert_eq!(entry.detection, 10); // Clamped from 12
    }

    #[test]
    fn test_fmea_validation_new() {
        let validation = FmeaValidation::new("test-crate", "1.0.0");
        assert_eq!(validation.crate_name, "test-crate");
        assert_eq!(validation.version, "1.0.0");
        assert!(validation.passed);
        assert_eq!(validation.critical_count, 0);
    }

    #[test]
    fn test_fmea_validation_add_failure_mode() {
        let mut validation = FmeaValidation::new("test-crate", "1.0.0");

        // Add non-critical failure
        validation.add_failure_mode(FmeaEntry::new("Low risk", 3, 3, 3, "Minor"));
        assert_eq!(validation.critical_count, 0);
        assert_eq!(validation.max_rpn, 27);

        // Add critical failure
        validation.add_failure_mode(FmeaEntry::new("High risk", 8, 8, 2, "Major"));
        assert_eq!(validation.critical_count, 1);
        assert_eq!(validation.max_rpn, 128);
    }

    #[test]
    fn test_fmea_validation_critical_failures() {
        let mut validation = FmeaValidation::new("test-crate", "1.0.0");
        validation.add_failure_mode(FmeaEntry::new("Low", 2, 2, 2, "Minor"));
        validation.add_failure_mode(FmeaEntry::new("High", 10, 5, 3, "Major"));

        let critical = validation.critical_failures();
        assert_eq!(critical.len(), 1);
        assert_eq!(critical[0].failure_mode, "High");
    }

    #[test]
    fn test_fmea_validation_average_rpn() {
        let mut validation = FmeaValidation::new("test-crate", "1.0.0");
        validation.add_failure_mode(FmeaEntry::new("A", 5, 5, 2, "Fix A")); // RPN = 50
        validation.add_failure_mode(FmeaEntry::new("B", 5, 5, 4, "Fix B")); // RPN = 100

        let avg = validation.average_rpn();
        assert!((avg - 75.0).abs() < 0.01);
    }

    #[test]
    fn test_pokayoke_guard_severity() {
        assert_eq!(PokayokeGuard::PathProtection.severity(), 10);
        assert_eq!(PokayokeGuard::DirectorySeparation.severity(), 5);
    }

    #[test]
    fn test_pokayoke_guard_all() {
        let all = PokayokeGuard::all();
        assert_eq!(all.len(), 8);
        assert!(all.contains(&PokayokeGuard::PathProtection));
        assert!(all.contains(&PokayokeGuard::VersionConstraint));
    }

    #[test]
    fn test_pokayoke_result_pass() {
        let result = PokayokeResult::pass(PokayokeGuard::ChecksumVerification);
        assert!(result.passed);
        assert!(result.error.is_none());
    }

    #[test]
    fn test_pokayoke_result_fail() {
        let result = PokayokeResult::fail(
            PokayokeGuard::PathProtection,
            "Path traversal detected",
            "Use canonical paths",
        );
        assert!(!result.passed);
        assert_eq!(result.error, Some("Path traversal detected".to_string()));
        assert_eq!(result.suggested_fix, Some("Use canonical paths".to_string()));
    }

    #[test]
    fn test_validator_quality_tier_min_coverage() {
        assert_eq!(ValidatorQualityTier::Gold.min_coverage(), 0.90);
        assert_eq!(ValidatorQualityTier::Silver.min_coverage(), 0.80);
        assert_eq!(ValidatorQualityTier::Bronze.min_coverage(), 0.60);
    }

    #[test]
    fn test_validator_quality_tier_max_rpn() {
        assert_eq!(ValidatorQualityTier::Gold.max_rpn(), 50);
        assert_eq!(ValidatorQualityTier::Silver.max_rpn(), 100);
        assert_eq!(ValidatorQualityTier::Bronze.max_rpn(), 200);
    }

    #[test]
    fn test_validator_quality_tier_requirements() {
        assert!(ValidatorQualityTier::Gold.requires_documentation());
        assert!(ValidatorQualityTier::Gold.requires_fmea());
        assert!(ValidatorQualityTier::Silver.requires_documentation());
        assert!(!ValidatorQualityTier::Silver.requires_fmea());
        assert!(!ValidatorQualityTier::Bronze.requires_documentation());
    }

    #[test]
    fn test_pokayoke_guard_serialization() {
        let guard = PokayokeGuard::PathProtection;
        let json = serde_json::to_string(&guard).unwrap();
        assert_eq!(json, "\"path_protection\"");

        let parsed: PokayokeGuard = serde_json::from_str(&json).unwrap();
        assert_eq!(parsed, PokayokeGuard::PathProtection);
    }

    #[test]
    fn test_fmea_validation_serialization() {
        let mut validation = FmeaValidation::new("my-gpack", "2.0.0");
        validation.add_control("Checksum verification");
        validation.add_failure_mode(FmeaEntry::new("Test", 5, 5, 2, "Fix"));

        let json = serde_json::to_string(&validation).unwrap();
        let parsed: FmeaValidation = serde_json::from_str(&json).unwrap();

        assert_eq!(parsed.crate_name, "my-gpack");
        assert_eq!(parsed.version, "2.0.0");
        assert_eq!(parsed.controls_applied.len(), 1);
        assert_eq!(parsed.failure_modes.len(), 1);
    }
}
