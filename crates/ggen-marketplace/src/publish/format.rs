//! T013: Manifest Validation and Schema
//!
//! Validates gpack manifests for crates.io publication compliance.
//! Enforces crate naming rules, SemVer, and FMEA requirements.
//!
//! ## Validation Rules
//!
//! 1. **Crate Name Pattern**: ASCII alphanumeric + underscore/hyphen, starts with letter
//! 2. **SemVer Compliance**: Valid semantic versioning (major.minor.patch)
//! 3. **Required Fields**: name, version, description, license
//! 4. **FMEA Requirement**: Documentation of failure modes (configurable)
//! 5. **Keywords/Categories**: Max 5 each, proper formatting

use crate::error::{Error, Result};
use crate::publish::manifest::{DependencySpec, GpackManifest};
use std::collections::HashSet;

/// Validation error with details
#[derive(Debug, Clone, PartialEq)]
pub struct ValidationError {
    /// Field or component that failed validation
    pub field: String,
    /// Error message
    pub message: String,
    /// Severity level
    pub severity: ErrorSeverity,
    /// Suggestion for fixing the error
    pub suggestion: Option<String>,
}

impl ValidationError {
    /// Create a new validation error
    pub fn new(field: impl Into<String>, message: impl Into<String>) -> Self {
        Self {
            field: field.into(),
            message: message.into(),
            severity: ErrorSeverity::Error,
            suggestion: None,
        }
    }

    /// Set severity to warning
    pub fn as_warning(mut self) -> Self {
        self.severity = ErrorSeverity::Warning;
        self
    }

    /// Add a suggestion
    pub fn with_suggestion(mut self, suggestion: impl Into<String>) -> Self {
        self.suggestion = Some(suggestion.into());
        self
    }
}

/// Error severity level
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ErrorSeverity {
    /// Informational - will not block publication
    Info,
    /// Warning - should be fixed but won't block
    Warning,
    /// Error - must be fixed before publication
    Error,
}

/// Validation result containing all errors and warnings
#[derive(Debug, Clone, Default)]
pub struct ValidationResult {
    /// List of validation errors
    pub errors: Vec<ValidationError>,
    /// Whether validation passed (no Error severity issues)
    pub is_valid: bool,
}

impl ValidationResult {
    /// Create a new empty result
    pub fn new() -> Self {
        Self {
            errors: Vec::new(),
            is_valid: true,
        }
    }

    /// Add an error to the result
    pub fn add_error(&mut self, error: ValidationError) {
        if error.severity == ErrorSeverity::Error {
            self.is_valid = false;
        }
        self.errors.push(error);
    }

    /// Get only errors (not warnings)
    pub fn errors_only(&self) -> Vec<&ValidationError> {
        self.errors
            .iter()
            .filter(|e| e.severity == ErrorSeverity::Error)
            .collect()
    }

    /// Get only warnings
    pub fn warnings_only(&self) -> Vec<&ValidationError> {
        self.errors
            .iter()
            .filter(|e| e.severity == ErrorSeverity::Warning)
            .collect()
    }

    /// Merge another result into this one
    pub fn merge(&mut self, other: ValidationResult) {
        for error in other.errors {
            self.add_error(error);
        }
    }
}

/// Manifest validator with configurable rules
#[derive(Debug, Clone)]
pub struct ManifestValidator {
    /// Whether FMEA documentation is required
    pub require_fmea: bool,
    /// Whether to validate dependencies exist
    pub validate_dependencies: bool,
    /// Maximum allowed keywords
    pub max_keywords: usize,
    /// Maximum allowed categories
    pub max_categories: usize,
    /// Reserved crate names that cannot be used
    pub reserved_names: HashSet<String>,
}

impl Default for ManifestValidator {
    fn default() -> Self {
        Self::new()
    }
}

impl ManifestValidator {
    /// Create a new validator with default settings
    pub fn new() -> Self {
        let reserved_names: HashSet<String> = [
            "std", "core", "alloc", "proc_macro", "test", "debug_assertions",
        ]
        .iter()
        .map(|s| (*s).to_string())
        .collect();

        Self {
            require_fmea: false,
            validate_dependencies: true,
            max_keywords: 5,
            max_categories: 5,
            reserved_names,
        }
    }

    /// Enable FMEA requirement
    pub fn with_fmea_required(mut self) -> Self {
        self.require_fmea = true;
        self
    }

    /// Disable dependency validation
    pub fn without_dependency_validation(mut self) -> Self {
        self.validate_dependencies = false;
        self
    }

    /// Validate a gpack manifest
    pub fn validate(&self, manifest: &GpackManifest) -> ValidationResult {
        let mut result = ValidationResult::new();

        // Validate package section
        self.validate_package_name(&manifest.package.name, &mut result);
        self.validate_version(&manifest.package.version, &mut result);
        self.validate_edition(&manifest.package.edition, &mut result);
        self.validate_required_fields(manifest, &mut result);
        self.validate_keywords(&manifest.package.keywords, &mut result);
        self.validate_categories(&manifest.package.categories, &mut result);
        self.validate_authors(&manifest.package.authors, &mut result);

        // Validate dependencies
        if self.validate_dependencies {
            self.validate_dependencies_section(&manifest.dependencies, "dependencies", &mut result);
            self.validate_dependencies_section(
                &manifest.dev_dependencies,
                "dev_dependencies",
                &mut result,
            );
            self.validate_dependencies_section(
                &manifest.build_dependencies,
                "build_dependencies",
                &mut result,
            );
        }

        // Validate FMEA requirement
        if self.require_fmea {
            self.validate_fmea(manifest, &mut result);
        }

        result
    }

    /// Validate crate name follows Rust/crates.io rules
    fn validate_package_name(&self, name: &str, result: &mut ValidationResult) {
        // Rule 1: Non-empty
        if name.is_empty() {
            result.add_error(
                ValidationError::new("package.name", "Package name cannot be empty")
                    .with_suggestion("Provide a valid crate name"),
            );
            return;
        }

        // Rule 2: Length limits (1-64 characters)
        if name.len() > 64 {
            result.add_error(
                ValidationError::new("package.name", "Package name exceeds 64 characters")
                    .with_suggestion("Use a shorter name"),
            );
        }

        // Rule 3: Must start with a letter
        let first_char = name.chars().next().unwrap_or('_');
        if !first_char.is_ascii_alphabetic() {
            result.add_error(
                ValidationError::new("package.name", "Package name must start with a letter")
                    .with_suggestion("Rename to start with a-z or A-Z"),
            );
        }

        // Rule 4: Only alphanumeric, underscore, hyphen allowed
        for (i, c) in name.chars().enumerate() {
            if !c.is_ascii_alphanumeric() && c != '_' && c != '-' {
                result.add_error(
                    ValidationError::new(
                        "package.name",
                        format!(
                            "Invalid character '{}' at position {}. Only a-z, 0-9, _, - allowed",
                            c, i
                        ),
                    )
                    .with_suggestion("Use only alphanumeric characters, underscores, or hyphens"),
                );
                break;
            }
        }

        // Rule 5: Cannot be a reserved name
        if self.reserved_names.contains(&name.to_lowercase()) {
            result.add_error(
                ValidationError::new("package.name", format!("'{}' is a reserved name", name))
                    .with_suggestion("Choose a different name"),
            );
        }

        // Rule 6: Cannot have consecutive hyphens or underscores
        if name.contains("--") || name.contains("__") {
            result.add_error(
                ValidationError::new(
                    "package.name",
                    "Package name cannot have consecutive hyphens or underscores",
                )
                .with_suggestion("Use single separator characters"),
            );
        }

        // Rule 7: Cannot end with hyphen or underscore
        if name.ends_with('-') || name.ends_with('_') {
            result.add_error(
                ValidationError::new(
                    "package.name",
                    "Package name cannot end with hyphen or underscore",
                )
                .with_suggestion("Remove trailing separator"),
            );
        }
    }

    /// Validate version follows SemVer
    fn validate_version(&self, version: &str, result: &mut ValidationResult) {
        if version.is_empty() {
            result.add_error(
                ValidationError::new("package.version", "Version cannot be empty")
                    .with_suggestion("Use semantic versioning: MAJOR.MINOR.PATCH"),
            );
            return;
        }

        // Parse SemVer
        let parts: Vec<&str> = version.split('.').collect();

        if parts.len() < 3 {
            result.add_error(
                ValidationError::new(
                    "package.version",
                    format!(
                        "Version '{}' is not valid SemVer (expected MAJOR.MINOR.PATCH)",
                        version
                    ),
                )
                .with_suggestion("Use format like 1.0.0"),
            );
            return;
        }

        // Validate each part is a number
        for (i, part) in parts.iter().take(3).enumerate() {
            // Handle pre-release/build metadata
            let numeric_part = part.split(|c| c == '-' || c == '+').next().unwrap_or("");

            if numeric_part.parse::<u32>().is_err() {
                let part_name = match i {
                    0 => "major",
                    1 => "minor",
                    _ => "patch",
                };
                result.add_error(
                    ValidationError::new(
                        "package.version",
                        format!("Invalid {} version number: '{}'", part_name, part),
                    )
                    .with_suggestion("Use numeric values only"),
                );
            }

            // Check for leading zeros (invalid in SemVer)
            if numeric_part.len() > 1 && numeric_part.starts_with('0') {
                result.add_error(
                    ValidationError::new(
                        "package.version",
                        format!("Version numbers cannot have leading zeros: '{}'", part),
                    )
                    .as_warning(),
                );
            }
        }
    }

    /// Validate Rust edition
    fn validate_edition(&self, edition: &str, result: &mut ValidationResult) {
        let valid_editions = ["2015", "2018", "2021", "2024"];
        if !valid_editions.contains(&edition) {
            result.add_error(
                ValidationError::new(
                    "package.edition",
                    format!(
                        "Invalid Rust edition '{}'. Valid options: {:?}",
                        edition, valid_editions
                    ),
                )
                .with_suggestion("Use 2021 for best compatibility"),
            );
        }
    }

    /// Validate required fields for crates.io
    fn validate_required_fields(&self, manifest: &GpackManifest, result: &mut ValidationResult) {
        // Description is recommended
        if manifest.package.description.is_none() {
            result.add_error(
                ValidationError::new(
                    "package.description",
                    "Package description is required for crates.io",
                )
                .with_suggestion("Add a short description of what the package does"),
            );
        }

        // License is required
        if manifest.package.license.is_none() && manifest.package.license_file.is_none() {
            result.add_error(
                ValidationError::new(
                    "package.license",
                    "Package license is required for crates.io",
                )
                .with_suggestion("Add license = \"MIT\" or another SPDX identifier"),
            );
        }
    }

    /// Validate keywords
    fn validate_keywords(&self, keywords: &[String], result: &mut ValidationResult) {
        if keywords.len() > self.max_keywords {
            result.add_error(
                ValidationError::new(
                    "package.keywords",
                    format!(
                        "Too many keywords ({} > {})",
                        keywords.len(),
                        self.max_keywords
                    ),
                )
                .with_suggestion(format!(
                    "Reduce to {} keywords or fewer",
                    self.max_keywords
                )),
            );
        }

        for keyword in keywords {
            // Keywords must be ASCII
            if !keyword.is_ascii() {
                result.add_error(
                    ValidationError::new(
                        "package.keywords",
                        format!("Keyword '{}' contains non-ASCII characters", keyword),
                    )
                    .with_suggestion("Use ASCII-only keywords"),
                );
            }

            // Keywords should not be too long
            if keyword.len() > 20 {
                result.add_error(
                    ValidationError::new(
                        "package.keywords",
                        format!("Keyword '{}' is too long (max 20 chars)", keyword),
                    )
                    .as_warning()
                    .with_suggestion("Use shorter, more specific keywords"),
                );
            }
        }
    }

    /// Validate categories
    fn validate_categories(&self, categories: &[String], result: &mut ValidationResult) {
        if categories.len() > self.max_categories {
            result.add_error(
                ValidationError::new(
                    "package.categories",
                    format!(
                        "Too many categories ({} > {})",
                        categories.len(),
                        self.max_categories
                    ),
                )
                .with_suggestion(format!(
                    "Reduce to {} categories or fewer",
                    self.max_categories
                )),
            );
        }

        // Known crates.io categories (partial list)
        let known_categories = [
            "accessibility",
            "api-bindings",
            "asynchronous",
            "authentication",
            "caching",
            "command-line-interface",
            "command-line-utilities",
            "compression",
            "concurrency",
            "config",
            "cryptography",
            "data-structures",
            "database",
            "database-implementations",
            "date-and-time",
            "development-tools",
            "email",
            "embedded",
            "encoding",
            "filesystem",
            "game-engines",
            "graphics",
            "hardware-support",
            "internationalization",
            "localization",
            "mathematics",
            "memory-management",
            "network-programming",
            "no-std",
            "os",
            "parser-implementations",
            "parsing",
            "rendering",
            "rust-patterns",
            "science",
            "simulation",
            "template-engine",
            "text-editors",
            "text-processing",
            "value-formatting",
            "visualization",
            "wasm",
            "web-programming",
        ];

        for category in categories {
            // Normalize for comparison
            let normalized = category.to_lowercase().replace('_', "-");
            if !known_categories.contains(&normalized.as_str()) {
                result.add_error(
                    ValidationError::new(
                        "package.categories",
                        format!("Unknown category '{}' may not be recognized by crates.io", category),
                    )
                    .as_warning()
                    .with_suggestion("See https://crates.io/category_slugs for valid categories"),
                );
            }
        }
    }

    /// Validate authors format
    fn validate_authors(&self, authors: &[String], result: &mut ValidationResult) {
        for author in authors {
            // Warn if author has no email
            if !author.contains('@') && !author.contains('<') {
                result.add_error(
                    ValidationError::new(
                        "package.authors",
                        format!(
                            "Author '{}' has no email address",
                            author
                        ),
                    )
                    .as_warning()
                    .with_suggestion("Use format: Name <email@example.com>"),
                );
            }

            // Validate email format if present
            if author.contains('<') && author.contains('>') {
                let email_start = author.find('<').unwrap_or(0);
                let email_end = author.find('>').unwrap_or(author.len());
                let email = &author[email_start + 1..email_end];

                if !email.contains('@') || email.len() < 5 {
                    result.add_error(
                        ValidationError::new(
                            "package.authors",
                            format!("Invalid email format in author: '{}'", author),
                        )
                        .with_suggestion("Use a valid email address"),
                    );
                }
            }
        }
    }

    /// Validate dependencies section
    fn validate_dependencies_section(
        &self,
        deps: &indexmap::IndexMap<String, DependencySpec>,
        section_name: &str,
        result: &mut ValidationResult,
    ) {
        for (name, spec) in deps {
            // Validate dependency name
            self.validate_dependency_name(name, section_name, result);

            // Validate version requirement
            match spec {
                DependencySpec::Version(v) => {
                    self.validate_version_requirement(v, name, section_name, result);
                }
                DependencySpec::Detailed(d) => {
                    if let Some(v) = &d.version {
                        self.validate_version_requirement(v, name, section_name, result);
                    }

                    // Git dependencies need version or rev/tag/branch
                    if d.git.is_some()
                        && d.version.is_none()
                        && d.rev.is_none()
                        && d.tag.is_none()
                        && d.branch.is_none()
                    {
                        result.add_error(
                            ValidationError::new(
                                section_name,
                                format!(
                                    "Git dependency '{}' should specify version, rev, tag, or branch",
                                    name
                                ),
                            )
                            .as_warning()
                            .with_suggestion("Add a version requirement or git reference"),
                        );
                    }
                }
            }
        }
    }

    /// Validate a dependency name
    fn validate_dependency_name(&self, name: &str, section: &str, result: &mut ValidationResult) {
        // Similar rules to package names but allow more flexibility
        if name.is_empty() {
            result.add_error(ValidationError::new(
                section,
                "Dependency name cannot be empty",
            ));
        }

        if !name.chars().next().unwrap_or('_').is_ascii_alphabetic() {
            result.add_error(
                ValidationError::new(section, format!("Dependency '{}' must start with a letter", name))
                    .as_warning(),
            );
        }
    }

    /// Validate version requirement string
    fn validate_version_requirement(
        &self, version: &str, dep_name: &str, section: &str, result: &mut ValidationResult,
    ) {
        // Empty version is invalid
        if version.is_empty() {
            result.add_error(ValidationError::new(
                section,
                format!("Dependency '{}' has empty version requirement", dep_name),
            ));
            return;
        }

        // Check for common version requirement operators
        let operators = ["^", "~", "=", ">", "<", ">=", "<=", "*"];
        let version_part = operators
            .iter()
            .fold(version.to_string(), |v, op| v.trim_start_matches(op).to_string());

        // Validate the version part (if not just "*")
        if version != "*" && !version_part.is_empty() {
            let parts: Vec<&str> = version_part.split('.').collect();
            if parts.is_empty() || parts[0].parse::<u32>().is_err() {
                result.add_error(
                    ValidationError::new(
                        section,
                        format!(
                            "Dependency '{}' has invalid version requirement: '{}'",
                            dep_name, version
                        ),
                    )
                    .with_suggestion("Use format like ^1.0, ~2.1, or 1.0.0"),
                );
            }
        }
    }

    /// Validate FMEA documentation requirement
    fn validate_fmea(&self, manifest: &GpackManifest, result: &mut ValidationResult) {
        if manifest.package.fmea_documented != Some(true) {
            result.add_error(
                ValidationError::new(
                    "package.fmea_documented",
                    "FMEA documentation is required for this registry",
                )
                .with_suggestion(
                    "Add fmea_documented: true after documenting failure modes in your README",
                ),
            );
        }
    }
}

/// Convert validation result to marketplace Error if invalid
impl From<ValidationResult> for Result<()> {
    fn from(result: ValidationResult) -> Self {
        if result.is_valid {
            Ok(())
        } else {
            let messages: Vec<String> = result
                .errors_only()
                .iter()
                .map(|e| format!("{}: {}", e.field, e.message))
                .collect();
            Err(Error::ValidationFailed {
                reason: messages.join("; "),
            })
        }
    }
}

// ==================== TESTS ====================

#[cfg(test)]
mod tests {
    use super::*;
    use crate::publish::manifest::{GpackManifest, GpackPackage};

    fn minimal_manifest() -> GpackManifest {
        GpackManifest {
            package: GpackPackage {
                name: "valid-crate".to_string(),
                version: "1.0.0".to_string(),
                edition: "2021".to_string(),
                authors: vec!["Test <test@example.com>".to_string()],
                description: Some("A test crate".to_string()),
                license: Some("MIT".to_string()),
                license_file: None,
                repository: None,
                homepage: None,
                documentation: None,
                readme: None,
                keywords: vec![],
                categories: vec![],
                exclude: vec![],
                include: vec![],
                publish: true,
                fmea_documented: None,
            },
            dependencies: indexmap::IndexMap::new(),
            dev_dependencies: indexmap::IndexMap::new(),
            build_dependencies: indexmap::IndexMap::new(),
            features: indexmap::IndexMap::new(),
            target: indexmap::IndexMap::new(),
            workspace: None,
        }
    }

    #[test]
    fn test_valid_manifest() {
        let validator = ManifestValidator::new();
        let manifest = minimal_manifest();
        let result = validator.validate(&manifest);
        assert!(result.is_valid);
        assert!(result.errors_only().is_empty());
    }

    #[test]
    fn test_invalid_crate_name_empty() {
        let validator = ManifestValidator::new();
        let mut manifest = minimal_manifest();
        manifest.package.name = "".to_string();
        let result = validator.validate(&manifest);
        assert!(!result.is_valid);
        assert!(result.errors.iter().any(|e| e.field == "package.name"));
    }

    #[test]
    fn test_invalid_crate_name_starts_with_number() {
        let validator = ManifestValidator::new();
        let mut manifest = minimal_manifest();
        manifest.package.name = "123abc".to_string();
        let result = validator.validate(&manifest);
        assert!(!result.is_valid);
        assert!(result
            .errors
            .iter()
            .any(|e| e.message.contains("start with a letter")));
    }

    #[test]
    fn test_invalid_crate_name_special_chars() {
        let validator = ManifestValidator::new();
        let mut manifest = minimal_manifest();
        manifest.package.name = "my@crate!".to_string();
        let result = validator.validate(&manifest);
        assert!(!result.is_valid);
        assert!(result
            .errors
            .iter()
            .any(|e| e.message.contains("Invalid character")));
    }

    #[test]
    fn test_reserved_name() {
        let validator = ManifestValidator::new();
        let mut manifest = minimal_manifest();
        manifest.package.name = "std".to_string();
        let result = validator.validate(&manifest);
        assert!(!result.is_valid);
        assert!(result
            .errors
            .iter()
            .any(|e| e.message.contains("reserved name")));
    }

    #[test]
    fn test_consecutive_separators() {
        let validator = ManifestValidator::new();
        let mut manifest = minimal_manifest();
        manifest.package.name = "my--crate".to_string();
        let result = validator.validate(&manifest);
        assert!(!result.is_valid);
        assert!(result
            .errors
            .iter()
            .any(|e| e.message.contains("consecutive")));
    }

    #[test]
    fn test_invalid_version_not_semver() {
        let validator = ManifestValidator::new();
        let mut manifest = minimal_manifest();
        manifest.package.version = "1.0".to_string();
        let result = validator.validate(&manifest);
        assert!(!result.is_valid);
        assert!(result
            .errors
            .iter()
            .any(|e| e.field == "package.version"));
    }

    #[test]
    fn test_invalid_version_text() {
        let validator = ManifestValidator::new();
        let mut manifest = minimal_manifest();
        manifest.package.version = "one.two.three".to_string();
        let result = validator.validate(&manifest);
        assert!(!result.is_valid);
    }

    #[test]
    fn test_valid_version_with_prerelease() {
        let validator = ManifestValidator::new();
        let mut manifest = minimal_manifest();
        manifest.package.version = "1.0.0-alpha.1".to_string();
        let result = validator.validate(&manifest);
        assert!(result.is_valid);
    }

    #[test]
    fn test_invalid_edition() {
        let validator = ManifestValidator::new();
        let mut manifest = minimal_manifest();
        manifest.package.edition = "2023".to_string();
        let result = validator.validate(&manifest);
        assert!(!result.is_valid);
        assert!(result
            .errors
            .iter()
            .any(|e| e.field == "package.edition"));
    }

    #[test]
    fn test_missing_description() {
        let validator = ManifestValidator::new();
        let mut manifest = minimal_manifest();
        manifest.package.description = None;
        let result = validator.validate(&manifest);
        assert!(!result.is_valid);
        assert!(result
            .errors
            .iter()
            .any(|e| e.field == "package.description"));
    }

    #[test]
    fn test_missing_license() {
        let validator = ManifestValidator::new();
        let mut manifest = minimal_manifest();
        manifest.package.license = None;
        let result = validator.validate(&manifest);
        assert!(!result.is_valid);
        assert!(result.errors.iter().any(|e| e.field == "package.license"));
    }

    #[test]
    fn test_license_file_alternative() {
        let validator = ManifestValidator::new();
        let mut manifest = minimal_manifest();
        manifest.package.license = None;
        manifest.package.license_file = Some("LICENSE".to_string());
        let result = validator.validate(&manifest);
        assert!(result.is_valid);
    }

    #[test]
    fn test_too_many_keywords() {
        let validator = ManifestValidator::new();
        let mut manifest = minimal_manifest();
        manifest.package.keywords = vec![
            "one".to_string(),
            "two".to_string(),
            "three".to_string(),
            "four".to_string(),
            "five".to_string(),
            "six".to_string(),
        ];
        let result = validator.validate(&manifest);
        assert!(!result.is_valid);
        assert!(result
            .errors
            .iter()
            .any(|e| e.message.contains("Too many keywords")));
    }

    #[test]
    fn test_too_many_categories() {
        let validator = ManifestValidator::new();
        let mut manifest = minimal_manifest();
        manifest.package.categories = vec![
            "one".to_string(),
            "two".to_string(),
            "three".to_string(),
            "four".to_string(),
            "five".to_string(),
            "six".to_string(),
        ];
        let result = validator.validate(&manifest);
        assert!(!result.is_valid);
    }

    #[test]
    fn test_fmea_required_when_enabled() {
        let validator = ManifestValidator::new().with_fmea_required();
        let manifest = minimal_manifest();
        let result = validator.validate(&manifest);
        assert!(!result.is_valid);
        assert!(result.errors.iter().any(|e| e.message.contains("FMEA")));
    }

    #[test]
    fn test_fmea_satisfied() {
        let validator = ManifestValidator::new().with_fmea_required();
        let mut manifest = minimal_manifest();
        manifest.package.fmea_documented = Some(true);
        let result = validator.validate(&manifest);
        assert!(result.is_valid);
    }

    #[test]
    fn test_dependencies_validation() {
        let validator = ManifestValidator::new();
        let mut manifest = minimal_manifest();
        manifest.dependencies.insert("serde".to_string(), DependencySpec::Version("1.0".to_string()));
        let result = validator.validate(&manifest);
        assert!(result.is_valid);
    }

    #[test]
    fn test_invalid_dependency_version() {
        let validator = ManifestValidator::new();
        let mut manifest = minimal_manifest();
        manifest.dependencies.insert("bad".to_string(), DependencySpec::Version("".to_string()));
        let result = validator.validate(&manifest);
        assert!(!result.is_valid);
        assert!(result
            .errors
            .iter()
            .any(|e| e.message.contains("empty version")));
    }

    #[test]
    fn test_author_format_warning() {
        let validator = ManifestValidator::new();
        let mut manifest = minimal_manifest();
        manifest.package.authors = vec!["Just A Name".to_string()];
        let result = validator.validate(&manifest);
        // Should have warning but still be valid
        assert!(result.is_valid);
        assert!(!result.warnings_only().is_empty());
    }

    #[test]
    fn test_validation_result_merge() {
        let mut result1 = ValidationResult::new();
        result1.add_error(ValidationError::new("field1", "error1"));

        let mut result2 = ValidationResult::new();
        result2.add_error(ValidationError::new("field2", "error2").as_warning());

        result1.merge(result2);
        assert_eq!(result1.errors.len(), 2);
        assert!(!result1.is_valid);
    }

    #[test]
    fn test_conversion_to_result() {
        let result = ValidationResult::new();
        let ok: Result<()> = result.into();
        assert!(ok.is_ok());

        let mut result = ValidationResult::new();
        result.add_error(ValidationError::new("test", "error"));
        let err: Result<()> = result.into();
        assert!(err.is_err());
    }
}
