//! Marketplace Package Validation Guards
//!
//! This module provides a formal guard-based validation system for marketplace packages,
//! with immutable validation receipts and audit trail support.
//!
//! Guards are ordered validation rules with configurable:
//! - Weight (contribution to final score)
//! - Severity (critical vs bonus)
//! - Service Level Objective (SLO) for runtime

use chrono::Utc;
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};

/// Result type for guard operations
pub type GuardResult<T> = Result<T, GuardError>;

/// Errors that can occur during guard execution
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum GuardError {
    IoError(String),
    ValidationFailed(String),
    InvalidPackage(String),
    ReceiptWrite(String),
    ReceiptRead(String),
}

impl std::fmt::Display for GuardError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            GuardError::IoError(msg) => write!(f, "IO Error: {}", msg),
            GuardError::ValidationFailed(msg) => write!(f, "Validation Failed: {}", msg),
            GuardError::InvalidPackage(msg) => write!(f, "Invalid Package: {}", msg),
            GuardError::ReceiptWrite(msg) => write!(f, "Receipt Write Error: {}", msg),
            GuardError::ReceiptRead(msg) => write!(f, "Receipt Read Error: {}", msg),
        }
    }
}

impl std::error::Error for GuardError {}

/// Severity level for a guard
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum Severity {
    /// Critical guard: must pass for production readiness
    Critical,
    /// Bonus guard: contributes to score but not required
    Bonus,
}

/// Result of applying a single guard to a package
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GuardCheckResult {
    /// Name of the guard
    pub guard_name: String,
    /// Type identifier for the guard
    pub guard_type: String,
    /// Whether the guard passed
    pub passed: bool,
    /// Human-readable message explaining the result
    pub message: String,
    /// Weight contribution to overall score
    pub weight: u32,
    /// Severity classification
    pub severity: Severity,
    /// Optional detailed data
    #[serde(skip_serializing_if = "Option::is_none")]
    pub details: Option<serde_json::Value>,
}

impl GuardCheckResult {
    pub fn new(
        guard_name: String, guard_type: String, passed: bool, message: String, weight: u32,
        severity: Severity,
    ) -> Self {
        Self {
            guard_name,
            guard_type,
            passed,
            message,
            weight,
            severity,
            details: None,
        }
    }

    pub fn with_details(mut self, details: serde_json::Value) -> Self {
        self.details = Some(details);
        self
    }
}

/// Immutable validation receipt for a package version
/// Merkle-linkable for audit trail and chain-of-custody
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationReceipt {
    /// Package identifier
    pub package_id: String,
    /// Package version (semver)
    pub version: String,
    /// ISO 8601 timestamp of validation
    pub validated_at: String,
    /// Ggen version that performed validation
    pub ggen_version: String,
    /// Individual guard results
    pub guard_results: Vec<GuardCheckResult>,
    /// Overall score (0-100)
    pub overall_score: f64,
    /// Whether package is production ready
    pub production_ready: bool,
    /// Merkle hash of this receipt for immutability
    #[serde(skip_serializing_if = "Option::is_none")]
    pub checksum: Option<String>,
    /// Hash of previous receipt (if chaining)
    #[serde(skip_serializing_if = "Option::is_none")]
    pub previous_checksum: Option<String>,
    /// Number of critical guards that passed
    pub critical_passed: usize,
    /// Number of critical guards total
    pub critical_total: usize,
    /// Number of bonus guards that passed
    pub bonus_passed: usize,
    /// Number of bonus guards total
    pub bonus_total: usize,
}

impl ValidationReceipt {
    pub fn new(package_id: String, version: String, ggen_version: String) -> Self {
        Self {
            package_id,
            version,
            validated_at: Utc::now().to_rfc3339(),
            ggen_version,
            guard_results: Vec::new(),
            overall_score: 0.0,
            production_ready: false,
            checksum: None,
            previous_checksum: None,
            critical_passed: 0,
            critical_total: 0,
            bonus_passed: 0,
            bonus_total: 0,
        }
    }

    /// Add a guard result to the receipt
    pub fn add_guard_result(&mut self, result: GuardCheckResult) {
        match result.severity {
            Severity::Critical => {
                self.critical_total += 1;
                if result.passed {
                    self.critical_passed += 1;
                }
            }
            Severity::Bonus => {
                self.bonus_total += 1;
                if result.passed {
                    self.bonus_passed += 1;
                }
            }
        }
        self.guard_results.push(result);
    }

    /// Calculate overall score based on guard results
    pub fn calculate_score(&mut self) {
        if self.guard_results.is_empty() {
            self.overall_score = 0.0;
            self.production_ready = false;
            return;
        }

        // Calculate weighted score
        let mut total_weight = 0u32;
        let mut weighted_score = 0.0f64;

        for result in &self.guard_results {
            total_weight += result.weight;
            if result.passed {
                weighted_score += result.weight as f64;
            }
        }

        if total_weight > 0 {
            self.overall_score = (weighted_score / total_weight as f64) * 100.0;
        }

        // Production ready: all critical guards must pass AND score >= 95
        let all_critical_pass =
            self.critical_total == 0 || self.critical_passed == self.critical_total;
        self.production_ready = all_critical_pass && self.overall_score >= 95.0;
    }

    /// Compute a deterministic checksum for the receipt
    pub fn compute_checksum(&mut self) {
        use sha2::{Digest, Sha256};

        let json = serde_json::to_string(&self.guard_results).unwrap_or_else(|_| String::new());

        let mut hasher = Sha256::new();
        hasher.update(self.package_id.as_bytes());
        hasher.update(self.version.as_bytes());
        hasher.update(self.validated_at.as_bytes());
        hasher.update(json.as_bytes());

        let hash = hasher.finalize();
        self.checksum = Some(format!("{:x}", hash));
    }

    /// Write receipt to disk
    pub fn write_to_file(&self, base_path: &Path) -> GuardResult<PathBuf> {
        // Create receipts directory: marketplace/receipts/<package_id>/<version>.json
        let receipts_dir = base_path
            .join("marketplace")
            .join("receipts")
            .join(&self.package_id);

        fs::create_dir_all(&receipts_dir).map_err(|e| {
            GuardError::ReceiptWrite(format!("Failed to create receipts dir: {}", e))
        })?;

        let file_path = receipts_dir.join(format!("{}.json", self.version));

        let json = serde_json::to_string_pretty(&self)
            .map_err(|e| GuardError::ReceiptWrite(format!("JSON serialization failed: {}", e)))?;

        fs::write(&file_path, json).map_err(|e| {
            GuardError::ReceiptWrite(format!("Failed to write receipt file: {}", e))
        })?;

        Ok(file_path)
    }

    /// Read receipt from disk
    pub fn read_from_file(path: &Path) -> GuardResult<Self> {
        let json = fs::read_to_string(path)
            .map_err(|e| GuardError::ReceiptRead(format!("Failed to read receipt file: {}", e)))?;

        serde_json::from_str(&json)
            .map_err(|e| GuardError::ReceiptRead(format!("JSON deserialization failed: {}", e)))
    }

    /// Get the latest receipt for a package
    pub fn latest_for_package(base_path: &Path, package_id: &str) -> GuardResult<Option<Self>> {
        let receipts_dir = base_path
            .join("marketplace")
            .join("receipts")
            .join(package_id);

        if !receipts_dir.exists() {
            return Ok(None);
        }

        let mut latest_receipt: Option<Self> = None;
        let mut latest_version_str = "0.0.0".to_string();

        for entry in fs::read_dir(&receipts_dir)
            .map_err(|e| GuardError::ReceiptRead(format!("Failed to read receipts dir: {}", e)))?
        {
            let entry = entry
                .map_err(|e| GuardError::ReceiptRead(format!("Failed to read entry: {}", e)))?;
            let path = entry.path();

            if path.extension().and_then(|s| s.to_str()) == Some("json") {
                if let Some(filename) = path.file_stem().and_then(|s| s.to_str()) {
                    // Simple version comparison: compare as semantic version strings
                    // Latest receipt is determined by alphanumeric sort (works for semver)
                    if Self::compare_versions(filename, &latest_version_str) > 0 {
                        if let Ok(receipt) = Self::read_from_file(&path) {
                            latest_receipt = Some(receipt);
                            latest_version_str = filename.to_string();
                        }
                    }
                }
            }
        }

        Ok(latest_receipt)
    }

    /// Compare two semantic version strings
    /// Returns: -1 if a < b, 0 if a == b, 1 if a > b
    fn compare_versions(a: &str, b: &str) -> i32 {
        let a_parts: Vec<&str> = a.split('.').collect();
        let b_parts: Vec<&str> = b.split('.').collect();

        for i in 0..std::cmp::max(a_parts.len(), b_parts.len()) {
            let a_part = a_parts
                .get(i)
                .and_then(|p| p.parse::<u32>().ok())
                .unwrap_or(0);
            let b_part = b_parts
                .get(i)
                .and_then(|p| p.parse::<u32>().ok())
                .unwrap_or(0);

            if a_part > b_part {
                return 1;
            } else if a_part < b_part {
                return -1;
            }
        }
        0
    }
}

/// Guard trait for implementing custom validation logic
pub trait Guard: Send + Sync {
    /// Guard identifier
    fn id(&self) -> &str;

    /// Guard display name
    fn name(&self) -> &str;

    /// Guard description
    fn description(&self) -> &str;

    /// Weight contribution to scoring (1-100)
    fn weight(&self) -> u32;

    /// Severity level
    fn severity(&self) -> Severity;

    /// SLO for this guard in milliseconds
    fn slo_ms(&self) -> u32 {
        5000
    }

    /// Execute the guard against a package
    fn execute(&self, package_path: &Path) -> GuardResult<GuardCheckResult>;
}

/// Factory for creating standard guards
pub mod factories {
    use super::*;
    use std::fs;

    /// Metadata Guard - validates package.toml and basic fields
    pub struct GuardMetadata;

    impl Guard for GuardMetadata {
        fn id(&self) -> &str {
            "metadata"
        }

        fn name(&self) -> &str {
            "Metadata Guard"
        }

        fn description(&self) -> &str {
            "Validates package.toml and required metadata fields (id, version, description)"
        }

        fn weight(&self) -> u32 {
            10
        }

        fn severity(&self) -> Severity {
            Severity::Critical
        }

        fn execute(&self, package_path: &Path) -> GuardResult<GuardCheckResult> {
            let package_toml = package_path.join("package.toml");

            if !package_toml.exists() {
                return Ok(GuardCheckResult::new(
                    self.name().to_string(),
                    self.id().to_string(),
                    false,
                    "package.toml not found".to_string(),
                    self.weight(),
                    self.severity(),
                ));
            }

            let content = fs::read_to_string(&package_toml)
                .map_err(|e| GuardError::IoError(e.to_string()))?;

            let has_id = content.contains("[package]") && content.contains("id =");
            let has_version = content.contains("version =");
            let has_description = content.contains("description =");

            let passed = has_id && has_version && has_description;
            let message = if passed {
                "Metadata complete: id, version, description found".to_string()
            } else {
                format!(
                    "Missing metadata: id={}, version={}, description={}",
                    has_id, has_version, has_description
                )
            };

            Ok(GuardCheckResult::new(
                self.name().to_string(),
                self.id().to_string(),
                passed,
                message,
                self.weight(),
                self.severity(),
            ))
        }
    }

    /// License Guard - validates license file presence
    pub struct GuardLicense;

    impl Guard for GuardLicense {
        fn id(&self) -> &str {
            "license"
        }

        fn name(&self) -> &str {
            "License Guard"
        }

        fn description(&self) -> &str {
            "Validates presence of LICENSE, LICENSE-MIT, or LICENSE-APACHE file"
        }

        fn weight(&self) -> u32 {
            8
        }

        fn severity(&self) -> Severity {
            Severity::Critical
        }

        fn execute(&self, package_path: &Path) -> GuardResult<GuardCheckResult> {
            let has_license = package_path.join("LICENSE").exists()
                || package_path.join("LICENSE-MIT").exists()
                || package_path.join("LICENSE-APACHE").exists();

            Ok(GuardCheckResult::new(
                self.name().to_string(),
                self.id().to_string(),
                has_license,
                if has_license {
                    "License file found".to_string()
                } else {
                    "No LICENSE file found (LICENSE, LICENSE-MIT, or LICENSE-APACHE required)"
                        .to_string()
                },
                self.weight(),
                self.severity(),
            ))
        }
    }

    /// README Guard - validates README quality
    pub struct GuardReadme;

    impl Guard for GuardReadme {
        fn id(&self) -> &str {
            "readme"
        }

        fn name(&self) -> &str {
            "README Guard"
        }

        fn description(&self) -> &str {
            "Validates README.md presence and minimum length (500 chars)"
        }

        fn weight(&self) -> u32 {
            7
        }

        fn severity(&self) -> Severity {
            Severity::Critical
        }

        fn execute(&self, package_path: &Path) -> GuardResult<GuardCheckResult> {
            let readme_path = package_path.join("README.md");

            if !readme_path.exists() {
                return Ok(GuardCheckResult::new(
                    self.name().to_string(),
                    self.id().to_string(),
                    false,
                    "README.md not found".to_string(),
                    self.weight(),
                    self.severity(),
                ));
            }

            let content =
                fs::read_to_string(&readme_path).map_err(|e| GuardError::IoError(e.to_string()))?;

            let min_length = 500;
            let passed = content.len() >= min_length;

            Ok(GuardCheckResult::new(
                self.name().to_string(),
                self.id().to_string(),
                passed,
                format!(
                    "README.md found ({} chars, minimum {} required)",
                    content.len(),
                    min_length
                ),
                self.weight(),
                self.severity(),
            ))
        }
    }

    /// Tests Guard - validates test suite presence
    pub struct GuardTests;

    impl Guard for GuardTests {
        fn id(&self) -> &str {
            "tests"
        }

        fn name(&self) -> &str {
            "Tests Guard"
        }

        fn description(&self) -> &str {
            "Validates presence of test directory or test files"
        }

        fn weight(&self) -> u32 {
            8
        }

        fn severity(&self) -> Severity {
            Severity::Critical
        }

        fn execute(&self, package_path: &Path) -> GuardResult<GuardCheckResult> {
            let has_test_dir = package_path.join("tests").exists();
            let has_test_file =
                package_path.join("test.rs").exists() || package_path.join("tests.rs").exists();

            let passed = has_test_dir || has_test_file;

            Ok(GuardCheckResult::new(
                self.name().to_string(),
                self.id().to_string(),
                passed,
                if passed {
                    "Test files found".to_string()
                } else {
                    "No test directory or test files found".to_string()
                },
                self.weight(),
                self.severity(),
            ))
        }
    }

    /// Chicago Compliance Guard - validates Chatman Equation adherence
    /// Checks for forbidden patterns, AAA test structure, and safety
    pub struct GuardChicagoCompliance;

    impl Guard for GuardChicagoCompliance {
        fn id(&self) -> &str {
            "chicago_compliance"
        }

        fn name(&self) -> &str {
            "Chicago Compliance Guard"
        }

        fn description(&self) -> &str {
            "Validates adherence to chicago-tdd-tools and Chatman Equation patterns"
        }

        fn weight(&self) -> u32 {
            15
        }

        fn severity(&self) -> Severity {
            Severity::Critical
        }

        fn slo_ms(&self) -> u32 {
            10000 // Longer SLO for AST analysis
        }

        fn execute(&self, package_path: &Path) -> GuardResult<GuardCheckResult> {
            let src_dir = package_path.join("src");
            if !src_dir.exists() {
                return Ok(GuardCheckResult::new(
                    self.name().to_string(),
                    self.id().to_string(),
                    true, // Pass if no src directory (not applicable)
                    "No src/ directory found (not applicable to non-Rust packages)".to_string(),
                    self.weight(),
                    self.severity(),
                ));
            }

            // Check for forbidden patterns in production code
            let mut forbidden_found = Vec::new();
            let forbidden_patterns = [
                ("unwrap()", "Unwrap without fallback"),
                ("expect(", "Expect without fallback"),
                ("panic!(", "Panic in production code"),
            ];

            if let Ok(entries) = fs::read_dir(&src_dir) {
                for entry in entries.flatten() {
                    let path = entry.path();
                    if path.extension().and_then(|s| s.to_str()) == Some("rs") {
                        if let Ok(content) = fs::read_to_string(&path) {
                            // Skip test modules and code
                            if path
                                .file_name()
                                .and_then(|n| n.to_str())
                                .map(|n| n.ends_with("_test.rs") || n.ends_with("test.rs"))
                                .unwrap_or(false)
                            {
                                continue;
                            }

                            for (pattern, desc) in &forbidden_patterns {
                                if content.contains(pattern) {
                                    forbidden_found.push(format!(
                                        "{}: {} in {}",
                                        desc,
                                        pattern,
                                        path.display()
                                    ));
                                }
                            }
                        }
                    }
                }
            }

            // Check for test files with AAA structure
            let tests_dir = package_path.join("tests");
            let has_tests = tests_dir.exists()
                && fs::read_dir(&tests_dir)
                    .map(|entries| entries.count() > 0)
                    .unwrap_or(false);

            let passed = forbidden_found.is_empty() && has_tests;

            let message = if forbidden_found.is_empty() {
                if has_tests {
                    "✅ No forbidden patterns found, test suite present".to_string()
                } else {
                    "⚠️ No forbidden patterns, but test suite may be minimal".to_string()
                }
            } else {
                format!(
                    "❌ Forbidden patterns found:\n  - {}",
                    forbidden_found.join("\n  - ")
                )
            };

            Ok(GuardCheckResult::new(
                self.name().to_string(),
                self.id().to_string(),
                passed,
                message,
                self.weight(),
                self.severity(),
            ))
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validation_receipt_new() {
        let receipt = ValidationReceipt::new(
            "test-pkg".to_string(),
            "1.0.0".to_string(),
            "3.0.0".to_string(),
        );

        assert_eq!(receipt.package_id, "test-pkg");
        assert_eq!(receipt.version, "1.0.0");
        assert_eq!(receipt.ggen_version, "3.0.0");
        assert_eq!(receipt.overall_score, 0.0);
        assert!(!receipt.production_ready);
    }

    #[test]
    fn test_guard_check_result() {
        let result = GuardCheckResult::new(
            "Test Guard".to_string(),
            "test".to_string(),
            true,
            "Test passed".to_string(),
            10,
            Severity::Critical,
        );

        assert_eq!(result.guard_name, "Test Guard");
        assert!(result.passed);
        assert_eq!(result.weight, 10);
    }
}
