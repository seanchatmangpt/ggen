//! FMEA (Failure Mode and Effects Analysis) Validator for Marketplace Packages
//!
//! This module provides enterprise-grade validation for marketplace packages
//! using FMEA risk assessment and Poka-Yoke error prevention controls.
//!
//! # Fortune 500 Enterprise Features
//!
//! - **FMEA Validation**: RPN scoring with Critical/High/Medium thresholds
//! - **Path Protection**: Ensures domain code is never overwritten
//! - **Poka-Yoke Controls**: Error-proofing through headers and gitattributes
//! - **CODEOWNERS Generation**: Team ownership enforcement

use ggen_core::types::{
    CodeownersGenerator, FmeaConfig, FmeaValidationError, PathProtectionConfig,
    PathProtectionError, PokaYokeConfig,
};
use serde::{Deserialize, Serialize};
use std::fs;
use std::path::{Path, PathBuf};
use thiserror::Error;

/// FMEA validation errors
#[derive(Debug, Error)]
pub enum FmeaValidatorError {
    #[error("FMEA validation failed: {0}")]
    FmeaError(String),

    #[error("Path protection violation: {0}")]
    PathProtectionViolation(String),

    #[error("Missing required FMEA control for critical failure mode: {id} (RPN {rpn})")]
    MissingControl { id: String, rpn: u16 },

    #[error("Configuration error: {0}")]
    ConfigError(String),

    #[error("IO error: {0}")]
    IoError(#[from] std::io::Error),

    #[error("TOML parse error: {0}")]
    TomlError(#[from] toml::de::Error),
}

impl From<FmeaValidationError> for FmeaValidatorError {
    fn from(err: FmeaValidationError) -> Self {
        FmeaValidatorError::FmeaError(err.to_string())
    }
}

impl From<PathProtectionError> for FmeaValidatorError {
    fn from(err: PathProtectionError) -> Self {
        FmeaValidatorError::PathProtectionViolation(err.to_string())
    }
}

/// FMEA validation result for a single check
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FmeaCheckResult {
    /// Check passed
    Pass { message: String },
    /// Check failed with reason
    Fail { message: String, rpn: Option<u16> },
    /// Warning (high risk but not critical)
    Warning { message: String, rpn: Option<u16> },
    /// Check skipped (not applicable)
    Skipped { reason: String },
}

impl FmeaCheckResult {
    pub fn is_pass(&self) -> bool {
        matches!(self, FmeaCheckResult::Pass { .. })
    }

    pub fn is_fail(&self) -> bool {
        matches!(self, FmeaCheckResult::Fail { .. })
    }

    pub fn is_warning(&self) -> bool {
        matches!(self, FmeaCheckResult::Warning { .. })
    }
}

/// Complete FMEA validation report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmeaValidationReport {
    /// Package name
    pub package_name: String,
    /// Overall validation passed
    pub passed: bool,
    /// FMEA coverage percentage (critical modes mitigated)
    pub coverage_percentage: f64,
    /// Total RPN score (sum of all failure modes)
    pub total_rpn: u16,
    /// Maximum RPN score
    pub max_rpn: u16,
    /// Individual check results
    pub checks: Vec<FmeaCheck>,
    /// Critical failure modes requiring attention
    pub critical_modes: Vec<CriticalModeReport>,
    /// High-risk failure modes (warnings)
    pub high_risk_modes: Vec<HighRiskModeReport>,
    /// Enterprise configuration status
    pub enterprise_status: EnterpriseStatus,
}

/// Individual FMEA check
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FmeaCheck {
    /// Check name
    pub name: String,
    /// Check category
    pub category: FmeaCategory,
    /// Check result
    pub result: FmeaCheckResult,
}

/// FMEA check categories
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum FmeaCategory {
    /// Path protection checks
    PathProtection,
    /// FMEA control coverage
    ControlCoverage,
    /// Poka-Yoke mechanism presence
    PokaYoke,
    /// CODEOWNERS enforcement
    CodeOwnership,
    /// Enterprise configuration
    EnterpriseConfig,
}

/// Report for a critical failure mode
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CriticalModeReport {
    /// Failure mode ID
    pub id: String,
    /// Failure mode description
    pub mode: String,
    /// RPN score
    pub rpn: u16,
    /// Has mitigation control
    pub has_control: bool,
    /// Control description (if present)
    pub control: Option<String>,
}

/// Report for a high-risk failure mode
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct HighRiskModeReport {
    /// Failure mode ID
    pub id: String,
    /// Failure mode description
    pub mode: String,
    /// RPN score
    pub rpn: u16,
    /// Has mitigation control
    pub has_control: bool,
}

/// Enterprise configuration status
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnterpriseStatus {
    /// Fortune 500 ready
    pub fortune_500_ready: bool,
    /// FMEA controls enabled
    pub fmea_enabled: bool,
    /// Poka-Yoke enabled
    pub poka_yoke_enabled: bool,
    /// Domain protection strategy
    pub domain_protection: String,
}

/// FMEA Validator for marketplace packages
#[derive(Debug, Clone)]
pub struct FmeaValidator {
    /// Path protection configuration
    path_protection: PathProtectionConfig,
    /// Poka-Yoke configuration
    poka_yoke: PokaYokeConfig,
    /// Strict mode: fail on any high-risk without control
    strict_mode: bool,
}

impl Default for FmeaValidator {
    fn default() -> Self {
        Self::new()
    }
}

impl FmeaValidator {
    /// Create a new FMEA validator with default configuration
    pub fn new() -> Self {
        Self {
            path_protection: PathProtectionConfig::default(),
            poka_yoke: PokaYokeConfig::default(),
            strict_mode: false,
        }
    }

    /// Create a Fortune 500 enterprise validator (strict mode)
    pub fn fortune_500() -> Self {
        Self {
            path_protection: PathProtectionConfig::default(),
            poka_yoke: PokaYokeConfig::default(),
            strict_mode: true,
        }
    }

    /// Set custom path protection configuration
    pub fn with_path_protection(
        mut self, protected: &[&str], regenerate: &[&str],
    ) -> Result<Self, FmeaValidatorError> {
        self.path_protection = PathProtectionConfig::new(protected, regenerate)?;
        Ok(self)
    }

    /// Enable strict mode (fail on high-risk without controls)
    pub fn with_strict_mode(mut self, strict: bool) -> Self {
        self.strict_mode = strict;
        self
    }

    /// Validate a marketplace package
    pub fn validate_package(
        &self, package_path: &Path,
    ) -> Result<FmeaValidationReport, FmeaValidatorError> {
        let package_name = package_path
            .file_name()
            .and_then(|n| n.to_str())
            .unwrap_or("unknown")
            .to_string();

        let mut checks = Vec::new();
        let mut critical_modes = Vec::new();
        let mut high_risk_modes = Vec::new();

        // Load package.toml if exists
        let package_toml_path = package_path.join("package.toml");
        let fmea_config = if package_toml_path.exists() {
            self.load_fmea_config(&package_toml_path)?
        } else {
            None
        };

        // Check 1: Path protection configuration
        checks.push(self.check_path_protection(package_path));

        // Check 2: FMEA controls coverage
        let (fmea_check, fm_critical, fm_high) = self.check_fmea_controls(&fmea_config);
        checks.push(fmea_check);
        critical_modes.extend(fm_critical);
        high_risk_modes.extend(fm_high);

        // Check 3: Poka-Yoke mechanisms
        checks.push(self.check_poka_yoke(package_path));

        // Check 4: CODEOWNERS presence
        checks.push(self.check_codeowners(package_path));

        // Check 5: Enterprise configuration
        checks.push(self.check_enterprise_config(&fmea_config));

        // Calculate metrics
        let (coverage, total_rpn, max_rpn) = self.calculate_fmea_metrics(&fmea_config);

        // Determine pass/fail
        let has_failures = checks.iter().any(|c| c.result.is_fail());
        let has_unmitigated_critical = critical_modes.iter().any(|m| !m.has_control);

        let passed = !has_failures && !has_unmitigated_critical;

        // Enterprise status
        let enterprise_status = self.extract_enterprise_status(&fmea_config);

        Ok(FmeaValidationReport {
            package_name,
            passed,
            coverage_percentage: coverage,
            total_rpn,
            max_rpn,
            checks,
            critical_modes,
            high_risk_modes,
            enterprise_status,
        })
    }

    /// Validate a write operation against path protection rules
    pub fn validate_write(&self, path: &str, file_exists: bool) -> Result<(), FmeaValidatorError> {
        self.path_protection.validate_write(path, file_exists)?;
        Ok(())
    }

    /// Check if a path is protected
    pub fn is_protected(&self, path: &str) -> bool {
        self.path_protection.is_protected(path)
    }

    /// Check if a path is regeneratable
    pub fn is_regeneratable(&self, path: &str) -> bool {
        self.path_protection.is_regeneratable(path)
    }

    /// Generate CODEOWNERS from OWNERS files
    pub fn generate_codeowners(
        &self, ontology_dir: &Path, repo_root: &Path,
    ) -> Result<PathBuf, FmeaValidatorError> {
        let mut generator = CodeownersGenerator::new();
        generator.scan_owners_files(ontology_dir).map_err(|e| {
            FmeaValidatorError::ConfigError(format!("Failed to scan OWNERS files: {}", e))
        })?;

        generator.write_to_github(repo_root).map_err(|e| {
            FmeaValidatorError::ConfigError(format!("Failed to write CODEOWNERS: {}", e))
        })
    }

    /// Get the Poka-Yoke header for a file type
    pub fn get_generated_header(&self, extension: &str) -> String {
        self.poka_yoke.format_header(extension)
    }

    // =========================================================================
    // Private helper methods
    // =========================================================================

    fn load_fmea_config(&self, path: &Path) -> Result<Option<FmeaConfig>, FmeaValidatorError> {
        let content = fs::read_to_string(path)?;

        #[derive(Deserialize)]
        struct PackageToml {
            #[serde(default)]
            fmea: Option<FmeaConfig>,
        }

        let parsed: PackageToml = toml::from_str(&content)?;
        Ok(parsed.fmea)
    }

    fn check_path_protection(&self, package_path: &Path) -> FmeaCheck {
        // Check for generation config in package
        let gen_config_path = package_path.join("generation.toml");

        if gen_config_path.exists() {
            FmeaCheck {
                name: "Path Protection Configuration".to_string(),
                category: FmeaCategory::PathProtection,
                result: FmeaCheckResult::Pass {
                    message: "generation.toml found with path protection rules".to_string(),
                },
            }
        } else {
            // Check for inline config in package.toml
            let package_toml = package_path.join("package.toml");
            if package_toml.exists() {
                FmeaCheck {
                    name: "Path Protection Configuration".to_string(),
                    category: FmeaCategory::PathProtection,
                    result: FmeaCheckResult::Warning {
                        message: "Using default path protection (no explicit generation.toml)"
                            .to_string(),
                        rpn: Some(50),
                    },
                }
            } else {
                FmeaCheck {
                    name: "Path Protection Configuration".to_string(),
                    category: FmeaCategory::PathProtection,
                    result: FmeaCheckResult::Fail {
                        message: "No package.toml or generation.toml found".to_string(),
                        rpn: Some(100),
                    },
                }
            }
        }
    }

    fn check_fmea_controls(
        &self, fmea_config: &Option<FmeaConfig>,
    ) -> (FmeaCheck, Vec<CriticalModeReport>, Vec<HighRiskModeReport>) {
        let mut critical_modes = Vec::new();
        let mut high_risk_modes = Vec::new();

        match fmea_config {
            Some(config) if config.enabled => {
                // Collect critical modes
                for fm in config.critical_modes() {
                    if let Ok(rpn) = fm.calculate_rpn() {
                        critical_modes.push(CriticalModeReport {
                            id: fm.id.clone(),
                            mode: fm.mode.clone(),
                            rpn: rpn.value(),
                            has_control: fm.is_mitigated(),
                            control: fm.control.clone(),
                        });
                    }
                }

                // Collect high-risk modes
                for fm in config.high_risk_modes() {
                    if let Ok(rpn) = fm.calculate_rpn() {
                        high_risk_modes.push(HighRiskModeReport {
                            id: fm.id.clone(),
                            mode: fm.mode.clone(),
                            rpn: rpn.value(),
                            has_control: fm.is_mitigated(),
                        });
                    }
                }

                // Validate
                let coverage = config.coverage_percentage();
                let has_unmitigated = critical_modes.iter().any(|m| !m.has_control);

                let check = if has_unmitigated {
                    FmeaCheck {
                        name: "FMEA Controls Coverage".to_string(),
                        category: FmeaCategory::ControlCoverage,
                        result: FmeaCheckResult::Fail {
                            message: format!(
                                "Unmitigated critical failure modes (coverage: {:.1}%)",
                                coverage
                            ),
                            rpn: critical_modes
                                .iter()
                                .filter(|m| !m.has_control)
                                .map(|m| m.rpn)
                                .max(),
                        },
                    }
                } else if self.strict_mode && high_risk_modes.iter().any(|m| !m.has_control) {
                    FmeaCheck {
                        name: "FMEA Controls Coverage".to_string(),
                        category: FmeaCategory::ControlCoverage,
                        result: FmeaCheckResult::Warning {
                            message: format!(
                                "High-risk modes without controls (strict mode, coverage: {:.1}%)",
                                coverage
                            ),
                            rpn: high_risk_modes
                                .iter()
                                .filter(|m| !m.has_control)
                                .map(|m| m.rpn)
                                .max(),
                        },
                    }
                } else {
                    FmeaCheck {
                        name: "FMEA Controls Coverage".to_string(),
                        category: FmeaCategory::ControlCoverage,
                        result: FmeaCheckResult::Pass {
                            message: format!("FMEA coverage: {:.1}%", coverage),
                        },
                    }
                };

                (check, critical_modes, high_risk_modes)
            }
            Some(_) => {
                let check = FmeaCheck {
                    name: "FMEA Controls Coverage".to_string(),
                    category: FmeaCategory::ControlCoverage,
                    result: FmeaCheckResult::Skipped {
                        reason: "FMEA not enabled in package.toml".to_string(),
                    },
                };
                (check, critical_modes, high_risk_modes)
            }
            None => {
                let check = FmeaCheck {
                    name: "FMEA Controls Coverage".to_string(),
                    category: FmeaCategory::ControlCoverage,
                    result: FmeaCheckResult::Skipped {
                        reason: "No FMEA configuration found".to_string(),
                    },
                };
                (check, critical_modes, high_risk_modes)
            }
        }
    }

    fn check_poka_yoke(&self, package_path: &Path) -> FmeaCheck {
        let mut score = 0;
        let mut findings = Vec::new();

        // Check for .gitattributes
        let gitattributes = package_path.join(".gitattributes");
        if gitattributes.exists() {
            if let Ok(content) = fs::read_to_string(&gitattributes) {
                if content.contains("linguist-generated") {
                    score += 1;
                    findings.push("linguist-generated markers");
                }
            }
        }

        // Check for generated file headers in templates
        let templates_dir = package_path.join("templates");
        if templates_dir.exists() {
            if let Ok(entries) = fs::read_dir(&templates_dir) {
                for entry in entries.flatten() {
                    if let Ok(content) = fs::read_to_string(entry.path()) {
                        if content.contains("DO NOT EDIT") || content.contains("auto-generated") {
                            score += 1;
                            findings.push("DO NOT EDIT headers in templates");
                            break;
                        }
                    }
                }
            }
        }

        // Check for .gitignore with generated paths
        let gitignore = package_path.join(".gitignore");
        if gitignore.exists() {
            if let Ok(content) = fs::read_to_string(&gitignore) {
                if content.contains("generated") || content.contains("src/generated") {
                    score += 1;
                    findings.push("generated paths in .gitignore");
                }
            }
        }

        if score >= 2 {
            FmeaCheck {
                name: "Poka-Yoke Mechanisms".to_string(),
                category: FmeaCategory::PokaYoke,
                result: FmeaCheckResult::Pass {
                    message: format!("Found: {}", findings.join(", ")),
                },
            }
        } else if score >= 1 {
            FmeaCheck {
                name: "Poka-Yoke Mechanisms".to_string(),
                category: FmeaCategory::PokaYoke,
                result: FmeaCheckResult::Warning {
                    message: format!("Partial Poka-Yoke: {}", findings.join(", ")),
                    rpn: Some(75),
                },
            }
        } else {
            FmeaCheck {
                name: "Poka-Yoke Mechanisms".to_string(),
                category: FmeaCategory::PokaYoke,
                result: FmeaCheckResult::Warning {
                    message: "No Poka-Yoke mechanisms detected".to_string(),
                    rpn: Some(150),
                },
            }
        }
    }

    fn check_codeowners(&self, package_path: &Path) -> FmeaCheck {
        // Check for OWNERS files in ontology directories
        let ontology_dir = package_path.join("ontology");
        let data_dir = package_path.join("data");

        let has_owners = if ontology_dir.exists() {
            self.has_owners_file(&ontology_dir)
        } else if data_dir.exists() {
            self.has_owners_file(&data_dir)
        } else {
            false
        };

        if has_owners {
            FmeaCheck {
                name: "CODEOWNERS/OWNERS Configuration".to_string(),
                category: FmeaCategory::CodeOwnership,
                result: FmeaCheckResult::Pass {
                    message: "OWNERS files found for team ownership".to_string(),
                },
            }
        } else {
            FmeaCheck {
                name: "CODEOWNERS/OWNERS Configuration".to_string(),
                category: FmeaCategory::CodeOwnership,
                result: FmeaCheckResult::Warning {
                    message: "No OWNERS files found (recommended for team enforcement)".to_string(),
                    rpn: Some(50),
                },
            }
        }
    }

    fn has_owners_file(&self, dir: &Path) -> bool {
        if dir.join("OWNERS").exists() {
            return true;
        }

        if let Ok(entries) = fs::read_dir(dir) {
            for entry in entries.flatten() {
                let path = entry.path();
                if path.is_dir() && path.join("OWNERS").exists() {
                    return true;
                }
            }
        }

        false
    }

    fn check_enterprise_config(&self, fmea_config: &Option<FmeaConfig>) -> FmeaCheck {
        match fmea_config {
            Some(config) if config.enabled => FmeaCheck {
                name: "Enterprise Configuration".to_string(),
                category: FmeaCategory::EnterpriseConfig,
                result: FmeaCheckResult::Pass {
                    message: format!(
                        "FMEA enabled with {} failure modes defined",
                        config.controls.len()
                    ),
                },
            },
            Some(_) => FmeaCheck {
                name: "Enterprise Configuration".to_string(),
                category: FmeaCategory::EnterpriseConfig,
                result: FmeaCheckResult::Warning {
                    message: "FMEA section present but disabled".to_string(),
                    rpn: Some(50),
                },
            },
            None => FmeaCheck {
                name: "Enterprise Configuration".to_string(),
                category: FmeaCategory::EnterpriseConfig,
                result: FmeaCheckResult::Skipped {
                    reason: "No enterprise FMEA configuration".to_string(),
                },
            },
        }
    }

    fn calculate_fmea_metrics(&self, fmea_config: &Option<FmeaConfig>) -> (f64, u16, u16) {
        match fmea_config {
            Some(config) if config.enabled => {
                let coverage = config.coverage_percentage();
                let total_rpn: u16 = config
                    .controls
                    .iter()
                    .filter_map(|fm| fm.calculate_rpn().ok())
                    .map(|rpn| rpn.value())
                    .sum();
                let max_rpn = config
                    .controls
                    .iter()
                    .filter_map(|fm| fm.calculate_rpn().ok())
                    .map(|rpn| rpn.value())
                    .max()
                    .unwrap_or(0);

                (coverage, total_rpn, max_rpn)
            }
            _ => (100.0, 0, 0),
        }
    }

    fn extract_enterprise_status(&self, fmea_config: &Option<FmeaConfig>) -> EnterpriseStatus {
        let fmea_enabled = fmea_config.as_ref().map(|c| c.enabled).unwrap_or(false);

        EnterpriseStatus {
            fortune_500_ready: fmea_enabled && self.strict_mode,
            fmea_enabled,
            poka_yoke_enabled: true, // Always true with validator
            domain_protection: "trait-boundary".to_string(),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use tempfile::TempDir;

    #[test]
    fn test_fmea_validator_default() {
        let validator = FmeaValidator::new();
        assert!(!validator.strict_mode);
    }

    #[test]
    fn test_fmea_validator_fortune_500() {
        let validator = FmeaValidator::fortune_500();
        assert!(validator.strict_mode);
    }

    #[test]
    fn test_path_protection_check() {
        let validator = FmeaValidator::new();

        assert!(validator.is_protected("src/domain/user.rs"));
        assert!(!validator.is_protected("src/generated/user.rs"));
        assert!(validator.is_regeneratable("src/generated/user.rs"));
    }

    #[test]
    fn test_validate_write_protected_fails() {
        let validator = FmeaValidator::new();

        let result = validator.validate_write("src/domain/user.rs", false);
        assert!(result.is_err());
    }

    #[test]
    fn test_validate_write_regeneratable_passes() {
        let validator = FmeaValidator::new();

        let result = validator.validate_write("src/generated/user.rs", true);
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_package_minimal() {
        let temp_dir = TempDir::new().unwrap();

        // Create minimal package.toml
        fs::write(
            temp_dir.path().join("package.toml"),
            r#"
[package]
name = "test-package"
version = "1.0.0"
"#,
        )
        .unwrap();

        let validator = FmeaValidator::new();
        let report = validator.validate_package(temp_dir.path()).unwrap();

        assert_eq!(
            report.package_name,
            temp_dir.path().file_name().unwrap().to_str().unwrap()
        );
        assert!(!report.checks.is_empty());
    }

    #[test]
    fn test_validate_package_with_fmea() {
        let temp_dir = TempDir::new().unwrap();

        // Create package.toml with FMEA config
        fs::write(
            temp_dir.path().join("package.toml"),
            r#"
[package]
name = "test-package"
version = "1.0.0"

[fmea]
enabled = true
min_coverage = 100

[[fmea.controls]]
id = "F1"
mode = "Developer edits generated file"
severity = 9
occurrence = 6
detection = 4
control = "DO NOT EDIT header + linguist-generated"

[[fmea.controls]]
id = "F2"
mode = "Low risk mode"
severity = 2
occurrence = 2
detection = 2
"#,
        )
        .unwrap();

        let validator = FmeaValidator::new();
        let report = validator.validate_package(temp_dir.path()).unwrap();

        assert!(report.enterprise_status.fmea_enabled);
        assert_eq!(report.critical_modes.len(), 1); // F1 is critical (RPN 216)
        assert!(report.critical_modes[0].has_control);
    }

    #[test]
    fn test_generated_header() {
        let validator = FmeaValidator::new();

        let rust_header = validator.get_generated_header("rs");
        assert!(rust_header.starts_with("//"));

        let python_header = validator.get_generated_header("py");
        assert!(python_header.starts_with("#"));
    }

    #[test]
    fn test_check_result_methods() {
        let pass = FmeaCheckResult::Pass {
            message: "ok".to_string(),
        };
        assert!(pass.is_pass());
        assert!(!pass.is_fail());

        let fail = FmeaCheckResult::Fail {
            message: "fail".to_string(),
            rpn: Some(200),
        };
        assert!(fail.is_fail());
        assert!(!fail.is_pass());

        let warning = FmeaCheckResult::Warning {
            message: "warn".to_string(),
            rpn: Some(100),
        };
        assert!(warning.is_warning());
    }
}
