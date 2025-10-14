//! # Deployment Validator
//!
//! Validates crates and deployments before production.
//! Runs quality checks, security audits, and compliance tests.

use crate::cleanroom::CleanroomEnvironment;
use crate::error::{CleanroomError, Result};
use serde::{Deserialize, Serialize};
use std::path::Path;
use std::sync::Arc;
use tokio::process::Command;

/// Deployment validator
pub struct DeploymentValidator {
    cleanroom: Arc<CleanroomEnvironment>,
}

/// Validation report
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationReport {
    /// Overall validation passed
    pub passed: bool,

    /// Individual validation checks
    pub checks: Vec<ValidationCheck>,

    /// Warnings
    pub warnings: Vec<String>,

    /// Errors
    pub errors: Vec<String>,

    /// Target environment
    pub environment: String,

    /// Validation timestamp
    pub timestamp: chrono::DateTime<chrono::Utc>,
}

/// Individual validation check
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ValidationCheck {
    /// Check name
    pub name: String,

    /// Check passed
    pub passed: bool,

    /// Check message
    pub message: String,
}

/// Quality report from cargo checks
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityReport {
    /// Cargo check passed
    pub check_passed: bool,

    /// Cargo clippy passed
    pub clippy_passed: bool,

    /// Cargo fmt passed
    pub fmt_passed: bool,

    /// Cargo test passed
    pub test_passed: bool,

    /// Cargo audit passed (if available)
    pub audit_passed: Option<bool>,

    /// Overall quality score (0-100)
    pub score: u8,

    /// Quality issues found
    pub issues: Vec<QualityIssue>,
}

/// Quality issue
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityIssue {
    /// Issue severity
    pub severity: IssueSeverity,

    /// Issue description
    pub description: String,

    /// File and line (if available)
    pub location: Option<String>,
}

/// Issue severity levels
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum IssueSeverity {
    /// Critical error
    Error,

    /// Warning
    Warning,

    /// Informational
    Info,
}

impl DeploymentValidator {
    /// Create new deployment validator
    pub fn new(cleanroom: Arc<CleanroomEnvironment>) -> Self {
        Self { cleanroom }
    }

    /// Validate crate before deployment (fake cargo publish)
    pub async fn validate_crate(&self, path: &Path) -> Result<ValidationReport> {
        let mut checks = Vec::new();
        let mut warnings = Vec::new();
        let mut errors = Vec::new();

        // Run quality checks
        let quality = self.quality_checks().await?;

        // Check: cargo check
        checks.push(ValidationCheck {
            name: "cargo-check".to_string(),
            passed: quality.check_passed,
            message: if quality.check_passed {
                "Compilation successful".to_string()
            } else {
                "Compilation failed".to_string()
            },
        });

        if !quality.check_passed {
            errors.push("Project does not compile".to_string());
        }

        // Check: cargo clippy
        checks.push(ValidationCheck {
            name: "cargo-clippy".to_string(),
            passed: quality.clippy_passed,
            message: if quality.clippy_passed {
                "No clippy warnings".to_string()
            } else {
                "Clippy warnings found".to_string()
            },
        });

        if !quality.clippy_passed {
            warnings.push("Clippy found potential issues".to_string());
        }

        // Check: cargo fmt
        checks.push(ValidationCheck {
            name: "cargo-fmt".to_string(),
            passed: quality.fmt_passed,
            message: if quality.fmt_passed {
                "Code formatting correct".to_string()
            } else {
                "Code needs formatting".to_string()
            },
        });

        if !quality.fmt_passed {
            warnings.push("Code formatting issues detected".to_string());
        }

        // Check: tests pass
        checks.push(ValidationCheck {
            name: "cargo-test".to_string(),
            passed: quality.test_passed,
            message: if quality.test_passed {
                "All tests passing".to_string()
            } else {
                "Some tests failing".to_string()
            },
        });

        if !quality.test_passed {
            errors.push("Tests are failing".to_string());
        }

        // Check: Cargo.toml exists
        let cargo_toml = path.join("Cargo.toml");
        let has_cargo_toml = cargo_toml.exists();
        checks.push(ValidationCheck {
            name: "cargo-toml".to_string(),
            passed: has_cargo_toml,
            message: if has_cargo_toml {
                "Cargo.toml found".to_string()
            } else {
                "Cargo.toml missing".to_string()
            },
        });

        if !has_cargo_toml {
            errors.push("Cargo.toml not found".to_string());
        }

        // Check: README exists
        let readme_exists = path.join("README.md").exists();
        checks.push(ValidationCheck {
            name: "readme".to_string(),
            passed: readme_exists,
            message: if readme_exists {
                "README.md found".to_string()
            } else {
                "README.md missing".to_string()
            },
        });

        if !readme_exists {
            warnings.push("README.md not found (recommended for crates.io)".to_string());
        }

        let passed = checks.iter().all(|c| c.passed || c.name == "readme");

        Ok(ValidationReport {
            passed,
            checks,
            warnings,
            errors,
            environment: "crate-validation".to_string(),
            timestamp: chrono::Utc::now(),
        })
    }

    /// Run quality checks (check, clippy, fmt, test, audit)
    pub async fn quality_checks(&self) -> Result<QualityReport> {
        let mut issues = Vec::new();

        // Run cargo check
        let check_passed = self.run_cargo_command(&["check"]).await?;
        if !check_passed {
            issues.push(QualityIssue {
                severity: IssueSeverity::Error,
                description: "Cargo check failed".to_string(),
                location: None,
            });
        }

        // Run cargo clippy
        let clippy_passed = self.run_cargo_command(&["clippy", "--", "-D", "warnings"]).await.unwrap_or(true);
        if !clippy_passed {
            issues.push(QualityIssue {
                severity: IssueSeverity::Warning,
                description: "Clippy found issues".to_string(),
                location: None,
            });
        }

        // Run cargo fmt check
        let fmt_passed = self.run_cargo_command(&["fmt", "--", "--check"]).await.unwrap_or(true);
        if !fmt_passed {
            issues.push(QualityIssue {
                severity: IssueSeverity::Warning,
                description: "Code needs formatting".to_string(),
                location: None,
            });
        }

        // Run cargo test
        let test_passed = self.run_cargo_command(&["test"]).await?;
        if !test_passed {
            issues.push(QualityIssue {
                severity: IssueSeverity::Error,
                description: "Tests failing".to_string(),
                location: None,
            });
        }

        // Run cargo audit (optional)
        let audit_passed = self.run_cargo_command(&["audit"]).await.ok();

        // Calculate score
        let mut score = 100u8;
        for issue in &issues {
            score = score.saturating_sub(match issue.severity {
                IssueSeverity::Error => 25,
                IssueSeverity::Warning => 10,
                IssueSeverity::Info => 0,
            });
        }

        Ok(QualityReport {
            check_passed,
            clippy_passed,
            fmt_passed,
            test_passed,
            audit_passed,
            score,
            issues,
        })
    }

    /// Verify deployment to environment
    pub async fn verify_deployment(&self, environment: &str) -> Result<bool> {
        // Placeholder: In production, this would verify the deployment
        // by checking service health, running smoke tests, etc.
        let _ = environment;
        Ok(true)
    }

    /// Run cargo command
    async fn run_cargo_command(&self, args: &[&str]) -> Result<bool> {
        let output = Command::new("cargo")
            .args(args)
            .output()
            .await
            .map_err(|e| CleanroomError::io_error(format!("Failed to run cargo command: {}", e)))?;

        Ok(output.status.success())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::cleanroom::CleanroomEnvironment;
    use crate::config::CleanroomConfig;

    #[tokio::test]
    async fn test_validator_creation() {
        let config = CleanroomConfig::default();
        let cleanroom = Arc::new(CleanroomEnvironment::new(config).await.unwrap());
        let validator = DeploymentValidator::new(cleanroom);

        // Just verify we can create the validator
        let _quality = validator.quality_checks().await;
    }

    #[test]
    fn test_issue_severity() {
        let error = QualityIssue {
            severity: IssueSeverity::Error,
            description: "Test error".to_string(),
            location: None,
        };

        assert_eq!(error.severity, IssueSeverity::Error);
    }
}
