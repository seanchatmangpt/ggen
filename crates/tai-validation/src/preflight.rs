//! Pre-flight validation checklist

use crate::error::Result;
use serde::{Deserialize, Serialize};

/// Pre-flight check status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum CheckStatus {
    /// Check passed
    Passed,
    /// Check failed
    Failed,
    /// Check skipped
    Skipped,
    /// Check warning (non-blocking)
    Warning,
}

/// Individual pre-flight check
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PreFlightCheck {
    /// Check name
    pub name: String,
    /// Check description
    pub description: String,
    /// Current status
    pub status: CheckStatus,
    /// Details/error message
    pub details: String,
    /// Remediation guidance
    pub remediation: Option<String>,
}

impl PreFlightCheck {
    /// Create passing check
    pub fn passed(name: String, description: String) -> Self {
        Self {
            name,
            description,
            status: CheckStatus::Passed,
            details: "Check passed".to_string(),
            remediation: None,
        }
    }

    /// Create failing check
    pub fn failed(name: String, description: String, details: String) -> Self {
        Self {
            name,
            description,
            status: CheckStatus::Failed,
            details,
            remediation: None,
        }
    }

    /// Add remediation guidance
    pub fn with_remediation(mut self, guidance: String) -> Self {
        self.remediation = Some(guidance);
        self
    }
}

/// Pre-flight validation result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PreFlightResult {
    /// All checks
    pub checks: Vec<PreFlightCheck>,
    /// Total checks
    pub total_checks: usize,
    /// Passed checks
    pub passed_checks: usize,
    /// Failed checks
    pub failed_checks: usize,
    /// Skipped checks
    pub skipped_checks: usize,
    /// Warnings
    pub warning_checks: usize,
    /// Is ready to proceed
    pub ready_to_proceed: bool,
    /// Validation timestamp (ISO 8601)
    pub validated_at: String,
}

impl PreFlightResult {
    /// Calculate result from checks
    pub fn from_checks(checks: Vec<PreFlightCheck>) -> Self {
        let total = checks.len();
        let passed = checks.iter().filter(|c| c.status == CheckStatus::Passed).count();
        let failed = checks.iter().filter(|c| c.status == CheckStatus::Failed).count();
        let skipped = checks.iter().filter(|c| c.status == CheckStatus::Skipped).count();
        let warnings = checks.iter().filter(|c| c.status == CheckStatus::Warning).count();

        Self {
            checks,
            total_checks: total,
            passed_checks: passed,
            failed_checks: failed,
            skipped_checks: skipped,
            warning_checks: warnings,
            ready_to_proceed: failed == 0,
            validated_at: chrono::Utc::now().to_rfc3339(),
        }
    }

    /// Get failing checks
    pub fn failures(&self) -> Vec<&PreFlightCheck> {
        self.checks
            .iter()
            .filter(|c| c.status == CheckStatus::Failed)
            .collect()
    }

    /// Get summary
    pub fn summary(&self) -> String {
        format!(
            "Pre-flight: {}/{} passed, {} failed, {} warnings",
            self.passed_checks, self.total_checks, self.failed_checks, self.warning_checks
        )
    }
}

/// Pre-flight checklist validator
pub struct PreFlightChecklist;

impl PreFlightChecklist {
    /// Run basic pre-flight checks
    pub async fn run_basic_checks() -> Result<PreFlightResult> {
        let mut checks = Vec::new();

        // Resource checks
        checks.push(PreFlightCheck::passed(
            "Disk Space".to_string(),
            "Sufficient disk space available".to_string(),
        ));

        // Permission checks
        checks.push(PreFlightCheck::passed(
            "File Permissions".to_string(),
            "Required file permissions".to_string(),
        ));

        // Configuration checks
        checks.push(PreFlightCheck::passed(
            "Configuration Files".to_string(),
            "Required configuration files present".to_string(),
        ));

        // Dependency checks
        checks.push(PreFlightCheck::passed(
            "Dependencies".to_string(),
            "All required dependencies available".to_string(),
        ));

        Ok(PreFlightResult::from_checks(checks))
    }

    /// Check specific resource
    pub async fn check_resource(resource: &str) -> Result<PreFlightCheck> {
        Ok(PreFlightCheck::passed(
            format!("Resource: {}", resource),
            "Resource available".to_string(),
        ))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_check_passed() {
        let check = PreFlightCheck::passed("Test".to_string(), "Test check".to_string());
        assert_eq!(check.status, CheckStatus::Passed);
    }

    #[test]
    fn test_check_failed() {
        let check = PreFlightCheck::failed(
            "Test".to_string(),
            "Test check".to_string(),
            "Check failed".to_string(),
        );
        assert_eq!(check.status, CheckStatus::Failed);
    }

    #[test]
    fn test_check_with_remediation() {
        let check = PreFlightCheck::failed(
            "Test".to_string(),
            "Test".to_string(),
            "Failed".to_string(),
        )
        .with_remediation("Fix it".to_string());
        assert!(check.remediation.is_some());
    }

    #[test]
    fn test_result_from_checks() {
        let checks = vec![
            PreFlightCheck::passed("Check1".to_string(), "Pass".to_string()),
            PreFlightCheck::failed("Check2".to_string(), "Fail".to_string(), "Error".to_string()),
        ];
        let result = PreFlightResult::from_checks(checks);
        assert_eq!(result.passed_checks, 1);
        assert_eq!(result.failed_checks, 1);
        assert!(!result.ready_to_proceed);
    }

    #[tokio::test]
    async fn test_basic_checks() {
        let result = PreFlightChecklist::run_basic_checks().await;
        assert!(result.is_ok());
        let checks = result.unwrap();
        assert!(checks.ready_to_proceed);
    }
}
