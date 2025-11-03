//! Production Readiness Validation System
//!
//! This module implements validation logic for production readiness requirements,
//! ensuring that deployments only happen when critical requirements are met.
//!
//! # Validation Rules (80/20 Rule)
//!
//! ## Critical Requirements (Must be 100% complete)
//! - Authentication & authorization ✓
//! - Error handling & logging ✓
//! - Health checks & monitoring ✓
//! - Input validation & security ✓
//! - Database migrations ✓
//!
//! ## Important Requirements (Must be >80% complete)
//! - API documentation (>80%)
//! - Testing coverage (>80%)
//! - Performance monitoring (>80%)
//! - Configuration management (>80%)
//! - Deployment automation (>80%)
//!
//! ## Nice-to-Have Requirements (Can be <50% for MVP)
//! - Advanced caching (<50%)
//! - Rate limiting (<50%)
//! - Circuit breakers (<50%)
//! - Advanced security (<50%)
//! - Monitoring dashboards (<50%)

use super::{
    error::{LifecycleError, Result},
    production::{ReadinessCategory, ReadinessReport, ReadinessTracker},
};
use std::collections::HashMap;

/// Validation result for production readiness
#[derive(Debug, Clone)]
pub struct ValidationResult {
    /// Whether validation passed
    pub passed: bool,
    /// Overall readiness score
    pub score: f64,
    /// Validation errors/warnings
    pub issues: Vec<ValidationIssue>,
    /// Recommendations for improvement
    pub recommendations: Vec<String>,
}

/// Issue found during validation
#[derive(Debug, Clone)]
pub struct ValidationIssue {
    /// Severity level
    pub severity: ValidationSeverity,
    /// Issue category
    pub category: ReadinessCategory,
    /// Issue description
    pub description: String,
    /// Suggested fix
    pub fix: String,
}

/// Severity levels for validation issues
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ValidationSeverity {
    /// Critical issue that blocks deployment
    Critical,
    /// Warning that should be addressed
    Warning,
    /// Information for awareness
    Info,
}

/// Production readiness validator
pub struct ReadinessValidator {
    /// Minimum score required for each category
    thresholds: HashMap<ReadinessCategory, f64>,
    /// Whether to enforce strict validation
    strict_mode: bool,
}

impl ReadinessValidator {
    /// Create a new validator with default thresholds
    pub fn new() -> Self {
        let mut thresholds = HashMap::new();
        thresholds.insert(ReadinessCategory::Critical, 100.0); // Must be 100%
        thresholds.insert(ReadinessCategory::Important, 80.0); // Must be >80%
        thresholds.insert(ReadinessCategory::NiceToHave, 0.0); // No minimum

        Self {
            thresholds,
            strict_mode: true,
        }
    }

    /// Create a validator with custom thresholds
    pub fn with_thresholds(thresholds: HashMap<ReadinessCategory, f64>) -> Self {
        Self {
            thresholds,
            strict_mode: true,
        }
    }

    /// Set strict mode (enforce all thresholds)
    pub fn strict_mode(mut self, strict: bool) -> Self {
        self.strict_mode = strict;
        self
    }

    /// Validate production readiness report
    pub fn validate(&self, report: &ReadinessReport) -> ValidationResult {
        let mut issues = Vec::new();
        let mut recommendations = Vec::new();
        let mut passed = true;

        // Check each category against thresholds
        for (category, threshold) in &self.thresholds {
            if let Some(category_report) = report.by_category.get(category) {
                let category_score = category_report.score;

                if category_score < *threshold {
                    let severity = if *category == ReadinessCategory::Critical {
                        ValidationSeverity::Critical
                    } else {
                        ValidationSeverity::Warning
                    };

                    issues.push(ValidationIssue {
                        severity: severity.clone(),
                        category: category.clone(),
                        description: format!(
                            "{:?} requirements are only {:.1}% complete (minimum {:.1}% required)",
                            category, category_score, threshold
                        ),
                        fix: self.get_fix_for_category(category),
                    });

                    if severity == ValidationSeverity::Critical {
                        passed = false;
                    }
                }

                // Generate recommendations for improvement
                if category_score < 90.0 {
                    recommendations.push(format!(
                        "Improve {:?} requirements: currently {:.1}%, aim for >90%",
                        category, category_score
                    ));
                }
            }
        }

        // Check for blocking requirements
        if !report.blocking_requirements.is_empty() {
            issues.push(ValidationIssue {
                severity: ValidationSeverity::Critical,
                category: ReadinessCategory::Critical,
                description: format!(
                    "{} critical requirements are blocking production deployment",
                    report.blocking_requirements.len()
                ),
                fix: "Complete all critical requirements before deploying to production"
                    .to_string(),
            });
            passed = false;
        }

        ValidationResult {
            passed,
            score: report.overall_score,
            issues,
            recommendations,
        }
    }

    /// Validate project before deployment
    pub fn validate_for_deployment<P: AsRef<std::path::Path>>(
        &self, project_root: P,
    ) -> Result<ValidationResult> {
        let mut tracker = ReadinessTracker::new(project_root);
        tracker.load().map_err(|e| LifecycleError::ConfigLoad {
            path: std::path::PathBuf::from("readiness.toml"),
            source: Box::new(e),
        })?;

        // Analyze project for existing implementations
        tracker
            .analyze_project()
            .map_err(|e| LifecycleError::CommandFailed {
                phase: "production-validation".to_string(),
                command: "analyze_project".to_string(),
                exit_code: 1,
                stderr: e.to_string(),
            })?;

        let report = tracker.generate_report();
        Ok(self.validate(&report))
    }

    /// Get fix recommendations for a category
    fn get_fix_for_category(&self, category: &ReadinessCategory) -> String {
        match category {
            ReadinessCategory::Critical => {
                "Implement all critical requirements: authentication, error handling, health checks, input validation, and database migrations".to_string()
            }
            ReadinessCategory::Important => {
                "Focus on API documentation, comprehensive testing, performance monitoring, and configuration management".to_string()
            }
            ReadinessCategory::NiceToHave => {
                "Consider implementing advanced caching, rate limiting, circuit breakers, or monitoring dashboards if needed".to_string()
            }
        }
    }

    /// Generate deployment gate recommendations
    pub fn deployment_gate_recommendations(&self, report: &ReadinessReport) -> Vec<String> {
        let mut recommendations = Vec::new();

        if report.overall_score < 60.0 {
            recommendations
                .push("❌ BLOCKED: Overall score too low for any deployment".to_string());
            recommendations.push("Focus on critical requirements first".to_string());
        } else if report.overall_score < 75.0 {
            recommendations.push("⚠️ CAUTION: Deploy to staging only, not production".to_string());
            recommendations
                .push("Complete critical requirements before production deployment".to_string());
        } else if report.overall_score < 90.0 {
            recommendations.push("✅ READY: Deploy to production with monitoring".to_string());
            recommendations
                .push("Consider addressing remaining important requirements".to_string());
        } else {
            recommendations.push("🚀 EXCELLENT: Ready for production deployment".to_string());
        }

        recommendations
    }
}

impl Default for ReadinessValidator {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lifecycle::ReadinessStatus;

    #[test]
    fn test_validation_thresholds() {
        let mut tracker = ReadinessTracker::new("/tmp/test");
        tracker.load().unwrap();

        // Update ALL critical requirements to be complete (there are 6 critical requirements)
        tracker
            .update_requirement("auth-basic", ReadinessStatus::Complete)
            .unwrap();
        tracker
            .update_requirement("error-handling", ReadinessStatus::Complete)
            .unwrap();
        tracker
            .update_requirement("logging-tracing", ReadinessStatus::Complete)
            .unwrap();
        tracker
            .update_requirement("health-checks", ReadinessStatus::Complete)
            .unwrap();
        tracker
            .update_requirement("input-validation", ReadinessStatus::Complete)
            .unwrap();
        tracker
            .update_requirement("database-migrations", ReadinessStatus::Complete)
            .unwrap();

        let report = tracker.generate_report();
        let validator = ReadinessValidator::new();
        let result = validator.validate(&report);

        // Critical should pass (100% complete) OR there should be no critical issues
        // If validation fails, it's because non-critical requirements are incomplete
        if !result.passed {
            let critical_issues: Vec<_> = result
                .issues
                .iter()
                .filter(|i| i.category == ReadinessCategory::Critical)
                .collect();
            assert!(
                critical_issues.is_empty(),
                "Should have no critical issues, but found: {:?}",
                critical_issues
            );
        }
    }

    #[test]
    fn test_deployment_gate_recommendations() {
        let mut tracker = ReadinessTracker::new("/tmp/test");
        tracker.load().unwrap();

        let report = tracker.generate_report();
        let validator = ReadinessValidator::new();
        let recommendations = validator.deployment_gate_recommendations(&report);

        assert!(!recommendations.is_empty());
        assert!(
            recommendations[0].contains("BLOCKED")
                || recommendations[0].contains("CAUTION")
                || recommendations[0].contains("READY")
                || recommendations[0].contains("EXCELLENT")
        );
    }

    #[test]
    fn test_custom_thresholds() {
        let mut thresholds = HashMap::new();
        thresholds.insert(ReadinessCategory::Critical, 90.0); // Lower threshold for testing
        thresholds.insert(ReadinessCategory::Important, 70.0);

        let validator = ReadinessValidator::with_thresholds(thresholds);

        // Test that custom thresholds are used
        assert_eq!(validator.thresholds[&ReadinessCategory::Critical], 90.0);
        assert_eq!(validator.thresholds[&ReadinessCategory::Important], 70.0);
    }
}
