//! Package quality scoring and metrics
//!
//! This module implements quality scoring for marketplace packages,
//! based on various metrics like documentation, testing, security, etc.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Overall quality score for a package
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityScore {
    /// Overall score (0.0 - 1.0)
    pub overall: f64,
    /// Individual metric scores
    pub metrics: QualityMetrics,
    /// Quality grade (A, B, C, D, F)
    pub grade: QualityGrade,
    /// Issues found during analysis
    pub issues: Vec<QualityIssue>,
    /// Recommendations for improvement
    pub recommendations: Vec<String>,
}

/// Individual quality metrics
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct QualityMetrics {
    /// Documentation quality (0.0 - 1.0)
    pub documentation: f64,
    /// Test coverage (0.0 - 1.0)
    pub testing: f64,
    /// Security score (0.0 - 1.0)
    pub security: f64,
    /// Code quality (0.0 - 1.0)
    pub code_quality: f64,
    /// Maintenance activity (0.0 - 1.0)
    pub maintenance: f64,
    /// Compatibility (0.0 - 1.0)
    pub compatibility: f64,
}

/// Quality grade based on overall score
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum QualityGrade {
    /// Excellent (>= 0.9)
    A,
    /// Good (>= 0.8)
    B,
    /// Acceptable (>= 0.7)
    C,
    /// Poor (>= 0.6)
    D,
    /// Failing (< 0.6)
    F,
}

/// A quality issue found during analysis
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QualityIssue {
    /// Issue category
    pub category: IssueCategory,
    /// Issue severity
    pub severity: IssueSeverity,
    /// Issue description
    pub description: String,
    /// How to fix this issue
    pub fix: Option<String>,
}

/// Categories of quality issues
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum IssueCategory {
    /// Documentation issue
    Documentation,
    /// Testing issue
    Testing,
    /// Security issue
    Security,
    /// Code quality issue
    CodeQuality,
    /// Maintenance issue
    Maintenance,
    /// Compatibility issue
    Compatibility,
}

/// Severity of quality issues
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum IssueSeverity {
    /// Critical - must fix
    Critical,
    /// Major - should fix
    Major,
    /// Minor - nice to fix
    Minor,
    /// Info - informational
    Info,
}

/// Quality scorer for packages
#[derive(Debug)]
pub struct QualityScorer {
    /// Weights for each metric
    weights: HashMap<String, f64>,
    /// Minimum passing score
    min_passing_score: f64,
}

impl Default for QualityScorer {
    fn default() -> Self {
        Self::new()
    }
}

impl QualityScorer {
    /// Create a new quality scorer with default weights
    pub fn new() -> Self {
        let mut weights = HashMap::new();
        weights.insert("documentation".to_string(), 0.20);
        weights.insert("testing".to_string(), 0.25);
        weights.insert("security".to_string(), 0.25);
        weights.insert("code_quality".to_string(), 0.15);
        weights.insert("maintenance".to_string(), 0.10);
        weights.insert("compatibility".to_string(), 0.05);

        Self {
            weights,
            min_passing_score: 0.6,
        }
    }

    /// Create scorer with custom weights
    pub fn with_weights(weights: HashMap<String, f64>) -> Self {
        Self {
            weights,
            min_passing_score: 0.6,
        }
    }

    /// Calculate quality score from metrics
    pub fn score(&self, metrics: &QualityMetrics) -> QualityScore {
        let mut issues = Vec::new();
        let mut recommendations = Vec::new();

        // Check documentation
        if metrics.documentation < 0.5 {
            issues.push(QualityIssue {
                category: IssueCategory::Documentation,
                severity: IssueSeverity::Major,
                description: "Documentation is incomplete".to_string(),
                fix: Some("Add README, API docs, and examples".to_string()),
            });
            recommendations.push("Improve documentation coverage".to_string());
        }

        // Check testing
        if metrics.testing < 0.5 {
            issues.push(QualityIssue {
                category: IssueCategory::Testing,
                severity: IssueSeverity::Major,
                description: "Test coverage is low".to_string(),
                fix: Some("Add unit and integration tests".to_string()),
            });
            recommendations.push("Increase test coverage to at least 80%".to_string());
        }

        // Check security
        if metrics.security < 0.7 {
            issues.push(QualityIssue {
                category: IssueCategory::Security,
                severity: IssueSeverity::Critical,
                description: "Security vulnerabilities detected".to_string(),
                fix: Some("Run cargo audit and fix vulnerabilities".to_string()),
            });
            recommendations.push("Address security vulnerabilities immediately".to_string());
        }

        // Check maintenance
        if metrics.maintenance < 0.4 {
            issues.push(QualityIssue {
                category: IssueCategory::Maintenance,
                severity: IssueSeverity::Minor,
                description: "Package appears unmaintained".to_string(),
                fix: Some("Update dependencies and respond to issues".to_string()),
            });
        }

        // Calculate weighted overall score
        let overall = self.calculate_weighted_score(metrics);
        let grade = self.score_to_grade(overall);

        QualityScore {
            overall,
            metrics: metrics.clone(),
            grade,
            issues,
            recommendations,
        }
    }

    /// Calculate weighted score from metrics
    fn calculate_weighted_score(&self, metrics: &QualityMetrics) -> f64 {
        let doc_weight = self.weights.get("documentation").unwrap_or(&0.20);
        let test_weight = self.weights.get("testing").unwrap_or(&0.25);
        let sec_weight = self.weights.get("security").unwrap_or(&0.25);
        let code_weight = self.weights.get("code_quality").unwrap_or(&0.15);
        let maint_weight = self.weights.get("maintenance").unwrap_or(&0.10);
        let compat_weight = self.weights.get("compatibility").unwrap_or(&0.05);

        metrics.documentation * doc_weight
            + metrics.testing * test_weight
            + metrics.security * sec_weight
            + metrics.code_quality * code_weight
            + metrics.maintenance * maint_weight
            + metrics.compatibility * compat_weight
    }

    /// Convert score to grade
    fn score_to_grade(&self, score: f64) -> QualityGrade {
        if score >= 0.9 {
            QualityGrade::A
        } else if score >= 0.8 {
            QualityGrade::B
        } else if score >= 0.7 {
            QualityGrade::C
        } else if score >= 0.6 {
            QualityGrade::D
        } else {
            QualityGrade::F
        }
    }

    /// Check if score passes minimum threshold
    pub fn passes(&self, score: &QualityScore) -> bool {
        score.overall >= self.min_passing_score
    }

    /// Get minimum passing score
    pub fn min_passing_score(&self) -> f64 {
        self.min_passing_score
    }
}

impl QualityMetrics {
    /// Create metrics with all values set
    pub fn new(
        documentation: f64,
        testing: f64,
        security: f64,
        code_quality: f64,
        maintenance: f64,
        compatibility: f64,
    ) -> Self {
        Self {
            documentation: documentation.clamp(0.0, 1.0),
            testing: testing.clamp(0.0, 1.0),
            security: security.clamp(0.0, 1.0),
            code_quality: code_quality.clamp(0.0, 1.0),
            maintenance: maintenance.clamp(0.0, 1.0),
            compatibility: compatibility.clamp(0.0, 1.0),
        }
    }

    /// Create high-quality metrics
    pub fn excellent() -> Self {
        Self::new(0.95, 0.95, 1.0, 0.9, 0.9, 0.95)
    }

    /// Create passing but not excellent metrics
    pub fn passing() -> Self {
        Self::new(0.7, 0.7, 0.8, 0.7, 0.7, 0.8)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_excellent_metrics() {
        let scorer = QualityScorer::new();
        let metrics = QualityMetrics::excellent();
        let score = scorer.score(&metrics);

        assert!(score.overall >= 0.9);
        assert_eq!(score.grade, QualityGrade::A);
        assert!(scorer.passes(&score));
    }

    #[test]
    fn test_passing_metrics() {
        let scorer = QualityScorer::new();
        let metrics = QualityMetrics::passing();
        let score = scorer.score(&metrics);

        assert!(score.overall >= 0.6);
        assert!(scorer.passes(&score));
    }

    #[test]
    fn test_failing_metrics() {
        let scorer = QualityScorer::new();
        let metrics = QualityMetrics::new(0.2, 0.2, 0.3, 0.3, 0.2, 0.3);
        let score = scorer.score(&metrics);

        assert!(score.overall < 0.6);
        assert_eq!(score.grade, QualityGrade::F);
        assert!(!scorer.passes(&score));
    }

    #[test]
    fn test_issues_generated() {
        let scorer = QualityScorer::new();
        let metrics = QualityMetrics::new(0.3, 0.3, 0.5, 0.7, 0.3, 0.8);
        let score = scorer.score(&metrics);

        // Should have issues for documentation, testing, security, maintenance
        assert!(!score.issues.is_empty());
        assert!(score.issues.iter().any(|i| i.category == IssueCategory::Documentation));
        assert!(score.issues.iter().any(|i| i.category == IssueCategory::Testing));
    }

    #[test]
    fn test_grade_boundaries() {
        let scorer = QualityScorer::new();

        assert_eq!(scorer.score_to_grade(0.95), QualityGrade::A);
        assert_eq!(scorer.score_to_grade(0.85), QualityGrade::B);
        assert_eq!(scorer.score_to_grade(0.75), QualityGrade::C);
        assert_eq!(scorer.score_to_grade(0.65), QualityGrade::D);
        assert_eq!(scorer.score_to_grade(0.55), QualityGrade::F);
    }
}
