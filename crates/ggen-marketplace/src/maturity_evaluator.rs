//! Marketplace Maturity Evaluator
//!
//! This module provides evaluation logic for assessing package maturity based on
//! actual package metadata, repository data, and quality metrics.

use crate::maturity::*;
use serde::{Deserialize, Serialize};
use std::path::Path;

/// Package evaluation input data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvaluationInput {
    /// Package ID
    pub package_id: String,
    /// Package name
    pub package_name: String,
    /// Number of downloads
    pub downloads: u64,
    /// Average rating (0-5)
    pub rating: f32,
    /// Has comprehensive README
    pub has_readme: bool,
    /// Has API documentation
    pub has_api_docs: bool,
    /// Has examples
    pub has_examples: bool,
    /// Has changelog
    pub has_changelog: bool,
    /// Test coverage percentage (0-100)
    pub test_coverage: f32,
    /// Has unit tests
    pub has_unit_tests: bool,
    /// Has integration tests
    pub has_integration_tests: bool,
    /// Has E2E tests
    pub has_e2e_tests: bool,
    /// Number of security vulnerabilities found
    pub vulnerabilities: u32,
    /// Has dependency audit performed
    pub has_dependency_audit: bool,
    /// Uses unsafe code (percentage)
    pub unsafe_code_percent: f32,
    /// Has benchmarks
    pub has_benchmarks: bool,
    /// Has performance optimization documentation
    pub has_optimization_docs: bool,
    /// Determinism verified
    pub determinism_verified: bool,
    /// Days since last release
    pub days_since_last_release: u32,
    /// Number of active contributors
    pub active_contributors: u32,
    /// Average issue response time (hours)
    pub avg_issue_response_hours: f32,
    /// Number of academic citations
    pub academic_citations: u32,
}

impl Default for EvaluationInput {
    fn default() -> Self {
        Self {
            package_id: String::new(),
            package_name: String::new(),
            downloads: 0,
            rating: 0.0,
            has_readme: false,
            has_api_docs: false,
            has_examples: false,
            has_changelog: false,
            test_coverage: 0.0,
            has_unit_tests: false,
            has_integration_tests: false,
            has_e2e_tests: false,
            vulnerabilities: 0,
            has_dependency_audit: false,
            unsafe_code_percent: 0.0,
            has_benchmarks: false,
            has_optimization_docs: false,
            determinism_verified: false,
            days_since_last_release: 0,
            active_contributors: 0,
            avg_issue_response_hours: 0.0,
            academic_citations: 0,
        }
    }
}

/// Maturity evaluator for packages
pub struct MaturityEvaluator;

impl MaturityEvaluator {
    /// Evaluate package maturity based on input data
    pub fn evaluate(input: EvaluationInput) -> MaturityAssessment {
        let mut assessment = MaturityAssessment::new(&input.package_id, &input.package_name);

        // Evaluate documentation (max 20 points)
        assessment.documentation = Self::evaluate_documentation(
            input.has_readme,
            input.has_api_docs,
            input.has_examples,
            input.has_changelog,
        );

        // Evaluate testing (max 20 points)
        assessment.testing = Self::evaluate_testing(
            input.test_coverage,
            input.has_unit_tests,
            input.has_integration_tests,
            input.has_e2e_tests,
        );

        // Evaluate security (max 20 points)
        assessment.security = Self::evaluate_security(
            input.vulnerabilities,
            input.has_dependency_audit,
            input.unsafe_code_percent,
        );

        // Evaluate performance (max 15 points)
        assessment.performance = Self::evaluate_performance(
            input.has_benchmarks,
            input.has_optimization_docs,
            input.determinism_verified,
        );

        // Evaluate adoption (max 15 points)
        assessment.adoption = Self::evaluate_adoption(
            input.downloads,
            input.academic_citations,
            input.active_contributors,
        );

        // Evaluate maintenance (max 10 points)
        assessment.maintenance = Self::evaluate_maintenance(
            input.days_since_last_release,
            input.active_contributors,
            input.avg_issue_response_hours,
        );

        assessment
    }

    /// Evaluate documentation dimension
    fn evaluate_documentation(
        has_readme: bool,
        has_api_docs: bool,
        has_examples: bool,
        has_changelog: bool,
    ) -> DocumentationScore {
        let mut score = DocumentationScore::default();

        if has_readme {
            score.readme = 5;
        }
        if has_api_docs {
            score.api_docs = 5;
        }
        if has_examples {
            score.examples = 5;
        }
        if has_changelog {
            score.changelog = 5;
        }

        score
    }

    /// Evaluate testing dimension
    fn evaluate_testing(
        test_coverage: f32,
        has_unit_tests: bool,
        has_integration_tests: bool,
        has_e2e_tests: bool,
    ) -> TestingScore {
        let mut score = TestingScore {
            coverage_percent: test_coverage,
            ..Default::default()
        };

        // Unit tests: 0-8 points based on coverage
        if has_unit_tests {
            if test_coverage >= 80.0 {
                score.unit_tests = 8;
            } else if test_coverage >= 60.0 {
                score.unit_tests = 6;
            } else if test_coverage >= 40.0 {
                score.unit_tests = 4;
            } else if test_coverage >= 20.0 {
                score.unit_tests = 2;
            } else {
                score.unit_tests = 1;
            }
        }

        // Integration tests: 0-6 points
        if has_integration_tests {
            score.integration_tests = 6;
        }

        // E2E tests: 0-4 points
        if has_e2e_tests {
            score.e2e_tests = 4;
        }

        score
    }

    /// Evaluate security dimension
    fn evaluate_security(
        vulnerabilities: u32,
        has_dependency_audit: bool,
        unsafe_code_percent: f32,
    ) -> SecurityScore {
        let mut score = SecurityScore::default();

        // Vulnerability scan: 0-10 points
        score.vulnerability_scan = match vulnerabilities {
            0 => 10,
            1..=2 => 8,
            3..=5 => 6,
            6..=10 => 4,
            _ => 2,
        };

        // Dependency audit: 0-5 points
        if has_dependency_audit {
            score.dependency_audit = 5;
        }

        // Safe code: 0-5 points
        score.safe_code = if unsafe_code_percent == 0.0 {
            5
        } else if unsafe_code_percent < 2.0 {
            4
        } else if unsafe_code_percent < 5.0 {
            3
        } else if unsafe_code_percent < 10.0 {
            2
        } else {
            1
        };

        score
    }

    /// Evaluate performance dimension
    fn evaluate_performance(
        has_benchmarks: bool,
        has_optimization_docs: bool,
        determinism_verified: bool,
    ) -> PerformanceScore {
        let mut score = PerformanceScore::default();

        if has_benchmarks {
            score.benchmarks = 8;
        }
        if has_optimization_docs {
            score.optimization = 4;
        }
        if determinism_verified {
            score.determinism = 3;
        }

        score
    }

    /// Evaluate adoption dimension
    fn evaluate_adoption(
        downloads: u64,
        academic_citations: u32,
        active_contributors: u32,
    ) -> AdoptionScore {
        let mut score = AdoptionScore::default();

        // Downloads: 0-6 points
        score.downloads = match downloads {
            0..=10 => 0,
            11..=50 => 1,
            51..=100 => 2,
            101..=500 => 3,
            501..=1000 => 4,
            1001..=5000 => 5,
            _ => 6,
        };

        // Academic citations: 0-5 points
        score.citations = match academic_citations {
            0 => 0,
            1..=2 => 1,
            3..=5 => 2,
            6..=10 => 3,
            11..=20 => 4,
            _ => 5,
        };

        // Community: 0-4 points
        score.community = match active_contributors {
            0 => 0,
            1 => 1,
            2..=3 => 2,
            4..=5 => 3,
            _ => 4,
        };

        score
    }

    /// Evaluate maintenance dimension
    fn evaluate_maintenance(
        days_since_last_release: u32,
        active_contributors: u32,
        avg_issue_response_hours: f32,
    ) -> MaintenanceScore {
        let mut score = MaintenanceScore::default();

        // Release cadence: 0-5 points
        score.release_cadence = match days_since_last_release {
            0..=30 => 5,
            31..=90 => 4,
            91..=180 => 3,
            181..=365 => 2,
            366..=730 => 1,
            _ => 0,
        };

        // Responsiveness: 0-3 points
        score.responsiveness = if avg_issue_response_hours > 0.0 {
            match avg_issue_response_hours {
                0.0..=24.0 => 3,
                24.1..=48.0 => 2,
                48.1..=72.0 => 1,
                _ => 0,
            }
        } else {
            0
        };

        // Active maintenance: 0-2 points
        score.active_maintenance = if active_contributors > 0 { 2 } else { 0 };

        score
    }

    /// Batch evaluate multiple packages
    pub fn evaluate_batch(inputs: Vec<EvaluationInput>) -> Vec<MaturityAssessment> {
        inputs.into_iter().map(Self::evaluate).collect()
    }

    /// Generate a maturity report for a single package
    pub fn generate_report(input: EvaluationInput) -> serde_json::Value {
        let assessment = Self::evaluate(input);
        assessment.to_report()
    }
}

/// Marketplace maturity dashboard
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MaturityDashboard {
    /// Dashboard generated timestamp
    pub generated_at: chrono::DateTime<chrono::Utc>,
    /// All package assessments
    pub assessments: Vec<MaturityAssessment>,
    /// Statistics across all packages
    pub statistics: DashboardStatistics,
}

/// Dashboard statistics
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct DashboardStatistics {
    /// Total packages
    pub total_packages: usize,
    /// Average maturity score
    pub average_score: f32,
    /// Distribution of maturity levels
    pub level_distribution: LevelDistribution,
    /// Average scores by dimension
    pub average_scores_by_dimension: AverageDimensionScores,
}

/// Maturity level distribution
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct LevelDistribution {
    pub experimental: usize,
    pub beta: usize,
    pub production: usize,
    pub enterprise: usize,
}

/// Average scores by dimension
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct AverageDimensionScores {
    pub documentation: f32,
    pub testing: f32,
    pub security: f32,
    pub performance: f32,
    pub adoption: f32,
    pub maintenance: f32,
}

impl MaturityDashboard {
    /// Create dashboard from assessments
    pub fn new(assessments: Vec<MaturityAssessment>) -> Self {
        let statistics = Self::calculate_statistics(&assessments);

        Self {
            generated_at: chrono::Utc::now(),
            assessments,
            statistics,
        }
    }

    /// Calculate dashboard statistics
    fn calculate_statistics(assessments: &[MaturityAssessment]) -> DashboardStatistics {
        let total_packages = assessments.len();

        if total_packages == 0 {
            return DashboardStatistics::default();
        }

        let mut level_distribution = LevelDistribution::default();
        let mut sum_score = 0u32;
        let mut sum_documentation = 0u32;
        let mut sum_testing = 0u32;
        let mut sum_security = 0u32;
        let mut sum_performance = 0u32;
        let mut sum_adoption = 0u32;
        let mut sum_maintenance = 0u32;

        for assessment in assessments {
            let score = assessment.total_score();
            sum_score += score;

            match assessment.level() {
                MaturityLevel::Experimental => level_distribution.experimental += 1,
                MaturityLevel::Beta => level_distribution.beta += 1,
                MaturityLevel::Production => level_distribution.production += 1,
                MaturityLevel::Enterprise => level_distribution.enterprise += 1,
            }

            sum_documentation += assessment.documentation.total() as u32;
            sum_testing += assessment.testing.total() as u32;
            sum_security += assessment.security.total() as u32;
            sum_performance += assessment.performance.total() as u32;
            sum_adoption += assessment.adoption.total() as u32;
            sum_maintenance += assessment.maintenance.total() as u32;
        }

        DashboardStatistics {
            total_packages,
            average_score: sum_score as f32 / total_packages as f32,
            level_distribution,
            average_scores_by_dimension: AverageDimensionScores {
                documentation: sum_documentation as f32 / total_packages as f32,
                testing: sum_testing as f32 / total_packages as f32,
                security: sum_security as f32 / total_packages as f32,
                performance: sum_performance as f32 / total_packages as f32,
                adoption: sum_adoption as f32 / total_packages as f32,
                maintenance: sum_maintenance as f32 / total_packages as f32,
            },
        }
    }

    /// Get packages by maturity level
    pub fn packages_by_level(&self, level: MaturityLevel) -> Vec<&MaturityAssessment> {
        self.assessments
            .iter()
            .filter(|a| a.level() == level)
            .collect()
    }

    /// Get top packages by score
    pub fn top_packages(&self, count: usize) -> Vec<&MaturityAssessment> {
        let mut packages: Vec<_> = self.assessments.iter().collect();
        packages.sort_by(|a, b| b.total_score().cmp(&a.total_score()));
        packages.into_iter().take(count).collect()
    }

    /// Get packages needing improvement (score < threshold)
    pub fn packages_needing_improvement(&self, threshold: u32) -> Vec<&MaturityAssessment> {
        self.assessments
            .iter()
            .filter(|a| a.total_score() < threshold)
            .collect()
    }

    /// Serialize to JSON report
    pub fn to_report(&self) -> serde_json::Value {
        serde_json::json!({
            "generated_at": self.generated_at.to_rfc3339(),
            "statistics": {
                "total_packages": self.statistics.total_packages,
                "average_score": self.statistics.average_score,
                "level_distribution": {
                    "experimental": self.statistics.level_distribution.experimental,
                    "beta": self.statistics.level_distribution.beta,
                    "production": self.statistics.level_distribution.production,
                    "enterprise": self.statistics.level_distribution.enterprise,
                },
                "average_scores_by_dimension": {
                    "documentation": self.statistics.average_scores_by_dimension.documentation,
                    "testing": self.statistics.average_scores_by_dimension.testing,
                    "security": self.statistics.average_scores_by_dimension.security,
                    "performance": self.statistics.average_scores_by_dimension.performance,
                    "adoption": self.statistics.average_scores_by_dimension.adoption,
                    "maintenance": self.statistics.average_scores_by_dimension.maintenance,
                },
            },
            "assessments": self.assessments.iter().map(|a| a.to_report()).collect::<Vec<_>>(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_evaluation() {
        let input = EvaluationInput {
            package_id: "io.test.package".to_string(),
            package_name: "Test Package".to_string(),
            has_readme: true,
            has_api_docs: true,
            test_coverage: 85.0,
            has_unit_tests: true,
            has_integration_tests: true,
            vulnerabilities: 0,
            has_dependency_audit: true,
            unsafe_code_percent: 0.0,
            has_benchmarks: true,
            determinism_verified: true,
            days_since_last_release: 15,
            active_contributors: 3,
            avg_issue_response_hours: 24.0,
            downloads: 500,
            academic_citations: 5,
            ..Default::default()
        };

        let assessment = MaturityEvaluator::evaluate(input);
        assert!(assessment.total_score() > 50);
    }

    #[test]
    fn test_dashboard() {
        let assessments = vec![MaturityAssessment::new("pkg1", "Package 1")];
        let dashboard = MaturityDashboard::new(assessments);
        assert_eq!(dashboard.statistics.total_packages, 1);
    }
}
