//! Marketplace Package Maturity Matrix
//!
//! This module provides a comprehensive maturity assessment system for marketplace packages.
//! Packages are evaluated across six dimensions:
//!
//! 1. **Documentation** (0-20 points): README, examples, API docs, tutorials
//! 2. **Testing** (0-20 points): Unit tests, integration tests, E2E tests, coverage
//! 3. **Security** (0-20 points): Vulnerability scanning, dependency audit, safe code
//! 4. **Performance** (0-15 points): Benchmarks, optimization, determinism verification
//! 5. **Adoption** (0-15 points): Downloads, citations, active usage, community
//! 6. **Maintenance** (0-10 points): Update frequency, issue response, release cadence
//!
//! Total possible score: 100 points
//!
//! ## Maturity Levels
//!
//! - **Experimental** (0-40): Early-stage, not recommended for production
//! - **Beta** (41-60): Functional but incomplete, suitable for testing
//! - **Production** (61-80): Stable and reliable, suitable for production use
//! - **Enterprise** (81-100): Fully mature, recommended for mission-critical systems

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Maturity level of a marketplace package
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum MaturityLevel {
    /// 0-40: Early-stage, not recommended for production
    Experimental,
    /// 41-60: Functional but incomplete
    Beta,
    /// 61-80: Stable and reliable
    Production,
    /// 81-100: Fully mature and recommended
    Enterprise,
}

impl MaturityLevel {
    /// Get the maturity level from a score (0-100)
    pub fn from_score(score: u32) -> Self {
        match score {
            0..=40 => MaturityLevel::Experimental,
            41..=60 => MaturityLevel::Beta,
            61..=80 => MaturityLevel::Production,
            81..=100 => MaturityLevel::Enterprise,
            _ => MaturityLevel::Enterprise,
        }
    }

    /// Get human-readable description of maturity level
    pub fn description(&self) -> &'static str {
        match self {
            MaturityLevel::Experimental => {
                "Early-stage package, not recommended for production use"
            }
            MaturityLevel::Beta => "Functional but incomplete, suitable for testing and feedback",
            MaturityLevel::Production => "Stable and reliable, suitable for production use",
            MaturityLevel::Enterprise => {
                "Fully mature, recommended for mission-critical systems"
            }
        }
    }

    /// Get recommended next steps
    pub fn recommendations(&self) -> Vec<&'static str> {
        match self {
            MaturityLevel::Experimental => vec![
                "Add comprehensive documentation (README, examples, API docs)",
                "Implement unit and integration tests",
                "Set up security scanning and vulnerability audits",
                "Create benchmarks and performance baselines",
            ],
            MaturityLevel::Beta => vec![
                "Expand test coverage to reach 80%+ (currently under 80%)",
                "Add E2E tests and integration tests",
                "Implement security audit automation",
                "Create performance optimization targets",
            ],
            MaturityLevel::Production => vec![
                "Maintain test coverage above 85%",
                "Establish regular release cadence",
                "Monitor and respond to community issues",
                "Plan for enterprise features and SLAs",
            ],
            MaturityLevel::Enterprise => vec![
                "Maintain comprehensive documentation",
                "Sustain test coverage above 90%",
                "Monitor adoption metrics and community health",
                "Plan for long-term maintenance and evolution",
            ],
        }
    }
}

/// Documentation scoring criteria
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct DocumentationScore {
    /// README exists and is comprehensive (0-5 points)
    pub readme: u32,
    /// API documentation present (0-5 points)
    pub api_docs: u32,
    /// Examples and tutorials (0-5 points)
    pub examples: u32,
    /// Changelog and release notes (0-5 points)
    pub changelog: u32,
}

impl DocumentationScore {
    /// Calculate total documentation score (max 20)
    pub fn total(&self) -> u32 {
        (self.readme + self.api_docs + self.examples + self.changelog).min(20)
    }

    /// Get feedback on documentation
    pub fn feedback(&self) -> Vec<String> {
        let mut feedback = Vec::new();
        if self.readme < 5 {
            feedback.push("Enhance README with more detailed descriptions and usage examples".to_string());
        }
        if self.api_docs < 5 {
            feedback.push("Add comprehensive API documentation with parameter descriptions".to_string());
        }
        if self.examples < 5 {
            feedback.push("Include more examples covering different use cases".to_string());
        }
        if self.changelog < 5 {
            feedback.push("Maintain a detailed changelog documenting all versions".to_string());
        }
        feedback
    }
}

/// Testing scoring criteria
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct TestingScore {
    /// Unit test coverage (0-8 points, based on coverage %)
    pub unit_tests: u32,
    /// Integration tests (0-6 points)
    pub integration_tests: u32,
    /// E2E tests (0-4 points)
    pub e2e_tests: u32,
    /// Test coverage percentage (recorded for reference)
    pub coverage_percent: f32,
}

impl TestingScore {
    /// Calculate total testing score (max 20)
    pub fn total(&self) -> u32 {
        (self.unit_tests + self.integration_tests + self.e2e_tests).min(20)
    }

    /// Get testing feedback
    pub fn feedback(&self) -> Vec<String> {
        let mut feedback = Vec::new();
        if self.coverage_percent < 60.0 {
            feedback.push(format!(
                "Increase test coverage from {:.0}% to at least 80%",
                self.coverage_percent
            ));
        }
        if self.unit_tests < 8 {
            feedback.push("Expand unit tests to cover more edge cases and error paths".to_string());
        }
        if self.integration_tests < 6 {
            feedback.push("Add integration tests covering component interactions".to_string());
        }
        if self.e2e_tests < 4 {
            feedback.push("Implement E2E tests covering complete workflows".to_string());
        }
        feedback
    }
}

/// Security scoring criteria
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct SecurityScore {
    /// Vulnerability scan results (0-10 points)
    pub vulnerability_scan: u32,
    /// Dependency audit (0-5 points)
    pub dependency_audit: u32,
    /// Safe code practices (0-5 points, no unsafe, no unwrap, etc.)
    pub safe_code: u32,
}

impl SecurityScore {
    /// Calculate total security score (max 20)
    pub fn total(&self) -> u32 {
        (self.vulnerability_scan + self.dependency_audit + self.safe_code).min(20)
    }

    /// Get security feedback
    pub fn feedback(&self) -> Vec<String> {
        let mut feedback = Vec::new();
        if self.vulnerability_scan < 10 {
            feedback.push("Run security vulnerability scans and address findings".to_string());
        }
        if self.dependency_audit < 5 {
            feedback.push("Audit dependencies for known vulnerabilities (cargo audit)".to_string());
        }
        if self.safe_code < 5 {
            feedback.push("Eliminate unsafe code and replace unwrap/expect with proper error handling".to_string());
        }
        feedback
    }
}

/// Performance scoring criteria
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct PerformanceScore {
    /// Performance benchmarks (0-8 points)
    pub benchmarks: u32,
    /// Optimization work (0-4 points)
    pub optimization: u32,
    /// Determinism verification (0-3 points, important for code generation)
    pub determinism: u32,
}

impl PerformanceScore {
    /// Calculate total performance score (max 15)
    pub fn total(&self) -> u32 {
        (self.benchmarks + self.optimization + self.determinism).min(15)
    }

    /// Get performance feedback
    pub fn feedback(&self) -> Vec<String> {
        let mut feedback = Vec::new();
        if self.benchmarks < 8 {
            feedback.push("Create comprehensive benchmarks measuring generation time and memory usage".to_string());
        }
        if self.optimization < 4 {
            feedback.push("Profile and optimize critical paths for better performance".to_string());
        }
        if self.determinism < 3 {
            feedback.push("Verify deterministic output (byte-identical generation with same inputs)".to_string());
        }
        feedback
    }
}

/// Adoption scoring criteria
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct AdoptionScore {
    /// Downloads/usage metrics (0-6 points)
    pub downloads: u32,
    /// Academic citations and references (0-5 points)
    pub citations: u32,
    /// Active community and users (0-4 points)
    pub community: u32,
}

impl AdoptionScore {
    /// Calculate total adoption score (max 15)
    pub fn total(&self) -> u32 {
        (self.downloads + self.citations + self.community).min(15)
    }

    /// Get adoption feedback
    pub fn feedback(&self) -> Vec<String> {
        let mut feedback = Vec::new();
        if self.downloads < 6 {
            feedback.push("Promote package through blog posts, conferences, and community channels".to_string());
        }
        if self.citations < 5 {
            feedback.push("Publish research papers or case studies demonstrating real-world impact".to_string());
        }
        if self.community < 4 {
            feedback.push("Build community through documentation, examples, and engagement".to_string());
        }
        feedback
    }
}

/// Maintenance scoring criteria
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct MaintenanceScore {
    /// Release frequency and currency (0-5 points)
    pub release_cadence: u32,
    /// Issue and PR response time (0-3 points)
    pub responsiveness: u32,
    /// Active maintenance (0-2 points)
    pub active_maintenance: u32,
}

impl MaintenanceScore {
    /// Calculate total maintenance score (max 10)
    pub fn total(&self) -> u32 {
        (self.release_cadence + self.responsiveness + self.active_maintenance).min(10)
    }

    /// Get maintenance feedback
    pub fn feedback(&self) -> Vec<String> {
        let mut feedback = Vec::new();
        if self.release_cadence < 5 {
            feedback.push("Establish and maintain a regular release schedule (monthly or quarterly)".to_string());
        }
        if self.responsiveness < 3 {
            feedback.push("Improve issue and PR response time (target: 48 hours)".to_string());
        }
        if self.active_maintenance < 2 {
            feedback.push("Commit to ongoing maintenance and support".to_string());
        }
        feedback
    }
}

/// Complete maturity assessment for a package
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct MaturityAssessment {
    /// Package ID
    pub package_id: String,
    /// Package name
    pub package_name: String,
    /// Assessment timestamp
    pub assessed_at: chrono::DateTime<chrono::Utc>,
    /// Documentation score
    pub documentation: DocumentationScore,
    /// Testing score
    pub testing: TestingScore,
    /// Security score
    pub security: SecurityScore,
    /// Performance score
    pub performance: PerformanceScore,
    /// Adoption score
    pub adoption: AdoptionScore,
    /// Maintenance score
    pub maintenance: MaintenanceScore,
}

impl MaturityAssessment {
    /// Create new maturity assessment
    pub fn new(package_id: impl Into<String>, package_name: impl Into<String>) -> Self {
        Self {
            package_id: package_id.into(),
            package_name: package_name.into(),
            assessed_at: chrono::Utc::now(),
            ..Default::default()
        }
    }

    /// Calculate total maturity score (0-100)
    pub fn total_score(&self) -> u32 {
        self.documentation.total()
            + self.testing.total()
            + self.security.total()
            + self.performance.total()
            + self.adoption.total()
            + self.maintenance.total()
    }

    /// Get maturity level based on total score
    pub fn level(&self) -> MaturityLevel {
        MaturityLevel::from_score(self.total_score())
    }

    /// Get score breakdown as percentage
    pub fn score_breakdown(&self) -> HashMap<String, f32> {
        let mut breakdown = HashMap::new();
        let _total = self.total_score() as f32;

        breakdown.insert(
            "documentation".to_string(),
            (self.documentation.total() as f32 / 20.0) * 100.0,
        );
        breakdown.insert(
            "testing".to_string(),
            (self.testing.total() as f32 / 20.0) * 100.0,
        );
        breakdown.insert(
            "security".to_string(),
            (self.security.total() as f32 / 20.0) * 100.0,
        );
        breakdown.insert(
            "performance".to_string(),
            (self.performance.total() as f32 / 15.0) * 100.0,
        );
        breakdown.insert(
            "adoption".to_string(),
            (self.adoption.total() as f32 / 15.0) * 100.0,
        );
        breakdown.insert(
            "maintenance".to_string(),
            (self.maintenance.total() as f32 / 10.0) * 100.0,
        );

        breakdown
    }

    /// Get all feedback across all dimensions
    pub fn all_feedback(&self) -> Vec<(String, Vec<String>)> {
        let mut feedback = Vec::new();

        if !self.documentation.feedback().is_empty() {
            feedback.push(("documentation".to_string(), self.documentation.feedback()));
        }
        if !self.testing.feedback().is_empty() {
            feedback.push(("testing".to_string(), self.testing.feedback()));
        }
        if !self.security.feedback().is_empty() {
            feedback.push(("security".to_string(), self.security.feedback()));
        }
        if !self.performance.feedback().is_empty() {
            feedback.push(("performance".to_string(), self.performance.feedback()));
        }
        if !self.adoption.feedback().is_empty() {
            feedback.push(("adoption".to_string(), self.adoption.feedback()));
        }
        if !self.maintenance.feedback().is_empty() {
            feedback.push(("maintenance".to_string(), self.maintenance.feedback()));
        }

        feedback
    }

    /// Generate maturity report as JSON
    pub fn to_report(&self) -> serde_json::Value {
        serde_json::json!({
            "package_id": self.package_id,
            "package_name": self.package_name,
            "assessed_at": self.assessed_at.to_rfc3339(),
            "total_score": self.total_score(),
            "maturity_level": match self.level() {
                MaturityLevel::Experimental => "experimental",
                MaturityLevel::Beta => "beta",
                MaturityLevel::Production => "production",
                MaturityLevel::Enterprise => "enterprise",
            },
            "description": self.level().description(),
            "scores": {
                "documentation": self.documentation.total(),
                "testing": self.testing.total(),
                "security": self.security.total(),
                "performance": self.performance.total(),
                "adoption": self.adoption.total(),
                "maintenance": self.maintenance.total(),
            },
            "percentages": self.score_breakdown(),
            "details": {
                "documentation": {
                    "readme": self.documentation.readme,
                    "api_docs": self.documentation.api_docs,
                    "examples": self.documentation.examples,
                    "changelog": self.documentation.changelog,
                },
                "testing": {
                    "unit_tests": self.testing.unit_tests,
                    "integration_tests": self.testing.integration_tests,
                    "e2e_tests": self.testing.e2e_tests,
                    "coverage_percent": self.testing.coverage_percent,
                },
                "security": {
                    "vulnerability_scan": self.security.vulnerability_scan,
                    "dependency_audit": self.security.dependency_audit,
                    "safe_code": self.security.safe_code,
                },
                "performance": {
                    "benchmarks": self.performance.benchmarks,
                    "optimization": self.performance.optimization,
                    "determinism": self.performance.determinism,
                },
                "adoption": {
                    "downloads": self.adoption.downloads,
                    "citations": self.adoption.citations,
                    "community": self.adoption.community,
                },
                "maintenance": {
                    "release_cadence": self.maintenance.release_cadence,
                    "responsiveness": self.maintenance.responsiveness,
                    "active_maintenance": self.maintenance.active_maintenance,
                },
            },
            "feedback": self.all_feedback(),
            "next_steps": self.level().recommendations(),
        })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_maturity_levels() {
        assert_eq!(MaturityLevel::from_score(30), MaturityLevel::Experimental);
        assert_eq!(MaturityLevel::from_score(50), MaturityLevel::Beta);
        assert_eq!(MaturityLevel::from_score(70), MaturityLevel::Production);
        assert_eq!(MaturityLevel::from_score(90), MaturityLevel::Enterprise);
    }

    #[test]
    fn test_assessment_scoring() {
        let mut assessment = MaturityAssessment::new("io.test.package", "Test Package");

        assessment.documentation.readme = 5;
        assessment.documentation.api_docs = 5;
        assessment.testing.unit_tests = 8;
        assessment.testing.coverage_percent = 85.0;
        assessment.security.safe_code = 5;

        let score = assessment.total_score();
        assert!(score >= 23 && score <= 100);
    }

    #[test]
    fn test_score_breakdown() {
        let assessment = MaturityAssessment::new("io.test.package", "Test Package");
        let breakdown = assessment.score_breakdown();

        assert!(breakdown.contains_key("documentation"));
        assert!(breakdown.contains_key("testing"));
        assert!(breakdown.contains_key("security"));
    }

    #[test]
    fn test_maturity_report() {
        let assessment = MaturityAssessment::new("io.test.package", "Test Package");
        let report = assessment.to_report();

        assert!(report["package_id"].is_string());
        assert!(report["total_score"].is_number());
        assert!(report["maturity_level"].is_string());
        assert!(report["scores"].is_object());
    }
}
