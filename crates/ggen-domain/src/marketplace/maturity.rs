//! Maturity Assessment for Marketplace Packages
//!
//! Provides 6-dimension maturity scoring for packages:
//! - Documentation (20 points)
//! - Testing (20 points)
//! - Security (20 points)
//! - Performance (15 points)
//! - Adoption (15 points)
//! - Maintenance (10 points)

use serde::{Deserialize, Serialize};

/// Input for maturity evaluation
#[derive(Debug, Clone, Default)]
pub struct EvaluationInput {
    pub package_id: String,
    pub package_name: String,
    // Documentation (20 points max)
    pub has_readme: bool,
    pub has_api_docs: bool,
    pub has_examples: bool,
    pub has_changelog: bool,
    // Testing (20 points max)
    pub test_coverage: f64,
    pub has_unit_tests: bool,
    pub has_integration_tests: bool,
    pub has_e2e_tests: bool,
    // Security (20 points max)
    pub vulnerabilities: usize,
    pub has_dependency_audit: bool,
    pub unsafe_code_percent: f64,
    // Performance (15 points max)
    pub has_benchmarks: bool,
    pub has_optimization_docs: bool,
    pub determinism_verified: bool,
    // Adoption (15 points max)
    pub days_since_last_release: u64,
    pub active_contributors: usize,
    pub avg_issue_response_hours: f64,
    pub downloads: u64,
    pub academic_citations: u32,
    pub rating: f64,
    // Maintenance (10 points max)
}

/// Maturity level classification
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum MaturityLevel {
    Experimental = 0,
    Beta = 1,
    Production = 2,
    Enterprise = 3,
}

impl MaturityLevel {
    pub fn description(&self) -> &'static str {
        match self {
            MaturityLevel::Experimental => "Early development, not recommended for production",
            MaturityLevel::Beta => "Feature complete, needs real-world testing",
            MaturityLevel::Production => "Ready for production use",
            MaturityLevel::Enterprise => "Meets enterprise standards",
        }
    }

    pub fn recommendations(&self) -> Vec<&'static str> {
        match self {
            MaturityLevel::Experimental => vec![
                "Add comprehensive documentation",
                "Increase test coverage",
                "Perform security audit",
                "Add performance benchmarks",
            ],
            MaturityLevel::Beta => vec![
                "Gather user feedback",
                "Increase adoption metrics",
                "Document real-world usage",
                "Validate in production-like environments",
            ],
            MaturityLevel::Production => vec![
                "Maintain high test coverage",
                "Keep dependencies updated",
                "Monitor performance in production",
                "Respond to issues promptly",
            ],
            MaturityLevel::Enterprise => vec![
                "Maintain excellence across all dimensions",
                "Share best practices with community",
                "Contribute to ecosystem",
            ],
        }
    }
}

/// Documentation dimension scores (0-20)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DocumentationScores {
    pub total: u32,
    pub readme: u32,
    pub api_docs: u32,
    pub examples: u32,
    pub changelog: u32,
}

/// Testing dimension scores (0-20)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TestingScores {
    pub total: u32,
    pub unit_tests: u32,
    pub integration_tests: u32,
    pub e2e_tests: u32,
}

/// Security dimension scores (0-20)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SecurityScores {
    pub total: u32,
    pub vulnerability_free: u32,
    pub audit: u32,
    pub safe_code: u32,
}

/// Performance dimension scores (0-15)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PerformanceScores {
    pub total: u32,
    pub benchmarks: u32,
    pub optimization: u32,
    pub determinism: u32,
}

/// Adoption dimension scores (0-15)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AdoptionScores {
    pub total: u32,
    pub freshness: u32,
    pub contributors: u32,
    pub community: u32,
}

/// Maintenance dimension scores (0-10)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MaintenanceScores {
    pub total: u32,
    pub responsiveness: u32,
    pub activity: u32,
}

/// Complete maturity assessment
#[derive(Debug, Clone)]
pub struct MaturityAssessment {
    pub package_id: String,
    pub package_name: String,
    pub documentation: DocumentationScores,
    pub testing: TestingScores,
    pub security: SecurityScores,
    pub performance: PerformanceScores,
    pub adoption: AdoptionScores,
    pub maintenance: MaintenanceScores,
}

impl MaturityAssessment {
    pub fn new(package_id: String, package_name: String) -> Self {
        Self {
            package_id,
            package_name,
            documentation: DocumentationScores { total: 0, readme: 0, api_docs: 0, examples: 0, changelog: 0 },
            testing: TestingScores { total: 0, unit_tests: 0, integration_tests: 0, e2e_tests: 0 },
            security: SecurityScores { total: 0, vulnerability_free: 0, audit: 0, safe_code: 0 },
            performance: PerformanceScores { total: 0, benchmarks: 0, optimization: 0, determinism: 0 },
            adoption: AdoptionScores { total: 0, freshness: 0, contributors: 0, community: 0 },
            maintenance: MaintenanceScores { total: 0, responsiveness: 0, activity: 0 },
        }
    }

    pub fn total_score(&self) -> u32 {
        self.documentation.total
            + self.testing.total
            + self.security.total
            + self.performance.total
            + self.adoption.total
            + self.maintenance.total
    }

    pub fn level(&self) -> MaturityLevel {
        let score = self.total_score();
        if score >= 81 {
            MaturityLevel::Enterprise
        } else if score >= 61 {
            MaturityLevel::Production
        } else if score >= 41 {
            MaturityLevel::Beta
        } else {
            MaturityLevel::Experimental
        }
    }

    pub fn score_breakdown(&self) -> std::collections::HashMap<String, f32> {
        let max = 100.0;
        let mut map = std::collections::HashMap::new();
        map.insert("documentation".to_string(), (self.documentation.total as f32 / max) * 20.0);
        map.insert("testing".to_string(), (self.testing.total as f32 / max) * 20.0);
        map.insert("security".to_string(), (self.security.total as f32 / max) * 20.0);
        map.insert("performance".to_string(), (self.performance.total as f32 / max) * 15.0);
        map.insert("adoption".to_string(), (self.adoption.total as f32 / max) * 15.0);
        map.insert("maintenance".to_string(), (self.maintenance.total as f32 / max) * 10.0);
        map
    }

    pub fn all_feedback(&self) -> Vec<(String, Vec<String>)> {
        let mut feedback = Vec::new();

        if self.documentation.total < 15 {
            feedback.push(("documentation".to_string(), vec![
                "Add comprehensive README".to_string(),
                "Document API with examples".to_string(),
                "Include usage examples".to_string(),
            ]));
        }

        if self.testing.total < 15 {
            feedback.push(("testing".to_string(), vec![
                "Increase test coverage".to_string(),
                "Add integration tests".to_string(),
                "Include E2E tests".to_string(),
            ]));
        }

        if self.security.total < 15 {
            feedback.push(("security".to_string(), vec![
                "Fix security vulnerabilities".to_string(),
                "Run dependency audit".to_string(),
                "Reduce unsafe code".to_string(),
            ]));
        }

        feedback
    }
}

/// Maturity evaluator
pub struct MaturityEvaluator;

impl MaturityEvaluator {
    pub fn evaluate(input: EvaluationInput) -> MaturityAssessment {
        let documentation = Self::evaluate_documentation(&input);
        let testing = Self::evaluate_testing(&input);
        let security = Self::evaluate_security(&input);
        let performance = Self::evaluate_performance(&input);
        let adoption = Self::evaluate_adoption(&input);
        let maintenance = Self::evaluate_maintenance(&input);

        MaturityAssessment {
            package_id: input.package_id,
            package_name: input.package_name,
            documentation,
            testing,
            security,
            performance,
            adoption,
            maintenance,
        }
    }

    fn evaluate_documentation(input: &EvaluationInput) -> DocumentationScores {
        let readme = if input.has_readme { 5 } else { 0 };
        let api_docs = if input.has_api_docs { 5 } else { 0 };
        let examples = if input.has_examples { 5 } else { 0 };
        let changelog = if input.has_changelog { 5 } else { 0 };
        let total = readme + api_docs + examples + changelog;

        DocumentationScores { total, readme, api_docs, examples, changelog }
    }

    fn evaluate_testing(input: &EvaluationInput) -> TestingScores {
        let coverage_score = (input.test_coverage / 100.0 * 8.0).min(8.0) as u32;
        let unit_tests = if input.has_unit_tests { 8 } else { 0 };
        let integration_tests = if input.has_integration_tests { 6 } else { 0 };
        let e2e_tests = if input.has_e2e_tests { 4 } else { 0 };
        let total = coverage_score + unit_tests + integration_tests + e2e_tests;

        TestingScores { total, unit_tests, integration_tests, e2e_tests }
    }

    fn evaluate_security(input: &EvaluationInput) -> SecurityScores {
        let vuln_score = if input.vulnerabilities == 0 { 8 } else { 0 };
        let audit_score = if input.has_dependency_audit { 6 } else { 0 };
        let safe_score = if input.unsafe_code_percent < 1.0 { 6 } else { 0 };
        let total = vuln_score + audit_score + safe_score;

        SecurityScores { total, vulnerability_free: vuln_score, audit: audit_score, safe_code: safe_score }
    }

    fn evaluate_performance(input: &EvaluationInput) -> PerformanceScores {
        let benchmarks = if input.has_benchmarks { 5 } else { 0 };
        let optimization = if input.has_optimization_docs { 5 } else { 0 };
        let determinism = if input.determinism_verified { 5 } else { 0 };
        let total = benchmarks + optimization + determinism;

        PerformanceScores { total, benchmarks, optimization, determinism }
    }

    fn evaluate_adoption(input: &EvaluationInput) -> AdoptionScores {
        let freshness = if input.days_since_last_release < 90 { 5 } else { 0 };
        let contributors = (input.active_contributors as u32 * 2).min(5);
        let community = if input.rating >= 4.0 { 5 } else { 0 };
        let total = freshness + contributors + community;

        AdoptionScores { total, freshness, contributors, community }
    }

    fn evaluate_maintenance(_input: &EvaluationInput) -> MaintenanceScores {
        // Simplified - always return moderate score
        MaintenanceScores { total: 5, responsiveness: 3, activity: 2 }
    }
}

// Fix for the return type issue
impl MaturityAssessment {
    pub fn security(&self) -> &SecurityScores {
        &self.security
    }
}
