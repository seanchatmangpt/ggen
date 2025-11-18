//! Test fixtures and helper functions for marketplace tests
//!
//! This module provides reusable test data and utilities.

use ggen_marketplace::prelude::*;

/// Create a test package with known maturity scores
pub fn create_experimental_package() -> MaturityAssessment {
    MaturityAssessment::new("test.experimental", "Experimental Package")
}

/// Create a beta-level test package
pub fn create_beta_package() -> MaturityAssessment {
    let mut assessment = MaturityAssessment::new("test.beta", "Beta Package");
    // Manually set scores to reach beta level (41-60)
    assessment.documentation.readme = 5;
    assessment.documentation.api_docs = 5;
    assessment.testing.unit_tests = 6;
    assessment
}

/// Create a production-ready test package
pub fn create_production_package() -> MaturityAssessment {
    let input = EvaluationInput {
        package_id: "test.production".to_string(),
        package_name: "Production Package".to_string(),
        has_readme: true,
        has_api_docs: true,
        has_examples: true,
        has_changelog: true,
        test_coverage: 85.0,
        has_unit_tests: true,
        has_integration_tests: true,
        has_e2e_tests: true,
        vulnerabilities: 0,
        has_dependency_audit: true,
        unsafe_code_percent: 0.0,
        has_benchmarks: true,
        downloads: 5000,
        active_contributors: 3,
        days_since_last_release: 30,
        avg_issue_response_hours: 24.0,
        academic_citations: 2,
        rating: 4.5,
    };
    MaturityEvaluator::evaluate(input)
}

/// Create an enterprise-grade test package
pub fn create_enterprise_package() -> MaturityAssessment {
    let input = EvaluationInput {
        package_id: "test.enterprise".to_string(),
        package_name: "Enterprise Package".to_string(),
        has_readme: true,
        has_api_docs: true,
        has_examples: true,
        has_changelog: true,
        test_coverage: 95.0,
        has_unit_tests: true,
        has_integration_tests: true,
        has_e2e_tests: true,
        vulnerabilities: 0,
        has_dependency_audit: true,
        unsafe_code_percent: 0.0,
        has_benchmarks: true,
        has_optimization_docs: true,
        determinism_verified: true,
        downloads: 100_000,
        active_contributors: 10,
        days_since_last_release: 7,
        avg_issue_response_hours: 6.0,
        academic_citations: 15,
        rating: 4.9,
    };
    MaturityEvaluator::evaluate(input)
}

/// Create a set of packages with varying maturity levels
pub fn create_test_package_set() -> Vec<MaturityAssessment> {
    vec![
        create_experimental_package(),
        create_beta_package(),
        create_production_package(),
        create_enterprise_package(),
    ]
}

/// Verify that a package meets production-ready criteria
pub fn assert_production_ready(assessment: &MaturityAssessment) {
    assert!(
        assessment.total_score() >= 61,
        "Production packages must score >= 61, got {}",
        assessment.total_score()
    );
    assert!(
        matches!(
            assessment.level(),
            MaturityLevel::Production | MaturityLevel::Enterprise
        ),
        "Expected Production or Enterprise level, got {:?}",
        assessment.level()
    );
}

/// Verify that a package has comprehensive documentation
pub fn assert_good_documentation(assessment: &MaturityAssessment) {
    assert!(
        assessment.documentation.total() >= 15,
        "Good documentation requires >= 15 points, got {}",
        assessment.documentation.total()
    );
}

/// Verify that a package has strong testing
pub fn assert_strong_testing(assessment: &MaturityAssessment) {
    assert!(
        assessment.testing.total() >= 15,
        "Strong testing requires >= 15 points, got {}",
        assessment.testing.total()
    );
}

/// Verify that a package is secure
pub fn assert_secure(assessment: &MaturityAssessment) {
    assert!(
        assessment.security.total() >= 18,
        "Secure packages require >= 18 security points, got {}",
        assessment.security.total()
    );
}
