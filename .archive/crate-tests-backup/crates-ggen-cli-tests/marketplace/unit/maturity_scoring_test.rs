//! Unit tests for marketplace maturity scoring algorithm
//!
//! Tests verify the 6-dimension maturity scoring system:
//! - Documentation (max 20 points)
//! - Testing (max 20 points)
//! - Security (max 20 points)
//! - Performance (max 15 points)
//! - Adoption (max 15 points)
//! - Maintenance (max 10 points)

use ggen_marketplace::prelude::*;

#[test]
fn test_documentation_dimension_full_score() {
    // Arrange: Package with all documentation elements
    let input = EvaluationInput {
        package_id: "test.pkg".to_string(),
        package_name: "Test Package".to_string(),
        has_readme: true,
        has_api_docs: true,
        has_examples: true,
        has_changelog: true,
        ..Default::default()
    };

    // Act: Evaluate maturity
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert: Documentation should be 20/20
    assert_eq!(
        assessment.documentation.total(),
        20,
        "Full documentation should score 20 points"
    );
    assert_eq!(assessment.documentation.readme, 5);
    assert_eq!(assessment.documentation.api_docs, 5);
    assert_eq!(assessment.documentation.examples, 5);
    assert_eq!(assessment.documentation.changelog, 5);
}

#[test]
fn test_documentation_dimension_partial_score() {
    // Arrange: Package with only README and examples
    let input = EvaluationInput {
        package_id: "test.pkg".to_string(),
        package_name: "Test Package".to_string(),
        has_readme: true,
        has_api_docs: false,
        has_examples: true,
        has_changelog: false,
        ..Default::default()
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert: Should score 10/20
    assert_eq!(assessment.documentation.total(), 10);
    assert_eq!(assessment.documentation.readme, 5);
    assert_eq!(assessment.documentation.api_docs, 0);
    assert_eq!(assessment.documentation.examples, 5);
    assert_eq!(assessment.documentation.changelog, 0);
}

#[test]
fn test_testing_dimension_high_coverage() {
    // Arrange: Package with 90% test coverage
    let input = EvaluationInput {
        package_id: "test.pkg".to_string(),
        package_name: "Test Package".to_string(),
        test_coverage: 90.0,
        has_unit_tests: true,
        has_integration_tests: true,
        has_e2e_tests: true,
        ..Default::default()
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert: Should score 18/20 (8 unit + 6 integration + 4 e2e)
    assert_eq!(assessment.testing.total(), 18);
    assert_eq!(assessment.testing.unit_tests, 8);
    assert_eq!(assessment.testing.integration_tests, 6);
    assert_eq!(assessment.testing.e2e_tests, 4);
}

#[test]
fn test_testing_dimension_medium_coverage() {
    // Arrange: Package with 65% coverage, only unit tests
    let input = EvaluationInput {
        package_id: "test.pkg".to_string(),
        package_name: "Test Package".to_string(),
        test_coverage: 65.0,
        has_unit_tests: true,
        has_integration_tests: false,
        has_e2e_tests: false,
        ..Default::default()
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert: Should score 6/20 (only unit tests at 60%+ coverage)
    assert_eq!(assessment.testing.total(), 6);
    assert_eq!(assessment.testing.unit_tests, 6);
}

#[test]
fn test_security_dimension_no_vulnerabilities() {
    // Arrange: Secure package
    let input = EvaluationInput {
        package_id: "test.pkg".to_string(),
        package_name: "Test Package".to_string(),
        vulnerabilities: 0,
        has_dependency_audit: true,
        unsafe_code_percent: 0.0,
        ..Default::default()
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert: Should score high on security
    assert!(
        assessment.security.total() >= 18,
        "Secure package should score at least 18/20"
    );
}

#[test]
fn test_security_dimension_with_vulnerabilities() {
    // Arrange: Package with 3 vulnerabilities
    let input = EvaluationInput {
        package_id: "test.pkg".to_string(),
        package_name: "Test Package".to_string(),
        vulnerabilities: 3,
        has_dependency_audit: false,
        unsafe_code_percent: 5.0,
        ..Default::default()
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert: Should score low on security
    assert!(
        assessment.security.total() < 10,
        "Package with vulnerabilities should score low"
    );
}

#[test]
fn test_maturity_level_thresholds() {
    // Test: Experimental (0-40)
    let experimental_input = EvaluationInput {
        package_id: "experimental.pkg".to_string(),
        package_name: "Experimental".to_string(),
        has_readme: true,
        test_coverage: 30.0,
        has_unit_tests: true,
        ..Default::default()
    };
    let assessment = MaturityEvaluator::evaluate(experimental_input);
    assert!(
        matches!(assessment.level(), MaturityLevel::Experimental),
        "Score {} should be Experimental",
        assessment.total_score()
    );

    // Test: Production (61-80)
    let production_input = EvaluationInput {
        package_id: "production.pkg".to_string(),
        package_name: "Production".to_string(),
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
        downloads: 1000,
        active_contributors: 3,
        ..Default::default()
    };
    let assessment = MaturityEvaluator::evaluate(production_input);
    assert!(
        matches!(
            assessment.level(),
            MaturityLevel::Production | MaturityLevel::Enterprise
        ),
        "Score {} should be Production or Enterprise",
        assessment.total_score()
    );
}

#[test]
fn test_total_score_calculation() {
    // Arrange: Package with known scores
    let input = EvaluationInput {
        package_id: "test.pkg".to_string(),
        package_name: "Test Package".to_string(),
        has_readme: true,
        has_api_docs: true,
        has_examples: true,
        has_changelog: true,
        test_coverage: 80.0,
        has_unit_tests: true,
        has_integration_tests: true,
        has_e2e_tests: true,
        vulnerabilities: 0,
        has_dependency_audit: true,
        unsafe_code_percent: 0.0,
        has_benchmarks: true,
        has_optimization_docs: true,
        determinism_verified: true,
        downloads: 1000,
        active_contributors: 5,
        days_since_last_release: 30,
        avg_issue_response_hours: 24.0,
        academic_citations: 2,
        rating: 4.5,
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert: Total should be sum of all dimensions
    let expected_total = assessment.documentation.total()
        + assessment.testing.total()
        + assessment.security.total()
        + assessment.performance.total()
        + assessment.adoption.total()
        + assessment.maintenance.total();

    assert_eq!(assessment.total_score(), expected_total);
    assert!(
        assessment.total_score() <= 100,
        "Total score should not exceed 100"
    );
}

#[test]
fn test_score_breakdown_percentages() {
    // Arrange
    let input = EvaluationInput {
        package_id: "test.pkg".to_string(),
        package_name: "Test Package".to_string(),
        has_readme: true,
        has_api_docs: true,
        test_coverage: 80.0,
        has_unit_tests: true,
        ..Default::default()
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);
    let percentages = assessment.score_breakdown();

    // Assert: All percentages should be between 0 and 100
    assert!(percentages["documentation"] >= 0.0 && percentages["documentation"] <= 100.0);
    assert!(percentages["testing"] >= 0.0 && percentages["testing"] <= 100.0);
    assert!(percentages["security"] >= 0.0 && percentages["security"] <= 100.0);
    assert!(percentages["performance"] >= 0.0 && percentages["performance"] <= 100.0);
    assert!(percentages["adoption"] >= 0.0 && percentages["adoption"] <= 100.0);
    assert!(percentages["maintenance"] >= 0.0 && percentages["maintenance"] <= 100.0);
}

#[test]
fn test_extreme_values() {
    // Test: All zeros
    let zero_input = EvaluationInput::default();
    let assessment = MaturityEvaluator::evaluate(zero_input);
    assert_eq!(assessment.total_score(), 0, "Default input should score 0");

    // Test: Maximum possible score
    let max_input = EvaluationInput {
        package_id: "max.pkg".to_string(),
        package_name: "Max Package".to_string(),
        has_readme: true,
        has_api_docs: true,
        has_examples: true,
        has_changelog: true,
        test_coverage: 100.0,
        has_unit_tests: true,
        has_integration_tests: true,
        has_e2e_tests: true,
        vulnerabilities: 0,
        has_dependency_audit: true,
        unsafe_code_percent: 0.0,
        has_benchmarks: true,
        has_optimization_docs: true,
        determinism_verified: true,
        downloads: 1_000_000,
        active_contributors: 50,
        days_since_last_release: 1,
        avg_issue_response_hours: 1.0,
        academic_citations: 100,
        rating: 5.0,
    };
    let assessment = MaturityEvaluator::evaluate(max_input);
    assert!(
        assessment.total_score() >= 95,
        "Maximum input should score very high"
    );
}

#[test]
fn test_maturity_level_descriptions() {
    // Arrange
    let levels = vec![
        MaturityLevel::Experimental,
        MaturityLevel::Beta,
        MaturityLevel::Production,
        MaturityLevel::Enterprise,
    ];

    // Assert: All levels have descriptions
    for level in levels {
        let description = level.description();
        assert!(
            !description.is_empty(),
            "Level {:?} should have description",
            level
        );
        assert!(description.len() > 10, "Description should be meaningful");
    }
}

#[test]
fn test_maturity_level_recommendations() {
    // Arrange
    let levels = vec![
        MaturityLevel::Experimental,
        MaturityLevel::Beta,
        MaturityLevel::Production,
        MaturityLevel::Enterprise,
    ];

    // Assert: All levels have recommendations
    for level in levels {
        let recommendations = level.recommendations();
        assert!(
            !recommendations.is_empty(),
            "Level {:?} should have recommendations",
            level
        );
    }
}

#[test]
fn test_maturity_dashboard_statistics() {
    // Arrange: Create multiple assessments
    let assessments = vec![
        MaturityAssessment::new("pkg1", "Package 1"),
        MaturityAssessment::new("pkg2", "Package 2"),
        MaturityAssessment::new("pkg3", "Package 3"),
    ];

    // Act
    let dashboard = MaturityDashboard::new(assessments);

    // Assert
    assert_eq!(dashboard.statistics.total_packages, 3);
    assert!(dashboard.statistics.average_score >= 0.0);
    assert!(dashboard.statistics.average_score <= 100.0);
    assert_eq!(
        dashboard.statistics.level_distribution.experimental
            + dashboard.statistics.level_distribution.beta
            + dashboard.statistics.level_distribution.production
            + dashboard.statistics.level_distribution.enterprise,
        3
    );
}
