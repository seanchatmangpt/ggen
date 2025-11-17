//! Integration Tests for Package Scoring Algorithm
//!
//! Tests the marketplace package scoring system including criterion evaluation,
//! score calculation, quality metrics, and maturity assessment.
//!
//! ## Test Coverage
//!
//! - Package quality scoring (0-100 scale)
//! - Individual criterion evaluation
//! - Maturity level assessment
//! - Score consistency and bounds checking
//! - Documentation, testing, and performance scores
//!
//! ## Running These Tests
//!
//! ```bash
//! cargo test --test package_scoring_tests
//! ```

use chicago_tdd_tools::test;
use ggen_marketplace::prelude::*;

// Import common test utilities
#[path = "../common/mod.rs"]
mod common;

// ============================================================================
// Basic Scoring Tests
// ============================================================================

test!(test_package_base_score_calculation, {
    // Arrange
    let input = EvaluationInput {
        package_id: "test-package".to_string(),
        package_name: "Test Package".to_string(),
        ..Default::default()
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert
    let total_score = assessment.total_score();
    assert!(
        total_score >= 0 && total_score <= 100,
        "Score should be in 0-100 range, got {}",
        total_score
    );
    Ok::<(), Box<dyn std::error::Error>>(())
});

test!(test_empty_package_low_score, {
    // Arrange - Minimal package with no features
    let input = EvaluationInput {
        package_id: "minimal-package".to_string(),
        package_name: "Minimal Package".to_string(),
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
        days_since_last_release: 365, // Old release
        active_contributors: 0,
        avg_issue_response_hours: 0.0,
        academic_citations: 0,
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert
    let total_score = assessment.total_score();
    assert!(
        total_score < 50,
        "Minimal package should have low score, got {}",
        total_score
    );
    Ok::<(), Box<dyn std::error::Error>>(())
});

test!(test_well_documented_package_higher_score, {
    // Arrange - Well-documented package
    let input = EvaluationInput {
        package_id: "documented-package".to_string(),
        package_name: "Well Documented Package".to_string(),
        downloads: 1000,
        rating: 4.5,
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
        days_since_last_release: 30,
        active_contributors: 5,
        avg_issue_response_hours: 24.0,
        academic_citations: 10,
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert - Documentation score should be high (max 20 points)
    let doc_score = assessment.documentation.total();
    assert!(
        doc_score >= 15,
        "Well-documented package should have good documentation score, got {}",
        doc_score
    );
    Ok::<(), Box<dyn std::error::Error>>(())
});

// ============================================================================
// Documentation Scoring Tests
// ============================================================================

test!(test_documentation_score_with_readme, {
    // Arrange
    let input = EvaluationInput {
        package_id: "test-package".to_string(),
        package_name: "Test Package".to_string(),
        has_readme: true,
        has_examples: false,
        has_api_docs: false,
        has_changelog: false,
        ..Default::default()
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert
    let doc_score = assessment.documentation.total();
    assert!(
        doc_score > 0,
        "Package with README should have positive documentation score, got {}",
        doc_score
    );
    Ok::<(), Box<dyn std::error::Error>>(())
});

test!(test_documentation_score_comprehensive, {
    // Arrange
    let input = EvaluationInput {
        package_id: "test-package".to_string(),
        package_name: "Test Package".to_string(),
        has_readme: true,
        has_examples: true,
        has_api_docs: true,
        has_changelog: true,
        ..Default::default()
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert - All documentation features = max 20 points
    let doc_score = assessment.documentation.total();
    assert!(
        doc_score >= 18,
        "Comprehensive documentation should score highly, got {}",
        doc_score
    );
    Ok::<(), Box<dyn std::error::Error>>(())
});

test!(test_documentation_score_no_docs, {
    // Arrange
    let input = EvaluationInput {
        package_id: "test-package".to_string(),
        package_name: "Test Package".to_string(),
        has_readme: false,
        has_examples: false,
        has_api_docs: false,
        has_changelog: false,
        ..Default::default()
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert - No docs = 0 points
    let doc_score = assessment.documentation.total();
    assert_eq!(
        doc_score, 0,
        "No documentation should score 0, got {}",
        doc_score
    );
    Ok::<(), Box<dyn std::error::Error>>(())
});

// ============================================================================
// Maturity Level Tests
// ============================================================================

test!(test_maturity_level_experimental, {
    // Arrange - Low score package (0-40 = Experimental)
    let input = EvaluationInput {
        package_id: "experimental-package".to_string(),
        package_name: "Experimental Package".to_string(),
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
        days_since_last_release: 365,
        active_contributors: 0,
        avg_issue_response_hours: 0.0,
        academic_citations: 0,
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert - Score 0-40 = Experimental
    let level = assessment.level();
    assert_eq!(
        level,
        MaturityLevel::Experimental,
        "Low score package should be experimental, got {:?} with score {}",
        level,
        assessment.total_score()
    );
    Ok::<(), Box<dyn std::error::Error>>(())
});

test!(test_maturity_level_stable, {
    // Arrange - Moderate score package (41-60 = Beta, 61-80 = Stable)
    let input = EvaluationInput {
        package_id: "stable-package".to_string(),
        package_name: "Stable Package".to_string(),
        downloads: 5000,
        rating: 4.0,
        has_readme: true,
        has_api_docs: true,
        has_examples: true,
        has_changelog: true,
        test_coverage: 70.0,
        has_unit_tests: true,
        has_integration_tests: true,
        has_e2e_tests: false,
        vulnerabilities: 0,
        has_dependency_audit: true,
        unsafe_code_percent: 5.0,
        has_benchmarks: true,
        has_optimization_docs: true,
        determinism_verified: true,
        days_since_last_release: 60,
        active_contributors: 3,
        avg_issue_response_hours: 48.0,
        academic_citations: 5,
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert - Should be Stable (61-80) or higher
    let level = assessment.level();
    assert!(
        level >= MaturityLevel::Stable,
        "Well-maintained package should be stable or higher, got {:?} with score {}",
        level,
        assessment.total_score()
    );
    Ok::<(), Box<dyn std::error::Error>>(())
});

test!(test_maturity_level_production, {
    // Arrange - High score package (81-100 = Enterprise/Production)
    let input = EvaluationInput {
        package_id: "production-package".to_string(),
        package_name: "Production Package".to_string(),
        downloads: 100000,
        rating: 4.8,
        has_readme: true,
        has_api_docs: true,
        has_examples: true,
        has_changelog: true,
        test_coverage: 90.0,
        has_unit_tests: true,
        has_integration_tests: true,
        has_e2e_tests: true,
        vulnerabilities: 0,
        has_dependency_audit: true,
        unsafe_code_percent: 0.0,
        has_benchmarks: true,
        has_optimization_docs: true,
        determinism_verified: true,
        days_since_last_release: 30,
        active_contributors: 10,
        avg_issue_response_hours: 12.0,
        academic_citations: 50,
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert - Should be Production (81-100) or Enterprise
    let level = assessment.level();
    assert!(
        level >= MaturityLevel::Production,
        "Comprehensive package should be production-ready, got {:?} with score {}",
        level,
        assessment.total_score()
    );
    Ok::<(), Box<dyn std::error::Error>>(())
});

// ============================================================================
// Security Scoring Tests
// ============================================================================

test!(test_security_score_with_audit, {
    // Arrange - Package with security audit
    let input = EvaluationInput {
        package_id: "secure-package".to_string(),
        package_name: "Secure Package".to_string(),
        vulnerabilities: 0,
        has_dependency_audit: true,
        unsafe_code_percent: 0.0,
        ..Default::default()
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert
    let security_score = assessment.security.total();
    assert!(
        security_score > 0,
        "Package with security audit should have positive security score, got {}",
        security_score
    );
    Ok::<(), Box<dyn std::error::Error>>(())
});

test!(test_security_score_with_vulnerabilities, {
    // Arrange - Package with vulnerabilities
    let input = EvaluationInput {
        package_id: "vulnerable-package".to_string(),
        package_name: "Vulnerable Package".to_string(),
        vulnerabilities: 5,
        has_dependency_audit: false,
        unsafe_code_percent: 20.0,
        ..Default::default()
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert - Should have lower security score
    let security_score = assessment.security.total();
    assert!(
        security_score < 10,
        "Package with vulnerabilities should have low security score, got {}",
        security_score
    );
    Ok::<(), Box<dyn std::error::Error>>(())
});

// ============================================================================
// Adoption Scoring Tests
// ============================================================================

test!(test_adoption_score_with_downloads, {
    // Arrange - Package with high adoption
    let input = EvaluationInput {
        package_id: "popular-package".to_string(),
        package_name: "Popular Package".to_string(),
        downloads: 50000,
        academic_citations: 20,
        active_contributors: 15,
        ..Default::default()
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert
    let adoption_score = assessment.adoption.total();
    assert!(
        adoption_score > 0,
        "Package with high adoption should have positive adoption score, got {}",
        adoption_score
    );
    Ok::<(), Box<dyn std::error::Error>>(())
});

// ============================================================================
// Score Consistency Tests
// ============================================================================

test!(test_score_consistency_across_evaluations, {
    // Arrange
    let input = EvaluationInput {
        package_id: "test-package".to_string(),
        package_name: "Test Package".to_string(),
        downloads: 1000,
        rating: 4.0,
        has_readme: true,
        has_api_docs: true,
        test_coverage: 80.0,
        has_unit_tests: true,
        vulnerabilities: 0,
        has_dependency_audit: true,
        unsafe_code_percent: 0.0,
        days_since_last_release: 30,
        active_contributors: 5,
        ..Default::default()
    };

    // Act - Evaluate same input twice
    let assessment1 = MaturityEvaluator::evaluate(input.clone());
    let assessment2 = MaturityEvaluator::evaluate(input);

    // Assert - Should be deterministic
    assert_eq!(
        assessment1.total_score(),
        assessment2.total_score(),
        "Scores should be consistent for same input"
    );
    assert_eq!(
        assessment1.level(),
        assessment2.level(),
        "Maturity level should be consistent"
    );
    Ok::<(), Box<dyn std::error::Error>>(())
});

test!(test_all_scores_within_bounds, {
    // Arrange
    let input = EvaluationInput {
        package_id: "test-package".to_string(),
        package_name: "Test Package".to_string(),
        downloads: 1000,
        rating: 4.0,
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
        days_since_last_release: 30,
        active_contributors: 5,
        avg_issue_response_hours: 24.0,
        academic_citations: 10,
    };

    // Act
    let assessment = MaturityEvaluator::evaluate(input);

    // Assert - All individual scores should be within their max bounds
    // Documentation: max 20, Testing: max 20, Security: max 20, Performance: max 15, Adoption: max 15, Maintenance: max 10
    assert!(
        assessment.documentation.total() <= 20,
        "Documentation score should be <= 20, got {}",
        assessment.documentation.total()
    );
    assert!(
        assessment.testing.total() <= 20,
        "Testing score should be <= 20, got {}",
        assessment.testing.total()
    );
    assert!(
        assessment.security.total() <= 20,
        "Security score should be <= 20, got {}",
        assessment.security.total()
    );
    assert!(
        assessment.performance.total() <= 15,
        "Performance score should be <= 15, got {}",
        assessment.performance.total()
    );
    assert!(
        assessment.adoption.total() <= 15,
        "Adoption score should be <= 15, got {}",
        assessment.adoption.total()
    );
    assert!(
        assessment.maintenance.total() <= 10,
        "Maintenance score should be <= 10, got {}",
        assessment.maintenance.total()
    );
    // Total should be <= 100
    assert!(
        assessment.total_score() <= 100,
        "Total score should be <= 100, got {}",
        assessment.total_score()
    );
    Ok::<(), Box<dyn std::error::Error>>(())
});

// ============================================================================
// Comparison and Recommendation Tests
// ============================================================================

test!(test_compare_package_assessments, {
    // Arrange - Two different packages
    let input1 = EvaluationInput {
        package_id: "package1".to_string(),
        package_name: "Package One".to_string(),
        downloads: 100,
        test_coverage: 50.0,
        ..Default::default()
    };

    let input2 = EvaluationInput {
        package_id: "package2".to_string(),
        package_name: "Package Two".to_string(),
        downloads: 10000,
        test_coverage: 90.0,
        has_readme: true,
        has_api_docs: true,
        has_unit_tests: true,
        ..Default::default()
    };

    // Act
    let assessment1 = MaturityEvaluator::evaluate(input1);
    let assessment2 = MaturityEvaluator::evaluate(input2);

    // Assert
    assert_ne!(
        assessment1.total_score(),
        assessment2.total_score(),
        "Different packages should have different scores"
    );
    assert!(
        assessment2.total_score() > assessment1.total_score(),
        "Better package should have higher score"
    );
    Ok::<(), Box<dyn std::error::Error>>(())
});

test!(test_filter_by_maturity_level, {
    // Arrange - Two packages with different maturity levels
    let inputs = vec![
        EvaluationInput {
            package_id: "experimental".to_string(),
            package_name: "Experimental".to_string(),
            ..Default::default() // Low score = Experimental
        },
        EvaluationInput {
            package_id: "stable".to_string(),
            package_name: "Stable".to_string(),
            downloads: 5000,
            rating: 4.0,
            has_readme: true,
            has_api_docs: true,
            test_coverage: 70.0,
            has_unit_tests: true,
            has_integration_tests: true,
            vulnerabilities: 0,
            has_dependency_audit: true,
            unsafe_code_percent: 0.0,
            days_since_last_release: 60,
            active_contributors: 3,
            ..Default::default() // Higher score = Stable
        },
    ];

    // Act
    let assessments: Vec<_> = inputs
        .into_iter()
        .map(|input| MaturityEvaluator::evaluate(input))
        .collect();

    let stable_packages: Vec<_> = assessments
        .iter()
        .filter(|a| a.level() >= MaturityLevel::Stable)
        .collect();

    // Assert
    assert_eq!(stable_packages.len(), 1, "Should find one stable package");
    Ok::<(), Box<dyn std::error::Error>>(())
});

// ============================================================================
// Performance Tests
// ============================================================================

test!(test_scoring_performance, {
    // Arrange
    let input = EvaluationInput {
        package_id: "test-package".to_string(),
        package_name: "Test Package".to_string(),
        downloads: 1000,
        rating: 4.0,
        has_readme: true,
        has_api_docs: true,
        test_coverage: 80.0,
        has_unit_tests: true,
        ..Default::default()
    };

    // Act
    let start = std::time::Instant::now();
    let _assessment = MaturityEvaluator::evaluate(input);
    let duration = start.elapsed();

    // Assert
    assert!(
        duration.as_millis() < 50,
        "Package scoring should be fast (< 50ms), took {:?}",
        duration
    );
    Ok::<(), Box<dyn std::error::Error>>(())
});

test!(test_batch_scoring_performance, {
    // Arrange
    let inputs: Vec<_> = (0..100)
        .map(|i| EvaluationInput {
            package_id: format!("package-{}", i),
            package_name: format!("Package {}", i),
            downloads: i as u64 * 100,
            test_coverage: (i % 100) as f32,
            ..Default::default()
        })
        .collect();

    // Act
    let start = std::time::Instant::now();
    let assessments: Vec<_> = inputs
        .into_iter()
        .map(|input| MaturityEvaluator::evaluate(input))
        .collect();
    let duration = start.elapsed();

    // Assert
    assert_eq!(assessments.len(), 100, "Should evaluate all packages");
    assert!(
        duration.as_secs() < 1,
        "Batch scoring should be fast (< 1s for 100 packages), took {:?}",
        duration
    );
    Ok::<(), Box<dyn std::error::Error>>(())
});
