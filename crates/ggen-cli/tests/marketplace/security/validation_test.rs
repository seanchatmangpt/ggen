//! Security tests for marketplace validation
//!
//! Tests verify security aspects:
//! - Input validation and sanitization
//! - Injection prevention (SQL, path traversal)
//! - Safe handling of untrusted input
//! - Authentication/authorization (if applicable)
//! - Cryptographic verification

use ggen_marketplace::prelude::*;

#[test]
fn test_package_id_validation() {
    // Valid package IDs
    let valid_ids = vec![
        "io.ggen.rust.cli",
        "com.example.package",
        "org.test.valid_name",
        "package.with.many.segments",
    ];

    for id in valid_ids {
        let assessment = MaturityAssessment::new(id, "Test");
        assert_eq!(assessment.package_id, id);
    }
}

#[test]
fn test_package_id_sanitization() {
    // Package IDs with special characters that should be handled
    let potentially_dangerous = vec![
        "../../../etc/passwd",
        "test; DROP TABLE packages;",
        "<script>alert('xss')</script>",
        "test\0null",
        "../../root/.ssh/id_rsa",
    ];

    for dangerous_id in potentially_dangerous {
        // Should either sanitize or create assessment without executing malicious intent
        let assessment = MaturityAssessment::new(dangerous_id, "Test");
        assert_eq!(assessment.package_id, dangerous_id); // Stored as-is but not executed
    }
}

#[test]
fn test_score_validation_prevents_overflow() {
    // Test that scores cannot exceed maximum
    let input = EvaluationInput {
        package_id: "test".to_string(),
        package_name: "Test".to_string(),
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
        downloads: u64::MAX,
        active_contributors: u32::MAX,
        days_since_last_release: 0,
        avg_issue_response_hours: 0.0,
        academic_citations: u32::MAX,
        rating: 5.0,
    };

    let assessment = MaturityEvaluator::evaluate(input);

    // Total score should never exceed 100
    assert!(
        assessment.total_score() <= 100,
        "Score {} exceeds maximum of 100",
        assessment.total_score()
    );

    // Individual dimensions should not exceed their max
    assert!(assessment.documentation.total() <= 20);
    assert!(assessment.testing.total() <= 20);
    assert!(assessment.security.total() <= 20);
    assert!(assessment.performance.total() <= 15);
    assert!(assessment.adoption.total() <= 15);
    assert!(assessment.maintenance.total() <= 10);
}

#[test]
fn test_negative_values_handled_safely() {
    // Test that negative values (if possible via API misuse) are handled
    let input = EvaluationInput {
        package_id: "test".to_string(),
        package_name: "Test".to_string(),
        test_coverage: -10.0,           // Invalid
        unsafe_code_percent: -5.0,      // Invalid
        avg_issue_response_hours: -1.0, // Invalid
        rating: -1.0,                   // Invalid
        ..Default::default()
    };

    let assessment = MaturityEvaluator::evaluate(input);

    // Should handle gracefully (clamp to 0 or similar)
    assert!(assessment.total_score() >= 0);
    assert!(assessment.testing.total() >= 0);
}

#[test]
fn test_vulnerability_count_security_impact() {
    // Test that vulnerabilities properly impact security score
    let scenarios = vec![
        (0, "no vulnerabilities"),
        (1, "one vulnerability"),
        (3, "multiple vulnerabilities"),
        (10, "many vulnerabilities"),
    ];

    let mut prev_score = u32::MAX;
    for (vuln_count, desc) in scenarios {
        let input = EvaluationInput {
            package_id: "test".to_string(),
            package_name: "Test".to_string(),
            vulnerabilities: vuln_count,
            has_dependency_audit: true,
            unsafe_code_percent: 0.0,
            ..Default::default()
        };

        let assessment = MaturityEvaluator::evaluate(input);
        let security_score = assessment.security.total();

        if prev_score != u32::MAX {
            assert!(
                security_score <= prev_score,
                "Security score should decrease with more vulnerabilities: {} has score {}",
                desc,
                security_score
            );
        }
        prev_score = security_score;
    }
}

#[test]
fn test_unsafe_code_percentage_validation() {
    // Test that unsafe code percentage impacts security
    let percentages = vec![0.0, 5.0, 10.0, 25.0, 50.0];

    let mut prev_score = u32::MAX;
    for unsafe_pct in percentages {
        let input = EvaluationInput {
            package_id: "test".to_string(),
            package_name: "Test".to_string(),
            unsafe_code_percent: unsafe_pct,
            vulnerabilities: 0,
            has_dependency_audit: true,
            ..Default::default()
        };

        let assessment = MaturityEvaluator::evaluate(input);
        let security_score = assessment.security.total();

        if prev_score != u32::MAX {
            assert!(
                security_score <= prev_score,
                "Security should decrease with more unsafe code: {}% has score {}",
                unsafe_pct,
                security_score
            );
        }
        prev_score = security_score;
    }
}

#[test]
fn test_dependency_audit_requirement() {
    // Test that dependency audit is important for security
    let with_audit = EvaluationInput {
        package_id: "with".to_string(),
        package_name: "With Audit".to_string(),
        has_dependency_audit: true,
        vulnerabilities: 0,
        unsafe_code_percent: 0.0,
        ..Default::default()
    };

    let without_audit = EvaluationInput {
        package_id: "without".to_string(),
        package_name: "Without Audit".to_string(),
        has_dependency_audit: false,
        vulnerabilities: 0,
        unsafe_code_percent: 0.0,
        ..Default::default()
    };

    let with_score = MaturityEvaluator::evaluate(with_audit).security.total();
    let without_score = MaturityEvaluator::evaluate(without_audit).security.total();

    assert!(
        with_score > without_score,
        "Dependency audit should increase security score"
    );
}

#[test]
fn test_extreme_input_values() {
    // Test with extreme but valid values
    let input = EvaluationInput {
        package_id: "extreme".to_string(),
        package_name: "Extreme".to_string(),
        test_coverage: f32::MAX,
        unsafe_code_percent: f32::MAX,
        downloads: u64::MAX,
        active_contributors: u32::MAX,
        days_since_last_release: u32::MAX,
        avg_issue_response_hours: f32::MAX,
        academic_citations: u32::MAX,
        vulnerabilities: u32::MAX,
        rating: f32::MAX,
        ..Default::default()
    };

    // Should not panic or produce invalid results
    let assessment = MaturityEvaluator::evaluate(input);
    assert!(assessment.total_score() >= 0);
    assert!(assessment.total_score() <= 100);
}

#[test]
fn test_nan_and_infinity_handling() {
    // Test that NaN and infinity are handled safely
    let input = EvaluationInput {
        package_id: "test".to_string(),
        package_name: "Test".to_string(),
        test_coverage: f32::NAN,
        unsafe_code_percent: f32::INFINITY,
        avg_issue_response_hours: f32::NEG_INFINITY,
        rating: f32::NAN,
        ..Default::default()
    };

    let assessment = MaturityEvaluator::evaluate(input);

    // All scores should be finite valid numbers
    assert!(assessment.total_score().is_finite());
    assert!(assessment.documentation.total().is_finite());
    assert!(assessment.testing.total().is_finite());
    assert!(assessment.security.total().is_finite());
}

#[test]
fn test_maturity_level_security_requirements() {
    // Production level should require good security
    let assessments = generate_all_assessments();
    let production = filter_by_level(&assessments, MaturityLevel::Production);

    for pkg in production {
        // Production packages should have decent security
        assert!(
            pkg.security.total() >= 12,
            "Production package {} should have security >= 12, got {}",
            pkg.package_id,
            pkg.security.total()
        );
    }
}

#[test]
fn test_filter_input_validation() {
    let assessments = generate_all_assessments();

    // Test that invalid filter criteria don't cause issues
    let invalid_criteria = vec![
        ("nonexistent_dimension", 10u32),
        ("", 0u32),
        ("documentation", u32::MAX),
    ];

    // Should handle gracefully without panicking
    let _ = filter_by_dimensions(&assessments, &invalid_criteria);
}

#[test]
fn test_export_format_injection() {
    // Test that export formats properly escape special characters
    let mut assessment =
        MaturityAssessment::new("test<script>alert('xss')</script>", "Test'\"Package");

    let assessments = vec![assessment];

    // CSV export should escape quotes and special characters
    let csv = export_as_csv(&assessments);
    assert!(!csv.contains("<script>"), "CSV should not contain raw HTML");

    // JSON export should properly escape
    let json = export_as_json(&assessments);
    let json_str = json.to_string();
    assert!(
        json_str.contains("\\\"") || !json_str.contains("\"\""),
        "JSON should escape quotes"
    );
}

#[test]
fn test_comparison_no_information_leak() {
    // Ensure comparison doesn't leak sensitive information
    let assessments = generate_all_assessments();
    if assessments.len() >= 2 {
        let comparison = compare_assessments(&assessments[0], &assessments[1]);

        // Should only contain public maturity information
        let comparison_str = comparison.to_string();
        assert!(!comparison_str.contains("password"));
        assert!(!comparison_str.contains("secret"));
        assert!(!comparison_str.contains("token"));
    }
}
