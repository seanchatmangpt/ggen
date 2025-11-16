//! Helper functions for marketplace assessment and filtering
//!
//! This module provides utilities for generating, filtering, comparing, and
//! exporting maturity assessments based on package data.

use crate::maturity::{MaturityAssessment, MaturityLevel};
use crate::maturity_evaluator::{EvaluationInput, MaturityEvaluator};
use serde_json::json;

/// Sample packages for demonstration (would come from registry in production)
pub fn sample_packages() -> Vec<EvaluationInput> {
    vec![
        EvaluationInput {
            package_id: "io.ggen.research-compiler".to_string(),
            package_name: "Research Compiler".to_string(),
            downloads: 5234,
            rating: 4.7,
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
            has_optimization_docs: true,
            determinism_verified: true,
            days_since_last_release: 15,
            active_contributors: 3,
            avg_issue_response_hours: 18.0,
            academic_citations: 8,
        },
        EvaluationInput {
            package_id: "io.ggen.data-processor".to_string(),
            package_name: "Data Processor".to_string(),
            downloads: 3421,
            rating: 4.5,
            has_readme: true,
            has_api_docs: true,
            has_examples: true,
            has_changelog: true,
            test_coverage: 82.0,
            has_unit_tests: true,
            has_integration_tests: true,
            has_e2e_tests: false,
            vulnerabilities: 0,
            has_dependency_audit: true,
            unsafe_code_percent: 1.5,
            has_benchmarks: true,
            has_optimization_docs: true,
            determinism_verified: true,
            days_since_last_release: 22,
            active_contributors: 2,
            avg_issue_response_hours: 24.0,
            academic_citations: 5,
        },
        EvaluationInput {
            package_id: "io.ggen.lightweight-parser".to_string(),
            package_name: "Lightweight Parser".to_string(),
            downloads: 892,
            rating: 4.2,
            has_readme: true,
            has_api_docs: true,
            has_examples: true,
            has_changelog: false,
            test_coverage: 65.0,
            has_unit_tests: true,
            has_integration_tests: true,
            has_e2e_tests: false,
            vulnerabilities: 1,
            has_dependency_audit: false,
            unsafe_code_percent: 3.2,
            has_benchmarks: false,
            has_optimization_docs: false,
            determinism_verified: false,
            days_since_last_release: 45,
            active_contributors: 1,
            avg_issue_response_hours: 72.0,
            academic_citations: 2,
        },
        EvaluationInput {
            package_id: "io.ggen.simple-compiler".to_string(),
            package_name: "Simple Compiler".to_string(),
            downloads: 156,
            rating: 3.8,
            has_readme: true,
            has_api_docs: false,
            has_examples: true,
            has_changelog: false,
            test_coverage: 45.0,
            has_unit_tests: true,
            has_integration_tests: false,
            has_e2e_tests: false,
            vulnerabilities: 2,
            has_dependency_audit: false,
            unsafe_code_percent: 5.1,
            has_benchmarks: false,
            has_optimization_docs: false,
            determinism_verified: false,
            days_since_last_release: 120,
            active_contributors: 1,
            avg_issue_response_hours: 0.0,
            academic_citations: 0,
        },
        EvaluationInput {
            package_id: "io.ggen.experimental-ai".to_string(),
            package_name: "Experimental AI Tools".to_string(),
            downloads: 234,
            rating: 3.5,
            has_readme: true,
            has_api_docs: false,
            has_examples: false,
            has_changelog: false,
            test_coverage: 30.0,
            has_unit_tests: true,
            has_integration_tests: false,
            has_e2e_tests: false,
            vulnerabilities: 3,
            has_dependency_audit: false,
            unsafe_code_percent: 8.5,
            has_benchmarks: false,
            has_optimization_docs: false,
            determinism_verified: false,
            days_since_last_release: 180,
            active_contributors: 1,
            avg_issue_response_hours: 0.0,
            academic_citations: 0,
        },
    ]
}

/// Generate assessments for all sample packages
pub fn generate_all_assessments() -> Vec<MaturityAssessment> {
    sample_packages()
        .into_iter()
        .map(|input| MaturityEvaluator::evaluate(input))
        .collect()
}

/// Filter assessments by maturity level
pub fn filter_by_level(
    assessments: &[MaturityAssessment],
    level: MaturityLevel,
) -> Vec<&MaturityAssessment> {
    assessments
        .iter()
        .filter(|a| a.level() == level)
        .collect()
}

/// Filter assessments by score range
pub fn filter_by_score_range(
    assessments: &[MaturityAssessment],
    min_score: u32,
    max_score: u32,
) -> Vec<&MaturityAssessment> {
    assessments
        .iter()
        .filter(|a| {
            let score = a.total_score();
            score >= min_score && score <= max_score
        })
        .collect()
}

/// Filter assessments by dimension scores
#[allow(clippy::too_many_arguments)]
pub fn filter_by_dimensions(
    assessments: &[MaturityAssessment],
    min_documentation: Option<u32>,
    min_testing: Option<u32>,
    min_security: Option<u32>,
    min_performance: Option<u32>,
    min_adoption: Option<u32>,
    min_maintenance: Option<u32>,
) -> Vec<&MaturityAssessment> {
    assessments
        .iter()
        .filter(|a| {
            let checks = vec![
                min_documentation.map(|m| a.documentation.total() >= m),
                min_testing.map(|m| a.testing.total() >= m),
                min_security.map(|m| a.security.total() >= m),
                min_performance.map(|m| a.performance.total() >= m),
                min_adoption.map(|m| a.adoption.total() >= m),
                min_maintenance.map(|m| a.maintenance.total() >= m),
            ];

            // All specified checks must pass
            checks.into_iter().all(|check| check.unwrap_or(true))
        })
        .collect()
}

/// Get recommendations for a use case
pub fn get_recommendations(
    use_case: &str,
    priority: Option<&str>,
    min_score: Option<u32>,
) -> Vec<String> {
    let default_score = match use_case {
        "production" => 65,
        "research" => 40,
        "enterprise" => 85,
        "startup" => 50,
        _ => 60,
    };

    let min = min_score.unwrap_or(default_score);

    // In production, would query based on these criteria
    // For now, return guidance
    let mut guidance = vec!["Start by searching packages meeting your criteria".to_string()];

    if let Some(pri) = priority {
        match pri {
            "security" => guidance.push("Focus on packages with security score >= 18".to_string()),
            "testing" => guidance.push("Focus on packages with testing score >= 16".to_string()),
            "documentation" => guidance.push("Focus on packages with documentation score >= 15".to_string()),
            "performance" => guidance.push("Focus on packages with performance score >= 12".to_string()),
            "adoption" => guidance.push("Focus on packages with adoption score >= 12".to_string()),
            "maintenance" => guidance.push("Focus on packages with maintenance score >= 5".to_string()),
            _ => {}
        }
    }

    guidance.push(format!("Minimum score threshold: {}", min));

    guidance
}

/// Compare two assessments and return detailed comparison
pub fn compare_assessments(
    a: &MaturityAssessment,
    b: &MaturityAssessment,
) -> serde_json::Value {
    let a_score = a.total_score();
    let b_score = b.total_score();
    let winner = if a_score > b_score {
        Some(&a.package_id)
    } else if b_score > a_score {
        Some(&b.package_id)
    } else {
        None
    };

    json!({
        "package_a": {
            "id": a.package_id,
            "name": a.package_name,
            "score": a_score,
            "level": format!("{:?}", a.level()),
        },
        "package_b": {
            "id": b.package_id,
            "name": b.package_name,
            "score": b_score,
            "level": format!("{:?}", b.level()),
        },
        "comparison": {
            "winner": winner.map(|w| w.clone()),
            "score_difference": (a_score as i32 - b_score as i32).abs(),
        }
    })
}

/// Export assessments to JSON format
pub fn export_as_json(assessments: &[MaturityAssessment]) -> serde_json::Value {
    let reports: Vec<_> = assessments.iter().map(|a| a.to_report()).collect();

    json!({
        "export_timestamp": chrono::Utc::now().to_rfc3339(),
        "total_packages": assessments.len(),
        "assessments": reports,
    })
}

/// Export assessments to CSV format
pub fn export_as_csv(assessments: &[MaturityAssessment]) -> String {
    let mut csv = String::from(
        "id,name,total_score,level,documentation,testing,security,performance,adoption,maintenance\n",
    );

    for a in assessments {
        csv.push_str(&format!(
            "{},{},{},{},{},{},{},{},{},{}\n",
            a.package_id,
            a.package_name,
            a.total_score(),
            format!("{:?}", a.level()),
            a.documentation.total(),
            a.testing.total(),
            a.security.total(),
            a.performance.total(),
            a.adoption.total(),
            a.maintenance.total(),
        ));
    }

    csv
}

/// Find packages matching use case criteria
pub fn find_for_use_case<'a>(
    assessments: &'a [MaturityAssessment],
    use_case: &str,
) -> Vec<&'a MaturityAssessment> {
    let (min_level, min_score) = match use_case {
        "production" => (MaturityLevel::Production, 61),
        "research" => (MaturityLevel::Beta, 40),
        "enterprise" => (MaturityLevel::Enterprise, 81),
        "startup" => (MaturityLevel::Beta, 50),
        _ => (MaturityLevel::Production, 60),
    };

    assessments
        .iter()
        .filter(|a| a.level() >= min_level && a.total_score() >= min_score)
        .collect()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_sample_packages() {
        let pkgs = sample_packages();
        assert_eq!(pkgs.len(), 5);
        assert_eq!(pkgs[0].package_id, "io.ggen.research-compiler");
    }

    #[test]
    fn test_generate_assessments() {
        let assessments = generate_all_assessments();
        assert_eq!(assessments.len(), 5);
        assert!(assessments[0].total_score() > 50);
    }

    #[test]
    fn test_filter_by_level() {
        let assessments = generate_all_assessments();
        let production = filter_by_level(&assessments, MaturityLevel::Production);
        assert!(!production.is_empty());
    }
}
