//! Test quality report generation
//!
//! Combines mutation testing, assertion analysis, and false positive detection
//! into comprehensive quality reports (JSON and Markdown formats).

use crate::assertion_analyzer::TestAssertion;
use crate::false_positive_detector::{FalsePositiveReport, Severity};
use crate::mutation_analyzer::MutationAnalyzer;
use crate::types::{AuditResult, MutationResult};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;

/// Test quality report generator
#[derive(Debug)]
pub struct ReportGenerator {
    /// Output directory for reports
    output_dir: PathBuf,
}

impl ReportGenerator {
    /// Create a new report generator
    ///
    /// # Arguments
    /// * `output_dir` - Directory to write reports (e.g., .ggen/test-metadata/)
    pub fn new(output_dir: impl Into<PathBuf>) -> AuditResult<Self> {
        let output_dir = output_dir.into();
        std::fs::create_dir_all(&output_dir)?;

        Ok(Self { output_dir })
    }

    /// Generate comprehensive quality report
    ///
    /// Combines all audit components into single unified report.
    ///
    /// # Arguments
    /// * `mutation_results` - Results from mutation testing
    /// * `assertions` - Assertion strength analysis
    /// * `false_positive_report` - False positive detection results
    #[must_use]
    pub fn generate_quality_report(
        &self, mutation_results: &[MutationResult], assertions: &[TestAssertion],
        false_positive_report: &FalsePositiveReport,
    ) -> QualityReport {
        let analyzer = MutationAnalyzer::new(".", &self.output_dir)
            .unwrap_or_else(|_| panic!("Failed to create mutation analyzer"));

        QualityReport {
            timestamp: Utc::now(),
            mutation_testing: MutationTestingSummary {
                total_mutants: mutation_results.len(),
                killed_mutants: mutation_results
                    .iter()
                    .filter(|r| !r.mutant_survived)
                    .count(),
                survived_mutants: mutation_results
                    .iter()
                    .filter(|r| r.mutant_survived)
                    .count(),
                kill_rate: analyzer.calculate_kill_rate(mutation_results),
                target_kill_rate: 0.80,
                meets_target: analyzer.calculate_kill_rate(mutation_results) >= 0.80,
            },
            assertion_analysis: AssertionAnalysisSummary {
                total_tests: assertions.len(),
                weak_assertions: assertions
                    .iter()
                    .filter(|a| {
                        matches!(a.assertion_strength, crate::types::AssertionStrength::Weak)
                    })
                    .count(),
                medium_assertions: assertions
                    .iter()
                    .filter(|a| {
                        matches!(
                            a.assertion_strength,
                            crate::types::AssertionStrength::Medium
                        )
                    })
                    .count(),
                strong_assertions: assertions
                    .iter()
                    .filter(|a| {
                        matches!(
                            a.assertion_strength,
                            crate::types::AssertionStrength::Strong
                        )
                    })
                    .count(),
            },
            false_positives: FalsePositiveSummary {
                total_false_positives: false_positive_report.execution_only_tests.len()
                    + false_positive_report.ggen_toml_issues.len(),
                execution_only_count: false_positive_report.execution_only_tests.len(),
                ggen_toml_issues_count: false_positive_report.ggen_toml_issues.len(),
                critical_path_gaps_count: false_positive_report.critical_path_gaps.len(),
                overall_severity: false_positive_report.overall_severity,
            },
            recommendations: self.generate_recommendations(
                mutation_results,
                assertions,
                false_positive_report,
            ),
        }
    }

    /// Generate actionable recommendations
    fn generate_recommendations(
        &self, mutation_results: &[MutationResult], assertions: &[TestAssertion],
        false_positive_report: &FalsePositiveReport,
    ) -> Vec<Recommendation> {
        let mut recommendations = Vec::new();

        let analyzer = MutationAnalyzer::new(".", &self.output_dir)
            .unwrap_or_else(|_| panic!("Failed to create mutation analyzer"));

        // Mutation kill rate recommendation
        let kill_rate = analyzer.calculate_kill_rate(mutation_results);
        if kill_rate < 0.80 {
            recommendations.push(Recommendation {
                priority: Priority::High,
                category: "Mutation Testing".to_string(),
                issue: format!("Kill rate {:.1}% below target 80%", kill_rate * 100.0),
                action: "Add stronger assertions to catch surviving mutants".to_string(),
            });
        }

        // Weak assertion recommendation
        let weak_count = assertions
            .iter()
            .filter(|a| matches!(a.assertion_strength, crate::types::AssertionStrength::Weak))
            .count();

        if weak_count > 0 {
            recommendations.push(Recommendation {
                priority: Priority::Medium,
                category: "Assertion Strength".to_string(),
                issue: format!("{} tests have weak assertions (is_ok, is_some)", weak_count),
                action: "Replace with assert_eq! verifying actual values".to_string(),
            });
        }

        // ggen.toml critical fix
        if !false_positive_report.ggen_toml_issues.is_empty() {
            recommendations.push(Recommendation {
                priority: Priority::Critical,
                category: "False Positives".to_string(),
                issue: "ggen.toml tests pass but don't validate parsed values".to_string(),
                action: "Add assert_eq!(config.field, expected) assertions".to_string(),
            });
        }

        // Critical path gaps
        for gap in &false_positive_report.critical_path_gaps {
            recommendations.push(Recommendation {
                priority: Priority::High,
                category: "Critical Path Coverage".to_string(),
                issue: format!("{} lacks strong assertions", gap.critical_path_name),
                action: gap.recommendation.clone(),
            });
        }

        recommendations
    }

    /// Export report to JSON
    ///
    /// # Errors
    /// Returns `AuditError::IoError` if file write fails
    /// Returns `AuditError::JsonError` if serialization fails
    pub fn export_json(&self, report: &QualityReport) -> AuditResult<PathBuf> {
        let path = self.output_dir.join("quality-report.json");
        let json = serde_json::to_string_pretty(report)?;
        std::fs::write(&path, json)?;
        Ok(path)
    }

    /// Export report to Markdown
    ///
    /// # Errors
    /// Returns `AuditError::IoError` if file write fails
    pub fn export_markdown(&self, report: &QualityReport) -> AuditResult<PathBuf> {
        let path = self.output_dir.join("quality-report.md");
        let markdown = self.format_markdown(report);
        std::fs::write(&path, markdown)?;
        Ok(path)
    }

    /// Format report as Markdown
    fn format_markdown(&self, report: &QualityReport) -> String {
        let mut md = String::new();

        md.push_str("# Test Quality Audit Report\n\n");
        md.push_str(&format!("**Generated**: {}\n\n", report.timestamp));

        // Mutation Testing Summary
        md.push_str("## Mutation Testing\n\n");
        md.push_str(&format!(
            "- **Total Mutants**: {}\n",
            report.mutation_testing.total_mutants
        ));
        md.push_str(&format!(
            "- **Killed**: {}\n",
            report.mutation_testing.killed_mutants
        ));
        md.push_str(&format!(
            "- **Survived**: {}\n",
            report.mutation_testing.survived_mutants
        ));
        md.push_str(&format!(
            "- **Kill Rate**: {:.1}% (Target: {:.1}%)\n",
            report.mutation_testing.kill_rate * 100.0,
            report.mutation_testing.target_kill_rate * 100.0
        ));
        md.push_str(&format!(
            "- **Meets Target**: {}\n\n",
            if report.mutation_testing.meets_target {
                "✅ YES"
            } else {
                "❌ NO"
            }
        ));

        // Assertion Analysis
        md.push_str("## Assertion Strength\n\n");
        md.push_str(&format!(
            "- **Total Tests**: {}\n",
            report.assertion_analysis.total_tests
        ));
        md.push_str(&format!(
            "- **Strong Assertions**: {}\n",
            report.assertion_analysis.strong_assertions
        ));
        md.push_str(&format!(
            "- **Medium Assertions**: {}\n",
            report.assertion_analysis.medium_assertions
        ));
        md.push_str(&format!(
            "- **Weak Assertions**: {}\n\n",
            report.assertion_analysis.weak_assertions
        ));

        // False Positives
        md.push_str("## False Positives\n\n");
        md.push_str(&format!(
            "- **Total**: {}\n",
            report.false_positives.total_false_positives
        ));
        md.push_str(&format!(
            "- **Execution-Only Tests**: {}\n",
            report.false_positives.execution_only_count
        ));
        md.push_str(&format!(
            "- **ggen.toml Issues**: {}\n",
            report.false_positives.ggen_toml_issues_count
        ));
        md.push_str(&format!(
            "- **Critical Path Gaps**: {}\n",
            report.false_positives.critical_path_gaps_count
        ));
        md.push_str(&format!(
            "- **Overall Severity**: {:?}\n\n",
            report.false_positives.overall_severity
        ));

        // Recommendations
        md.push_str("## Recommendations\n\n");
        for (i, rec) in report.recommendations.iter().enumerate() {
            md.push_str(&format!(
                "{}. **[{:?}]** {} - {}\n",
                i + 1,
                rec.priority,
                rec.category,
                rec.issue
            ));
            md.push_str(&format!("   - Action: {}\n\n", rec.action));
        }

        md
    }
}

/// Comprehensive test quality report
#[derive(Debug, Serialize, Deserialize)]
pub struct QualityReport {
    /// Report generation timestamp
    pub timestamp: DateTime<Utc>,
    /// Mutation testing summary
    pub mutation_testing: MutationTestingSummary,
    /// Assertion analysis summary
    pub assertion_analysis: AssertionAnalysisSummary,
    /// False positive summary
    pub false_positives: FalsePositiveSummary,
    /// Actionable recommendations
    pub recommendations: Vec<Recommendation>,
}

/// Mutation testing summary
#[derive(Debug, Serialize, Deserialize)]
pub struct MutationTestingSummary {
    /// Total mutants generated
    pub total_mutants: usize,
    /// Mutants killed by tests
    pub killed_mutants: usize,
    /// Mutants that survived
    pub survived_mutants: usize,
    /// Kill rate (0.0 - 1.0)
    pub kill_rate: f64,
    /// Target kill rate (0.80 for Feature 004)
    pub target_kill_rate: f64,
    /// Whether kill rate meets target
    pub meets_target: bool,
}

/// Assertion strength analysis summary
#[derive(Debug, Serialize, Deserialize)]
pub struct AssertionAnalysisSummary {
    /// Total tests analyzed
    pub total_tests: usize,
    /// Tests with weak assertions
    pub weak_assertions: usize,
    /// Tests with medium assertions
    pub medium_assertions: usize,
    /// Tests with strong assertions
    pub strong_assertions: usize,
}

/// False positive detection summary
#[derive(Debug, Serialize, Deserialize)]
pub struct FalsePositiveSummary {
    /// Total false positives detected
    pub total_false_positives: usize,
    /// Execution-only tests
    pub execution_only_count: usize,
    /// ggen.toml specific issues
    pub ggen_toml_issues_count: usize,
    /// Critical path gaps
    pub critical_path_gaps_count: usize,
    /// Overall severity
    pub overall_severity: Severity,
}

/// Actionable recommendation
#[derive(Debug, Serialize, Deserialize)]
pub struct Recommendation {
    /// Priority level
    pub priority: Priority,
    /// Category
    pub category: String,
    /// Issue description
    pub issue: String,
    /// Recommended action
    pub action: String,
}

/// Recommendation priority
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Priority {
    /// Low priority
    Low,
    /// Medium priority
    Medium,
    /// High priority
    High,
    /// Critical priority (blocks release)
    Critical,
}
