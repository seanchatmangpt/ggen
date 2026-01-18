//! False positive detection for test suite quality
//!
//! Identifies tests that pass but don't properly validate behavior:
//! - Execution-only tests (just check code runs, not correctness)
//! - Missing critical path coverage (ggen.toml, RDF parsing, etc.)
//! - Weak assertions that don't catch bugs

use crate::assertion_analyzer::TestAssertion;
use crate::types::{AssertionStrength, AuditResult, TestId};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use walkdir::WalkDir;

/// False positive detector for test quality analysis
#[derive(Debug)]
pub struct FalsePositiveDetector {
    /// Test directories to analyze
    test_dirs: Vec<PathBuf>,
    /// Critical paths that MUST have strong tests
    critical_paths: Vec<CriticalPath>,
}

impl FalsePositiveDetector {
    /// Create a new false positive detector
    ///
    /// # Arguments
    /// * `test_dirs` - Directories containing test files
    pub fn new(test_dirs: Vec<PathBuf>) -> Self {
        let mut detector = Self {
            test_dirs,
            critical_paths: Vec::new(),
        };

        // Add ggen critical paths (Feature 004 requirement)
        detector.add_ggen_critical_paths();
        detector
    }

    /// Add ggen-specific critical paths
    ///
    /// These paths MUST have strong assertion coverage:
    /// - RDF parsing and validation
    /// - Ontology projection
    /// - Code generation
    /// - Configuration (ggen.toml) handling
    fn add_ggen_critical_paths(&mut self) {
        self.critical_paths = vec![
            CriticalPath {
                name: "RDF Parsing".to_string(),
                code_paths: vec!["crates/ggen-rdf/src/parser.rs".into()],
                test_patterns: vec!["rdf_parser", "parse_triple", "parse_rdf"],
                required_strength: AssertionStrength::Strong,
            },
            CriticalPath {
                name: "Ontology Projection".to_string(),
                code_paths: vec!["crates/ggen-ontology/src/projection.rs".into()],
                test_patterns: vec!["ontology", "project", "transform"],
                required_strength: AssertionStrength::Strong,
            },
            CriticalPath {
                name: "Code Generation".to_string(),
                code_paths: vec!["crates/ggen-core/src/generator.rs".into()],
                test_patterns: vec!["generate", "codegen", "emit"],
                required_strength: AssertionStrength::Strong,
            },
            CriticalPath {
                name: "ggen.toml Configuration".to_string(),
                code_paths: vec!["crates/ggen-config/src/toml.rs".into()],
                test_patterns: vec!["ggen_toml", "config", "parse_toml"],
                required_strength: AssertionStrength::Strong,
            },
        ];
    }

    /// Detect execution-only tests (no state assertions)
    ///
    /// Finds tests that only check code executes (is_ok, is_some) without validating correctness.
    ///
    /// # Arguments
    /// * `assertions` - Test assertion analysis results
    #[must_use]
    pub fn detect_execution_only_tests(&self, assertions: &[TestAssertion]) -> Vec<FalsePositive> {
        assertions
            .iter()
            .filter(|a| matches!(a.assertion_strength, AssertionStrength::Weak))
            .map(|a| FalsePositive {
                test_id: a.test_id.clone(),
                file_path: a.file_path.clone(),
                reason: FalsePositiveReason::ExecutionOnly,
                severity: Severity::Warning,
                recommendation: format!(
                    "Replace weak assertions (is_ok, is_some) with value assertions (assert_eq!) that verify actual behavior in {}",
                    a.test_id.as_str()
                ),
            })
            .collect()
    }

    /// Analyze ggen.toml tests specifically
    ///
    /// The critical bug: ggen.toml is broken but tests pass. This finds why.
    ///
    /// # Errors
    /// Returns `AuditError::IoError` if test files cannot be read
    pub fn analyze_ggen_toml_tests(&self) -> AuditResult<Vec<FalsePositive>> {
        let mut false_positives = Vec::new();

        for test_dir in &self.test_dirs {
            for entry in WalkDir::new(test_dir)
                .into_iter()
                .filter_map(Result::ok)
                .filter(|e| e.path().extension().and_then(|s| s.to_str()) == Some("rs"))
            {
                let content = std::fs::read_to_string(entry.path())?;

                // Find ggen.toml related tests
                if content.contains("ggen.toml")
                    || content.contains("config")
                    || content.contains("toml")
                {
                    // Check if tests validate TOML parsing or just execution
                    if self.has_only_execution_checks(&content) {
                        let test_id =
                            TestId::new(format!("ggen_toml_test_{}", entry.path().display()))?;

                        false_positives.push(FalsePositive {
                            test_id,
                            file_path: entry.path().to_path_buf(),
                            reason: FalsePositiveReason::GgenTomlNotValidated,
                            severity: Severity::Critical,
                            recommendation: "Add assertions that verify parsed TOML values (assert_eq!(config.field, expected_value)) instead of just checking is_ok()".to_string(),
                        });
                    }
                }
            }
        }

        Ok(false_positives)
    }

    /// Check if test content has only execution checks (is_ok, is_some)
    fn has_only_execution_checks(&self, content: &str) -> bool {
        let has_weak = content.contains("is_ok()") || content.contains("is_some()");
        let has_strong = content.contains("assert_eq!") || content.contains("assert_ne!");

        has_weak && !has_strong
    }

    /// Identify critical path coverage gaps
    ///
    /// Checks if critical code paths have sufficient test coverage with strong assertions.
    ///
    /// # Arguments
    /// * `assertions` - Test assertion analysis results
    #[must_use]
    pub fn identify_critical_path_gaps(
        &self, assertions: &[TestAssertion],
    ) -> Vec<CriticalPathGap> {
        let mut gaps = Vec::new();

        for critical_path in &self.critical_paths {
            // Find tests covering this critical path
            let covering_tests: Vec<&TestAssertion> = assertions
                .iter()
                .filter(|a| {
                    let test_name = a.test_id.as_str().to_lowercase();
                    critical_path
                        .test_patterns
                        .iter()
                        .any(|pattern| test_name.contains(pattern))
                })
                .collect();

            // Check if any tests have strong assertions
            let has_strong = covering_tests
                .iter()
                .any(|t| matches!(t.assertion_strength, AssertionStrength::Strong));

            if !has_strong {
                gaps.push(CriticalPathGap {
                    critical_path_name: critical_path.name.clone(),
                    code_paths: critical_path.code_paths.clone(),
                    weak_test_count: covering_tests.len(),
                    required_strength: critical_path.required_strength,
                    recommendation: format!(
                        "Add strong assertions (assert_eq! with expected values) to {} tests",
                        critical_path.name
                    ),
                });
            }
        }

        gaps
    }

    /// Generate comprehensive false positive report
    ///
    /// Combines all detection methods into a single report.
    ///
    /// # Arguments
    /// * `assertions` - Test assertion analysis results
    ///
    /// # Errors
    /// Returns `AuditError` if analysis fails
    pub fn generate_report(
        &self, assertions: &[TestAssertion],
    ) -> AuditResult<FalsePositiveReport> {
        let execution_only = self.detect_execution_only_tests(assertions);
        let ggen_toml_issues = self.analyze_ggen_toml_tests()?;
        let critical_gaps = self.identify_critical_path_gaps(assertions);

        Ok(FalsePositiveReport {
            total_tests_analyzed: assertions.len(),
            execution_only_tests: execution_only,
            ggen_toml_issues,
            critical_path_gaps: critical_gaps,
            overall_severity: self.calculate_overall_severity(assertions),
        })
    }

    /// Calculate overall severity based on assertion distribution
    fn calculate_overall_severity(&self, assertions: &[TestAssertion]) -> Severity {
        let weak_count = assertions
            .iter()
            .filter(|a| matches!(a.assertion_strength, AssertionStrength::Weak))
            .count();

        let weak_percentage = weak_count as f64 / assertions.len() as f64;

        if weak_percentage > 0.5 {
            Severity::Critical
        } else if weak_percentage > 0.25 {
            Severity::Error
        } else {
            Severity::Warning
        }
    }
}

/// Critical code path requiring strong test coverage
#[derive(Debug, Clone)]
pub struct CriticalPath {
    /// Human-readable name
    pub name: String,
    /// Code file paths in this critical path
    pub code_paths: Vec<PathBuf>,
    /// Test name patterns that cover this path
    pub test_patterns: Vec<&'static str>,
    /// Minimum required assertion strength
    pub required_strength: AssertionStrength,
}

/// False positive test detection result
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct FalsePositive {
    /// Test identifier
    pub test_id: TestId,
    /// Test file path
    pub file_path: PathBuf,
    /// Reason for false positive classification
    pub reason: FalsePositiveReason,
    /// Severity level
    pub severity: Severity,
    /// Fix recommendation
    pub recommendation: String,
}

/// Reason a test is classified as false positive
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum FalsePositiveReason {
    /// Test only checks execution (is_ok, is_some) without validating values
    ExecutionOnly,
    /// ggen.toml test doesn't validate parsed TOML values
    GgenTomlNotValidated,
    /// Critical path lacks strong assertion coverage
    CriticalPathGap,
}

/// Severity classification
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum Severity {
    /// Low impact
    Warning,
    /// Medium impact
    Error,
    /// High impact (e.g., ggen.toml broken but tests pass)
    Critical,
}

/// Critical path coverage gap
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CriticalPathGap {
    /// Critical path name
    pub critical_path_name: String,
    /// Code paths with insufficient coverage
    pub code_paths: Vec<PathBuf>,
    /// Number of weak tests covering this path
    pub weak_test_count: usize,
    /// Required assertion strength
    pub required_strength: AssertionStrength,
    /// Fix recommendation
    pub recommendation: String,
}

/// Comprehensive false positive analysis report
#[derive(Debug, Serialize, Deserialize)]
pub struct FalsePositiveReport {
    /// Total tests analyzed
    pub total_tests_analyzed: usize,
    /// Execution-only tests (weak assertions)
    pub execution_only_tests: Vec<FalsePositive>,
    /// ggen.toml specific issues
    pub ggen_toml_issues: Vec<FalsePositive>,
    /// Critical path coverage gaps
    pub critical_path_gaps: Vec<CriticalPathGap>,
    /// Overall severity assessment
    pub overall_severity: Severity,
}
