//! Compliance checking for ggen code quality standards
//!
//! This module validates that code follows ggen's production-ready standards:
//! - No unwrap/expect in production code
//! - Atom exhaustion prevention patterns
//! - SPARQL injection prevention
//! - Proper error handling with Result<T,E>

use ggen_utils::error::{Context, Result};
use regex::Regex;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::{Path, PathBuf};
use walkdir::WalkDir;

/// Compliance violation severity
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Serialize, Deserialize)]
pub enum ViolationSeverity {
    /// Critical - must be fixed immediately
    Critical,
    /// High - should be fixed before release
    High,
    /// Medium - should be fixed soon
    Medium,
    /// Low - can be fixed later
    Low,
}

/// Type of compliance violation
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub enum ViolationType {
    /// unwrap() or expect() in production code
    UnwrapInProduction,
    /// Missing error handling (no Result<T,E>)
    MissingErrorHandling,
    /// Potential SPARQL injection vulnerability
    SparqlInjection,
    /// Atom exhaustion risk (Erlang-specific)
    AtomExhaustion,
    /// Panic in production code
    PanicInProduction,
    /// Unvalidated user input
    UnvalidatedInput,
}

impl ViolationType {
    /// Get default severity for violation type
    fn default_severity(&self) -> ViolationSeverity {
        match self {
            Self::UnwrapInProduction => ViolationSeverity::High,
            Self::MissingErrorHandling => ViolationSeverity::Medium,
            Self::SparqlInjection => ViolationSeverity::Critical,
            Self::AtomExhaustion => ViolationSeverity::High,
            Self::PanicInProduction => ViolationSeverity::Critical,
            Self::UnvalidatedInput => ViolationSeverity::High,
        }
    }

    /// Get description for violation type
    fn description(&self) -> &'static str {
        match self {
            Self::UnwrapInProduction => {
                "unwrap() or expect() found in production code - use Result<T,E> instead"
            }
            Self::MissingErrorHandling => {
                "Function should return Result<T,E> for proper error handling"
            }
            Self::SparqlInjection => {
                "Potential SPARQL injection - use parameterized queries"
            }
            Self::AtomExhaustion => {
                "Potential atom exhaustion - use string_to_existing_atom or limit atom creation"
            }
            Self::PanicInProduction => "panic!() found in production code - use Result<T,E> instead",
            Self::UnvalidatedInput => "User input should be validated before use",
        }
    }
}

/// A compliance violation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Violation {
    /// Type of violation
    pub violation_type: ViolationType,
    /// Severity level
    pub severity: ViolationSeverity,
    /// File where violation was found
    pub file: PathBuf,
    /// Line number (1-indexed)
    pub line: usize,
    /// Column number (1-indexed)
    pub column: usize,
    /// Code snippet showing the violation
    pub snippet: String,
    /// Suggested fix
    pub suggestion: Option<String>,
}

/// Configuration for compliance checking
#[derive(Debug, Clone)]
pub struct ComplianceConfig {
    /// Root directory to scan
    pub root_dir: PathBuf,
    /// File patterns to include (glob patterns)
    pub include_patterns: Vec<String>,
    /// File patterns to exclude
    pub exclude_patterns: Vec<String>,
    /// Check for unwrap/expect violations
    pub check_unwrap: bool,
    /// Check for SPARQL injection
    pub check_sparql_injection: bool,
    /// Check for atom exhaustion (Erlang)
    pub check_atom_exhaustion: bool,
}

impl Default for ComplianceConfig {
    fn default() -> Self {
        Self {
            root_dir: PathBuf::from("."),
            include_patterns: vec!["**/*.rs".to_string()],
            exclude_patterns: vec![
                "**/target/**".to_string(),
                "**/tests/**".to_string(),
                "**/*_test.rs".to_string(),
                "**/*_tests.rs".to_string(),
            ],
            check_unwrap: true,
            check_sparql_injection: true,
            check_atom_exhaustion: false, // Erlang-specific
        }
    }
}

/// Compliance check results
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ComplianceResults {
    /// All violations found
    pub violations: Vec<Violation>,
    /// Violations by file
    pub violations_by_file: HashMap<PathBuf, Vec<Violation>>,
    /// Count by severity
    pub critical_count: usize,
    pub high_count: usize,
    pub medium_count: usize,
    pub low_count: usize,
}

impl ComplianceResults {
    /// Create empty compliance results
    fn new() -> Self {
        Self {
            violations: Vec::new(),
            violations_by_file: HashMap::new(),
            critical_count: 0,
            high_count: 0,
            medium_count: 0,
            low_count: 0,
        }
    }

    /// Add a violation to results
    fn add_violation(&mut self, violation: Violation) {
        // Update counts by severity
        match violation.severity {
            ViolationSeverity::Critical => self.critical_count += 1,
            ViolationSeverity::High => self.high_count += 1,
            ViolationSeverity::Medium => self.medium_count += 1,
            ViolationSeverity::Low => self.low_count += 1,
        }

        // Add to file-specific list
        self.violations_by_file
            .entry(violation.file.clone())
            .or_insert_with(Vec::new)
            .push(violation.clone());

        // Add to global list
        self.violations.push(violation);
    }

    /// Check if there are any critical violations
    pub fn has_critical_violations(&self) -> bool {
        self.critical_count > 0
    }

    /// Get total violation count
    pub fn total_count(&self) -> usize {
        self.violations.len()
    }
}

/// Compliance checker
pub struct ComplianceChecker {
    config: ComplianceConfig,
    // Compiled regex patterns for performance
    unwrap_pattern: Regex,
    expect_pattern: Regex,
    panic_pattern: Regex,
    sparql_concat_pattern: Regex,
}

impl ComplianceChecker {
    /// Create a new compliance checker
    pub fn new(config: ComplianceConfig) -> Result<Self> {
        Ok(Self {
            config,
            unwrap_pattern: Regex::new(r"\.unwrap\(\)")
                .context("Failed to compile unwrap pattern")?,
            expect_pattern: Regex::new(r"\.expect\(")
                .context("Failed to compile expect pattern")?,
            panic_pattern: Regex::new(r"\bpanic!\(")
                .context("Failed to compile panic pattern")?,
            sparql_concat_pattern: Regex::new(r#"format!\([^)]*SELECT[^)]*\)"#)
                .context("Failed to compile SPARQL concat pattern")?,
        })
    }

    /// Run comprehensive compliance check
    pub fn check(&self) -> Result<ComplianceResults> {
        let mut results = ComplianceResults::new();

        // Find all files to check
        let files = self.find_files_to_check()?;

        // Check each file
        for file in files {
            let file_violations = self.check_file(&file)?;
            for violation in file_violations {
                results.add_violation(violation);
            }
        }

        Ok(results)
    }

    /// Find all files to check based on config
    fn find_files_to_check(&self) -> Result<Vec<PathBuf>> {
        let mut files = Vec::new();

        for entry in WalkDir::new(&self.config.root_dir)
            .follow_links(true)
            .into_iter()
            .filter_entry(|e| !self.is_excluded(e.path()))
        {
            let entry = entry.context("Failed to read directory entry")?;
            if entry.file_type().is_file() && self.should_check_file(entry.path()) {
                files.push(entry.path().to_path_buf());
            }
        }

        Ok(files)
    }

    /// Check if path should be excluded
    fn is_excluded(&self, path: &Path) -> bool {
        let path_str = path.to_string_lossy();
        self.config
            .exclude_patterns
            .iter()
            .any(|pattern| path_str.contains(pattern.trim_start_matches("**/")))
    }

    /// Check if file should be checked based on include patterns
    fn should_check_file(&self, path: &Path) -> bool {
        let path_str = path.to_string_lossy();
        self.config
            .include_patterns
            .iter()
            .any(|pattern| path_str.ends_with(pattern.trim_start_matches("**/")))
    }

    /// Check a single file for violations
    fn check_file(&self, file: &Path) -> Result<Vec<Violation>> {
        let content = fs::read_to_string(file)
            .with_context(|| format!("Failed to read file: {}", file.display()))?;

        let mut violations = Vec::new();

        // Check if this is a test file (allow unwrap in tests)
        let is_test_file = self.is_test_file(&content);

        // Check each line
        for (line_num, line) in content.lines().enumerate() {
            let line_number = line_num + 1;

            // Skip comments
            if line.trim_start().starts_with("//") {
                continue;
            }

            // Check for unwrap violations (not in test code)
            if self.config.check_unwrap && !is_test_file {
                if let Some(column) = self.find_unwrap_violation(line) {
                    violations.push(Violation {
                        violation_type: ViolationType::UnwrapInProduction,
                        severity: ViolationSeverity::High,
                        file: file.to_path_buf(),
                        line: line_number,
                        column,
                        snippet: line.trim().to_string(),
                        suggestion: Some(
                            "Use Result<T,E> and proper error handling instead of unwrap()"
                                .to_string(),
                        ),
                    });
                }

                if let Some(column) = self.find_expect_violation(line) {
                    violations.push(Violation {
                        violation_type: ViolationType::UnwrapInProduction,
                        severity: ViolationSeverity::High,
                        file: file.to_path_buf(),
                        line: line_number,
                        column,
                        snippet: line.trim().to_string(),
                        suggestion: Some(
                            "Use Result<T,E> and proper error handling instead of expect()"
                                .to_string(),
                        ),
                    });
                }

                if let Some(column) = self.find_panic_violation(line) {
                    violations.push(Violation {
                        violation_type: ViolationType::PanicInProduction,
                        severity: ViolationSeverity::Critical,
                        file: file.to_path_buf(),
                        line: line_number,
                        column,
                        snippet: line.trim().to_string(),
                        suggestion: Some(
                            "Use Result<T,E> and return errors instead of panic!()".to_string(),
                        ),
                    });
                }
            }

            // Check for SPARQL injection
            if self.config.check_sparql_injection {
                if let Some(column) = self.find_sparql_injection(line) {
                    violations.push(Violation {
                        violation_type: ViolationType::SparqlInjection,
                        severity: ViolationSeverity::Critical,
                        file: file.to_path_buf(),
                        line: line_number,
                        column,
                        snippet: line.trim().to_string(),
                        suggestion: Some(
                            "Use parameterized SPARQL queries instead of string concatenation"
                                .to_string(),
                        ),
                    });
                }
            }
        }

        Ok(violations)
    }

    /// Check if content is from a test file
    fn is_test_file(&self, content: &str) -> bool {
        content.contains("#[cfg(test)]") || content.contains("#[test]")
    }

    /// Find unwrap violation in line
    fn find_unwrap_violation(&self, line: &str) -> Option<usize> {
        self.unwrap_pattern
            .find(line)
            .map(|m| m.start() + 1) // 1-indexed
    }

    /// Find expect violation in line
    fn find_expect_violation(&self, line: &str) -> Option<usize> {
        self.expect_pattern
            .find(line)
            .map(|m| m.start() + 1) // 1-indexed
    }

    /// Find panic violation in line
    fn find_panic_violation(&self, line: &str) -> Option<usize> {
        self.panic_pattern
            .find(line)
            .map(|m| m.start() + 1) // 1-indexed
    }

    /// Find SPARQL injection vulnerability
    fn find_sparql_injection(&self, line: &str) -> Option<usize> {
        // Look for string concatenation with SPARQL keywords
        if line.contains("SELECT") || line.contains("INSERT") || line.contains("DELETE") {
            if self.sparql_concat_pattern.is_match(line) {
                return Some(1); // Return start of line
            }
        }
        None
    }

    /// Generate compliance report as markdown
    pub fn generate_report(&self, results: &ComplianceResults) -> String {
        let mut report = String::new();

        report.push_str("# Compliance Check Report\n\n");
        report.push_str(&format!("Total violations: {}\n\n", results.total_count()));
        report.push_str("## Summary\n\n");
        report.push_str(&format!("- Critical: {}\n", results.critical_count));
        report.push_str(&format!("- High: {}\n", results.high_count));
        report.push_str(&format!("- Medium: {}\n", results.medium_count));
        report.push_str(&format!("- Low: {}\n\n", results.low_count));

        report.push_str("## Violations by File\n\n");
        for (file, violations) in &results.violations_by_file {
            report.push_str(&format!("### {}\n\n", file.display()));
            for violation in violations {
                report.push_str(&format!(
                    "- Line {}: {:?} ({:?})\n",
                    violation.line,
                    violation.violation_type,
                    violation.severity
                ));
                report.push_str(&format!("  ```\n  {}\n  ```\n", violation.snippet));
                if let Some(suggestion) = &violation.suggestion {
                    report.push_str(&format!("  Suggestion: {}\n", suggestion));
                }
                report.push('\n');
            }
        }

        report
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_violation_type_severity() {
        // Arrange: Create violation types
        let sparql = ViolationType::SparqlInjection;
        let unwrap = ViolationType::UnwrapInProduction;
        let missing_error = ViolationType::MissingErrorHandling;

        // Act & Assert: Verify default severities
        assert_eq!(sparql.default_severity(), ViolationSeverity::Critical);
        assert_eq!(unwrap.default_severity(), ViolationSeverity::High);
        assert_eq!(
            missing_error.default_severity(),
            ViolationSeverity::Medium
        );
    }

    #[test]
    fn test_compliance_results_add_violation() {
        // Arrange: Create results and violation
        let mut results = ComplianceResults::new();
        let violation = Violation {
            violation_type: ViolationType::UnwrapInProduction,
            severity: ViolationSeverity::High,
            file: PathBuf::from("src/main.rs"),
            line: 42,
            column: 10,
            snippet: "result.unwrap()".to_string(),
            suggestion: Some("Use ? operator".to_string()),
        };

        // Act: Add violation
        results.add_violation(violation);

        // Assert: Verify counts and storage
        assert_eq!(results.high_count, 1);
        assert_eq!(results.total_count(), 1);
        assert!(!results.has_critical_violations());
        assert_eq!(results.violations_by_file.len(), 1);
    }

    #[test]
    fn test_compliance_results_multiple_files() {
        // Arrange: Create results
        let mut results = ComplianceResults::new();

        // Act: Add violations from different files
        results.add_violation(Violation {
            violation_type: ViolationType::PanicInProduction,
            severity: ViolationSeverity::Critical,
            file: PathBuf::from("src/foo.rs"),
            line: 10,
            column: 5,
            snippet: "panic!(\"error\")".to_string(),
            suggestion: None,
        });

        results.add_violation(Violation {
            violation_type: ViolationType::UnwrapInProduction,
            severity: ViolationSeverity::High,
            file: PathBuf::from("src/bar.rs"),
            line: 20,
            column: 8,
            snippet: "x.unwrap()".to_string(),
            suggestion: None,
        });

        // Assert: Verify results
        assert_eq!(results.total_count(), 2);
        assert_eq!(results.critical_count, 1);
        assert_eq!(results.high_count, 1);
        assert!(results.has_critical_violations());
        assert_eq!(results.violations_by_file.len(), 2);
    }

    #[test]
    fn test_unwrap_detection() {
        // Arrange: Create checker
        let config = ComplianceConfig::default();
        let checker = ComplianceChecker::new(config).unwrap();

        // Act: Check for unwrap in line
        let line = "let value = result.unwrap();";
        let column = checker.find_unwrap_violation(line);

        // Assert: Verify unwrap detected
        assert!(column.is_some());
    }

    #[test]
    fn test_expect_detection() {
        // Arrange: Create checker
        let config = ComplianceConfig::default();
        let checker = ComplianceChecker::new(config).unwrap();

        // Act: Check for expect in line
        let line = "let value = result.expect(\"should work\");";
        let column = checker.find_expect_violation(line);

        // Assert: Verify expect detected
        assert!(column.is_some());
    }

    #[test]
    fn test_panic_detection() {
        // Arrange: Create checker
        let config = ComplianceConfig::default();
        let checker = ComplianceChecker::new(config).unwrap();

        // Act: Check for panic in line
        let line = "panic!(\"This should not happen\");";
        let column = checker.find_panic_violation(line);

        // Assert: Verify panic detected
        assert!(column.is_some());
    }

    #[test]
    fn test_test_file_detection() {
        // Arrange: Create checker
        let config = ComplianceConfig::default();
        let checker = ComplianceChecker::new(config).unwrap();

        // Act: Check test file detection
        let test_content = "#[cfg(test)]\nmod tests {\n    #[test]\n    fn test_foo() {}\n}";
        let is_test = checker.is_test_file(test_content);

        // Assert: Verify test file detected
        assert!(is_test);
    }

    #[test]
    fn test_production_file_detection() {
        // Arrange: Create checker
        let config = ComplianceConfig::default();
        let checker = ComplianceChecker::new(config).unwrap();

        // Act: Check production file detection
        let prod_content = "pub fn process() -> Result<()> {\n    Ok(())\n}";
        let is_test = checker.is_test_file(prod_content);

        // Assert: Verify production file detected
        assert!(!is_test);
    }
}
