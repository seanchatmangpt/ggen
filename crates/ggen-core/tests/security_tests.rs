//! Integration tests for security scanning infrastructure
//!
//! These tests verify:
//! - Vulnerability scanning with cargo audit integration
//! - Compliance checking for code quality standards
//! - SARIF output generation for GitHub Security
//! - Detection of unwrap/expect in production code
//! - SPARQL injection prevention validation

use ggen_core::security::{
    ComplianceChecker, ComplianceConfig, ScanConfig, Violation, ViolationSeverity,
    ViolationType, VulnerabilityScanner,
};
use std::fs;
use std::path::PathBuf;
use tempfile::TempDir;

// ============================================================================
// Helper Functions (Chicago TDD - Real Collaborators)
// ============================================================================

/// Create temporary Rust project with sample code
fn create_sample_project(tmp_dir: &TempDir) -> PathBuf {
    let project_root = tmp_dir.path().to_path_buf();

    // Create Cargo.toml
    let cargo_toml = r#"[package]
name = "test-project"
version = "0.1.0"
edition = "2021"

[dependencies]
"#;
    fs::write(project_root.join("Cargo.toml"), cargo_toml).unwrap();

    // Create src directory
    fs::create_dir_all(project_root.join("src")).unwrap();

    project_root
}

/// Create sample Rust file with violations
fn create_file_with_violations(project_root: &PathBuf, filename: &str, content: &str) {
    let file_path = project_root.join("src").join(filename);
    fs::write(file_path, content).unwrap();
}

// ============================================================================
// Vulnerability Scanner Tests (Chicago TDD)
// ============================================================================

#[test]
fn test_vulnerability_scanner_creation() {
    // Arrange: Create scan configuration
    let config = ScanConfig::default();

    // Act: Create scanner
    let scanner = VulnerabilityScanner::new(config.clone());

    // Assert: Scanner created successfully with expected configuration
    // (Scanner doesn't expose config, so we verify it doesn't panic)
    drop(scanner);
    assert!(config.fail_on_high_severity);
}

#[test]
fn test_scan_config_custom_values() {
    // Arrange: Create custom configuration
    let config = ScanConfig {
        project_root: PathBuf::from("/custom/path"),
        fail_on_high_severity: false,
        scan_docker: true,
        docker_image: Some("test:latest".to_string()),
    };

    // Act: Create scanner with custom config
    let scanner = VulnerabilityScanner::new(config.clone());

    // Assert: Verify configuration preserved
    drop(scanner);
    assert_eq!(config.project_root, PathBuf::from("/custom/path"));
    assert!(!config.fail_on_high_severity);
    assert!(config.scan_docker);
    assert_eq!(config.docker_image, Some("test:latest".to_string()));
}

#[test]
fn test_sarif_generation_with_empty_results() {
    // Arrange: Create scanner and empty results
    let scanner = VulnerabilityScanner::new(ScanConfig::default());
    let results = ggen_core::security::ScanResults {
        vulnerabilities: Vec::new(),
        critical_count: 0,
        high_count: 0,
        medium_count: 0,
        low_count: 0,
    };

    // Act: Generate SARIF output
    let sarif = scanner.generate_sarif(&results);

    // Assert: SARIF generated successfully with expected structure
    assert!(sarif.is_ok());
    let sarif_str = sarif.unwrap();
    assert!(sarif_str.contains("\"version\":\"2.1.0\""));
    assert!(sarif_str.contains("ggen-security-scanner"));
}

#[test]
fn test_sarif_output_structure() {
    // Arrange: Create scanner and results with violations
    let scanner = VulnerabilityScanner::new(ScanConfig::default());
    let mut results = ggen_core::security::ScanResults {
        vulnerabilities: Vec::new(),
        critical_count: 0,
        high_count: 0,
        medium_count: 0,
        low_count: 0,
    };

    // Add sample vulnerability
    let vuln = ggen_core::security::Vulnerability {
        id: "RUSTSEC-2024-001".to_string(),
        package: "test-crate".to_string(),
        version: "1.0.0".to_string(),
        severity: ggen_core::security::Severity::High,
        description: "Test vulnerability".to_string(),
        url: Some("https://rustsec.org/advisories/RUSTSEC-2024-001".to_string()),
        fixed_version: Some("1.0.1".to_string()),
    };
    results.vulnerabilities.push(vuln);
    results.high_count = 1;

    // Act: Generate SARIF
    let sarif = scanner.generate_sarif(&results);

    // Assert: SARIF contains vulnerability information
    assert!(sarif.is_ok());
    let sarif_str = sarif.unwrap();
    assert!(sarif_str.contains("RUSTSEC-2024-001"));
    assert!(sarif_str.contains("test-crate"));
    assert!(sarif_str.contains("error")); // High severity maps to "error"
}

// ============================================================================
// Compliance Checker Tests (Chicago TDD)
// ============================================================================

#[test]
fn test_compliance_checker_creation() {
    // Arrange: Create compliance configuration
    let config = ComplianceConfig::default();

    // Act: Create checker
    let result = ComplianceChecker::new(config);

    // Assert: Checker created successfully
    assert!(result.is_ok());
}

#[test]
fn test_detect_unwrap_in_production_code() {
    // Arrange: Create temporary project with unwrap violation
    let tmp_dir = tempfile::tempdir().unwrap();
    let project_root = create_sample_project(&tmp_dir);

    let code_with_unwrap = r#"
pub fn process_data(input: Option<String>) -> String {
    input.unwrap() // This should be detected
}
"#;
    create_file_with_violations(&project_root, "main.rs", code_with_unwrap);

    let config = ComplianceConfig {
        root_dir: project_root.clone(),
        include_patterns: vec!["**/*.rs".to_string()],
        exclude_patterns: vec!["**/target/**".to_string()],
        check_unwrap: true,
        check_sparql_injection: false,
        check_atom_exhaustion: false,
    };

    // Act: Run compliance check
    let checker = ComplianceChecker::new(config).unwrap();
    let results = checker.check();

    // Assert: Unwrap violation detected
    assert!(results.is_ok());
    let results = results.unwrap();
    assert!(results.total_count() > 0);
    assert!(results.high_count > 0);

    // Verify violation details
    let violation = &results.violations[0];
    assert_eq!(violation.violation_type, ViolationType::UnwrapInProduction);
    assert_eq!(violation.severity, ViolationSeverity::High);
    assert!(violation.snippet.contains("unwrap"));
}

#[test]
fn test_detect_expect_in_production_code() {
    // Arrange: Create temporary project with expect violation
    let tmp_dir = tempfile::tempdir().unwrap();
    let project_root = create_sample_project(&tmp_dir);

    let code_with_expect = r#"
pub fn process_data(input: Option<String>) -> String {
    input.expect("input should always be present") // This should be detected
}
"#;
    create_file_with_violations(&project_root, "main.rs", code_with_expect);

    let config = ComplianceConfig {
        root_dir: project_root.clone(),
        include_patterns: vec!["**/*.rs".to_string()],
        exclude_patterns: vec!["**/target/**".to_string()],
        check_unwrap: true,
        check_sparql_injection: false,
        check_atom_exhaustion: false,
    };

    // Act: Run compliance check
    let checker = ComplianceChecker::new(config).unwrap();
    let results = checker.check();

    // Assert: Expect violation detected
    assert!(results.is_ok());
    let results = results.unwrap();
    assert!(results.total_count() > 0);
    assert!(results.high_count > 0);

    // Verify violation details
    let violation = &results.violations[0];
    assert_eq!(violation.violation_type, ViolationType::UnwrapInProduction);
    assert!(violation.snippet.contains("expect"));
}

#[test]
fn test_detect_panic_in_production_code() {
    // Arrange: Create temporary project with panic violation
    let tmp_dir = tempfile::tempdir().unwrap();
    let project_root = create_sample_project(&tmp_dir);

    let code_with_panic = r#"
pub fn process_data(input: String) {
    if input.is_empty() {
        panic!("Input cannot be empty"); // This should be detected
    }
}
"#;
    create_file_with_violations(&project_root, "main.rs", code_with_panic);

    let config = ComplianceConfig {
        root_dir: project_root.clone(),
        include_patterns: vec!["**/*.rs".to_string()],
        exclude_patterns: vec!["**/target/**".to_string()],
        check_unwrap: true,
        check_sparql_injection: false,
        check_atom_exhaustion: false,
    };

    // Act: Run compliance check
    let checker = ComplianceChecker::new(config).unwrap();
    let results = checker.check();

    // Assert: Panic violation detected
    assert!(results.is_ok());
    let results = results.unwrap();
    assert!(results.total_count() > 0);
    assert!(results.critical_count > 0);

    // Verify violation details
    let violation = &results.violations[0];
    assert_eq!(violation.violation_type, ViolationType::PanicInProduction);
    assert_eq!(violation.severity, ViolationSeverity::Critical);
    assert!(violation.snippet.contains("panic!"));
}

#[test]
fn test_allow_unwrap_in_test_code() {
    // Arrange: Create temporary project with unwrap in test
    let tmp_dir = tempfile::tempdir().unwrap();
    let project_root = create_sample_project(&tmp_dir);

    let test_code_with_unwrap = r#"
#[cfg(test)]
mod tests {
    #[test]
    fn test_something() {
        let value = Some(42).unwrap(); // This should NOT be detected (test code)
        assert_eq!(value, 42);
    }
}
"#;
    create_file_with_violations(&project_root, "tests.rs", test_code_with_unwrap);

    let config = ComplianceConfig {
        root_dir: project_root.clone(),
        include_patterns: vec!["**/*.rs".to_string()],
        exclude_patterns: vec!["**/target/**".to_string()],
        check_unwrap: true,
        check_sparql_injection: false,
        check_atom_exhaustion: false,
    };

    // Act: Run compliance check
    let checker = ComplianceChecker::new(config).unwrap();
    let results = checker.check();

    // Assert: No violations in test code
    assert!(results.is_ok());
    let results = results.unwrap();
    assert_eq!(results.total_count(), 0); // Test code should be allowed
}

#[test]
fn test_detect_sparql_injection() {
    // Arrange: Create temporary project with SPARQL injection risk
    let tmp_dir = tempfile::tempdir().unwrap();
    let project_root = create_sample_project(&tmp_dir);

    let code_with_sparql_injection = r#"
pub fn query_user(user_id: &str) -> String {
    format!("SELECT * FROM users WHERE id = '{}'", user_id) // SPARQL injection risk
}
"#;
    create_file_with_violations(&project_root, "query.rs", code_with_sparql_injection);

    let config = ComplianceConfig {
        root_dir: project_root.clone(),
        include_patterns: vec!["**/*.rs".to_string()],
        exclude_patterns: vec!["**/target/**".to_string()],
        check_unwrap: false,
        check_sparql_injection: true,
        check_atom_exhaustion: false,
    };

    // Act: Run compliance check
    let checker = ComplianceChecker::new(config).unwrap();
    let results = checker.check();

    // Assert: SPARQL injection detected
    assert!(results.is_ok());
    let results = results.unwrap();
    assert!(results.total_count() > 0);
    assert!(results.critical_count > 0);

    // Verify violation details
    let violation = &results.violations[0];
    assert_eq!(violation.violation_type, ViolationType::SparqlInjection);
    assert_eq!(violation.severity, ViolationSeverity::Critical);
}

#[test]
fn test_compliance_report_generation() {
    // Arrange: Create compliance results with violations
    let mut results = ggen_core::security::ComplianceResults {
        violations: Vec::new(),
        violations_by_file: std::collections::HashMap::new(),
        critical_count: 1,
        high_count: 2,
        medium_count: 1,
        low_count: 0,
    };

    results.violations.push(Violation {
        violation_type: ViolationType::PanicInProduction,
        severity: ViolationSeverity::Critical,
        file: PathBuf::from("src/main.rs"),
        line: 10,
        column: 5,
        snippet: "panic!(\"error\")".to_string(),
        suggestion: Some("Use Result<T,E>".to_string()),
    });

    let config = ComplianceConfig::default();
    let checker = ComplianceChecker::new(config).unwrap();

    // Act: Generate report
    let report = checker.generate_report(&results);

    // Assert: Report contains expected information
    assert!(report.contains("Compliance Check Report"));
    assert!(report.contains("Critical: 1"));
    assert!(report.contains("High: 2"));
    assert!(report.contains("src/main.rs"));
    assert!(report.contains("panic!(\"error\")"));
}

#[test]
fn test_multiple_violations_in_same_file() {
    // Arrange: Create temporary project with multiple violations
    let tmp_dir = tempfile::tempdir().unwrap();
    let project_root = create_sample_project(&tmp_dir);

    let code_with_multiple_violations = r#"
pub fn process(data: Option<String>) -> String {
    let value = data.unwrap(); // Violation 1
    if value.is_empty() {
        panic!("Empty value"); // Violation 2
    }
    value.expect("Should work") // Violation 3
}
"#;
    create_file_with_violations(&project_root, "main.rs", code_with_multiple_violations);

    let config = ComplianceConfig {
        root_dir: project_root.clone(),
        include_patterns: vec!["**/*.rs".to_string()],
        exclude_patterns: vec!["**/target/**".to_string()],
        check_unwrap: true,
        check_sparql_injection: false,
        check_atom_exhaustion: false,
    };

    // Act: Run compliance check
    let checker = ComplianceChecker::new(config).unwrap();
    let results = checker.check();

    // Assert: All violations detected
    assert!(results.is_ok());
    let results = results.unwrap();
    assert_eq!(results.total_count(), 3);
    assert!(results.critical_count > 0); // panic
    assert!(results.high_count > 0); // unwrap and expect
}

#[test]
fn test_exclude_patterns_work() {
    // Arrange: Create temporary project with violations in excluded directory
    let tmp_dir = tempfile::tempdir().unwrap();
    let project_root = create_sample_project(&tmp_dir);

    // Create tests directory (should be excluded)
    fs::create_dir_all(project_root.join("tests")).unwrap();
    let test_file = project_root.join("tests").join("test_file.rs");
    fs::write(
        test_file,
        "fn test() { let x = Some(1).unwrap(); } // Should be ignored",
    )
    .unwrap();

    let config = ComplianceConfig {
        root_dir: project_root.clone(),
        include_patterns: vec!["**/*.rs".to_string()],
        exclude_patterns: vec!["**/tests/**".to_string()],
        check_unwrap: true,
        check_sparql_injection: false,
        check_atom_exhaustion: false,
    };

    // Act: Run compliance check
    let checker = ComplianceChecker::new(config).unwrap();
    let results = checker.check();

    // Assert: Excluded files not checked
    assert!(results.is_ok());
    let results = results.unwrap();
    // Should not detect violations in excluded tests directory
    assert_eq!(results.total_count(), 0);
}

// ============================================================================
// Integration Tests (Real Workflows)
// ============================================================================

#[test]
fn test_full_security_scan_workflow() {
    // Arrange: Create project with both vulnerability and compliance issues
    let tmp_dir = tempfile::tempdir().unwrap();
    let project_root = create_sample_project(&tmp_dir);

    let problematic_code = r#"
pub fn unsafe_query(user_input: &str) -> String {
    let query = format!("SELECT * WHERE {{ ?s ?p '{}' }}", user_input); // SPARQL injection
    execute_query(&query).unwrap() // unwrap in production
}

fn execute_query(_query: &str) -> Option<String> {
    Some("result".to_string())
}
"#;
    create_file_with_violations(&project_root, "main.rs", problematic_code);

    // Act: Run both vulnerability scan and compliance check
    let scan_config = ScanConfig {
        project_root: project_root.clone(),
        fail_on_high_severity: true,
        scan_docker: false,
        docker_image: None,
    };
    let scanner = VulnerabilityScanner::new(scan_config);
    let scan_results = scanner.scan();

    let compliance_config = ComplianceConfig {
        root_dir: project_root,
        include_patterns: vec!["**/*.rs".to_string()],
        exclude_patterns: vec!["**/target/**".to_string()],
        check_unwrap: true,
        check_sparql_injection: true,
        check_atom_exhaustion: false,
    };
    let checker = ComplianceChecker::new(compliance_config).unwrap();
    let compliance_results = checker.check();

    // Assert: Both scans complete successfully
    assert!(scan_results.is_ok());
    assert!(compliance_results.is_ok());

    let compliance = compliance_results.unwrap();
    assert!(compliance.total_count() > 0);
    assert!(compliance.has_critical_violations()); // SPARQL injection
}
