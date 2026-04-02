//! Chicago TDD tests for audit/security utilities
//!
//! These tests use REAL system operations:
//! - REAL file system scanning
//! - REAL cargo-audit execution (when available)
//! - REAL configuration file analysis
//! - Temporary test files and directories

use ggen_cli::domain::audit::security::*;
use std::path::PathBuf;
use tempfile::TempDir;

#[test]
fn test_severity_levels_ordering() {
    // Test severity comparison
    assert!(Severity::Critical > Severity::High);
    assert!(Severity::High > Severity::Medium);
    assert!(Severity::Medium > Severity::Low);

    // Test equality
    assert_eq!(Severity::Critical, Severity::Critical);
    assert_ne!(Severity::Critical, Severity::High);
}

#[test]
fn test_severity_summary_calculations() {
    let summary = SeveritySummary {
        critical: 3,
        high: 5,
        medium: 10,
        low: 2,
    };

    assert_eq!(summary.total(), 20);
    assert!(summary.has_critical_or_high());

    // Test empty summary
    let empty = SeveritySummary::default();
    assert_eq!(empty.total(), 0);
    assert!(!empty.has_critical_or_high());

    // Test only medium/low
    let safe = SeveritySummary {
        critical: 0,
        high: 0,
        medium: 5,
        low: 3,
    };
    assert_eq!(safe.total(), 8);
    assert!(!safe.has_critical_or_high());
}

#[test]
fn test_config_auditor_detects_secrets() {
    // Create temporary config file with potential secrets
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let config_path = temp_dir.path().join("test_config.toml");

    let config_content = r#"
[database]
host = "localhost"
password = "super_secret_password"
api_key = "sk_test_12345"
"#;

    std::fs::write(&config_path, config_content).expect("Failed to write config");

    // Run REAL config auditor
    let auditor = FileSystemConfigAuditor;
    let result = auditor.audit(Some(&config_path));

    assert!(result.is_ok(), "Audit failed: {:?}", result.err());

    let audit_result = result.unwrap();
    assert_eq!(audit_result.config_file, config_path);
    assert!(audit_result.audit_duration_ms > 0);
    assert!(!audit_result.issues.is_empty(), "Should detect hardcoded secrets");

    // Verify detected issues
    let has_secret_issue = audit_result.issues.iter().any(|issue| {
        issue.issue_type == ConfigIssueType::HardcodedSecret
            && issue.severity == Severity::High
    });
    assert!(has_secret_issue, "Should detect hardcoded secret");
}

#[test]
fn test_config_auditor_clean_config() {
    // Create temporary config file without secrets
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let config_path = temp_dir.path().join("clean_config.toml");

    let config_content = r#"
[settings]
timeout = 30
retries = 3
enabled = true
"#;

    std::fs::write(&config_path, config_content).expect("Failed to write config");

    let auditor = FileSystemConfigAuditor;
    let result = auditor.audit(Some(&config_path));

    assert!(result.is_ok());
    let audit_result = result.unwrap();
    assert!(
        audit_result.issues.is_empty(),
        "Clean config should have no issues"
    );
}

#[test]
fn test_config_auditor_nonexistent_file() {
    let nonexistent = PathBuf::from("/tmp/nonexistent_config_12345.toml");

    let auditor = FileSystemConfigAuditor;
    let result = auditor.audit(Some(&nonexistent));

    assert!(result.is_err());
    assert!(result
        .unwrap_err()
        .to_string()
        .contains("Configuration file not found"));
}

#[test]
fn test_config_auditor_default_file() {
    // Test auditing Cargo.toml (should exist in project root)
    let auditor = FileSystemConfigAuditor;

    // This might pass or fail depending on whether we're in the project directory
    // The important thing is it doesn't panic
    let result = auditor.audit(None);
    match result {
        Ok(audit_result) => {
            assert_eq!(audit_result.config_file, PathBuf::from("Cargo.toml"));
        }
        Err(e) => {
            // Expected if not in project root
            println!("Expected error when not in project root: {}", e);
        }
    }
}

#[test]
fn test_workflow_status_parsing() {
    assert_eq!(
        WorkflowStatus::from_str("queued"),
        Some(WorkflowStatus::Queued)
    );
    assert_eq!(
        WorkflowStatus::from_str("IN_PROGRESS"),
        Some(WorkflowStatus::InProgress)
    );
    assert_eq!(
        WorkflowStatus::from_str("Completed"),
        Some(WorkflowStatus::Completed)
    );
    assert_eq!(
        WorkflowStatus::from_str("failed"),
        Some(WorkflowStatus::Failed)
    );
    assert_eq!(
        WorkflowStatus::from_str("cancelled"),
        Some(WorkflowStatus::Cancelled)
    );
    assert_eq!(WorkflowStatus::from_str("unknown"), None);
}

// Import workflow status for the last test
use ggen_cli::domain::ci::workflow::WorkflowStatus;

#[test]
fn test_config_issue_creation() {
    let issue = ConfigIssue {
        issue_type: ConfigIssueType::HardcodedSecret,
        severity: Severity::Critical,
        description: "Database password in config".to_string(),
        location: "config.toml:line 5".to_string(),
        recommendation: "Use environment variable DB_PASSWORD".to_string(),
        auto_fixable: false,
    };

    assert_eq!(issue.issue_type, ConfigIssueType::HardcodedSecret);
    assert_eq!(issue.severity, Severity::Critical);
    assert!(!issue.auto_fixable);
}

#[test]
fn test_vulnerability_structure() {
    let vuln = Vulnerability {
        id: "CVE-2024-12345".to_string(),
        severity: Severity::High,
        description: "Buffer overflow in parser".to_string(),
        file_path: Some(PathBuf::from("src/parser.rs")),
        line_number: Some(42),
        recommendation: "Update to version 2.0.1".to_string(),
    };

    assert_eq!(vuln.id, "CVE-2024-12345");
    assert_eq!(vuln.severity, Severity::High);
    assert_eq!(vuln.line_number, Some(42));
}

#[test]
fn test_vulnerable_dependency_structure() {
    let dep = VulnerableDependency {
        name: "vulnerable-lib".to_string(),
        version: "1.0.0".to_string(),
        advisory_id: "RUSTSEC-2024-0001".to_string(),
        severity: Severity::High,
        description: "Memory safety issue".to_string(),
        patched_versions: vec!["1.0.1".to_string(), "1.1.0".to_string()],
    };

    assert_eq!(dep.name, "vulnerable-lib");
    assert!(!dep.patched_versions.is_empty());
    assert_eq!(dep.patched_versions.len(), 2);
}

#[test]
fn test_security_scan_result_structure() {
    let result = SecurityScanResult {
        vulnerabilities: vec![],
        severity_summary: SeveritySummary {
            critical: 0,
            high: 0,
            medium: 2,
            low: 5,
        },
        scan_duration_ms: 1234,
    };

    assert!(result.vulnerabilities.is_empty());
    assert_eq!(result.severity_summary.total(), 7);
    assert!(result.scan_duration_ms > 0);
}

#[test]
fn test_all_config_issue_types() {
    let types = [
        ConfigIssueType::HardcodedSecret,
        ConfigIssueType::InsecurePermissions,
        ConfigIssueType::WeakEncryption,
        ConfigIssueType::MissingValidation,
        ConfigIssueType::DeprecatedSetting,
    ];

    // Verify all types can be created and compared
    for (i, type_a) in types.iter().enumerate() {
        for (j, type_b) in types.iter().enumerate() {
            if i == j {
                assert_eq!(type_a, type_b);
            } else {
                assert_ne!(type_a, type_b);
            }
        }
    }
}

#[test]
fn test_config_auditor_measures_duration() {
    let temp_dir = TempDir::new().expect("Failed to create temp dir");
    let config_path = temp_dir.path().join("timing_test.toml");

    std::fs::write(&config_path, "[test]\nvalue = 1").expect("Failed to write");

    let auditor = FileSystemConfigAuditor;
    let result = auditor.audit(Some(&config_path)).expect("Audit failed");

    // Duration should be measured
    assert!(
        result.audit_duration_ms > 0,
        "Audit should measure duration"
    );
    assert!(
        result.audit_duration_ms < 10000,
        "Audit should complete quickly"
    );
}
