//! FMEA Validation Tests (Story 5 - FMEA Validation)
//!
//! Tests for FMEA validation, critical failure detection, and guard application.
//!
//! # Test Categories
//!
//! - T040: Critical vs non-critical failure modes
//! - T040: Blocking behavior (default vs --force-fmea)
//! - T040: Guard application
//! - T040: Audit logging
//! - T041: Poka-yoke guards (directory, trait, path, version)

use ggen_domain::marketplace::{
    FmeaCategory, FmeaCheck, FmeaCheckResult, FmeaValidationReport, FmeaValidator,
};
use std::fs;
use tempfile::TempDir;

/// T040: Critical failure mode detection (RPN >= 200)
#[test]
fn test_critical_failure_mode_detection() {
    let temp_dir = TempDir::new().unwrap();

    // Create package with critical failure mode (S=9, O=6, D=4 -> RPN=216)
    fs::write(
        temp_dir.path().join("package.toml"),
        r#"
[package]
name = "critical-test"
version = "1.0.0"

[fmea]
enabled = true
min_coverage = 100

[[fmea.controls]]
id = "FM1"
mode = "Critical: Developer overwrites domain code"
severity = 9
occurrence = 6
detection = 4
"#,
    )
    .unwrap();

    let validator = FmeaValidator::new();
    let report = validator.validate_package(temp_dir.path()).unwrap();

    // Should detect critical mode (RPN 216 >= 200)
    assert!(!report.critical_modes.is_empty());
    assert_eq!(report.critical_modes[0].id, "FM1");
    assert!(report.critical_modes[0].rpn >= 200);

    // Without control, should fail validation
    assert!(!report.critical_modes[0].has_control);
    assert!(!report.passed);
}

/// T040: Non-critical failure mode detection (RPN < 200)
#[test]
fn test_non_critical_failure_mode_detection() {
    let temp_dir = TempDir::new().unwrap();

    // Create package with non-critical failure mode (S=5, O=3, D=2 -> RPN=30)
    fs::write(
        temp_dir.path().join("package.toml"),
        r#"
[package]
name = "non-critical-test"
version = "1.0.0"

[fmea]
enabled = true
min_coverage = 100

[[fmea.controls]]
id = "FM1"
mode = "Low risk: Minor formatting issue"
severity = 5
occurrence = 3
detection = 2
"#,
    )
    .unwrap();

    let validator = FmeaValidator::new();
    let report = validator.validate_package(temp_dir.path()).unwrap();

    // Should NOT be critical (RPN 30 < 200)
    assert!(report.critical_modes.is_empty());

    // Might be in high-risk (RPN >= 100) or neither
    // RPN 30 is low risk, so should pass
    assert!(report.passed);
}

/// T040: High risk failure mode detection (RPN 100-199)
#[test]
fn test_high_risk_failure_mode_detection() {
    let temp_dir = TempDir::new().unwrap();

    // Create package with high-risk failure mode (S=7, O=5, D=3 -> RPN=105)
    fs::write(
        temp_dir.path().join("package.toml"),
        r#"
[package]
name = "high-risk-test"
version = "1.0.0"

[fmea]
enabled = true
min_coverage = 100

[[fmea.controls]]
id = "FM1"
mode = "High risk: Template rendering error"
severity = 7
occurrence = 5
detection = 3
"#,
    )
    .unwrap();

    let validator = FmeaValidator::new();
    let report = validator.validate_package(temp_dir.path()).unwrap();

    // Should NOT be critical but should be high-risk
    assert!(report.critical_modes.is_empty());
    assert!(!report.high_risk_modes.is_empty());
    assert!(report.high_risk_modes[0].rpn >= 100);
    assert!(report.high_risk_modes[0].rpn < 200);
}

/// T040: Mitigated critical failure passes
#[test]
fn test_mitigated_critical_failure_passes() {
    let temp_dir = TempDir::new().unwrap();

    // Create package with mitigated critical failure mode
    fs::write(
        temp_dir.path().join("package.toml"),
        r#"
[package]
name = "mitigated-test"
version = "1.0.0"

[fmea]
enabled = true
min_coverage = 100

[[fmea.controls]]
id = "FM1"
mode = "Critical: Developer overwrites domain code"
severity = 9
occurrence = 6
detection = 4
control = "Path protection + DO NOT EDIT headers + linguist-generated"
"#,
    )
    .unwrap();

    let validator = FmeaValidator::new();
    let report = validator.validate_package(temp_dir.path()).unwrap();

    // Critical mode exists but has control
    assert!(!report.critical_modes.is_empty());
    assert!(report.critical_modes[0].has_control);

    // Should pass because control exists
    assert!(report.passed);
}

/// T040: Strict mode fails on unmitigated high-risk
#[test]
fn test_strict_mode_fails_on_high_risk() {
    let temp_dir = TempDir::new().unwrap();

    // Create package with unmitigated high-risk failure mode
    fs::write(
        temp_dir.path().join("package.toml"),
        r#"
[package]
name = "strict-test"
version = "1.0.0"

[fmea]
enabled = true
min_coverage = 100

[[fmea.controls]]
id = "FM1"
mode = "High risk: No control defined"
severity = 7
occurrence = 5
detection = 3
"#,
    )
    .unwrap();

    // Fortune 500 mode is strict
    let validator = FmeaValidator::fortune_500();
    let report = validator.validate_package(temp_dir.path()).unwrap();

    // In strict mode, high-risk without control generates warning
    let has_warning = report.checks.iter().any(|c| c.result.is_warning());
    assert!(has_warning);
}

/// T040: FMEA coverage percentage calculation
#[test]
fn test_fmea_coverage_percentage() {
    let temp_dir = TempDir::new().unwrap();

    // Create package with 2 critical modes, 1 mitigated
    fs::write(
        temp_dir.path().join("package.toml"),
        r#"
[package]
name = "coverage-test"
version = "1.0.0"

[fmea]
enabled = true
min_coverage = 100

[[fmea.controls]]
id = "FM1"
mode = "Critical mode 1"
severity = 9
occurrence = 6
detection = 4
control = "Mitigated"

[[fmea.controls]]
id = "FM2"
mode = "Critical mode 2"
severity = 9
occurrence = 5
detection = 5
"#,
    )
    .unwrap();

    let validator = FmeaValidator::new();
    let report = validator.validate_package(temp_dir.path()).unwrap();

    // 50% coverage (1 of 2 critical mitigated)
    assert_eq!(report.critical_modes.len(), 2);
    assert!(report.coverage_percentage < 100.0);
}

/// T040: Total RPN calculation
#[test]
fn test_total_rpn_calculation() {
    let temp_dir = TempDir::new().unwrap();

    // Create package with known RPN values
    fs::write(
        temp_dir.path().join("package.toml"),
        r#"
[package]
name = "rpn-test"
version = "1.0.0"

[fmea]
enabled = true
min_coverage = 100

[[fmea.controls]]
id = "FM1"
mode = "Mode 1"
severity = 5
occurrence = 4
detection = 3
control = "Control 1"

[[fmea.controls]]
id = "FM2"
mode = "Mode 2"
severity = 3
occurrence = 2
detection = 2
control = "Control 2"
"#,
    )
    .unwrap();

    let validator = FmeaValidator::new();
    let report = validator.validate_package(temp_dir.path()).unwrap();

    // FM1: 5*4*3 = 60, FM2: 3*2*2 = 12, Total = 72
    assert_eq!(report.total_rpn, 72);
    assert_eq!(report.max_rpn, 60);
}

/// T041: Path protection guard - protected paths
#[test]
fn test_path_protection_guard_protected() {
    let validator = FmeaValidator::new();

    // Domain paths should be protected (must match src/domain/** pattern)
    assert!(validator.is_protected("src/domain/user.rs"));
    assert!(validator.is_protected("src/domain/order/model.rs"));
    assert!(validator.is_protected("src/domain/entities.rs"));
}

/// T041: Path protection guard - regeneratable paths
#[test]
fn test_path_protection_guard_regeneratable() {
    let validator = FmeaValidator::new();

    // Generated paths should be regeneratable (must match src/generated/** pattern)
    assert!(validator.is_regeneratable("src/generated/user.rs"));
    assert!(validator.is_regeneratable("src/generated/models/order.rs"));
    assert!(!validator.is_regeneratable("src/domain/user.rs"));
}

/// T041: Write validation - blocks protected path creation
#[test]
fn test_write_validation_blocks_protected_creation() {
    let validator = FmeaValidator::new();

    // Creating new file in protected path should fail
    let result = validator.validate_write("src/domain/new_file.rs", false);
    assert!(result.is_err());
}

/// T041: Write validation - allows regeneratable overwrite
#[test]
fn test_write_validation_allows_regeneratable() {
    let validator = FmeaValidator::new();

    // Overwriting file in regeneratable path should pass
    let result = validator.validate_write("src/generated/model.rs", true);
    assert!(result.is_ok());
}

/// T041: Poka-yoke header generation
#[test]
fn test_poka_yoke_header_generation() {
    let validator = FmeaValidator::new();

    // Rust files should have // comments
    let rs_header = validator.get_generated_header("rs");
    assert!(rs_header.starts_with("//"));
    assert!(rs_header.contains("DO NOT EDIT"));

    // Python files should have # comments
    let py_header = validator.get_generated_header("py");
    assert!(py_header.starts_with("#"));
    assert!(py_header.contains("DO NOT EDIT"));

    // JavaScript files should have // comments
    let js_header = validator.get_generated_header("js");
    assert!(js_header.starts_with("//"));
}

/// T040: FmeaCheckResult pass/fail/warning states
#[test]
fn test_fmea_check_result_states() {
    let pass = FmeaCheckResult::Pass {
        message: "All good".to_string(),
    };
    assert!(pass.is_pass());
    assert!(!pass.is_fail());
    assert!(!pass.is_warning());

    let fail = FmeaCheckResult::Fail {
        message: "Critical failure".to_string(),
        rpn: Some(250),
    };
    assert!(fail.is_fail());
    assert!(!fail.is_pass());
    assert!(!fail.is_warning());

    let warning = FmeaCheckResult::Warning {
        message: "High risk".to_string(),
        rpn: Some(150),
    };
    assert!(warning.is_warning());
    assert!(!warning.is_pass());
    assert!(!warning.is_fail());
}

/// T040: FmeaCategory classification
#[test]
fn test_fmea_category_classification() {
    let check = FmeaCheck {
        name: "Test Check".to_string(),
        category: FmeaCategory::ControlCoverage,
        result: FmeaCheckResult::Pass {
            message: "OK".to_string(),
        },
    };

    assert_eq!(check.category, FmeaCategory::ControlCoverage);
}

/// T040: Enterprise status tracking
#[test]
fn test_enterprise_status_tracking() {
    let temp_dir = TempDir::new().unwrap();

    // Create enterprise-ready package
    fs::write(
        temp_dir.path().join("package.toml"),
        r#"
[package]
name = "enterprise-test"
version = "1.0.0"

[fmea]
enabled = true
min_coverage = 100

[[fmea.controls]]
id = "FM1"
mode = "Critical mode"
severity = 9
occurrence = 6
detection = 4
control = "Full mitigation"
"#,
    )
    .unwrap();

    // Fortune 500 validator
    let validator = FmeaValidator::fortune_500();
    let report = validator.validate_package(temp_dir.path()).unwrap();

    assert!(report.enterprise_status.fmea_enabled);
    assert!(report.enterprise_status.poka_yoke_enabled);
    assert!(report.enterprise_status.fortune_500_ready);
}

/// T040: No FMEA config is skipped
#[test]
fn test_no_fmea_config_skipped() {
    let temp_dir = TempDir::new().unwrap();

    // Create package without FMEA config
    fs::write(
        temp_dir.path().join("package.toml"),
        r#"
[package]
name = "no-fmea-test"
version = "1.0.0"
"#,
    )
    .unwrap();

    let validator = FmeaValidator::new();
    let report = validator.validate_package(temp_dir.path()).unwrap();

    // FMEA check should be skipped
    let fmea_check = report
        .checks
        .iter()
        .find(|c| c.category == FmeaCategory::ControlCoverage);

    assert!(fmea_check.is_some());
    match &fmea_check.unwrap().result {
        FmeaCheckResult::Skipped { reason } => {
            assert!(reason.contains("No FMEA configuration"));
        }
        _ => panic!("Expected Skipped result"),
    }
}

/// T040: FMEA disabled is skipped
#[test]
fn test_fmea_disabled_skipped() {
    let temp_dir = TempDir::new().unwrap();

    // Create package with FMEA disabled
    fs::write(
        temp_dir.path().join("package.toml"),
        r#"
[package]
name = "disabled-fmea-test"
version = "1.0.0"

[fmea]
enabled = false
"#,
    )
    .unwrap();

    let validator = FmeaValidator::new();
    let report = validator.validate_package(temp_dir.path()).unwrap();

    // FMEA check should be skipped
    let fmea_check = report
        .checks
        .iter()
        .find(|c| c.category == FmeaCategory::ControlCoverage);

    assert!(fmea_check.is_some());
    match &fmea_check.unwrap().result {
        FmeaCheckResult::Skipped { reason } => {
            assert!(reason.contains("not enabled"));
        }
        _ => panic!("Expected Skipped result"),
    }
}

/// T041: Directory separation guard - templates detected
#[test]
fn test_directory_separation_templates() {
    let temp_dir = TempDir::new().unwrap();

    // Create templates directory with proper headers
    let templates_dir = temp_dir.path().join("templates");
    fs::create_dir_all(&templates_dir).unwrap();
    fs::write(
        templates_dir.join("model.rs.tera"),
        r#"
// DO NOT EDIT - This file is auto-generated
// Modify the template instead

pub struct {{ name }} {
    // ...
}
"#,
    )
    .unwrap();

    fs::write(
        temp_dir.path().join("package.toml"),
        r#"
[package]
name = "template-test"
version = "1.0.0"
"#,
    )
    .unwrap();

    let validator = FmeaValidator::new();
    let report = validator.validate_package(temp_dir.path()).unwrap();

    // Poka-yoke check should find headers
    let poka_yoke_check = report
        .checks
        .iter()
        .find(|c| c.category == FmeaCategory::PokaYoke);

    assert!(poka_yoke_check.is_some());
    // Should pass or warn with detected headers
    assert!(
        poka_yoke_check.unwrap().result.is_pass()
            || poka_yoke_check.unwrap().result.is_warning()
    );
}

/// T041: CODEOWNERS detection
#[test]
fn test_codeowners_detection() {
    let temp_dir = TempDir::new().unwrap();

    // Create ontology directory with OWNERS file
    let ontology_dir = temp_dir.path().join("ontology");
    fs::create_dir_all(&ontology_dir).unwrap();
    fs::write(
        ontology_dir.join("OWNERS"),
        r#"
# Ontology team
@team-ontology
"#,
    )
    .unwrap();

    fs::write(
        temp_dir.path().join("package.toml"),
        r#"
[package]
name = "owners-test"
version = "1.0.0"
"#,
    )
    .unwrap();

    let validator = FmeaValidator::new();
    let report = validator.validate_package(temp_dir.path()).unwrap();

    // CODEOWNERS check should pass
    let owners_check = report
        .checks
        .iter()
        .find(|c| c.category == FmeaCategory::CodeOwnership);

    assert!(owners_check.is_some());
    assert!(owners_check.unwrap().result.is_pass());
}

/// T041: Missing CODEOWNERS generates warning
#[test]
fn test_missing_codeowners_warning() {
    let temp_dir = TempDir::new().unwrap();

    // Create package without OWNERS
    fs::write(
        temp_dir.path().join("package.toml"),
        r#"
[package]
name = "no-owners-test"
version = "1.0.0"
"#,
    )
    .unwrap();

    let validator = FmeaValidator::new();
    let report = validator.validate_package(temp_dir.path()).unwrap();

    // CODEOWNERS check should warn
    let owners_check = report
        .checks
        .iter()
        .find(|c| c.category == FmeaCategory::CodeOwnership);

    assert!(owners_check.is_some());
    assert!(owners_check.unwrap().result.is_warning());
}
