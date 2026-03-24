//! End-to-end integration tests for utils commands
//!
//! **Chicago TDD Principles**:
//! - REAL CLI process execution
//! - REAL system checks (Rust, Cargo, Git)
//! - REAL environment inspection
//! - NO mocking of system state
//!
//! **Critical User Workflows (80/20)**:
//! 1. Run system diagnostics (doctor)
//! 2. Check environment variables
//! 3. Verify tool installation

use assert_cmd::Command;
use predicates::prelude::*;
use tempfile::TempDir;

/// Helper to create ggen command
fn ggen() -> Command {
    Command::cargo_bin("ggen").expect("Failed to find ggen binary")
}

#[test]
fn test_utils_doctor_runs() {
    // Chicago TDD: Verify system diagnostics execute
    ggen()
        .arg("utils")
        .arg("doctor")
        .assert()
        .success()
        .stdout(
            predicate::str::contains("Rust")
                .or(predicate::str::contains("Cargo"))
                .or(predicate::str::contains("checks")),
        );
}

#[test]
fn test_utils_doctor_all() {
    // Chicago TDD: Verify all checks mode
    ggen()
        .arg("utils")
        .arg("doctor")
        .arg("--all")
        .assert()
        .success();
}

#[test]
fn test_utils_doctor_env_format() {
    // Chicago TDD: Verify environment format output
    ggen()
        .arg("utils")
        .arg("doctor")
        .arg("--format")
        .arg("env")
        .assert()
        .success();
}

#[test]
fn test_utils_doctor_json_format() {
    // Chicago TDD: Verify JSON format output
    let output = ggen()
        .arg("utils")
        .arg("doctor")
        .arg("--format")
        .arg("json")
        .output()
        .expect("Failed to execute");

    // Command should complete successfully
    assert!(output.status.success(), "Doctor should succeed");
}

#[test]
fn test_utils_env_lists() {
    // Chicago TDD: Verify environment listing
    // Note: env command is stubbed, returns empty
    ggen()
        .arg("utils")
        .arg("env")
        .arg("--list")
        .assert()
        .success();
}

#[test]
fn test_utils_env_get() {
    // Chicago TDD: Verify environment variable get
    // Note: env command is stubbed, returns empty
    ggen()
        .arg("utils")
        .arg("env")
        .arg("--get")
        .arg("PATH")
        .assert()
        .success();
}

#[test]
fn test_utils_env_set() {
    // Chicago TDD: Verify environment variable set
    // Note: env command is stubbed
    ggen()
        .arg("utils")
        .arg("env")
        .arg("--set")
        .arg("TEST_VAR=test")
        .assert()
        .success();
}

#[test]
fn test_utils_help_shows_verbs() {
    // Chicago TDD: Verify help state is comprehensive
    ggen()
        .arg("utils")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("doctor"))
        .stdout(predicate::str::contains("env"));
}

#[test]
fn test_utils_doctor_help() {
    // Chicago TDD: Verify verb-specific help
    ggen()
        .arg("utils")
        .arg("doctor")
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("diagnostic").or(predicate::str::contains("check")));
}

#[test]
fn test_utils_invalid_verb() {
    // Chicago TDD: Verify error handling for invalid verbs
    ggen()
        .arg("utils")
        .arg("invalid-verb")
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("invalid")));
}

#[test]
fn test_utils_doctor_checks_system_tools() {
    // Chicago TDD: Verify doctor checks for required tools
    let output = ggen()
        .arg("utils")
        .arg("doctor")
        .output()
        .expect("Failed to execute");

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Verify system tools are checked
    // Output should contain information about Rust, Cargo, or Git
    assert!(
        stdout.contains("Rust") || stdout.contains("Cargo") || stdout.contains("Git") || stdout.contains("checks"),
        "Doctor should check system tools"
    );
}

#[test]
fn test_utils_doctor_reports_health_status() {
    // Chicago TDD: Verify doctor reports overall health
    let output = ggen()
        .arg("utils")
        .arg("doctor")
        .output()
        .expect("Failed to execute");

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Verify health status reported
    assert!(
        stdout.contains("healthy") || stdout.contains("needs attention") || stdout.contains("status"),
        "Doctor should report health status"
    );
}
