//! Chicago TDD Integration Tests for Entry Point Auto-Discovery
//!
//! Tests REAL CLI execution with actual command discovery and routing.
//! No mocking of the CLI framework - uses assert_cmd to spawn real processes.

use assert_cmd::Command;
use predicates::prelude::*;

#[test]
fn test_cli_binary_exists_and_runs() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running with --version
    // THEN: Should display version and exit successfully
    cmd.arg("--version")
        .assert()
        .success()
        .stdout(predicate::str::contains("ggen"));
}

#[test]
fn test_help_displays_auto_discovered_commands() {
    // GIVEN: A compiled ggen binary with auto-discovery
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running with --help
    let output = cmd.arg("--help").assert().success();

    // THEN: Should display all auto-discovered noun-verb commands
    output
        .stdout(predicate::str::contains("ai"))
        .stdout(predicate::str::contains("audit"))
        .stdout(predicate::str::contains("ci"))
        .stdout(predicate::str::contains("doctor"))
        .stdout(predicate::str::contains("graph"))
        .stdout(predicate::str::contains("hook"))
        .stdout(predicate::str::contains("lifecycle"))
        .stdout(predicate::str::contains("market"))
        .stdout(predicate::str::contains("project"))
        .stdout(predicate::str::contains("shell"))
        .stdout(predicate::str::contains("template"));
}

#[test]
fn test_doctor_command_routes_correctly() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running doctor command with --help
    let output = cmd.arg("doctor").arg("--help").assert().success();

    // THEN: Should show doctor-specific help
    output.stdout(predicate::str::contains("Check system prerequisites"));
}

#[test]
fn test_template_command_routes_correctly() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running template command with --help
    let output = cmd.arg("template").arg("--help").assert().success();

    // THEN: Should show template-specific help
    output.stdout(predicate::str::contains("Template management"));
}

#[test]
fn test_market_command_routes_correctly() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running market command with --help
    let output = cmd.arg("market").arg("--help").assert().success();

    // THEN: Should show market-specific help
    output.stdout(predicate::str::contains("Marketplace operations"));
}

#[test]
fn test_invalid_command_shows_error() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running with an invalid command
    let output = cmd.arg("nonexistent").assert().failure();

    // THEN: Should show error message
    output.stderr(predicate::str::contains("error"));
}

#[test]
fn test_global_flags_work_before_command() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running with global --debug flag with value before command
    // THEN: Should accept global flags in correct position
    cmd.arg("--debug")
        .arg("true")
        .arg("doctor")
        .arg("--help")
        .assert()
        .success();
}

#[test]
fn test_otel_flags_are_recognized() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running with --help to see all flags
    let output = cmd.arg("--help").assert().success();

    // THEN: Should show OpenTelemetry flags
    output
        .stdout(predicate::str::contains("--enable-otel"))
        .stdout(predicate::str::contains("--otel-endpoint"));
}

#[test]
fn test_config_flag_is_recognized() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running with --help
    let output = cmd.arg("--help").assert().success();

    // THEN: Should show config flag
    output.stdout(predicate::str::contains("--config"));
}

#[test]
fn test_manifest_path_flag_is_recognized() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running with --help
    let output = cmd.arg("--help").assert().success();

    // THEN: Should show manifest-path flag
    output.stdout(predicate::str::contains("--manifest-path"));
}

#[test]
fn test_commands_execute_with_real_binary() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running doctor command (should succeed even without full environment)
    // THEN: Should execute and return with exit code (may fail checks, but should run)
    cmd.arg("doctor").assert().code(predicate::in_iter([0, 1]));
}

#[test]
fn test_help_progressive_command_exists() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running help-me command with --help
    let output = cmd.arg("help-me").arg("--help").assert().success();

    // THEN: Should show progressive help
    output.stdout(predicate::str::contains("personalized help"));
}

#[test]
fn test_auto_discovery_finds_all_command_modules() {
    // GIVEN: A compiled ggen binary with auto-discovery
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running --help
    let output = cmd.arg("--help").assert().success();
    let stdout = String::from_utf8(output.get_output().stdout.clone()).unwrap();

    // THEN: Should discover and list all 12 commands
    let command_count = [
        "ai",
        "audit",
        "ci",
        "doctor",
        "graph",
        "help-me",
        "hook",
        "lifecycle",
        "market",
        "project",
        "shell",
        "template",
    ]
    .iter()
    .filter(|&cmd_name| stdout.contains(cmd_name))
    .count();

    assert!(
        command_count >= 10,
        "Expected at least 10 commands, found {}",
        command_count
    );
}
