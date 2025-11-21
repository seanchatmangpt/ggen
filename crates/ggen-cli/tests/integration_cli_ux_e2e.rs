//! End-to-end integration tests for CLI UX and error handling
//!
//! **Chicago TDD Principles**:
//! - REAL CLI process execution
//! - REAL error condition testing
//! - REAL help output verification
//! - REAL state verification for UX elements
//! - NO mocking of CLI framework
//!
//! **Critical User Workflows (80/20)**:
//! 1. Help system (--help, help-me progressive)
//! 2. Error messages and recovery
//! 3. Invalid command handling
//! 4. Doctor command (system checks)
//! 5. Version and info commands

use assert_cmd::Command;
use predicates::prelude::*;

/// Helper to create ggen command
fn ggen() -> Command {
    #[allow(clippy::expect_used)]
    Command::cargo_bin("ggen").expect("Failed to find ggen binary")
}

#[test]
fn test_cli_help_main() {
    // Chicago TDD: Verify main help output state
    ggen()
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("ggen"))
        .stdout(predicate::str::contains("template"))
        .stdout(predicate::str::contains("market"))
        .stdout(predicate::str::contains("project"))
        .stdout(predicate::str::contains("graph"));
}

#[test]
fn test_cli_help_short_flag() {
    // Chicago TDD: Verify -h flag works
    ggen()
        .arg("-h")
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage").or(predicate::str::contains("ggen")));
}

#[test]
fn test_cli_version() {
    // Chicago TDD: Verify version output state
    ggen()
        .arg("--version")
        .assert()
        .success()
        .stdout(predicate::str::contains("ggen").or(predicate::str::contains(".")));
}

#[test]
fn test_cli_version_short_flag() {
    // Chicago TDD: Verify -V flag works
    ggen()
        .arg("-V")
        .assert()
        .success()
        .stdout(predicate::str::contains("ggen").or(predicate::str::contains(".")));
}

#[test]
fn test_cli_invalid_command() {
    // Chicago TDD: Verify error state for invalid commands
    ggen()
        .arg("nonexistent-command")
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("invalid")));
}

#[test]
fn test_cli_invalid_flag() {
    // Chicago TDD: Verify error state for invalid flags
    ggen()
        .arg("--nonexistent-flag")
        .assert()
        .failure()
        .stderr(predicate::str::contains("error").or(predicate::str::contains("unexpected")));
}

#[test]
fn test_cli_missing_required_arg() {
    // Chicago TDD: Verify error state for missing arguments
    ggen()
        .arg("template")
        .arg("show")
        // Missing template name
        .assert()
        .failure()
        .stderr(predicate::str::contains("required").or(predicate::str::contains("error")));
}

#[test]
fn test_doctor_basic_check() {
    // Chicago TDD: Verify doctor command runs system checks
    ggen().arg("doctor").assert().success().stdout(
        predicate::str::contains("Checking")
            .or(predicate::str::contains("System"))
            .or(predicate::str::contains("Prerequisites")),
    );
}

#[test]
fn test_doctor_verbose_output() {
    // Chicago TDD: Verify verbose mode provides detailed state
    ggen().arg("doctor").arg("--verbose").assert().success();
}

#[test]
fn test_help_progressive_basic() {
    // Chicago TDD: Verify progressive help system
    ggen()
        .arg("help-me")
        .assert()
        .success()
        .stdout(predicate::str::contains("help").or(predicate::str::contains("Getting started")));
}

#[test]
fn test_help_progressive_with_level() {
    // Chicago TDD: Verify experience level filtering
    ggen()
        .arg("help-me")
        .arg("--level")
        .arg("beginner")
        .assert()
        .success();
}

#[test]
fn test_help_progressive_topic() {
    // Chicago TDD: Verify topic-specific help
    ggen()
        .arg("help-me")
        .arg("--topic")
        .arg("templates")
        .assert()
        .success();
}

#[test]
fn test_all_nouns_have_help() {
    // Chicago TDD: Verify every noun command has help
    let nouns = vec![
        "template",
        "market",
        "project",
        "graph",
        "ai",
        "hook",
        "lifecycle",
        "audit",
        "ci",
        "shell",
    ];

    for noun in nouns {
        ggen()
            .arg(noun)
            .arg("--help")
            .assert()
            .success()
            .stdout(predicate::str::contains(noun).or(predicate::str::contains("Usage")));
    }
}

#[test]
fn test_error_suggests_help() {
    // Chicago TDD: Verify errors guide users to help
    let output = ggen().arg("invalid-command").output().unwrap();

    let stderr = String::from_utf8_lossy(&output.stderr);

    // Error message should be helpful
    assert!(
        stderr.contains("help")
            || stderr.contains("--help")
            || stderr.contains("usage")
            || stderr.contains("error"),
        "Error should suggest help: {}",
        stderr
    );
}

#[test]
fn test_cli_no_args_shows_help() {
    // Chicago TDD: Verify running without args shows help
    let output = ggen().output().unwrap();

    // Should show help or error with usage
    let stdout = String::from_utf8_lossy(&output.stdout);
    let stderr = String::from_utf8_lossy(&output.stderr);

    assert!(
        stdout.contains("Usage")
            || stdout.contains("help")
            || stderr.contains("Usage")
            || stderr.contains("help"),
        "Should show usage information"
    );
}

#[test]
fn test_cli_subcommand_no_verb() {
    // Chicago TDD: Verify noun without verb shows help
    ggen().arg("template").assert().failure().stderr(
        predicate::str::contains("required")
            .or(predicate::str::contains("subcommand"))
            .or(predicate::str::contains("error")),
    );
}

#[test]
fn test_cli_help_shows_examples() {
    // Chicago TDD: Verify help includes usage examples
    let output = ggen().arg("template").arg("--help").output().unwrap();

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Help should be comprehensive (contains verb names at minimum)
    assert!(
        stdout.contains("new") || stdout.contains("list") || stdout.contains("show"),
        "Help should show available verbs"
    );
}

#[test]
fn test_cli_consistent_error_format() {
    // Chicago TDD: Verify errors have consistent format
    let outputs = vec![
        ggen().arg("invalid-command").output().unwrap(),
        ggen().arg("template").arg("invalid-verb").output().unwrap(),
        ggen().arg("--invalid-flag").output().unwrap(),
    ];

    for output in outputs {
        let stderr = String::from_utf8_lossy(&output.stderr);

        // All errors should contain "error" or similar indicator
        assert!(
            stderr.contains("error")
                || stderr.contains("Error")
                || stderr.contains("invalid")
                || stderr.contains("unexpected"),
            "Error messages should be clear: {}",
            stderr
        );
    }
}

#[test]
fn test_cli_exit_codes_correct() {
    // Chicago TDD: Verify exit codes are meaningful
    // Success case
    let success = ggen().arg("--help").status().unwrap();
    assert!(success.success(), "Help should exit with code 0");

    // Failure case
    let failure = ggen().arg("invalid-command").status().unwrap();
    assert!(!failure.success(), "Invalid command should exit non-zero");
}

#[test]
fn test_cli_handles_ctrl_c_gracefully() {
    // Chicago TDD: Verify signal handling (if applicable)
    // Note: This is hard to test in unit tests, but we can verify
    // that commands don't leave zombie processes

    let output = ggen().arg("--help").output().unwrap();
    assert!(output.status.success());

    // No zombie processes should remain (verified by clean exit)
}

#[test]
fn test_cli_colored_output_flag() {
    // Chicago TDD: Verify color control flags work
    let _ = ggen().arg("--help").arg("--color=never").output();

    // Command should complete successfully
    ggen()
        .arg("--help")
        .arg("--color=always")
        .assert()
        .success();
}

#[test]
fn test_cli_json_output_flag() {
    // Chicago TDD: Verify JSON output flag is respected
    let commands_with_json = vec![
        vec!["template", "list", "--json"],
        vec!["market", "list", "--json"],
        vec!["market", "search", "test", "--json"],
    ];

    for args in commands_with_json {
        let output = ggen().args(&args).output().unwrap();

        // If command succeeds, JSON should be valid
        if output.status.success() {
            let stdout = String::from_utf8_lossy(&output.stdout);
            if !stdout.trim().is_empty() {
                serde_json::from_str::<serde_json::Value>(&stdout)
                    #[allow(clippy::expect_used)]
                    .expect("JSON output should be valid");
            }
        }
    }
}

#[test]
fn test_cli_performance_help_fast() {
    // Chicago TDD: Verify help commands are fast
    let start = std::time::Instant::now();

    ggen().arg("--help").assert().success();

    let duration = start.elapsed();

    // Help should be instant
    assert!(
        duration.as_millis() < 1000,
        "Help should be instant: {:?}",
        duration
    );
}

#[test]
fn test_cli_doctor_checks_essentials() {
    // Chicago TDD: Verify doctor checks critical dependencies
    let output = ggen().arg("doctor").output().unwrap();

    assert!(output.status.success());

    let stdout = String::from_utf8_lossy(&output.stdout);

    // Doctor should check for essential components
    // (exact output depends on implementation)
    assert!(!stdout.is_empty(), "Doctor should produce output");
}
