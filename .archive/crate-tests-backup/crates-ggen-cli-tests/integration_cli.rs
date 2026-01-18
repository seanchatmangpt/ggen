//! CLI help test
//!
//! This test validates that the CLI binary runs and can display help.
//! This is the most basic smoke test for CLI functionality.

use assert_cmd::Command;

#[test]
fn test_cli_help() {
    Command::cargo_bin("ggen-cli-lib")
        .unwrap()
        .arg("--help")
        .assert()
        .success()
        .stdout(predicates::str::contains("Usage"));
}
