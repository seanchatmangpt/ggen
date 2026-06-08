#![allow(
    clippy::unwrap_used,
    clippy::expect_used,
    clippy::panic,
    clippy::needless_raw_string_hashes,
    clippy::duration_suboptimal_units,
    clippy::branches_sharing_code,
    clippy::used_underscore_binding,
    clippy::single_char_pattern,
    clippy::ignore_without_reason,
    clippy::cloned_ref_to_slice_refs,
    clippy::doc_overindented_list_items,
    clippy::match_wildcard_for_single_variants,
    clippy::ignored_unit_patterns,
    clippy::needless_collect,
    clippy::unnecessary_map_or,
    clippy::manual_flatten,
    clippy::manual_strip,
    clippy::future_not_send,
    clippy::unnested_or_patterns,
    clippy::no_effect_underscore_binding,
    clippy::literal_string_with_formatting_args
)]
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
        .stdout(predicate::str::contains("init"))
        .stdout(predicate::str::contains("sync"))
        .stdout(predicate::str::contains("utils"))
        .stdout(predicate::str::contains("doctor"))
        .stdout(predicate::str::contains("pack"))
        .stdout(predicate::str::contains("graph"))
        .stdout(predicate::str::contains("policy"));
}

#[test]
fn test_doctor_command_routes_correctly() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running doctor command with --help
    let output = cmd.arg("doctor").arg("--help").assert().success();

    // THEN: Should show doctor-specific help
    output.stdout(predicate::str::contains("health check"));
}

#[test]
fn test_pack_command_routes_correctly() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running pack command with --help
    let output = cmd.arg("pack").arg("--help").assert().success();

    // THEN: Should show pack-specific help
    output.stdout(predicate::str::contains("add"));
}

#[test]
fn test_sync_command_routes_correctly() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running sync command with --help
    let output = cmd.arg("sync").arg("--help").assert().success();

    // THEN: Should show sync-specific help
    output.stdout(predicate::str::contains("code synchronization"));
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
fn test_manifest_flag_is_recognized() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running sync with --help
    let output = cmd.arg("sync").arg("--help").assert().success();

    // THEN: Should show manifest flag
    output.stdout(predicate::str::contains("--manifest"));
}

#[test]
fn test_sync_options_are_recognized() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running sync with --help
    let output = cmd.arg("sync").arg("--help").assert().success();

    // THEN: Should show key sync command options
    output
        .stdout(predicate::str::contains("--dry-run"))
        .stdout(predicate::str::contains("--force"))
        .stdout(predicate::str::contains("--audit"))
        .stdout(predicate::str::contains("--watch"));
}

#[test]
fn test_commands_execute_with_real_binary() {
    // GIVEN: A compiled ggen binary
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running doctor check subcommand
    // THEN: Should execute and return with exit code (succeeds or fails gracefully)
    cmd.arg("doctor").arg("check").assert().code(predicate::in_iter([0, 1]));
}

#[test]
fn test_auto_discovery_finds_all_command_modules() {
    // GIVEN: A compiled ggen binary with auto-discovery
    let mut cmd = Command::cargo_bin("ggen").unwrap();

    // WHEN: Running --help
    let output = cmd.arg("--help").assert().success();
    let stdout = String::from_utf8(output.get_output().stdout.clone()).unwrap();

    // THEN: Should discover and list consolidated commands
    let commands = ["sync", "init", "utils", "doctor", "policy", "pack", "graph"];
    let command_count = commands
        .iter()
        .filter(|&cmd_name| stdout.contains(cmd_name))
        .count();

    assert!(
        command_count >= 5,
        "Expected at least 5 commands, found {}",
        command_count
    );
}
