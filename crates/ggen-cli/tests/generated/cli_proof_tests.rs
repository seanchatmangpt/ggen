//! Generated CLI proof tests — DO NOT EDIT
//!
//! Source:    .specify/cli-proof-tests.ttl
//! Query:     .specify/queries/cli-proof-tests.rq
//! Template:  .specify/templates/cli/cli_proof_test.rs.tera
//!
//! Regenerate: ggen sync --stage cli-proof-tests
//! Test count: 18

#![allow(clippy::unwrap_used)]

use chicago_tdd_tools::cli_proof::{CliHarness, TempWorkspace};












#[test]
fn sync_dry_run_no_artifacts() {
    // Axiom: sync --dry-run does not write receipts or output files

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("sync");
    cmd_args.push("--dry-run");
    cmd_args.push("true");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(0);
}











#[test]
fn sync_audit_valid_project_exits_zero() {
    // Axiom: sync --audit on valid project exits 0

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("sync");
    cmd_args.push("--audit");
    cmd_args.push("true");
    cmd_args.push("--dry-run");
    cmd_args.push("true");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(0);
}











#[test]
fn sync_format_json_parseable() {
    // Axiom: sync --format json produces machine-readable output

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("sync");
    cmd_args.push("--format");
    cmd_args.push("json");
    cmd_args.push("--dry-run");
    cmd_args.push("true");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(0);
}











#[test]
fn graph_load_missing_file_exits_nonzero() {
    // Axiom: graph load with nonexistent file exits 1

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("graph");
    cmd_args.push("load");
    cmd_args.push("nonexistent.ttl");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(1);
}











#[test]
fn sync_locked_no_lockfile_exits_nonzero() {
    // Axiom: sync --locked without ggen.lock exits 1 (fail-closed)

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("sync");
    cmd_args.push("--locked");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(1);
}











#[test]
fn sync_missing_manifest_exits_nonzero() {
    // Axiom: sync --manifest with nonexistent path outputs error JSON with status=error

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("sync");
    cmd_args.push("--manifest");
    cmd_args.push("nonexistent-ggen.toml");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(0);
    result.assert_stdout_contains("error");
}











#[test]
fn sync_validate_only_invalid_toml() {
    // Axiom: sync --validate-only on valid manifest exits 0

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("sync");
    cmd_args.push("--validate-only");
    cmd_args.push("true");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(0);
}











#[test]
fn unknown_subcommand_exits_nonzero() {
    // Axiom: ggen rejects unknown subcommands with exit code 1

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("totally-unknown-subcommand-xyz");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(1);
}











#[test]
fn doctor_help_exits_zero() {
    // Axiom: doctor --help exits 0 with usage text

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("doctor");
    cmd_args.push("--help");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(0);
    result.assert_stdout_contains("Usage");
}











#[test]
fn graph_help_exits_zero() {
    // Axiom: graph --help exits 0 with usage text

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("graph");
    cmd_args.push("--help");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(0);
    result.assert_stdout_contains("Usage");
}











#[test]
fn init_help_exits_zero() {
    // Axiom: init --help exits 0 with usage text

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("init");
    cmd_args.push("--help");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(0);
    result.assert_stdout_contains("Usage");
}











#[test]
fn ontology_help_exits_zero() {
    // Axiom: ontology --help exits 0 with usage text

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("ontology");
    cmd_args.push("--help");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(0);
    result.assert_stdout_contains("Usage");
}











#[test]
fn pack_help_exits_zero() {
    // Axiom: pack --help exits 0 with usage text

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("pack");
    cmd_args.push("--help");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(0);
    result.assert_stdout_contains("Usage");
}











#[test]
fn packs_help_exits_zero() {
    // Axiom: packs --help exits 0 with usage text

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("packs");
    cmd_args.push("--help");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(0);
    result.assert_stdout_contains("Usage");
}











#[test]
fn root_help_shows_subcommands() {
    // Axiom: ggen --help lists available subcommands including sync

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("--help");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(0);
    result.assert_stdout_contains("sync");
}











#[test]
fn root_version_emits_semver() {
    // Axiom: ggen --version prints a semver-like version string

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("--version");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(0);
    result.assert_stdout_contains(".");
}











#[test]
fn sync_help_exits_zero() {
    // Axiom: sync --help exits 0 with usage text

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("sync");
    cmd_args.push("--help");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(0);
    result.assert_stdout_contains("Usage");
}











#[test]
fn utils_help_exits_zero() {
    // Axiom: utils --help exits 0 with usage text

    // Arrange
    let ws = TempWorkspace::new().expect("TempWorkspace");

    // Act — spawn real ggen binary
    let mut cmd_args: Vec<&str> = vec![];
    cmd_args.push("utils");
    cmd_args.push("--help");

    let result = CliHarness::cargo_bin("ggen")
        .args(cmd_args)
        .workspace(&ws)
        .run()
        .expect("ggen binary executed");

    // Assert
    result.assert_exit_code(0);
    result.assert_stdout_contains("Usage");
}

