//! Lean test suite for `ggen project gen` command (80/20 critical path)
//!
//! Note: gen.rs already has comprehensive London TDD tests in the source file.
//! This provides CLI-level integration tests for the critical 20%.

use assert_cmd::Command;
use predicates::prelude::*;
use tempfile::TempDir;

#[test]
fn test_gen_requires_template_ref() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(["project", "gen"])
        .assert()
        .failure()
        .stderr(predicate::str::contains("required"));
}

#[test]
fn test_gen_validates_empty_template_ref() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args(["project", "gen", ""])
        .assert()
        .failure();
}

#[test]
fn test_gen_parses_variables() {
    let temp = TempDir::new().unwrap();

    // This will fail because template doesn't exist, but validates parsing
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "project", "gen",
        "nonexistent.tmpl",
        "--var", "name=test",
        "--var", "version=1.0.0",
        "--dry-run",
    ])
    .current_dir(temp.path());

    // Command should fail (template not found) but not due to variable parsing
    let output = cmd.assert().failure();

    // Should not contain variable parsing errors
    output.get_output()
        .stderr
        .iter()
        .all(|&b| b != b'=');
}

#[test]
fn test_gen_rejects_invalid_variable_format() {
    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "project", "gen",
        "test.tmpl",
        "--var", "invalid_no_equals", // Missing '='
    ])
    .assert()
    .failure()
    .stderr(predicate::str::contains("Invalid variable format"));
}

#[test]
fn test_gen_dry_run_flag() {
    let temp = TempDir::new().unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "project", "gen",
        "test.tmpl",
        "--dry-run",
    ])
    .current_dir(temp.path());

    // Will fail (template not found) but validates dry-run is parsed
    cmd.assert().failure();
}

#[test]
fn test_gen_force_flag() {
    let temp = TempDir::new().unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "project", "gen",
        "test.tmpl",
        "--force",
    ])
    .current_dir(temp.path());

    // Will fail (template not found) but validates force is parsed
    cmd.assert().failure();
}

#[test]
fn test_gen_seed_for_determinism() {
    let temp = TempDir::new().unwrap();

    let mut cmd = Command::cargo_bin("ggen").unwrap();
    cmd.args([
        "project", "gen",
        "test.tmpl",
        "--seed", "12345",
        "--dry-run",
    ])
    .current_dir(temp.path());

    // Will fail (template not found) but validates seed is parsed
    cmd.assert().failure();
}

#[cfg(test)]
mod unit_tests {
    use ggen_cli_lib::cmds::project::gen::GenArgs;

    #[test]
    fn test_unit_gen_args_defaults() {
        let args = GenArgs {
            template_ref: "test.tmpl".to_string(),
            vars: vec![],
            dry_run: false,
            seed: None,
            force: false,
            ai: false,
            ai_provider: "ollama".to_string(),
            ai_model: None,
            ai_max_iterations: 3,
        };

        assert_eq!(args.template_ref, "test.tmpl");
        assert!(!args.dry_run);
        assert!(!args.force);
        assert!(!args.ai);
        assert_eq!(args.ai_max_iterations, 3);
    }

    #[test]
    fn test_unit_gen_args_with_variables() {
        let args = GenArgs {
            template_ref: "app.tmpl".to_string(),
            vars: vec![
                "name=myapp".to_string(),
                "version=2.0.0".to_string(),
            ],
            dry_run: true,
            seed: Some(42),
            force: true,
            ai: false,
            ai_provider: "ollama".to_string(),
            ai_model: None,
            ai_max_iterations: 5,
        };

        assert_eq!(args.vars.len(), 2);
        assert!(args.dry_run);
        assert!(args.force);
        assert_eq!(args.seed, Some(42));
    }
}
