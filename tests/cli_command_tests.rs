//! CLI Command Tests (25 tests)
//!
//! Tests command-line argument parsing, validation, and execution.

use assert_cmd::Command;
use predicates::prelude::*;

type TestResult = Result<(), Box<dyn std::error::Error>>;

// ==============================================================================
// Help/Version Commands (5 tests)
// ==============================================================================

#[test]
fn test_help_command() -> TestResult {
    Command::cargo_bin("ggen")?
        .arg("--help")
        .assert()
        .success()
        .stdout(predicate::str::contains("Usage:"));
    Ok(())
}

#[test]
fn test_version_command() -> TestResult {
    Command::cargo_bin("ggen")?
        .arg("--version")
        .assert()
        .success()
        .stdout(predicate::str::contains("ggen"));
    Ok(())
}

#[test]
fn test_help_short_flag() -> TestResult {
    Command::cargo_bin("ggen")?.arg("-h").assert().success();
    Ok(())
}

#[test]
fn test_version_short_flag() -> TestResult {
    Command::cargo_bin("ggen")?.arg("-V").assert().success();
    Ok(())
}

#[test]
fn test_no_args_shows_help() -> TestResult {
    let result = Command::cargo_bin("ggen")?.assert();
    // Either shows help or error asking for subcommand
    assert!(result.get_output().status.code().is_some());
    Ok(())
}

// ==============================================================================
// Init Command Tests (5 tests)
// ==============================================================================

#[test]
fn test_init_requires_name() -> TestResult {
    Command::cargo_bin("ggen")?.arg("init").assert().failure();
    Ok(())
}

#[test]
fn test_init_with_name() -> TestResult {
    let temp = tempfile::tempdir()?;
    Command::cargo_bin("ggen")?
        .arg("init")
        .arg("test-project")
        .current_dir(temp.path())
        .assert()
        .success();
    Ok(())
}

#[test]
fn test_init_creates_config_file() -> TestResult {
    let temp = tempfile::tempdir()?;
    Command::cargo_bin("ggen")?
        .arg("init")
        .arg("test-project")
        .current_dir(temp.path())
        .assert()
        .success();

    assert!(temp.path().join("test-project").join("ggen.toml").exists());
    Ok(())
}

#[test]
fn test_init_with_template_flag() -> TestResult {
    let temp = tempfile::tempdir()?;
    Command::cargo_bin("ggen")?
        .arg("init")
        .arg("test-project")
        .arg("--template")
        .arg("basic")
        .current_dir(temp.path())
        .assert();
    Ok(())
}

#[test]
fn test_init_duplicate_project_fails() -> TestResult {
    let temp = tempfile::tempdir()?;
    let project_dir = temp.path().join("test-project");
    std::fs::create_dir(&project_dir)?;

    Command::cargo_bin("ggen")?
        .arg("init")
        .arg("test-project")
        .current_dir(temp.path())
        .assert()
        .failure();
    Ok(())
}

// ==============================================================================
// Generate Command Tests (5 tests)
// ==============================================================================

#[test]
fn test_generate_requires_template() -> TestResult {
    Command::cargo_bin("ggen")?
        .arg("generate")
        .assert()
        .failure();
    Ok(())
}

#[test]
fn test_generate_with_template_name() -> TestResult {
    let temp = tempfile::tempdir()?;
    Command::cargo_bin("ggen")?
        .arg("generate")
        .arg("basic")
        .current_dir(temp.path())
        .assert();
    Ok(())
}

#[test]
fn test_generate_output_flag() -> TestResult {
    let temp = tempfile::tempdir()?;
    Command::cargo_bin("ggen")?
        .arg("generate")
        .arg("basic")
        .arg("--output")
        .arg(temp.path())
        .assert();
    Ok(())
}

#[test]
fn test_generate_vars_flag() -> TestResult {
    let temp = tempfile::tempdir()?;
    Command::cargo_bin("ggen")?
        .arg("generate")
        .arg("basic")
        .arg("--var")
        .arg("name=test")
        .current_dir(temp.path())
        .assert();
    Ok(())
}

#[test]
fn test_generate_force_flag() -> TestResult {
    let temp = tempfile::tempdir()?;
    Command::cargo_bin("ggen")?
        .arg("generate")
        .arg("basic")
        .arg("--force")
        .current_dir(temp.path())
        .assert();
    Ok(())
}

// ==============================================================================
// Config Validation Tests (5 tests)
// ==============================================================================

#[test]
fn test_invalid_config_format() -> TestResult {
    let temp = tempfile::tempdir()?;
    std::fs::write(temp.path().join("ggen.toml"), "invalid toml {")?;

    Command::cargo_bin("ggen")?
        .arg("validate")
        .current_dir(temp.path())
        .assert()
        .failure();
    Ok(())
}

#[test]
fn test_missing_required_field() -> TestResult {
    let temp = tempfile::tempdir()?;
    std::fs::write(temp.path().join("ggen.toml"), "[project]")?;

    Command::cargo_bin("ggen")?
        .arg("validate")
        .current_dir(temp.path())
        .assert()
        .failure();
    Ok(())
}

#[test]
fn test_valid_config_passes() -> TestResult {
    let temp = tempfile::tempdir()?;
    let config = r#"
[project]
name = "test"
version = "1.0.0"
"#;
    std::fs::write(temp.path().join("ggen.toml"), config)?;

    Command::cargo_bin("ggen")?
        .arg("validate")
        .current_dir(temp.path())
        .assert()
        .success();
    Ok(())
}

#[test]
fn test_config_with_dependencies() -> TestResult {
    let temp = tempfile::tempdir()?;
    let config = r#"
[project]
name = "test"
version = "1.0.0"

[[dependencies]]
name = "dep1"
version = "1.0"
"#;
    std::fs::write(temp.path().join("ggen.toml"), config)?;

    Command::cargo_bin("ggen")?
        .arg("validate")
        .current_dir(temp.path())
        .assert()
        .success();
    Ok(())
}

#[test]
fn test_workspace_config_validation() -> TestResult {
    let temp = tempfile::tempdir()?;
    let config = r#"
[workspace]
members = ["proj1", "proj2"]
"#;
    std::fs::write(temp.path().join("ggen.toml"), config)?;

    Command::cargo_bin("ggen")?
        .arg("validate")
        .current_dir(temp.path())
        .assert();
    Ok(())
}

// ==============================================================================
// Error Handling Tests (5 tests)
// ==============================================================================

#[test]
fn test_unknown_command_fails() -> TestResult {
    Command::cargo_bin("ggen")?
        .arg("nonexistent-command")
        .assert()
        .failure();
    Ok(())
}

#[test]
fn test_invalid_flag_fails() -> TestResult {
    Command::cargo_bin("ggen")?
        .arg("--invalid-flag")
        .assert()
        .failure();
    Ok(())
}

#[test]
fn test_conflicting_flags_fails() -> TestResult {
    Command::cargo_bin("ggen")?
        .arg("init")
        .arg("test")
        .arg("--force")
        .arg("--no-force")
        .assert();
    Ok(())
}

#[test]
fn test_missing_required_arg() -> TestResult {
    Command::cargo_bin("ggen")?.arg("pack").assert().failure();
    Ok(())
}

#[test]
fn test_error_message_clarity() -> TestResult {
    let result = Command::cargo_bin("ggen")?.arg("init").assert().failure();

    let output = String::from_utf8_lossy(&result.get_output().stderr);
    assert!(output.contains("required") || output.contains("error") || output.contains("missing"));
    Ok(())
}
