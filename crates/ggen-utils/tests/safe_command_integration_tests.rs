//! Integration tests for SafeCommand with real command execution
//!
//! These tests verify that SafeCommand works correctly with actual std::process::Command
//! execution. They use safe, read-only commands that don't modify the system.

use ggen_utils::safe_command::SafeCommand;
use ggen_utils::safe_path::SafePath;
use std::process::Stdio;

// ============================================================================
// Real Command Execution Tests
// ============================================================================

#[test]
fn test_execute_cargo_version() {
    // Arrange
    let safe_cmd = SafeCommand::new("cargo")
        .unwrap()
        .arg("--version")
        .unwrap()
        .validate()
        .unwrap();

    // Act
    let mut cmd = safe_cmd.into_command();
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());

    let output = cmd.output();

    // Assert
    assert!(output.is_ok(), "Should execute cargo --version");
    let output = output.unwrap();
    assert!(output.status.success(), "Command should succeed");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("cargo"),
        "Output should contain 'cargo': {}",
        stdout
    );
}

#[test]
fn test_execute_git_version() {
    // Arrange
    let safe_cmd = SafeCommand::new("git")
        .unwrap()
        .arg("--version")
        .unwrap()
        .validate()
        .unwrap();

    // Act
    let mut cmd = safe_cmd.into_command();
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());

    let output = cmd.output();

    // Assert
    assert!(output.is_ok(), "Should execute git --version");
    let output = output.unwrap();
    assert!(output.status.success(), "Command should succeed");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("git"),
        "Output should contain 'git': {}",
        stdout
    );
}

#[test]
fn test_execute_rustc_version() {
    // Arrange
    let safe_cmd = SafeCommand::new("rustc")
        .unwrap()
        .arg("--version")
        .unwrap()
        .validate()
        .unwrap();

    // Act
    let mut cmd = safe_cmd.into_command();
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());

    let output = cmd.output();

    // Assert
    assert!(output.is_ok(), "Should execute rustc --version");
    let output = output.unwrap();
    assert!(output.status.success(), "Command should succeed");

    let stdout = String::from_utf8_lossy(&output.stdout);
    assert!(
        stdout.contains("rustc"),
        "Output should contain 'rustc': {}",
        stdout
    );
}

#[test]
#[cfg(unix)]
fn test_execute_timeout_command() {
    // Arrange - timeout 1s should work
    let safe_cmd = SafeCommand::new("timeout")
        .unwrap()
        .arg("1s")
        .unwrap()
        .arg("echo")
        .unwrap()
        .arg("test")
        .unwrap()
        .validate()
        .unwrap();

    // Act
    let mut cmd = safe_cmd.into_command();
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());

    let output = cmd.output();

    // Assert
    assert!(output.is_ok(), "Should execute timeout command");
    // Note: timeout may or may not be installed, so we don't assert success
}

#[test]
fn test_execute_invalid_arg_fails() {
    // Arrange - valid command but invalid argument
    let safe_cmd = SafeCommand::new("cargo")
        .unwrap()
        .arg("invalid-subcommand-xyz")
        .unwrap()
        .validate()
        .unwrap();

    // Act
    let mut cmd = safe_cmd.into_command();
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());

    let output = cmd.output();

    // Assert
    assert!(output.is_ok(), "Command should execute (but may fail)");
    let output = output.unwrap();
    // cargo with invalid subcommand should exit with error
    assert!(!output.status.success(), "Invalid subcommand should fail");
}

// ============================================================================
// SafePath Integration Tests
// ============================================================================

#[test]
#[cfg(unix)]
fn test_execute_with_safe_path() {
    // Arrange - use a safe path to /dev/null (safe to read on Unix)
    let safe_path = SafePath::new_absolute("/dev/null").unwrap();

    let safe_cmd = SafeCommand::new("git")
        .unwrap()
        .arg("hash-object")
        .unwrap()
        .arg_path(&safe_path)
        .validate()
        .unwrap();

    // Act
    let mut cmd = safe_cmd.into_command();
    cmd.stdout(Stdio::piped()).stderr(Stdio::piped());

    let output = cmd.output();

    // Assert
    assert!(output.is_ok(), "Should execute with SafePath");
    let output = output.unwrap();
    assert!(output.status.success(), "Command should succeed");
}

// ============================================================================
// Builder Pattern Tests
// ============================================================================

#[test]
fn test_builder_pattern_chaining() {
    // Arrange & Act
    let result = SafeCommand::new("cargo")
        .unwrap()
        .arg("build")
        .unwrap()
        .arg("--release")
        .unwrap()
        .arg("--all-features")
        .unwrap()
        .validate();

    // Assert
    assert!(result.is_ok(), "Builder pattern should chain correctly");
    let cmd = result.unwrap();
    assert_eq!(cmd.to_string_debug(), "cargo build --release --all-features");
}

#[test]
fn test_builder_pattern_with_args_bulk() {
    // Arrange & Act
    let result = SafeCommand::new("cargo")
        .unwrap()
        .args(&["build", "--release", "--all-features"])
        .unwrap()
        .validate();

    // Assert
    assert!(result.is_ok(), "Bulk args should work");
    let cmd = result.unwrap();
    assert_eq!(cmd.to_string_debug(), "cargo build --release --all-features");
}

#[test]
fn test_builder_pattern_mixed_arg_types() {
    // Arrange
    let path = SafePath::new("src/main.rs").unwrap();

    // Act
    let result = SafeCommand::new("rustfmt")
        .unwrap()
        .arg("--check")
        .unwrap()
        .arg_path(&path)
        .validate();

    // Assert
    assert!(result.is_ok(), "Mixed arg types should work");
    let cmd = result.unwrap();
    let debug_str = cmd.to_string_debug();
    assert!(debug_str.contains("rustfmt"));
    assert!(debug_str.contains("--check"));
    assert!(debug_str.contains("src/main.rs"));
}

// ============================================================================
// Type-State Pattern Tests
// ============================================================================

#[test]
fn test_type_state_unvalidated_to_validated() {
    // Arrange - create unvalidated command
    let unvalidated = SafeCommand::new("cargo").unwrap().arg("build").unwrap();

    // Act - validate
    let validated = unvalidated.validate();

    // Assert
    assert!(validated.is_ok(), "Should transition to validated state");
}

#[test]
fn test_type_state_validated_has_accessors() {
    // Arrange
    let validated = SafeCommand::new("cargo")
        .unwrap()
        .arg("build")
        .unwrap()
        .validate()
        .unwrap();

    // Act - use validated-only methods
    let command = validated.command();
    let args = validated.args();
    let debug_str = validated.to_string_debug();

    // Assert
    assert_eq!(command.as_str(), "cargo");
    assert_eq!(args.len(), 1);
    assert_eq!(args[0].as_str(), "build");
    assert_eq!(debug_str, "cargo build");
}

// Note: The following test won't compile, which is the point - demonstrating type safety
// #[test]
// fn test_type_state_cannot_execute_unvalidated() {
//     let unvalidated = SafeCommand::new("cargo").unwrap();
//     let _cmd = unvalidated.into_command(); // ❌ Compile error: method not available
// }
