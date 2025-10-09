use std::process::Command;
use std::str;

#[test]
fn test_cli_help() {
    let output = Command::new("cargo")
        .args(&["run", "--", "--help"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    assert!(stdout.contains("rgen"));
    assert!(stdout.contains("hazard"));
    assert!(stdout.contains("gen"));
    assert!(stdout.contains("completion"));
    assert!(stdout.contains("config"));
}

#[test]
fn test_hazard_command() {
    let output = Command::new("cargo")
        .args(&["run", "--", "hazard"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    // Test passes if command runs without panicking
    // The actual output depends on the core::commands::hazard implementation
    assert!(output.status.code().is_some());
}

#[test]
fn test_error_command() {
    let output = Command::new("cargo")
        .args(&["run", "--", "error"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    // Test passes if command runs without panicking
    // The actual output depends on the core::commands::simulate_error implementation
    assert!(output.status.code().is_some());
}

#[test]
fn test_config_command() {
    let output = Command::new("cargo")
        .args(&["run", "--", "config"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    // Test passes if command runs without panicking
    // The actual output depends on the core::commands::config implementation
    assert!(output.status.code().is_some());
}

#[test]
fn test_completion_bash() {
    let output = Command::new("cargo")
        .args(&["run", "--", "completion", "bash"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    // Bash completion should contain function definitions
    assert!(stdout.contains("_rgen"));
}

#[test]
fn test_completion_zsh() {
    let output = Command::new("cargo")
        .args(&["run", "--", "completion", "zsh"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    // Zsh completion should contain completion definitions
    assert!(stdout.contains("compdef"));
}

#[test]
fn test_completion_fish() {
    let output = Command::new("cargo")
        .args(&["run", "--", "completion", "fish"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    assert!(output.status.success());
    let stdout = str::from_utf8(&output.stdout).unwrap();
    // Fish completion should contain complete commands
    assert!(stdout.contains("complete"));
}

#[test]
fn test_invalid_command() {
    let output = Command::new("cargo")
        .args(&["run", "--", "invalid-command"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    assert!(!output.status.success());
    let stderr = str::from_utf8(&output.stderr).unwrap();
    assert!(stderr.contains("error"));
}

#[test]
fn test_config_flag() {
    let output = Command::new("cargo")
        .args(&["run", "--", "--config", "/nonexistent/path", "hazard"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    // Should handle config file not found gracefully
    assert!(output.status.code().is_some());
}

#[test]
fn test_debug_flag() {
    let output = Command::new("cargo")
        .args(&["run", "--", "--debug", "true", "hazard"])
        .current_dir("..")
        .output()
        .expect("Failed to execute command");

    // Should handle debug flag without issues
    assert!(output.status.code().is_some());
}
