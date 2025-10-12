//! Integration tests for LocalBackend
//!
//! Tests real command execution with LocalBackend to verify stdout/stderr
//! capture, exit code handling, and environment variable passing.

use cleanroom::backend::{Backend, LocalBackend, Cmd};
use crate::common::fixtures::*;

#[test]
fn local_backend_executes_echo_command_successfully() {
    // Given: A LocalBackend
    let backend = LocalBackend::new();

    // When: Executing an echo command
    let cmd = echo_command();
    let result = backend.run_cmd(cmd).unwrap();

    // Then: The command executes successfully
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("hello world"));
    assert!(result.stderr.is_empty());
}

#[test]
fn local_backend_captures_stdout_and_stderr() {
    // Given: A LocalBackend
    let backend = LocalBackend::new();

    // When: Executing a command that outputs to both stdout and stderr
    let cmd = Cmd::new("sh")
        .args(["-c", "echo 'stdout message' >&1; echo 'stderr message' >&2"]);
    let result = backend.run_cmd(cmd).unwrap();

    // Then: Both stdout and stderr are captured
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("stdout message"));
    assert!(result.stderr.contains("stderr message"));
}

#[test]
fn local_backend_handles_exit_codes_correctly() {
    // Given: A LocalBackend
    let backend = LocalBackend::new();

    // When: Executing a failing command
    let cmd = failing_command();
    let result = backend.run_cmd(cmd).unwrap();

    // Then: The exit code is captured correctly
    assert_eq!(result.exit_code, 1);
    assert!(result.stdout.is_empty());
}

#[test]
fn local_backend_passes_environment_variables() {
    // Given: A LocalBackend
    let backend = LocalBackend::new();

    // When: Executing a command with environment variables
    let cmd = env_command();
    let result = backend.run_cmd(cmd).unwrap();

    // Then: Environment variables are passed correctly
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("test_value"));
}

#[test]
fn local_backend_handles_working_directory() {
    // Given: A LocalBackend
    let backend = LocalBackend::new();

    // When: Executing a command with a specific working directory
    let cmd = Cmd::new("pwd").workdir("/tmp");
    let result = backend.run_cmd(cmd).unwrap();

    // Then: The working directory is set correctly
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("/tmp"));
}

#[test]
fn local_backend_handles_nonexistent_command() {
    // Given: A LocalBackend
    let backend = LocalBackend::new();

    // When: Executing a nonexistent command
    let cmd = Cmd::new("nonexistent_command_12345");
    let result = backend.run_cmd(cmd);

    // Then: An error is returned
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert!(error.to_string().contains("failed to execute"));
}

#[test]
fn local_backend_measures_execution_time() {
    // Given: A LocalBackend
    let backend = LocalBackend::new();

    // When: Executing a command that takes some time
    let cmd = Cmd::new("sleep").args(["0.1"]);
    let result = backend.run_cmd(cmd).unwrap();

    // Then: Execution time is measured
    assert_eq!(result.exit_code, 0);
    assert!(result.duration_ms > 0);
    assert!(result.duration_ms >= 100); // At least 100ms for sleep 0.1
}

#[test]
fn local_backend_handles_multiple_arguments() {
    // Given: A LocalBackend
    let backend = LocalBackend::new();

    // When: Executing a command with multiple arguments
    let cmd = Cmd::new("echo")
        .args(["arg1", "arg2", "arg3"]);
    let result = backend.run_cmd(cmd).unwrap();

    // Then: All arguments are passed correctly
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("arg1 arg2 arg3"));
}

#[test]
fn local_backend_handles_empty_arguments() {
    // Given: A LocalBackend
    let backend = LocalBackend::new();

    // When: Executing a command with no arguments
    let cmd = Cmd::new("echo");
    let result = backend.run_cmd(cmd).unwrap();

    // Then: The command executes successfully
    assert_eq!(result.exit_code, 0);
    assert_eq!(result.stdout.trim(), "");
}

#[test]
fn local_backend_handles_special_characters_in_output() {
    // Given: A LocalBackend
    let backend = LocalBackend::new();

    // When: Executing a command that outputs special characters
    let cmd = Cmd::new("printf")
        .args(["'Special chars: \\n\\t\\r\\0\\x1b[31mred\\x1b[0m'"]);
    let result = backend.run_cmd(cmd).unwrap();

    // Then: Special characters are captured correctly
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("Special chars"));
    assert!(result.stdout.contains("red"));
}

#[test]
fn local_backend_handles_large_output() {
    // Given: A LocalBackend
    let backend = LocalBackend::new();

    // When: Executing a command that produces large output
    let cmd = Cmd::new("seq").args(["1", "1000"]);
    let result = backend.run_cmd(cmd).unwrap();

    // Then: Large output is captured correctly
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("1"));
    assert!(result.stdout.contains("1000"));
    assert!(result.stdout.len() > 1000); // Should be much larger than 1000 chars
}

#[test]
fn local_backend_handles_unicode_output() {
    // Given: A LocalBackend
    let backend = LocalBackend::new();

    // When: Executing a command that outputs Unicode characters
    let cmd = Cmd::new("printf")
        .args(["'Unicode: 🚀 🌟 💻 🎉'"]);
    let result = backend.run_cmd(cmd).unwrap();

    // Then: Unicode characters are captured correctly
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("🚀"));
    assert!(result.stdout.contains("🌟"));
    assert!(result.stdout.contains("💻"));
    assert!(result.stdout.contains("🎉"));
}
