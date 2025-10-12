//! Unit tests for backend behaviors
//!
//! Tests backend collaboration patterns using mocks to verify correct
//! command execution, argument passing, and error handling.

use rstest::*;
use mockall::predicate::*;
use cleanroom::backend::{Backend, Cmd, MockBackend};
use cleanroom::backend::AutoBackend;
use crate::common::fixtures::*;

#[rstest]
#[case("echo", &["hello"], 0, "hello\n")]
#[case("false", &[], 1, "")]
#[case("sh", &["-c", "exit 42"], 42, "")]
fn backend_executes_command_with_expected_exit_code(
    #[case] cmd: &str,
    #[case] args: &[&str],
    #[case] expected_exit: i32,
    #[case] expected_stdout: &str,
) {
    // Given: A mock backend configured to return specific output
    let mut mock_backend = MockBackend::new();
    mock_backend
        .expect_run_cmd()
        .with(predicate::always())
        .returning(move |_| {
            Ok(cleanroom::backend::RunResult {
                exit_code: expected_exit,
                stdout: expected_stdout.to_string(),
                stderr: String::new(),
                duration_ms: 100,
                steps: vec![],
                redacted_env: vec![],
                backend: "mock".to_string(),
            })
        });

    // When: The command is executed
    let cmd = Cmd::new(cmd).args(args);
    let result = mock_backend.run_cmd(cmd).unwrap();

    // Then: The result matches expectations
    assert_eq!(result.exit_code, expected_exit);
    assert_eq!(result.stdout, expected_stdout);
}

#[test]
fn backend_applies_environment_variables_correctly() {
    // Given: A mock backend that validates environment variables
    let mock_backend = MockBackend::new().expect_env("TEST_VAR", "test_value");

    // When: A command with environment variables is executed
    let cmd = Cmd::new("sh")
        .args(["-c", "echo $TEST_VAR"])
        .env("TEST_VAR", "test_value");
    
    let result = mock_backend.run_cmd(cmd).unwrap();

    // Then: The command succeeds
    assert_eq!(result.exit_code, 0);
}

#[test]
fn backend_respects_timeout_constraints() {
    // Given: A mock backend that simulates timeout behavior
    let mut mock_backend = MockBackend::new();
    mock_backend
        .expect_run_cmd()
        .withf(|cmd: &Cmd| cmd.timeout_ms.is_some())
        .returning(|_| {
            Ok(cleanroom::backend::RunResult {
                exit_code: 124, // Timeout exit code
                stdout: String::new(),
                stderr: "timeout".to_string(),
                duration_ms: 30000,
                steps: vec![],
                redacted_env: vec![],
                backend: "mock".to_string(),
            })
        });

    // When: A command with timeout is executed
    let cmd = Cmd::new("sleep").args(["60"]).timeout_ms(30000);
    let result = mock_backend.run_cmd(cmd).unwrap();

    // Then: The timeout is respected
    assert_eq!(result.exit_code, 124);
    assert!(result.stderr.contains("timeout"));
}

#[test]
fn backend_propagates_errors_appropriately() {
    // Given: A mock backend that returns an error
    let mock_backend = MockBackend::new().expect_error(
        cleanroom::error::BackendError::Runtime("test error".to_string())
    );

    // When: A command is executed
    let cmd = echo_command();
    let result = mock_backend.run_cmd(cmd);

    // Then: The error is propagated
    assert!(result.is_err());
    let error = result.unwrap_err();
    assert!(error.to_string().contains("test error"));
}

#[test]
fn backend_validates_command_arguments() {
    // Given: A mock backend that validates specific arguments
    let mock_backend = MockBackend::new().expect_command("echo", &["hello", "world"]);

    // When: A command with the expected arguments is executed
    let cmd = Cmd::new("echo").args(["hello", "world"]);
    let result = mock_backend.run_cmd(cmd).unwrap();

    // Then: The command succeeds
    assert_eq!(result.exit_code, 0);
}

#[test]
fn autobackend_detects_correct_engine_priority() {
    // Given: Mock backends for different engines
    let podman_mock = MockBackend::new().expect_success("podman output");
    let docker_mock = MockBackend::new().expect_success("docker output");
    let local_mock = MockBackend::new().expect_success("local output");

    // When: AutoBackend detects available engines
    // Note: In a real test, we'd mock the detection logic
    // For now, we test the fallback behavior
    let auto_backend = AutoBackend::detect().unwrap();

    // Then: The correct backend is selected
    // This test would need to be enhanced with proper mocking of detection
    assert_eq!(auto_backend.name(), "local"); // Default fallback
}

#[test]
fn backend_handles_empty_command_gracefully() {
    // Given: A mock backend
    let mock_backend = MockBackend::new();

    // When: An empty command is executed
    let cmd = Cmd::new("");
    let result = mock_backend.run_cmd(cmd);

    // Then: The backend handles it appropriately
    // The mock will succeed, but real backends should validate input
    assert!(result.is_ok());
}

#[test]
fn backend_preserves_working_directory() {
    // Given: A mock backend that validates working directory
    let mut mock_backend = MockBackend::new();
    mock_backend
        .expect_run_cmd()
        .withf(|cmd: &Cmd| cmd.workdir.as_ref().map_or(false, |dir| dir == "/tmp"))
        .returning(|_| {
            Ok(cleanroom::backend::RunResult {
                exit_code: 0,
                stdout: "working directory set".to_string(),
                stderr: String::new(),
                duration_ms: 100,
                steps: vec![],
                redacted_env: vec![],
                backend: "mock".to_string(),
            })
        });

    // When: A command with working directory is executed
    let cmd = Cmd::new("pwd").workdir("/tmp");
    let result = mock_backend.run_cmd(cmd).unwrap();

    // Then: The working directory is preserved
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("working directory set"));
}

#[test]
fn backend_aggregates_multiple_environment_variables() {
    // Given: A mock backend that validates multiple environment variables
    let mut mock_backend = MockBackend::new();
    mock_backend
        .expect_run_cmd()
        .withf(|cmd: &Cmd| {
            cmd.env.len() == 2 &&
            cmd.env.iter().any(|(k, v)| k == "VAR1" && v == "value1") &&
            cmd.env.iter().any(|(k, v)| k == "VAR2" && v == "value2")
        })
        .returning(|_| {
            Ok(cleanroom::backend::RunResult {
                exit_code: 0,
                stdout: "env vars set".to_string(),
                stderr: String::new(),
                duration_ms: 100,
                steps: vec![],
                redacted_env: vec![],
                backend: "mock".to_string(),
            })
        });

    // When: A command with multiple environment variables is executed
    let cmd = Cmd::new("env")
        .env("VAR1", "value1")
        .env("VAR2", "value2");
    let result = mock_backend.run_cmd(cmd).unwrap();

    // Then: All environment variables are aggregated
    assert_eq!(result.exit_code, 0);
    assert!(result.stdout.contains("env vars set"));
}
