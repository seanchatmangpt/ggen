//! Swarm Security Tests
//!
//! Tests for command injection prevention, permission validation, and secure execution.
//! Covers critical security scenarios that protect against common attack vectors.

use ggen_core::security::command::{CommandError, CommandExecutor, SafeCommand};
use std::path::Path;

/// Test command whitelist enforcement
#[test]
fn test_command_whitelist() {
    // Allowed commands
    assert!(SafeCommand::new("git").is_ok());
    assert!(SafeCommand::new("cargo").is_ok());
    assert!(SafeCommand::new("npm").is_ok());
    assert!(SafeCommand::new("rustc").is_ok());

    // Disallowed commands
    assert!(SafeCommand::new("rm").is_err());
    assert!(SafeCommand::new("sh").is_err());
    assert!(SafeCommand::new("bash").is_err());
    assert!(SafeCommand::new("python").is_err());
}

/// Test command injection prevention via command name
#[test]
fn test_command_injection_via_name() {
    // Attempt to inject via semicolon
    assert!(SafeCommand::new("git; rm -rf /").is_err());

    // Attempt to inject via pipe
    assert!(SafeCommand::new("git | cat /etc/passwd").is_err());

    // Attempt to inject via ampersand
    assert!(SafeCommand::new("git && whoami").is_err());

    // Attempt to inject via backticks
    assert!(SafeCommand::new("git `whoami`").is_err());

    // Attempt to inject via dollar sign
    assert!(SafeCommand::new("git $(whoami)").is_err());
}

/// Test command injection prevention via arguments
#[test]
fn test_command_injection_via_arguments() {
    let cmd = SafeCommand::new("git").unwrap();

    // Semicolon injection
    assert!(cmd.clone().arg("init; rm -rf /").is_err());

    // Pipe injection
    assert!(cmd.clone().arg("init | cat").is_err());

    // Ampersand injection
    assert!(cmd.clone().arg("init && ls").is_err());

    // Backtick injection
    assert!(cmd.clone().arg("`whoami`").is_err());

    // Dollar expansion injection
    assert!(cmd.clone().arg("$(whoami)").is_err());

    // Redirect injection
    assert!(cmd.clone().arg("init > /etc/passwd").is_err());
    assert!(cmd.clone().arg("init < /etc/passwd").is_err());
}

/// Test dangerous metacharacter detection
#[test]
fn test_dangerous_metacharacters() {
    let dangerous_chars = vec![
        ';', '|', '&', '$', '`', '\n', '\r', '<', '>', '(', ')', '{', '}',
    ];

    for ch in dangerous_chars {
        let malicious_cmd = format!("git{}ls", ch);
        assert!(
            SafeCommand::new(&malicious_cmd).is_err(),
            "Should reject command with dangerous char: {}",
            ch
        );

        let cmd = SafeCommand::new("git").unwrap();
        let malicious_arg = format!("init{}ls", ch);
        assert!(
            cmd.arg(&malicious_arg).is_err(),
            "Should reject argument with dangerous char: {}",
            ch
        );
    }
}

/// Test safe argument handling
#[test]
fn test_safe_argument_handling() {
    let cmd = SafeCommand::new("git").unwrap();

    // Safe arguments should work
    assert!(cmd.clone().arg("init").is_ok());
    assert!(cmd.clone().arg("status").is_ok());
    assert!(cmd.clone().arg("--version").is_ok());
    assert!(cmd.clone().arg("--help").is_ok());

    // Multiple safe arguments
    let multi_cmd = SafeCommand::new("git")
        .unwrap()
        .arg("log")
        .unwrap()
        .arg("--oneline")
        .unwrap();
    assert!(multi_cmd.args(vec!["--max-count", "10"]).is_ok());
}

/// Test empty command rejection
#[test]
fn test_empty_command_rejection() {
    assert!(SafeCommand::new("").is_err());
}

/// Test command executor helpers
#[test]
fn test_command_executor_git() {
    // Test git helper (will succeed if git is installed)
    let result = CommandExecutor::git(&["--version"]);
    // Just verify it returns a Result type
    let _ = result;
}

#[test]
fn test_command_executor_cargo() {
    // Test cargo helper
    let result = CommandExecutor::cargo(&["--version"]);
    let _ = result;
}

#[test]
fn test_command_executor_npm() {
    // Test npm helper (may fail if npm not installed)
    let result = CommandExecutor::npm(&["--version"]);
    let _ = result;
}

/// Test directory traversal prevention in commands
#[test]
fn test_directory_traversal_in_args() {
    let cmd = SafeCommand::new("git").unwrap();

    // Paths with .. should be rejected if they contain dangerous chars
    // Note: Simple paths are allowed, but shell metacharacters are not
    assert!(cmd.clone().arg("../safe-path").is_ok());

    // But injection attempts should still fail
    assert!(cmd.clone().arg("../; rm -rf /").is_err());
}

/// Test multiple argument validation
#[test]
fn test_multiple_arguments() {
    let safe_args = vec!["log", "--oneline", "--max-count", "5"];
    let cmd = SafeCommand::new("git").unwrap();
    assert!(cmd.args(safe_args).is_ok());

    let unsafe_args = vec!["log", "; rm -rf /"];
    let cmd2 = SafeCommand::new("git").unwrap();
    assert!(cmd2.args(unsafe_args).is_err());
}

/// Test real-world safe command patterns
#[test]
fn test_real_world_safe_patterns() {
    // Git init
    let git_init = SafeCommand::new("git").unwrap().arg("init");
    assert!(git_init.is_ok());

    // Git status
    let git_status = SafeCommand::new("git")
        .unwrap()
        .arg("status")
        .unwrap()
        .arg("--short");
    assert!(git_status.is_ok());

    // Cargo build
    let cargo_build = SafeCommand::new("cargo")
        .unwrap()
        .arg("build")
        .unwrap()
        .arg("--release");
    assert!(cargo_build.is_ok());

    // Cargo test
    let cargo_test = SafeCommand::new("cargo").unwrap().arg("test");
    assert!(cargo_test.is_ok());
}

/// Test real-world attack patterns
#[test]
fn test_real_world_attack_patterns() {
    // Classic injection: ; rm -rf /
    let cmd1 = SafeCommand::new("git").unwrap().arg("init; rm -rf /");
    assert!(cmd1.is_err());

    // Piped command: | cat /etc/passwd
    let cmd2 = SafeCommand::new("git")
        .unwrap()
        .arg("status | cat /etc/passwd");
    assert!(cmd2.is_err());

    // Command substitution: $(whoami)
    let cmd3 = SafeCommand::new("git").unwrap().arg("init $(whoami)");
    assert!(cmd3.is_err());

    // Backtick substitution: `whoami`
    let cmd4 = SafeCommand::new("git").unwrap().arg("init `whoami`");
    assert!(cmd4.is_err());

    // Chained commands: && whoami
    let cmd5 = SafeCommand::new("git").unwrap().arg("init && whoami");
    assert!(cmd5.is_err());
}

/// Test error message quality
#[test]
fn test_error_messages() {
    // Test that errors contain helpful information
    let err1 = SafeCommand::new("rm").unwrap_err();
    assert!(err1.to_string().contains("not in allowed list"));

    let cmd = SafeCommand::new("git").unwrap();
    let err2 = cmd.arg("; rm -rf /").unwrap_err();
    assert!(err2.to_string().contains("dangerous characters"));
}

/// Test command cloning
#[test]
fn test_command_cloning() {
    let cmd = SafeCommand::new("git").unwrap();
    let cmd1 = cmd.clone().arg("init");
    let cmd2 = cmd.clone().arg("status");

    assert!(cmd1.is_ok());
    assert!(cmd2.is_ok());
}

/// Test working directory validation
#[test]
fn test_current_dir_validation() {
    let cmd = SafeCommand::new("git").unwrap();

    // Valid directory
    let temp_dir = std::env::temp_dir();
    assert!(cmd.clone().current_dir(&temp_dir).is_ok());

    // Non-existent directory should fail
    let fake_dir = Path::new("/nonexistent/directory/path");
    assert!(cmd.clone().current_dir(fake_dir).is_err());
}

/// Benchmark: Validation overhead
#[test]
fn test_validation_performance() {
    let iterations = 1000;
    let start = std::time::Instant::now();

    for _ in 0..iterations {
        let _ = SafeCommand::new("git")
            .unwrap()
            .arg("status")
            .unwrap()
            .arg("--short")
            .unwrap();
    }

    let duration = start.elapsed();
    let avg_micros = duration.as_micros() / iterations;

    // Validation should be fast (<100μs per command)
    assert!(avg_micros < 100, "Validation too slow: {}μs", avg_micros);
}

/// Test CommandError conversion
#[test]
fn test_command_error_conversion() {
    use ggen_utils::error::Error;

    let cmd_err = CommandError::NotAllowed("test".to_string());
    let generic_err: Error = cmd_err.into();
    assert!(generic_err.to_string().contains("not allowed"));
}
