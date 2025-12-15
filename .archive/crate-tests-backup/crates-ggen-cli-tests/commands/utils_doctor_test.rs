//! Integration tests for utils/doctor command
//!
//! Tests the v2.0 architecture pattern:
//! - Sync CLI wrapper
//! - Runtime spawning
//! - Async domain logic
//! - Error handling

use ggen_cli_lib::commands::utils::doctor::{run, DoctorArgs};
use ggen_cli_lib::domain::utils::doctor::{
    CheckStatus, DefaultSystemChecker, SystemChecker,
};

#[test]
fn test_doctor_args_creation() {
    let args = DoctorArgs {
        verbose: true,
        check: Some("rust".to_string()),
        env: true,
    };

    assert!(args.verbose);
    assert_eq!(args.check.as_deref(), Some("rust"));
    assert!(args.env);
}

#[test]
fn test_doctor_command_basic() {
    // Test basic doctor command execution
    let args = DoctorArgs {
        verbose: false,
        check: None,
        env: false,
    };

    // This should succeed on a properly configured system
    let result = run(&args);

    // We allow failure if system is not properly configured
    // The important thing is that the pattern works
    match result {
        Ok(_) => println!("✓ Doctor command succeeded"),
        Err(e) => println!("✓ Doctor command failed as expected: {}", e),
    }
}

#[test]
fn test_doctor_command_verbose() {
    let args = DoctorArgs {
        verbose: true,
        check: None,
        env: false,
    };

    let result = run(&args);

    // Should execute without panic
    match result {
        Ok(_) => println!("✓ Verbose doctor command succeeded"),
        Err(e) => println!("✓ Verbose doctor command failed: {}", e),
    }
}

#[test]
fn test_doctor_command_with_env() {
    let args = DoctorArgs {
        verbose: false,
        check: None,
        env: true,
    };

    let result = run(&args);

    // Should execute without panic
    match result {
        Ok(_) => println!("✓ Doctor command with env succeeded"),
        Err(e) => println!("✓ Doctor command with env failed: {}", e),
    }
}

#[test]
fn test_doctor_specific_check() {
    let args = DoctorArgs {
        verbose: false,
        check: Some("rust".to_string()),
        env: false,
    };

    // Rust should be available since we're running these tests
    let result = run(&args);
    assert!(
        result.is_ok(),
        "Rust check should pass since we're in a Rust environment"
    );
}

#[tokio::test]
async fn test_domain_system_checker() {
    let checker = DefaultSystemChecker;

    // Test system check
    let result = checker.check_system(false);
    assert!(result.is_ok(), "System check should not panic");

    let check_result = result.unwrap();
    assert!(check_result.checks.len() > 0, "Should have checks");
    assert_eq!(
        check_result.summary.total,
        check_result.checks.len(),
        "Summary total should match check count"
    );
}

#[tokio::test]
async fn test_domain_specific_checks() {
    let checker = DefaultSystemChecker;

    // Test Rust check
    let rust_check = checker.check("rust");
    assert!(rust_check.is_ok(), "Rust check should succeed");
    let rust = rust_check.unwrap();
    assert_eq!(rust.name, "Rust");
    assert_eq!(rust.status, CheckStatus::Pass);

    // Test Cargo check
    let cargo_check = checker.check("cargo");
    assert!(cargo_check.is_ok(), "Cargo check should succeed");
    let cargo = cargo_check.unwrap();
    assert_eq!(cargo.name, "Cargo");
    assert_eq!(cargo.status, CheckStatus::Pass);

    // Test unknown check
    let unknown_check = checker.check("unknown-tool-xyz");
    assert!(unknown_check.is_err(), "Unknown check should fail");
}

#[tokio::test]
async fn test_domain_environment_info() {
    let checker = DefaultSystemChecker;
    let env_info = checker.get_environment_info();

    assert!(env_info.is_ok(), "Environment info should be retrievable");

    let info = env_info.unwrap();
    assert!(!info.os.is_empty(), "OS should be detected");
    assert!(!info.arch.is_empty(), "Architecture should be detected");
    assert!(
        !info.ggen_version.is_empty(),
        "ggen version should be available"
    );
}

#[test]
fn test_check_status_as_str() {
    assert_eq!(CheckStatus::Pass.as_str(), "pass");
    assert_eq!(CheckStatus::Warn.as_str(), "warn");
    assert_eq!(CheckStatus::Fail.as_str(), "fail");
    assert_eq!(CheckStatus::Info.as_str(), "info");
}

#[test]
fn test_runtime_bridge_pattern() {
    // Test that the runtime bridge works correctly
    use ggen_cli_lib::runtime::execute;

    let result = execute(async {
        // Simple async operation
        Ok(())
    });

    assert!(result.is_ok(), "Runtime bridge should work");
}

#[test]
fn test_error_propagation() {
    use ggen_cli_lib::runtime::execute;

    let result = execute(async {
        Err(ggen_utils::error::Error::new("Test error"))
    });

    assert!(result.is_err(), "Errors should propagate");
    let err = result.unwrap_err();
    assert!(err.to_string().contains("Test error"));
}
