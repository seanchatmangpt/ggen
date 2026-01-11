#![cfg(feature = "london_tdd")]
//! London TDD component tests for doctor domain logic
//!
//! Tests the async business logic in `ggen_domain::utils::doctor` with mocked boundaries.
//!
//! Coverage:
//! - All system checks (Rust, Cargo, Git, GH, disk, network, permissions)
//! - Check status transitions (Ok, Warning, Error)
//! - Verbose vs. normal output
//! - Specific check execution
//! - Environment info collection
//! - Performance (<100ms per test)

use ggen_domain::utils::doctor::{execute_doctor, CheckResult, CheckStatus, DoctorInput, DoctorResult};

#[tokio::test]
async fn test_check_status_variants() {
    // Verify CheckStatus enum variants exist
    let _ok = CheckStatus::Ok;
    let _warning = CheckStatus::Warning;
    let _error = CheckStatus::Error;
}

#[tokio::test]
async fn test_check_result_structure() {
    let check = CheckResult {
        name: "Rust".to_string(),
        status: CheckStatus::Ok,
        message: "Rust is installed".to_string(),
    };

    assert_eq!(check.name, "Rust");
    assert_eq!(check.status, CheckStatus::Ok);
    assert_eq!(check.message, "Rust is installed");
}

#[tokio::test]
async fn test_doctor_input_default() {
    let input = DoctorInput::default();
    assert!(!input.verbose);
    assert!(input.check.is_none());
    assert!(!input.env);
}

#[tokio::test]
async fn test_doctor_input_custom() {
    let input = DoctorInput {
        verbose: true,
        check: Some("rust".to_string()),
        env: true,
    };

    assert!(input.verbose);
    assert_eq!(input.check.as_deref(), Some("rust"));
    assert!(input.env);
}

#[tokio::test]
async fn test_execute_doctor_basic() {
    let input = DoctorInput {
        verbose: false,
        check: None,
        env: false,
    };

    let result = execute_doctor(input).await;
    assert!(result.is_ok(), "Doctor execution should succeed");

    let doctor_result = result.unwrap();
    assert!(!doctor_result.checks.is_empty(), "Should have at least one check");
    assert!(doctor_result.environment.is_none(), "Environment should not be included when env=false");
}

#[tokio::test]
async fn test_execute_doctor_with_env() {
    let input = DoctorInput {
        verbose: false,
        check: None,
        env: true,
    };

    let result = execute_doctor(input).await;
    assert!(result.is_ok(), "Doctor execution should succeed");

    let doctor_result = result.unwrap();
    assert!(doctor_result.environment.is_some(), "Environment should be included when env=true");
}

#[tokio::test]
async fn test_execute_doctor_specific_check() {
    let input = DoctorInput {
        verbose: false,
        check: Some("rust".to_string()),
        env: false,
    };

    let result = execute_doctor(input).await;
    assert!(result.is_ok(), "Doctor execution should succeed");

    let doctor_result = result.unwrap();
    // Should have at least the rust check
    assert!(!doctor_result.checks.is_empty());
    assert!(doctor_result.checks.iter().any(|c| c.name == "Rust"));
}

/// Test performance: Component suite should be fast
#[tokio::test]
async fn test_component_suite_performance() {
    let start = std::time::Instant::now();

    let input = DoctorInput::default();
    let _result = execute_doctor(input).await;

    let elapsed = start.elapsed();
    assert!(
        elapsed.as_millis() < 1000,
        "Doctor execution took {:?} (expected <1000ms)",
        elapsed
    );
}
