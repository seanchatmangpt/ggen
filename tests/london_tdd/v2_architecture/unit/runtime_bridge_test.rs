#![cfg(feature = "london_tdd")]
//! London TDD unit tests for runtime::execute bridge
//!
//! Tests the async→sync boundary wrapper that bridges sync CLI code to async domain logic.
//!
//! Coverage:
//! - Successful async execution
//! - Error propagation from async to sync
//! - Tokio runtime creation
//! - Performance (<10ms per execution)

use ggen_utils::error::Result;
use ggen_cli_lib;

/// Test successful async execution through runtime::execute
#[test]
fn test_runtime_execute_success() {
    let start = std::time::Instant::now();

    let result = ggen_cli_lib::runtime::execute(async {
        tokio::time::sleep(tokio::time::Duration::from_micros(100)).await;
        Ok(())
    });

    let elapsed = start.elapsed();

    assert!(result.is_ok(), "Expected successful execution");
    assert!(
        elapsed.as_millis() < 10,
        "Runtime bridge took {:?} (expected <10ms)",
        elapsed
    );
}

/// Test error propagation from async function
#[test]
fn test_runtime_execute_error_propagation() {
    let result = ggen_cli_lib::runtime::execute(async {
        Err(ggen_utils::error::Error::new("Test error"))
    });

    assert!(result.is_err(), "Expected error to propagate");
    assert!(
        result.unwrap_err().to_string().contains("Test error"),
        "Error message should be preserved"
    );
}

/// Test runtime bridge with async computation
#[test]
fn test_runtime_execute_async_computation() {
    let result = ggen_cli_lib::runtime::execute(async {
        let value = async_compute(42).await;
        assert_eq!(value, 84);
        Ok(())
    });

    assert!(result.is_ok());
}

/// Test multiple sequential async operations
#[test]
fn test_runtime_execute_sequential_operations() {
    let result = ggen_cli_lib::runtime::execute(async {
        let a = async_compute(10).await;
        let b = async_compute(20).await;
        let c = async_compute(30).await;
        assert_eq!(a + b + c, 120);
        Ok(())
    });

    assert!(result.is_ok());
}

/// Test runtime bridge performance with minimal async operation
#[test]
fn test_runtime_execute_minimal_performance() {
    let start = std::time::Instant::now();

    let result = ggen_cli_lib::runtime::execute(async { Ok(()) });

    let elapsed = start.elapsed();

    assert!(result.is_ok());
    assert!(
        elapsed.as_millis() < 5,
        "Minimal runtime bridge took {:?} (expected <5ms)",
        elapsed
    );
}

/// Test runtime bridge with Result::Err path
#[test]
fn test_runtime_execute_handles_domain_errors() {
    let result = ggen_cli_lib::runtime::execute(async {
        simulate_domain_error().await
    });

    assert!(result.is_err());
    let err = result.unwrap_err();
    assert!(err.to_string().contains("Domain error"));
}

/// Test runtime bridge with complex async operations
#[test]
fn test_runtime_execute_complex_async_flow() {
    let result = ggen_cli_lib::runtime::execute(async {
        // Simulate multiple async I/O operations
        tokio::time::sleep(tokio::time::Duration::from_micros(50)).await;
        let data = fetch_data().await?;
        tokio::time::sleep(tokio::time::Duration::from_micros(50)).await;
        process_data(data).await?;
        tokio::time::sleep(tokio::time::Duration::from_micros(50)).await;
        Ok(())
    });

    assert!(result.is_ok());
}

/// Test runtime bridge creates new runtime each time
#[test]
fn test_runtime_execute_creates_fresh_runtime() {
    // First execution
    let result1 = ggen_cli_lib::runtime::execute(async {
        Ok(())
    });
    assert!(result1.is_ok());

    // Second execution should work with fresh runtime
    let result2 = ggen_cli_lib::runtime::execute(async {
        Ok(())
    });
    assert!(result2.is_ok());
}

/// Test runtime bridge with nested async calls
#[test]
fn test_runtime_execute_nested_async() {
    let result = ggen_cli_lib::runtime::execute(async {
        let outer = outer_async_fn().await?;
        assert_eq!(outer, 42);
        Ok(())
    });

    assert!(result.is_ok());
}

/// Test performance: Full suite should be <100ms
#[test]
fn test_runtime_bridge_suite_performance() {
    let start = std::time::Instant::now();

    // Run 10 minimal executions
    for _ in 0..10 {
        let _ = ggen_cli_lib::runtime::execute(async { Ok(()) });
    }

    let elapsed = start.elapsed();
    assert!(
        elapsed.as_millis() < 100,
        "10 runtime executions took {:?} (expected <100ms)",
        elapsed
    );
}

// Helper functions for testing

async fn async_compute(value: i32) -> i32 {
    tokio::time::sleep(tokio::time::Duration::from_micros(10)).await;
    value * 2
}

async fn simulate_domain_error() -> Result<()> {
    Err(ggen_utils::error::Error::new("Domain error: Operation failed"))
}

async fn fetch_data() -> Result<String> {
    Ok("test data".to_string())
}

async fn process_data(_data: String) -> Result<()> {
    Ok(())
}

async fn outer_async_fn() -> Result<i32> {
    let inner = inner_async_fn().await?;
    Ok(inner * 2)
}

async fn inner_async_fn() -> Result<i32> {
    Ok(21)
}
