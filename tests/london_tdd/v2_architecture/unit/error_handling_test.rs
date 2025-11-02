#![cfg(feature = "london-tdd")]
//! London TDD unit tests for error handling utilities
//!
//! Tests error propagation, transformation, and formatting across the async/sync boundary.

use ggen_utils::error::{Error, Result};

#[test]
fn test_error_creation() {
    let err = Error::new("Test error");
    assert_eq!(err.to_string(), "Test error");
}

#[test]
fn test_error_from_string() {
    let err: Error = "String error".into();
    assert!(err.to_string().contains("String error"));
}

#[test]
fn test_error_chain_preservation() {
    let original = std::io::Error::new(std::io::ErrorKind::NotFound, "File not found");
    let wrapped = Error::new(&format!("Failed to read config: {}", original));

    assert!(wrapped.to_string().contains("Failed to read config"));
    assert!(wrapped.to_string().contains("File not found"));
}

#[test]
fn test_result_ok_path() {
    let result: Result<i32> = Ok(42);
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 42);
}

#[test]
fn test_result_err_path() {
    let result: Result<i32> = Err(Error::new("Computation failed"));
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Computation failed"));
}

#[test]
fn test_error_propagation_through_question_mark() {
    fn inner() -> Result<i32> {
        Err(Error::new("Inner error"))
    }

    fn outer() -> Result<i32> {
        let _value = inner()?;
        Ok(0)
    }

    let result = outer();
    assert!(result.is_err());
    assert!(result.unwrap_err().to_string().contains("Inner error"));
}

#[test]
fn test_anyhow_compatibility() {
    fn returns_anyhow() -> anyhow::Result<i32> {
        Ok(42)
    }

    fn converts_to_ggen_error() -> Result<i32> {
        let value = returns_anyhow()
            .map_err(|e| Error::new(&e.to_string()))?;
        Ok(value)
    }

    let result = converts_to_ggen_error();
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), 42);
}

#[test]
fn test_error_context_preservation() {
    let err = Error::new_fmt(format_args!("Failed at step {}: {}", 3, "timeout"));
    assert!(err.to_string().contains("Failed at step 3"));
    assert!(err.to_string().contains("timeout"));
}
