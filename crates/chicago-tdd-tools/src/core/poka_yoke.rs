//! Poka-Yoke (Error Prevention) Design Patterns
//!
//! Provides type-level error prevention to make invalid states unrepresentable.
//! Uses Rust's type system to prevent errors at compile time.
//!
//! # Poka-Yoke Principles
//!
//! - **Make invalid states unrepresentable**: Use types to prevent errors
//! - **Compile-time prevention**: Catch errors before runtime
//! - **Type-level invariants**: Encode constraints in types
//!
//! # Error Modes Prevented
//!
//! 1. **Behavior Verification**: Prevents tests that only check `assert_ok!()` without verifying outputs
//! 2. **Error Handling**: Prevents `unwrap()` in tests, forces proper error handling
//! 3. **Test Structure**: Enforces AAA pattern through type state
//!
//! # Example
//!
//! ```rust
//! use chicago_tdd_tools::core::poka_yoke::*;
//!
//! // Behavior verification enforcement
//! let result: Result<i32, String> = Ok(42);
//! let verification = BehaviorVerification::<AssertOkCalled, i32>::new(result.unwrap());
//! // Must verify behavior to proceed
//! let verified = verification.verify_behavior(|v| *v == 42);
//! assert_eq!(verified.value(), 42);
//!
//! // Error handling enforcement
//! let result: Result<i32, String> = Ok(42);
//! let test_result = TestResult::new(result);
//! // No unwrap() method - must use assert_ok() or assert_err()
//! let value = test_result.assert_ok("Operation should succeed");
//! assert_eq!(value, 42);
//! ```

use std::marker::PhantomData;

// ============================================================================
// Behavior Verification Enforcement
// ============================================================================

/// Type-level state marker: `AssertOk` has been called
///
/// **Poka-yoke**: This marker type prevents tests that only call `assert_ok!()`
/// without verifying behavior. Tests must verify outputs to proceed.
pub struct AssertOkCalled;

/// Type-level state marker: Behavior has been verified
///
/// **Poka-yoke**: This marker type indicates that behavior verification
/// has been completed after calling `assert_ok!()`.
pub struct BehaviorVerified;

/// Behavior verification tracker
///
/// **Poka-yoke**: This type enforces that tests verify observable outputs
/// after calling `assert_ok!()`. The type system prevents meaningless tests
/// that only check function existence.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::core::poka_yoke::{BehaviorVerification, AssertOkCalled};
///
/// let value: i32 = 42;
/// let verification = BehaviorVerification::<AssertOkCalled, i32>::new(value);
/// // Must verify behavior to get value
/// let verified = verification.verify_behavior(|v| *v == 42);
/// assert_eq!(verified.value(), 42);
/// ```
pub struct BehaviorVerification<State, T> {
    /// The value being verified
    value: T,
    /// Type-level state marker
    _state: PhantomData<State>,
}

impl<T> BehaviorVerification<AssertOkCalled, T> {
    /// Create a new behavior verification tracker
    ///
    /// **Poka-yoke**: This is called after `assert_ok!()` to track that
    /// behavior verification is required.
    pub const fn new(value: T) -> Self {
        Self { value, _state: PhantomData }
    }

    /// Verify behavior by checking observable outputs
    ///
    /// **Poka-yoke**: This method must be called to transition from
    /// `AssertOkCalled` to `BehaviorVerified`. The type system prevents
    /// accessing the value without verification.
    ///
    /// # Arguments
    ///
    /// * `predicate` - Function that verifies the value meets expectations
    ///
    /// # Panics
    ///
    /// Panics if the predicate returns `false`.
    pub fn verify_behavior<F>(self, predicate: F) -> BehaviorVerification<BehaviorVerified, T>
    where
        F: FnOnce(&T) -> bool,
        T: std::fmt::Debug,
    {
        assert!(predicate(&self.value), "Behavior verification failed for value: {:?}", self.value);
        BehaviorVerification { value: self.value, _state: PhantomData }
    }
}

impl<T> BehaviorVerification<BehaviorVerified, T> {
    /// Get the verified value
    ///
    /// **Poka-yoke**: This method is only available after behavior verification.
    /// The type system ensures verification happens before accessing the value.
    pub fn value(self) -> T {
        self.value
    }

    /// Get a reference to the verified value
    ///
    /// **Poka-yoke**: This method is only available after behavior verification.
    pub const fn value_ref(&self) -> &T {
        &self.value
    }
}

// ============================================================================
// Error Handling Enforcement
// ============================================================================

/// Test result wrapper that prevents `unwrap()` in tests
///
/// **Poka-yoke**: This type prevents the use of `unwrap()` in tests by
/// not providing an `unwrap()` method. Tests must use `assert_ok!()` or
/// `assert_err!()` macros instead, which provide better error messages.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::core::poka_yoke::TestResult;
///
/// let result: Result<i32, String> = Ok(42);
/// let test_result = TestResult::new(result);
/// // No unwrap() method - must use assert_ok!() or assert_err!()
/// let value = test_result.assert_ok("Operation should succeed");
/// assert_eq!(value, 42);
/// ```
pub struct TestResult<T, E> {
    /// The wrapped result
    result: Result<T, E>,
}

impl<T, E> TestResult<T, E> {
    /// Create a new test result wrapper
    ///
    /// **Poka-yoke**: Wraps a `Result` to prevent `unwrap()` usage.
    pub const fn new(result: Result<T, E>) -> Self {
        Self { result }
    }

    /// Assert that the result is Ok, returning the value
    ///
    /// **Poka-yoke**: This method replaces `unwrap()` with better error messages.
    /// The method name `assert_ok` makes it clear this is a test assertion.
    ///
    /// **Kaizen improvement**: Error message format matches `assert_ok!` macro pattern
    /// for consistency: `"{message}: Expected Ok, but got Err: {:?}"`
    ///
    /// # Arguments
    ///
    /// * `message` - Optional message to display if assertion fails
    ///
    /// # Panics
    ///
    /// Panics if the result is an error, with a detailed error message.
    ///
    /// **Root Cause Prevention**: `#[allow(clippy::panic)]` must be at function level,
    /// not before macro invocation. Rust attributes apply to items (functions, structs),
    /// not macro invocations. Pattern: Place `#[allow(...)]` before function definition.
    #[allow(clippy::panic)] // Test helper - panic is appropriate for test failures
    pub fn assert_ok(self, message: &str) -> T
    where
        E: std::fmt::Debug,
    {
        match self.result {
            Ok(value) => value,
            Err(e) => {
                panic!("{message}: Expected Ok, but got Err: {e:?}")
            }
        }
    }

    /// Assert that the result is Err
    ///
    /// **Poka-yoke**: This method provides better error messages than
    /// checking `is_err()` and then unwrapping.
    ///
    /// **Kaizen improvement**: Error message format matches `assert_err!` macro pattern
    /// for consistency: `"{message}: Expected Err, but got Ok: {:?}"`
    ///
    /// # Arguments
    ///
    /// * `message` - Optional message to display if assertion fails
    ///
    /// # Panics
    ///
    /// Panics if the result is Ok, with a detailed error message.
    ///
    /// **Root Cause Prevention**: `#[allow(clippy::panic)]` must be at function level,
    /// not before macro invocation. Rust attributes apply to items (functions, structs),
    /// not macro invocations. Pattern: Place `#[allow(...)]` before function definition.
    #[allow(clippy::panic)] // Test helper - panic is appropriate for test failures
    pub fn assert_err(self, message: &str) -> E
    where
        T: std::fmt::Debug,
    {
        match self.result {
            Err(e) => e,
            Ok(value) => {
                panic!("{message}: Expected Err, but got Ok: {value:?}")
            }
        }
    }

    /// Check if the result is Ok without consuming it
    ///
    /// **Poka-yoke**: This method allows checking without consuming, but
    /// still encourages using `assert_ok!()` for better error messages.
    pub const fn is_ok(&self) -> bool {
        self.result.is_ok()
    }

    /// Check if the result is Err without consuming it
    ///
    /// **Poka-yoke**: This method allows checking without consuming, but
    /// still encourages using `assert_err!()` for better error messages.
    pub const fn is_err(&self) -> bool {
        self.result.is_err()
    }
}

impl<T, E> From<Result<T, E>> for TestResult<T, E> {
    fn from(result: Result<T, E>) -> Self {
        Self::new(result)
    }
}

// Note: AAA pattern enforcement is already provided by the `state` module.
// See `chicago_tdd_tools::state::TestState` for type-level AAA pattern enforcement.

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_behavior_verification() {
        // Arrange
        let result: Result<i32, String> = Ok(42);
        // **Kaizen improvement**: Use TestResult instead of unwrap() to demonstrate poka-yoke pattern
        let test_result = TestResult::new(result);
        let value = test_result.assert_ok("Should extract value for verification");

        // Act
        let verification = BehaviorVerification::<AssertOkCalled, i32>::new(value);
        let verified = verification.verify_behavior(|v| *v == 42);

        // Assert
        assert_eq!(verified.value(), 42);
    }

    #[test]
    fn test_test_result_assert_ok() {
        // Arrange
        let result: Result<i32, String> = Ok(42);
        let test_result = TestResult::new(result);

        // Act
        let value = test_result.assert_ok("Should be Ok");

        // Assert
        assert_eq!(value, 42);
    }

    #[test]
    #[should_panic(expected = "Expected Ok, but got Err")]
    fn test_test_result_assert_ok_fails_on_error() {
        // Arrange
        let result: Result<i32, String> = Err("error".to_string());
        let test_result = TestResult::new(result);

        // Act & Assert: Should panic
        let _ = test_result.assert_ok("Should fail");
    }

    #[test]
    fn test_test_result_assert_err() {
        // Arrange
        let result: Result<i32, String> = Err("error".to_string());
        let test_result = TestResult::new(result);

        // Act
        let error = test_result.assert_err("Should be Err");

        // Assert
        assert_eq!(error, "error");
    }
}
