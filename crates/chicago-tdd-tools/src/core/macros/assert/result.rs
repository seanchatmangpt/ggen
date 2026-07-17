//! Result Assertion Macros
//!
//! Assertions for testing `Result` types with enhanced error messages.

/// Assert that a result is successful with detailed error message
///
/// Provides better error messages than standard `assert!` when testing Results.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::assert_ok;
///
/// let result: Result<u32, String> = Ok(42);
/// assert_ok!(result);
///
/// // With custom message
/// let result2: Result<u32, String> = Ok(42);
/// assert_ok!(result2, "Expected successful operation");
/// ```
#[macro_export]
macro_rules! assert_ok {
    ($result:expr) => {
        match $result {
            Ok(_) => {}
            Err(e) => panic!("Expected Ok, but got Err: {:?}", e),
        }
    };
    ($result:expr, $msg:expr) => {
        match $result {
            Ok(_) => {}
            Err(e) => panic!("{}: Expected Ok, but got Err: {:?}", $msg, e),
        }
    };
}

/// Assert that a result is an error with detailed message
///
/// Provides better error messages when testing error cases.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::assert_err;
///
/// let result: Result<u32, String> = Err("error".to_string());
/// assert_err!(result);
///
/// // With custom message
/// let result2: Result<u32, String> = Err("error".to_string());
/// assert_err!(result2, "Expected error case");
/// ```
#[macro_export]
macro_rules! assert_err {
    ($result:expr) => {
        match $result {
            Ok(v) => panic!("Expected Err, but got Ok: {:?}", v),
            Err(_) => {}
        }
    };
    ($result:expr, $msg:expr) => {
        match $result {
            Ok(v) => panic!("{}: Expected Err, but got Ok: {:?}", $msg, v),
            Err(_) => {}
        }
    };
}

/// Assert that a function call fails, returning the error value
///
/// Convenience macro for testing error paths. Calls the function and asserts it returns `Err`,
/// then returns the error value for further assertions.
///
/// **Ergonomics**: With `test!` macro's new `Result` return type support, this provides
/// a concise way to test error cases without intermediate variables.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::{assert_fail, test};
///
/// # fn fallible_function() -> Result<u32, String> { Err("error".to_string()) }
/// test!(test_should_fail, {
///     // Arrange: Function that should fail
///
///     // Act & Assert: Verify function fails and extract error
///     let error = assert_fail!(fallible_function());
///     assert_eq!(error, "error");
/// });
/// ```
///
/// # Example with custom message
///
/// ```rust
/// use chicago_tdd_tools::{assert_fail, test};
///
/// # fn fallible_function() -> Result<u32, String> { Err("error".to_string()) }
/// test!(test_should_fail_with_msg, {
///     // Act & Assert: Verify function fails with custom message
///     let error = assert_fail!(fallible_function(), "Operation should fail");
///     assert_eq!(error, "error");
/// });
/// ```
#[macro_export]
macro_rules! assert_fail {
    ($call:expr) => {
        match $call {
            Ok(v) => panic!("Expected function to fail, but got Ok: {:?}", v),
            Err(e) => e,
        }
    };
    ($call:expr, $msg:expr) => {
        match $call {
            Ok(v) => panic!("{}: Expected function to fail, but got Ok: {:?}", $msg, v),
            Err(e) => e,
        }
    };
}

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use crate::test;

    test!(test_assert_ok_macro, {
        // Arrange: Create successful result
        let result: Result<u32, String> = Ok(42);

        // Act & Assert: Verify assert_ok! macro works
        assert_ok!(result);
        assert_ok!(result, "Should succeed");
    });

    #[test]
    #[should_panic(expected = "Expected Ok")]
    fn test_assert_ok_macro_fails() {
        // Arrange: Create error result
        let result: Result<u32, String> = Err("error".to_string());

        // Act & Assert: Should panic
        assert_ok!(result);
    }

    test!(test_assert_err_macro, {
        // Arrange: Create error result
        let result: Result<u32, String> = Err("error".to_string());

        // Act & Assert: Verify assert_err! macro works
        assert_err!(result);
        assert_err!(result, "Should fail");
    });

    #[test]
    #[should_panic(expected = "Expected Err")]
    fn test_assert_err_macro_fails() {
        // Arrange: Create successful result
        let result: Result<u32, String> = Ok(42);

        // Act & Assert: Should panic
        assert_err!(result);
    }

    test!(test_assert_fail_macro, {
        // Arrange: Function that returns error
        fn fallible_function() -> Result<u32, String> {
            Err("error".to_string())
        }

        // Act & Assert: Verify assert_fail! macro works and returns error
        let error = assert_fail!(fallible_function());
        assert_eq!(error, "error");

        // With custom message
        let error2 = assert_fail!(fallible_function(), "Operation should fail");
        assert_eq!(error2, "error");
    });

    #[test]
    #[should_panic(expected = "Expected function to fail")]
    fn test_assert_fail_macro_fails() {
        // Arrange: Function that succeeds
        fn successful_function() -> Result<u32, String> {
            Ok(42)
        }

        // Act & Assert: Should panic
        let _ = assert_fail!(successful_function());
    }
}
