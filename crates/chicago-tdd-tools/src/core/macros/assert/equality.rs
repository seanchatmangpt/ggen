//! Equality Assertion Macros
//!
//! Assertions for testing equality with enhanced error messages and approximate comparisons.

/// Assert equality with detailed error message and diff output
///
/// Provides better error messages for equality assertions with automatic diff generation.
///
/// # Example
///
/// ```rust,should_panic
/// use chicago_tdd_tools::assert_eq_msg;
///
/// let actual = 42;
/// let expected = 43;
/// assert_eq_msg!(actual, expected, "Values should match");
/// // Panics with: "Values should match: expected 43, got 42"
/// ```
#[macro_export]
macro_rules! assert_eq_msg {
    ($actual:expr, $expected:expr, $msg:expr) => {{
        let actual_val = &$actual;
        let expected_val = &$expected;
        if actual_val != expected_val {
            panic!("{}: expected {:?}, got {:?}", $msg, expected_val, actual_val);
        }
    }};
}

/// Assert equality with automatic type inference and diff output
///
/// Enhanced version that provides better error messages with context.
#[macro_export]
macro_rules! assert_eq_enhanced {
    ($actual:expr, $expected:expr $(,)?) => {
        {
            let actual_val = &$actual;
            let expected_val = &$expected;
            if actual_val != expected_val {
                panic!(
                    "assertion failed: `(left == right)`\n  left: `{:?}`\n right: `{:?}`",
                    actual_val, expected_val
                );
            }
        }
    };
    ($actual:expr, $expected:expr, $($arg:tt)+) => {
        {
            let actual_val = &$actual;
            let expected_val = &$expected;
            if actual_val != expected_val {
                panic!(
                    "assertion failed: `(left == right)`\n  left: `{:?}`\n right: `{:?}`\n{}",
                    actual_val, expected_val, format!($($arg)+)
                );
            }
        }
    };
}

/// Assert that two floating-point values are approximately equal
///
/// **New in v1.3.0**: Floating-point comparison with configurable tolerance.
///
/// Compares floating-point values within a specified epsilon (tolerance).
/// Works with `f32` and `f64` types.
/// Provides clear failure messages showing the actual difference.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::assert_approx_eq;
///
/// let pi = 3.14159265_f64;
/// let approx_pi = 3.14_f64;
/// assert_approx_eq!(pi, approx_pi, 0.01);
///
/// // With custom message
/// let calculated = 2.0_f64 / 3.0_f64;
/// let expected = 0.6667_f64;
/// assert_approx_eq!(calculated, expected, 0.0001, "Division result should be close");
/// ```
#[macro_export]
macro_rules! assert_approx_eq {
    ($actual:expr, $expected:expr, $epsilon:expr) => {{
        #[allow(clippy::float_cmp)] // Intentional approximate comparison
        {
            let actual_val = $actual as f64;
            let expected_val = $expected as f64;
            let epsilon_val = $epsilon as f64;
            let diff = (actual_val - expected_val).abs();
            if diff > epsilon_val {
                panic!(
                    "Values not approximately equal.\n  actual: {}\n  expected: {}\n  epsilon: {}\n  difference: {}",
                    actual_val, expected_val, epsilon_val, diff
                );
            }
        }
    }};
    ($actual:expr, $expected:expr, $epsilon:expr, $msg:expr) => {{
        #[allow(clippy::float_cmp)] // Intentional approximate comparison
        {
            let actual_val = $actual as f64;
            let expected_val = $expected as f64;
            let epsilon_val = $epsilon as f64;
            let diff = (actual_val - expected_val).abs();
            if diff > epsilon_val {
                panic!(
                    "{}: Values not approximately equal.\n  actual: {}\n  expected: {}\n  epsilon: {}\n  difference: {}",
                    $msg, actual_val, expected_val, epsilon_val, diff
                );
            }
        }
    }};
}

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use crate::test;

    test!(test_assert_eq_msg_macro, {
        // Arrange: Equal values
        let actual = 42;
        let expected = 42;

        // Act & Assert: Verify equality with message
        assert_eq_msg!(actual, expected, "Values should match");
    });

    #[test]
    #[should_panic(expected = "Values should match")]
    fn test_assert_eq_msg_macro_fails() {
        // Arrange: Unequal values
        let actual = 41;
        let expected = 42;

        // Act & Assert: Should panic
        assert_eq_msg!(actual, expected, "Values should match");
    }

    test!(test_assert_approx_eq_macro, {
        // Arrange: Floating-point values
        let pi = 3.14159265;
        let approx_pi = 3.14;
        let calculated = 2.0 / 3.0;
        let expected = 0.6667;

        // Act & Assert: Verify assert_approx_eq! macro works
        assert_approx_eq!(pi, approx_pi, 0.01);
        assert_approx_eq!(calculated, expected, 0.0001);
        assert_approx_eq!(calculated, expected, 0.0001, "Division result should be close");
    });

    #[test]
    #[should_panic(expected = "Values not approximately equal")]
    fn test_assert_approx_eq_macro_fails() {
        // Arrange: Values not within epsilon
        let actual = 3.14159;
        let expected = 2.71828;

        // Act & Assert: Should panic
        assert_approx_eq!(actual, expected, 0.01);
    }
}
