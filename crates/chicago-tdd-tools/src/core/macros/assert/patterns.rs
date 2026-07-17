//! Pattern Matching Assertion Macros
//!
//! Assertions for pattern matching with enhanced error messages.

/// Assert that a value matches a pattern
///
/// Similar to `matches!` but provides assertion failure with context.
/// Useful for testing enums, structs, and complex patterns.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::assert_matches;
///
/// #[derive(Debug)]
/// enum Response {
///     Success(u32),
///     Error(String),
/// }
///
/// let response = Response::Success(42);
/// assert_matches!(response, Response::Success(_));
/// assert_matches!(response, Response::Success(42));
///
/// // With custom message
/// assert_matches!(response, Response::Success(x) if x > 0, "Should be positive success");
/// ```
#[macro_export]
macro_rules! assert_matches {
    ($value:expr, $pattern:pat) => {
        match $value {
            $pattern => {}
            ref v => panic!(
                "assertion failed: value does not match pattern\n  value: {:?}\n  expected pattern: {}",
                v,
                stringify!($pattern)
            ),
        }
    };
    ($value:expr, $pattern:pat, $msg:expr) => {
        match $value {
            $pattern => {}
            ref v => panic!(
                "{}: value does not match pattern\n  value: {:?}\n  expected pattern: {}",
                $msg,
                v,
                stringify!($pattern)
            ),
        }
    };
    ($value:expr, $pattern:pat if $guard:expr) => {
        match $value {
            $pattern if $guard => {}
            ref v => panic!(
                "assertion failed: value does not match pattern with guard\n  value: {:?}\n  expected pattern: {} if {}",
                v,
                stringify!($pattern),
                stringify!($guard)
            ),
        }
    };
    ($value:expr, $pattern:pat if $guard:expr, $msg:expr) => {
        match $value {
            $pattern if $guard => {}
            ref v => panic!(
                "{}: value does not match pattern with guard\n  value: {:?}\n  expected pattern: {} if {}",
                $msg,
                v,
                stringify!($pattern),
                stringify!($guard)
            ),
        }
    };
}

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use crate::test;

    #[derive(Debug)]
    #[allow(dead_code)] // Test enum - not all variants used in every test
    enum TestEnum {
        Variant1(u32),
        Variant2(String),
        Variant3 { x: i32, y: i32 },
    }

    test!(test_assert_matches_simple, {
        // Arrange
        let value = TestEnum::Variant1(42);

        // Act & Assert
        assert_matches!(value, TestEnum::Variant1(_));
        assert_matches!(value, TestEnum::Variant1(42));
    });

    test!(test_assert_matches_with_guard, {
        // Arrange
        let value = TestEnum::Variant1(42);

        // Act & Assert
        assert_matches!(value, TestEnum::Variant1(x) if x > 0);
        assert_matches!(value, TestEnum::Variant1(x) if x == 42);
    });

    test!(test_assert_matches_struct_pattern, {
        // Arrange
        let value = TestEnum::Variant3 { x: 10, y: 20 };

        // Act & Assert
        assert_matches!(value, TestEnum::Variant3 { .. });
        assert_matches!(value, TestEnum::Variant3 { x: 10, .. });
    });

    test!(test_assert_matches_with_message, {
        // Arrange
        let value = TestEnum::Variant2("test".to_string());

        // Act & Assert
        assert_matches!(value, TestEnum::Variant2(_), "Should be Variant2");
    });

    #[test]
    #[should_panic(expected = "value does not match pattern")]
    fn test_assert_matches_fails() {
        // Arrange
        let value = TestEnum::Variant1(42);

        // Act & Assert: Should panic
        assert_matches!(value, TestEnum::Variant2(_));
    }

    #[test]
    #[should_panic(expected = "value does not match pattern with guard")]
    fn test_assert_matches_guard_fails() {
        // Arrange
        let value = TestEnum::Variant1(42);

        // Act & Assert: Should panic (guard fails)
        assert_matches!(value, TestEnum::Variant1(x) if x > 100);
    }
}
