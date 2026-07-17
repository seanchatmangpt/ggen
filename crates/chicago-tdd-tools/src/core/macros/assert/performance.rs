//! Performance and Constraint Assertion Macros
//!
//! Assertions for performance validation and constraint checking.

/// Assert that a value is within tick budget (≤8 ticks in release builds)
///
/// Validates performance constraints according to the Chatman Constant.
///
/// # Debug builds substitute a 1,000,000-tick budget
///
/// Under `cfg(debug_assertions)` the budget is NOT 8 — it is 1,000,000 ticks.
/// A debug-mode pass says nothing about the 8-tick budget; only release builds
/// check the real constant.
///
/// # This is not the Chatman Constant gate
///
/// The Chatman Constant is a count of deterministic logical steps (WASM-portable),
/// not elapsed timer ticks. Timer-derived tick counts are informational,
/// native-only, and platform-dependent (on `aarch64`, `cntvct_el0` ticks are
/// ~1 ns of wall time, not cycles). Gate the constant via explicit step counting
/// or structural verification, and treat this macro as a native smoke check only.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::assert_within_tick_budget;
///
/// let ticks = 5;
/// assert_within_tick_budget!(ticks);
///
/// // With custom message
/// let ticks2 = 5;
/// assert_within_tick_budget!(ticks2, "Hot path operation");
/// ```
#[macro_export]
macro_rules! assert_within_tick_budget {
    ($ticks:expr) => {
        let max_ticks = if cfg!(debug_assertions) { 1_000_000 } else { 8 };
        assert!(
            $ticks <= max_ticks,
            "Tick budget exceeded: {} > {} (Chatman Constant violation)",
            $ticks,
            max_ticks
        );
    };
    ($ticks:expr, $msg:expr) => {
        let max_ticks = if cfg!(debug_assertions) { 1_000_000 } else { 8 };
        assert!(
            $ticks <= max_ticks,
            "{}: Tick budget exceeded: {} > {} (Chatman Constant violation)",
            $msg,
            $ticks,
            max_ticks
        );
    };
}

/// Assert that a value is within a range with detailed error message
///
/// Provides better error messages for range assertions.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::assert_in_range;
///
/// let value = 5;
/// assert_in_range!(value, 0, 10);
///
/// // With custom message
/// let value2 = 5;
/// assert_in_range!(value2, 0, 10, "Value should be in valid range");
/// ```
#[macro_export]
macro_rules! assert_in_range {
    ($value:expr, $min:expr, $max:expr) => {
        assert!(
            ($min..=$max).contains(&$value),
            "Value {} not in range [{}, {}]",
            $value,
            $min,
            $max
        );
    };
    ($value:expr, $min:expr, $max:expr, $msg:expr) => {
        assert!(
            ($min..=$max).contains(&$value),
            "{}: Value {} not in range [{}, {}]",
            $msg,
            $value,
            $min,
            $max
        );
    };
}

/// Assert that a guard constraint is satisfied
///
/// Validates guard constraints like `max_run_len` ≤ 8.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::assert_guard_constraint;
///
/// let max_run_len = 5;
/// assert_guard_constraint!(max_run_len <= 8, "max_run_len");
/// ```
#[macro_export]
macro_rules! assert_guard_constraint {
    ($condition:expr, $constraint_name:expr) => {
        assert!($condition, "Guard constraint violation: {}", $constraint_name);
    };
}

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use crate::test;

    test!(test_assert_within_tick_budget_macro, {
        // Arrange: Various tick values
        let ticks_valid = 5;
        let ticks_max = 8;
        let ticks_zero = 0;

        // Act & Assert: Verify tick budget validation
        assert_within_tick_budget!(ticks_valid);
        assert_within_tick_budget!(ticks_max);
        assert_within_tick_budget!(ticks_zero);
        assert_within_tick_budget!(ticks_valid, "Test operation");
    });

    #[test]
    #[should_panic(expected = "Tick budget exceeded")]
    fn test_assert_within_tick_budget_macro_fails() {
        // Arrange: Tick value exceeding budget
        let ticks = 2_000_000;

        // Act & Assert: Should panic
        assert_within_tick_budget!(ticks);
    }

    test!(test_assert_in_range_macro, {
        // Arrange: Values within and at boundaries
        let value_mid = 5;
        let value_min = 0;
        let value_max = 10;

        // Act & Assert: Verify range validation
        assert_in_range!(value_mid, 0, 10);
        assert_in_range!(value_min, 0, 10);
        assert_in_range!(value_max, 0, 10);
        assert_in_range!(value_mid, 0, 10, "Value should be valid");
    });

    #[test]
    #[should_panic(expected = "not in range")]
    fn test_assert_in_range_macro_fails_below() {
        // Arrange: Value below range
        let value = -1;

        // Act & Assert: Should panic
        assert_in_range!(value, 0, 10);
    }

    #[test]
    #[should_panic(expected = "not in range")]
    fn test_assert_in_range_macro_fails_above() {
        // Arrange: Value above range
        let value = 11;

        // Act & Assert: Should panic
        assert_in_range!(value, 0, 10);
    }

    test!(test_assert_guard_constraint_macro, {
        // Arrange: Valid constraint values
        let max_run_len = 5;

        // Act & Assert: Verify guard constraint validation
        assert_guard_constraint!(max_run_len <= 8, "max_run_len");
        assert_guard_constraint!(true, "always_true");
    });

    #[test]
    #[should_panic(expected = "Guard constraint violation")]
    fn test_assert_guard_constraint_macro_fails() {
        // Arrange: Invalid constraint value
        let max_run_len = 9;

        // Act & Assert: Should panic
        assert_guard_constraint!(max_run_len <= 8, "max_run_len");
    }
}
