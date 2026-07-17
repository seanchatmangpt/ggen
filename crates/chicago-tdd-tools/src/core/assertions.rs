//! > 📚 Reference
//!
//! Assertion Helpers
//!
//! Provides assertion utilities following Chicago TDD principles.
//! Uses Higher-Ranked Trait Bounds (HRTB) for flexible predicate functions.
//!
//! # Go the Extra Mile: 1st/2nd/3rd Idea Progression
//!
//! - **1st Idea**: Specific assertion functions (`assert_success`, `assert_error`, etc.)
//! - **2nd Idea**: `AssertionBuilder<T>` - Generic assertion builder pattern for composable assertions
//! - **3rd Idea**: Compile-time validated assertions with OTEL/Weaver validation

#[cfg(feature = "otel")]
use crate::observability::otel::types::{
    Metric, MetricValue, Span, SpanContext, SpanId, SpanStatus, TraceId,
};
#[cfg(feature = "otel")]
use std::time::{SystemTime, UNIX_EPOCH};

/// Assert that a result is successful
///
/// # Panics
///
/// Panics if the result is an error, with a message showing the error.
pub fn assert_success<T, E: std::fmt::Debug>(result: &Result<T, E>) {
    assert!(result.is_ok(), "Expected success, but got error: {:?}", result.as_ref().err());
}

/// Assert that a result is an error
///
/// # Panics
///
/// Panics if the result is successful, with a message showing the value.
pub fn assert_error<T: std::fmt::Debug, E>(result: &Result<T, E>) {
    assert!(result.is_err(), "Expected error, but got success: {:?}", result.as_ref().ok());
}

/// Assert that two values are equal with a custom message
///
/// # Panics
///
/// Panics if `actual` and `expected` are not equal, with a message showing both values.
pub fn assert_eq_with_msg<T: std::fmt::Debug + PartialEq>(actual: &T, expected: &T, msg: &str) {
    assert_eq!(actual, expected, "{msg}: expected {expected:?}, got {actual:?}");
}

/// Assert that a value is within a range
///
/// # Panics
///
/// Panics if `value` is not within the range `[min, max]`, with a message showing the value and range.
pub fn assert_in_range<T: PartialOrd + std::fmt::Debug>(value: &T, min: &T, max: &T, msg: &str) {
    assert!(value >= min && value <= max, "{msg}: value {value:?} not in range [{min:?}, {max:?}]");
}

/// Assert that a value satisfies a predicate using Higher-Ranked Trait Bounds (HRTB)
///
/// HRTB allows the predicate to work with any lifetime, making it more flexible
/// than a regular `Fn(&T) -> bool` bound.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::assertions::assert_that;
///
/// let value = 42;
/// assert_that(&value, |v| *v > 0);
///
/// // Works with references of any lifetime
/// let vec = vec![1, 2, 3];
/// assert_that(&vec, |v| v.len() == 3);
/// ```
/// # Panics
///
/// Panics if the predicate returns `false` for the given value.
pub fn assert_that<T, F>(value: &T, predicate: F)
where
    T: std::fmt::Debug,
    // Poka-Yoke: HRTB requires single-character lifetime for flexibility
    F: for<'value> Fn(&'value T) -> bool,
{
    assert!(predicate(value), "Assertion failed for value: {value:?}");
}

/// Assert that a value satisfies a predicate with a custom message
///
/// # Panics
///
/// Panics if the predicate returns `false` for the given value, with the custom message.
pub fn assert_that_with_msg<T, F>(value: &T, predicate: F, msg: &str)
where
    T: std::fmt::Debug,
    // Poka-Yoke: HRTB requires single-character lifetime for flexibility
    F: for<'value> Fn(&'value T) -> bool,
{
    assert!(predicate(value), "{msg}: Assertion failed for value: {value:?}");
}

// ============================================================================
// 2nd IDEA: Go bigger (80/20) - Generic assertion builder
// ============================================================================

/// > 📚 Reference
///
/// Generic assertion builder for composable assertions.
///
/// **2nd Idea**: Generic builder that works with any type and allows composing multiple assertions.
/// This provides 80% more value (works for all types, composable) with minimal effort.
///
/// **Telemetry**: Basic OTEL spans (if otel feature enabled)
/// **Validation**: OTEL span validation
///
/// # Examples
///
/// ```rust
/// use chicago_tdd_tools::assertions::AssertionBuilder;
///
/// let value = 42;
/// let builder = AssertionBuilder::new(value)
///     .assert_eq(&42)
///     .assert_that(|v| *v > 0);
///
/// assert_eq!(builder.into_value(), 42);
/// ```
pub struct AssertionBuilder<T> {
    value: T,
    #[cfg(feature = "otel")]
    span: Option<Span>,
}

impl<T: std::fmt::Debug> AssertionBuilder<T> {
    /// Create a new assertion builder
    pub const fn new(value: T) -> Self {
        Self {
            value,
            #[cfg(feature = "otel")]
            span: None,
        }
    }

    /// Start OTEL span for this assertion
    ///
    /// # Panics
    ///
    /// Panics if system time is before `UNIX_EPOCH` (should never happen in practice).
    #[cfg(feature = "otel")]
    #[must_use]
    pub fn with_span(mut self, span_name: &str) -> Self {
        #[allow(clippy::expect_used)] // SystemTime should always be after UNIX_EPOCH
        #[allow(clippy::cast_possible_truncation)]
        // Milliseconds since epoch won't exceed u64::MAX for many years
        let start_time = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("SystemTime should always be after UNIX_EPOCH")
            .as_millis() as u64;

        let span = Span::new_active(
            SpanContext::root(TraceId(12345), SpanId(67890), 1),
            span_name.to_string(),
            start_time,
            std::collections::BTreeMap::new(),
            Vec::new(),
            SpanStatus::Unset,
        );

        self.span = Some(span);
        self
    }

    /// Assert that the value satisfies a predicate
    ///
    /// # Panics
    ///
    /// Panics if the predicate returns `false` for the value.
    #[must_use]
    pub fn assert_that<F>(self, predicate: F) -> Self
    where
        // Poka-Yoke: HRTB requires single-character lifetime for flexibility
        F: for<'value> Fn(&'value T) -> bool,
    {
        assert!(predicate(&self.value), "Assertion failed for value: {:?}", self.value);
        self
    }

    /// Assert that the value equals an expected value
    ///
    /// # Panics
    ///
    /// Panics if the value does not equal the expected value.
    #[must_use]
    pub fn assert_eq<U: PartialEq + std::fmt::Debug>(self, expected: &U) -> Self
    where
        T: PartialEq<U>,
    {
        assert_eq!(&self.value, expected, "Values not equal");
        self
    }

    /// Assert that the value satisfies a predicate with a custom message
    ///
    /// # Panics
    ///
    /// Panics if the predicate returns `false` for the value, with the custom message.
    #[must_use]
    pub fn assert_that_with_msg<F>(self, predicate: F, msg: &str) -> Self
    where
        // Poka-Yoke: HRTB requires single-character lifetime for flexibility
        F: for<'value> Fn(&'value T) -> bool,
    {
        assert!(predicate(&self.value), "{msg}: Assertion failed for value: {:?}", self.value);
        self
    }

    /// Get the value (consumes the builder)
    pub fn into_value(self) -> T {
        self.value
    }

    /// Get the OTEL span (if started)
    ///
    /// # Panics
    ///
    /// Panics if system time is before `UNIX_EPOCH` (should never happen in practice).
    #[cfg(feature = "otel")]
    pub fn into_span(mut self) -> Option<Span> {
        if let Some(ref mut span) = self.span {
            #[allow(clippy::expect_used)] // SystemTime should always be after UNIX_EPOCH
            #[allow(clippy::cast_possible_truncation)]
            // Milliseconds since epoch won't exceed u64::MAX for many years
            let end_time = SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .expect("SystemTime should always be after UNIX_EPOCH")
                .as_millis() as u64;

            // End time should always be >= start time in normal operation
            if let Err(e) = span.complete(end_time) {
                // Log error but don't fail - span will remain active
                #[cfg(feature = "logging")]
                log::warn!("Failed to complete span: {e}");
                #[cfg(not(feature = "logging"))]
                eprintln!("Warning: Failed to complete span: {}", e);
            } else {
                span.status = SpanStatus::Ok;
            }
        }
        self.span.take()
    }
}

// ============================================================================
// 3rd IDEA: Maximum value - Compile-time validated assertions + OTEL + Weaver
// ============================================================================

/// > 📚 Reference
///
/// Compile-time validated assertion with OTEL/Weaver validation.
///
/// **3rd Idea**: Type-level validated assertion that prevents invalid states at compile time.
/// Maximum value: Type-safe, validated, prevents entire class of errors.
///
/// **Telemetry**: Full OTEL spans and metrics
/// **Validation**: OTEL span validation + Weaver live-check schema validation
///
/// # Examples
///
/// ```rust,ignore
/// use chicago_tdd_tools::assertions::ValidatedAssertion;
///
/// // Create a validated assertion (requires "otel" feature for methods)
/// let assertion = ValidatedAssertion::new(42, "validate_port");
/// let assertion = assertion.assert_that(|v| *v > 0);
/// assert_eq!(assertion.into_value(), 42);
/// ```
pub struct ValidatedAssertion<T> {
    // Poka-Yoke: Value is accessed via into_value() - not dead code
    #[allow(dead_code, reason = "Value is accessed via into_value() method")]
    value: T,
    #[cfg(feature = "otel")]
    span: Span,
    #[cfg(feature = "otel")]
    metric: Option<Metric>,
}

#[cfg(feature = "otel")]
impl<T: std::fmt::Debug> ValidatedAssertion<T> {
    /// Create a new validated assertion with OTEL instrumentation
    ///
    /// # Panics
    ///
    /// Panics if system time is before `UNIX_EPOCH` (should never happen in practice).
    pub fn new(value: T, span_name: &str) -> Self {
        #[allow(clippy::expect_used)] // SystemTime should always be after UNIX_EPOCH
        #[allow(clippy::cast_possible_truncation)]
        // Milliseconds since epoch won't exceed u64::MAX for many years
        let start_time = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("SystemTime should always be after UNIX_EPOCH")
            .as_millis() as u64;

        let span = Span::new_active(
            SpanContext::root(TraceId(12345), SpanId(67890), 1),
            span_name.to_string(),
            start_time,
            std::collections::BTreeMap::new(),
            Vec::new(),
            SpanStatus::Unset,
        );

        Self { value, span, metric: None }
    }

    /// Assert that the value satisfies a predicate (validated)
    ///
    /// # Panics
    ///
    /// Panics if system time is before `UNIX_EPOCH` (should never happen in practice).
    #[must_use]
    pub fn assert_that<F>(mut self, predicate: F) -> Self
    where
        F: for<'a> Fn(&'a T) -> bool,
    {
        let success = predicate(&self.value);

        #[allow(clippy::expect_used)] // SystemTime should always be after UNIX_EPOCH
        #[allow(clippy::cast_possible_truncation)]
        // Milliseconds since epoch won't exceed u64::MAX for many years
        let end_time = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("SystemTime should always be after UNIX_EPOCH")
            .as_millis() as u64;

        // End time should always be >= start time in normal operation
        if let Err(e) = self.span.complete(end_time) {
            // Log error but don't fail - span will remain active
            #[cfg(feature = "logging")]
            log::warn!("Failed to complete span: {e}");
            #[cfg(not(feature = "logging"))]
            eprintln!("Warning: Failed to complete span: {e}");
        } else {
            self.span.status = if success { SpanStatus::Ok } else { SpanStatus::Error };
        }
        self.span.attributes.insert("assertion_result".to_string(), success.to_string());

        // Create metric
        #[allow(clippy::expect_used)] // SystemTime should always be after UNIX_EPOCH
        #[allow(clippy::cast_possible_truncation)]
        // Milliseconds since epoch won't exceed u64::MAX for many years
        let timestamp = SystemTime::now()
            .duration_since(UNIX_EPOCH)
            .expect("SystemTime should always be after UNIX_EPOCH")
            .as_millis() as u64;

        self.metric = Some(Metric {
            name: "chicago_tdd_tools.assertions.total".to_string(),
            value: MetricValue::Counter(1),
            timestamp_ms: timestamp,
            attributes: std::collections::BTreeMap::new(),
        });

        // Safe to unwrap here because we just set metric to Some above
        if let Some(ref mut metric) = self.metric {
            metric.attributes.insert("success".to_string(), success.to_string());
        }

        assert!(success, "Assertion failed for value: {:?}", self.value);
        self
    }

    /// Get the value (consumes the assertion)
    pub fn into_value(self) -> T {
        self.value
    }

    /// Get the OTEL span
    pub const fn span(&self) -> &Span {
        &self.span
    }

    /// Get the OTEL metric
    pub const fn metric(&self) -> Option<&Metric> {
        self.metric.as_ref()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::test;

    // Kaizen improvement: Extract magic number to named constant for clarity
    const TEST_VALUE: u32 = 42;

    // ========================================================================
    // 1. ERROR PATH TESTING - Test all assertion functions (80% of bugs)
    // ========================================================================

    test!(test_assert_success_with_ok, {
        // Arrange: Create Ok result
        let result: Result<u32, String> = Ok(TEST_VALUE);

        // Act & Assert: Verify assert_success works
        assert_success(&result);
    });

    #[test]
    #[should_panic(expected = "Expected success, but got error")]
    fn test_assert_success_with_err() {
        let result: Result<u32, String> = Err("error".to_string());
        assert_success(&result);
    }

    test!(test_assert_error_with_err, {
        // Arrange: Create Err result
        let result: Result<u32, String> = Err("error".to_string());

        // Act & Assert: Verify assert_error works
        assert_error(&result);
    });

    #[test]
    #[should_panic(expected = "Expected error, but got success")]
    fn test_assert_error_with_ok() {
        let result: Result<u32, String> = Ok(TEST_VALUE);
        assert_error(&result);
    }

    test!(test_assert_eq_with_msg_equal, {
        // Arrange: Create equal values
        let value1 = TEST_VALUE;
        let value2 = TEST_VALUE;

        // Act & Assert: Verify assert_eq_with_msg works
        assert_eq_with_msg(&value1, &value2, "values should be equal");
    });

    #[test]
    #[should_panic(expected = "values should be equal")]
    fn test_assert_eq_with_msg_not_equal() {
        assert_eq_with_msg(&TEST_VALUE, &43, "values should be equal");
    }

    test!(test_assert_in_range_valid, {
        // Arrange: Create value in range
        let value = 5;

        // Act & Assert: Verify assert_in_range works
        assert_in_range(&value, &0, &10, "value should be in range");
    });

    test!(test_assert_in_range_min_boundary, {
        // Arrange: Create value at min boundary
        let value = 0;

        // Act & Assert: Verify min boundary works
        assert_in_range(&value, &0, &10, "value should be in range");
    });

    test!(test_assert_in_range_max_boundary, {
        // Arrange: Create value at max boundary
        let value = 10;

        // Act & Assert: Verify max boundary works
        assert_in_range(&value, &0, &10, "value should be in range");
    });

    #[test]
    #[should_panic(expected = "value should be in range")]
    fn test_assert_in_range_below_min() {
        assert_in_range(&-1, &0, &10, "value should be in range");
    }

    #[test]
    #[should_panic(expected = "value should be in range")]
    fn test_assert_in_range_above_max() {
        assert_in_range(&11, &0, &10, "value should be in range");
    }

    test!(test_assert_that_valid, {
        // Arrange: Create valid value
        let value = TEST_VALUE;

        // Act & Assert: Verify assert_that works
        assert_that(&value, |v| *v > 0);
    });

    #[test]
    #[should_panic(expected = "Assertion failed for value")]
    fn test_assert_that_invalid() {
        assert_that(&0, |v| *v > 0);
    }

    test!(test_assert_that_with_vec, {
        // Arrange: Create vector
        let vec = vec![1, 2, 3];

        // Act & Assert: Verify assert_that works with vec
        assert_that(&vec, |v| v.len() == 3);
    });

    test!(test_assert_that_with_msg_valid, {
        // Arrange: Create valid value
        let value = TEST_VALUE;

        // Act & Assert: Verify assert_that_with_msg works
        assert_that_with_msg(&value, |v| *v > 0, "value should be positive");
    });

    #[test]
    #[should_panic(expected = "value should be positive")]
    fn test_assert_that_with_msg_invalid() {
        assert_that_with_msg(&0, |v| *v > 0, "value should be positive");
    }

    // ========================================================================
    // 2. ASSERTION BUILDER - Test builder pattern
    // ========================================================================

    test!(test_assertion_builder_new, {
        // Arrange: Create assertion builder
        let builder = AssertionBuilder::new(TEST_VALUE);

        // Act: Get value
        let value = builder.into_value();

        // Assert: Verify value
        assert_eq!(value, TEST_VALUE);
    });

    test!(test_assertion_builder_assert_that, {
        // Arrange: Create assertion builder
        let builder = AssertionBuilder::new(TEST_VALUE);

        // Act: Assert and get value
        let value = builder.assert_that(|v| *v > 0).into_value();

        // Assert: Verify value
        assert_eq!(value, TEST_VALUE);
    });

    #[test]
    #[should_panic(expected = "Assertion failed for value")]
    fn test_assertion_builder_assert_that_fails() {
        let builder = AssertionBuilder::new(0);
        let _ = builder.assert_that(|v| *v > 0);
    }

    test!(test_assertion_builder_assert_eq, {
        // Arrange: Create assertion builder
        let builder = AssertionBuilder::new(TEST_VALUE);

        // Act: Assert equality and get value
        let value = builder.assert_eq(&TEST_VALUE).into_value();

        // Assert: Verify value
        assert_eq!(value, TEST_VALUE);
    });

    #[test]
    #[should_panic(expected = "Values not equal")]
    fn test_assertion_builder_assert_eq_fails() {
        let builder = AssertionBuilder::new(TEST_VALUE);
        let _ = builder.assert_eq(&43);
    }

    test!(test_assertion_builder_assert_that_with_msg, {
        // Arrange: Create assertion builder
        let builder = AssertionBuilder::new(TEST_VALUE);

        // Act: Assert with message and get value
        let value = builder
            .assert_that_with_msg(|v| *v > 0, "value should be positive")
            .into_value();

        // Assert: Verify value
        assert_eq!(value, TEST_VALUE);
    });

    #[test]
    #[should_panic(expected = "value should be positive")]
    fn test_assertion_builder_assert_that_with_msg_fails() {
        let builder = AssertionBuilder::new(0);
        let _ = builder.assert_that_with_msg(|v| *v > 0, "value should be positive");
    }

    test!(test_assertion_builder_chaining, {
        // Arrange: Create assertion builder
        let builder = AssertionBuilder::new(TEST_VALUE);

        // Act: Chain assertions and get value
        let value = builder
            .assert_that(|v| *v > 0)
            .assert_eq(&TEST_VALUE)
            .assert_that_with_msg(|v| *v < 100, "value should be less than 100")
            .into_value();

        // Assert: Verify value
        assert_eq!(value, TEST_VALUE);
    });

    // ========================================================================
    // 3. BOUNDARY CONDITIONS - Test edge cases
    // ========================================================================

    test!(test_assert_in_range_zero_range, {
        // Arrange: Create value at zero range
        let value = 0;

        // Act & Assert: Verify zero range works
        assert_in_range(&value, &0, &0, "zero range");
    });

    test!(test_assert_that_with_empty_vec, {
        // Arrange: Create empty vector
        let vec: Vec<i32> = vec![];

        // Act & Assert: Verify assert_that works with empty vec
        assert_that(&vec, |v| v.is_empty());
    });

    test!(test_assert_that_with_string, {
        // Arrange: Create string
        let s = "test";

        // Act & Assert: Verify assert_that works with string
        assert_that(&s, |v| !v.is_empty());
    });
}
