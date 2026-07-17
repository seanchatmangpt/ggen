//! JSON Assertion Macros
//!
//! **New in v1.3.0**: Assertions for semantic JSON comparison.

/// Assert that two JSON values are semantically equal
///
/// **New in v1.3.0**: Semantic JSON comparison with pretty-printed diffs.
///
/// Compares JSON values semantically:
/// - Ignores key order in objects
/// - Ignores whitespace differences
/// - Provides pretty-printed diff on failure
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::assert_json_eq;
/// use serde_json::json;
///
/// let actual = json!({
///     "name": "Alice",
///     "age": 30
/// });
/// let expected = json!({
///     "age": 30,
///     "name": "Alice"
/// });
/// assert_json_eq!(actual, expected); // Passes despite different key order
///
/// // With custom message
/// let response = json!({"status": "ok"});
/// let expected_response = json!({"status": "ok"});
/// assert_json_eq!(response, expected_response, "API response should match");
/// ```
#[macro_export]
macro_rules! assert_json_eq {
    ($actual:expr, $expected:expr) => {{
        let actual_ref = &$actual;
        let expected_ref = &$expected;
        if actual_ref != expected_ref {
            let actual_pretty = serde_json::to_string_pretty(actual_ref)
                .unwrap_or_else(|_| format!("{:?}", actual_ref));
            let expected_pretty = serde_json::to_string_pretty(expected_ref)
                .unwrap_or_else(|_| format!("{:?}", expected_ref));
            panic!(
                "JSON values are not equal.\n  actual:\n{}\n  expected:\n{}",
                actual_pretty, expected_pretty
            );
        }
    }};
    ($actual:expr, $expected:expr, $msg:expr) => {{
        let actual_ref = &$actual;
        let expected_ref = &$expected;
        if actual_ref != expected_ref {
            let actual_pretty = serde_json::to_string_pretty(actual_ref)
                .unwrap_or_else(|_| format!("{:?}", actual_ref));
            let expected_pretty = serde_json::to_string_pretty(expected_ref)
                .unwrap_or_else(|_| format!("{:?}", expected_ref));
            panic!(
                "{}: JSON values are not equal.\n  actual:\n{}\n  expected:\n{}",
                $msg, actual_pretty, expected_pretty
            );
        }
    }};
}

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use crate::test;

    test!(test_assert_json_eq_macro, {
        use serde_json::json;

        // Arrange: JSON values with different key orders
        let actual = json!({
            "name": "Alice",
            "age": 30
        });
        let expected = json!({
            "age": 30,
            "name": "Alice"
        });
        let response = json!({"status": "ok"});
        let expected_response = json!({"status": "ok"});

        // Act & Assert: Verify assert_json_eq! macro works (ignores key order)
        assert_json_eq!(actual, expected);
        assert_json_eq!(response, expected_response);
        assert_json_eq!(response, expected_response, "API response should match");
    });

    #[test]
    #[should_panic(expected = "JSON values are not equal")]
    fn test_assert_json_eq_macro_fails() {
        use serde_json::json;

        // Arrange: Different JSON values
        let actual = json!({"name": "Alice"});
        let expected = json!({"name": "Bob"});

        // Act & Assert: Should panic
        assert_json_eq!(actual, expected);
    }
}
