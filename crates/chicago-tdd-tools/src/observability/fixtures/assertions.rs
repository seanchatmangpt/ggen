//! Assertion helpers for Weaver live-check validation results.

#![cfg(all(feature = "weaver", feature = "otel"))]

use crate::observability::{ObservabilityError, ObservabilityResult};

use super::ValidationResults;

/// Ensure no Weaver violations were detected.
///
/// # Errors
///
/// Returns an error if any violations were detected.
pub fn assert_telemetry_valid(results: &ValidationResults) -> ObservabilityResult<()> {
    if results.has_violations() {
        Err(ObservabilityError::ValidationFailed(results.violations_summary()))
    } else {
        Ok(())
    }
}

/// Ensure the number of Weaver violations matches the expected value.
///
/// # Errors
///
/// Returns an error if the violation count doesn't match the expected value.
pub fn assert_violation_count(
    results: &ValidationResults,
    expected: usize,
) -> ObservabilityResult<()> {
    let actual = results
        .advices()
        .filter(|advice| matches!(advice.level, super::AdviceLevel::Violation))
        .count();

    if actual == expected {
        Ok(())
    } else {
        Err(ObservabilityError::ValidationFailed(format!(
            "Expected {expected} Weaver violations, found {actual}\n{}",
            results.violations_summary()
        )))
    }
}

/// Panic if Weaver live-check detected any violations.
#[macro_export]
macro_rules! assert_telemetry_valid {
    ($results:expr $(,)?) => {{
        $crate::observability::fixtures::assert_telemetry_valid($results)
            .unwrap_or_else(|err| panic!("Weaver live-check validation failed: {err}"));
    }};
    ($results:expr, $($arg:tt)+) => {{
        $crate::observability::fixtures::assert_telemetry_valid($results)
            .unwrap_or_else(|err| panic!("{}: {err}", format_args!($($arg)+)));
    }};
}

/// Panic if Weaver live-check produced a different number of violations.
#[macro_export]
macro_rules! assert_violation_count {
    ($results:expr, $expected:expr $(,)?) => {{
        $crate::observability::fixtures::assert_violation_count($results, $expected)
            .unwrap_or_else(|err| {
                panic!(
                    "Weaver live-check violation count assertion failed: {err}"
                )
            });
    }};
    ($results:expr, $expected:expr, $($arg:tt)+) => {{
        $crate::observability::fixtures::assert_violation_count($results, $expected)
            .unwrap_or_else(|err| {
                panic!(
                    "{}: {err}",
                    format_args!($($arg)+)
                )
            });
    }};
}
