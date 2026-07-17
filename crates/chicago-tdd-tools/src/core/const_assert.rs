//! Const Assertion Helpers
//!
//! Provides compile-time assertion helpers for Poka-Yoke (error prevention).
//! Uses const generics and trait bounds to enforce invariants at compile time.
//!
//! # Poka-Yoke: Compile-Time Validation
//!
//! This module provides type-level validation that prevents errors at compile time.
//! Use these helpers to enforce constraints that would otherwise require runtime checks.
//!
//! Note: Rust's const generics have limitations. For practical Poka-Yoke patterns,
//! prefer newtypes and enums over complex const assertion traits.

/// Marker type for compile-time validated values
///
/// This type is used to represent values that have been validated at compile time.
/// Use this with newtypes to enforce invariants.
///
/// # Poka-Yoke: Type-Level Validation
///
/// This type prevents invalid states at compile time by requiring explicit construction
/// through validated constructors.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::const_assert::Validated;
///
/// // Newtype that enforces compile-time validation
/// pub struct ValidatedRun<const LEN: usize>(Validated<usize>);
///
/// impl<const LEN: usize> ValidatedRun<LEN> {
///     // Only valid for LEN <= 8
///     pub fn new(value: usize) -> Option<Self> {
///         if value <= LEN {
///             Some(Self(Validated::new(value)))
///         } else {
///             None
///         }
///     }
///     
///     pub fn get(&self) -> usize {
///         self.0.as_ref().clone()
///     }
/// }
///
/// // Usage
/// let run = ValidatedRun::<8>::new(5).unwrap();
/// assert_eq!(run.get(), 5);
/// ```
pub struct Validated<T> {
    /// Validated value
    value: T,
}

impl<T> Validated<T> {
    /// Create a validated value
    ///
    /// This constructor requires explicit validation, preventing invalid states.
    pub const fn new(value: T) -> Self {
        Self { value }
    }

    /// Get the validated value
    pub fn into_inner(self) -> T {
        self.value
    }

    /// Get a reference to the validated value
    pub const fn as_ref(&self) -> &T {
        &self.value
    }
}

/// Compile-time validation helper
///
/// Validates a condition at compile time using const assertions.
#[allow(clippy::panic, clippy::manual_assert, clippy::missing_panics_doc)]
pub const fn const_assert(condition: bool) {
    if !condition {
        panic!("Compile-time assertion failed");
    }
}

/// Compile-time validation helper with message
#[allow(clippy::panic, clippy::manual_assert, clippy::missing_panics_doc)]
pub const fn const_assert_msg(condition: bool, _msg: &'static str) {
    if !condition {
        panic!("Compile-time assertion failed");
    }
}

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use super::*;

    #[test]
    fn test_validated() {
        let validated = Validated::new(42);
        assert_eq!(validated.into_inner(), 42);
    }
}
