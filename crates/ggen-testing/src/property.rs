//! Property-Based Testing Integration
//!
//! Integrates proptest for property-based testing with Chicago TDD principles.
//! Focuses on verifying properties of state transformations.
//!
//! # Examples
//!
//! ```
//! use ggen_testing::property::*;
//! use proptest::prelude::*;
//!
//! proptest! {
//!     #[test]
//!     fn test_addition_commutative(a: i32, b: i32) {
//!         prop_assert_eq!(a + b, b + a);
//!     }
//! }
//! ```

pub use proptest::prelude::*;
use anyhow::Result;

/// Strategy builder for common test data patterns
pub struct StrategyBuilder;

impl StrategyBuilder {
    /// Generate non-empty strings
    #[must_use]
    pub fn non_empty_string() -> impl Strategy<Value = String> {
        "[a-zA-Z0-9]{1,100}".prop_map(|s| s.to_string())
    }

    /// Generate valid identifiers (alphanumeric with underscores)
    #[must_use]
    pub fn identifier() -> impl Strategy<Value = String> {
        "[a-zA-Z_][a-zA-Z0-9_]{0,63}".prop_map(|s| s.to_string())
    }

    /// Generate valid file names
    #[must_use]
    pub fn file_name() -> impl Strategy<Value = String> {
        "[a-zA-Z0-9_-]{1,255}\\.[a-z]{1,10}".prop_map(|s| s.to_string())
    }

    /// Generate positive integers
    #[must_use]
    pub fn positive_int() -> impl Strategy<Value = i32> {
        1..=i32::MAX
    }

    /// Generate non-negative integers
    #[must_use]
    pub fn non_negative_int() -> impl Strategy<Value = i32> {
        0..=i32::MAX
    }

    /// Generate bounded integers
    #[must_use]
    pub fn bounded_int(min: i32, max: i32) -> impl Strategy<Value = i32> {
        min..=max
    }

    /// Generate non-empty vectors
    #[must_use]
    pub fn non_empty_vec<T: std::fmt::Debug + Clone>(
        element: impl Strategy<Value = T>,
    ) -> impl Strategy<Value = Vec<T>> {
        prop::collection::vec(element, 1..100)
    }

    /// Generate unique vectors (no duplicates)
    #[must_use]
    pub fn unique_vec<T: std::fmt::Debug + Clone + Eq + std::hash::Hash>(
        element: impl Strategy<Value = T>,
    ) -> impl Strategy<Value = Vec<T>> {
        prop::collection::hash_set(element, 1..100).prop_map(|s| s.into_iter().collect())
    }
}

/// Property test helper for state transformations
///
/// Verifies that a transformation maintains certain properties
pub struct PropertyTest<S> {
    initial_state: S,
}

impl<S: Clone> PropertyTest<S> {
    /// Create a new property test with initial state
    #[must_use]
    pub fn new(initial_state: S) -> Self {
        Self { initial_state }
    }

    /// Test that transformation is idempotent
    ///
    /// # Errors
    ///
    /// Returns error if transformation fails
    pub fn assert_idempotent<F>(&self, transform: F) -> Result<()>
    where
        F: Fn(S) -> Result<S>,
        S: PartialEq + std::fmt::Debug,
    {
        let once = transform(self.initial_state.clone())?;
        let twice = transform(once.clone())?;
        anyhow::ensure!(
            once == twice,
            "Transformation is not idempotent: {:?} != {:?}",
            once,
            twice
        );
        Ok(())
    }

    /// Test that transformation is reversible
    ///
    /// # Errors
    ///
    /// Returns error if transformation or reverse fails
    pub fn assert_reversible<F, R>(&self, transform: F, reverse: R) -> Result<()>
    where
        F: Fn(S) -> Result<S>,
        R: Fn(S) -> Result<S>,
        S: PartialEq + std::fmt::Debug,
    {
        let transformed = transform(self.initial_state.clone())?;
        let reversed = reverse(transformed)?;
        anyhow::ensure!(
            self.initial_state == reversed,
            "Transformation is not reversible: {:?} != {:?}",
            self.initial_state,
            reversed
        );
        Ok(())
    }

    /// Test that transformation is commutative with another
    ///
    /// # Errors
    ///
    /// Returns error if transformations fail
    pub fn assert_commutative<F1, F2>(&self, f1: F1, f2: F2) -> Result<()>
    where
        F1: Fn(S) -> Result<S>,
        F2: Fn(S) -> Result<S>,
        S: PartialEq + std::fmt::Debug,
    {
        let order1 = f2(f1(self.initial_state.clone())?)?;
        let order2 = f1(f2(self.initial_state.clone())?)?;
        anyhow::ensure!(
            order1 == order2,
            "Transformations are not commutative: {:?} != {:?}",
            order1,
            order2
        );
        Ok(())
    }
}

/// Test invariants that should hold for all generated values
///
/// # Panics
///
/// Panics if the invariant does not hold
pub fn assert_invariant<T, F>(value: &T, predicate: F, message: &str)
where
    T: std::fmt::Debug,
    F: Fn(&T) -> bool,
{
    assert!(predicate(value), "{message}: {value:?}");
}

/// Test monotonic properties (order preservation)
///
/// # Panics
///
/// Panics if monotonicity is violated
pub fn assert_monotonic<T, F>(values: &[T], transform: F)
where
    T: PartialOrd + std::fmt::Debug + Clone,
    F: Fn(&T) -> T,
{
    for i in 0..values.len().saturating_sub(1) {
        let a = &values[i];
        let b = &values[i + 1];
        if a <= b {
            let ta = transform(a);
            let tb = transform(b);
            assert!(
                ta <= tb,
                "Monotonicity violated: transform({a:?}) = {ta:?}, transform({b:?}) = {tb:?}"
            );
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    proptest! {
        #[test]
        fn test_non_empty_string_generates(s in StrategyBuilder::non_empty_string()) {
            assert!(!s.is_empty());
        }

        #[test]
        fn test_identifier_valid(id in StrategyBuilder::identifier()) {
            assert!(id.chars().next().map_or(false, |c| c.is_alphabetic() || c == '_'));
        }

        #[test]
        fn test_positive_int_positive(n in StrategyBuilder::positive_int()) {
            assert!(n > 0);
        }
    }

    #[test]
    fn test_idempotent_property() {
        let test = PropertyTest::new(vec![1, 2, 3]);
        let sort = |mut v: Vec<i32>| -> Result<Vec<i32>> {
            v.sort();
            Ok(v)
        };
        assert!(test.assert_idempotent(sort).is_ok());
    }

    #[test]
    fn test_reversible_property() {
        let test = PropertyTest::new(5);
        let add = |x: i32| -> Result<i32> { Ok(x + 10) };
        let sub = |x: i32| -> Result<i32> { Ok(x - 10) };
        assert!(test.assert_reversible(add, sub).is_ok());
    }
}
