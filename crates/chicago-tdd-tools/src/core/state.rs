//! Type-Level Programming for Test State
//!
//! Implements type state pattern for compile-time test lifecycle guarantees.
//! Ensures AAA pattern is enforced at compile time - impossible to call Act before Arrange.
//!
//! # Advanced Rust Features
//!
//! - **Type State Pattern**: Compile-time state machine using `PhantomData`
//! - **Sealed Traits**: API safety and extensibility control
//! - **Zero-Sized Types**: Zero-cost abstractions for state tracking

/// Sealed trait pattern for phase markers
///
/// This trait is sealed (only implementable within this crate) to prevent
/// external implementations that might violate invariants.
mod private {
    /// Sealed marker trait for test phases
    /// Note: This trait is used via the sealed trait pattern, not directly
    #[allow(dead_code)]
    pub trait Sealed {}
}

/// Marker type for Arrange phase
///
/// This is a zero-sized type used for type-level state tracking.
/// Implements Sealed to prevent external implementations.
pub struct Arrange;

impl private::Sealed for Arrange {}

/// Marker type for Act phase
///
/// This is a zero-sized type used for type-level state tracking.
/// Implements Sealed to prevent external implementations.
pub struct Act;

impl private::Sealed for Act {}

/// Marker type for Assert phase
///
/// This is a zero-sized type used for type-level state tracking.
/// Implements Sealed to prevent external implementations.
pub struct Assert;

impl private::Sealed for Assert {}

/// Test state with type-level phase tracking
///
/// This type ensures that test phases are followed in the correct order:
/// Arrange -> Act -> Assert
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::state::{TestState, Arrange, Act, Assert};
///
/// // Start with Arrange phase
/// let arrange_state = TestState::<Arrange>::new();
///
/// // Transition to Act phase
/// let act_state = arrange_state.act();
///
/// // Transition to Assert phase
/// let assert_state = act_state.assert();
///
/// // Verify state transitions work (type system enforces order)
/// // arrange_state can only transition to act_state
/// // act_state can only transition to assert_state
/// ```
pub struct TestState<Phase> {
    /// Phase marker (zero-sized type)
    _phase: std::marker::PhantomData<Phase>,
    /// Test data (can be extended)
    data: TestData,
}

/// Test data container
#[derive(Default)]
struct TestData {
    /// Arrange data
    arrange_data: Option<Vec<u8>>,
    /// Act result
    act_result: Option<Vec<u8>>,
}

impl TestState<Arrange> {
    /// Create a new test state in Arrange phase
    #[must_use]
    pub fn new() -> Self {
        Self { _phase: std::marker::PhantomData, data: TestData::default() }
    }

    /// Add arrange data
    #[must_use]
    pub fn with_arrange_data(mut self, data: Vec<u8>) -> Self {
        self.data.arrange_data = Some(data);
        self
    }

    /// Transition to Act phase
    ///
    /// This consumes the Arrange state and returns an Act state.
    /// This ensures that Act can only be called after Arrange.
    #[must_use]
    pub fn act(self) -> TestState<Act> {
        TestState { _phase: std::marker::PhantomData, data: self.data }
    }
}

impl TestState<Act> {
    /// Execute act operation
    #[must_use]
    pub fn execute<F>(mut self, f: F) -> Self
    where
        F: FnOnce(Option<Vec<u8>>) -> Vec<u8>,
    {
        let input = self.data.act_result.clone().or_else(|| self.data.arrange_data.clone());
        let result = f(input);
        self.data.act_result = Some(result);
        self
    }

    /// Transition to Assert phase
    ///
    /// This consumes the Act state and returns an Assert state.
    /// This ensures that Assert can only be called after Act.
    #[must_use]
    pub fn assert(self) -> TestState<Assert> {
        TestState { _phase: std::marker::PhantomData, data: self.data }
    }
}

impl TestState<Assert> {
    /// Get act result for assertion
    #[must_use]
    pub const fn act_result(&self) -> Option<&Vec<u8>> {
        self.data.act_result.as_ref()
    }

    /// Get arrange data for assertion
    #[must_use]
    pub const fn arrange_data(&self) -> Option<&Vec<u8>> {
        self.data.arrange_data.as_ref()
    }

    /// Assert with predicate
    pub fn assert_that<F>(&self, predicate: F) -> bool
    where
        F: FnOnce(Option<&Vec<u8>>) -> bool,
    {
        predicate(self.act_result())
    }
}

impl Default for TestState<Arrange> {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use super::*;

    #[test]
    fn test_state_lifecycle() {
        // Arrange
        let arrange_state = TestState::<Arrange>::new().with_arrange_data(vec![1, 2, 3]);

        // Act
        let act_state = arrange_state.act();
        let act_state = act_state.execute(|data| {
            let mut result = data.unwrap_or_default();
            result.push(4);
            result
        });

        // Assert
        let assert_state = act_state.assert();
        assert!(
            assert_state.assert_that(|result| { result.map(|r| r.len() == 4).unwrap_or(false) })
        );
    }

    #[test]
    fn test_state_prevents_wrong_order() {
        // Verify that only the lawful Arrange -> Act -> Assert path is reachable.
        // Invalid transitions (e.g. calling assert() on Arrange, or constructing
        // Act directly) are rejected at compile time by the type system — there
        // is no assert() method on TestState<Arrange> and no public Act::new().
        //
        // This test confirms the positive invariant: every method in the lawful
        // sequence is callable, and the final assert_that predicate fires
        // exactly once with the value produced by the Act phase.

        let mut predicate_called = false;

        let arrange = TestState::<Arrange>::new().with_arrange_data(vec![10, 20, 30]);
        let act = arrange.act().execute(|data| {
            let mut v = data.unwrap_or_default();
            v.push(40);
            v
        });
        let result = act.assert();

        // The act result must contain the four elements appended across both phases.
        assert!(result.assert_that(|r| {
            predicate_called = true;
            r.map(|v| v == &[10u8, 20, 30, 40]).unwrap_or(false)
        }));

        // Ensure the predicate actually ran — not vacuously true.
        assert!(predicate_called, "assert_that predicate was never invoked");

        // Arrange data must remain accessible after the act phase.
        assert_eq!(result.arrange_data(), Some(&vec![10u8, 20, 30]));
    }
}
