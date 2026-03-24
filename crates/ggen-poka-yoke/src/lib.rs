//! Poka-yoke (error proofing) mechanisms using type-level guarantees.
//!
//! This crate provides compile-time error prevention through:
//! - Phantom types for state machine transitions
//! - Builder patterns with compile-time validation
//! - Runtime validators as last resort
//!
//! # Philosophy
//!
//! Make invalid states unrepresentable via the type system.
//! If code compiles, it's correct by construction.

pub mod builders;
pub mod type_guards;
pub mod validators;

/// Core trait for error-proofing mechanisms.
///
/// Types implementing this trait provide guarantees about their invariants.
pub trait ErrorProofing: Sized {
    /// The error type for validation failures.
    type Error: std::error::Error + Send + Sync + 'static;

    /// Validates invariants at construction time.
    ///
    /// This is a fallback for runtime validation when compile-time
    /// guarantees are insufficient.
    fn validate(&self) -> Result<(), Self::Error>;

    /// Constructs from validated input.
    ///
    /// Returns `Ok(Self)` if validation passes, `Err` otherwise.
    fn from_validated<T>(input: T) -> Result<Self, Self::Error>
    where
        T: TryInto<Self, Error = Self::Error>,
    {
        input.try_into()
    }
}

/// Marker trait for types that are valid by construction.
///
/// Types implementing this trait guarantee their invariants through
/// the type system alone, requiring no runtime validation.
pub trait ValidByConstruction: ErrorProofing {
    /// Always succeeds because construction is only possible in valid states.
    fn validate_infallible(&self) {
        // Type system guarantees correctness
    }
}

/// Marker trait for types with compile-time validated state transitions.
pub trait StateTransition {
    /// The next state type after transition.
    type Next;

    /// Transitions to the next state.
    ///
    /// The type system ensures only valid transitions are possible.
    fn transition(self) -> Self::Next;
}

/// Result type for error-proofing operations.
pub type PokaYokeResult<T, E> = Result<T, E>;

/// Marker trait for types that cannot be constructed in invalid states.
pub trait Unrepresentable {}

#[cfg(test)]
mod tests {
    use super::*;

    struct ValidType;

    impl ErrorProofing for ValidType {
        type Error = std::io::Error;

        fn validate(&self) -> Result<(), Self::Error> {
            Ok(())
        }
    }

    #[test]
    fn test_error_proofing_trait() {
        let valid = ValidType;
        assert!(valid.validate().is_ok());
    }
}
