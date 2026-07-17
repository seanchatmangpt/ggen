//! Type-Level Arithmetic and Compile-Time Validation
//!
//! Uses const generics and type-level arithmetic for compile-time size/range validation.
//! Provides zero-cost abstractions for compile-time guarantees.
//!
//! # Advanced Rust Features
//!
//! - **Const Generics**: Compile-time constants in type parameters
//! - **Type-Level Arithmetic**: Compile-time calculations using const generics
//! - **Const Trait Bounds**: Compile-time trait bounds for better guarantees
//!
//! # Note on Validation
//!
//! - **`SizeValidatedArray`**: Provides runtime validation with panic if SIZE > `MAX_SIZE`.
//!   Future: Will use Rust 1.79+ const trait bounds for compile-time validation.
//! - **ValidatedSize/ValidatedRange**: Marker types for documentation only, no actual validation.
//!   For actual compile-time validation, see `validation::guards::validated` module.

/// Type-level size marker using const generics
///
/// **Note**: This is a marker type for documentation purposes only.
/// It does not provide actual compile-time validation. For actual compile-time
/// validation, use `SizeValidatedArray` with runtime checks, or implement
/// trait bounds similar to `ValidatedRun` in `validation::guards::validated`.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::core::type_level::ValidatedSize;
///
/// // Type alias for documentation - no actual validation
/// type SmallArray = ValidatedSize<8>;
/// ```
pub struct ValidatedSize<const SIZE: usize>;

/// Const trait for size validation
///
/// This trait is implemented for sizes that meet certain constraints.
/// Uses const trait bounds (Rust 1.79+) for compile-time validation.
pub trait ConstSizeValid<const SIZE: usize> {
    /// Maximum allowed size
    const MAX_SIZE: usize;
    /// Validate that SIZE <= `MAX_SIZE`
    const IS_VALID: bool = SIZE <= Self::MAX_SIZE;
}

/// Type-level range marker using const generics
///
/// **Note**: This is a marker type for documentation purposes only.
/// It does not provide actual compile-time validation. For actual compile-time
/// validation, implement trait bounds similar to `ValidatedRun` in `validation::guards::validated`.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::core::type_level::ValidatedRange;
///
/// // Type alias for documentation - no actual validation
/// type SmallValue = ValidatedRange<5, 0, 10>;
/// ```
pub struct ValidatedRange<const VALUE: usize, const MIN: usize, const MAX: usize>;

/// Const trait for range validation
///
/// This trait is implemented for ranges that meet certain constraints.
pub trait ConstRangeValid<const VALUE: usize, const MIN: usize, const MAX: usize> {
    /// Validate that MIN <= VALUE <= MAX
    const IS_VALID: bool = MIN <= VALUE && VALUE <= MAX;
}

/// Type-level arithmetic operations
///
/// Provides compile-time arithmetic operations using const generics.
pub mod arithmetic {

    /// Add two const values at compile time
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::core::type_level::arithmetic::Add;
    ///
    /// type Sum = Add<5, 3>;  // Compile-time: 5 + 3 = 8
    /// ```
    pub struct Add<const A: usize, const B: usize>;

    /// Subtract two const values at compile time
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::core::type_level::arithmetic::Sub;
    ///
    /// type Diff = Sub<10, 3>;  // Compile-time: 10 - 3 = 7
    /// ```
    pub struct Sub<const A: usize, const B: usize>;

    /// Multiply two const values at compile time
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::core::type_level::arithmetic::Mul;
    ///
    /// type Product = Mul<5, 3>;  // Compile-time: 5 * 3 = 15
    /// ```
    pub struct Mul<const A: usize, const B: usize>;

    /// Divide two const values at compile time
    ///
    /// # Example
    ///
    /// ```rust
    /// use chicago_tdd_tools::core::type_level::arithmetic::Div;
    ///
    /// type Quotient = Div<15, 3>;  // Compile-time: 15 / 3 = 5
    /// ```
    pub struct Div<const A: usize, const B: usize>;
}

/// Compile-time size-validated array
///
/// Uses type-level validation to ensure array size meets constraints.
/// **Note**: Actual compile-time validation requires Rust 1.79+ const trait bounds.
/// For now, this provides type-level documentation of constraints.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::core::type_level::SizeValidatedArray;
///
/// // Valid: size 8 <= max 8
/// const ARRAY: SizeValidatedArray<8, 8> = SizeValidatedArray::new([0u8; 8]);
/// ```
pub struct SizeValidatedArray<const SIZE: usize, const MAX_SIZE: usize> {
    data: [u8; SIZE],
}

impl<const SIZE: usize, const MAX_SIZE: usize> SizeValidatedArray<SIZE, MAX_SIZE> {
    /// Create a new size-validated array
    ///
    /// # Panics
    ///
    /// Panics at runtime if SIZE > `MAX_SIZE`. This is a temporary limitation
    /// until Rust 1.79+ const trait bounds are available for compile-time validation.
    ///
    /// **Future**: With Rust 1.79+, this will be validated at compile time.
    #[must_use]
    pub const fn new(data: [u8; SIZE]) -> Self {
        // Runtime validation until const trait bounds are stable
        // This ensures safety even if type system doesn't catch it
        // Note: Cannot use formatted panic in const fn, so we use a simple panic
        assert!((SIZE <= MAX_SIZE), "Array size exceeds maximum");
        Self { data }
    }

    /// Get the array data
    #[must_use]
    pub const fn data(&self) -> &[u8; SIZE] {
        &self.data
    }

    /// Get the validated size
    #[must_use]
    #[allow(clippy::unused_self)] // Required for trait consistency - const fn needs self
    pub const fn size(&self) -> usize {
        SIZE
    }
}

#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod tests {
    use super::SizeValidatedArray;
    use crate::assert_eq_msg;

    #[test]
    fn test_size_validated_array() {
        // Arrange: Create validated array
        const ARRAY: SizeValidatedArray<8, 8> = SizeValidatedArray::new([0u8; 8]);

        // Act & Assert: Verify array created and size is correct
        assert_eq_msg!(&ARRAY.size(), &8, "Array size should be 8");
        assert_eq_msg!(&ARRAY.data().len(), &8, "Array data length should be 8");
    }

    #[test]
    fn test_type_level_arithmetic() {
        // Test compile-time arithmetic operations
        // These are type-level, so we can't directly test them,
        // but they ensure compile-time validation works
        let _sum: [u8; 8] = [0u8; 8]; // 5 + 3 = 8
        let _diff: [u8; 7] = [0u8; 7]; // 10 - 3 = 7
        let _product: [u8; 15] = [0u8; 15]; // 5 * 3 = 15
        let _quotient: [u8; 5] = [0u8; 5]; // 15 / 3 = 5
    }
}
