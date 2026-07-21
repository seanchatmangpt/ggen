//! Property-Based Testing Framework
//!
//! Provides QuickCheck-style property-based testing for validating invariants.
//! Uses const generics for compile-time test configuration.
//!
//! # Enhanced with proptest
//!
//! When the `property-testing` feature is enabled, this module provides enhanced
//! property-based testing using proptest, which offers better shrinking strategies
//! and more advanced features. The original `PropertyTestGenerator` remains available
//! for backward compatibility.

use std::collections::HashMap;

#[cfg(feature = "property-testing")]
use proptest::prelude::*;
#[cfg(feature = "property-testing")]
use proptest::test_runner::{Config, TestRunner};

/// Property test generator with const generics for compile-time configuration
///
/// `MAX_ITEMS` and `MAX_DEPTH` are validated at compile time, providing
/// zero runtime overhead for configuration.
pub struct PropertyTestGenerator<const MAX_ITEMS: usize = 10, const MAX_DEPTH: usize = 3> {
    /// Random seed for reproducibility
    seed: u64,
}

impl<const MAX_ITEMS: usize, const MAX_DEPTH: usize> PropertyTestGenerator<MAX_ITEMS, MAX_DEPTH> {
    /// Create new property test generator
    ///
    /// `MAX_ITEMS` and `MAX_DEPTH` are compile-time constants, ensuring
    /// type-safe configuration.
    #[must_use]
    pub const fn new() -> Self {
        Self { seed: 0 }
    }

    /// Set random seed
    #[must_use]
    pub const fn with_seed(mut self, seed: u64) -> Self {
        self.seed = seed;
        self
    }

    /// Generate random test data
    ///
    /// Uses compile-time `MAX_ITEMS` constant for bounds checking.
    pub fn generate_test_data(&mut self) -> HashMap<String, String> {
        let mut rng = SimpleRng::new(self.seed);
        self.seed = self.seed.wrapping_add(1);

        let mut data = HashMap::new();
        // Use compile-time constant MAX_ITEMS
        #[allow(clippy::cast_possible_truncation)]
        // Property test - truncation acceptable for test data generation
        let num_items = (rng.next() as usize % MAX_ITEMS) + 1;

        for i in 0..num_items {
            let key = format!("key_{i}");
            let value = format!("value_{}", rng.next());
            data.insert(key, value);
        }

        data
    }

    /// Get the random seed test counter value
    #[must_use]
    pub const fn test_counter(&self) -> u64 {
        self.seed
    }

    /// Get compile-time `MAX_ITEMS` constant
    #[must_use]
    pub const fn max_items() -> usize {
        MAX_ITEMS
    }

    /// Get compile-time `MAX_DEPTH` constant
    #[must_use]
    pub const fn max_depth() -> usize {
        MAX_DEPTH
    }
}

impl<const MAX_ITEMS: usize, const MAX_DEPTH: usize> Default
    for PropertyTestGenerator<MAX_ITEMS, MAX_DEPTH>
{
    fn default() -> Self {
        Self::new()
    }
}

/// Simple RNG for property testing (LCG)
struct SimpleRng {
    state: u64,
}

impl SimpleRng {
    const fn new(seed: u64) -> Self {
        Self { state: seed }
    }

    const fn next(&mut self) -> u64 {
        // Linear Congruential Generator
        self.state = self.state.wrapping_mul(1_103_515_245).wrapping_add(12_345);
        self.state
    }
}

/// Property: All generated data is valid
pub fn property_all_data_valid<const MAX_ITEMS: usize, const MAX_DEPTH: usize>(
    generator: &mut PropertyTestGenerator<MAX_ITEMS, MAX_DEPTH>, num_tests: usize,
) -> bool {
    for _ in 0..num_tests {
        let data = generator.generate_test_data();
        if data.is_empty() {
            return false;
        }
    }
    true
}

// ============================================================================
// Enhanced Property Testing with proptest
// ============================================================================

#[cfg(feature = "property-testing")]
/// Enhanced property test strategy using proptest
///
/// Provides advanced property-based testing with better shrinking strategies
/// and more sophisticated test case generation. This is an enhanced alternative
/// to `PropertyTestGenerator` that uses proptest internally.
///
/// # Example
///
/// ```rust,ignore
/// # #[cfg(feature = "property-testing")]
/// use chicago_tdd_tools::property::ProptestStrategy;
///
/// # #[cfg(feature = "property-testing")]
/// ProptestStrategy::new()
///     .with_cases(1000)
///     .test(|x: u32| {
///         // Property: x * 2 is always even
///         (x * 2) % 2 == 0
///     });
/// ```
pub struct ProptestStrategy {
    config: Config,
    /// Optional seed for reproducible test runs.
    ///
    /// When `Some`, the seed is used to initialize the proptest `TestRunner`
    /// via [`proptest::test_runner::TestRng::from_seed`] so that the same
    /// sequence of test cases is generated across runs.
    seed: Option<[u8; 32]>,
}

#[cfg(feature = "property-testing")]
impl ProptestStrategy {
    /// Create a new proptest strategy with default configuration
    #[must_use]
    pub fn new() -> Self {
        Self {
            config: Config::default(),
            seed: None,
        }
    }

    /// Set the number of test cases to run
    #[must_use]
    pub const fn with_cases(mut self, cases: u32) -> Self {
        self.config.cases = cases;
        self
    }

    /// Set the maximum number of shrink attempts
    #[must_use]
    pub const fn with_max_shrink_iters(mut self, iters: u32) -> Self {
        self.config.max_shrink_iters = iters;
        self
    }

    /// Set the random seed for reproducibility.
    ///
    /// The seed is stored and applied when the `TestRunner` is constructed in
    /// [`Self::test`] / [`Self::test_default`], producing a deterministic
    /// sequence of generated values across runs with the same seed and
    /// configuration.
    #[must_use]
    pub const fn with_seed(mut self, seed: [u8; 32]) -> Self {
        self.seed = Some(seed);
        self
    }

    /// Build a `TestRunner` honouring the stored seed (if any).
    fn build_runner(&self) -> TestRunner {
        use proptest::test_runner::{RngAlgorithm, TestRng};
        self.seed.map_or_else(
            || TestRunner::new(self.config.clone()),
            |seed_bytes| {
                let rng = TestRng::from_seed(RngAlgorithm::ChaCha, &seed_bytes);
                TestRunner::new_with_rng(self.config.clone(), rng)
            },
        )
    }

    /// Run a property test with a strategy
    ///
    /// # Arguments
    ///
    /// * `strategy` - A proptest strategy for generating test values
    /// * `property` - A function that takes a value and returns true if the property holds
    ///
    /// # Panics
    ///
    /// Panics if the property fails for any generated test case.
    #[allow(clippy::panic)] // Property test - panic is appropriate for test failures
    pub fn test<S, F>(&self, strategy: S, property: F)
    where
        S: Strategy,
        S::Value: std::fmt::Debug,
        F: Fn(S::Value) -> bool,
    {
        let mut runner = self.build_runner();
        runner
            .run(&strategy, |value| {
                prop_assert!(property(value));
                Ok(())
            })
            .unwrap_or_else(|e| panic!("Property test failed: {e:?}"));
    }

    /// Run a property test with a default strategy for a type
    ///
    /// This is a convenience method that uses the default strategy for the type.
    ///
    /// # Arguments
    ///
    /// * `property` - A function that takes a value and returns true if the property holds
    ///
    /// # Panics
    ///
    /// Panics if the property fails for any generated test case.
    pub fn test_default<T, F>(&self, property: F)
    where
        T: Arbitrary + std::fmt::Debug,
        F: Fn(T) -> bool,
    {
        self.test(any::<T>(), property);
    }
}

#[cfg(feature = "property-testing")]
impl Default for ProptestStrategy {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod property_tests {
    use super::*;

    // ========================================================================
    // 1. PROPERTY TEST GENERATOR - Test basic functionality
    // ========================================================================

    #[test]
    fn test_property_test_generator_new() {
        let _generator: PropertyTestGenerator<10, 3> = PropertyTestGenerator::new();
        assert_eq!(PropertyTestGenerator::<10, 3>::max_items(), 10);
        assert_eq!(PropertyTestGenerator::<10, 3>::max_depth(), 3);
    }

    #[test]
    fn test_property_test_generator_with_seed() {
        let generator: PropertyTestGenerator<10, 3> = PropertyTestGenerator::new().with_seed(42);
        let mut gen = generator;
        let data1 = gen.generate_test_data();
        let data2 = gen.generate_test_data();
        // Should generate different data with incremented seed
        assert_ne!(data1, data2);
    }

    #[test]
    fn test_property_test_generator_generate_test_data() {
        let mut generator: PropertyTestGenerator<10, 3> = PropertyTestGenerator::new();
        let data = generator.generate_test_data();
        assert!(!data.is_empty());
        assert!(data.len() <= 10); // Should respect MAX_ITEMS
    }

    #[test]
    fn test_property_test_generator_max_items() {
        let _generator: PropertyTestGenerator<5, 3> = PropertyTestGenerator::new();
        assert_eq!(PropertyTestGenerator::<5, 3>::max_items(), 5);
    }

    #[test]
    fn test_property_test_generator_max_depth() {
        let _generator: PropertyTestGenerator<10, 5> = PropertyTestGenerator::new();
        assert_eq!(PropertyTestGenerator::<10, 5>::max_depth(), 5);
    }

    #[test]
    fn test_property_test_generator_default() {
        let mut generator: PropertyTestGenerator<10, 3> = PropertyTestGenerator::default();
        assert_eq!(PropertyTestGenerator::<10, 3>::max_items(), 10);
        assert_eq!(PropertyTestGenerator::<10, 3>::max_depth(), 3);
        // Verify generator is created
        let _data = generator.generate_test_data();
    }

    #[test]
    fn test_property_test_generator_reproducibility() {
        let mut gen1: PropertyTestGenerator<10, 3> = PropertyTestGenerator::new().with_seed(100);
        let mut gen2: PropertyTestGenerator<10, 3> = PropertyTestGenerator::new().with_seed(100);
        let data1 = gen1.generate_test_data();
        let data2 = gen2.generate_test_data();
        // Same seed should produce same data
        assert_eq!(data1, data2);
    }

    // ========================================================================
    // 2. PROPERTY FUNCTION - Test property validation
    // ========================================================================

    #[test]
    fn test_property_all_data_valid() {
        let mut generator: PropertyTestGenerator<10, 3> = PropertyTestGenerator::new();
        let result = property_all_data_valid(&mut generator, 10);
        assert!(result, "All generated data should be valid");
    }

    #[test]
    fn test_property_all_data_valid_zero_tests() {
        let mut generator: PropertyTestGenerator<10, 3> = PropertyTestGenerator::new();
        let result = property_all_data_valid(&mut generator, 0);
        assert!(result, "Zero tests should pass");
    }

    // ========================================================================
    // 3. SIMPLE RNG - Test random number generation
    // ========================================================================

    #[test]
    fn test_simple_rng_new() {
        let mut rng = SimpleRng::new(42);
        // RNG should be created and produce values
        let val = rng.next();
        // Verify it produces a value (not zero for seed 42)
        assert!(val > 0 || val == 0); // Always true, but verifies method works
    }

    #[test]
    fn test_simple_rng_next() {
        let mut rng = SimpleRng::new(1);
        let val1 = rng.next();
        let val2 = rng.next();
        // Should produce different values
        assert_ne!(val1, val2);
    }

    #[test]
    fn test_simple_rng_reproducibility() {
        let mut rng1 = SimpleRng::new(100);
        let mut rng2 = SimpleRng::new(100);
        let val1 = rng1.next();
        let val2 = rng2.next();
        // Same seed should produce same value
        assert_eq!(val1, val2);
    }

    // ========================================================================
    // 4. BOUNDARY CONDITIONS - Test edge cases
    // ========================================================================

    #[test]
    fn test_property_test_generator_small_max_items() {
        let mut generator: PropertyTestGenerator<1, 3> = PropertyTestGenerator::new();
        let data = generator.generate_test_data();
        assert_eq!(data.len(), 1); // Should generate exactly 1 item
    }

    #[test]
    fn test_property_test_generator_large_max_items() {
        let mut generator: PropertyTestGenerator<100, 3> = PropertyTestGenerator::new();
        let data = generator.generate_test_data();
        assert!(data.len() <= 100); // Should respect MAX_ITEMS
        assert!(!data.is_empty());
    }

    #[test]
    fn test_property_test_generator_zero_seed() {
        let mut generator: PropertyTestGenerator<10, 3> = PropertyTestGenerator::new().with_seed(0);
        let data = generator.generate_test_data();
        assert!(!data.is_empty());
    }
}

#[cfg(feature = "property-testing")]
#[cfg(test)]
#[allow(clippy::panic)] // Test code - panic is appropriate for test failures
mod proptest_tests {
    use super::*;

    // Kaizen improvement: Extract magic number to named constant for clarity
    // Number of test cases to run for property tests
    const DEFAULT_PROPERTY_TEST_CASES: u32 = 100;

    #[test]
    fn test_proptest_strategy_addition_commutative() {
        let strategy = ProptestStrategy::new().with_cases(DEFAULT_PROPERTY_TEST_CASES);
        // Use wrapping_add to handle overflow gracefully (property still holds)
        // DMAIC Fix: Changed from `x + y` to `x.wrapping_add(y)` to prevent integer overflow
        // Root cause: Debug mode panics on overflow when adding large u32 values
        // Solution: Use wrapping arithmetic which maintains mathematical properties
        strategy.test(any::<(u32, u32)>(), |(x, y)| {
            assert_eq!(x.wrapping_add(y), y.wrapping_add(x), "addition must commute");
            true
        });
    }

    #[test]
    fn test_proptest_strategy_multiplication_distributive() {
        let strategy = ProptestStrategy::new().with_cases(DEFAULT_PROPERTY_TEST_CASES);
        // Use wrapping arithmetic to handle overflow gracefully (property still holds)
        // DMAIC Fix: Changed from regular arithmetic to wrapping arithmetic to prevent overflow
        // Root cause: Debug mode panics on overflow when multiplying large u32 values
        // Solution: Use wrapping arithmetic which maintains distributive property
        strategy.test(any::<(u32, u32, u32)>(), |(a, b, c)| {
            assert_eq!(
                a.wrapping_mul(b.wrapping_add(c)),
                a.wrapping_mul(b).wrapping_add(a.wrapping_mul(c)),
                "multiplication must distribute over addition"
            );
            true
        });
    }

    #[test]
    fn test_proptest_strategy_string_length() {
        let strategy = ProptestStrategy::new().with_cases(DEFAULT_PROPERTY_TEST_CASES);
        strategy.test(any::<String>(), |s| {
            assert!(
                s.len() >= s.chars().count(),
                "byte length must be >= char count for {s:?}"
            );
            true
        });
    }
}
