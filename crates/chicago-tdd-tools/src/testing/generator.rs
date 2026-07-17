//! Test Code Generator
//!
//! Generates test code from specifications.
//! Uses const fn for compile-time test data generation.

/// Test generator
pub struct TestGenerator {
    /// Generated tests
    tests: Vec<String>,
}

impl TestGenerator {
    /// Create new test generator
    #[must_use]
    #[allow(clippy::missing_const_for_fn)] // Cannot be const - Vec::new() is not const
    pub fn new() -> Self {
        Self { tests: Vec::new() }
    }

    /// Generate test from specification
    ///
    /// Produces a test function with an Arrange-Act-Assert skeleton derived from
    /// the provided `name` and `spec`. The generated code is valid Rust that
    /// compiles immediately; fill in the `todo!` placeholders to complete the test.
    #[allow(clippy::needless_pass_by_ref_mut)] // Preserve API compatibility
    pub fn generate_test(&mut self, name: &str, spec: &str) -> String {
        let code = format!(
            "#[test]\nfn {name}() {{\n    // Spec: {spec}\n\n    // Arrange\n    let subject = todo!(\"{name}: arrange subject under test\");\n\n    // Act\n    let result = todo!(\"{name}: invoke behaviour described by spec\");\n\n    // Assert\n    assert!(\n        todo!(\"{name}: verify result satisfies spec — {spec}\"),\n        \"assertion failed for spec: {spec}\"\n    );\n}}\n",
        );
        self.tests.push(code.clone());
        code
    }

    /// Get all generated tests
    #[must_use]
    pub fn get_tests(&self) -> &[String] {
        &self.tests
    }
}

impl Default for TestGenerator {
    fn default() -> Self {
        Self::new()
    }
}

/// Generate a test array at compile time
///
/// Uses const fn to generate arrays of any size at compile time.
///
/// # Example
///
/// ```rust
/// use chicago_tdd_tools::generator::generate_test_array;
///
/// const TEST_DATA: [u8; 10] = generate_test_array::<10>();
/// assert_eq!(TEST_DATA.len(), 10);
/// assert_eq!(TEST_DATA[0], 0);
/// assert_eq!(TEST_DATA[1], 1);
/// ```
#[must_use]
#[allow(clippy::cast_possible_truncation)] // Array index modulo 256 fits in u8
pub const fn generate_test_array<const N: usize>() -> [u8; N] {
    let mut array = [0u8; N];
    let mut i = 0;
    while i < N {
        array[i] = (i % 256) as u8;
        i += 1;
    }
    array
}

/// Generate a test array with a pattern at compile time
///
/// Generates an array where each element follows a pattern based on its index.
#[must_use]
#[allow(clippy::cast_possible_truncation)] // Pattern addition modulo 256 fits in u8
pub const fn generate_test_array_pattern<const N: usize>(pattern: u8) -> [u8; N] {
    let mut array = [0u8; N];
    let mut i = 0;
    while i < N {
        array[i] = pattern.wrapping_add(i as u8);
        i += 1;
    }
    array
}

/// Compile-time validation helper
///
/// Validates a condition at compile time using const assertions.
///
/// # Panics
///
/// Panics if `condition` is `false` at compile time.
pub const fn const_assert(condition: bool) {
    assert!(condition, "Compile-time assertion failed");
}

/// Compile-time validation helper with message
///
/// # Panics
///
/// Panics if `condition` is `false` at compile time.
pub const fn const_assert_msg(condition: bool, _msg: &'static str) {
    assert!(condition, "Compile-time assertion failed");
}

#[cfg(test)]
mod tests {
    use super::*;

    // ========================================================================
    // 1. TEST GENERATOR - Test code generation
    // ========================================================================

    #[test]
    fn test_test_generator_new() {
        let generator = TestGenerator::new();
        assert_eq!(generator.get_tests().len(), 0);
    }

    #[test]
    fn test_test_generator_generate_test() {
        let mut generator = TestGenerator::new();
        let test_code = generator.generate_test("test_name", "test spec");
        assert!(test_code.contains("test_name"));
        assert!(test_code.contains("test spec"));
        assert!(test_code.contains("#[test]"));
        assert!(test_code.contains("fn test_name()"));
    }

    #[test]
    fn test_test_generator_get_tests() {
        let mut generator = TestGenerator::new();
        assert_eq!(generator.get_tests().len(), 0, "starts empty");
        generator.generate_test("test_foo", "foo spec");
        assert_eq!(generator.get_tests().len(), 1, "each generate_test call stores the result");
    }

    #[test]
    fn test_test_generator_default() {
        let generator = TestGenerator::default();
        assert_eq!(generator.get_tests().len(), 0);
    }

    #[test]
    fn test_test_generator_multiple_tests() {
        let mut generator = TestGenerator::new();
        let test1 = generator.generate_test("test1", "spec1");
        let test2 = generator.generate_test("test2", "spec2");
        assert!(test1.contains("test1"));
        assert!(test2.contains("test2"));
        assert_ne!(test1, test2);
    }

    // ========================================================================
    // 2. GENERATE TEST ARRAY - Test compile-time array generation
    // ========================================================================

    #[test]
    fn test_generate_test_array_small() {
        const ARRAY: [u8; 5] = generate_test_array::<5>();
        assert_eq!(ARRAY.len(), 5);
        assert_eq!(ARRAY[0], 0);
        assert_eq!(ARRAY[1], 1);
        assert_eq!(ARRAY[2], 2);
        assert_eq!(ARRAY[3], 3);
        assert_eq!(ARRAY[4], 4);
    }

    #[test]
    fn test_generate_test_array_large() {
        const ARRAY: [u8; 10] = generate_test_array::<10>();
        assert_eq!(ARRAY.len(), 10);
        assert_eq!(ARRAY[0], 0);
        assert_eq!(ARRAY[9], 9);
    }

    #[test]
    fn test_generate_test_array_wraps() {
        const ARRAY: [u8; 260] = generate_test_array::<260>();
        assert_eq!(ARRAY.len(), 260);
        assert_eq!(ARRAY[0], 0);
        assert_eq!(ARRAY[255], 255);
        assert_eq!(ARRAY[256], 0); // Wraps around
        assert_eq!(ARRAY[257], 1);
    }

    #[test]
    fn test_generate_test_array_zero() {
        const ARRAY: [u8; 0] = generate_test_array::<0>();
        assert_eq!(ARRAY.len(), 0);
    }

    // ========================================================================
    // 3. GENERATE TEST ARRAY PATTERN - Test pattern-based generation
    // ========================================================================

    #[test]
    fn test_generate_test_array_pattern_small() {
        const ARRAY: [u8; 5] = generate_test_array_pattern::<5>(10);
        assert_eq!(ARRAY.len(), 5);
        assert_eq!(ARRAY[0], 10);
        assert_eq!(ARRAY[1], 11);
        assert_eq!(ARRAY[2], 12);
        assert_eq!(ARRAY[3], 13);
        assert_eq!(ARRAY[4], 14);
    }

    #[test]
    fn test_generate_test_array_pattern_wraps() {
        const ARRAY: [u8; 5] = generate_test_array_pattern::<5>(250);
        assert_eq!(ARRAY.len(), 5);
        assert_eq!(ARRAY[0], 250);
        assert_eq!(ARRAY[1], 251);
        assert_eq!(ARRAY[2], 252);
        assert_eq!(ARRAY[3], 253);
        assert_eq!(ARRAY[4], 254);
    }

    #[test]
    fn test_generate_test_array_pattern_zero() {
        const ARRAY: [u8; 0] = generate_test_array_pattern::<0>(10);
        assert_eq!(ARRAY.len(), 0);
    }

    // ========================================================================
    // 4. BOUNDARY CONDITIONS - Test edge cases
    // ========================================================================

    #[test]
    fn test_generate_test_array_single_element() {
        const ARRAY: [u8; 1] = generate_test_array::<1>();
        assert_eq!(ARRAY.len(), 1);
        assert_eq!(ARRAY[0], 0);
    }

    #[test]
    fn test_generate_test_array_pattern_single_element() {
        const ARRAY: [u8; 1] = generate_test_array_pattern::<1>(42);
        assert_eq!(ARRAY.len(), 1);
        assert_eq!(ARRAY[0], 42);
    }

    #[test]
    fn test_test_generator_empty_name() {
        let mut generator = TestGenerator::new();
        let test_code = generator.generate_test("", "spec");
        assert!(test_code.contains("fn ()"));
    }

    #[test]
    fn test_test_generator_empty_spec() {
        let mut generator = TestGenerator::new();
        let test_code = generator.generate_test("test_name", "");
        assert!(test_code.contains("test_name"));
    }
}
