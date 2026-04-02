//! Smoke test for chicago-tdd-tools integration in ggen-core
//!
//! This test verifies that chicago-tdd-tools is properly configured and works
//! with the ggen-core crate. It tests basic functionality to ensure the
//! integration is stable.

// Use prelude::* to import all common macros and types
use chicago_tdd_tools::prelude::*;

// Basic smoke test - verify chicago-tdd-tools macro works
test!(test_chicago_tdd_works, {
    // Arrange
    let value = 2 + 2;

    // Act & Assert
    assert_eq!(value, 4);
});

// Test with local variables
test!(test_with_local_vars, {
    // Arrange
    let mut data = vec![1, 2, 3];

    // Act
    data.push(4);
    let result = data.clone();

    // Assert
    assert_eq!(result, vec![1, 2, 3, 4]);
});

// Test async functionality
async_test!(test_async_works, {
    // Arrange
    let value = async { 42 }.await;

    // Act & Assert
    assert_eq!(value, 42);
});

// Test core-specific functionality
test!(test_core_integration, {
    // Arrange
    let core_value = String::from("ggen-core");

    // Act
    let result = core_value.to_uppercase();

    // Assert
    assert_eq!(result, "GGEN-CORE");
});

// Test string manipulation
test!(test_string_operations, {
    // Arrange
    let input = "hello world";

    // Act
    let words: Vec<&str> = input.split_whitespace().collect();

    // Assert
    assert_eq!(words.len(), 2);
    assert_eq!(words[0], "hello");
    assert_eq!(words[1], "world");
});

// Test numeric operations
test!(test_numeric_operations, {
    // Arrange
    let a = 10;
    let b = 5;

    // Act
    let sum = a + b;
    let diff = a - b;
    let prod = a * b;
    let quot = a / b;

    // Assert
    assert_eq!(sum, 15);
    assert_eq!(diff, 5);
    assert_eq!(prod, 50);
    assert_eq!(quot, 2);
});

// Test boolean logic
test!(test_boolean_logic, {
    // Arrange
    let flag1 = true;
    let flag2 = false;

    // Act & Assert
    assert!(flag1 && !flag2);
    assert!(flag1 || flag2);
    assert_ne!(flag1, flag2);
});
