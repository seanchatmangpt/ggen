//! Smoke test for chicago-tdd-tools integration in ggen-domain
//!
//! This test verifies that chicago-tdd-tools is properly configured and works
//! with the ggen-domain crate. It tests basic functionality to ensure the
//! integration is stable.

// Import macros from crate root (they're exported via #[macro_export])
use chicago_tdd_tools::{async_test, test};

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

// Test domain-specific functionality
test!(test_domain_integration, {
    // Arrange
    let domain_value = String::from("ggen-domain");

    // Act
    let result = domain_value.to_uppercase();

    // Assert
    assert_eq!(result, "GGEN-DOMAIN");
});

// Test error handling
test!(test_error_handling, {
    // Arrange
    let result: Result<i32, String> = Err("test error".to_string());

    // Act & Assert
    assert!(result.is_err());
    assert_eq!(result.unwrap_err(), "test error");
});

// Test collections
test!(test_collections, {
    // Arrange
    let mut items = vec!["a", "b", "c"];

    // Act
    items.push("d");

    // Assert
    assert_eq!(items.len(), 4);
    assert_eq!(items[3], "d");
});
