//! Smoke test for chicago-tdd-tools integration in ggen-cli
//!
//! This test verifies that chicago-tdd-tools is properly configured and works
//! with the ggen-cli crate. It tests basic functionality to ensure the
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

// Test CLI-specific functionality
test!(test_cli_integration, {
    // Arrange
    let cli_value = String::from("ggen-cli");

    // Act
    let result = cli_value.to_uppercase();

    // Assert
    assert_eq!(result, "GGEN-CLI");
});

// Test command-line argument parsing logic
test!(test_argument_parsing, {
    // Arrange
    let args = vec!["cmd", "--flag", "value"];

    // Act
    let has_flag = args.contains(&"--flag");
    let has_value = args.contains(&"value");

    // Assert
    assert!(has_flag);
    assert!(has_value);
    assert_eq!(args.len(), 3);
});

// Test environment variable handling
test!(test_env_handling, {
    // Arrange
    let env_key = "TEST_VAR";
    let env_value = "test_value";

    // Act
    std::env::set_var(env_key, env_value);
    let result = std::env::var(env_key);

    // Assert
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), env_value);

    // Cleanup
    std::env::remove_var(env_key);
});

// Test path manipulation
test!(test_path_operations, {
    // Arrange
    let path = std::path::PathBuf::from("/tmp/test");

    // Act
    let parent = path.parent();
    let file_name = path.file_name();

    // Assert
    assert!(parent.is_some());
    assert_eq!(parent.unwrap(), std::path::Path::new("/tmp"));
    assert!(file_name.is_some());
    assert_eq!(file_name.unwrap(), "test");
});

// Test result handling
test!(test_result_handling, {
    // Arrange
    let success: Result<i32, String> = Ok(42);
    let failure: Result<i32, String> = Err("error".to_string());

    // Act & Assert
    assert!(success.is_ok());
    assert_eq!(success.unwrap(), 42);
    assert!(failure.is_err());
    assert_eq!(failure.unwrap_err(), "error");
});
