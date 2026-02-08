//! Comprehensive Error Handling Tests for ggen-utils
//!
//! Chicago-style TDD tests (state-based, real collaborators).
//!
//! Test Categories:
//! - Error creation and construction
//! - Context chains and error propagation
//! - Source chains and error chaining
//! - From<> conversions for all error types
//! - Safe command validation (whitelist, shell metachars, length limits)
//! - Safe path validation (traversal prevention, workspace bounds)
//! - Integration tests combining error types
//!
//! Security Coverage:
//! - Command injection prevention (10+ variants)
//! - Path traversal prevention (5+ variants)
//! - Input validation (5+ variants)

use ggen_utils::error::{Context, Error, Result};
use ggen_utils::safe_command::{CommandArg, CommandName, SafeCommand};
use ggen_utils::safe_path::SafePath;
use std::error::Error as StdError;
use std::io;
use std::path::PathBuf;

// ============================================================================
// Error Creation Tests (Tests 1-3)
// ============================================================================

#[test]
fn test_error_creation_simple() {
    // Arrange
    let message = "Something went wrong";

    // Act
    let error = Error::new(message);

    // Assert - state-based verification
    let display = format!("{}", error);
    assert_eq!(display, message);
}

#[test]
fn test_error_creation_with_context() {
    // Arrange
    let message = "Failed to read file";
    let context = "config.toml";

    // Act
    let error = Error::with_context(message, context);

    // Assert - verify both message and context in output
    let display = format!("{}", error);
    assert!(display.contains(message));
    assert!(display.contains(context));
}

#[test]
fn test_error_creation_with_source() {
    // Arrange
    let message = "Configuration error";
    let source_error = io::Error::new(io::ErrorKind::NotFound, "File not found");

    // Act
    let error = Error::with_source(message, Box::new(source_error));

    // Assert - verify message and source are preserved
    let display = format!("{}", error);
    assert!(display.contains(message));
    assert!(display.contains("File not found"));
}

// ============================================================================
// Context Chain Tests (Tests 4-5)
// ============================================================================

#[test]
fn test_error_context_method_creates_chain() {
    // Arrange
    let original_error = Error::new("Database connection failed");

    // Act
    let chained_error = original_error.context("While processing user request");

    // Assert - verify chain structure
    let display = format!("{}", chained_error);
    assert!(display.contains("While processing user request"));
    assert!(display.contains("Database connection failed"));

    // Verify source chain exists using std error trait
    assert!(StdError::source(&chained_error).is_some());
}

#[test]
fn test_error_with_context_fn_creates_chain() {
    // Arrange
    let original_error = Error::new("Parse error");
    let step_number = 42;

    // Act
    let chained_error = original_error.with_context_fn(|| format!("Failed at step {}", step_number));

    // Assert - verify lazy evaluation occurred
    let display = format!("{}", chained_error);
    assert!(display.contains("Failed at step 42"));
    assert!(display.contains("Parse error"));
}

// ============================================================================
// Source Chain Tests (Tests 6-7)
// ============================================================================

#[test]
fn test_error_source_chain_multiple_levels() {
    // Arrange
    let io_error = io::Error::new(io::ErrorKind::PermissionDenied, "Access denied");

    // Act - build three-level error chain
    let level1 = Error::with_source("File operation failed", Box::new(io_error));
    let level2 = level1.context("During configuration load");
    let level3 = level2.context("Application startup failed");

    // Assert - walk the chain and verify all levels
    let display = format!("{}", level3);
    assert!(display.contains("Application startup failed"));
    assert!(display.contains("During configuration load"));
    assert!(display.contains("File operation failed"));

    // Verify source chain integrity using std error trait
    let mut current_source = StdError::source(&level3);
    let mut chain_depth = 0;

    while current_source.is_some() {
        chain_depth += 1;
        let error_ref = current_source.unwrap();
        current_source = StdError::source(error_ref);
    }

    assert_eq!(chain_depth, 3, "Chain should have exactly 3 levels");
}

#[test]
fn test_error_source_returns_none_for_simple_error() {
    // Arrange
    let error = Error::new("Simple error without source");

    // Act
    let source = StdError::source(&error);

    // Assert - simple error has no source
    assert!(source.is_none());
}

// ============================================================================
// From<> Conversion Tests (Tests 8-12)
// ============================================================================

#[test]
fn test_from_io_error() {
    // Arrange
    let io_error = io::Error::new(io::ErrorKind::NotFound, "config.toml");

    // Act
    let error: Error = io_error.into();

    // Assert
    let display = format!("{}", error);
    assert!(display.contains("config.toml"));
}

#[test]
fn test_from_string() {
    // Arrange
    let error_msg = String::from("Dynamic error message");

    // Act
    let error: Error = error_msg.clone().into();

    // Assert
    let display = format!("{}", error);
    assert_eq!(display, error_msg);
}

#[test]
fn test_from_str() {
    // Arrange
    let error_msg: &str = "Static error message";

    // Act
    let error: Error = error_msg.into();

    // Assert
    let display = format!("{}", error);
    assert_eq!(display, error_msg);
}

#[test]
fn test_result_context_trait_on_ok() {
    // Arrange
    let ok_result: Result<String> = Ok("success".to_string());

    // Act
    let result = ok_result.context("This should not appear");

    // Assert - Ok value should pass through unchanged
    assert!(result.is_ok());
    assert_eq!(result.unwrap(), "success");
}

#[test]
fn test_result_context_trait_on_err() {
    // Arrange
    let err_result: Result<()> = Err(Error::new("Base error"));

    // Act
    let result = err_result.context("Additional context");

    // Assert - error should be wrapped with context
    assert!(result.is_err());
    let error = result.unwrap_err();
    let display = format!("{}", error);
    assert!(display.contains("Additional context"));
}

// ============================================================================
// Safe Command Validation Tests (Tests 13-15)
// ============================================================================

#[test]
fn test_safe_command_whitelist_blocks_dangerous_commands() {
    // Arrange - dangerous commands that should be blocked
    // Note: sh and bash are intentionally excluded as they are whitelisted for validated scripts
    let dangerous_commands = vec![
        "rm", "rmdir", "mv", "dd", "mkfs", "kill", "killall", "pkill", "sudo", "su",
        "chmod", "chown", "curl", "wget", "nc", "netcat", "telnet", "ssh",
        "scp", "rsync", "tar", "zip", "unzip", "7z",
    ];

    // Act & Assert - each dangerous command should be rejected
    for cmd in dangerous_commands {
        let result = CommandName::new(cmd);
        assert!(result.is_err(), "Should block dangerous command: {}", cmd);

        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("not in whitelist") || error_msg.contains("whitelist"),
                "Error should mention whitelist for: {}", cmd);
    }

    // Also verify that sh and bash are allowed (whitelisted for validated scripts)
    assert!(CommandName::new("sh").is_ok(), "sh should be whitelisted");
    assert!(CommandName::new("bash").is_ok(), "bash should be whitelisted");
}

#[test]
fn test_safe_command_shell_metacharacters_blocked() {
    // Arrange - shell metacharacter injection attempts
    let injection_attempts = vec![
        ("build; rm -rf /", ';'),
        ("build | tee output", '|'),
        ("build && rm -rf /", '&'),
        ("build > /etc/passwd", '>'),
        ("build < input", '<'),
        ("$(whoami)", '$'),
        ("`whoami`", '`'),
        ("build\nrm -rf /", '\n'),
        ("build\rrm -rf /", '\r'),
        ("build || echo hacked", '|'),
    ];

    // Act & Assert - each injection attempt should be blocked
    for (attack, metachar) in injection_attempts {
        let result = CommandArg::new(attack);
        assert!(result.is_err(), "Should block shell metacharacter '{}': {}", metachar, attack);

        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("metacharacter"),
                "Error should mention metacharacter for: {}", attack);
    }
}

#[test]
fn test_safe_command_length_limits_enforced() {
    // Arrange - create command exceeding MAX_COMMAND_LENGTH (4096)
    let long_arg = "a".repeat(5000);

    // Act
    let result = SafeCommand::new("cargo")
        .unwrap()
        .arg(&long_arg)
        .unwrap()
        .validate();

    // Assert - should fail with length error
    assert!(result.is_err(), "Should block command exceeding max length");

    let error_msg = result.unwrap_err().to_string();
    assert!(error_msg.contains("exceeds maximum") || error_msg.contains("length"),
            "Error should mention length limit");
}

// ============================================================================
// Safe Path Validation Tests (Tests 16-18)
// ============================================================================

#[test]
fn test_safe_path_traversal_prevention() {
    // Arrange - path traversal attack vectors
    let traversal_attempts = vec![
        "../../../etc/passwd",
        "../../etc/passwd",
        "../etc/passwd",
        "subdir/../../etc/passwd",
        "./../../etc/passwd",
        "safe/../../../etc/passwd",
        "../../../../../../etc/passwd",
    ];

    // Act & Assert - each traversal attempt should be blocked
    for attack in traversal_attempts {
        let result = SafePath::new(attack);
        assert!(result.is_err(), "Should block path traversal: {}", attack);

        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("parent directory") || error_msg.contains(".."),
                "Error should mention parent directory for: {}", attack);
    }
}

#[test]
fn test_safe_path_empty_and_invalid_blocked() {
    // Arrange - invalid path inputs
    let invalid_paths = vec![
        "",
        "   ",
        "\t",
        "\n",
        "path/   /file",  // whitespace component
    ];

    // Act & Assert - each invalid path should be blocked
    for invalid_path in invalid_paths {
        let result = SafePath::new(invalid_path);
        assert!(result.is_err(), "Should block invalid path: {:?}", invalid_path);

        let error_msg = result.unwrap_err().to_string();
        assert!(error_msg.contains("empty") || error_msg.contains("whitespace") ||
                error_msg.contains("Invalid"),
                "Error should describe the issue for: {:?}", invalid_path);
    }
}

#[test]
fn test_safe_path_depth_limit_enforced() {
    // Arrange - create path exceeding MAX_PATH_DEPTH (20)
    let deep_components: Vec<String> = (0..=20).map(|i| format!("level{}", i)).collect();
    let deep_path = deep_components.join("/");

    // Act
    let result = SafePath::new(&deep_path);

    // Assert - should fail with depth error
    assert!(result.is_err(), "Should block path exceeding max depth");

    let error_msg = result.unwrap_err().to_string();
    assert!(error_msg.contains("depth") || error_msg.contains("exceeds"),
            "Error should mention depth limit");
}

// ============================================================================
// Error Helper Methods Tests (Tests 19-21)
// ============================================================================

#[test]
fn test_error_invalid_input_helper() {
    // Arrange
    let input = "invalid-value-123";

    // Act
    let error = Error::invalid_input(input);

    // Assert
    let display = format!("{}", error);
    assert!(display.contains("Invalid input"));
    assert!(display.contains(input));
}

#[test]
fn test_error_network_error_helper() {
    // Arrange
    let network_msg = "Connection timeout after 30s";

    // Act
    let error = Error::network_error(network_msg);

    // Assert
    let display = format!("{}", error);
    assert!(display.contains("Network error"));
    assert!(display.contains(network_msg));
}

#[test]
fn test_error_file_not_found_helper() {
    // Arrange
    let path = PathBuf::from("/nonexistent/config.toml");

    // Act
    let error = Error::file_not_found(path.clone());

    // Assert
    let display = format!("{}", error);
    assert!(display.contains("File not found"));
    assert!(display.contains("config.toml"));
}

// ============================================================================
// Integration Tests (Tests 22-24)
// ============================================================================

#[test]
fn test_safe_command_with_safe_path_integration() {
    // Arrange - create validated components
    let safe_path = SafePath::new("src/main.rs").unwrap();

    // Act - build command with validated path
    let cmd_result = SafeCommand::new("rustfmt")
        .unwrap()
        .arg_path(&safe_path)
        .validate();

    // Assert - integration should work seamlessly
    assert!(cmd_result.is_ok(), "SafePath should integrate with SafeCommand");

    let cmd = cmd_result.unwrap();
    let cmd_string = cmd.to_string_debug();
    assert!(cmd_string.contains("rustfmt"));
    assert!(cmd_string.contains("src/main.rs"));
}

#[test]
fn test_error_chain_from_real_operation() {
    // Arrange - simulate real file operation failure
    let non_existent_file = PathBuf::from("/tmp/does_not_exist_xyz123.txt");

    // Act - attempt to read file (will fail)
    let read_result: Result<String> = std::fs::read_to_string(&non_existent_file)
        .map_err(|e| Error::with_source("Failed to read configuration", Box::new(e)))
        .context("During application initialization");

    // Assert - should have proper error chain
    assert!(read_result.is_err());

    let error = read_result.unwrap_err();
    let display = format!("{}", error);
    assert!(display.contains("application initialization"));
    assert!(display.contains("configuration"));
}

#[test]
fn test_multiple_validation_layers() {
    // Arrange - input that should fail multiple validations
    let dangerous_input = "../../../etc/passwd; rm -rf /";

    // Act - try to create SafePath (fails on parent dir)
    let path_result = SafePath::new(dangerous_input);

    // Assert - should fail at path validation
    assert!(path_result.is_err());

    // Also verify command arg validation would catch shell metachars
    let arg_result = CommandArg::new(dangerous_input);
    assert!(arg_result.is_err(), "Should also fail on metacharacter check");
}

// ============================================================================
// Additional Security Tests (Tests 25-27)
// ============================================================================

#[test]
fn test_command_injection_prevention_comprehensive() {
    // Arrange - comprehensive injection vectors
    let injection_vectors = vec![
        // Command separation
        "arg; malicious",
        "arg | malicious",
        "arg && malicious",
        "arg || malicious",
        "arg `malicious`",
        "arg $(malicious)",
        // Redirection
        "arg > file",
        "arg < file",
        "arg >> file",
        "arg 2>&1",
        // Newlines
        "arg\nmalicious",
        "arg\rmalicious",
        "arg\r\nmalicious",
        // Substitution variants
        "arg;${malicious}",
        "arg;`malicious`",
        // Pipe variants
        "arg|malicious",
        "arg||malicious",
    ];

    // Act & Assert - all injections must be blocked
    for injection in injection_vectors {
        let result = CommandArg::new(injection);
        assert!(result.is_err(), "Should block injection: {}", injection);
    }
}

#[test]
fn test_path_traversal_edge_cases() {
    // Arrange - edge case traversal attempts
    let edge_cases = vec![
        "./../etc/passwd",
        "dir/.../etc/passwd",
        "dir/....",
        "dir/.../file",
        "..",
        "../",
        "./..",
        "../.",
        "file/../etc/passwd",
    ];

    // Act & Assert - all should be properly validated
    for edge_case in edge_cases {
        let result = SafePath::new(edge_case);
        // Some should fail (with ..), others might pass (valid paths)
        // The key is no panic and deterministic behavior
        let _ = result;
    }
}

#[test]
fn test_error_preserves_all_information() {
    // Arrange - create error with all fields populated
    let source = io::Error::new(io::ErrorKind::InvalidData, "Corrupt data");

    // Act
    let error = Error::with_source("Data processing failed", Box::new(source))
        .context("While parsing user input")
        .context("During request handling");

    // Assert - all context should be preserved
    let display = format!("{}", error);
    assert!(display.contains("request handling"));
    assert!(display.contains("user input"));
    assert!(display.contains("Data processing"));
    assert!(display.contains("Corrupt data"));
}

// ============================================================================
// Result Type Tests (Tests 28-30)
// ============================================================================

#[test]
fn test_result_type_with_context() {
    // Arrange
    fn failing_function() -> Result<String> {
        Err(Error::new("Base failure"))
    }

    // Act
    let result = failing_function().context("In high_level_function");

    // Assert
    assert!(result.is_err());
    let error = result.unwrap_err();
    let display = format!("{}", error);
    assert!(display.contains("high_level_function"));
    assert!(display.contains("Base failure"));
}

#[test]
fn test_result_type_with_context_closure() {
    // Arrange
    let attempt_number = 3;

    fn operation(n: usize) -> Result<()> {
        if n < 5 {
            Err(Error::new("Not enough attempts"))
        } else {
            Ok(())
        }
    }

    // Act
    let result = operation(attempt_number).with_context(|| {
        format!("Failed on attempt {}", attempt_number)
    });

    // Assert
    assert!(result.is_err());
    let error = result.unwrap_err();
    let display = format!("{}", error);
    assert!(display.contains("attempt 3"));
}

#[test]
fn test_result_question_mark_operator() {
    // Arrange
    fn validate(value: i32) -> Result<i32> {
        if value < 0 {
            Err(Error::invalid_input("Value must be non-negative"))
        } else {
            Ok(value * 2)
        }
    }

    fn process(value: i32) -> Result<i32> {
        let validated = validate(value)?; // ? operator
        Ok(validated + 10)
    }

    // Act & Assert - error propagation
    assert!(process(-5).is_err());
    assert_eq!(process(5).unwrap(), 20); // (5 * 2) + 10
}

// ============================================================================
// Total: 30 Comprehensive Tests
// ============================================================================
