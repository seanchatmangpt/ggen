//! Erlang-specific validation helpers
//!
//! This module provides validation functions for Erlang code generation,
//! including syntax validation, module structure checks, and naming conventions.

use ggen_utils::error::{Error, Result};

/// Validate Erlang module name
///
/// Ensures the module name follows Erlang atom naming conventions:
/// - Starts with lowercase letter
/// - Contains only lowercase letters, digits, underscores, and @ symbol
/// - Not a reserved keyword
///
/// # Arguments
///
/// * `name` - The module name to validate
///
/// # Returns
///
/// `Ok(())` if the module name is valid.
///
/// # Errors
///
/// Returns an error if the module name is invalid.
///
/// # Examples
///
/// ```rust
/// use ggen_core::validation::erlang::validate_module_name;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// validate_module_name("job_worker")?;
/// validate_module_name("my_gen_server")?;
/// # Ok(())
/// # }
/// ```
pub fn validate_module_name(name: &str) -> Result<()> {
    if name.is_empty() {
        return Err(Error::new("Module name cannot be empty"));
    }

    // Check first character (must be lowercase letter)
    let first_char = name
        .chars()
        .next()
        .ok_or_else(|| Error::new("Module name cannot be empty"))?;

    if !first_char.is_ascii_lowercase() {
        return Err(Error::new(&format!(
            "Module name '{}' must start with a lowercase letter",
            name
        )));
    }

    // Check all characters (lowercase, digits, underscore, @)
    for ch in name.chars() {
        if !ch.is_ascii_lowercase() && !ch.is_ascii_digit() && ch != '_' && ch != '@' {
            return Err(Error::new(&format!(
                "Module name '{}' contains invalid character '{}'. Only lowercase letters, digits, '_', and '@' are allowed",
                name, ch
            )));
        }
    }

    // Check for reserved keywords
    let reserved = [
        "after", "and", "andalso", "band", "begin", "bnot", "bor", "bsl", "bsr", "bxor", "case",
        "catch", "cond", "div", "end", "fun", "if", "let", "not", "of", "or", "orelse", "query",
        "receive", "rem", "try", "when", "xor",
    ];

    if reserved.contains(&name) {
        return Err(Error::new(&format!(
            "Module name '{}' is a reserved Erlang keyword",
            name
        )));
    }

    Ok(())
}

/// Validate Erlang function name
///
/// Ensures the function name follows Erlang naming conventions:
/// - Starts with lowercase letter
/// - Contains only lowercase letters, digits, and underscores
/// - Not a reserved keyword
///
/// # Arguments
///
/// * `name` - The function name to validate
///
/// # Returns
///
/// `Ok(())` if the function name is valid.
///
/// # Errors
///
/// Returns an error if the function name is invalid.
///
/// # Examples
///
/// ```rust
/// use ggen_core::validation::erlang::validate_function_name;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// validate_function_name("init")?;
/// validate_function_name("handle_call")?;
/// validate_function_name("process_job")?;
/// # Ok(())
/// # }
/// ```
pub fn validate_function_name(name: &str) -> Result<()> {
    if name.is_empty() {
        return Err(Error::new("Function name cannot be empty"));
    }

    // Same rules as module names
    validate_module_name(name)
}

/// Validate Erlang record name
///
/// Ensures the record name follows Erlang naming conventions.
/// Record names follow the same rules as module names.
///
/// # Arguments
///
/// * `name` - The record name to validate
///
/// # Returns
///
/// `Ok(())` if the record name is valid.
///
/// # Errors
///
/// Returns an error if the record name is invalid.
///
/// # Examples
///
/// ```rust
/// use ggen_core::validation::erlang::validate_record_name;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// validate_record_name("state")?;
/// validate_record_name("worker_state")?;
/// validate_record_name("job_config")?;
/// # Ok(())
/// # }
/// ```
pub fn validate_record_name(name: &str) -> Result<()> {
    validate_module_name(name)
}

/// Validate Erlang variable name
///
/// Ensures the variable name follows Erlang naming conventions:
/// - Starts with uppercase letter or underscore
/// - Contains only letters, digits, and underscores
///
/// # Arguments
///
/// * `name` - The variable name to validate
///
/// # Returns
///
/// `Ok(())` if the variable name is valid.
///
/// # Errors
///
/// Returns an error if the variable name is invalid.
///
/// # Examples
///
/// ```rust
/// use ggen_core::validation::erlang::validate_variable_name;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// validate_variable_name("State")?;
/// validate_variable_name("JobId")?;
/// validate_variable_name("_Unused")?;
/// # Ok(())
/// # }
/// ```
pub fn validate_variable_name(name: &str) -> Result<()> {
    if name.is_empty() {
        return Err(Error::new("Variable name cannot be empty"));
    }

    // Check first character (must be uppercase letter or underscore)
    let first_char = name
        .chars()
        .next()
        .ok_or_else(|| Error::new("Variable name cannot be empty"))?;

    if !first_char.is_ascii_uppercase() && first_char != '_' {
        return Err(Error::new(&format!(
            "Variable name '{}' must start with an uppercase letter or underscore",
            name
        )));
    }

    // Check all characters (letters, digits, underscore)
    for ch in name.chars() {
        if !ch.is_ascii_alphanumeric() && ch != '_' {
            return Err(Error::new(&format!(
                "Variable name '{}' contains invalid character '{}'. Only letters, digits, and '_' are allowed",
                name, ch
            )));
        }
    }

    Ok(())
}

/// Validate Erlang module structure
///
/// Performs basic validation of an Erlang module file to ensure it has
/// required components: module attribute, exports, and no obvious syntax errors.
///
/// # Arguments
///
/// * `content` - The Erlang module file content
///
/// # Returns
///
/// `Ok(())` if the module structure is valid.
///
/// # Errors
///
/// Returns an error if required components are missing or syntax is invalid.
///
/// # Examples
///
/// ```rust
/// use ggen_core::validation::erlang::validate_module_structure;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let content = r#"
/// -module(job_worker).
/// -export([start_link/0, init/1]).
///
/// start_link() -> gen_server:start_link(?MODULE, [], []).
/// init([]) -> {ok, #state{}}.
/// "#;
///
/// validate_module_structure(content)?;
/// # Ok(())
/// # }
/// ```
pub fn validate_module_structure(content: &str) -> Result<()> {
    if content.trim().is_empty() {
        return Err(Error::new("Module content cannot be empty"));
    }

    // Check for -module() attribute
    if !content.contains("-module(") {
        return Err(Error::new("Module must have -module() attribute"));
    }

    // Check for -export() or -compile(export_all)
    let has_export = content.contains("-export(");
    let has_export_all = content.contains("-compile(export_all)");

    if !has_export && !has_export_all {
        return Err(Error::new(
            "Module must have -export() attribute or -compile(export_all)",
        ));
    }

    // Basic syntax check: count matching parentheses and braces
    let open_paren = content.matches('(').count();
    let close_paren = content.matches(')').count();
    let open_brace = content.matches('{').count();
    let close_brace = content.matches('}').count();
    let open_bracket = content.matches('[').count();
    let close_bracket = content.matches(']').count();

    if open_paren != close_paren {
        return Err(Error::new(&format!(
            "Unmatched parentheses: {} open, {} close",
            open_paren, close_paren
        )));
    }

    if open_brace != close_brace {
        return Err(Error::new(&format!(
            "Unmatched braces: {} open, {} close",
            open_brace, close_brace
        )));
    }

    if open_bracket != close_bracket {
        return Err(Error::new(&format!(
            "Unmatched brackets: {} open, {} close",
            open_bracket, close_bracket
        )));
    }

    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_validate_module_name_valid() {
        // Arrange
        let names = vec!["job_worker", "my_gen_server", "app_supervisor"];

        // Act & Assert
        for name in names {
            assert!(validate_module_name(name).is_ok());
        }
    }

    #[test]
    fn test_validate_module_name_empty() {
        // Arrange
        let name = "";

        // Act
        let result = validate_module_name(name);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("cannot be empty"));
    }

    #[test]
    fn test_validate_module_name_uppercase_start() {
        // Arrange
        let name = "JobWorker";

        // Act
        let result = validate_module_name(name);

        // Assert
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("start with a lowercase"));
    }

    #[test]
    fn test_validate_module_name_reserved_keyword() {
        // Arrange
        let name = "case";

        // Act
        let result = validate_module_name(name);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("reserved"));
    }

    #[test]
    fn test_validate_function_name_valid() {
        // Arrange
        let names = vec!["init", "handle_call", "terminate"];

        // Act & Assert
        for name in names {
            assert!(validate_function_name(name).is_ok());
        }
    }

    #[test]
    fn test_validate_variable_name_valid() {
        // Arrange
        let names = vec!["State", "JobId", "_Unused", "Count"];

        // Act & Assert
        for name in names {
            assert!(validate_variable_name(name).is_ok());
        }
    }

    #[test]
    fn test_validate_variable_name_lowercase_start() {
        // Arrange
        let name = "state";

        // Act
        let result = validate_variable_name(name);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("uppercase"));
    }

    #[test]
    fn test_validate_module_structure_valid() {
        // Arrange
        let content = r#"
-module(job_worker).
-export([start_link/0, init/1]).

start_link() -> gen_server:start_link(?MODULE, [], []).
init([]) -> {ok, #state{}}.
"#;

        // Act
        let result = validate_module_structure(content);

        // Assert
        assert!(result.is_ok());
    }

    #[test]
    fn test_validate_module_structure_missing_module() {
        // Arrange
        let content = r#"
-export([start_link/0]).
start_link() -> ok.
"#;

        // Act
        let result = validate_module_structure(content);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("-module()"));
    }

    #[test]
    fn test_validate_module_structure_missing_export() {
        // Arrange
        let content = r#"
-module(job_worker).
start_link() -> ok.
"#;

        // Act
        let result = validate_module_structure(content);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("-export()"));
    }

    #[test]
    fn test_validate_module_structure_unmatched_parens() {
        // Arrange
        let content = r#"
-module(job_worker).
-export([start_link/0]).
start_link() -> gen_server:start_link(?MODULE, [], [].
"#;

        // Act
        let result = validate_module_structure(content);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Unmatched"));
    }
}
