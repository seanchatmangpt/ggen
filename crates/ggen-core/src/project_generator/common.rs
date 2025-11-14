//! Common utilities for project generators
//!
//! This module provides shared utilities used across different project generators,
//! including validation functions, configuration file generators, and helper functions.
//!
//! ## Features
//!
//! - **Project name validation**: Ensures project names follow naming conventions
//! - **Directory utilities**: Check if directories are empty
//! - **Configuration file generation**: Generate common config files (.editorconfig, .prettierrc, .eslintrc)
//!
//! ## Examples
//!
//! ### Validating a Project Name
//!
//! ```rust,no_run
//! use ggen_core::project_generator::common::validate_project_name;
//!
//! # fn main() -> ggen_utils::error::Result<()> {
//! // Valid names
//! validate_project_name("my-project")?;
//! validate_project_name("my_project")?;
//! validate_project_name("myproject123")?;
//!
//! // Invalid names
//! assert!(validate_project_name("my project").is_err()); // Contains space
//! assert!(validate_project_name("-myproject").is_err()); // Starts with dash
//! # Ok(())
//! # }
//! ```
//!
//! ### Generating Configuration Files
//!
//! ```rust,no_run
//! use ggen_core::project_generator::common::{generate_editorconfig, generate_prettierrc};
//!
//! let editorconfig = generate_editorconfig();
//! let prettierrc = generate_prettierrc();
//! ```

use ggen_utils::error::{Error, Result};
use std::path::Path;

/// Validates project name
pub fn validate_project_name(name: &str) -> Result<()> {
    if name.is_empty() {
        return Err(Error::new("Project name cannot be empty"));
    }

    if name.contains(char::is_whitespace) {
        return Err(Error::new("Project name cannot contain whitespace"));
    }

    if name.starts_with('-') || name.starts_with('_') {
        return Err(Error::new("Project name cannot start with '-' or '_'"));
    }

    // Check for valid characters (alphanumeric, dash, underscore)
    if !name
        .chars()
        .all(|c| c.is_alphanumeric() || c == '-' || c == '_')
    {
        return Err(Error::new(
            "Project name can only contain alphanumeric characters, dashes, and underscores",
        ));
    }

    Ok(())
}

/// Checks if a directory is empty
pub fn is_directory_empty(path: &Path) -> Result<bool> {
    if !path.exists() {
        return Ok(true);
    }

    let entries = std::fs::read_dir(path)
        .map_err(|e| Error::with_source("Failed to read directory", Box::new(e)))?;

    Ok(entries.count() == 0)
}

/// Creates a basic .editorconfig file
pub fn generate_editorconfig() -> String {
    r#"root = true

[*]
charset = utf-8
end_of_line = lf
insert_final_newline = true
trim_trailing_whitespace = true

[*.{js,jsx,ts,tsx,vue}]
indent_style = space
indent_size = 2

[*.{rs,toml}]
indent_style = space
indent_size = 4

[*.md]
trim_trailing_whitespace = false
"#
    .to_string()
}

/// Creates a basic .prettierrc file
pub fn generate_prettierrc() -> String {
    r#"{
  "semi": true,
  "trailingComma": "es5",
  "singleQuote": true,
  "printWidth": 100,
  "tabWidth": 2
}
"#
    .to_string()
}

/// Creates a basic .eslintrc file
pub fn generate_eslintrc() -> String {
    r#"{
  "extends": ["next/core-web-vitals"],
  "rules": {
    "no-console": "warn",
    "no-unused-vars": "warn"
  }
}
"#
    .to_string()
}

#[cfg(test)]
mod tests {
    use super::*;
    use chicago_tdd_tools::{async_test, test};

    test!(test_validate_project_name, {
        assert!(validate_project_name("my-project").is_ok());
        assert!(validate_project_name("my_project").is_ok());
        assert!(validate_project_name("myproject123").is_ok());

        assert!(validate_project_name("").is_err());
        assert!(validate_project_name("my project").is_err());
        assert!(validate_project_name("-myproject").is_err());
        assert!(validate_project_name("my@project").is_err());
    });

    test!(test_generate_editorconfig, {
        let config = generate_editorconfig();
        assert!(config.contains("root = true"));
        assert!(config.contains("charset = utf-8"));
    });
}
