//! Erlang-specific template helper functions
//!
//! This module provides template helper functions for generating Erlang code,
//! including module name formatting, record generation, supervisor child specs,
//! and application resource file generation.
//!
//! ## Features
//!
//! - **Module name formatting**: Convert snake_case to valid Erlang module names
//! - **Record generation**: Generate Erlang record definitions from RDF entities
//! - **Supervisor child specs**: Generate OTP supervisor child specifications
//! - **Application resources**: Generate .app resource files
//!
//! ## Examples
//!
//! ### Module Name Formatting
//!
//! ```rust
//! use ggen_core::templates::helpers::erlang::snake_case_to_module;
//!
//! let module = snake_case_to_module("job_processor");
//! assert_eq!(module, "job_processor");
//!
//! let module = snake_case_to_module("my_worker");
//! assert_eq!(module, "my_worker");
//! ```

use ggen_utils::error::{Error, Result};
use serde_json::Value;
use std::collections::BTreeMap;

/// Convert snake_case string to valid Erlang module name
///
/// Erlang module names must be valid atoms (lowercase, alphanumeric, underscores).
/// This function validates and formats the input string.
///
/// # Arguments
///
/// * `name` - The snake_case name to convert
///
/// # Returns
///
/// A valid Erlang module name string.
///
/// # Errors
///
/// Returns an error if the name is empty or contains invalid characters.
///
/// # Examples
///
/// ```rust
/// use ggen_core::templates::helpers::erlang::snake_case_to_module;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let module = snake_case_to_module("job_processor")?;
/// assert_eq!(module, "job_processor");
///
/// let module = snake_case_to_module("my_gen_server")?;
/// assert_eq!(module, "my_gen_server");
/// # Ok(())
/// # }
/// ```
pub fn snake_case_to_module(name: &str) -> Result<String> {
    if name.is_empty() {
        return Err(Error::new("Module name cannot be empty"));
    }

    // Validate that name contains only lowercase letters, numbers, and underscores
    if !name
        .chars()
        .all(|c| c.is_ascii_lowercase() || c.is_ascii_digit() || c == '_')
    {
        return Err(Error::new(&format!(
            "Invalid Erlang module name '{}': must contain only lowercase letters, digits, and underscores",
            name
        )));
    }

    // Erlang modules cannot start with a digit
    if name.chars().next().map_or(false, |c| c.is_ascii_digit()) {
        return Err(Error::new(&format!(
            "Invalid Erlang module name '{}': cannot start with a digit",
            name
        )));
    }

    Ok(name.to_string())
}

/// Format an Erlang record definition from RDF entity data
///
/// Generates an Erlang record definition with typed fields based on
/// the provided field specifications.
///
/// # Arguments
///
/// * `record_name` - Name of the record (must be valid atom)
/// * `fields` - Map of field names to field types
///
/// # Returns
///
/// A formatted Erlang record definition string.
///
/// # Errors
///
/// Returns an error if the record name or field names are invalid.
///
/// # Examples
///
/// ```rust
/// use ggen_core::templates::helpers::erlang::format_record;
/// use std::collections::BTreeMap;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let mut fields = BTreeMap::new();
/// fields.insert("id".to_string(), "binary()".to_string());
/// fields.insert("status".to_string(), "atom()".to_string());
/// fields.insert("data".to_string(), "map()".to_string());
///
/// let record = format_record("job", &fields)?;
/// assert!(record.contains("-record(job"));
/// assert!(record.contains("id :: binary()"));
/// # Ok(())
/// # }
/// ```
pub fn format_record(record_name: &str, fields: &BTreeMap<String, String>) -> Result<String> {
    // Validate record name
    snake_case_to_module(record_name)?;

    if fields.is_empty() {
        return Err(Error::new("Record must have at least one field"));
    }

    let mut lines = vec![format!("-record({}, {{", record_name)];

    let field_count = fields.len();
    for (idx, (field_name, field_type)) in fields.iter().enumerate() {
        // Validate field name
        snake_case_to_module(field_name)?;

        let separator = if idx == field_count - 1 { "" } else { "," };
        lines.push(format!("    {} :: {}{}", field_name, field_type, separator));
    }

    lines.push("}).".to_string());

    Ok(lines.join("\n"))
}

/// Format a supervisor child specification
///
/// Generates an Erlang supervisor child spec map for use in supervisor
/// init callbacks. Supports different worker types (worker, supervisor).
///
/// # Arguments
///
/// * `id` - Unique child ID (atom)
/// * `module` - Module name implementing the behavior
/// * `args` - Start arguments as JSON array
/// * `child_type` - Type of child ("worker" or "supervisor")
///
/// # Returns
///
/// A formatted supervisor child spec string.
///
/// # Errors
///
/// Returns an error if parameters are invalid.
///
/// # Examples
///
/// ```rust
/// use ggen_core::templates::helpers::erlang::format_supervisor_child;
/// use serde_json::json;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let args = json!(["arg1", "arg2"]);
/// let spec = format_supervisor_child("worker1", "job_worker", &args, "worker")?;
/// assert!(spec.contains("id => worker1"));
/// assert!(spec.contains("start => {job_worker, start_link"));
/// # Ok(())
/// # }
/// ```
pub fn format_supervisor_child(
    id: &str, module: &str, args: &Value, child_type: &str,
) -> Result<String> {
    // Validate inputs
    snake_case_to_module(id)?;
    snake_case_to_module(module)?;

    let valid_types = ["worker", "supervisor"];
    if !valid_types.contains(&child_type) {
        return Err(Error::new(&format!(
            "Invalid child type '{}': must be 'worker' or 'supervisor'",
            child_type
        )));
    }

    // Format arguments
    let args_str = if let Value::Array(arr) = args {
        let formatted: Vec<String> = arr
            .iter()
            .map(|v| match v {
                Value::String(s) => format!("<<\"{}\">>", s),
                Value::Number(n) => n.to_string(),
                Value::Bool(b) => b.to_string(),
                _ => "undefined".to_string(),
            })
            .collect();
        format!("[{}]", formatted.join(", "))
    } else {
        "[]".to_string()
    };

    let spec = format!(
        r#"#{{{indent}id => {id},
{indent}start => {{{module}, start_link, {args}}},
{indent}restart => permanent,
{indent}shutdown => 5000,
{indent}type => {child_type}
{indent}}}"#,
        indent = "    ",
        id = id,
        module = module,
        args = args_str,
        child_type = child_type
    );

    Ok(spec)
}

/// Format an application resource file (.app) content
///
/// Generates the content of an Erlang .app resource file with
/// application metadata and configuration.
///
/// # Arguments
///
/// * `app_name` - Application name (valid atom)
/// * `version` - Version string (semver format)
/// * `description` - Application description
/// * `modules` - List of module names
/// * `applications` - List of dependency applications
///
/// # Returns
///
/// A formatted .app file content string.
///
/// # Errors
///
/// Returns an error if any parameter is invalid.
///
/// # Examples
///
/// ```rust
/// use ggen_core::templates::helpers::erlang::format_app_resource;
///
/// # fn main() -> ggen_utils::error::Result<()> {
/// let modules = vec!["job_app".to_string(), "job_sup".to_string()];
/// let apps = vec!["kernel".to_string(), "stdlib".to_string()];
/// let app = format_app_resource("job_processor", "1.0.0", "Job processing application", &modules, &apps)?;
/// assert!(app.contains("{application, job_processor"));
/// # Ok(())
/// # }
/// ```
pub fn format_app_resource(
    app_name: &str, version: &str, description: &str, modules: &[String], applications: &[String],
) -> Result<String> {
    // Validate app name
    snake_case_to_module(app_name)?;

    // Validate version format (basic semver check)
    if !version.chars().all(|c| c.is_ascii_digit() || c == '.') {
        return Err(Error::new(&format!(
            "Invalid version '{}': must be in semver format (e.g., 1.0.0)",
            version
        )));
    }

    // Validate all module names
    for module in modules {
        snake_case_to_module(module)?;
    }

    // Validate all application names
    for app in applications {
        snake_case_to_module(app)?;
    }

    let modules_str = modules
        .iter()
        .map(|m| m.to_string())
        .collect::<Vec<_>>()
        .join(",\n               ");

    let apps_str = applications
        .iter()
        .map(|a| a.to_string())
        .collect::<Vec<_>>()
        .join(",\n                   ");

    let app_content = format!(
        r#"{{application, {app_name},
 [{{description, "{description}"}},
  {{vsn, "{version}"}},
  {{registered, []}},
  {{applications, [
                   {apps}
                  ]}},
  {{modules, [
               {modules}
              ]}},
  {{mod, {{{app_name}_app, []}}}}
 ]}}.
"#,
        app_name = app_name,
        description = description,
        version = version,
        apps = apps_str,
        modules = modules_str
    );

    Ok(app_content)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_snake_case_to_module_valid() {
        // Arrange
        let name = "job_processor";

        // Act
        let result = snake_case_to_module(name);

        // Assert
        assert!(result.is_ok());
        assert_eq!(result.unwrap(), "job_processor");
    }

    #[test]
    fn test_snake_case_to_module_empty() {
        // Arrange
        let name = "";

        // Act
        let result = snake_case_to_module(name);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("cannot be empty"));
    }

    #[test]
    fn test_snake_case_to_module_uppercase() {
        // Arrange
        let name = "JobProcessor";

        // Act
        let result = snake_case_to_module(name);

        // Assert
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("lowercase letters"));
    }

    #[test]
    fn test_snake_case_to_module_starts_with_digit() {
        // Arrange
        let name = "1_job";

        // Act
        let result = snake_case_to_module(name);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("cannot start"));
    }

    #[test]
    fn test_format_record_valid() {
        // Arrange
        let mut fields = BTreeMap::new();
        fields.insert("id".to_string(), "binary()".to_string());
        fields.insert("status".to_string(), "atom()".to_string());

        // Act
        let result = format_record("job", &fields);

        // Assert
        assert!(result.is_ok());
        let record = result.unwrap();
        assert!(record.contains("-record(job"));
        assert!(record.contains("id :: binary()"));
        assert!(record.contains("status :: atom()"));
    }

    #[test]
    fn test_format_record_empty_fields() {
        // Arrange
        let fields = BTreeMap::new();

        // Act
        let result = format_record("job", &fields);

        // Assert
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("at least one field"));
    }

    #[test]
    fn test_format_supervisor_child_worker() {
        // Arrange
        let args = serde_json::json!(["config1", "config2"]);

        // Act
        let result = format_supervisor_child("worker1", "job_worker", &args, "worker");

        // Assert
        assert!(result.is_ok());
        let spec = result.unwrap();
        assert!(spec.contains("id => worker1"));
        assert!(spec.contains("start => {job_worker, start_link"));
        assert!(spec.contains("type => worker"));
    }

    #[test]
    fn test_format_supervisor_child_invalid_type() {
        // Arrange
        let args = serde_json::json!([]);

        // Act
        let result = format_supervisor_child("worker1", "job_worker", &args, "invalid");

        // Assert
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Invalid child type"));
    }

    #[test]
    fn test_format_app_resource_valid() {
        // Arrange
        let modules = vec!["job_app".to_string(), "job_sup".to_string()];
        let apps = vec!["kernel".to_string(), "stdlib".to_string()];

        // Act
        let result = format_app_resource(
            "job_processor",
            "1.0.0",
            "Job processing application",
            &modules,
            &apps,
        );

        // Assert
        assert!(result.is_ok());
        let app = result.unwrap();
        assert!(app.contains("{application, job_processor"));
        assert!(app.contains("\"1.0.0\""));
        assert!(app.contains("job_app"));
        assert!(app.contains("kernel"));
    }

    #[test]
    fn test_format_app_resource_invalid_version() {
        // Arrange
        let modules = vec!["job_app".to_string()];
        let apps = vec!["kernel".to_string()];

        // Act
        let result = format_app_resource(
            "job_processor",
            "v1.0.0-alpha",
            "Description",
            &modules,
            &apps,
        );

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Invalid version"));
    }
}
