//! CLI utility functions for ggen command-line interface
//!
//! This module provides shared utilities for CLI argument parsing and processing.

use std::collections::BTreeMap;

/// Parse command-line variables in key=value format
///
/// # Arguments
///
/// * `vars` - Slice of variable strings in "key=value" format
///
/// # Returns
///
/// Returns a BTreeMap of parsed key-value pairs, or an error if any variable
/// is not in the correct format.
///
/// # Example
///
/// ```
/// # use ggen_utils::cli::parse_variables;
/// let vars = vec!["name=value".to_string(), "count=42".to_string()];
/// let result = parse_variables(&vars);
/// assert!(result.is_ok());
/// ```
pub fn parse_variables(vars: &[String]) -> std::result::Result<BTreeMap<String, String>, String> {
    let mut map = BTreeMap::new();
    for var in vars {
        if let Some((key, value)) = var.split_once('=') {
            map.insert(key.to_string(), value.to_string());
        } else {
            return Err(format!(
                "Invalid variable format: {}. Expected key=value",
                var
            ));
        }
    }
    Ok(map)
}

#[cfg(test)]
mod tests {
    use super::*;

    #[allow(clippy::expect_used)]
    #[test]
    fn test_parse_variables_valid() {
        let vars = vec!["name=value".to_string(), "count=42".to_string()];
        let result = parse_variables(&vars).expect("should parse valid variables");

        assert_eq!(result.get("name").map(|s| s.as_str()), Some("value"));
        assert_eq!(result.get("count").map(|s| s.as_str()), Some("42"));
    }

    #[allow(clippy::expect_used)]
    #[test]
    fn test_parse_variables_empty() {
        let vars: Vec<String> = vec![];
        let result = parse_variables(&vars).expect("should parse empty list");

        assert!(result.is_empty());
    }

    #[allow(clippy::expect_used)]
    #[test]
    fn test_parse_variables_invalid() {
        let vars = vec!["invalid_format".to_string()];
        let result = parse_variables(&vars);

        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Invalid variable format"));
    }

    #[allow(clippy::expect_used)]
    #[test]
    fn test_parse_variables_multiple_equals() {
        let vars = vec!["path=/usr/bin=/path".to_string()];
        let result = parse_variables(&vars).expect("should handle values with equals");

        // split_once only splits on the first '=', so the value contains the rest
        assert_eq!(
            result.get("path").map(|s| s.as_str()),
            Some("/usr/bin=/path")
        );
    }
}
