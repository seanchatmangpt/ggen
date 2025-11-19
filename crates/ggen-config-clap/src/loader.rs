//! Configuration loader for clap integration
//!
//! Provides trait for loading ggen.toml into clap applications

use crate::error::{ConfigClapError, Result};
use ggen_config::GgenConfig;
use std::path::Path;

/// Trait for loading ggen.toml configuration into clap types
pub trait LoadConfigFromGgenToml: Sized {
    /// Load configuration from ggen.toml
    ///
    /// # Errors
    /// Returns error if file cannot be read or parsed
    fn from_ggen_toml<P: AsRef<Path>>(path: P) -> Result<Self>;

    /// Merge with CLI arguments (CLI takes precedence)
    fn merge_with_cli(self, cli_args: Self) -> Self;
}

/// Load ggen.toml configuration
///
/// # Errors
/// Returns error if file cannot be read or parsed
pub fn load_ggen_config<P: AsRef<Path>>(path: P) -> Result<GgenConfig> {
    let config_str = std::fs::read_to_string(path.as_ref()).map_err(|e| {
        ConfigClapError::LoadError(format!("Failed to read ggen.toml: {e}"))
    })?;

    let config: GgenConfig = toml::from_str(&config_str).map_err(|e| {
        ConfigClapError::ParseError(format!("Failed to parse ggen.toml: {e}"))
    })?;

    Ok(config)
}

/// Expand environment variables in a string
///
/// Replaces ${VAR} and $VAR patterns with environment variable values
#[must_use]
pub fn expand_env_vars(input: &str) -> String {
    let mut result = input.to_string();

    // Replace ${VAR} patterns
    while let Some(start) = result.find("${") {
        if let Some(end) = result[start..].find('}') {
            let var_name = &result[start + 2..start + end];
            if let Ok(value) = std::env::var(var_name) {
                result.replace_range(start..start + end + 1, &value);
            }
        } else {
            break;
        }
    }

    // Replace $VAR patterns (simple word characters only)
    let mut chars: Vec<char> = result.chars().collect();
    let mut i = 0;
    while i < chars.len() {
        if chars[i] == '$' && i + 1 < chars.len() && chars[i + 1].is_alphabetic() {
            let start = i;
            i += 1;
            while i < chars.len() && (chars[i].is_alphanumeric() || chars[i] == '_') {
                i += 1;
            }
            let var_name: String = chars[start + 1..i].iter().collect();
            if let Ok(value) = std::env::var(&var_name) {
                result.replace_range(
                    start..start + var_name.len() + 1,
                    &value,
                );
                chars = result.chars().collect();
                i = start + value.len();
            }
        } else {
            i += 1;
        }
    }

    result
}

#[cfg(test)]
mod tests {
    use super::*;
    use std::env;

    #[test]
    fn test_expand_env_vars_dollar_brace() {
        env::set_var("TEST_VAR", "test_value");
        let input = "Path: ${TEST_VAR}/output";
        let result = expand_env_vars(input);
        assert_eq!(result, "Path: test_value/output");
        env::remove_var("TEST_VAR");
    }

    #[test]
    fn test_expand_env_vars_dollar() {
        env::set_var("USER", "testuser");
        let input = "Home: /home/$USER";
        let result = expand_env_vars(input);
        assert_eq!(result, "Home: /home/testuser");
    }

    #[test]
    fn test_expand_env_vars_missing() {
        let input = "Path: ${NONEXISTENT_VAR}/output";
        let result = expand_env_vars(input);
        // Should leave unexpanded if variable doesn't exist
        assert!(result.contains("${NONEXISTENT_VAR}") || result == input);
    }

    #[test]
    fn test_load_missing_config() {
        let result = load_ggen_config("/nonexistent/ggen.toml");
        assert!(result.is_err());
        assert!(matches!(result, Err(ConfigClapError::LoadError(_))));
    }
}
