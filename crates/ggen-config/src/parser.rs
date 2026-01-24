//! Configuration file parser
//!
//! This module provides functionality for loading and parsing ggen.toml files.

use crate::{ConfigError, GgenConfig, Result};
use ggen_utils::SafePath;
use std::fs;
use std::path::Path;

/// Configuration loader and parser
pub struct ConfigLoader {
    path: SafePath,
}

impl ConfigLoader {
    /// Create a new config loader from a file path
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the ggen.toml file
    ///
    /// # Errors
    ///
    /// Returns an error if the file doesn't exist or path validation fails
    pub fn new<P: AsRef<Path>>(path: P) -> Result<Self> {
        let path = SafePath::new_absolute(path.as_ref())
            .map_err(|e| ConfigError::Validation(format!("Path validation failed: {}", e)))?;

        if !path.exists() {
            return Err(ConfigError::FileNotFound(path.as_path_buf()));
        }
        Ok(Self { path })
    }

    /// Load and parse configuration from a file
    ///
    /// # Arguments
    ///
    /// * `path` - Path to the ggen.toml file
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be read or parsed
    ///
    /// # Example
    ///
    /// ```no_run
    /// use ggen_config::ConfigLoader;
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// let config = ConfigLoader::from_file("ggen.toml")?;
    /// println!("Loaded project: {}", config.project.name);
    /// # Ok(())
    /// # }
    /// ```
    pub fn from_file<P: AsRef<Path>>(path: P) -> Result<GgenConfig> {
        let loader = Self::new(path)?;
        loader.load()
    }

    /// Load and parse configuration from a string
    ///
    /// # Arguments
    ///
    /// * `content` - TOML content as a string
    ///
    /// # Errors
    ///
    /// Returns an error if the TOML cannot be parsed
    ///
    /// # Example
    ///
    /// ```
    /// use ggen_config::ConfigLoader;
    ///
    /// let toml = r#"
    ///     [project]
    ///     name = "my-project"
    ///     version = "1.0.0"
    /// "#;
    ///
    /// let config = ConfigLoader::from_str(toml).unwrap();
    /// assert_eq!(config.project.name, "my-project");
    /// ```
    pub fn from_str(content: &str) -> Result<GgenConfig> {
        let config: GgenConfig = toml::from_str(content)?;
        Ok(config)
    }

    /// Load configuration from the stored file path
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be read or parsed
    pub fn load(&self) -> Result<GgenConfig> {
        let content = fs::read_to_string(&self.path)?;
        Self::from_str(&content)
    }

    /// Find and load ggen.toml from current or parent directories
    ///
    /// Searches upward through the directory tree until finding ggen.toml
    /// or reaching the filesystem root.
    ///
    /// # Errors
    ///
    /// Returns an error if no ggen.toml is found or if it cannot be parsed
    ///
    /// # Example
    ///
    /// ```no_run
    /// use ggen_config::ConfigLoader;
    ///
    /// # fn main() -> Result<(), Box<dyn std::error::Error>> {
    /// // Searches current directory and parents for ggen.toml
    /// let config = ConfigLoader::find_and_load()?;
    /// # Ok(())
    /// # }
    /// ```
    pub fn find_and_load() -> Result<GgenConfig> {
        let path = Self::find_config_file()?;
        Self::from_file(path)
    }

    /// Find ggen.toml by searching current and parent directories
    ///
    /// # Errors
    ///
    /// Returns an error if no configuration file is found or path validation fails
    pub fn find_config_file() -> Result<SafePath> {
        let mut current = SafePath::current_dir()
            .map_err(|e| ConfigError::Validation(format!("Failed to get current directory: {}", e)))?;

        loop {
            let candidate = current
                .join("ggen.toml")
                .map_err(|e| ConfigError::Validation(format!("Path join failed: {}", e)))?;

            if candidate.exists() {
                return Ok(candidate);
            }

            // Try parent directory
            current = current
                .parent()
                .map_err(|_| {
                    ConfigError::FileNotFound(
                        std::path::PathBuf::from("ggen.toml (searched all parent directories)")
                    )
                })?;
        }
    }

    /// Load configuration with environment-specific overrides
    ///
    /// # Arguments
    ///
    /// * `environment` - Environment name (e.g., "development", "production")
    ///
    /// # Errors
    ///
    /// Returns an error if the file cannot be read or parsed
    pub fn load_with_env(&self, environment: &str) -> Result<GgenConfig> {
        let mut config = self.load()?;

        // Apply environment-specific overrides if present
        if let Some(env_overrides) = config.env.clone() {
            if let Some(overrides) = env_overrides.get(environment) {
                apply_env_overrides(&mut config, overrides)?;
            }
        }

        Ok(config)
    }

    /// Get the config file path
    #[must_use]
    pub fn path(&self) -> &Path {
        self.path.as_path()
    }
}

/// Apply environment-specific overrides to configuration
///
/// Uses JSON pointer notation to update nested fields
fn apply_env_overrides(config: &mut GgenConfig, overrides: &serde_json::Value) -> Result<()> {
    if let Some(obj) = overrides.as_object() {
        for (key, value) in obj {
            // Simple key-based override (supports one level of nesting)
            apply_single_override(config, key, value)?;
        }
    }
    Ok(())
}

/// Apply a single configuration override
fn apply_single_override(
    config: &mut GgenConfig, key: &str, value: &serde_json::Value,
) -> Result<()> {
    // Parse dotted key notation (e.g., "ai.temperature")
    let parts: Vec<&str> = key.split('.').collect();

    match parts.as_slice() {
        ["ai", field] => {
            if let Some(ai_config) = config.ai.as_mut() {
                update_ai_field(ai_config, field, value)?;
            }
        }
        ["logging", "level"] => {
            if let Some(logging) = config.logging.as_mut() {
                if let Some(s) = value.as_str() {
                    logging.level = s.to_string();
                }
            }
        }
        ["security", field] => {
            if let Some(security) = config.security.as_mut() {
                update_security_field(security, field, value)?;
            }
        }
        _ => {
            // Unsupported override path - log or ignore
        }
    }

    Ok(())
}

/// Update AI configuration field
fn update_ai_field(
    ai: &mut crate::schema::AiConfig, field: &str, value: &serde_json::Value,
) -> Result<()> {
    match field {
        "model" => {
            if let Some(s) = value.as_str() {
                ai.model = s.to_string();
            }
        }
        "temperature" => {
            if let Some(f) = value.as_f64() {
                ai.temperature = f as f32;
            }
        }
        "max_tokens" => {
            if let Some(n) = value.as_u64() {
                ai.max_tokens = n as u32;
            }
        }
        _ => {}
    }
    Ok(())
}

/// Update security configuration field
fn update_security_field(
    security: &mut crate::schema::SecurityConfig, field: &str, value: &serde_json::Value,
) -> Result<()> {
    match field {
        "require_confirmation" => {
            if let Some(b) = value.as_bool() {
                security.require_confirmation = b;
            }
        }
        "audit_operations" => {
            if let Some(b) = value.as_bool() {
                security.audit_operations = b;
            }
        }
        _ => {}
    }
    Ok(())
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_parse_minimal_config() {
        let toml = r#"
            [project]
            name = "test-project"
            version = "1.0.0"
        "#;

        let config = ConfigLoader::from_str(toml).unwrap();
        assert_eq!(config.project.name, "test-project");
        assert_eq!(config.project.version, "1.0.0");
        assert!(config.ai.is_none());
    }

    #[test]
    fn test_parse_full_config() {
        let toml = r#"
            [project]
            name = "full-project"
            version = "2.0.0"
            description = "A test project"

            [ai]
            provider = "openai"
            model = "gpt-4"
            temperature = 0.8
            max_tokens = 3000

            [templates]
            directory = "templates"
            backup_enabled = true
        "#;

        let config = ConfigLoader::from_str(toml).unwrap();
        assert_eq!(config.project.name, "full-project");

        let ai = config.ai.as_ref().unwrap();
        assert_eq!(ai.provider, "openai");
        assert_eq!(ai.model, "gpt-4");
        assert!((ai.temperature - 0.8).abs() < f32::EPSILON);

        let templates = config.templates.as_ref().unwrap();
        assert_eq!(templates.directory.as_ref().unwrap(), "templates");
        assert!(templates.backup_enabled);
    }

    #[test]
    fn test_default_values() {
        let toml = r#"
            [project]
            name = "defaults"
            version = "1.0.0"

            [ai]
            provider = "ollama"
            model = "llama2"
        "#;

        let config = ConfigLoader::from_str(toml).unwrap();
        let ai = config.ai.as_ref().unwrap();

        // Check default values
        assert!((ai.temperature - 0.7).abs() < f32::EPSILON);
        assert_eq!(ai.max_tokens, 2000);
        assert_eq!(ai.timeout, 30);
    }
}
