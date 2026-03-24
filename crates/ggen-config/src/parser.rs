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
            .map_err(|e| ConfigError::Validation(format!("Path validation failed: {e}")))?;

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
        let mut current = SafePath::current_dir().map_err(|e| {
            ConfigError::Validation(format!("Failed to get current directory: {e}"))
        })?;

        loop {
            let candidate = current
                .join("ggen.toml")
                .map_err(|e| ConfigError::Validation(format!("Path join failed: {e}")))?;

            if candidate.exists() {
                return Ok(candidate);
            }

            // Try parent directory
            current = current.parent().map_err(|_e: ggen_utils::error::Error| {
                ConfigError::FileNotFound(std::path::PathBuf::from(
                    "ggen.toml (searched all parent directories)",
                ))
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
                apply_env_overrides(&mut config, overrides);
            }
        }

        Ok(config)
    }

    /// Load configuration with environment-specific overrides from a map
    ///
    /// # Arguments
    ///
    /// * `overrides` - Slice of (environment_name, overrides) tuples
    ///
    /// # Errors
    ///
    /// Returns an error if the config cannot be loaded
    ///
    /// # Example
    ///
    /// ```
    /// use ggen_config::ConfigLoader;
    /// use serde_json::json;
    ///
    /// let toml = r#"
    ///     [project]
    ///     name = "test"
    ///     version = "1.0.0"
    ///
    ///     [ai]
    ///     provider = "anthropic"
    ///     model = "claude-3-opus"
    /// "#;
    ///
    /// let config = ConfigLoader::from_str(toml)
    ///     .unwrap()
    ///     .load_with_env_from_map(&[("zai", json!({"ai.provider": "zai"}))])
    ///     .unwrap();
    ///
    /// assert_eq!(config.ai.unwrap().provider, "zai");
    /// ```
    pub fn load_with_env_from_map(
        self, overrides: &[(&str, serde_json::Value)],
    ) -> Result<GgenConfig> {
        let mut config = self.load()?;

        for (_env_name, env_overrides) in overrides {
            if let Some(obj) = env_overrides.as_object() {
                for (key, value) in obj {
                    apply_single_override(&mut config, key, value);
                }
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
fn apply_env_overrides(config: &mut GgenConfig, overrides: &serde_json::Value) {
    if let Some(obj) = overrides.as_object() {
        for (key, value) in obj {
            // Simple key-based override (supports one level of nesting)
            apply_single_override(config, key, value);
        }
    }
}

/// Apply a single configuration override
fn apply_single_override(config: &mut GgenConfig, key: &str, value: &serde_json::Value) {
    // Parse dotted key notation (e.g., "ai.temperature", "mcp.enabled")
    let parts: Vec<&str> = key.split('.').collect();

    match parts.as_slice() {
        ["ai", field] => {
            if let Some(ai_config) = config.ai.as_mut() {
                update_ai_field(ai_config, field, value);
            }
        }
        ["logging", "level"] => {
            if let Some(logging) = config.logging.as_mut() {
                if let Some(s) = value.as_str() {
                    logging.level = s.to_string();
                }
            }
        }
        ["logging", field] => {
            if let Some(logging) = config.logging.as_mut() {
                update_logging_field(logging, field, value);
            }
        }
        ["security", field] => {
            if let Some(security) = config.security.as_mut() {
                update_security_field(security, field, value);
            }
        }
        ["performance", field] => {
            if let Some(performance) = config.performance.as_mut() {
                update_performance_field(performance, field, value);
            }
        }
        ["mcp", field] => {
            if config.mcp.is_none() {
                config.mcp = Some(crate::schema::McpConfig {
                    name: None,
                    version: None,
                    tool_timeout_ms: default_mcp_tool_timeout(),
                    max_concurrent_requests: default_mcp_max_concurrent(),
                    transport: None,
                    tools: None,
                    zai: None,
                    enabled: default_mcp_enabled(),
                    discovery: None,
                });
            }
            if let Some(mcp) = config.mcp.as_mut() {
                update_mcp_field(mcp, field, value);
            }
        }
        ["a2a", field] => {
            if config.a2a.is_none() {
                config.a2a = Some(crate::schema::A2AConfig {
                    agent_id: None,
                    agent_name: None,
                    agent_type: None,
                    transport: None,
                    messaging: None,
                    orchestration: None,
                    capabilities: None,
                    enabled: default_a2a_enabled(),
                });
            }
            if let Some(a2a) = config.a2a.as_mut() {
                update_a2a_field(a2a, field, value);
            }
        }
        _ => {
            // Unsupported override path - log or ignore
        }
    }
}

/// Update AI configuration field
fn update_ai_field(ai: &mut crate::schema::AiConfig, field: &str, value: &serde_json::Value) {
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
}

/// Update security configuration field
fn update_security_field(
    security: &mut crate::schema::SecurityConfig, field: &str, value: &serde_json::Value,
) {
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
}

/// Update logging configuration field
fn update_logging_field(
    logging: &mut crate::schema::LoggingConfig, field: &str, value: &serde_json::Value,
) {
    match field {
        "format" => {
            if let Some(s) = value.as_str() {
                logging.format = s.to_string();
            }
        }
        "file" => {
            if let Some(s) = value.as_str() {
                logging.file = Some(s.to_string());
            }
        }
        "rotation" => {
            if let Some(s) = value.as_str() {
                logging.rotation = Some(s.to_string());
            }
        }
        _ => {}
    }
}

/// Update performance configuration field
fn update_performance_field(
    performance: &mut crate::schema::PerformanceConfig, field: &str, value: &serde_json::Value,
) {
    match field {
        "max_workers" => {
            if let Some(n) = value.as_u64() {
                performance.max_workers = n as u32;
            }
        }
        "cache_size" => {
            if let Some(s) = value.as_str() {
                performance.cache_size = Some(s.to_string());
            }
        }
        "memory_limit_mb" => {
            if let Some(n) = value.as_u64() {
                performance.memory_limit_mb = Some(n as u32);
            }
        }
        "parallel_execution" => {
            if let Some(b) = value.as_bool() {
                performance.parallel_execution = b;
            }
        }
        _ => {}
    }
}

/// Update MCP configuration field
fn update_mcp_field(mcp: &mut crate::schema::McpConfig, field: &str, value: &serde_json::Value) {
    match field {
        "enabled" => {
            if let Some(b) = value.as_bool() {
                mcp.enabled = b;
            }
        }
        "name" => {
            if let Some(s) = value.as_str() {
                mcp.name = Some(s.to_string());
            }
        }
        "version" => {
            if let Some(s) = value.as_str() {
                mcp.version = Some(s.to_string());
            }
        }
        "tool_timeout_ms" => {
            if let Some(n) = value.as_u64() {
                mcp.tool_timeout_ms = n;
            }
        }
        "max_concurrent_requests" => {
            if let Some(n) = value.as_u64() {
                mcp.max_concurrent_requests = n as usize;
            }
        }
        _ => {}
    }
}

/// Update A2A configuration field
fn update_a2a_field(a2a: &mut crate::schema::A2AConfig, field: &str, value: &serde_json::Value) {
    match field {
        "enabled" => {
            if let Some(b) = value.as_bool() {
                a2a.enabled = b;
            }
        }
        "agent_id" => {
            if let Some(s) = value.as_str() {
                a2a.agent_id = Some(s.to_string());
            }
        }
        "agent_name" => {
            if let Some(s) = value.as_str() {
                a2a.agent_name = Some(s.to_string());
            }
        }
        "agent_type" => {
            if let Some(s) = value.as_str() {
                a2a.agent_type = Some(s.to_string());
            }
        }
        _ => {}
    }
}

// Default value functions (re-export from schema for parser use)
const fn default_mcp_tool_timeout() -> u64 {
    30000
}

const fn default_mcp_max_concurrent() -> usize {
    100
}

fn default_mcp_enabled() -> bool {
    false
}

fn default_a2a_enabled() -> bool {
    false
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
