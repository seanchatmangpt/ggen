//! Turtle Configuration Loader
//!
//! This module provides loading and parsing of Turtle (.ttl) configuration files.
//! All marketplace configuration is stored in RDF/Turtle format, not YAML/JSON.

use serde::{Deserialize, Serialize};
use std::fmt::Write;
use std::fs;

use super::ontology::generate_prefixes;

/// Marketplace configuration loaded from Turtle
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MarketplaceConfig {
    /// URL of the primary package registry
    pub registry_url: String,
    /// Directory path for caching downloaded packages
    pub cache_dir: String,
    /// Maximum size in bytes for package downloads (default: 100MB)
    pub max_download_size: u64,
    /// Enable package validation against RDF schema
    pub validation_enabled: bool,
    /// Enable automatic updates for installed packages
    pub auto_update_enabled: bool,
    /// Enable telemetry and usage tracking
    pub telemetry_enabled: bool,
    /// List of configured registry endpoints
    pub registries: Vec<RegistryConfig>,
    /// SPARQL validation rules for packages
    pub validation_rules: Vec<String>,
}

/// Configuration for a package registry endpoint
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegistryConfig {
    /// Human-readable registry name
    pub name: String,
    /// Full URL to the registry API endpoint
    pub url: String,
    /// Priority order for registry fallback (lower = higher priority)
    pub priority: u32,
    /// Whether this registry is currently enabled
    pub enabled: bool,
    /// Whether authentication credentials are required
    pub auth_required: bool,
}

/// Turtle configuration loader for reading RDF/Turtle config files
pub struct TurtleConfigLoader {
    /// Directory containing marketplace configuration files (marketplace.ttl, validation-rules.ttl, etc.)
    config_dir: String,
}

impl TurtleConfigLoader {
    /// Create a new Turtle configuration loader for the specified directory
    pub fn new(config_dir: impl Into<String>) -> Self {
        Self {
            config_dir: config_dir.into(),
        }
    }

    /// Load main marketplace configuration
    ///
    /// # Errors
    ///
    /// * [`ConfigError::FileReadError`] - When the marketplace.ttl file cannot be read
    /// * [`ConfigError::ParseError`] - When the Turtle content cannot be parsed
    /// * [`ConfigError::MissingRequiredField`] - When required fields are missing
    pub fn load_marketplace_config(&self) -> Result<MarketplaceConfig, ConfigError> {
        let config_path = format!("{}/marketplace.ttl", self.config_dir);
        let turtle_content =
            fs::read_to_string(&config_path).map_err(|e| ConfigError::FileReadError {
                path: config_path.clone(),
                error: e.to_string(),
            })?;

        Self::parse_marketplace_config(&turtle_content)
    }

    /// Load validation rules from Turtle
    ///
    /// # Errors
    ///
    /// * [`ConfigError::FileReadError`] - When the validation-rules.ttl file cannot be read
    pub fn load_validation_rules(&self) -> Result<Vec<String>, ConfigError> {
        let rules_path = format!("{}/validation-rules.ttl", self.config_dir);
        let turtle_content =
            fs::read_to_string(&rules_path).map_err(|e| ConfigError::FileReadError {
                path: rules_path.clone(),
                error: e.to_string(),
            })?;

        Ok(vec![turtle_content])
    }

    /// Load state machine definitions
    ///
    /// # Errors
    ///
    /// * [`ConfigError::FileReadError`] - When the state-machines.ttl file cannot be read
    /// * [`ConfigError::ParseError`] - When the Turtle content cannot be parsed
    pub fn load_state_machines(&self) -> Result<Vec<StateMachine>, ConfigError> {
        let sm_path = format!("{}/state-machines.ttl", self.config_dir);
        let turtle_content =
            fs::read_to_string(&sm_path).map_err(|e| ConfigError::FileReadError {
                path: sm_path.clone(),
                error: e.to_string(),
            })?;

        Ok(Self::parse_state_machines(&turtle_content))
    }

    /// Parse marketplace configuration from Turtle
    fn parse_marketplace_config(turtle: &str) -> Result<MarketplaceConfig, ConfigError> {
        // Simplified parsing - in production, use a proper Turtle parser like sophia or oxigraph
        let mut config = MarketplaceConfig {
            registry_url: String::new(),
            cache_dir: String::new(),
            max_download_size: 100 * 1024 * 1024, // 100MB default
            validation_enabled: true,
            auto_update_enabled: false,
            telemetry_enabled: true,
            registries: Vec::new(),
            validation_rules: Vec::new(),
        };

        // Parse key-value pairs from Turtle
        for line in turtle.lines() {
            let line = line.trim();
            if line.starts_with('#') || line.is_empty() {
                continue;
            }

            if line.contains("ggen:registryUrl") {
                if let Some(value) = Self::extract_string_value(line) {
                    config.registry_url = value;
                }
            } else if line.contains("ggen:cacheDir") {
                if let Some(value) = Self::extract_string_value(line) {
                    config.cache_dir = value;
                }
            } else if line.contains("ggen:maxDownloadSize") {
                if let Some(value) = Self::extract_integer_value(line) {
                    // Config values are non-negative; reject negative values
                    #[allow(clippy::cast_sign_loss)]
                    let config_value = value as u64;
                    config.max_download_size = config_value;
                }
            } else if line.contains("ggen:validationEnabled") {
                if let Some(value) = Self::extract_boolean_value(line) {
                    config.validation_enabled = value;
                }
            } else if line.contains("ggen:autoUpdateEnabled") {
                if let Some(value) = Self::extract_boolean_value(line) {
                    config.auto_update_enabled = value;
                }
            }
        }

        if config.registry_url.is_empty() {
            return Err(ConfigError::MissingRequiredField {
                field: "registry_url".to_string(),
            });
        }

        Ok(config)
    }

    /// Parse state machine definitions
    fn parse_state_machines(_turtle: &str) -> Vec<StateMachine> {
        // Stub - would parse state machine triples
        Vec::new()
    }

    fn extract_string_value(line: &str) -> Option<String> {
        if let Some(start) = line.find('"') {
            if let Some(end) = line[start + 1..].find('"') {
                return Some(line[start + 1..start + 1 + end].to_string());
            }
        }
        None
    }

    fn extract_integer_value(line: &str) -> Option<i64> {
        if let Some(value_str) = Self::extract_string_value(line) {
            value_str.parse::<i64>().ok()
        } else {
            None
        }
    }

    fn extract_boolean_value(line: &str) -> Option<bool> {
        if let Some(value_str) = Self::extract_string_value(line) {
            match value_str.as_str() {
                "true" => Some(true),
                "false" => Some(false),
                _ => None,
            }
        } else {
            None
        }
    }

    /// Save configuration back to Turtle
    ///
    /// # Errors
    ///
    /// * [`ConfigError::FileWriteError`] - When the marketplace.ttl file cannot be written
    pub fn save_marketplace_config(&self, config: &MarketplaceConfig) -> Result<(), ConfigError> {
        let config_path = format!("{}/marketplace.ttl", self.config_dir);
        let turtle = Self::generate_marketplace_turtle(config);

        fs::write(&config_path, turtle).map_err(|e| ConfigError::FileWriteError {
            path: config_path,
            error: e.to_string(),
        })
    }

    /// Generate Turtle representation of configuration
    fn generate_marketplace_turtle(config: &MarketplaceConfig) -> String {
        let mut ttl = generate_prefixes();

        writeln!(
            ttl,
            r#"
:marketplace a ggen:MarketplaceConfig ;
    ggen:registryUrl "{registry_url}" ;
    ggen:cacheDir "{cache_dir}" ;
    ggen:maxDownloadSize "{max_download_size}"^^xsd:integer ;
    ggen:validationEnabled "{validation_enabled}"^^xsd:boolean ;
    ggen:autoUpdateEnabled "{auto_update_enabled}"^^xsd:boolean ;
    ggen:telemetryEnabled "{telemetry_enabled}"^^xsd:boolean .

"#,
            registry_url = config.registry_url,
            cache_dir = config.cache_dir,
            max_download_size = config.max_download_size,
            validation_enabled = config.validation_enabled,
            auto_update_enabled = config.auto_update_enabled,
            telemetry_enabled = config.telemetry_enabled,
        )
        .unwrap();

        // Add registries
        for (i, registry) in config.registries.iter().enumerate() {
            writeln!(
                ttl,
                r#"
:registry{index} a ggen:RegistryConfig ;
    ggen:name "{name}" ;
    ggen:url "{url}" ;
    ggen:priority "{priority}"^^xsd:integer ;
    ggen:enabled "{enabled}"^^xsd:boolean ;
    ggen:authRequired "{auth_required}"^^xsd:boolean .

:marketplace ggen:hasRegistry :registry{index} .

"#,
                index = i,
                name = registry.name,
                url = registry.url,
                priority = registry.priority,
                enabled = registry.enabled,
                auth_required = registry.auth_required,
            )
            .unwrap();
        }

        ttl
    }
}

/// State machine definition for package lifecycle loaded from Turtle
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateMachine {
    /// Unique identifier for this state machine
    pub id: String,
    /// Human-readable name of the state machine
    pub name: String,
    /// ID of the initial state
    pub initial_state: String,
    /// All states in the state machine
    pub states: Vec<State>,
    /// All possible transitions between states
    pub transitions: Vec<Transition>,
}

/// A state in a state machine defining package lifecycle stages
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct State {
    /// Unique identifier for this state
    pub id: String,
    /// Human-readable label for the state
    pub label: String,
    /// Whether this state is a terminal/final state
    pub is_final: bool,
}

/// A transition between states triggered by an event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Transition {
    /// ID of the source state
    pub from_state: String,
    /// ID of the destination state
    pub to_state: String,
    /// Name of the event that triggers this transition
    pub event: String,
    /// Conditions that must be met to execute this transition
    pub conditions: Vec<String>,
}

/// Configuration error types
#[derive(Debug, Clone)]
pub enum ConfigError {
    /// Error when reading a configuration file from disk
    FileReadError {
        /// Path to the file that failed to read
        path: String,
        /// Description of the I/O error
        error: String,
    },
    /// Error when writing a configuration file to disk
    FileWriteError {
        /// Path to the file that failed to write
        path: String,
        /// Description of the I/O error
        error: String,
    },
    /// Error when parsing Turtle configuration content
    ParseError {
        /// Line number where parse error occurred
        line: usize,
        /// Description of the parse error
        error: String,
    },
    /// Error when a required configuration field is missing
    MissingRequiredField {
        /// Name of the missing field
        field: String,
    },
    /// Error when a configuration field has an invalid value
    InvalidValue {
        /// Name of the field with invalid value
        field: String,
        /// The invalid value that was provided
        value: String,
    },
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FileReadError { path, error } => {
                write!(f, "Failed to read config file {path}: {error}")
            }
            Self::FileWriteError { path, error } => {
                write!(f, "Failed to write config file {path}: {error}")
            }
            Self::ParseError { line, error } => {
                write!(f, "Parse error at line {line}: {error}")
            }
            Self::MissingRequiredField { field } => {
                write!(f, "Missing required field: {field}")
            }
            Self::InvalidValue { field, value } => {
                write!(f, "Invalid value for {field}: {value}")
            }
        }
    }
}

impl std::error::Error for ConfigError {}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_extract_string_value() {
        let line = r#"ggen:registryUrl "https://registry.ggen.dev" ;"#;
        assert_eq!(
            TurtleConfigLoader::extract_string_value(line),
            Some("https://registry.ggen.dev".to_string())
        );
    }

    #[test]
    fn test_extract_boolean_value() {
        let line = r#"ggen:validationEnabled "true"^^xsd:boolean ;"#;
        assert_eq!(TurtleConfigLoader::extract_boolean_value(line), Some(true));
    }

    #[test]
    fn test_generate_marketplace_turtle() {
        let config = MarketplaceConfig {
            registry_url: "https://registry.ggen.dev".to_string(),
            cache_dir: "/tmp/ggen".to_string(),
            max_download_size: 104857600,
            validation_enabled: true,
            auto_update_enabled: false,
            telemetry_enabled: true,
            registries: vec![],
            validation_rules: vec![],
        };

        let turtle = TurtleConfigLoader::generate_marketplace_turtle(&config);

        assert!(turtle.contains("@prefix ggen:"));
        assert!(turtle.contains("https://registry.ggen.dev"));
        assert!(turtle.contains("ggen:MarketplaceConfig"));
    }
}
