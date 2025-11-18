//! Turtle Configuration Loader
//!
//! This module provides loading and parsing of Turtle (.ttl) configuration files.
//! All marketplace configuration is stored in RDF/Turtle format, not YAML/JSON.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::fs;
use std::path::Path;

use super::ontology::{generate_prefixes, namespaces};
use super::poka_yoke::{Literal, PokaYokeError, RdfGraph, ResourceId, Triple};

/// Marketplace configuration loaded from Turtle
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct MarketplaceConfig {
    pub registry_url: String,
    pub cache_dir: String,
    pub max_download_size: u64,
    pub validation_enabled: bool,
    pub auto_update_enabled: bool,
    pub telemetry_enabled: bool,
    pub registries: Vec<RegistryConfig>,
    pub validation_rules: Vec<String>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RegistryConfig {
    pub name: String,
    pub url: String,
    pub priority: u32,
    pub enabled: bool,
    pub auth_required: bool,
}

/// Turtle configuration loader
pub struct TurtleConfigLoader {
    config_dir: String,
}

impl TurtleConfigLoader {
    pub fn new(config_dir: impl Into<String>) -> Self {
        Self {
            config_dir: config_dir.into(),
        }
    }

    /// Load main marketplace configuration
    pub fn load_marketplace_config(&self) -> Result<MarketplaceConfig, ConfigError> {
        let config_path = format!("{}/marketplace.ttl", self.config_dir);
        let turtle_content =
            fs::read_to_string(&config_path).map_err(|e| ConfigError::FileReadError {
                path: config_path.clone(),
                error: e.to_string(),
            })?;

        self.parse_marketplace_config(&turtle_content)
    }

    /// Load validation rules from Turtle
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
    pub fn load_state_machines(&self) -> Result<Vec<StateMachine>, ConfigError> {
        let sm_path = format!("{}/state-machines.ttl", self.config_dir);
        let turtle_content =
            fs::read_to_string(&sm_path).map_err(|e| ConfigError::FileReadError {
                path: sm_path.clone(),
                error: e.to_string(),
            })?;

        self.parse_state_machines(&turtle_content)
    }

    /// Parse marketplace configuration from Turtle
    fn parse_marketplace_config(&self, turtle: &str) -> Result<MarketplaceConfig, ConfigError> {
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
                    config.max_download_size = value as u64;
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
    fn parse_state_machines(&self, _turtle: &str) -> Result<Vec<StateMachine>, ConfigError> {
        // Stub - would parse state machine triples
        Ok(Vec::new())
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
    pub fn save_marketplace_config(&self, config: &MarketplaceConfig) -> Result<(), ConfigError> {
        let config_path = format!("{}/marketplace.ttl", self.config_dir);
        let turtle = self.generate_marketplace_turtle(config);

        fs::write(&config_path, turtle).map_err(|e| ConfigError::FileWriteError {
            path: config_path,
            error: e.to_string(),
        })
    }

    /// Generate Turtle representation of configuration
    fn generate_marketplace_turtle(&self, config: &MarketplaceConfig) -> String {
        let mut ttl = generate_prefixes();

        ttl.push_str(&format!(
            r#"
:marketplace a ggen:MarketplaceConfig ;
    ggen:registryUrl "{}" ;
    ggen:cacheDir "{}" ;
    ggen:maxDownloadSize "{}"^^xsd:integer ;
    ggen:validationEnabled "{}"^^xsd:boolean ;
    ggen:autoUpdateEnabled "{}"^^xsd:boolean ;
    ggen:telemetryEnabled "{}"^^xsd:boolean .

"#,
            config.registry_url,
            config.cache_dir,
            config.max_download_size,
            config.validation_enabled,
            config.auto_update_enabled,
            config.telemetry_enabled,
        ));

        // Add registries
        for (i, registry) in config.registries.iter().enumerate() {
            ttl.push_str(&format!(
                r#"
:registry{} a ggen:RegistryConfig ;
    ggen:name "{}" ;
    ggen:url "{}" ;
    ggen:priority "{}"^^xsd:integer ;
    ggen:enabled "{}"^^xsd:boolean ;
    ggen:authRequired "{}"^^xsd:boolean .

:marketplace ggen:hasRegistry :registry{} .

"#,
                i,
                registry.name,
                registry.url,
                registry.priority,
                registry.enabled,
                registry.auth_required,
                i,
            ));
        }

        ttl
    }
}

/// State machine definition from Turtle
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StateMachine {
    pub id: String,
    pub name: String,
    pub initial_state: String,
    pub states: Vec<State>,
    pub transitions: Vec<Transition>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct State {
    pub id: String,
    pub label: String,
    pub is_final: bool,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Transition {
    pub from_state: String,
    pub to_state: String,
    pub event: String,
    pub conditions: Vec<String>,
}

/// Configuration error types
#[derive(Debug, Clone)]
pub enum ConfigError {
    FileReadError { path: String, error: String },
    FileWriteError { path: String, error: String },
    ParseError { line: usize, error: String },
    MissingRequiredField { field: String },
    InvalidValue { field: String, value: String },
}

impl std::fmt::Display for ConfigError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::FileReadError { path, error } => {
                write!(f, "Failed to read config file {}: {}", path, error)
            }
            Self::FileWriteError { path, error } => {
                write!(f, "Failed to write config file {}: {}", path, error)
            }
            Self::ParseError { line, error } => {
                write!(f, "Parse error at line {}: {}", line, error)
            }
            Self::MissingRequiredField { field } => {
                write!(f, "Missing required field: {}", field)
            }
            Self::InvalidValue { field, value } => {
                write!(f, "Invalid value for {}: {}", field, value)
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

        let loader = TurtleConfigLoader::new("/tmp");
        let turtle = loader.generate_marketplace_turtle(&config);

        assert!(turtle.contains("@prefix ggen:"));
        assert!(turtle.contains("https://registry.ggen.dev"));
        assert!(turtle.contains("ggen:MarketplaceConfig"));
    }
}
