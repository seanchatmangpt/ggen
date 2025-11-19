//! Configuration validation
//!
//! This module provides validation logic for ggen configuration.

use crate::{ConfigError, GgenConfig, Result};
use std::collections::HashSet;

/// Configuration validator
pub struct ConfigValidator<'a> {
    config: &'a GgenConfig,
    errors: Vec<String>,
}

impl<'a> ConfigValidator<'a> {
    /// Create a new validator for a configuration
    #[must_use]
    pub const fn new(config: &'a GgenConfig) -> Self {
        Self {
            config,
            errors: Vec::new(),
        }
    }

    /// Validate the configuration
    ///
    /// # Errors
    ///
    /// Returns an error if validation fails with details of all issues found
    ///
    /// # Example
    ///
    /// ```
    /// use ggen_config::{ConfigLoader, ConfigValidator};
    ///
    /// let toml = r#"
    ///     [project]
    ///     name = "my-project"
    ///     version = "1.0.0"
    /// "#;
    ///
    /// let config = ConfigLoader::from_str(toml).unwrap();
    /// let result = ConfigValidator::validate(&config);
    /// assert!(result.is_ok());
    /// ```
    pub fn validate(config: &'a GgenConfig) -> Result<()> {
        let mut validator = Self::new(config);
        validator.validate_all()?;
        Ok(())
    }

    /// Run all validation checks
    fn validate_all(&mut self) -> Result<()> {
        self.validate_project();
        self.validate_ai();
        self.validate_templates();
        self.validate_security();
        self.validate_performance();
        self.validate_logging();

        if self.errors.is_empty() {
            Ok(())
        } else {
            Err(ConfigError::Validation(self.errors.join("; ")))
        }
    }

    /// Validate project configuration
    fn validate_project(&mut self) {
        let project = &self.config.project;

        // Name validation
        if project.name.is_empty() {
            self.errors.push("Project name cannot be empty".to_string());
        }

        // Version validation (basic semver check)
        if !is_valid_version(&project.version) {
            self.errors.push(format!(
                "Invalid version format: '{}'. Expected semver format (e.g., 1.0.0)",
                project.version
            ));
        }
    }

    /// Validate AI configuration
    fn validate_ai(&mut self) {
        if let Some(ai) = &self.config.ai {
            // Provider validation
            let valid_providers = ["openai", "ollama", "anthropic", "cohere", "huggingface"];
            if !valid_providers.contains(&ai.provider.as_str()) {
                self.errors.push(format!(
                    "Unknown AI provider: '{}'. Valid providers: {:?}",
                    ai.provider, valid_providers
                ));
            }

            // Temperature validation (0.0 - 1.0)
            if !(0.0..=1.0).contains(&ai.temperature) {
                self.errors.push(format!(
                    "AI temperature must be between 0.0 and 1.0, got {}",
                    ai.temperature
                ));
            }

            // Max tokens validation
            if ai.max_tokens == 0 {
                self.errors.push("AI max_tokens must be greater than 0".to_string());
            }

            // Timeout validation
            if ai.timeout == 0 {
                self.errors.push("AI timeout must be greater than 0".to_string());
            }

            // Validation settings
            if let Some(validation) = &ai.validation {
                if !(0.0..=1.0).contains(&validation.quality_threshold) {
                    self.errors.push(format!(
                        "AI validation quality_threshold must be between 0.0 and 1.0, got {}",
                        validation.quality_threshold
                    ));
                }
            }
        }
    }

    /// Validate templates configuration
    fn validate_templates(&mut self) {
        if let Some(templates) = &self.config.templates {
            // Check that directories are not empty strings
            if let Some(dir) = &templates.directory {
                if dir.is_empty() {
                    self.errors.push("Templates directory cannot be empty".to_string());
                }
            }

            if let Some(out_dir) = &templates.output_directory {
                if out_dir.is_empty() {
                    self.errors.push("Templates output_directory cannot be empty".to_string());
                }
            }
        }
    }

    /// Validate security configuration
    fn validate_security(&self) {
        // Security settings are all boolean flags, no complex validation needed
        // Could add checks for conflicting settings if needed
    }

    /// Validate performance configuration
    fn validate_performance(&mut self) {
        if let Some(perf) = &self.config.performance {
            // Validate max_workers
            if perf.parallel_execution && perf.max_workers == 0 {
                self.errors.push(
                    "Performance max_workers must be greater than 0 when parallel_execution is enabled"
                        .to_string(),
                );
            }

            // Validate cache_size format
            if let Some(cache_size) = &perf.cache_size {
                if !is_valid_size_format(cache_size) {
                    self.errors.push(format!(
                        "Invalid cache_size format: '{cache_size}'. Expected format like '1GB', '512MB'"
                    ));
                }
            }
        }
    }

    /// Validate logging configuration
    fn validate_logging(&mut self) {
        if let Some(logging) = &self.config.logging {
            // Validate log level
            let valid_levels = ["trace", "debug", "info", "warn", "error"];
            if !valid_levels.contains(&logging.level.to_lowercase().as_str()) {
                self.errors.push(format!(
                    "Invalid log level: '{}'. Valid levels: {:?}",
                    logging.level, valid_levels
                ));
            }

            // Validate log format
            let valid_formats = ["json", "text", "pretty"];
            if !valid_formats.contains(&logging.format.to_lowercase().as_str()) {
                self.errors.push(format!(
                    "Invalid log format: '{}'. Valid formats: {:?}",
                    logging.format, valid_formats
                ));
            }
        }
    }
}

/// Validate version string (basic semver check)
fn is_valid_version(version: &str) -> bool {
    let parts: Vec<&str> = version.split('.').collect();
    if parts.len() != 3 {
        return false;
    }

    parts.iter().all(|part| part.parse::<u32>().is_ok())
}

/// Validate size format (e.g., "1GB", "512MB")
fn is_valid_size_format(size: &str) -> bool {
    let size = size.to_uppercase();
    let valid_suffixes = ["B", "KB", "MB", "GB", "TB"];

    valid_suffixes.iter().any(|suffix| {
        size.strip_suffix(suffix).map_or(false, |num_str| num_str.parse::<u32>().is_ok())
    })
}

/// Validate that there are no duplicate keys in a collection
#[allow(dead_code)]
fn has_duplicates<T: Eq + std::hash::Hash>(items: &[T]) -> bool {
    let mut seen = HashSet::new();
    items.iter().any(|item| !seen.insert(item))
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::{ConfigLoader, ProjectConfig};

    #[test]
    fn test_valid_minimal_config() {
        let config = GgenConfig {
            project: ProjectConfig {
                name: "test".to_string(),
                version: "1.0.0".to_string(),
                description: None,
                authors: None,
                license: None,
                repository: None,
            },
            ..Default::default()
        };

        assert!(ConfigValidator::validate(&config).is_ok());
    }

    #[test]
    fn test_invalid_empty_name() {
        let config = GgenConfig {
            project: ProjectConfig {
                name: String::new(),
                version: "1.0.0".to_string(),
                description: None,
                authors: None,
                license: None,
                repository: None,
            },
            ..Default::default()
        };

        assert!(ConfigValidator::validate(&config).is_err());
    }

    #[test]
    fn test_invalid_version() {
        let config = GgenConfig {
            project: ProjectConfig {
                name: "test".to_string(),
                version: "invalid".to_string(),
                description: None,
                authors: None,
                license: None,
                repository: None,
            },
            ..Default::default()
        };

        assert!(ConfigValidator::validate(&config).is_err());
    }

    #[test]
    fn test_version_validation() {
        assert!(is_valid_version("1.0.0"));
        assert!(is_valid_version("0.1.0"));
        assert!(is_valid_version("10.20.30"));

        assert!(!is_valid_version("1.0"));
        assert!(!is_valid_version("invalid"));
        assert!(!is_valid_version("1.0.0.0"));
    }

    #[test]
    fn test_size_format_validation() {
        assert!(is_valid_size_format("1GB"));
        assert!(is_valid_size_format("512MB"));
        assert!(is_valid_size_format("100kb"));

        assert!(!is_valid_size_format("invalid"));
        assert!(!is_valid_size_format("GB"));
        assert!(!is_valid_size_format("100"));
    }

    #[test]
    fn test_validate_ai_temperature() {
        let toml = r#"
            [project]
            name = "test"
            version = "1.0.0"

            [ai]
            provider = "openai"
            model = "gpt-4"
            temperature = 1.5
        "#;

        let config = ConfigLoader::from_str(toml).unwrap();
        assert!(ConfigValidator::validate(&config).is_err());
    }

    #[test]
    fn test_validate_log_level() {
        let toml = r#"
            [project]
            name = "test"
            version = "1.0.0"

            [logging]
            level = "invalid"
            format = "json"
        "#;

        let config = ConfigLoader::from_str(toml).unwrap();
        assert!(ConfigValidator::validate(&config).is_err());
    }
}
