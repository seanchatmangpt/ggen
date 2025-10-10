//! Configuration management for LLM providers
//!
//! Handles provider-specific configuration, API keys, endpoints,
//! and other settings required for LLM interactions.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use super::error::{LlmError, LlmResult};

/// Configuration for a specific LLM provider
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProviderConfig {
    /// API key for authentication
    pub api_key: String,
    /// Optional custom API endpoint
    #[serde(skip_serializing_if = "Option::is_none")]
    pub endpoint: Option<String>,
    /// Default model to use
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_model: Option<String>,
    /// Timeout in seconds
    #[serde(default = "default_timeout")]
    pub timeout_secs: u64,
    /// Additional custom headers
    #[serde(default)]
    pub headers: HashMap<String, String>,
}

fn default_timeout() -> u64 {
    30
}

impl ProviderConfig {
    /// Create a new provider configuration
    pub fn new(api_key: impl Into<String>) -> Self {
        Self {
            api_key: api_key.into(),
            endpoint: None,
            default_model: None,
            timeout_secs: default_timeout(),
            headers: HashMap::new(),
        }
    }

    /// Set custom endpoint
    pub fn with_endpoint(mut self, endpoint: impl Into<String>) -> Self {
        self.endpoint = Some(endpoint.into());
        self
    }

    /// Set default model
    pub fn with_default_model(mut self, model: impl Into<String>) -> Self {
        self.default_model = Some(model.into());
        self
    }

    /// Set timeout
    pub fn with_timeout(mut self, secs: u64) -> Self {
        self.timeout_secs = secs;
        self
    }

    /// Add a custom header
    pub fn with_header(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.headers.insert(key.into(), value.into());
        self
    }

    /// Validate the configuration
    pub fn validate(&self) -> LlmResult<()> {
        if self.api_key.is_empty() {
            return Err(LlmError::ConfigError {
                message: "API key cannot be empty".to_string(),
            });
        }

        if self.timeout_secs == 0 {
            return Err(LlmError::ConfigError {
                message: "Timeout must be greater than 0".to_string(),
            });
        }

        Ok(())
    }
}

/// Global LLM configuration for multiple providers
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct LlmConfig {
    /// Provider-specific configurations
    pub providers: HashMap<String, ProviderConfig>,
    /// Default provider to use
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_provider: Option<String>,
}

impl LlmConfig {
    /// Create a new empty configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a provider configuration
    pub fn add_provider(
        mut self,
        name: impl Into<String>,
        config: ProviderConfig,
    ) -> LlmResult<Self> {
        config.validate()?;
        self.providers.insert(name.into(), config);
        Ok(self)
    }

    /// Set the default provider
    pub fn with_default_provider(mut self, provider: impl Into<String>) -> Self {
        self.default_provider = Some(provider.into());
        self
    }

    /// Get a provider configuration
    pub fn get_provider(&self, name: &str) -> LlmResult<&ProviderConfig> {
        self.providers.get(name).ok_or_else(|| LlmError::ConfigError {
            message: format!("Provider '{}' not found in configuration", name),
        })
    }

    /// Get the default provider configuration
    pub fn get_default_provider(&self) -> LlmResult<(&str, &ProviderConfig)> {
        let name = self.default_provider.as_ref().ok_or_else(|| {
            LlmError::ConfigError {
                message: "No default provider set".to_string(),
            }
        })?;

        Ok((name, self.get_provider(name)?))
    }

    /// Load configuration from environment variables
    ///
    /// Looks for:
    /// - OPENAI_API_KEY
    /// - ANTHROPIC_API_KEY
    /// - LLM_DEFAULT_PROVIDER
    pub fn from_env() -> LlmResult<Self> {
        let mut config = Self::new();

        // OpenAI
        if let Ok(api_key) = std::env::var("OPENAI_API_KEY") {
            config = config.add_provider("openai", ProviderConfig::new(api_key))?;
        }

        // Anthropic
        if let Ok(api_key) = std::env::var("ANTHROPIC_API_KEY") {
            config = config.add_provider("anthropic", ProviderConfig::new(api_key))?;
        }

        // Default provider
        if let Ok(default) = std::env::var("LLM_DEFAULT_PROVIDER") {
            config = config.with_default_provider(default);
        }

        Ok(config)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_provider_config_creation() {
        let config = ProviderConfig::new("test-key")
            .with_endpoint("https://api.example.com")
            .with_default_model("gpt-4")
            .with_timeout(60);

        assert_eq!(config.api_key, "test-key");
        assert_eq!(config.endpoint, Some("https://api.example.com".to_string()));
        assert_eq!(config.default_model, Some("gpt-4".to_string()));
        assert_eq!(config.timeout_secs, 60);
    }

    #[test]
    fn test_config_validation() {
        let valid = ProviderConfig::new("key");
        assert!(valid.validate().is_ok());

        let invalid = ProviderConfig::new("");
        assert!(invalid.validate().is_err());
    }

    #[test]
    fn test_llm_config() {
        let config = LlmConfig::new()
            .add_provider("openai", ProviderConfig::new("key1"))
            .unwrap()
            .add_provider("anthropic", ProviderConfig::new("key2"))
            .unwrap()
            .with_default_provider("openai");

        assert_eq!(config.providers.len(), 2);
        assert!(config.get_provider("openai").is_ok());
        assert!(config.get_default_provider().is_ok());
    }

    #[test]
    fn test_get_nonexistent_provider() {
        let config = LlmConfig::new();
        assert!(config.get_provider("unknown").is_err());
    }
}
