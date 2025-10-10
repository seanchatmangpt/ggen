//! Configuration management for ggen-ai providers
//!
//! Handles provider-specific configuration, API keys, endpoints,
//! and other settings required for LLM interactions.

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::path::Path;
use std::time::Duration;

use crate::error::{GgenAiError, Result};

/// OpenAI provider configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OpenAIConfig {
    /// API key for authentication
    pub api_key: String,
    /// API endpoint (default: https://api.openai.com/v1)
    #[serde(default = "default_openai_url")]
    pub base_url: String,
    /// Default model to use
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_model: Option<String>,
    /// Request timeout in seconds
    #[serde(default = "default_timeout")]
    pub timeout_secs: u64,
    /// Additional custom headers
    #[serde(default)]
    pub headers: HashMap<String, String>,
}

fn default_openai_url() -> String {
    "https://api.openai.com/v1".to_string()
}

fn default_timeout() -> u64 {
    30
}

impl OpenAIConfig {
    /// Create a new OpenAI configuration with an API key
    pub fn new(api_key: impl Into<String>) -> Self {
        Self {
            api_key: api_key.into(),
            base_url: default_openai_url(),
            default_model: Some("gpt-3.5-turbo".to_string()),
            timeout_secs: default_timeout(),
            headers: HashMap::new(),
        }
    }

    /// Create a configuration builder
    pub fn builder() -> OpenAIConfigBuilder {
        OpenAIConfigBuilder::default()
    }

    /// Load configuration from environment variables
    ///
    /// Looks for:
    /// - OPENAI_API_KEY (required)
    /// - OPENAI_BASE_URL (optional)
    /// - OPENAI_DEFAULT_MODEL (optional)
    /// - OPENAI_TIMEOUT_SECS (optional)
    pub fn from_env() -> Result<Self> {
        dotenvy::dotenv().ok(); // Load .env file if present

        let api_key = std::env::var("OPENAI_API_KEY").map_err(|_| {
            GgenAiError::configuration("OPENAI_API_KEY environment variable not set")
        })?;

        let base_url = std::env::var("OPENAI_BASE_URL").unwrap_or_else(|_| default_openai_url());

        let default_model = std::env::var("OPENAI_DEFAULT_MODEL").ok();

        let timeout_secs = std::env::var("OPENAI_TIMEOUT_SECS")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or_else(default_timeout);

        Ok(Self {
            api_key,
            base_url,
            default_model,
            timeout_secs,
            headers: HashMap::new(),
        })
    }

    /// Set custom base URL
    pub fn with_base_url(mut self, url: impl Into<String>) -> Self {
        self.base_url = url.into();
        self
    }

    /// Set default model
    pub fn with_default_model(mut self, model: impl Into<String>) -> Self {
        self.default_model = Some(model.into());
        self
    }

    /// Set timeout in seconds
    pub fn with_timeout(mut self, secs: u64) -> Self {
        self.timeout_secs = secs;
        self
    }

    /// Add a custom header
    pub fn with_header(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.headers.insert(key.into(), value.into());
        self
    }

    /// Get timeout as Duration
    pub fn timeout(&self) -> Duration {
        Duration::from_secs(self.timeout_secs)
    }

    /// Validate the configuration
    pub fn validate(&self) -> Result<()> {
        if self.api_key.is_empty() {
            return Err(GgenAiError::configuration("API key cannot be empty"));
        }

        if self.timeout_secs == 0 {
            return Err(GgenAiError::configuration(
                "Timeout must be greater than 0",
            ));
        }

        Ok(())
    }
}

/// Builder for OpenAI configuration
#[derive(Default)]
pub struct OpenAIConfigBuilder {
    api_key: Option<String>,
    base_url: Option<String>,
    default_model: Option<String>,
    timeout_secs: Option<u64>,
    headers: HashMap<String, String>,
}

impl OpenAIConfigBuilder {
    pub fn api_key(mut self, key: impl Into<String>) -> Self {
        self.api_key = Some(key.into());
        self
    }

    pub fn base_url(mut self, url: impl Into<String>) -> Self {
        self.base_url = Some(url.into());
        self
    }

    pub fn default_model(mut self, model: impl Into<String>) -> Self {
        self.default_model = Some(model.into());
        self
    }

    pub fn timeout_secs(mut self, secs: u64) -> Self {
        self.timeout_secs = Some(secs);
        self
    }

    pub fn header(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.headers.insert(key.into(), value.into());
        self
    }

    pub fn build(self) -> Result<OpenAIConfig> {
        let api_key = self
            .api_key
            .ok_or_else(|| GgenAiError::configuration("API key is required"))?;

        let config = OpenAIConfig {
            api_key,
            base_url: self.base_url.unwrap_or_else(default_openai_url),
            default_model: self.default_model,
            timeout_secs: self.timeout_secs.unwrap_or_else(default_timeout),
            headers: self.headers,
        };

        config.validate()?;
        Ok(config)
    }
}

/// Anthropic provider configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AnthropicConfig {
    /// API key for authentication
    pub api_key: String,
    /// API endpoint (default: https://api.anthropic.com/v1)
    #[serde(default = "default_anthropic_url")]
    pub base_url: String,
    /// Default model to use
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_model: Option<String>,
    /// Request timeout in seconds
    #[serde(default = "default_timeout")]
    pub timeout_secs: u64,
    /// Additional custom headers
    #[serde(default)]
    pub headers: HashMap<String, String>,
}

fn default_anthropic_url() -> String {
    "https://api.anthropic.com/v1".to_string()
}

impl AnthropicConfig {
    /// Create a new Anthropic configuration with an API key
    pub fn new(api_key: impl Into<String>) -> Self {
        Self {
            api_key: api_key.into(),
            base_url: default_anthropic_url(),
            default_model: Some("claude-3-sonnet-20240229".to_string()),
            timeout_secs: default_timeout(),
            headers: HashMap::new(),
        }
    }

    /// Create a configuration builder
    pub fn builder() -> AnthropicConfigBuilder {
        AnthropicConfigBuilder::default()
    }

    /// Load configuration from environment variables
    ///
    /// Looks for:
    /// - ANTHROPIC_API_KEY (required)
    /// - ANTHROPIC_BASE_URL (optional)
    /// - ANTHROPIC_DEFAULT_MODEL (optional)
    /// - ANTHROPIC_TIMEOUT_SECS (optional)
    pub fn from_env() -> Result<Self> {
        dotenvy::dotenv().ok(); // Load .env file if present

        let api_key = std::env::var("ANTHROPIC_API_KEY").map_err(|_| {
            GgenAiError::configuration("ANTHROPIC_API_KEY environment variable not set")
        })?;

        let base_url =
            std::env::var("ANTHROPIC_BASE_URL").unwrap_or_else(|_| default_anthropic_url());

        let default_model = std::env::var("ANTHROPIC_DEFAULT_MODEL").ok();

        let timeout_secs = std::env::var("ANTHROPIC_TIMEOUT_SECS")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or_else(default_timeout);

        Ok(Self {
            api_key,
            base_url,
            default_model,
            timeout_secs,
            headers: HashMap::new(),
        })
    }

    /// Set custom base URL
    pub fn with_base_url(mut self, url: impl Into<String>) -> Self {
        self.base_url = url.into();
        self
    }

    /// Set default model
    pub fn with_default_model(mut self, model: impl Into<String>) -> Self {
        self.default_model = Some(model.into());
        self
    }

    /// Set timeout in seconds
    pub fn with_timeout(mut self, secs: u64) -> Self {
        self.timeout_secs = secs;
        self
    }

    /// Add a custom header
    pub fn with_header(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.headers.insert(key.into(), value.into());
        self
    }

    /// Get timeout as Duration
    pub fn timeout(&self) -> Duration {
        Duration::from_secs(self.timeout_secs)
    }

    /// Validate the configuration
    pub fn validate(&self) -> Result<()> {
        if self.api_key.is_empty() {
            return Err(GgenAiError::configuration("API key cannot be empty"));
        }

        if self.timeout_secs == 0 {
            return Err(GgenAiError::configuration(
                "Timeout must be greater than 0",
            ));
        }

        Ok(())
    }
}

/// Builder for Anthropic configuration
#[derive(Default)]
pub struct AnthropicConfigBuilder {
    api_key: Option<String>,
    base_url: Option<String>,
    default_model: Option<String>,
    timeout_secs: Option<u64>,
    headers: HashMap<String, String>,
}

impl AnthropicConfigBuilder {
    pub fn api_key(mut self, key: impl Into<String>) -> Self {
        self.api_key = Some(key.into());
        self
    }

    pub fn base_url(mut self, url: impl Into<String>) -> Self {
        self.base_url = Some(url.into());
        self
    }

    pub fn default_model(mut self, model: impl Into<String>) -> Self {
        self.default_model = Some(model.into());
        self
    }

    pub fn timeout_secs(mut self, secs: u64) -> Self {
        self.timeout_secs = Some(secs);
        self
    }

    pub fn header(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.headers.insert(key.into(), value.into());
        self
    }

    pub fn build(self) -> Result<AnthropicConfig> {
        let api_key = self
            .api_key
            .ok_or_else(|| GgenAiError::configuration("API key is required"))?;

        let config = AnthropicConfig {
            api_key,
            base_url: self.base_url.unwrap_or_else(default_anthropic_url),
            default_model: self.default_model,
            timeout_secs: self.timeout_secs.unwrap_or_else(default_timeout),
            headers: self.headers,
        };

        config.validate()?;
        Ok(config)
    }
}

/// Ollama provider configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OllamaConfig {
    /// API endpoint (default: http://localhost:11434)
    #[serde(default = "default_ollama_url")]
    pub base_url: String,
    /// Default model to use
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_model: Option<String>,
    /// Request timeout in seconds
    #[serde(default = "default_ollama_timeout")]
    pub timeout_secs: u64,
}

fn default_ollama_url() -> String {
    "http://localhost:11434".to_string()
}

fn default_ollama_timeout() -> u64 {
    60 // Ollama can be slower for local models
}

impl OllamaConfig {
    /// Create a new Ollama configuration with default settings
    pub fn new() -> Self {
        Self {
            base_url: default_ollama_url(),
            default_model: Some("llama2".to_string()),
            timeout_secs: default_ollama_timeout(),
        }
    }

    /// Create a configuration builder
    pub fn builder() -> OllamaConfigBuilder {
        OllamaConfigBuilder::default()
    }

    /// Load configuration from environment variables
    ///
    /// Looks for:
    /// - OLLAMA_BASE_URL (optional)
    /// - OLLAMA_DEFAULT_MODEL (optional)
    /// - OLLAMA_TIMEOUT_SECS (optional)
    pub fn from_env() -> Result<Self> {
        dotenvy::dotenv().ok(); // Load .env file if present

        let base_url = std::env::var("OLLAMA_BASE_URL").unwrap_or_else(|_| default_ollama_url());

        let default_model = std::env::var("OLLAMA_DEFAULT_MODEL").ok();

        let timeout_secs = std::env::var("OLLAMA_TIMEOUT_SECS")
            .ok()
            .and_then(|s| s.parse().ok())
            .unwrap_or_else(default_ollama_timeout);

        Ok(Self {
            base_url,
            default_model,
            timeout_secs,
        })
    }

    /// Set custom base URL
    pub fn with_base_url(mut self, url: impl Into<String>) -> Self {
        self.base_url = url.into();
        self
    }

    /// Set default model
    pub fn with_default_model(mut self, model: impl Into<String>) -> Self {
        self.default_model = Some(model.into());
        self
    }

    /// Set timeout in seconds
    pub fn with_timeout(mut self, secs: u64) -> Self {
        self.timeout_secs = secs;
        self
    }

    /// Get timeout as Duration
    pub fn timeout(&self) -> Duration {
        Duration::from_secs(self.timeout_secs)
    }

    /// Validate the configuration
    pub fn validate(&self) -> Result<()> {
        if self.timeout_secs == 0 {
            return Err(GgenAiError::configuration(
                "Timeout must be greater than 0",
            ));
        }

        Ok(())
    }
}

impl Default for OllamaConfig {
    fn default() -> Self {
        Self::new()
    }
}

/// Builder for Ollama configuration
#[derive(Default)]
pub struct OllamaConfigBuilder {
    base_url: Option<String>,
    default_model: Option<String>,
    timeout_secs: Option<u64>,
}

impl OllamaConfigBuilder {
    pub fn base_url(mut self, url: impl Into<String>) -> Self {
        self.base_url = Some(url.into());
        self
    }

    pub fn default_model(mut self, model: impl Into<String>) -> Self {
        self.default_model = Some(model.into());
        self
    }

    pub fn timeout_secs(mut self, secs: u64) -> Self {
        self.timeout_secs = Some(secs);
        self
    }

    pub fn build(self) -> Result<OllamaConfig> {
        let config = OllamaConfig {
            base_url: self.base_url.unwrap_or_else(default_ollama_url),
            default_model: self.default_model,
            timeout_secs: self.timeout_secs.unwrap_or_else(default_ollama_timeout),
        };

        config.validate()?;
        Ok(config)
    }
}

/// Global AI configuration for multiple providers
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct AiConfig {
    /// OpenAI configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub openai: Option<OpenAIConfig>,
    /// Anthropic configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub anthropic: Option<AnthropicConfig>,
    /// Ollama configuration
    #[serde(skip_serializing_if = "Option::is_none")]
    pub ollama: Option<OllamaConfig>,
    /// Default provider to use
    #[serde(skip_serializing_if = "Option::is_none")]
    pub default_provider: Option<String>,
}

impl AiConfig {
    /// Create a new empty configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Load configuration from environment variables
    pub fn from_env() -> Result<Self> {
        dotenvy::dotenv().ok(); // Load .env file if present

        let openai = OpenAIConfig::from_env().ok();
        let anthropic = AnthropicConfig::from_env().ok();
        let ollama = OllamaConfig::from_env().ok();

        let default_provider = std::env::var("AI_DEFAULT_PROVIDER").ok();

        Ok(Self {
            openai,
            anthropic,
            ollama,
            default_provider,
        })
    }

    /// Load configuration from a TOML file
    pub fn from_file(path: impl AsRef<Path>) -> Result<Self> {
        let content = std::fs::read_to_string(path)
            .map_err(|e| GgenAiError::configuration(format!("Failed to read config file: {}", e)))?;

        toml::from_str(&content)
            .map_err(|e| GgenAiError::configuration(format!("Failed to parse config file: {}", e)))
    }

    /// Set OpenAI configuration
    pub fn with_openai(mut self, config: OpenAIConfig) -> Self {
        self.openai = Some(config);
        self
    }

    /// Set Anthropic configuration
    pub fn with_anthropic(mut self, config: AnthropicConfig) -> Self {
        self.anthropic = Some(config);
        self
    }

    /// Set Ollama configuration
    pub fn with_ollama(mut self, config: OllamaConfig) -> Self {
        self.ollama = Some(config);
        self
    }

    /// Set default provider
    pub fn with_default_provider(mut self, provider: impl Into<String>) -> Self {
        self.default_provider = Some(provider.into());
        self
    }

    /// Get OpenAI configuration
    pub fn get_openai(&self) -> Result<&OpenAIConfig> {
        self.openai
            .as_ref()
            .ok_or_else(|| GgenAiError::configuration("OpenAI configuration not found"))
    }

    /// Get Anthropic configuration
    pub fn get_anthropic(&self) -> Result<&AnthropicConfig> {
        self.anthropic
            .as_ref()
            .ok_or_else(|| GgenAiError::configuration("Anthropic configuration not found"))
    }

    /// Get Ollama configuration
    pub fn get_ollama(&self) -> Result<&OllamaConfig> {
        self.ollama
            .as_ref()
            .ok_or_else(|| GgenAiError::configuration("Ollama configuration not found"))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_openai_config_creation() {
        let config = OpenAIConfig::new("test-key")
            .with_base_url("https://api.example.com")
            .with_default_model("gpt-4")
            .with_timeout(60);

        assert_eq!(config.api_key, "test-key");
        assert_eq!(config.base_url, "https://api.example.com");
        assert_eq!(config.default_model, Some("gpt-4".to_string()));
        assert_eq!(config.timeout_secs, 60);
    }

    #[test]
    fn test_openai_config_validation() {
        let valid = OpenAIConfig::new("key");
        assert!(valid.validate().is_ok());

        let invalid = OpenAIConfig::new("");
        assert!(invalid.validate().is_err());
    }

    #[test]
    fn test_openai_config_builder() {
        let config = OpenAIConfig::builder()
            .api_key("test-key")
            .base_url("https://custom.com")
            .default_model("gpt-4")
            .timeout_secs(120)
            .build()
            .unwrap();

        assert_eq!(config.api_key, "test-key");
        assert_eq!(config.base_url, "https://custom.com");
    }

    #[test]
    fn test_anthropic_config_creation() {
        let config = AnthropicConfig::new("test-key")
            .with_base_url("https://api.example.com")
            .with_default_model("claude-3-opus-20240229")
            .with_timeout(60);

        assert_eq!(config.api_key, "test-key");
        assert_eq!(config.base_url, "https://api.example.com");
        assert_eq!(
            config.default_model,
            Some("claude-3-opus-20240229".to_string())
        );
        assert_eq!(config.timeout_secs, 60);
    }

    #[test]
    fn test_ollama_config_creation() {
        let config = OllamaConfig::new()
            .with_base_url("http://custom:11434")
            .with_default_model("llama2")
            .with_timeout(90);

        assert_eq!(config.base_url, "http://custom:11434");
        assert_eq!(config.default_model, Some("llama2".to_string()));
        assert_eq!(config.timeout_secs, 90);
    }

    #[test]
    fn test_ollama_config_builder() {
        let config = OllamaConfig::builder()
            .base_url("http://custom:11434")
            .default_model("qwen3-coder:30b")
            .timeout_secs(120)
            .build()
            .unwrap();

        assert_eq!(config.base_url, "http://custom:11434");
        assert_eq!(config.default_model, Some("qwen3-coder:30b".to_string()));
    }

    #[test]
    fn test_ai_config() {
        let config = AiConfig::new()
            .with_openai(OpenAIConfig::new("key1"))
            .with_anthropic(AnthropicConfig::new("key2"))
            .with_ollama(OllamaConfig::new())
            .with_default_provider("openai");

        assert!(config.openai.is_some());
        assert!(config.anthropic.is_some());
        assert!(config.ollama.is_some());
        assert_eq!(config.default_provider, Some("openai".to_string()));
    }

    #[test]
    fn test_get_missing_provider() {
        let config = AiConfig::new();
        assert!(config.get_openai().is_err());
        assert!(config.get_anthropic().is_err());
        assert!(config.get_ollama().is_err());
    }
}
