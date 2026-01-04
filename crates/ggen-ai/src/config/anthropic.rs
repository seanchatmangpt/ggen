//! Anthropic configuration

use crate::client::LlmConfig;
use crate::error::{GgenAiError, Result};
use crate::security::SecretString;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Anthropic configuration
#[derive(Clone, Serialize, Deserialize)]
pub struct AnthropicConfig {
    /// API key for Anthropic (securely masked in debug/display)
    pub api_key: SecretString,
    /// Base URL for Anthropic API
    pub base_url: String,
    /// Default model to use
    pub model: String,
}

impl Default for AnthropicConfig {
    fn default() -> Self {
        Self {
            api_key: SecretString::new(String::new()),
            base_url: "https://api.anthropic.com".to_string(),
            model: "claude-haiku-4-5-20251001".to_string(),
        }
    }
}

impl std::fmt::Debug for AnthropicConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("AnthropicConfig")
            .field("api_key", &self.api_key) // Will use SecretString's Debug impl
            .field("base_url", &self.base_url)
            .field("model", &self.model)
            .finish()
    }
}

impl AnthropicConfig {
    /// Create a new Anthropic configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Create with API key
    pub fn with_api_key(mut self, api_key: impl Into<SecretString>) -> Self {
        self.api_key = api_key.into();
        self
    }

    /// Create with custom base URL
    pub fn with_base_url(mut self, base_url: String) -> Self {
        self.base_url = base_url;
        self
    }

    /// Create with custom model
    pub fn with_model(mut self, model: String) -> Self {
        self.model = model;
        self
    }

    /// Validate the configuration
    pub fn validate(&self) -> Result<()> {
        if self.api_key.is_empty() {
            return Err(GgenAiError::configuration(
                "Anthropic API key cannot be empty",
            ));
        }

        if self.base_url.is_empty() {
            return Err(GgenAiError::configuration(
                "Anthropic base URL cannot be empty",
            ));
        }

        if self.model.is_empty() {
            return Err(GgenAiError::configuration(
                "Anthropic model cannot be empty",
            ));
        }

        Ok(())
    }

    /// Convert to LlmConfig
    pub fn to_llm_config(&self) -> LlmConfig {
        LlmConfig {
            model: self.model.clone(),
            max_tokens: Some(4096),
            temperature: Some(0.7),
            top_p: Some(0.9),
            stop: None,
            extra: HashMap::new(),
        }
    }

    /// Create from environment variables
    pub fn from_env() -> Result<Self> {
        let api_key = std::env::var("ANTHROPIC_API_KEY").map_err(|_| {
            GgenAiError::configuration("ANTHROPIC_API_KEY environment variable not set")
        })?;

        let base_url = std::env::var("ANTHROPIC_BASE_URL")
            .unwrap_or_else(|_| "https://api.anthropic.com".to_string());

        let model = std::env::var("ANTHROPIC_MODEL")
            .unwrap_or_else(|_| "claude-3-sonnet-20240229".to_string());

        let config = Self {
            api_key: SecretString::new(api_key),
            base_url,
            model,
        };

        config.validate()?;
        Ok(config)
    }
}
