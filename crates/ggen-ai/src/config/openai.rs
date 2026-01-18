//! OpenAI configuration

use crate::client::LlmConfig;
use crate::error::{GgenAiError, Result};
use crate::security::SecretString;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// OpenAI configuration
#[derive(Clone, Serialize, Deserialize)]
pub struct OpenAIConfig {
    /// API key for OpenAI (securely masked in debug/display)
    pub api_key: SecretString,
    /// Base URL for OpenAI API
    pub base_url: String,
    /// Default model to use
    pub model: String,
    /// Organization ID
    pub organization: Option<String>,
}

impl Default for OpenAIConfig {
    fn default() -> Self {
        Self {
            api_key: SecretString::new(String::new()),
            base_url: "https://api.openai.com/v1".to_string(),
            model: std::env::var("OPENAI_MODEL")
                .or_else(|_| std::env::var("GGEN_DEFAULT_MODEL"))
                .or_else(|_| std::env::var("DEFAULT_MODEL"))
                .unwrap_or_else(|_| "qwen3-coder:30b".to_string()),
            organization: None,
        }
    }
}

impl std::fmt::Debug for OpenAIConfig {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        f.debug_struct("OpenAIConfig")
            .field("api_key", &self.api_key) // Will use SecretString's Debug impl
            .field("base_url", &self.base_url)
            .field("model", &self.model)
            .field("organization", &self.organization)
            .finish()
    }
}

impl OpenAIConfig {
    /// Create a new OpenAI configuration
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
            return Err(GgenAiError::configuration("OpenAI API key cannot be empty"));
        }

        if self.base_url.is_empty() {
            return Err(GgenAiError::configuration(
                "OpenAI base URL cannot be empty",
            ));
        }

        if self.model.is_empty() {
            return Err(GgenAiError::configuration("OpenAI model cannot be empty"));
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
        let api_key = std::env::var("OPENAI_API_KEY").map_err(|_| {
            GgenAiError::configuration("OPENAI_API_KEY environment variable not set")
        })?;

        let base_url = std::env::var("OPENAI_BASE_URL")
            .unwrap_or_else(|_| "https://api.openai.com/v1".to_string());

        let model = std::env::var("OPENAI_MODEL")
            .or_else(|_| std::env::var("GGEN_DEFAULT_MODEL"))
            .or_else(|_| std::env::var("DEFAULT_MODEL"))
            .unwrap_or_else(|_| "qwen3-coder:30b".to_string());

        let organization = std::env::var("OPENAI_ORGANIZATION").ok();

        let config = Self {
            api_key: SecretString::new(api_key),
            base_url,
            model,
            organization,
        };

        config.validate()?;
        Ok(config)
    }
}
