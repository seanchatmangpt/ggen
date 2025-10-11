//! Ollama configuration

use crate::client::LlmConfig;
use crate::error::{GgenAiError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Ollama configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OllamaConfig {
    /// Base URL for Ollama API
    pub base_url: String,
    /// Default model to use
    pub model: String,
    /// Timeout in seconds
    pub timeout: u64,
}

impl Default for OllamaConfig {
    fn default() -> Self {
        Self {
            base_url: "http://localhost:11434".to_string(),
            model: "qwen3-coder:30b".to_string(),
            timeout: 30,
        }
    }
}

impl OllamaConfig {
    /// Create a new Ollama configuration
    pub fn new() -> Self {
        Self::default()
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
        if self.base_url.is_empty() {
            return Err(GgenAiError::configuration("Ollama base URL cannot be empty"));
        }
        
        if self.model.is_empty() {
            return Err(GgenAiError::configuration("Ollama model cannot be empty"));
        }
        
        if self.timeout == 0 {
            return Err(GgenAiError::configuration("Ollama timeout must be greater than 0"));
        }
        
        Ok(())
    }

    /// Convert to LlmConfig
    pub fn to_llm_config(&self) -> LlmConfig {
        LlmConfig {
            model: self.model.clone(),
            max_tokens: Some(2048),
            temperature: Some(0.7),
            top_p: Some(0.9),
            stop: None,
            extra: HashMap::new(),
        }
    }

    /// Create from environment variables
    pub fn from_env() -> Result<Self> {
        let base_url = std::env::var("OLLAMA_BASE_URL")
            .unwrap_or_else(|_| "http://localhost:11434".to_string());
        
        let model = std::env::var("OLLAMA_MODEL")
            .unwrap_or_else(|_| "qwen3-coder:30b".to_string());
        
        let timeout = std::env::var("OLLAMA_TIMEOUT")
            .unwrap_or_else(|_| "30".to_string())
            .parse()
            .map_err(|_| GgenAiError::configuration("Invalid OLLAMA_TIMEOUT value"))?;
        
        let config = Self {
            base_url,
            model,
            timeout,
        };
        
        config.validate()?;
        Ok(config)
    }
}

