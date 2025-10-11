//! AI configuration

use crate::client::LlmConfig;
use crate::error::{GgenAiError, Result};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// AI configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AiConfig {
    /// Default LLM configuration
    pub llm: LlmConfig,
    /// Generation settings
    pub generation: GenerationConfig,
}

/// Generation configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct GenerationConfig {
    /// Default temperature
    pub temperature: f32,
    /// Default max tokens
    pub max_tokens: u32,
    /// Default top-p
    pub top_p: f32,
    /// Enable streaming by default
    pub streaming: bool,
}

impl Default for AiConfig {
    fn default() -> Self {
        Self {
            llm: LlmConfig::default(),
            generation: GenerationConfig::default(),
        }
    }
}

impl Default for GenerationConfig {
    fn default() -> Self {
        Self {
            temperature: 0.7,
            max_tokens: 4096,
            top_p: 0.9,
            streaming: false,
        }
    }
}

impl AiConfig {
    /// Create a new AI configuration
    pub fn new() -> Self {
        Self::default()
    }

    /// Create with LLM configuration
    pub fn with_llm(mut self, llm: LlmConfig) -> Self {
        self.llm = llm;
        self
    }

    /// Create with generation configuration
    pub fn with_generation(mut self, generation: GenerationConfig) -> Self {
        self.generation = generation;
        self
    }

    /// Validate the configuration
    pub fn validate(&self) -> Result<()> {
        self.llm.validate()?;

        if self.generation.temperature < 0.0 || self.generation.temperature > 2.0 {
            return Err(GgenAiError::configuration(
                "Temperature must be between 0.0 and 2.0",
            ));
        }

        if self.generation.top_p < 0.0 || self.generation.top_p > 1.0 {
            return Err(GgenAiError::configuration(
                "Top-p must be between 0.0 and 1.0",
            ));
        }

        if self.generation.max_tokens == 0 {
            return Err(GgenAiError::configuration(
                "Max tokens must be greater than 0",
            ));
        }

        Ok(())
    }

    /// Create from environment variables
    pub fn from_env() -> Result<Self> {
        let llm = LlmConfig {
            model: std::env::var("GGEN_LLM_MODEL")
                .or_else(|_| std::env::var("GGEN_DEFAULT_MODEL"))
                .or_else(|_| std::env::var("DEFAULT_MODEL"))
                .unwrap_or_else(|_| "qwen3-coder:30b".to_string()),
            max_tokens: std::env::var("GGEN_LLM_MAX_TOKENS")
                .ok()
                .and_then(|s| s.parse().ok()),
            temperature: std::env::var("GGEN_LLM_TEMPERATURE")
                .ok()
                .and_then(|s| s.parse().ok()),
            top_p: std::env::var("GGEN_LLM_TOP_P")
                .ok()
                .and_then(|s| s.parse().ok()),
            stop: None,
            extra: HashMap::new(),
        };

        let generation = GenerationConfig {
            temperature: std::env::var("GGEN_GENERATION_TEMPERATURE")
                .unwrap_or_else(|_| "0.7".to_string())
                .parse()
                .map_err(|_| GgenAiError::configuration("Invalid GGEN_GENERATION_TEMPERATURE"))?,
            max_tokens: std::env::var("GGEN_GENERATION_MAX_TOKENS")
                .unwrap_or_else(|_| "4096".to_string())
                .parse()
                .map_err(|_| GgenAiError::configuration("Invalid GGEN_GENERATION_MAX_TOKENS"))?,
            top_p: std::env::var("GGEN_GENERATION_TOP_P")
                .unwrap_or_else(|_| "0.9".to_string())
                .parse()
                .map_err(|_| GgenAiError::configuration("Invalid GGEN_GENERATION_TOP_P"))?,
            streaming: std::env::var("GGEN_GENERATION_STREAMING")
                .unwrap_or_else(|_| "false".to_string())
                .parse()
                .unwrap_or(false),
        };

        let config = Self { llm, generation };
        config.validate()?;
        Ok(config)
    }
}
