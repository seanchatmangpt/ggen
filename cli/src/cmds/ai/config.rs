//! AI configuration for CLI commands

use clap::Args;
use ggen_ai::{LlmConfig, client::GenAiClient};
use ggen_utils::error::Result;
use std::collections::HashMap;

#[derive(Debug, Args)]
pub struct AiConfigArgs {
    /// AI model to use (e.g., gpt-4, claude-3-sonnet, qwen3-coder:30b)
    #[arg(short, long, default_value = "qwen3-coder:30b")]
    pub model: String,

    /// Maximum tokens to generate
    #[arg(long, default_value = "4096")]
    pub max_tokens: u32,

    /// Temperature for sampling (0.0 to 2.0)
    #[arg(long, default_value = "0.7")]
    pub temperature: f32,

    /// Top-p for nucleus sampling (0.0 to 1.0)
    #[arg(long, default_value = "0.9")]
    pub top_p: f32,

    /// Stop sequences (comma-separated)
    #[arg(long)]
    pub stop: Option<String>,

    /// Provider-specific API key (overrides environment variables)
    #[arg(long)]
    pub api_key: Option<String>,

    /// Custom endpoint URL (for self-hosted models)
    #[arg(long)]
    pub endpoint: Option<String>,
}

impl AiConfigArgs {
    /// Create a GenAI client from the configuration
    pub fn create_client(&self) -> Result<GenAiClient> {
        let config = self.to_llm_config();
        GenAiClient::new(config).map_err(|e| ggen_utils::error::Error::new(&e.to_string()))
    }

    /// Convert to LlmConfig
    pub fn to_llm_config(&self) -> LlmConfig {
        let mut extra = HashMap::new();
        
        if let Some(api_key) = &self.api_key {
            extra.insert("api_key".to_string(), serde_json::Value::String(api_key.clone()));
        }
        
        if let Some(endpoint) = &self.endpoint {
            extra.insert("endpoint".to_string(), serde_json::Value::String(endpoint.clone()));
        }

        LlmConfig {
            model: self.model.clone(),
            max_tokens: Some(self.max_tokens),
            temperature: Some(self.temperature),
            top_p: Some(self.top_p),
            stop: self.stop.as_ref().map(|s| s.split(',').map(|s| s.trim().to_string()).collect()),
            extra,
        }
    }

    /// Validate the configuration
    pub fn validate(&self) -> Result<()> {
        if self.model.is_empty() {
            return Err(ggen_utils::error::Error::new("Model name cannot be empty"));
        }

        if self.max_tokens == 0 {
            return Err(ggen_utils::error::Error::new("Max tokens must be greater than 0"));
        }

        if self.max_tokens > 128000 {
            return Err(ggen_utils::error::Error::new("Max tokens cannot exceed 128000"));
        }

        if self.temperature < 0.0 || self.temperature > 2.0 {
            return Err(ggen_utils::error::Error::new("Temperature must be between 0.0 and 2.0"));
        }

        if self.top_p < 0.0 || self.top_p > 1.0 {
            return Err(ggen_utils::error::Error::new("Top-p must be between 0.0 and 1.0"));
        }

        Ok(())
    }

    /// Get provider name from model
    pub fn provider_name(&self) -> &'static str {
        if self.model.starts_with("gpt") {
            "OpenAI"
        } else if self.model.starts_with("claude") {
            "Anthropic"
        } else if self.model.starts_with("command") {
            "Cohere"
        } else if self.model.starts_with("gemini") {
            "Gemini"
        } else if self.model.contains("groq") {
            "Groq"
        } else {
            "Ollama"
        }
    }

    /// Check if API key is required for this provider
    pub fn requires_api_key(&self) -> bool {
        match self.provider_name() {
            "OpenAI" | "Anthropic" | "Cohere" | "Gemini" | "Groq" => true,
            "Ollama" => false,
            _ => true,
        }
    }

    /// Get environment variable name for API key
    pub fn api_key_env_var(&self) -> Option<&'static str> {
        match self.provider_name() {
            "OpenAI" => Some("OPENAI_API_KEY"),
            "Anthropic" => Some("ANTHROPIC_API_KEY"),
            "Cohere" => Some("COHERE_API_KEY"),
            "Gemini" => Some("GEMINI_API_KEY"),
            "Groq" => Some("GROQ_API_KEY"),
            "Ollama" => None,
            _ => None,
        }
    }
}

impl Default for AiConfigArgs {
    fn default() -> Self {
        Self {
            model: "qwen3-coder:30b".to_string(),
            max_tokens: 4096,
            temperature: 0.7,
            top_p: 0.9,
            stop: Some("```".to_string()),
            api_key: None,
            endpoint: None,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_ai_config_args_creation() {
        let config = AiConfigArgs::default();
        assert_eq!(config.model, "qwen3-coder:30b");
        assert_eq!(config.max_tokens, 4096);
        assert_eq!(config.temperature, 0.7);
        assert_eq!(config.top_p, 0.9);
    }

    #[test]
    fn test_provider_detection() {
        let config = AiConfigArgs {
            model: "gpt-4".to_string(),
            ..Default::default()
        };
        assert_eq!(config.provider_name(), "OpenAI");
        assert!(config.requires_api_key());

        let config = AiConfigArgs {
            model: "qwen3-coder:30b".to_string(),
            ..Default::default()
        };
        assert_eq!(config.provider_name(), "Ollama");
        assert!(!config.requires_api_key());
    }

    #[test]
    fn test_validation() {
        let config = AiConfigArgs::default();
        assert!(config.validate().is_ok());

        let mut config = AiConfigArgs::default();
        config.model = "".to_string();
        assert!(config.validate().is_err());

        let mut config = AiConfigArgs::default();
        config.max_tokens = 0;
        assert!(config.validate().is_err());

        let mut config = AiConfigArgs::default();
        config.temperature = -0.1;
        assert!(config.validate().is_err());
    }
}

