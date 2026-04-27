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
            return Err(GgenAiError::configuration(
                "Ollama base URL cannot be empty",
            ));
        }

        if self.model.is_empty() {
            return Err(GgenAiError::configuration("Ollama model cannot be empty"));
        }

        if self.timeout == 0 {
            return Err(GgenAiError::configuration(
                "Ollama timeout must be greater than 0",
            ));
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

        let model = std::env::var("OLLAMA_MODEL").unwrap_or_else(|_| "qwen3-coder:30b".to_string());

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

#[cfg(test)]
mod tests {
    use super::*;

    /// Chicago TDD: Verify default configuration creates valid state
    #[test]
    fn test_default_ollama_config_is_valid() {
        // Arrange & Act
        let config = OllamaConfig::default();

        // Assert - Verify observable state
        assert_eq!(config.base_url, "http://localhost:11434");
        assert_eq!(config.model, "qwen3-coder:30b");
        assert_eq!(config.timeout, 30);
        assert!(config.validate().is_ok());
    }

    /// Chicago TDD: Verify new() produces same state as default()
    #[test]
    fn test_new_creates_default_config() {
        // Arrange & Act
        let config = OllamaConfig::new();
        let default = OllamaConfig::default();

        // Assert - Verify state equality
        assert_eq!(config.base_url, default.base_url);
        assert_eq!(config.model, default.model);
        assert_eq!(config.timeout, default.timeout);
    }

    /// Chicago TDD: Verify builder pattern correctly modifies base_url
    #[test]
    fn test_with_base_url_modifies_state() {
        // Arrange
        let original_url = "http://localhost:11434".to_string();
        let new_url = "http://custom-host:8080".to_string();

        // Act
        let config = OllamaConfig::new().with_base_url(new_url.clone());

        // Assert - Verify state change
        assert_eq!(config.base_url, new_url);
        assert_ne!(config.base_url, original_url);
        // Verify other fields unchanged
        assert_eq!(config.model, "qwen3-coder:30b");
        assert_eq!(config.timeout, 30);
    }

    /// Chicago TDD: Verify builder pattern correctly modifies model
    #[test]
    fn test_with_model_modifies_state() {
        // Arrange
        let original_model = "qwen3-coder:30b".to_string();
        let new_model = "llama2:7b".to_string();

        // Act
        let config = OllamaConfig::new().with_model(new_model.clone());

        // Assert - Verify state change
        assert_eq!(config.model, new_model);
        assert_ne!(config.model, original_model);
        // Verify other fields unchanged
        assert_eq!(config.base_url, "http://localhost:11434");
        assert_eq!(config.timeout, 30);
    }

    /// Chicago TDD: Verify builder chaining preserves all changes
    #[test]
    fn test_builder_chaining_applies_all_changes() {
        // Arrange
        let custom_url = "http://prod-server:11434".to_string();
        let custom_model = "codellama:34b".to_string();

        // Act
        let config = OllamaConfig::new()
            .with_base_url(custom_url.clone())
            .with_model(custom_model.clone());

        // Assert - Verify all state changes applied
        assert_eq!(config.base_url, custom_url);
        assert_eq!(config.model, custom_model);
        assert_eq!(config.timeout, 30); // Unchanged
    }

    /// Chicago TDD: Verify validation rejects empty base_url
    #[test]
    fn test_validate_rejects_empty_base_url() {
        // Arrange
        let mut config = OllamaConfig::default();
        config.base_url = String::new();

        // Act
        let result = config.validate();

        // Assert - Verify error returned
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("base URL cannot be empty"));
    }

    /// Chicago TDD: Verify validation rejects empty model
    #[test]
    fn test_validate_rejects_empty_model() {
        // Arrange
        let mut config = OllamaConfig::default();
        config.model = String::new();

        // Act
        let result = config.validate();

        // Assert - Verify error returned
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("model cannot be empty"));
    }

    /// Chicago TDD: Verify validation rejects zero timeout
    #[test]
    fn test_validate_rejects_zero_timeout() {
        // Arrange
        let mut config = OllamaConfig::default();
        config.timeout = 0;

        // Act
        let result = config.validate();

        // Assert - Verify error returned
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("timeout must be greater than 0"));
    }

    /// Chicago TDD: Verify validation accepts valid config
    #[test]
    fn test_validate_accepts_valid_config() {
        // Arrange
        let config = OllamaConfig {
            base_url: "http://valid-url:11434".to_string(),
            model: "valid-model:7b".to_string(),
            timeout: 60,
        };

        // Act
        let result = config.validate();

        // Assert - Verify success
        assert!(result.is_ok());
    }

    /// Chicago TDD: Verify to_llm_config produces correct output
    #[test]
    fn test_to_llm_config_conversion() {
        // Arrange
        let config = OllamaConfig {
            base_url: "http://localhost:11434".to_string(),
            model: "test-model:13b".to_string(),
            timeout: 45,
        };

        // Act
        let llm_config = config.to_llm_config();

        // Assert - Verify correct conversion
        assert_eq!(llm_config.model, "test-model:13b");
        assert_eq!(llm_config.max_tokens, Some(2048));
        assert_eq!(llm_config.temperature, Some(0.7));
        assert_eq!(llm_config.top_p, Some(0.9));
        assert!(llm_config.stop.is_none());
        assert!(llm_config.extra.is_empty());
    }

    /// Chicago TDD: Verify from_env uses defaults when vars not set
    #[test]
    fn test_from_env_uses_defaults_when_vars_not_set() {
        // Arrange - Ensure env vars are not set
        std::env::remove_var("OLLAMA_BASE_URL");
        std::env::remove_var("OLLAMA_MODEL");
        std::env::remove_var("OLLAMA_TIMEOUT");

        // Act
        let config = OllamaConfig::from_env().expect("Should use defaults");

        // Assert - Verify default values used
        assert_eq!(config.base_url, "http://localhost:11434");
        assert_eq!(config.model, "qwen3-coder:30b");
        assert_eq!(config.timeout, 30);
    }

    /// Chicago TDD: Verify from_env reads OLLAMA_BASE_URL
    #[test]
    fn test_from_env_reads_base_url_from_environment() {
        // Arrange
        let custom_url = "http://env-test:9999";
        std::env::set_var("OLLAMA_BASE_URL", custom_url);

        // Act
        let config = OllamaConfig::from_env().expect("Should read from env");

        // Assert - Verify env var was read
        assert_eq!(config.base_url, custom_url);

        // Cleanup
        std::env::remove_var("OLLAMA_BASE_URL");
    }

    /// Chicago TDD: Verify from_env reads OLLAMA_MODEL
    #[test]
    fn test_from_env_reads_model_from_environment() {
        // Arrange
        let custom_model = "mistral:7b";
        std::env::set_var("OLLAMA_MODEL", custom_model);

        // Act
        let config = OllamaConfig::from_env().expect("Should read from env");

        // Assert - Verify env var was read
        assert_eq!(config.model, custom_model);

        // Cleanup
        std::env::remove_var("OLLAMA_MODEL");
    }

    /// Chicago TDD: Verify from_env reads OLLAMA_TIMEOUT
    #[test]
    fn test_from_env_reads_timeout_from_environment() {
        // Arrange
        std::env::set_var("OLLAMA_TIMEOUT", "120");

        // Act
        let config = OllamaConfig::from_env().expect("Should read from env");

        // Assert - Verify env var was read and parsed
        assert_eq!(config.timeout, 120);

        // Cleanup
        std::env::remove_var("OLLAMA_TIMEOUT");
    }

    /// Chicago TDD: Verify from_env rejects invalid timeout format
    #[test]
    fn test_from_env_rejects_invalid_timeout_format() {
        // Arrange
        std::env::set_var("OLLAMA_TIMEOUT", "not-a-number");

        // Act
        let result = OllamaConfig::from_env();

        // Assert - Verify error returned
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("Invalid OLLAMA_TIMEOUT value"));

        // Cleanup
        std::env::remove_var("OLLAMA_TIMEOUT");
    }

    /// Chicago TDD: Verify from_env validates result
    #[test]
    fn test_from_env_validates_resulting_config() {
        // Arrange - Set invalid config via env
        std::env::set_var("OLLAMA_BASE_URL", "");
        std::env::set_var("OLLAMA_MODEL", "valid-model");
        std::env::set_var("OLLAMA_TIMEOUT", "30");

        // Act
        let result = OllamaConfig::from_env();

        // Assert - Verify validation caught invalid config
        assert!(result.is_err());
        let err = result.unwrap_err();
        assert!(err.to_string().contains("base URL cannot be empty"));

        // Cleanup
        std::env::remove_var("OLLAMA_BASE_URL");
        std::env::remove_var("OLLAMA_MODEL");
        std::env::remove_var("OLLAMA_TIMEOUT");
    }

    /// Chicago TDD: Verify config is cloneable
    #[test]
    fn test_config_is_cloneable() {
        // Arrange
        let original = OllamaConfig::new().with_model("test-model".to_string());

        // Act
        let cloned = original.clone();

        // Assert - Verify clone has same state
        assert_eq!(cloned.base_url, original.base_url);
        assert_eq!(cloned.model, original.model);
        assert_eq!(cloned.timeout, original.timeout);
    }

    /// Chicago TDD: Verify config serialization roundtrip
    #[test]
    fn test_config_serialization_roundtrip() {
        // Arrange
        let config = OllamaConfig {
            base_url: "http://test:11434".to_string(),
            model: "test-model:7b".to_string(),
            timeout: 45,
        };

        // Act
        let json = serde_json::to_string(&config).expect("Should serialize");
        let deserialized: OllamaConfig =
            serde_json::from_str(&json).expect("Should deserialize");

        // Assert - Verify roundtrip preserves state
        assert_eq!(deserialized.base_url, config.base_url);
        assert_eq!(deserialized.model, config.model);
        assert_eq!(deserialized.timeout, config.timeout);
    }
}
