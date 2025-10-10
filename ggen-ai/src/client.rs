//! LLM client abstraction and adapter

use async_trait::async_trait;
use futures::stream::BoxStream;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

use crate::error::{GgenAiError, Result};

/// Configuration for LLM requests
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmConfig {
    /// Model name/identifier
    pub model: String,
    /// Maximum tokens to generate
    pub max_tokens: Option<u32>,
    /// Temperature for sampling (0.0 to 2.0)
    pub temperature: Option<f32>,
    /// Top-p for nucleus sampling (0.0 to 1.0)
    pub top_p: Option<f32>,
    /// Stop sequences
    pub stop: Option<Vec<String>>,
    /// Additional parameters
    pub extra: HashMap<String, serde_json::Value>,
}

impl LlmConfig {
    /// Validate the configuration
    pub fn validate(&self) -> Result<()> {
        if self.model.is_empty() {
            return Err(GgenAiError::invalid_config("model", "Model name cannot be empty"));
        }

        if let Some(max_tokens) = self.max_tokens {
            if max_tokens == 0 {
                return Err(GgenAiError::invalid_config("max_tokens", "Must be greater than 0"));
            }
            if max_tokens > 128000 {
                return Err(GgenAiError::invalid_config(
                    "max_tokens",
                    "Cannot exceed 128000 tokens",
                ));
            }
        }

        if let Some(temp) = self.temperature {
            GgenAiError::validate_temperature(temp, "temperature")?;
        }

        if let Some(top_p) = self.top_p {
            GgenAiError::validate_top_p(top_p, "top_p")?;
        }

        Ok(())
    }
}

impl Default for LlmConfig {
    fn default() -> Self {
        Self {
            model: "qwen3-coder:30b".to_string(),
            max_tokens: Some(4096),
            temperature: Some(0.3),
            top_p: Some(0.9),
            stop: None,
            extra: HashMap::new(),
        }
    }
}

/// Response from LLM completion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmResponse {
    /// Generated text content
    pub content: String,
    /// Usage statistics
    pub usage: Option<UsageStats>,
    /// Model used
    pub model: String,
    /// Finish reason
    pub finish_reason: Option<String>,
    /// Additional metadata
    pub extra: HashMap<String, serde_json::Value>,
}

/// Usage statistics for LLM requests
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UsageStats {
    /// Prompt tokens used
    pub prompt_tokens: u32,
    /// Completion tokens used
    pub completion_tokens: u32,
    /// Total tokens used
    pub total_tokens: u32,
}

/// Streaming response chunk
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmChunk {
    /// Partial content
    pub content: String,
    /// Model used
    pub model: String,
    /// Finish reason
    pub finish_reason: Option<String>,
    /// Usage statistics (only in final chunk)
    pub usage: Option<UsageStats>,
    /// Additional metadata
    pub extra: HashMap<String, serde_json::Value>,
}

/// Trait for LLM client implementations
#[async_trait]
pub trait LlmClient: Send + Sync + std::fmt::Debug {
    /// Complete a prompt synchronously
    async fn complete(&self, prompt: &str) -> Result<LlmResponse>;
    
    /// Stream completion of a prompt
    async fn stream_complete(&self, prompt: &str) -> Result<BoxStream<LlmChunk>>;
    
    /// Get the current configuration
    fn get_config(&self) -> &LlmConfig;
    
    /// Update the configuration
    fn update_config(&mut self, config: LlmConfig);
}

/// Unified adapter for multiple LLM providers
pub struct LlmAdapter {
    clients: HashMap<String, Box<dyn LlmClient>>,
    default_client: String,
}

impl LlmAdapter {
    /// Create a new adapter
    pub fn new() -> Self {
        Self {
            clients: HashMap::new(),
            default_client: String::new(),
        }
    }
    
    /// Add a client to the adapter
    pub fn add_client(&mut self, name: String, client: Box<dyn LlmClient>) {
        if self.clients.is_empty() {
            self.default_client = name.clone();
        }
        self.clients.insert(name, client);
    }
    
    /// Set the default client
    pub fn set_default(&mut self, name: &str) -> Result<()> {
        if self.clients.contains_key(name) {
            self.default_client = name.to_string();
            Ok(())
        } else {
            Err(GgenAiError::configuration(format!("Client '{}' not found", name)))
        }
    }
    
    /// Get a client by name
    pub fn get_client(&self, name: Option<&str>) -> Result<&dyn LlmClient> {
        let name = name.unwrap_or(&self.default_client);
        self.clients
            .get(name)
            .map(|client| client.as_ref())
            .ok_or_else(|| GgenAiError::configuration(format!("Client '{}' not found", name)))
    }
    
    /// Complete using the default client
    pub async fn complete(&self, prompt: &str, config: Option<LlmConfig>) -> Result<LlmResponse> {
        self.get_client(None)?.complete(prompt, config).await
    }
    
    /// Stream completion using the default client
    pub async fn stream_complete(
        &self,
        prompt: &str,
        config: Option<LlmConfig>,
    ) -> Result<BoxStream<'static, Result<LlmChunk>>> {
        self.get_client(None)?.stream_complete(prompt, config).await
    }
    
    /// Generate embeddings using the default client
    pub async fn embed(&self, text: &str) -> Result<Vec<f32>> {
        self.get_client(None)?.embed(text).await
    }
    
    /// List available clients
    pub fn list_clients(&self) -> Vec<String> {
        self.clients.keys().cloned().collect()
    }
    
    /// Get the default client name
    pub fn default_client(&self) -> &str {
        &self.default_client
    }
}

impl Default for LlmAdapter {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_llm_adapter() {
        let adapter = LlmAdapter::new();

        // Test empty adapter
        assert!(adapter.get_client(None).is_err());
        assert!(adapter.complete("test", None).await.is_err());

        // Test with mock client (would need actual implementation)
        // This is a placeholder for when we implement the providers
    }

    #[test]
    fn test_llm_config_validation() {
        // Valid config
        let config = LlmConfig::default();
        assert!(config.validate().is_ok());

        // Empty model
        let mut config = LlmConfig::default();
        config.model = "".to_string();
        assert!(config.validate().is_err());

        // Invalid max_tokens
        let mut config = LlmConfig::default();
        config.max_tokens = Some(0);
        assert!(config.validate().is_err());

        config.max_tokens = Some(200000);
        assert!(config.validate().is_err());

        // Invalid temperature
        let mut config = LlmConfig::default();
        config.temperature = Some(-0.1);
        assert!(config.validate().is_err());

        config.temperature = Some(2.5);
        assert!(config.validate().is_err());

        // Invalid top_p
        let mut config = LlmConfig::default();
        config.top_p = Some(-0.1);
        assert!(config.validate().is_err());

        config.top_p = Some(1.5);
        assert!(config.validate().is_err());
    }
}

