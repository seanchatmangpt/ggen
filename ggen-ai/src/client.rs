//! Simplified client interface using rust-genai

use genai::{Client, chat::{ChatRequest, ChatMessage, ChatOptions}};
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

/// Simplified LLM client using rust-genai
#[derive(Debug)]
pub struct LlmClient {
    client: Client,
    config: LlmConfig,
}

impl LlmClient {
    /// Create a new LLM client with configuration
    pub fn new(config: LlmConfig) -> Result<Self> {
        config.validate()?;
        Ok(Self {
            client: Client::default(),
            config,
        })
    }

    /// Complete a prompt synchronously
    pub async fn complete(&self, prompt: &str) -> Result<LlmResponse> {
        let chat_req = ChatRequest::new(vec![
            ChatMessage::user(prompt),
        ]);

        let chat_options = self.create_chat_options();
        
        let response = self.client
            .exec_chat(&self.config.model, chat_req, Some(chat_options))
            .await
            .map_err(|e| GgenAiError::llm_provider("GenAI", &format!("Request failed: {}", e)))?;

        Ok(LlmResponse {
            content: response.first_text().unwrap_or_default(),
            usage: response.usage().map(|u| UsageStats {
                prompt_tokens: u.prompt_tokens.unwrap_or(0),
                completion_tokens: u.completion_tokens.unwrap_or(0),
                total_tokens: u.total_tokens.unwrap_or(0),
            }),
            model: self.config.model.clone(),
            finish_reason: Some("stop".to_string()),
            extra: HashMap::new(),
        })
    }

    /// Stream completion of a prompt
    pub async fn stream_complete(&self, prompt: &str) -> Result<futures::stream::BoxStream<'static, LlmChunk>> {
        let chat_req = ChatRequest::new(vec![
            ChatMessage::user(prompt),
        ]);

        let chat_options = self.create_chat_options();
        
        let stream = self.client
            .exec_chat_stream(&self.config.model, chat_req, Some(chat_options))
            .await
            .map_err(|e| GgenAiError::llm_provider("GenAI", &format!("Stream request failed: {}", e)))?;

        let model = self.config.model.clone();
        let stream = stream.map(move |chunk| {
            match chunk {
                Ok(genai_chunk) => {
                    let content = genai_chunk.first_text().unwrap_or_default();
                    LlmChunk {
                        content,
                        model: model.clone(),
                        finish_reason: if genai_chunk.is_finished() { Some("stop".to_string()) } else { None },
                        usage: genai_chunk.usage().map(|u| UsageStats {
                            prompt_tokens: u.prompt_tokens.unwrap_or(0),
                            completion_tokens: u.completion_tokens.unwrap_or(0),
                            total_tokens: u.total_tokens.unwrap_or(0),
                        }),
                        extra: HashMap::new(),
                    }
                }
                Err(e) => {
                    LlmChunk {
                        content: format!("Error: {}", e),
                        model: model.clone(),
                        finish_reason: Some("error".to_string()),
                        usage: None,
                        extra: HashMap::new(),
                    }
                }
            }
        });

        Ok(Box::pin(stream))
    }

    /// Create chat options from config
    fn create_chat_options(&self) -> ChatOptions {
        ChatOptions::default()
            .with_temperature(self.config.temperature.unwrap_or(0.7))
            .with_max_tokens(self.config.max_tokens.unwrap_or(4096))
            .with_top_p(self.config.top_p.unwrap_or(0.9))
    }

    /// Get the current configuration
    pub fn config(&self) -> &LlmConfig {
        &self.config
    }

    /// Update the configuration
    pub fn update_config(&mut self, config: LlmConfig) {
        self.config = config;
    }
}


#[cfg(test)]
mod tests {
    use super::*;
    
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

    #[test]
    fn test_llm_client_creation() {
        let config = LlmConfig::default();
        let client = LlmClient::new(config);
        assert!(client.is_ok());
    }
}

