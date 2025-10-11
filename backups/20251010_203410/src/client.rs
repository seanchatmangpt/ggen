//! Simplified client interface using rust-genai

use genai::{Client, chat::{ChatRequest, ChatMessage, ChatOptions}};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use futures::stream::BoxStream;
use futures::StreamExt;
use async_trait::async_trait;

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
    /// Additional configuration
    pub extra: HashMap<String, serde_json::Value>,
}

impl Default for LlmConfig {
    fn default() -> Self {
        Self {
            model: "gpt-3.5-turbo".to_string(),
            max_tokens: Some(4096),
            temperature: Some(0.7),
            top_p: Some(0.9),
            stop: None,
            extra: HashMap::new(),
        }
    }
}

impl LlmConfig {
    /// Validate the configuration
    pub fn validate(&self) -> Result<()> {
        if self.model.is_empty() {
            return Err(GgenAiError::configuration("Model name cannot be empty"));
        }
        
        if let Some(max_tokens) = self.max_tokens {
            if max_tokens == 0 || max_tokens >= 200000 {
                return Err(GgenAiError::configuration("Max tokens must be between 1 and 199999"));
            }
        }
        
        if let Some(temperature) = self.temperature {
            if temperature < 0.0 || temperature > 2.0 {
                return Err(GgenAiError::configuration("Temperature must be between 0.0 and 2.0"));
            }
        }
        
        if let Some(top_p) = self.top_p {
            if top_p < 0.0 || top_p > 1.0 {
                return Err(GgenAiError::configuration("Top-p must be between 0.0 and 1.0"));
            }
        }
        
        Ok(())
    }
}

/// Usage statistics for LLM requests
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UsageStats {
    /// Number of prompt tokens
    pub prompt_tokens: u32,
    /// Number of completion tokens
    pub completion_tokens: u32,
    /// Total number of tokens
    pub total_tokens: u32,
}

/// LLM response structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct LlmResponse {
    /// Generated content
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

/// Trait for LLM clients
#[async_trait]
pub trait LlmClient: Send + Sync + std::fmt::Debug {
    /// Complete a prompt synchronously
    async fn complete(&self, prompt: &str) -> Result<LlmResponse>;
    
    /// Stream completion of a prompt
    async fn complete_stream(&self, prompt: &str) -> Result<BoxStream<'static, LlmChunk>>;
    
    /// Get the current configuration
    fn get_config(&self) -> &LlmConfig;
    
    /// Update the configuration
    fn update_config(&mut self, config: LlmConfig);
}

/// Simplified LLM client using rust-genai
#[derive(Debug, Clone)]
pub struct GenAiClient {
    client: Client,
    config: LlmConfig,
}

impl GenAiClient {
    /// Create a new LLM client with configuration
    pub fn new(config: LlmConfig) -> Result<Self> {
        config.validate()?;
        Ok(Self {
            client: Client::default(),
            config,
        })
    }


    /// Create chat options from config
    fn create_chat_options(&self) -> ChatOptions {
        ChatOptions::default()
            .with_temperature(self.config.temperature.unwrap_or(0.7) as f64)
            .with_max_tokens(self.config.max_tokens.unwrap_or(4096) as u32)
            .with_top_p(self.config.top_p.unwrap_or(0.9) as f64)
    }
}

#[async_trait]
impl LlmClient for GenAiClient {
    async fn complete(&self, prompt: &str) -> Result<LlmResponse> {
        let chat_req = ChatRequest::new(vec![
            ChatMessage::user(prompt),
        ]);

        let chat_options = self.create_chat_options();
        
        let response = self.client
            .exec_chat(&self.config.model, chat_req, Some(&chat_options))
            .await
            .map_err(|e| GgenAiError::llm_provider("GenAI", &format!("Request failed: {}", e)))?;

        Ok(LlmResponse {
            content: response.content_text_as_str().unwrap_or_default().to_string(),
            usage: Some(UsageStats {
                prompt_tokens: response.usage.prompt_tokens.unwrap_or(0) as u32,
                completion_tokens: response.usage.completion_tokens.unwrap_or(0) as u32,
                total_tokens: response.usage.total_tokens.unwrap_or(0) as u32,
            }),
            model: self.config.model.clone(),
            finish_reason: Some("stop".to_string()),
            extra: HashMap::new(),
        })
    }

    async fn complete_stream(&self, prompt: &str) -> Result<BoxStream<'static, LlmChunk>> {
        let chat_req = ChatRequest::new(vec![
            ChatMessage::user(prompt),
        ]);

        let chat_options = self.create_chat_options();
        
        let stream = self.client
            .exec_chat_stream(&self.config.model, chat_req, Some(&chat_options))
            .await
            .map_err(|e| GgenAiError::llm_provider("GenAI", &format!("Stream request failed: {}", e)))?;

        let model = self.config.model.clone();
        let stream = stream.stream.map(move |chunk_result| {
            match chunk_result {
                Ok(event) => {
                    match event {
                        genai::chat::ChatStreamEvent::Chunk(chunk) => {
                            LlmChunk {
                                content: chunk.content,
                                model: model.clone(),
                                finish_reason: None,
                                usage: None,
                                extra: HashMap::new(),
                            }
                        }
                        genai::chat::ChatStreamEvent::ReasoningChunk(chunk) => {
                            LlmChunk {
                                content: chunk.content,
                                model: model.clone(),
                                finish_reason: None,
                                usage: None,
                                extra: HashMap::new(),
                            }
                        }
                        genai::chat::ChatStreamEvent::End(end) => {
                            LlmChunk {
                                content: String::new(),
                                model: model.clone(),
                                finish_reason: Some("stop".to_string()),
                                usage: end.captured_usage.map(|u| UsageStats {
                                    prompt_tokens: u.prompt_tokens.unwrap_or(0) as u32,
                                    completion_tokens: u.completion_tokens.unwrap_or(0) as u32,
                                    total_tokens: u.total_tokens.unwrap_or(0) as u32,
                                }),
                                extra: HashMap::new(),
                            }
                        }
                        genai::chat::ChatStreamEvent::Start => {
                            LlmChunk {
                                content: String::new(),
                                model: model.clone(),
                                finish_reason: None,
                                usage: None,
                                extra: HashMap::new(),
                            }
                        }
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

    fn get_config(&self) -> &LlmConfig {
        &self.config
    }

    fn update_config(&mut self, config: LlmConfig) {
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
    fn test_genai_client_creation() {
        use crate::client::GenAiClient;
        let config = LlmConfig::default();
        let client = GenAiClient::new(config);
        assert!(client.is_ok());
    }
}