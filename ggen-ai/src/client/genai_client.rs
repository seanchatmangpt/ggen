//! GenAI multi-provider client wrapper

use async_trait::async_trait;
use futures::stream::BoxStream;
use futures::StreamExt;
use genai::{Client, ChatRequest, ChatMessage, ChatOptions};
use serde_json::Value;
use std::collections::HashMap;

use crate::client::{LlmClient, LlmConfig, LlmResponse, LlmChunk, UsageStats};
use crate::error::{GgenAiError, Result};

/// GenAI multi-provider client wrapper
#[derive(Debug)]
pub struct GenAiClient {
    client: Client,
    model: String,
    config: LlmConfig,
}

impl GenAiClient {
    /// Create a new GenAI client with specified model and config
    pub fn new(model: String, config: LlmConfig) -> Result<Self> {
        Ok(Self {
            client: Client::default(),
            model,
            config,
        })
    }

    /// Create a new GenAI client with default config
    pub fn with_model(model: String) -> Result<Self> {
        let config = LlmConfig {
            model: model.clone(),
            max_tokens: Some(4096),
            temperature: Some(0.7),
            top_p: Some(0.9),
            stop: Some(vec!["```".to_string()]),
            extra: HashMap::new(),
        };
        Self::new(model, config)
    }

    /// Get the current model name
    pub fn model(&self) -> &str {
        &self.model
    }

    /// Get the current config
    pub fn config(&self) -> &LlmConfig {
        &self.config
    }

    /// Update the config
    pub fn update_config(&mut self, config: LlmConfig) {
        self.config = config;
    }

    /// Create chat options from config
    fn create_chat_options(&self) -> ChatOptions {
        ChatOptions::default()
            .with_temperature(self.config.temperature.unwrap_or(0.7))
            .with_max_tokens(self.config.max_tokens.unwrap_or(4096))
            .with_top_p(self.config.top_p.unwrap_or(0.9))
    }

    /// Convert genai response to our LlmResponse format
    fn convert_response(&self, response: genai::ChatResponse) -> LlmResponse {
        let usage = response.usage().map(|u| UsageStats {
            prompt_tokens: u.prompt_tokens.unwrap_or(0),
            completion_tokens: u.completion_tokens.unwrap_or(0),
            total_tokens: u.total_tokens.unwrap_or(0),
        });

        LlmResponse {
            content: response.first_text().unwrap_or_default(),
            usage,
            model: self.model.clone(),
            finish_reason: Some("stop".to_string()),
            extra: HashMap::new(),
        }
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
            .exec_chat(&self.model, chat_req, Some(chat_options))
            .await
            .map_err(|e| GgenAiError::llm_client(format!("GenAI request failed: {}", e)))?;

        Ok(self.convert_response(response))
    }

    async fn stream_complete(&self, prompt: &str) -> Result<BoxStream<LlmChunk>> {
        let chat_req = ChatRequest::new(vec![
            ChatMessage::user(prompt),
        ]);

        let chat_options = self.create_chat_options();
        
        let stream = self.client
            .exec_chat_stream(&self.model, chat_req, Some(chat_options))
            .await
            .map_err(|e| GgenAiError::llm_client(format!("GenAI stream request failed: {}", e)))?;

        let model = self.model.clone();
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

    #[tokio::test]
    async fn test_genai_client_creation() {
        let client = GenAiClient::with_model("gpt-3.5-turbo".to_string());
        assert!(client.is_ok());
        
        let client = client.unwrap();
        assert_eq!(client.model(), "gpt-3.5-turbo");
    }

    #[tokio::test]
    async fn test_config_update() {
        let mut client = GenAiClient::with_model("gpt-3.5-turbo".to_string()).unwrap();
        
        let new_config = LlmConfig {
            model: "gpt-4".to_string(),
            max_tokens: Some(2048),
            temperature: Some(0.5),
            top_p: Some(0.8),
            stop: Some(vec!["STOP".to_string()]),
            extra: HashMap::new(),
        };
        
        client.update_config(new_config);
        assert_eq!(client.config().model, "gpt-4");
        assert_eq!(client.config().max_tokens, Some(2048));
        assert_eq!(client.config().temperature, Some(0.5));
    }
}
