//! Provider adapter utilities using genai

use crate::client::{LlmClient, LlmConfig, LlmResponse, LlmChunk};
use crate::error::{GgenAiError, Result};
use async_trait::async_trait;
use futures::stream::BoxStream;
use genai::{Client, chat::{ChatRequest, ChatMessage, ChatOptions}};
use std::collections::HashMap;

/// Mock client for testing
#[derive(Debug)]
pub struct MockClient {
    responses: Vec<String>,
    current_index: usize,
    config: LlmConfig,
}

impl MockClient {
    /// Create a new mock client
    pub fn new(responses: Vec<String>) -> Self {
        Self {
            responses,
            current_index: 0,
            config: LlmConfig::default(),
        }
    }
    
    /// Create a mock client with a single response
    pub fn with_response(response: &str) -> Self {
        Self::new(vec![response.to_string()])
    }
}

#[async_trait]
impl LlmClient for MockClient {
    async fn complete(&self, prompt: &str) -> Result<LlmResponse> {
        let response = self.responses.get(self.current_index)
            .ok_or_else(|| GgenAiError::llm_provider("MockClient", "No more mock responses"))?;
        
        Ok(LlmResponse {
            content: response.clone(),
            usage: Some(crate::client::UsageStats {
                prompt_tokens: prompt.len() as u32 / 4, // Rough estimate
                completion_tokens: response.len() as u32 / 4,
                total_tokens: (prompt.len() + response.len()) as u32 / 4,
            }),
            model: "mock-model".to_string(),
            finish_reason: Some("stop".to_string()),
            extra: std::collections::HashMap::new(),
        })
    }
    
    async fn stream_complete(&self, prompt: &str) -> Result<BoxStream<LlmChunk>> {
        let response = self.responses.get(self.current_index)
            .ok_or_else(|| GgenAiError::llm_provider("MockClient", "No more mock responses"))?;
        
        // Split response into chunks for streaming
        let chunks: Vec<LlmChunk> = response
            .chars()
            .collect::<Vec<_>>()
            .chunks(10)
            .map(|chunk| LlmChunk {
                content: chunk.iter().collect(),
                model: "mock-model".to_string(),
                finish_reason: None,
                usage: None,
                extra: std::collections::HashMap::new(),
            })
            .collect();
        
        let mut final_chunks = chunks;
        if let Some(last) = final_chunks.last_mut() {
            last.finish_reason = Some("stop".to_string());
            last.usage = Some(crate::client::UsageStats {
                prompt_tokens: prompt.len() as u32 / 4,
                completion_tokens: response.len() as u32 / 4,
                total_tokens: (prompt.len() + response.len()) as u32 / 4,
            });
        }
        
        let stream = futures::stream::iter(final_chunks.into_iter());
        Ok(Box::pin(stream))
    }
    
    fn get_config(&self) -> &LlmConfig {
        &self.config
    }
    
    fn update_config(&mut self, config: LlmConfig) {
        self.config = config;
    }
}

/// GenAI client wrapper for production use
#[derive(Debug)]
pub struct GenAiClient {
    client: Client,
    config: LlmConfig,
}

impl GenAiClient {
    /// Create a new GenAI client with configuration
    pub fn new(config: LlmConfig) -> Result<Self> {
        config.validate()?;
        Ok(Self {
            client: Client::default(),
            config,
        })
    }
}

#[async_trait]
impl LlmClient for GenAiClient {
    async fn complete(&self, prompt: &str) -> Result<LlmResponse> {
        let chat_req = ChatRequest::new(vec![
            ChatMessage::user(prompt),
        ]);

        let chat_options = ChatOptions::default()
            .with_temperature(self.config.temperature.unwrap_or(0.7) as f64)
            .with_max_tokens(self.config.max_tokens.unwrap_or(4096) as u32)
            .with_top_p(self.config.top_p.unwrap_or(0.9) as f64);
        
        let response = self.client
            .exec_chat(&self.config.model, chat_req, Some(&chat_options))
            .await
            .map_err(|e| GgenAiError::llm_provider("GenAI", &format!("Request failed: {}", e)))?;

        Ok(LlmResponse {
            content: response.first_text().unwrap_or_default(),
            usage: response.usage().map(|u| crate::client::UsageStats {
                prompt_tokens: u.prompt_tokens.unwrap_or(0),
                completion_tokens: u.completion_tokens.unwrap_or(0),
                total_tokens: u.total_tokens.unwrap_or(0),
            }),
            model: self.config.model.clone(),
            finish_reason: Some("stop".to_string()),
            extra: HashMap::new(),
        })
    }
    
    async fn stream_complete(&self, prompt: &str) -> Result<BoxStream<LlmChunk>> {
        let chat_req = ChatRequest::new(vec![
            ChatMessage::user(prompt),
        ]);

        let chat_options = ChatOptions::default()
            .with_temperature(self.config.temperature.unwrap_or(0.7) as f64)
            .with_max_tokens(self.config.max_tokens.unwrap_or(4096) as u32)
            .with_top_p(self.config.top_p.unwrap_or(0.9) as f64);
        
        let stream = self.client
            .exec_chat_stream(&self.config.model, chat_req, Some(&chat_options))
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
                        usage: genai_chunk.usage().map(|u| crate::client::UsageStats {
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
    use futures::StreamExt;
    
    #[tokio::test]
    async fn test_mock_client() {
        let client = MockClient::with_response("Hello, world!");
        let response = client.complete("Test prompt").await
            .expect("Failed to complete mock request");

        assert_eq!(response.content, "Hello, world!");
        assert_eq!(response.model, "mock-model");
        assert!(response.usage.is_some());
    }
    
    #[tokio::test]
    async fn test_mock_client_streaming() {
        let client = MockClient::with_response("Hello, world!");
        let mut stream = client.stream_complete("Test prompt").await
            .expect("Failed to create stream");

        let mut content = String::new();
        while let Some(chunk) = stream.next().await {
            content.push_str(&chunk.content);
            if chunk.finish_reason.is_some() {
                break;
            }
        }
        
        assert_eq!(content, "Hello, world!");
    }
}

