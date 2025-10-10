//! Anthropic provider implementation

use async_trait::async_trait;
use futures::StreamExt;
use reqwest::Client;
use serde::{Deserialize, Serialize};

use crate::client::{LlmClient, LlmConfig, LlmResponse, LlmChunk, UsageStats};
use crate::error::{GgenAiError, Result};

/// Anthropic API client
pub struct AnthropicClient {
    client: Client,
    api_key: String,
    base_url: String,
}

impl AnthropicClient {
    /// Create a new Anthropic client
    pub fn new(api_key: String) -> Self {
        Self {
            client: Client::new(),
            api_key,
            base_url: "https://api.anthropic.com/v1".to_string(),
        }
    }
    
    /// Create a new Anthropic client with custom base URL
    pub fn with_base_url(api_key: String, base_url: String) -> Self {
        Self {
            client: Client::new(),
            api_key,
            base_url,
        }
    }
}

#[derive(Serialize)]
struct MessageRequest {
    model: String,
    max_tokens: u32,
    messages: Vec<Message>,
    temperature: Option<f32>,
    top_p: Option<f32>,
    stop_sequences: Option<Vec<String>>,
    stream: bool,
}

#[derive(Serialize)]
struct Message {
    role: String,
    content: String,
}

#[derive(Deserialize)]
struct MessageResponse {
    id: String,
    #[serde(rename = "type")]
    response_type: String,
    role: String,
    content: Vec<ContentBlock>,
    model: String,
    stop_reason: Option<String>,
    stop_sequence: Option<String>,
    usage: Usage,
}

#[derive(Deserialize)]
struct ContentBlock {
    #[serde(rename = "type")]
    block_type: String,
    text: String,
}

#[derive(Deserialize)]
struct Usage {
    input_tokens: u32,
    output_tokens: u32,
}

#[async_trait]
impl LlmClient for AnthropicClient {
    async fn complete(&self, prompt: &str, config: Option<LlmConfig>) -> Result<LlmResponse> {
        let config = config.unwrap_or_default();
        
        let request = MessageRequest {
            model: config.model,
            max_tokens: config.max_tokens.unwrap_or(2048),
            messages: vec![Message {
                role: "user".to_string(),
                content: prompt.to_string(),
            }],
            temperature: config.temperature,
            top_p: config.top_p,
            stop_sequences: config.stop,
            stream: false,
        };
        
        let response = self
            .client
            .post(&format!("{}/messages", self.base_url))
            .header("x-api-key", &self.api_key)
            .header("anthropic-version", "2023-06-01")
            .header("Content-Type", "application/json")
            .json(&request)
            .send()
            .await?;
        
        if !response.status().is_success() {
            let error_text = response.text().await.unwrap_or_default();
            return Err(GgenAiError::llm_provider(format!(
                "Anthropic API error: {}",
                error_text
            )));
        }
        
        let message_response: MessageResponse = response.json().await?;
        
        let content = message_response
            .content
            .into_iter()
            .map(|block| block.text)
            .collect::<Vec<_>>()
            .join("");
        
        let usage = UsageStats {
            prompt_tokens: message_response.usage.input_tokens,
            completion_tokens: message_response.usage.output_tokens,
            total_tokens: message_response.usage.input_tokens + message_response.usage.output_tokens,
        };
        
        Ok(LlmResponse {
            content,
            usage: Some(usage),
            model: message_response.model,
            finish_reason: message_response.stop_reason,
        })
    }
    
    async fn stream_complete(
        &self,
        prompt: &str,
        config: Option<LlmConfig>,
    ) -> Result<futures::stream::BoxStream<'static, Result<LlmChunk>>> {
        let config = config.unwrap_or_default();
        
        let request = MessageRequest {
            model: config.model,
            max_tokens: config.max_tokens.unwrap_or(2048),
            messages: vec![Message {
                role: "user".to_string(),
                content: prompt.to_string(),
            }],
            temperature: config.temperature,
            top_p: config.top_p,
            stop_sequences: config.stop,
            stream: true,
        };
        
        let response = self
            .client
            .post(&format!("{}/messages", self.base_url))
            .header("x-api-key", &self.api_key)
            .header("anthropic-version", "2023-06-01")
            .header("Content-Type", "application/json")
            .json(&request)
            .send()
            .await?;
        
        if !response.status().is_success() {
            let error_text = response.text().await.unwrap_or_default();
            return Err(GgenAiError::llm_provider(format!(
                "Anthropic API error: {}",
                error_text
            )));
        }
        
        let stream = response
            .bytes_stream()
            .map(|chunk_result| {
                let chunk = chunk_result?;
                let text = String::from_utf8(chunk.to_vec())?;
                
                // Parse SSE format
                for line in text.lines() {
                    if line.starts_with("data: ") {
                        let data = &line[6..];
                        if data == "[DONE]" {
                            return Ok(LlmChunk {
                                content: String::new(),
                                done: true,
                                usage: None,
                            });
                        }
                        
                        if let Ok(stream_response) = serde_json::from_str::<serde_json::Value>(data) {
                            if let Some(delta) = stream_response.get("delta") {
                                if let Some(text) = delta.get("text").and_then(|t| t.as_str()) {
                                    return Ok(LlmChunk {
                                        content: text.to_string(),
                                        done: false,
                                        usage: None,
                                    });
                                }
                            }
                        }
                    }
                }
                
                Ok(LlmChunk {
                    content: String::new(),
                    done: false,
                    usage: None,
                })
            });
        
        Ok(Box::pin(stream))
    }
    
    async fn embed(&self, _text: &str) -> Result<Vec<f32>> {
        // Anthropic doesn't provide embeddings API
        Err(GgenAiError::llm_provider(
            "Anthropic does not provide embeddings API"
        ))
    }
    
    fn provider_name(&self) -> &str {
        "anthropic"
    }
    
    fn supported_models(&self) -> Vec<String> {
        vec![
            "claude-3-opus-20240229".to_string(),
            "claude-3-sonnet-20240229".to_string(),
            "claude-3-haiku-20240307".to_string(),
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_anthropic_client_creation() {
        let client = AnthropicClient::new("test-key".to_string());
        assert_eq!(client.provider_name(), "anthropic");
        assert!(client.supports_model("claude-3-sonnet-20240229"));
        assert!(!client.supports_model("unknown-model"));
    }
    
    #[tokio::test]
    async fn test_anthropic_client_with_base_url() {
        let client = AnthropicClient::with_base_url(
            "test-key".to_string(),
            "https://custom.anthropic.com".to_string(),
        );
        assert_eq!(client.base_url, "https://custom.anthropic.com");
    }
}
