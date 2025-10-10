//! Anthropic provider implementation

use async_trait::async_trait;
use futures::StreamExt;
use reqwest::Client;
use serde::{Deserialize, Serialize};

use crate::client::{LlmClient, LlmConfig, LlmResponse, LlmChunk, UsageStats};
use crate::config::AnthropicConfig;
use crate::error::{GgenAiError, Result};

/// Anthropic API client
/// Client for interacting with Anthropic API
#[derive(Debug)]
pub struct AnthropicClient {
    client: Client,
    api_key: String,
    base_url: String,
}

impl AnthropicClient {
    /// Create a new Anthropic client from configuration
    pub fn new(config: AnthropicConfig) -> Result<Self> {
        config.validate()?;
        Ok(Self {
            client: Client::new(),
            api_key: config.api_key,
            base_url: config.base_url,
        })
    }

    /// Create a new Anthropic client from environment variables
    pub fn from_env() -> Result<Self> {
        let config = AnthropicConfig::from_env()?;
        Self::new(config)
    }

    /// Create a new Anthropic client with API key (deprecated - use new() with config)
    #[deprecated(since = "0.2.4", note = "Use new() with AnthropicConfig instead")]
    pub fn with_api_key(api_key: String) -> Self {
        Self {
            client: Client::new(),
            api_key,
            base_url: "https://api.anthropic.com/v1".to_string(),
        }
    }

    /// Create a new Anthropic client with custom base URL (deprecated - use new() with config)
    #[deprecated(since = "0.2.4", note = "Use new() with AnthropicConfig instead")]
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
#[allow(dead_code)]
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
#[allow(dead_code)]
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

        // Validate configuration
        config.validate()?;
        if let Some(temp) = config.temperature {
            GgenAiError::validate_temperature(temp, "temperature")?;
        }
        if let Some(top_p) = config.top_p {
            GgenAiError::validate_top_p(top_p, "top_p")?;
        }

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
            .await
            .map_err(|e| {
                if e.is_timeout() {
                    GgenAiError::timeout("anthropic", 30)
                } else if e.is_connect() {
                    GgenAiError::network_error(format!("Failed to connect to Anthropic API: {}", e))
                } else {
                    GgenAiError::network_error(e.to_string())
                }
            })?;

        let status = response.status();
        if !status.is_success() {
            let error_text = response.text().await.unwrap_or_default();

            return Err(if status.as_u16() == 429 {
                GgenAiError::rate_limit("anthropic", None)
            } else if status.as_u16() == 401 {
                GgenAiError::invalid_api_key("anthropic", "Invalid or expired API key")
            } else {
                GgenAiError::anthropic(
                    format!("API request failed: {}", error_text),
                    status.as_u16(),
                )
            });
        }

        let message_response: MessageResponse = response.json().await
            .map_err(|e| GgenAiError::parse_error("anthropic", format!("Failed to parse response: {}", e)))?;
        
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

        // Validate configuration
        config.validate()?;
        if let Some(temp) = config.temperature {
            GgenAiError::validate_temperature(temp, "temperature")?;
        }
        if let Some(top_p) = config.top_p {
            GgenAiError::validate_top_p(top_p, "top_p")?;
        }

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
            .await
            .map_err(|e| {
                if e.is_timeout() {
                    GgenAiError::timeout("anthropic", 30)
                } else if e.is_connect() {
                    GgenAiError::network_error(format!("Failed to connect to Anthropic API: {}", e))
                } else {
                    GgenAiError::network_error(e.to_string())
                }
            })?;

        let status = response.status();
        if !status.is_success() {
            let error_text = response.text().await.unwrap_or_default();

            return Err(if status.as_u16() == 429 {
                GgenAiError::rate_limit("anthropic", None)
            } else if status.as_u16() == 401 {
                GgenAiError::invalid_api_key("anthropic", "Invalid or expired API key")
            } else {
                GgenAiError::anthropic(
                    format!("Streaming request failed: {}", error_text),
                    status.as_u16(),
                )
            });
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
        Err(GgenAiError::llm_provider("Anthropic", "Anthropic does not provide embeddings API"))
    }
    
    fn provider_name(&self) -> &str {
        "anthropic"
    }
    
    fn supported_models(&self) -> Vec<String> {
        vec![
            // Claude 3.5 models (latest)
            "claude-3-5-sonnet-20241022".to_string(),
            "claude-3-5-sonnet-20240620".to_string(),
            "claude-3-5-haiku-20241022".to_string(),
            // Claude 3 models
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
        let config = AnthropicConfig::new("test-key");
        let client = AnthropicClient::new(config).unwrap();
        assert_eq!(client.provider_name(), "anthropic");
        assert!(client.supports_model("claude-3-sonnet-20240229"));
        assert!(!client.supports_model("unknown-model"));
    }

    #[tokio::test]
    async fn test_anthropic_client_with_base_url() {
        let config = AnthropicConfig::new("test-key")
            .with_base_url("https://custom.anthropic.com");
        let client = AnthropicClient::new(config).unwrap();
        assert_eq!(client.base_url, "https://custom.anthropic.com");
    }
}
