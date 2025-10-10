//! Anthropic provider implementation
//!
//! Implements the LlmProvider trait for Anthropic's Claude API with support for
//! streaming and non-streaming completions.

use async_trait::async_trait;
use futures::{Stream, StreamExt};
use reqwest::Client;
use serde::{Deserialize, Serialize};
use std::pin::Pin;
use std::time::Duration;

use super::config::ProviderConfig;
use super::error::{LlmError, LlmResult};
use super::provider::LlmProvider;
use super::streaming::StreamChunk;
use super::types::{ChatRequest, ChatResponse, Role, TokenUsage};

const ANTHROPIC_API_BASE: &str = "https://api.anthropic.com/v1";
const ANTHROPIC_VERSION: &str = "2023-06-01";

/// Anthropic provider implementation
pub struct AnthropicProvider {
    client: Client,
    config: ProviderConfig,
    base_url: String,
}

impl AnthropicProvider {
    /// Create a new Anthropic provider with API key
    pub fn new(api_key: impl Into<String>) -> Self {
        Self::with_config(ProviderConfig::new(api_key))
    }

    /// Create with full configuration
    pub fn with_config(config: ProviderConfig) -> Self {
        let base_url = config
            .endpoint
            .clone()
            .unwrap_or_else(|| ANTHROPIC_API_BASE.to_string());

        let client = Client::builder()
            .timeout(Duration::from_secs(config.timeout_secs))
            .build()
            .expect("Failed to create HTTP client");

        Self {
            client,
            config,
            base_url,
        }
    }

    /// Build request headers
    fn build_headers(&self) -> reqwest::header::HeaderMap {
        let mut headers = reqwest::header::HeaderMap::new();
        headers.insert("x-api-key", self.config.api_key.parse().unwrap());
        headers.insert("anthropic-version", ANTHROPIC_VERSION.parse().unwrap());
        headers.insert("Content-Type", "application/json".parse().unwrap());

        // Add custom headers
        for (key, value) in &self.config.headers {
            if let (Ok(k), Ok(v)) = (
                reqwest::header::HeaderName::try_from(key),
                reqwest::header::HeaderValue::try_from(value),
            ) {
                headers.insert(k, v);
            }
        }

        headers
    }

    /// Convert ChatRequest to Anthropic's format
    fn to_anthropic_request(&self, request: &ChatRequest) -> AnthropicRequest {
        // Separate system message from other messages
        let mut system = None;
        let mut messages = Vec::new();

        for msg in &request.messages {
            match msg.role {
                Role::System => {
                    system = Some(msg.content.clone());
                }
                _ => messages.push(AnthropicMessage {
                    role: match msg.role {
                        Role::User => "user".to_string(),
                        Role::Assistant => "assistant".to_string(),
                        Role::System => "user".to_string(), // Fallback
                    },
                    content: msg.content.clone(),
                }),
            }
        }

        AnthropicRequest {
            model: request.model.clone(),
            messages,
            system,
            max_tokens: request.max_tokens.unwrap_or(4096),
            temperature: request.temperature,
            top_p: request.top_p,
            stream: Some(request.stream),
        }
    }
}

#[async_trait]
impl LlmProvider for AnthropicProvider {
    fn name(&self) -> &str {
        "anthropic"
    }

    fn supported_models(&self) -> Vec<String> {
        vec![
            "claude-3-5-sonnet-20241022".to_string(),
            "claude-3-5-haiku-20241022".to_string(),
            "claude-3-opus-20240229".to_string(),
            "claude-3-sonnet-20240229".to_string(),
            "claude-3-haiku-20240307".to_string(),
        ]
    }

    fn default_model(&self) -> &str {
        self.config
            .default_model
            .as_deref()
            .unwrap_or("claude-3-5-sonnet-20241022")
    }

    async fn chat(&self, request: ChatRequest) -> LlmResult<ChatResponse> {
        let anthropic_request = self.to_anthropic_request(&request);

        let url = format!("{}/messages", self.base_url);
        let response = self
            .client
            .post(&url)
            .headers(self.build_headers())
            .json(&anthropic_request)
            .send()
            .await?;

        if !response.status().is_success() {
            let status = response.status().as_u16();
            let error_text = response.text().await.unwrap_or_default();
            return Err(LlmError::ApiError {
                provider: "anthropic".to_string(),
                status_code: status,
                message: error_text,
            });
        }

        let anthropic_response: AnthropicResponse = response.json().await?;
        Ok(anthropic_response.into())
    }

    async fn chat_stream(
        &self,
        request: ChatRequest,
    ) -> LlmResult<Pin<Box<dyn Stream<Item = LlmResult<StreamChunk>> + Send>>> {
        let mut anthropic_request = self.to_anthropic_request(&request);
        anthropic_request.stream = Some(true);

        let url = format!("{}/messages", self.base_url);
        let response = self
            .client
            .post(&url)
            .headers(self.build_headers())
            .json(&anthropic_request)
            .send()
            .await?;

        if !response.status().is_success() {
            let status = response.status().as_u16();
            let error_text = response.text().await.unwrap_or_default();
            return Err(LlmError::ApiError {
                provider: "anthropic".to_string(),
                status_code: status,
                message: error_text,
            });
        }

        let stream = response.bytes_stream().map(|result| {
            result
                .map_err(|e| LlmError::StreamError {
                    message: e.to_string(),
                })
                .and_then(parse_anthropic_sse)
        });

        Ok(Box::pin(stream))
    }

    async fn validate(&self) -> LlmResult<()> {
        self.config.validate()?;

        // Anthropic doesn't have a dedicated validation endpoint
        // We'll just validate the config structure
        if self.config.api_key.is_empty() {
            return Err(LlmError::InvalidApiKey {
                provider: "anthropic".to_string(),
            });
        }

        Ok(())
    }
}

// Anthropic API structures
#[derive(Debug, Serialize)]
struct AnthropicRequest {
    model: String,
    messages: Vec<AnthropicMessage>,
    #[serde(skip_serializing_if = "Option::is_none")]
    system: Option<String>,
    max_tokens: u32,
    #[serde(skip_serializing_if = "Option::is_none")]
    temperature: Option<f32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    top_p: Option<f32>,
    #[serde(skip_serializing_if = "Option::is_none")]
    stream: Option<bool>,
}

#[derive(Debug, Serialize, Deserialize)]
struct AnthropicMessage {
    role: String,
    content: String,
}

#[derive(Debug, Deserialize)]
struct AnthropicResponse {
    content: Vec<ContentBlock>,
    model: String,
    usage: AnthropicUsage,
    stop_reason: Option<String>,
}

#[derive(Debug, Deserialize)]
struct ContentBlock {
    text: String,
}

#[derive(Debug, Deserialize)]
struct AnthropicUsage {
    input_tokens: u32,
    output_tokens: u32,
}

impl From<AnthropicResponse> for ChatResponse {
    fn from(response: AnthropicResponse) -> Self {
        let content = response
            .content
            .first()
            .map(|c| c.text.clone())
            .unwrap_or_default();

        let usage = TokenUsage {
            prompt_tokens: response.usage.input_tokens,
            completion_tokens: response.usage.output_tokens,
            total_tokens: response.usage.input_tokens + response.usage.output_tokens,
        };

        ChatResponse {
            content,
            model: response.model,
            usage: Some(usage),
            finish_reason: response.stop_reason,
        }
    }
}

/// Parse Server-Sent Events from Anthropic stream
fn parse_anthropic_sse(bytes: bytes::Bytes) -> LlmResult<StreamChunk> {
    let text = String::from_utf8_lossy(&bytes);

    for line in text.lines() {
        if let Some(data) = line.strip_prefix("data: ") {

            let event: serde_json::Value = serde_json::from_str(data)?;

            match event["type"].as_str() {
                Some("content_block_delta") => {
                    if let Some(delta) = event["delta"]["text"].as_str() {
                        return Ok(StreamChunk::new(delta));
                    }
                }
                Some("message_stop") => {
                    return Ok(StreamChunk::final_chunk("", "end_turn"));
                }
                _ => {}
            }
        }
    }

    Ok(StreamChunk::new(""))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_provider_creation() {
        let provider = AnthropicProvider::new("test-key");
        assert_eq!(provider.name(), "anthropic");
        assert!(provider.supports_model("claude-3-5-sonnet-20241022"));
    }

    #[test]
    fn test_default_model() {
        let provider = AnthropicProvider::new("test-key");
        assert_eq!(provider.default_model(), "claude-3-5-sonnet-20241022");
    }

    #[test]
    fn test_request_conversion() {
        let provider = AnthropicProvider::new("key");
        let request = ChatRequest::builder()
            .model("claude-3-opus")
            .message(Role::System, "You are helpful")
            .message(Role::User, "Hello")
            .build()
            .unwrap();

        let anthropic_req = provider.to_anthropic_request(&request);
        assert_eq!(anthropic_req.system, Some("You are helpful".to_string()));
        assert_eq!(anthropic_req.messages.len(), 1);
    }
}
