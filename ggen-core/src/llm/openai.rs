//! OpenAI provider implementation
//!
//! Implements the LlmProvider trait for OpenAI's API with support for
//! streaming and non-streaming chat completions.

use async_trait::async_trait;
use futures::{Stream, StreamExt};
use reqwest::Client;
use serde::Deserialize;
use std::pin::Pin;
use std::time::Duration;

use super::config::ProviderConfig;
use super::error::{LlmError, LlmResult};
use super::provider::LlmProvider;
use super::streaming::StreamChunk;
use super::types::{ChatRequest, ChatResponse, Message, TokenUsage};

const OPENAI_API_BASE: &str = "https://api.openai.com/v1";

/// OpenAI provider implementation
pub struct OpenAiProvider {
    client: Client,
    config: ProviderConfig,
    base_url: String,
}

impl OpenAiProvider {
    /// Create a new OpenAI provider with API key
    pub fn new(api_key: impl Into<String>) -> Self {
        Self::with_config(ProviderConfig::new(api_key))
    }

    /// Create with full configuration
    pub fn with_config(config: ProviderConfig) -> Self {
        let base_url = config
            .endpoint
            .clone()
            .unwrap_or_else(|| OPENAI_API_BASE.to_string());

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
        headers.insert(
            "Authorization",
            format!("Bearer {}", self.config.api_key).parse().unwrap(),
        );
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
}

#[async_trait]
impl LlmProvider for OpenAiProvider {
    fn name(&self) -> &str {
        "openai"
    }

    fn supported_models(&self) -> Vec<String> {
        vec![
            "gpt-4".to_string(),
            "gpt-4-turbo".to_string(),
            "gpt-4o".to_string(),
            "gpt-3.5-turbo".to_string(),
            "gpt-4o-mini".to_string(),
        ]
    }

    fn default_model(&self) -> &str {
        self.config
            .default_model
            .as_deref()
            .unwrap_or("gpt-4o-mini")
    }

    async fn chat(&self, mut request: ChatRequest) -> LlmResult<ChatResponse> {
        request.stream = false;

        let url = format!("{}/chat/completions", self.base_url);
        let response = self
            .client
            .post(&url)
            .headers(self.build_headers())
            .json(&request)
            .send()
            .await?;

        if !response.status().is_success() {
            let status = response.status().as_u16();
            let error_text = response.text().await.unwrap_or_default();
            return Err(LlmError::ApiError {
                provider: "openai".to_string(),
                status_code: status,
                message: error_text,
            });
        }

        let openai_response: OpenAiChatResponse = response.json().await?;
        Ok(openai_response.into())
    }

    async fn chat_stream(
        &self, mut request: ChatRequest,
    ) -> LlmResult<Pin<Box<dyn Stream<Item = LlmResult<StreamChunk>> + Send>>> {
        request.stream = true;

        let url = format!("{}/chat/completions", self.base_url);
        let response = self
            .client
            .post(&url)
            .headers(self.build_headers())
            .json(&request)
            .send()
            .await?;

        if !response.status().is_success() {
            let status = response.status().as_u16();
            let error_text = response.text().await.unwrap_or_default();
            return Err(LlmError::ApiError {
                provider: "openai".to_string(),
                status_code: status,
                message: error_text,
            });
        }

        let stream = response.bytes_stream().map(|result| {
            result
                .map_err(|e| LlmError::StreamError {
                    message: e.to_string(),
                })
                .and_then(parse_sse_chunk)
        });

        Ok(Box::pin(stream))
    }

    async fn validate(&self) -> LlmResult<()> {
        self.config.validate()?;

        // Validate API key by making a minimal request
        let url = format!("{}/models", self.base_url);
        let response = self
            .client
            .get(&url)
            .headers(self.build_headers())
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(LlmError::InvalidApiKey {
                provider: "openai".to_string(),
            });
        }

        Ok(())
    }
}

// OpenAI API response structures
#[derive(Debug, Deserialize)]
struct OpenAiChatResponse {
    choices: Vec<OpenAiChoice>,
    model: String,
    usage: Option<OpenAiUsage>,
}

#[derive(Debug, Deserialize)]
struct OpenAiChoice {
    message: Message,
    finish_reason: Option<String>,
}

#[derive(Debug, Deserialize)]
struct OpenAiUsage {
    prompt_tokens: u32,
    completion_tokens: u32,
    total_tokens: u32,
}

impl From<OpenAiChatResponse> for ChatResponse {
    fn from(response: OpenAiChatResponse) -> Self {
        let content = response
            .choices
            .first()
            .map(|c| c.message.content.clone())
            .unwrap_or_default();

        let finish_reason = response
            .choices
            .first()
            .and_then(|c| c.finish_reason.clone());

        let usage = response.usage.map(|u| TokenUsage {
            prompt_tokens: u.prompt_tokens,
            completion_tokens: u.completion_tokens,
            total_tokens: u.total_tokens,
        });

        ChatResponse {
            content,
            model: response.model,
            usage,
            finish_reason,
        }
    }
}

/// Parse Server-Sent Events chunk from OpenAI stream
fn parse_sse_chunk(bytes: bytes::Bytes) -> LlmResult<StreamChunk> {
    let text = String::from_utf8_lossy(&bytes);

    for line in text.lines() {
        if let Some(data) = line.strip_prefix("data: ") {
            if data == "[DONE]" {
                return Ok(StreamChunk::final_chunk("", "stop"));
            }

            let chunk: serde_json::Value = serde_json::from_str(data)?;

            if let Some(delta) = chunk["choices"][0]["delta"]["content"].as_str() {
                return Ok(StreamChunk::new(delta));
            }

            if let Some(reason) = chunk["choices"][0]["finish_reason"].as_str() {
                return Ok(StreamChunk::final_chunk("", reason));
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
        let provider = OpenAiProvider::new("test-key");
        assert_eq!(provider.name(), "openai");
        assert!(provider.supports_model("gpt-4"));
    }

    #[test]
    fn test_default_model() {
        let provider = OpenAiProvider::new("test-key");
        assert_eq!(provider.default_model(), "gpt-4o-mini");
    }

    #[test]
    fn test_custom_config() {
        let config = ProviderConfig::new("key")
            .with_endpoint("https://custom.api")
            .with_default_model("gpt-4");

        let provider = OpenAiProvider::with_config(config);
        assert_eq!(provider.base_url, "https://custom.api");
        assert_eq!(provider.default_model(), "gpt-4");
    }

    #[test]
    fn test_parse_sse_done() {
        let data = b"data: [DONE]\n\n";
        let chunk = parse_sse_chunk(bytes::Bytes::from(data.to_vec())).unwrap();
        assert!(chunk.is_final);
    }
}
