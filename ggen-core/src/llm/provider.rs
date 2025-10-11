//! Core LLM provider trait
//!
//! Defines the unified interface that all LLM providers must implement.
//! This follows the adapter pattern from rust-genai for consistent multi-provider support.

use async_trait::async_trait;
use futures::Stream;
use std::pin::Pin;

use super::error::LlmResult;
use super::streaming::StreamChunk;
use super::types::{ChatRequest, ChatResponse};

/// Unified interface for LLM providers
///
/// All provider implementations (OpenAI, Anthropic, etc.) must implement this trait
/// to ensure consistent behavior across different backends.
#[async_trait]
pub trait LlmProvider: Send + Sync {
    /// Get the provider name (e.g., "openai", "anthropic")
    fn name(&self) -> &str;

    /// Get supported models for this provider
    fn supported_models(&self) -> Vec<String>;

    /// Check if a model is supported
    fn supports_model(&self, model: &str) -> bool {
        self.supported_models()
            .iter()
            .any(|m| m.eq_ignore_ascii_case(model))
    }

    /// Send a chat completion request (non-streaming)
    ///
    /// # Arguments
    /// * `request` - The chat request with messages and configuration
    ///
    /// # Returns
    /// Complete chat response with all content
    async fn chat(&self, request: ChatRequest) -> LlmResult<ChatResponse>;

    /// Send a chat completion request with streaming
    ///
    /// # Arguments
    /// * `request` - The chat request with messages and configuration
    ///
    /// # Returns
    /// A stream of response chunks that can be processed incrementally
    async fn chat_stream(
        &self, request: ChatRequest,
    ) -> LlmResult<Pin<Box<dyn Stream<Item = LlmResult<StreamChunk>> + Send>>>;

    /// Validate configuration (API key, model availability, etc.)
    ///
    /// # Returns
    /// Ok if configuration is valid, Err otherwise
    async fn validate(&self) -> LlmResult<()>;

    /// Get the default model for this provider
    fn default_model(&self) -> &str;
}

#[cfg(test)]
mod tests {
    use super::*;

    // Mock provider for testing
    struct MockProvider;

    #[async_trait]
    impl LlmProvider for MockProvider {
        fn name(&self) -> &str {
            "mock"
        }

        fn supported_models(&self) -> Vec<String> {
            vec!["model-1".to_string(), "model-2".to_string()]
        }

        async fn chat(&self, _request: ChatRequest) -> LlmResult<ChatResponse> {
            Ok(ChatResponse {
                content: "Mock response".to_string(),
                model: "model-1".to_string(),
                usage: None,
                finish_reason: Some("stop".to_string()),
            })
        }

        async fn chat_stream(
            &self, _request: ChatRequest,
        ) -> LlmResult<Pin<Box<dyn Stream<Item = LlmResult<StreamChunk>> + Send>>> {
            unimplemented!("Streaming not implemented for mock")
        }

        async fn validate(&self) -> LlmResult<()> {
            Ok(())
        }

        fn default_model(&self) -> &str {
            "model-1"
        }
    }

    #[tokio::test]
    async fn test_mock_provider() {
        let provider = MockProvider;
        assert_eq!(provider.name(), "mock");
        assert!(provider.supports_model("model-1"));
        assert!(!provider.supports_model("unknown"));
    }

    #[tokio::test]
    async fn test_provider_chat() {
        let provider = MockProvider;
        let request = ChatRequest::builder()
            .model("model-1")
            .message("user", "test")
            .build()
            .unwrap();

        let response = provider.chat(request).await.unwrap();
        assert_eq!(response.content, "Mock response");
    }
}
