//! Mock LLM Provider Implementation
//!
//! Provides mock implementations for testing without making actual API calls.

use async_trait::async_trait;
use futures::Stream;
use ggen_core::llm::{ChatRequest, ChatResponse, LlmError, LlmProvider, LlmResult, StreamChunk};
use std::pin::Pin;
use std::sync::{Arc, Mutex};

/// Mock provider for testing
pub struct MockLlmProvider {
    pub name: String,
    pub responses: Arc<Mutex<Vec<String>>>,
    pub call_count: Arc<Mutex<usize>>,
    pub should_fail: bool,
    pub api_key: Option<String>,
}

impl MockLlmProvider {
    pub fn new(name: &str) -> Self {
        Self {
            name: name.to_string(),
            responses: Arc::new(Mutex::new(vec![
                "Mock response 1".to_string(),
                "Mock response 2".to_string(),
            ])),
            call_count: Arc::new(Mutex::new(0)),
            should_fail: false,
            api_key: Some("mock-api-key".to_string()),
        }
    }

    pub fn with_responses(mut self, responses: Vec<String>) -> Self {
        self.responses = Arc::new(Mutex::new(responses));
        self
    }

    pub fn with_failure(mut self) -> Self {
        self.should_fail = true;
        self
    }

    pub fn without_api_key(mut self) -> Self {
        self.api_key = None;
        self
    }

    pub fn get_call_count(&self) -> usize {
        *self.call_count.lock().unwrap()
    }
}

#[async_trait]
impl LlmProvider for MockLlmProvider {
    fn name(&self) -> &str {
        &self.name
    }

    fn supported_models(&self) -> Vec<String> {
        vec!["mock-model-1".to_string(), "mock-model-2".to_string()]
    }

    fn default_model(&self) -> &str {
        "mock-model-1"
    }

    async fn chat(&self, request: ChatRequest) -> LlmResult<ChatResponse> {
        if self.should_fail {
            return Err(LlmError::ApiError {
                provider: self.name.clone(),
                message: "Mock provider failure".to_string(),
                status_code: 500,
            });
        }

        let mut count = self.call_count.lock().unwrap();
        *count += 1;

        let responses = self.responses.lock().unwrap();
        let response_idx = (*count - 1) % responses.len();
        let content = responses.get(response_idx).unwrap().clone();

        // Calculate token usage based on content length
        let prompt_tokens: u32 = request
            .messages
            .iter()
            .map(|m| (m.content.len() / 4) as u32)
            .sum();
        let completion_tokens = (content.len() / 4) as u32;

        Ok(ChatResponse {
            content,
            model: request.model,
            usage: Some(ggen_core::llm::types::TokenUsage {
                prompt_tokens,
                completion_tokens,
                total_tokens: prompt_tokens + completion_tokens,
            }),
            finish_reason: Some("stop".to_string()),
        })
    }

    async fn chat_stream(
        &self, _request: ChatRequest,
    ) -> LlmResult<Pin<Box<dyn Stream<Item = LlmResult<StreamChunk>> + Send>>> {
        if self.should_fail {
            return Err(LlmError::ApiError {
                provider: self.name.clone(),
                message: "Mock provider stream failure".to_string(),
                status_code: 500,
            });
        }

        // For testing, return empty stream
        let stream = futures::stream::empty();
        Ok(Box::pin(stream))
    }

    async fn validate(&self) -> LlmResult<()> {
        if self.api_key.is_none() {
            return Err(LlmError::InvalidApiKey {
                provider: self.name.clone(),
            });
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use ggen_core::llm::Role;

    #[tokio::test]
    async fn test_mock_provider_creation() {
        let provider = MockLlmProvider::new("test-provider");
        assert_eq!(provider.name(), "test-provider");
        assert!(!provider.should_fail);
    }

    #[tokio::test]
    async fn test_mock_provider_chat() {
        let provider = MockLlmProvider::new("test");
        let request = ChatRequest::builder()
            .model("mock-model-1")
            .message(Role::User, "Hello")
            .build()
            .unwrap();

        let response = provider.chat(request).await.unwrap();
        assert_eq!(response.content, "Mock response 1");
        assert_eq!(provider.get_call_count(), 1);
    }

    #[tokio::test]
    async fn test_mock_provider_multiple_calls() {
        let provider = MockLlmProvider::new("test");
        let request1 = ChatRequest::builder()
            .model("mock-model-1")
            .message(Role::User, "Hello")
            .build()
            .unwrap();

        let request2 = request1.clone();

        let response1 = provider.chat(request1).await.unwrap();
        let response2 = provider.chat(request2).await.unwrap();

        assert_eq!(response1.content, "Mock response 1");
        assert_eq!(response2.content, "Mock response 2");
        assert_eq!(provider.get_call_count(), 2);
    }

    #[tokio::test]
    async fn test_mock_provider_custom_responses() {
        let custom_responses = vec![
            "Custom 1".to_string(),
            "Custom 2".to_string(),
            "Custom 3".to_string(),
        ];
        let provider = MockLlmProvider::new("test").with_responses(custom_responses);

        for i in 1..=3 {
            let request = ChatRequest::builder()
                .model("mock-model-1")
                .message(Role::User, "Test")
                .build()
                .unwrap();

            let response = provider.chat(request).await.unwrap();
            assert_eq!(response.content, format!("Custom {}", i));
        }
    }

    #[tokio::test]
    async fn test_mock_provider_failure() {
        let provider = MockLlmProvider::new("test").with_failure();
        let request = ChatRequest::builder()
            .model("mock-model-1")
            .message(Role::User, "Hello")
            .build()
            .unwrap();

        let result = provider.chat(request).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_mock_provider_validation_success() {
        let provider = MockLlmProvider::new("test");
        assert!(provider.validate().await.is_ok());
    }

    #[tokio::test]
    async fn test_mock_provider_validation_failure() {
        let provider = MockLlmProvider::new("test").without_api_key();
        let result = provider.validate().await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_mock_provider_token_counting() {
        let provider = MockLlmProvider::new("test");
        let request = ChatRequest::builder()
            .model("mock-model-1")
            .message(Role::User, "This is a test message with some content")
            .build()
            .unwrap();

        let response = provider.chat(request).await.unwrap();
        let usage = response.usage.unwrap();

        assert!(usage.prompt_tokens > 0);
        assert!(usage.completion_tokens > 0);
        assert_eq!(
            usage.total_tokens,
            usage.prompt_tokens + usage.completion_tokens
        );
    }

    #[tokio::test]
    async fn test_mock_provider_streaming_failure() {
        let provider = MockLlmProvider::new("test").with_failure();
        let request = ChatRequest::builder()
            .model("mock-model-1")
            .message(Role::User, "Test")
            .build()
            .unwrap();

        let result = provider.chat_stream(request).await;
        assert!(result.is_err());
    }

    #[tokio::test]
    async fn test_mock_provider_supports_model() {
        let provider = MockLlmProvider::new("test");
        assert!(provider.supports_model("mock-model-1"));
        assert!(provider.supports_model("mock-model-2"));
        assert!(!provider.supports_model("unknown-model"));
    }
}
