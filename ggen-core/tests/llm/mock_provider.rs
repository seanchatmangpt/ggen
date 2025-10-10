//! Mock LLM Provider Implementation
//!
//! Provides mock implementations for testing without making actual API calls.

use super::provider_trait_tests::{LlmConfig, LlmProvider, LlmResponse, Message, TokenUsage};
use std::collections::HashMap;
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

impl LlmProvider for MockLlmProvider {
    fn complete(
        &self,
        messages: Vec<Message>,
        config: LlmConfig,
    ) -> Result<LlmResponse, String> {
        if self.should_fail {
            return Err("Mock provider failure".to_string());
        }

        let mut count = self.call_count.lock().unwrap();
        *count += 1;

        let responses = self.responses.lock().unwrap();
        let response_idx = (*count - 1) % responses.len();
        let content = responses.get(response_idx).unwrap().clone();

        // Calculate token usage based on content length
        let prompt_tokens = messages.iter().map(|m| m.content.len() / 4).sum();
        let completion_tokens = content.len() / 4;

        Ok(LlmResponse {
            content,
            model: config.model,
            usage: TokenUsage {
                prompt_tokens,
                completion_tokens,
                total_tokens: prompt_tokens + completion_tokens,
            },
            finish_reason: "stop".to_string(),
        })
    }

    fn stream_complete(
        &self,
        messages: Vec<Message>,
        config: LlmConfig,
    ) -> Result<Box<dyn Iterator<Item = Result<String, String>>>, String> {
        if self.should_fail {
            return Err("Mock provider stream failure".to_string());
        }

        let response = self.complete(messages, config)?;
        let chunks: Vec<Result<String, String>> = response
            .content
            .chars()
            .map(|c| Ok(c.to_string()))
            .collect();

        Ok(Box::new(chunks.into_iter()))
    }

    fn provider_name(&self) -> &str {
        &self.name
    }

    fn validate_config(&self) -> Result<(), String> {
        if self.api_key.is_none() {
            return Err("API key not configured".to_string());
        }
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_mock_provider_creation() {
        let provider = MockLlmProvider::new("test-provider");
        assert_eq!(provider.provider_name(), "test-provider");
        assert!(!provider.should_fail);
    }

    #[test]
    fn test_mock_provider_complete() {
        let provider = MockLlmProvider::new("test");
        let messages = vec![Message {
            role: "user".to_string(),
            content: "Hello".to_string(),
        }];
        let config = LlmConfig::default();

        let response = provider.complete(messages, config).unwrap();
        assert_eq!(response.content, "Mock response 1");
        assert_eq!(provider.get_call_count(), 1);
    }

    #[test]
    fn test_mock_provider_multiple_calls() {
        let provider = MockLlmProvider::new("test");
        let messages = vec![Message {
            role: "user".to_string(),
            content: "Hello".to_string(),
        }];
        let config = LlmConfig::default();

        let response1 = provider.complete(messages.clone(), config.clone()).unwrap();
        let response2 = provider.complete(messages, config).unwrap();

        assert_eq!(response1.content, "Mock response 1");
        assert_eq!(response2.content, "Mock response 2");
        assert_eq!(provider.get_call_count(), 2);
    }

    #[test]
    fn test_mock_provider_custom_responses() {
        let custom_responses = vec![
            "Custom 1".to_string(),
            "Custom 2".to_string(),
            "Custom 3".to_string(),
        ];
        let provider = MockLlmProvider::new("test").with_responses(custom_responses.clone());

        let messages = vec![Message {
            role: "user".to_string(),
            content: "Test".to_string(),
        }];
        let config = LlmConfig::default();

        for expected in custom_responses {
            let response = provider.complete(messages.clone(), config.clone()).unwrap();
            assert_eq!(response.content, expected);
        }
    }

    #[test]
    fn test_mock_provider_failure() {
        let provider = MockLlmProvider::new("test").with_failure();
        let messages = vec![Message {
            role: "user".to_string(),
            content: "Hello".to_string(),
        }];
        let config = LlmConfig::default();

        let result = provider.complete(messages, config);
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "Mock provider failure");
    }

    #[test]
    fn test_mock_provider_validation_success() {
        let provider = MockLlmProvider::new("test");
        assert!(provider.validate_config().is_ok());
    }

    #[test]
    fn test_mock_provider_validation_failure() {
        let provider = MockLlmProvider::new("test").without_api_key();
        let result = provider.validate_config();
        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "API key not configured");
    }

    #[test]
    fn test_mock_provider_token_counting() {
        let provider = MockLlmProvider::new("test");
        let messages = vec![Message {
            role: "user".to_string(),
            content: "This is a test message with some content".to_string(),
        }];
        let config = LlmConfig::default();

        let response = provider.complete(messages, config).unwrap();
        assert!(response.usage.prompt_tokens > 0);
        assert!(response.usage.completion_tokens > 0);
        assert_eq!(
            response.usage.total_tokens,
            response.usage.prompt_tokens + response.usage.completion_tokens
        );
    }

    #[test]
    fn test_mock_provider_streaming() {
        let provider = MockLlmProvider::new("test");
        let messages = vec![Message {
            role: "user".to_string(),
            content: "Test".to_string(),
        }];
        let config = LlmConfig::default();

        let stream = provider.stream_complete(messages, config).unwrap();
        let collected: Vec<String> = stream.map(|r| r.unwrap()).collect();

        assert!(!collected.is_empty());
        assert_eq!(collected.join(""), "Mock response 1");
    }

    #[test]
    fn test_mock_provider_streaming_failure() {
        let provider = MockLlmProvider::new("test").with_failure();
        let messages = vec![Message {
            role: "user".to_string(),
            content: "Test".to_string(),
        }];
        let config = LlmConfig::default();

        let result = provider.stream_complete(messages, config);
        assert!(result.is_err());
    }
}
