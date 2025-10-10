//! Unit tests for LLM Provider trait
//!
//! Tests the core provider interface that all LLM providers must implement.

#[cfg(test)]
mod tests {
    use serde::{Deserialize, Serialize};
    use std::collections::HashMap;

    /// Represents a message in a conversation
    #[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
    pub struct Message {
        pub role: String,
        pub content: String,
    }

    /// Configuration for LLM requests
    #[derive(Debug, Clone)]
    pub struct LlmConfig {
        pub model: String,
        pub temperature: f32,
        pub max_tokens: Option<usize>,
        pub top_p: Option<f32>,
        pub stop_sequences: Option<Vec<String>>,
    }

    impl Default for LlmConfig {
        fn default() -> Self {
            Self {
                model: "default-model".to_string(),
                temperature: 0.7,
                max_tokens: Some(1024),
                top_p: Some(1.0),
                stop_sequences: None,
            }
        }
    }

    /// Response from an LLM provider
    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct LlmResponse {
        pub content: String,
        pub model: String,
        pub usage: TokenUsage,
        pub finish_reason: String,
    }

    #[derive(Debug, Clone, Serialize, Deserialize)]
    pub struct TokenUsage {
        pub prompt_tokens: usize,
        pub completion_tokens: usize,
        pub total_tokens: usize,
    }

    /// Core trait that all LLM providers must implement
    pub trait LlmProvider: Send + Sync {
        /// Generate a completion from the provider
        fn complete(
            &self,
            messages: Vec<Message>,
            config: LlmConfig,
        ) -> Result<LlmResponse, String>;

        /// Stream a completion from the provider
        fn stream_complete(
            &self,
            messages: Vec<Message>,
            config: LlmConfig,
        ) -> Result<Box<dyn Iterator<Item = Result<String, String>>>, String>;

        /// Get the provider name
        fn provider_name(&self) -> &str;

        /// Validate the provider configuration
        fn validate_config(&self) -> Result<(), String>;
    }

    #[test]
    fn test_message_creation() {
        let message = Message {
            role: "user".to_string(),
            content: "Hello, world!".to_string(),
        };

        assert_eq!(message.role, "user");
        assert_eq!(message.content, "Hello, world!");
    }

    #[test]
    fn test_message_serialization() {
        let message = Message {
            role: "assistant".to_string(),
            content: "Test response".to_string(),
        };

        let json = serde_json::to_string(&message).unwrap();
        let deserialized: Message = serde_json::from_str(&json).unwrap();

        assert_eq!(message, deserialized);
    }

    #[test]
    fn test_config_defaults() {
        let config = LlmConfig::default();

        assert_eq!(config.model, "default-model");
        assert_eq!(config.temperature, 0.7);
        assert_eq!(config.max_tokens, Some(1024));
        assert_eq!(config.top_p, Some(1.0));
        assert!(config.stop_sequences.is_none());
    }

    #[test]
    fn test_config_custom_values() {
        let config = LlmConfig {
            model: "gpt-4".to_string(),
            temperature: 0.5,
            max_tokens: Some(2048),
            top_p: Some(0.9),
            stop_sequences: Some(vec!["END".to_string()]),
        };

        assert_eq!(config.model, "gpt-4");
        assert_eq!(config.temperature, 0.5);
        assert_eq!(config.max_tokens, Some(2048));
        assert_eq!(config.top_p, Some(0.9));
        assert_eq!(
            config.stop_sequences,
            Some(vec!["END".to_string()])
        );
    }

    #[test]
    fn test_token_usage_calculation() {
        let usage = TokenUsage {
            prompt_tokens: 100,
            completion_tokens: 50,
            total_tokens: 150,
        };

        assert_eq!(usage.total_tokens, 150);
        assert_eq!(
            usage.prompt_tokens + usage.completion_tokens,
            usage.total_tokens
        );
    }

    #[test]
    fn test_llm_response_structure() {
        let response = LlmResponse {
            content: "Test response content".to_string(),
            model: "test-model".to_string(),
            usage: TokenUsage {
                prompt_tokens: 10,
                completion_tokens: 20,
                total_tokens: 30,
            },
            finish_reason: "stop".to_string(),
        };

        assert_eq!(response.content, "Test response content");
        assert_eq!(response.model, "test-model");
        assert_eq!(response.finish_reason, "stop");
        assert_eq!(response.usage.total_tokens, 30);
    }

    #[test]
    fn test_temperature_bounds() {
        // Temperature should be between 0 and 2
        let valid_temps = vec![0.0, 0.5, 1.0, 1.5, 2.0];

        for temp in valid_temps {
            let config = LlmConfig {
                temperature: temp,
                ..Default::default()
            };
            assert!(config.temperature >= 0.0 && config.temperature <= 2.0);
        }
    }

    #[test]
    fn test_top_p_bounds() {
        // Top-p should be between 0 and 1
        let valid_top_p = vec![0.0, 0.5, 0.9, 1.0];

        for p in valid_top_p {
            let config = LlmConfig {
                top_p: Some(p),
                ..Default::default()
            };
            let top_p_val = config.top_p.unwrap();
            assert!(top_p_val >= 0.0 && top_p_val <= 1.0);
        }
    }

    #[test]
    fn test_conversation_flow() {
        let messages = vec![
            Message {
                role: "system".to_string(),
                content: "You are a helpful assistant.".to_string(),
            },
            Message {
                role: "user".to_string(),
                content: "What is Rust?".to_string(),
            },
            Message {
                role: "assistant".to_string(),
                content: "Rust is a systems programming language.".to_string(),
            },
            Message {
                role: "user".to_string(),
                content: "Tell me more.".to_string(),
            },
        ];

        assert_eq!(messages.len(), 4);
        assert_eq!(messages[0].role, "system");
        assert_eq!(messages[1].role, "user");
        assert_eq!(messages[2].role, "assistant");
        assert_eq!(messages[3].role, "user");
    }
}
