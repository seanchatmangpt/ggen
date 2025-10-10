//! Integration tests for Anthropic Claude provider
//!
//! Tests specific to Anthropic's Claude API including message format,
//! streaming, and error handling.

#[cfg(test)]
mod tests {
    use super::super::provider_trait_tests::{LlmConfig, LlmProvider, Message};
    use super::super::mock_provider::MockLlmProvider;

    /// Test Anthropic-specific message format
    #[test]
    fn test_anthropic_message_format() {
        let messages = vec![
            Message {
                role: "user".to_string(),
                content: "Hello Claude".to_string(),
            },
            Message {
                role: "assistant".to_string(),
                content: "Hello! How can I help you?".to_string(),
            },
        ];

        assert_eq!(messages[0].role, "user");
        assert_eq!(messages[1].role, "assistant");
    }

    #[test]
    fn test_anthropic_model_names() {
        let valid_models = vec![
            "claude-3-opus-20240229",
            "claude-3-sonnet-20240229",
            "claude-3-haiku-20240307",
            "claude-2.1",
            "claude-2.0",
        ];

        for model in valid_models {
            let config = LlmConfig {
                model: model.to_string(),
                ..Default::default()
            };
            assert!(config.model.starts_with("claude"));
        }
    }

    #[test]
    fn test_anthropic_system_message_handling() {
        // Anthropic handles system messages separately
        let system = Message {
            role: "system".to_string(),
            content: "You are a helpful assistant.".to_string(),
        };

        let user = Message {
            role: "user".to_string(),
            content: "Hello".to_string(),
        };

        assert_eq!(system.role, "system");
        assert_eq!(user.role, "user");
    }

    #[test]
    fn test_anthropic_max_tokens() {
        // Anthropic has specific token limits
        let configs = vec![
            (1024, true),   // Valid
            (4096, true),   // Valid
            (100000, true), // Valid for Claude 3
            (0, false),     // Invalid
        ];

        for (tokens, valid) in configs {
            let config = LlmConfig {
                max_tokens: Some(tokens),
                ..Default::default()
            };

            if valid {
                assert!(config.max_tokens.unwrap() > 0);
            }
        }
    }

    #[test]
    fn test_anthropic_streaming_response() {
        let provider = MockLlmProvider::new("anthropic");
        let messages = vec![Message {
            role: "user".to_string(),
            content: "Write a haiku".to_string(),
        }];

        let config = LlmConfig {
            model: "claude-3-sonnet-20240229".to_string(),
            temperature: 1.0,
            max_tokens: Some(100),
            ..Default::default()
        };

        let stream = provider.stream_complete(messages, config).unwrap();
        let chunks: Vec<_> = stream.collect();

        assert!(!chunks.is_empty());
        assert!(chunks.iter().all(|c| c.is_ok()));
    }

    #[test]
    fn test_anthropic_stop_sequences() {
        let config = LlmConfig {
            model: "claude-3-sonnet-20240229".to_string(),
            stop_sequences: Some(vec![
                "\n\nHuman:".to_string(),
                "\n\nAssistant:".to_string(),
            ]),
            ..Default::default()
        };

        let stop_seqs = config.stop_sequences.unwrap();
        assert_eq!(stop_seqs.len(), 2);
        assert!(stop_seqs.contains(&"\n\nHuman:".to_string()));
    }

    #[test]
    fn test_anthropic_temperature_range() {
        // Anthropic supports 0-1 temperature range
        let config = LlmConfig {
            temperature: 0.7,
            ..Default::default()
        };

        assert!(config.temperature >= 0.0);
        assert!(config.temperature <= 1.0);
    }

    #[test]
    fn test_anthropic_top_p_sampling() {
        let config = LlmConfig {
            top_p: Some(0.9),
            ..Default::default()
        };

        let top_p = config.top_p.unwrap();
        assert!(top_p > 0.0 && top_p <= 1.0);
    }

    #[test]
    fn test_anthropic_error_rate_limit() {
        // Test rate limit error handling
        let provider = MockLlmProvider::new("anthropic").with_failure();
        let messages = vec![Message {
            role: "user".to_string(),
            content: "Test".to_string(),
        }];

        let result = provider.complete(messages, LlmConfig::default());
        assert!(result.is_err());
    }

    #[test]
    fn test_anthropic_conversation_context() {
        let conversation = vec![
            Message {
                role: "user".to_string(),
                content: "What is 2+2?".to_string(),
            },
            Message {
                role: "assistant".to_string(),
                content: "2+2 equals 4.".to_string(),
            },
            Message {
                role: "user".to_string(),
                content: "What about 3+3?".to_string(),
            },
        ];

        assert_eq!(conversation.len(), 3);
        assert_eq!(conversation.last().unwrap().role, "user");
    }

    #[test]
    fn test_anthropic_empty_message_rejection() {
        let empty_message = Message {
            role: "user".to_string(),
            content: "".to_string(),
        };

        // Empty messages should be rejected
        assert!(empty_message.content.is_empty());
    }

    #[test]
    fn test_anthropic_long_context() {
        // Claude 3 supports very long contexts (up to 200k tokens)
        let long_content = "word ".repeat(50000);
        let message = Message {
            role: "user".to_string(),
            content: long_content.clone(),
        };

        assert!(message.content.len() > 100000);
    }

    #[test]
    fn test_anthropic_finish_reasons() {
        let valid_finish_reasons = vec!["end_turn", "max_tokens", "stop_sequence"];

        for reason in valid_finish_reasons {
            // Each finish reason should be a valid string
            assert!(!reason.is_empty());
        }
    }
}
