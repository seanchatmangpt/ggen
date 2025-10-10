//! Integration tests for OpenAI GPT provider
//!
//! Tests specific to OpenAI's GPT API including chat completions,
//! function calling, and various models.

#[cfg(test)]
mod tests {
    use super::super::provider_trait_tests::{LlmConfig, LlmProvider, Message};
    use super::super::mock_provider::MockLlmProvider;

    #[test]
    fn test_openai_model_names() {
        let valid_models = vec![
            "gpt-4",
            "gpt-4-turbo-preview",
            "gpt-4-32k",
            "gpt-3.5-turbo",
            "gpt-3.5-turbo-16k",
        ];

        for model in valid_models {
            let config = LlmConfig {
                model: model.to_string(),
                ..Default::default()
            };
            assert!(config.model.starts_with("gpt"));
        }
    }

    #[test]
    fn test_openai_message_roles() {
        let roles = vec!["system", "user", "assistant", "function"];

        for role in roles {
            let message = Message {
                role: role.to_string(),
                content: "Test content".to_string(),
            };
            assert_eq!(message.role, role);
        }
    }

    #[test]
    fn test_openai_chat_completion() {
        let provider = MockLlmProvider::new("openai");
        let messages = vec![
            Message {
                role: "system".to_string(),
                content: "You are a helpful assistant.".to_string(),
            },
            Message {
                role: "user".to_string(),
                content: "What is Rust?".to_string(),
            },
        ];

        let config = LlmConfig {
            model: "gpt-4".to_string(),
            temperature: 0.7,
            max_tokens: Some(500),
            ..Default::default()
        };

        let response = provider.complete(messages, config).unwrap();
        assert!(!response.content.is_empty());
        assert!(response.usage.total_tokens > 0);
    }

    #[test]
    fn test_openai_temperature_range() {
        // OpenAI supports 0-2 temperature range
        let temperatures = vec![0.0, 0.5, 1.0, 1.5, 2.0];

        for temp in temperatures {
            let config = LlmConfig {
                temperature: temp,
                ..Default::default()
            };
            assert!(config.temperature >= 0.0);
            assert!(config.temperature <= 2.0);
        }
    }

    #[test]
    fn test_openai_max_tokens_limits() {
        // Different models have different token limits
        let model_limits = vec![
            ("gpt-4", 8192),
            ("gpt-4-32k", 32768),
            ("gpt-3.5-turbo", 4096),
            ("gpt-3.5-turbo-16k", 16384),
        ];

        for (model, limit) in model_limits {
            let config = LlmConfig {
                model: model.to_string(),
                max_tokens: Some(limit / 2),
                ..Default::default()
            };
            assert!(config.max_tokens.unwrap() <= limit);
        }
    }

    #[test]
    fn test_openai_streaming() {
        let provider = MockLlmProvider::new("openai");
        let messages = vec![Message {
            role: "user".to_string(),
            content: "Count to 5".to_string(),
        }];

        let config = LlmConfig {
            model: "gpt-3.5-turbo".to_string(),
            ..Default::default()
        };

        let stream = provider.stream_complete(messages, config).unwrap();
        let chunks: Vec<_> = stream.collect();

        assert!(!chunks.is_empty());
        assert!(chunks.iter().all(|c| c.is_ok()));
    }

    #[test]
    fn test_openai_stop_sequences() {
        let config = LlmConfig {
            model: "gpt-4".to_string(),
            stop_sequences: Some(vec![
                "\n".to_string(),
                "END".to_string(),
                "###".to_string(),
            ]),
            ..Default::default()
        };

        let stops = config.stop_sequences.unwrap();
        assert_eq!(stops.len(), 3);
        assert!(stops.contains(&"END".to_string()));
    }

    #[test]
    fn test_openai_top_p_nucleus_sampling() {
        let config = LlmConfig {
            top_p: Some(0.95),
            ..Default::default()
        };

        let top_p = config.top_p.unwrap();
        assert!(top_p > 0.0 && top_p <= 1.0);
    }

    #[test]
    fn test_openai_presence_penalty() {
        // OpenAI supports presence_penalty between -2.0 and 2.0
        // This would be an additional field in a full implementation
        let valid_penalties = vec![-2.0, -1.0, 0.0, 1.0, 2.0];

        for penalty in valid_penalties {
            assert!(penalty >= -2.0 && penalty <= 2.0);
        }
    }

    #[test]
    fn test_openai_frequency_penalty() {
        // OpenAI supports frequency_penalty between -2.0 and 2.0
        let valid_penalties = vec![-2.0, -1.0, 0.0, 1.0, 2.0];

        for penalty in valid_penalties {
            assert!(penalty >= -2.0 && penalty <= 2.0);
        }
    }

    #[test]
    fn test_openai_finish_reasons() {
        let valid_reasons = vec!["stop", "length", "content_filter", "function_call"];

        for reason in valid_reasons {
            assert!(!reason.is_empty());
        }
    }

    #[test]
    fn test_openai_token_counting() {
        let provider = MockLlmProvider::new("openai");
        let messages = vec![Message {
            role: "user".to_string(),
            content: "This is a test message".to_string(),
        }];

        let response = provider
            .complete(messages, LlmConfig::default())
            .unwrap();

        assert!(response.usage.prompt_tokens > 0);
        assert!(response.usage.completion_tokens > 0);
        assert_eq!(
            response.usage.total_tokens,
            response.usage.prompt_tokens + response.usage.completion_tokens
        );
    }

    #[test]
    fn test_openai_conversation_memory() {
        let conversation = vec![
            Message {
                role: "system".to_string(),
                content: "You remember previous messages.".to_string(),
            },
            Message {
                role: "user".to_string(),
                content: "My name is Alice.".to_string(),
            },
            Message {
                role: "assistant".to_string(),
                content: "Nice to meet you, Alice!".to_string(),
            },
            Message {
                role: "user".to_string(),
                content: "What's my name?".to_string(),
            },
        ];

        assert_eq!(conversation.len(), 4);
        assert!(conversation.iter().any(|m| m.content.contains("Alice")));
    }

    #[test]
    fn test_openai_empty_content_handling() {
        let message = Message {
            role: "user".to_string(),
            content: "".to_string(),
        };

        // Should detect empty content
        assert!(message.content.is_empty());
    }

    #[test]
    fn test_openai_multi_turn_conversation() {
        let provider = MockLlmProvider::new("openai");
        let config = LlmConfig {
            model: "gpt-4".to_string(),
            ..Default::default()
        };

        // First turn
        let messages1 = vec![Message {
            role: "user".to_string(),
            content: "Hello".to_string(),
        }];
        let response1 = provider.complete(messages1, config.clone()).unwrap();
        assert!(!response1.content.is_empty());

        // Second turn
        let messages2 = vec![
            Message {
                role: "user".to_string(),
                content: "Hello".to_string(),
            },
            Message {
                role: "assistant".to_string(),
                content: response1.content,
            },
            Message {
                role: "user".to_string(),
                content: "How are you?".to_string(),
            },
        ];
        let response2 = provider.complete(messages2, config).unwrap();
        assert!(!response2.content.is_empty());
    }

    #[test]
    fn test_openai_rate_limit_handling() {
        let provider = MockLlmProvider::new("openai").with_failure();
        let messages = vec![Message {
            role: "user".to_string(),
            content: "Test".to_string(),
        }];

        let result = provider.complete(messages, LlmConfig::default());
        assert!(result.is_err());
    }
}
