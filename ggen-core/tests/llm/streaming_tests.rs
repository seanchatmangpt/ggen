//! Streaming response tests
//!
//! Tests for streaming completions, chunk handling, and real-time processing.

#[cfg(test)]
mod tests {
    use super::super::provider_trait_tests::{LlmConfig, LlmProvider, Message};
    use super::super::mock_provider::MockLlmProvider;

    #[test]
    fn test_streaming_basic() {
        let provider = MockLlmProvider::new("test");
        let messages = vec![Message {
            role: "user".to_string(),
            content: "Tell me a story".to_string(),
        }];

        let stream = provider
            .stream_complete(messages, LlmConfig::default())
            .unwrap();

        let chunks: Vec<_> = stream.collect();
        assert!(!chunks.is_empty());
    }

    #[test]
    fn test_streaming_chunk_order() {
        let provider = MockLlmProvider::new("test").with_responses(vec!["ABC".to_string()]);

        let messages = vec![Message {
            role: "user".to_string(),
            content: "Test".to_string(),
        }];

        let stream = provider
            .stream_complete(messages, LlmConfig::default())
            .unwrap();

        let result: String = stream.map(|r| r.unwrap()).collect();
        assert_eq!(result, "ABC");
    }

    #[test]
    fn test_streaming_error_handling() {
        let provider = MockLlmProvider::new("test").with_failure();
        let messages = vec![Message {
            role: "user".to_string(),
            content: "Test".to_string(),
        }];

        let result = provider.stream_complete(messages, LlmConfig::default());
        assert!(result.is_err());
    }

    #[test]
    fn test_streaming_early_termination() {
        let provider = MockLlmProvider::new("test")
            .with_responses(vec!["This is a long response".to_string()]);

        let messages = vec![Message {
            role: "user".to_string(),
            content: "Test".to_string(),
        }];

        let stream = provider
            .stream_complete(messages, LlmConfig::default())
            .unwrap();

        // Take only first 5 chunks
        let chunks: Vec<_> = stream.take(5).collect();
        assert_eq!(chunks.len(), 5);
    }

    #[test]
    fn test_streaming_empty_response() {
        let provider = MockLlmProvider::new("test").with_responses(vec!["".to_string()]);

        let messages = vec![Message {
            role: "user".to_string(),
            content: "Test".to_string(),
        }];

        let stream = provider
            .stream_complete(messages, LlmConfig::default())
            .unwrap();

        let chunks: Vec<_> = stream.collect();
        assert!(chunks.is_empty() || chunks.iter().all(|c| c.is_ok()));
    }

    #[test]
    fn test_streaming_large_response() {
        let large_text = "word ".repeat(1000);
        let provider = MockLlmProvider::new("test").with_responses(vec![large_text.clone()]);

        let messages = vec![Message {
            role: "user".to_string(),
            content: "Generate long text".to_string(),
        }];

        let stream = provider
            .stream_complete(messages, LlmConfig::default())
            .unwrap();

        let result: String = stream.map(|r| r.unwrap()).collect();
        assert_eq!(result.len(), large_text.len());
    }

    #[test]
    fn test_streaming_with_stop_sequence() {
        let config = LlmConfig {
            stop_sequences: Some(vec!["STOP".to_string()]),
            ..Default::default()
        };

        let provider = MockLlmProvider::new("test")
            .with_responses(vec!["Text before STOP text after".to_string()]);

        let messages = vec![Message {
            role: "user".to_string(),
            content: "Test".to_string(),
        }];

        let stream = provider.stream_complete(messages, config).unwrap();
        let result: String = stream.map(|r| r.unwrap()).collect();

        assert!(result.contains("before"));
    }

    #[test]
    fn test_streaming_chunk_timing() {
        // Simulate real-time streaming behavior
        let provider = MockLlmProvider::new("test").with_responses(vec!["Hello".to_string()]);

        let messages = vec![Message {
            role: "user".to_string(),
            content: "Hi".to_string(),
        }];

        let stream = provider
            .stream_complete(messages, LlmConfig::default())
            .unwrap();

        let mut chunk_count = 0;
        for chunk in stream {
            assert!(chunk.is_ok());
            chunk_count += 1;
        }

        assert!(chunk_count > 0);
    }

    #[test]
    fn test_streaming_unicode_handling() {
        let unicode_text = "Hello 世界 🌍 مرحبا";
        let provider = MockLlmProvider::new("test").with_responses(vec![unicode_text.to_string()]);

        let messages = vec![Message {
            role: "user".to_string(),
            content: "Test unicode".to_string(),
        }];

        let stream = provider
            .stream_complete(messages, LlmConfig::default())
            .unwrap();

        let result: String = stream.map(|r| r.unwrap()).collect();
        assert_eq!(result, unicode_text);
    }

    #[test]
    fn test_streaming_special_characters() {
        let special_text = "Code: ```rust\nfn main() {}\n```";
        let provider = MockLlmProvider::new("test").with_responses(vec![special_text.to_string()]);

        let messages = vec![Message {
            role: "user".to_string(),
            content: "Show code".to_string(),
        }];

        let stream = provider
            .stream_complete(messages, LlmConfig::default())
            .unwrap();

        let result: String = stream.map(|r| r.unwrap()).collect();
        assert!(result.contains("```"));
    }

    #[test]
    fn test_streaming_progress_tracking() {
        let provider = MockLlmProvider::new("test").with_responses(vec!["1234567890".to_string()]);

        let messages = vec![Message {
            role: "user".to_string(),
            content: "Count".to_string(),
        }];

        let stream = provider
            .stream_complete(messages, LlmConfig::default())
            .unwrap();

        let mut processed_chars = 0;
        for chunk in stream {
            if let Ok(text) = chunk {
                processed_chars += text.len();
            }
        }

        assert!(processed_chars > 0);
    }

    #[test]
    fn test_streaming_buffer_accumulation() {
        let provider = MockLlmProvider::new("test").with_responses(vec!["ABC".to_string()]);

        let messages = vec![Message {
            role: "user".to_string(),
            content: "Test".to_string(),
        }];

        let stream = provider
            .stream_complete(messages, LlmConfig::default())
            .unwrap();

        let mut buffer = String::new();
        for chunk in stream {
            if let Ok(text) = chunk {
                buffer.push_str(&text);
            }
        }

        assert_eq!(buffer, "ABC");
    }
}
