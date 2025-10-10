//! Integration tests for LLM module
//!
//! These tests verify the complete LLM integration functionality including
//! provider adapters, streaming, configuration, and error handling.

#[cfg(test)]
mod tests {
    use ggen_core::llm::{
        ChatRequest, LlmConfig, LlmProvider, Message, OpenAiProvider, ProviderConfig, Role,
        StreamHandler,
    };

    #[test]
    fn test_message_builder() {
        let msg = Message::user("Hello, world!");
        assert_eq!(msg.role, Role::User);
        assert_eq!(msg.content, "Hello, world!");

        let system_msg = Message::system("You are helpful");
        assert_eq!(system_msg.role, Role::System);
    }

    #[test]
    fn test_chat_request_builder() {
        let request = ChatRequest::builder()
            .model("gpt-4")
            .message(Role::System, "You are helpful")
            .message(Role::User, "Hello")
            .temperature(0.7)
            .max_tokens(100)
            .build()
            .unwrap();

        assert_eq!(request.model, "gpt-4");
        assert_eq!(request.messages.len(), 2);
        assert_eq!(request.temperature, Some(0.7));
        assert_eq!(request.max_tokens, Some(100));
    }

    #[test]
    fn test_request_builder_validation() {
        // Missing model
        let result = ChatRequest::builder().message(Role::User, "test").build();
        assert!(result.is_err());

        // Missing messages
        let result = ChatRequest::builder().model("gpt-4").build();
        assert!(result.is_err());
    }

    #[test]
    fn test_provider_config() {
        let config = ProviderConfig::new("test-key")
            .with_endpoint("https://api.example.com")
            .with_default_model("gpt-4")
            .with_timeout(60)
            .with_header("X-Custom", "value");

        assert_eq!(config.api_key, "test-key");
        assert_eq!(config.endpoint, Some("https://api.example.com".to_string()));
        assert_eq!(config.default_model, Some("gpt-4".to_string()));
        assert_eq!(config.timeout_secs, 60);
        assert_eq!(config.headers.get("X-Custom"), Some(&"value".to_string()));

        // Validation should pass
        assert!(config.validate().is_ok());
    }

    #[test]
    fn test_config_validation_fails() {
        let invalid_config = ProviderConfig::new("").with_timeout(0);
        assert!(invalid_config.validate().is_err());
    }

    #[test]
    fn test_llm_config_multiple_providers() {
        let config = LlmConfig::new()
            .add_provider("openai", ProviderConfig::new("openai-key"))
            .unwrap()
            .add_provider("anthropic", ProviderConfig::new("anthropic-key"))
            .unwrap()
            .with_default_provider("openai");

        assert_eq!(config.providers.len(), 2);
        assert!(config.get_provider("openai").is_ok());
        assert!(config.get_provider("anthropic").is_ok());

        let (name, _) = config.get_default_provider().unwrap();
        assert_eq!(name, "openai");
    }

    #[test]
    fn test_config_missing_provider() {
        let config = LlmConfig::new();
        assert!(config.get_provider("nonexistent").is_err());
        assert!(config.get_default_provider().is_err());
    }

    #[test]
    fn test_stream_handler() {
        use ggen_core::llm::StreamChunk;

        let mut handler = StreamHandler::new();

        handler.handle_chunk(&StreamChunk::new("Hello ").with_tokens(2));
        handler.handle_chunk(&StreamChunk::new("World").with_tokens(1));
        handler.handle_chunk(&StreamChunk::final_chunk("!", "stop"));

        assert_eq!(handler.content(), "Hello World!");
        assert_eq!(handler.total_tokens(), 3);

        handler.reset();
        assert_eq!(handler.content(), "");
        assert_eq!(handler.total_tokens(), 0);
    }

    #[test]
    fn test_openai_provider_creation() {
        let provider = OpenAiProvider::new("test-key");
        assert_eq!(provider.name(), "openai");
        assert!(provider.supports_model("gpt-4"));
        assert!(provider.supports_model("gpt-3.5-turbo"));
        assert!(!provider.supports_model("claude-3"));
    }

    #[test]
    fn test_provider_default_model() {
        let provider = OpenAiProvider::new("key");
        assert_eq!(provider.default_model(), "gpt-4o-mini");

        let custom_config = ProviderConfig::new("key").with_default_model("gpt-4");
        let custom_provider = OpenAiProvider::with_config(custom_config);
        assert_eq!(custom_provider.default_model(), "gpt-4");
    }

    #[test]
    fn test_role_conversion() {
        assert_eq!(Role::from_str("user"), Some(Role::User));
        assert_eq!(Role::from_str("SYSTEM"), Some(Role::System));
        assert_eq!(Role::from_str("assistant"), Some(Role::Assistant));
        assert_eq!(Role::from_str("invalid"), None);
    }

    // Mock-based async tests would require actual API keys or mocking framework
    // These are marked with #[ignore] for now

    #[tokio::test]
    #[ignore] // Requires API key
    async fn test_openai_chat_completion() {
        let api_key = std::env::var("OPENAI_API_KEY").expect("OPENAI_API_KEY not set");
        let provider = OpenAiProvider::new(api_key);

        let request = ChatRequest::builder()
            .model("gpt-4o-mini")
            .message(Role::User, "Say 'test' and nothing else")
            .max_tokens(10)
            .build()
            .unwrap();

        let response = provider.chat(request).await.unwrap();
        assert!(!response.content.is_empty());
        assert_eq!(response.model, "gpt-4o-mini");
    }

    #[tokio::test]
    #[ignore] // Requires API key
    async fn test_provider_validation() {
        let api_key = std::env::var("OPENAI_API_KEY").expect("OPENAI_API_KEY not set");
        let provider = OpenAiProvider::new(api_key);

        assert!(provider.validate().await.is_ok());

        let invalid_provider = OpenAiProvider::new("invalid-key");
        assert!(invalid_provider.validate().await.is_err());
    }

    #[tokio::test]
    #[ignore] // Requires API key
    async fn test_streaming_response() {
        use futures::StreamExt;

        let api_key = std::env::var("OPENAI_API_KEY").expect("OPENAI_API_KEY not set");
        let provider = OpenAiProvider::new(api_key);

        let request = ChatRequest::builder()
            .model("gpt-4o-mini")
            .message(Role::User, "Count from 1 to 5")
            .stream(true)
            .build()
            .unwrap();

        let mut stream = provider.chat_stream(request).await.unwrap();
        let mut handler = StreamHandler::new();

        while let Some(result) = stream.next().await {
            match result {
                Ok(chunk) => {
                    handler.handle_chunk(&chunk);
                    if chunk.is_final {
                        break;
                    }
                }
                Err(e) => panic!("Stream error: {}", e),
            }
        }

        assert!(!handler.content().is_empty());
    }
}
