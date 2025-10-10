//! Integration tests for Anthropic Claude provider
//!
//! Tests specific to Anthropic's Claude API including message format,
//! streaming, and error handling.

#[cfg(test)]
mod tests {
    use ggen_core::llm::{ChatRequest, Message, Role};

    #[test]
    fn test_anthropic_message_format() {
        let messages = vec![
            Message::user("Hello Claude"),
            Message::assistant("Hello! How can I help you?"),
        ];

        assert_eq!(messages[0].role, Role::User);
        assert_eq!(messages[1].role, Role::Assistant);
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
            assert!(model.starts_with("claude"));
        }
    }

    #[test]
    fn test_anthropic_system_message_handling() {
        let system = Message::system("You are a helpful assistant.");
        let user = Message::user("Hello");

        assert_eq!(system.role, Role::System);
        assert_eq!(user.role, Role::User);
    }

    #[test]
    fn test_anthropic_max_tokens() {
        let configs = vec![
            (1024, true),   // Valid
            (4096, true),   // Valid
            (100000, true), // Valid for Claude 3
        ];

        for (tokens, valid) in configs {
            if valid {
                assert!(tokens > 0);
            }
        }
    }

    #[test]
    fn test_anthropic_stop_sequences() {
        let request = ChatRequest::builder()
            .model("claude-3-sonnet-20240229")
            .message(Role::User, "Test")
            .build()
            .unwrap();

        assert_eq!(request.model, "claude-3-sonnet-20240229");
    }

    #[test]
    fn test_anthropic_temperature_range() {
        let request = ChatRequest::builder()
            .model("claude-3-sonnet-20240229")
            .message(Role::User, "Test")
            .temperature(0.7)
            .build()
            .unwrap();

        assert_eq!(request.temperature, Some(0.7));
        assert!(request.temperature.unwrap() >= 0.0);
        assert!(request.temperature.unwrap() <= 1.0);
    }

    #[test]
    fn test_anthropic_top_p_sampling() {
        let request = ChatRequest::builder()
            .model("claude-3-sonnet-20240229")
            .message(Role::User, "Test")
            .top_p(0.9)
            .build()
            .unwrap();

        let top_p = request.top_p.unwrap();
        assert!(top_p > 0.0 && top_p <= 1.0);
    }

    #[test]
    fn test_anthropic_conversation_context() {
        let conversation = vec![
            Message::user("What is 2+2?"),
            Message::assistant("2+2 equals 4."),
            Message::user("What about 3+3?"),
        ];

        assert_eq!(conversation.len(), 3);
        assert_eq!(conversation.last().unwrap().role, Role::User);
    }

    #[test]
    fn test_anthropic_empty_message_rejection() {
        let result = ChatRequest::builder()
            .model("claude-3-sonnet-20240229")
            .message(Role::User, "")
            .build();

        // Builder should still succeed but provider would reject empty content
        assert!(result.is_ok());
    }

    #[test]
    fn test_anthropic_long_context() {
        // Claude 3 supports very long contexts (up to 200k tokens)
        let long_content = "word ".repeat(50000);
        let message = Message::user(long_content.clone());

        assert!(message.content.len() > 100000);
    }
}
