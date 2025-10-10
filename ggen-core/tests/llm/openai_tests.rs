//! Integration tests for OpenAI GPT provider
//!
//! Tests specific to OpenAI's GPT API including chat completions and various models.

#[cfg(test)]
mod tests {
    use ggen_core::llm::{ChatRequest, Message, Role};

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
            assert!(model.starts_with("gpt"));
        }
    }

    #[test]
    fn test_openai_message_roles() {
        let roles = vec![Role::System, Role::User, Role::Assistant];

        for role in roles {
            let message = Message::new(role, "Test content");
            assert_eq!(message.role, role);
        }
    }

    #[test]
    fn test_openai_chat_request_builder() {
        let request = ChatRequest::builder()
            .model("gpt-4")
            .message(Role::System, "You are a helpful assistant.")
            .message(Role::User, "What is Rust?")
            .temperature(0.7)
            .max_tokens(500)
            .build()
            .unwrap();

        assert_eq!(request.model, "gpt-4");
        assert_eq!(request.messages.len(), 2);
        assert_eq!(request.temperature, Some(0.7));
        assert_eq!(request.max_tokens, Some(500));
    }

    #[test]
    fn test_openai_temperature_range() {
        let temperatures = vec![0.0, 0.5, 1.0, 1.5, 2.0];

        for temp in temperatures {
            let request = ChatRequest::builder()
                .model("gpt-4")
                .message(Role::User, "Test")
                .temperature(temp)
                .build()
                .unwrap();

            assert!(request.temperature.unwrap() >= 0.0);
            assert!(request.temperature.unwrap() <= 2.0);
        }
    }

    #[test]
    fn test_openai_streaming_request() {
        let request = ChatRequest::builder()
            .model("gpt-3.5-turbo")
            .message(Role::User, "Count to 5")
            .stream(true)
            .build()
            .unwrap();

        assert!(request.stream);
    }

    #[test]
    fn test_openai_top_p_nucleus_sampling() {
        let request = ChatRequest::builder()
            .model("gpt-4")
            .message(Role::User, "Test")
            .top_p(0.95)
            .build()
            .unwrap();

        let top_p = request.top_p.unwrap();
        assert!(top_p > 0.0 && top_p <= 1.0);
    }

    #[test]
    fn test_openai_conversation_memory() {
        let conversation = vec![
            Message::system("You remember previous messages."),
            Message::user("My name is Alice."),
            Message::assistant("Nice to meet you, Alice!"),
            Message::user("What's my name?"),
        ];

        assert_eq!(conversation.len(), 4);
        assert!(conversation.iter().any(|m| m.content.contains("Alice")));
    }

    #[test]
    fn test_openai_multi_turn_builder() {
        let request = ChatRequest::builder()
            .model("gpt-4")
            .message(Role::User, "Hello")
            .message(Role::Assistant, "Hi there!")
            .message(Role::User, "How are you?")
            .build()
            .unwrap();

        assert_eq!(request.messages.len(), 3);
    }

    #[test]
    fn test_openai_builder_validation() {
        // Should fail without model
        let result = ChatRequest::builder()
            .message(Role::User, "Test")
            .build();
        assert!(result.is_err());

        // Should fail without messages
        let result = ChatRequest::builder()
            .model("gpt-4")
            .build();
        assert!(result.is_err());
    }
}
