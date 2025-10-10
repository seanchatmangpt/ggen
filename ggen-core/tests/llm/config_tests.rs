//! Configuration tests
//!
//! Tests for configuration and request building.

#[cfg(test)]
mod tests {
    use ggen_core::llm::{ChatRequest, Message, Role};

    #[test]
    fn test_request_builder_pattern() {
        let request = ChatRequest::builder()
            .model("gpt-4")
            .message(Role::System, "You are helpful")
            .message(Role::User, "Hello")
            .temperature(0.7)
            .max_tokens(100)
            .top_p(0.9)
            .stream(false)
            .build()
            .unwrap();

        assert_eq!(request.model, "gpt-4");
        assert_eq!(request.messages.len(), 2);
        assert_eq!(request.temperature, Some(0.7));
        assert_eq!(request.max_tokens, Some(100));
        assert_eq!(request.top_p, Some(0.9));
        assert!(!request.stream);
    }

    #[test]
    fn test_message_helpers() {
        let system = Message::system("System prompt");
        let user = Message::user("User message");
        let assistant = Message::assistant("Assistant response");

        assert_eq!(system.role, Role::System);
        assert_eq!(user.role, Role::User);
        assert_eq!(assistant.role, Role::Assistant);
    }

    #[test]
    fn test_role_from_str() {
        assert_eq!(Role::from("user"), Role::User);
        assert_eq!(Role::from("system"), Role::System);
        assert_eq!(Role::from("assistant"), Role::Assistant);
        assert_eq!(Role::from("invalid"), Role::User); // defaults to User
    }

    #[test]
    fn test_optional_parameters() {
        let minimal = ChatRequest::builder()
            .model("gpt-4")
            .message(Role::User, "Test")
            .build()
            .unwrap();

        assert!(minimal.temperature.is_none());
        assert!(minimal.max_tokens.is_none());
        assert!(minimal.top_p.is_none());
    }

    #[test]
    fn test_message_chain() {
        let request = ChatRequest::builder()
            .model("gpt-4")
            .messages(vec![
                Message::system("Be helpful"),
                Message::user("Question 1"),
                Message::assistant("Answer 1"),
                Message::user("Question 2"),
            ])
            .build()
            .unwrap();

        assert_eq!(request.messages.len(), 4);
    }
}
