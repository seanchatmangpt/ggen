//! LLM Integration Test Suite
//!
//! Comprehensive test coverage for LLM provider integration.

mod llm;

#[cfg(test)]
mod integration_tests {
    use ggen_core::llm::{ChatRequest, Message, Role};

    #[test]
    fn test_message_creation() {
        let msg = Message::user("Hello");
        assert_eq!(msg.role, Role::User);
        assert_eq!(msg.content, "Hello");
    }

    #[test]
    fn test_chat_request_builder() {
        let request = ChatRequest::builder()
            .model("gpt-4")
            .message(Role::User, "Hello")
            .temperature(0.7)
            .build()
            .unwrap();

        assert_eq!(request.model, "gpt-4");
        assert_eq!(request.messages.len(), 1);
        assert_eq!(request.temperature, Some(0.7));
    }

    #[test]
    fn test_builder_validation() {
        let result = ChatRequest::builder().build();
        assert!(result.is_err());
    }

    #[test]
    fn test_role_conversions() {
        assert_eq!(Role::parse_role("user"), Some(Role::User));
        assert_eq!(Role::parse_role("SYSTEM"), Some(Role::System));
        assert_eq!(Role::parse_role("invalid"), None);
    }

    #[test]
    fn test_conversation_flow() {
        let messages = vec![
            Message::system("You are helpful"),
            Message::user("What is Rust?"),
            Message::assistant("Rust is a systems programming language"),
            Message::user("Tell me more"),
        ];

        assert_eq!(messages.len(), 4);
        assert_eq!(messages[0].role, Role::System);
        assert_eq!(messages[1].role, Role::User);
        assert_eq!(messages[2].role, Role::Assistant);
    }
}
