//! Streaming response tests
//!
//! Tests for streaming completions and chunk handling.

#[cfg(test)]
mod tests {
    use ggen_core::llm::{ChatRequest, Role};

    #[test]
    fn test_streaming_request_flag() {
        let request = ChatRequest::builder()
            .model("gpt-4")
            .message(Role::User, "Tell me a story")
            .stream(true)
            .build()
            .unwrap();

        assert!(request.stream);
    }

    #[test]
    fn test_non_streaming_default() {
        let request = ChatRequest::builder()
            .model("gpt-4")
            .message(Role::User, "Test")
            .build()
            .unwrap();

        assert!(!request.stream);
    }

    #[test]
    fn test_streaming_with_max_tokens() {
        let request = ChatRequest::builder()
            .model("gpt-4")
            .message(Role::User, "Count to 100")
            .stream(true)
            .max_tokens(500)
            .build()
            .unwrap();

        assert!(request.stream);
        assert_eq!(request.max_tokens, Some(500));
    }
}
