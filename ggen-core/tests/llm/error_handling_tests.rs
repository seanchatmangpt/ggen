//! Error handling tests
//!
//! Tests for error cases and validation.

#[cfg(test)]
mod tests {
    use ggen_core::llm::{ChatRequest, Role};

    #[test]
    fn test_builder_requires_model() {
        let result = ChatRequest::builder()
            .message(Role::User, "Test")
            .build();

        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "Model is required");
    }

    #[test]
    fn test_builder_requires_messages() {
        let result = ChatRequest::builder()
            .model("gpt-4")
            .build();

        assert!(result.is_err());
        assert_eq!(result.unwrap_err(), "At least one message is required");
    }

    #[test]
    fn test_temperature_bounds() {
        // Valid temperatures
        for temp in [0.0, 0.5, 1.0, 1.5, 2.0] {
            let request = ChatRequest::builder()
                .model("gpt-4")
                .message(Role::User, "Test")
                .temperature(temp)
                .build();

            assert!(request.is_ok());
        }
    }

    #[test]
    fn test_top_p_bounds() {
        // Valid top-p values
        for p in [0.0, 0.5, 0.9, 1.0] {
            let request = ChatRequest::builder()
                .model("gpt-4")
                .message(Role::User, "Test")
                .top_p(p)
                .build();

            assert!(request.is_ok());
        }
    }
}
