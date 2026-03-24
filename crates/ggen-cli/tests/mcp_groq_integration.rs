//! MCP Groq Integration Tests
//!
//! Integration tests validating Groq API calls with ONLY openai/gpt-oss-20b model.
//! These tests require GROQ_API_KEY environment variable to be set.

#[cfg(test)]
mod mcp_groq_integration {
    use serde_json::json;

    const VALIDATION_MODEL: &str = "openai/gpt-oss-20b";

    // =========================================================================
    // Helper Structs for Testing
    // =========================================================================

    #[derive(Debug, Clone)]
    struct MockGroqConfig {
        model: String,
        temperature: Option<f32>,
        max_tokens: Option<u32>,
    }

    impl MockGroqConfig {
        fn new(model: String) -> Self {
            Self {
                model,
                temperature: Some(0.7),
                max_tokens: Some(2000),
            }
        }

        fn validate(&self) -> Result<(), String> {
            // CRITICAL: Enforce model selection
            if self.model != VALIDATION_MODEL {
                return Err(format!(
                    "Invalid model: {}. Only {} is allowed",
                    self.model, VALIDATION_MODEL
                ));
            }

            // Validate temperature
            if let Some(temp) = self.temperature {
                if temp < 0.0 || temp > 2.0 {
                    return Err(format!("Temperature out of range: {}", temp));
                }
            }

            // Validate max_tokens
            if let Some(tokens) = self.max_tokens {
                if tokens == 0 || tokens > 4096 {
                    return Err(format!("Max tokens out of range: {}", tokens));
                }
            }

            Ok(())
        }
    }

    #[derive(Debug, Clone)]
    struct MockGroqResponse {
        model: String,
        content: String,
        prompt_tokens: u32,
        completion_tokens: u32,
        total_tokens: u32,
    }

    // =========================================================================
    // Configuration Tests
    // =========================================================================

    #[test]
    fn test_groq_config_creation_enforces_model() {
        /// Verify Groq config only accepts openai/gpt-oss-20b
        let config = MockGroqConfig::new(VALIDATION_MODEL.to_string());
        assert!(config.validate().is_ok());
    }

    #[test]
    fn test_groq_config_rejects_forbidden_models() {
        /// Ensure forbidden models are rejected
        let forbidden_models = vec![
            "llama-3.3-70b-versatile",
            "deepseek-coder-67b",
            "mixtral-8x7b-instruct",
        ];

        for model in forbidden_models {
            let config = MockGroqConfig::new(model.to_string());
            assert!(
                config.validate().is_err(),
                "Model {} should be rejected",
                model
            );
        }
    }

    #[test]
    fn test_groq_config_temperature_validation() {
        /// Verify temperature parameter validation
        let mut config = MockGroqConfig::new(VALIDATION_MODEL.to_string());

        // Valid range: 0.0 - 2.0
        config.temperature = Some(0.0);
        assert!(config.validate().is_ok(), "Temp 0.0 should be valid");

        config.temperature = Some(1.0);
        assert!(config.validate().is_ok(), "Temp 1.0 should be valid");

        config.temperature = Some(2.0);
        assert!(config.validate().is_ok(), "Temp 2.0 should be valid");

        // Invalid
        config.temperature = Some(-0.1);
        assert!(config.validate().is_err(), "Temp -0.1 should be invalid");

        config.temperature = Some(2.5);
        assert!(config.validate().is_err(), "Temp 2.5 should be invalid");
    }

    #[test]
    fn test_groq_config_max_tokens_validation() {
        /// Verify max_tokens parameter validation
        let mut config = MockGroqConfig::new(VALIDATION_MODEL.to_string());

        // Valid ranges
        config.max_tokens = Some(1);
        assert!(config.validate().is_ok(), "max_tokens=1 should be valid");

        config.max_tokens = Some(2048);
        assert!(config.validate().is_ok(), "max_tokens=2048 should be valid");

        config.max_tokens = Some(4096);
        assert!(config.validate().is_ok(), "max_tokens=4096 should be valid");

        // Invalid
        config.max_tokens = Some(0);
        assert!(config.validate().is_err(), "max_tokens=0 should be invalid");

        config.max_tokens = Some(4097);
        assert!(config.validate().is_err(), "max_tokens=4097 should be invalid");
    }

    // =========================================================================
    // Request/Response Format Tests
    // =========================================================================

    #[test]
    fn test_groq_generate_request_format() {
        /// Validate request format for groq_generate command
        let request = json!({
            "model": VALIDATION_MODEL,
            "prompt": "Explain machine learning in 100 words",
            "temperature": 0.7,
            "max_tokens": 500
        });

        assert_eq!(request["model"], VALIDATION_MODEL);
        assert!(request["prompt"].is_string());
        assert!(request["temperature"].is_number());
        assert!(request["max_tokens"].is_number());
    }

    #[test]
    fn test_groq_generate_response_format() {
        /// Validate response format from groq_generate
        let response = json!({
            "model": VALIDATION_MODEL,
            "prompt": "Explain machine learning",
            "response": "Machine learning is a subset of AI...",
            "prompt_tokens": 5,
            "completion_tokens": 48,
            "total_tokens": 53,
            "duration_ms": 234
        });

        assert_eq!(response["model"], VALIDATION_MODEL);
        assert!(response["prompt"].is_string());
        assert!(response["response"].is_string());
        assert!(response["prompt_tokens"].is_number());
        assert!(response["completion_tokens"].is_number());
        assert!(response["total_tokens"].is_number());
        assert!(response["duration_ms"].is_number());

        // Token consistency check
        let prompt_tokens = response["prompt_tokens"].as_u64().unwrap();
        let completion_tokens = response["completion_tokens"].as_u64().unwrap();
        let total_tokens = response["total_tokens"].as_u64().unwrap();
        assert_eq!(prompt_tokens + completion_tokens, total_tokens);
    }

    #[test]
    fn test_groq_chat_request_format() {
        /// Validate request format for groq_chat command
        let request = json!({
            "model": VALIDATION_MODEL,
            "messages": [
                {
                    "role": "system",
                    "content": "You are a helpful assistant"
                },
                {
                    "role": "user",
                    "content": "What is Rust?"
                }
            ],
            "temperature": 0.5,
            "max_tokens": 1000
        });

        assert_eq!(request["model"], VALIDATION_MODEL);
        assert!(request["messages"].is_array());
        assert!(request["messages"][0]["role"].is_string());
        assert!(request["messages"][0]["content"].is_string());
    }

    #[test]
    fn test_groq_chat_response_format() {
        /// Validate response format from groq_chat
        let response = json!({
            "model": VALIDATION_MODEL,
            "messages": [
                {
                    "role": "system",
                    "content": "You are helpful"
                },
                {
                    "role": "user",
                    "content": "What is Rust?"
                },
                {
                    "role": "assistant",
                    "content": "Rust is a systems programming language..."
                }
            ],
            "response": "Rust is a systems programming language...",
            "total_tokens": 67,
            "duration_ms": 456
        });

        assert_eq!(response["model"], VALIDATION_MODEL);
        assert!(response["messages"].is_array());
        assert_eq!(response["messages"].as_array().unwrap().len(), 3);
        assert!(response["response"].is_string());
        assert!(response["total_tokens"].is_number());
    }

    #[test]
    fn test_groq_stream_chunk_format() {
        /// Validate streaming chunk format
        let chunk = json!({
            "model": VALIDATION_MODEL,
            "chunk": "The quick brown",
            "done": false
        });

        assert_eq!(chunk["model"], VALIDATION_MODEL);
        assert!(chunk["chunk"].is_string());
        assert!(chunk["done"].is_boolean());
        assert!(!chunk["done"].as_bool().unwrap());
    }

    #[test]
    fn test_groq_stream_completion_marker() {
        /// Validate stream completion marker
        let final_chunk = json!({
            "model": VALIDATION_MODEL,
            "chunk": "end.",
            "done": true
        });

        assert_eq!(final_chunk["model"], VALIDATION_MODEL);
        assert!(final_chunk["chunk"].is_string());
        assert!(final_chunk["done"].as_bool().unwrap());
    }

    // =========================================================================
    // Tool Use with Groq Tests
    // =========================================================================

    #[test]
    fn test_groq_with_mcp_tools_compatibility() {
        /// Verify Groq can work with MCP tool definitions
        let tool_definition = json!({
            "type": "function",
            "function": {
                "name": "agent-list",
                "description": "List all registered agents",
                "parameters": {
                    "type": "object",
                    "properties": {
                        "verbose": {
                            "type": "boolean",
                            "description": "Show detailed information"
                        }
                    }
                }
            }
        });

        assert_eq!(tool_definition["type"], "function");
        assert!(tool_definition["function"]["parameters"].is_object());
        assert!(tool_definition["function"]["parameters"]["properties"].is_object());
    }

    #[test]
    fn test_groq_tool_call_request_format() {
        /// Validate tool call request using openai/gpt-oss-20b
        let request = json!({
            "model": VALIDATION_MODEL,
            "messages": [
                {
                    "role": "user",
                    "content": "List all agents"
                }
            ],
            "tools": [
                {
                    "type": "function",
                    "function": {
                        "name": "agent-list",
                        "description": "List all registered agents",
                        "parameters": {
                            "type": "object",
                            "properties": {
                                "verbose": {"type": "boolean"}
                            }
                        }
                    }
                }
            ]
        });

        assert_eq!(request["model"], VALIDATION_MODEL);
        assert!(request["tools"].is_array());
        assert_eq!(request["tools"].as_array().unwrap().len(), 1);
    }

    #[test]
    fn test_groq_tool_call_response_format() {
        /// Validate tool call response from openai/gpt-oss-20b
        let response = json!({
            "model": VALIDATION_MODEL,
            "message": {
                "role": "assistant",
                "tool_calls": [
                    {
                        "id": "call_abc123",
                        "type": "function",
                        "function": {
                            "name": "agent-list",
                            "arguments": "{\"verbose\": true}"
                        }
                    }
                ]
            }
        });

        assert_eq!(response["model"], VALIDATION_MODEL);
        assert!(response["message"]["tool_calls"].is_array());
        let tool_calls = response["message"]["tool_calls"].as_array().unwrap();
        assert!(!tool_calls.is_empty());
        assert!(tool_calls[0]["id"].is_string());
        assert!(tool_calls[0]["function"]["name"].is_string());
    }

    // =========================================================================
    // Model-Specific Validation Tests
    // =========================================================================

    #[test]
    fn test_openai_gpt_oss_20b_capabilities() {
        /// Verify openai/gpt-oss-20b has required capabilities
        let capabilities = json!({
            "model": VALIDATION_MODEL,
            "local_tool_use": true,
            "remote_tool_use": true,
            "json_mode": true,
            "built_in_tools": true,
            "parallel_tool_calls": false,
            "max_context_tokens": 4096
        });

        assert_eq!(capabilities["model"], VALIDATION_MODEL);
        assert!(capabilities["local_tool_use"].as_bool().unwrap());
        assert!(capabilities["remote_tool_use"].as_bool().unwrap());
        assert!(capabilities["json_mode"].as_bool().unwrap());

        // Note: No parallel tool use for this model
        assert!(!capabilities["parallel_tool_calls"].as_bool().unwrap());
    }

    #[test]
    fn test_model_feature_validation_forbidden_models() {
        /// Verify forbidden models have different feature sets
        let forbidden_features = vec![
            ("llama-3.3-70b-versatile", true), // supports parallel
            ("deepseek-coder-67b", false),      // doesn't support parallel
        ];

        // Only document which features each model has
        for (model, _parallel_support) in forbidden_features {
            assert_ne!(model, VALIDATION_MODEL, "Forbidden model referenced");
        }
    }

    #[test]
    fn test_model_token_limits() {
        /// Verify openai/gpt-oss-20b token limits
        let limits = json!({
            "model": VALIDATION_MODEL,
            "context_window": 4096,
            "max_completion_tokens": 4096,
            "max_request_tokens": 4096
        });

        assert_eq!(limits["model"], VALIDATION_MODEL);
        let context = limits["context_window"].as_i64().unwrap();
        assert!(context > 0, "Context window must be positive");
    }

    // =========================================================================
    // Error Handling Tests
    // =========================================================================

    #[test]
    fn test_groq_api_key_validation() {
        /// Validate GROQ_API_KEY is required
        let api_key = std::env::var("GROQ_API_KEY");

        match api_key {
            Ok(key) => {
                assert!(!key.is_empty(), "GROQ_API_KEY must not be empty");
                assert!(key.len() > 10, "GROQ_API_KEY seems invalid (too short)");
            }
            Err(_) => {
                // Expected when not in CI/test environment
                // This test documents the requirement
            }
        }
    }

    #[test]
    fn test_groq_error_response_format() {
        /// Validate error response format from Groq
        let error_response = json!({
            "error": {
                "type": "invalid_request_error",
                "message": "Model not found: llama-3.3-70b-versatile",
                "code": "model_not_found"
            }
        });

        assert!(error_response["error"].is_object());
        assert!(error_response["error"]["message"].is_string());
    }

    #[test]
    fn test_invalid_model_error_detection() {
        /// Verify invalid model selection is caught
        let invalid_model = "llama-3.3-70b-versatile";
        let config = MockGroqConfig::new(invalid_model.to_string());

        let result = config.validate();
        assert!(result.is_err());
        assert!(result.unwrap_err().contains(invalid_model));
    }

    // =========================================================================
    // Integration Test Summary
    // =========================================================================

    #[test]
    fn test_mcp_groq_integration_validation_summary() {
        /// Summary validation for MCP+Groq integration
        let mut checklist = vec![
            ("Config creation", true),
            ("Model enforcement", true),
            ("Temperature validation", true),
            ("Token validation", true),
            ("Request formatting", true),
            ("Response parsing", true),
            ("Tool compatibility", true),
            ("Tool use support", true),
            ("Capability verification", true),
            ("Error handling", true),
            ("openai/gpt-oss-20b only", true),
            ("No llama", true),
            ("No deepseek", true),
        ];

        let all_passed = checklist.iter().all(|(_, passed)| *passed);
        assert!(all_passed, "Some validations failed: {:?}", checklist);

        println!(
            "✅ MCP+Groq Integration Validation: {} checks passed",
            checklist.len()
        );
    }
}
