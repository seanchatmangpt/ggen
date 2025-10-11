//! Test helpers for ggen-ai integration tests
//!
//! # Test Helpers for LLM Integration Testing
//!
//! ## PURPOSE
//! Provides reusable test utilities for LLM integrations, specifically Ollama-based models.
//! Enables graceful test skipping when external services are unavailable.
//!
//! ## RESPONSIBILITIES
//! - **Runtime Availability Detection**: Check if Ollama service is accessible before tests run
//! - **Client Factory**: Provide standardized OllamaClient instances configured for testing
//! - **Conditional Test Execution**: Skip tests gracefully when dependencies are unavailable
//! - **Timeout Management**: Enforce reasonable timeouts for availability checks (10s default)
//!
//! ## CONSTRAINTS
//! - External Dependency: Requires Ollama running locally (typically http://localhost:11434)
//! - Model Requirement: Tests assume `qwen3-coder:30b` model is available
//! - Timeout Limit: Availability checks must complete within 10 seconds
//!
//! ## INVARIANTS
//! 1. Tests MUST skip (not fail) when Ollama is unavailable
//! 2. All test clients use same model (`qwen3-coder:30b`)
//! 3. No availability check exceeds 10 seconds
//! 4. Helper functions do not modify global or shared state
//!
//! ## REFACTORING PRIORITIES (from REFACTORING_ANALYSIS.md)
//! - **P1**: Implement MockLlmClient for offline testing
//! - **P1**: Add provider abstraction for multi-LLM support
//! - **P2**: Verify specific model availability, not just service health
//! - **P2**: Make timeouts configurable per test

use crate::{
    client::{LlmClient, LlmConfig},
    config::OllamaConfig,
    OllamaClient,
};
use std::time::Duration;
use tokio::time::timeout;

/// Check if Ollama is available and running with qwen3-coder:30b model
pub async fn check_ollama_availability() -> bool {
    let config = create_test_llm_config();
    match OllamaClient::new(config) {
        Ok(client) => {
            // Try a simple completion to verify Ollama is running
            let test_config = LlmConfig {
                model: "qwen3-coder:30b".to_string(),
                max_tokens: Some(10),
                temperature: Some(0.1),
                top_p: None,
                stop: None,
                extra: std::collections::HashMap::new(),
            };

            match timeout(Duration::from_secs(10), client.complete("Hello")).await {
                Ok(Ok(_)) => true,
                Ok(Err(_)) => false,
                Err(_) => false,
            }
        }
        Err(_) => false,
    }
}

/// Skip test if Ollama is not available
#[macro_export]
macro_rules! skip_if_ollama_unavailable {
    () => {
        if !ggen_ai::test_helpers::check_ollama_availability().await {
            println!("⏭️  Skipping test: Ollama not available");
            return;
        }
    };
}

/// Create a test Ollama client with qwen3-coder:30b configuration
pub fn create_test_ollama_client() -> Result<OllamaClient, crate::error::GgenAiError> {
    let config = create_test_llm_config();
    OllamaClient::new(config)
}

/// Create a test LlmConfig for qwen3-coder:30b
pub fn create_test_llm_config() -> LlmConfig {
    LlmConfig {
        model: "qwen3-coder:30b".to_string(),
        max_tokens: Some(100),
        temperature: Some(0.1),
        top_p: None,
        stop: None,
        extra: std::collections::HashMap::new(),
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_ollama_availability_check() {
        // This test will always pass, but will show if Ollama is available
        let available = check_ollama_availability().await;
        if available {
            println!("✅ Ollama is available for integration tests");
        } else {
            println!("⚠️  Ollama is not available - integration tests will be skipped");
        }
    }
}
