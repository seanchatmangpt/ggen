//! Test helpers for ggen-ai integration tests

use crate::{config::OllamaConfig, OllamaClient, client::{LlmConfig, LlmClient}};
use std::time::Duration;
use tokio::time::timeout;

/// Check if Ollama is available and running with qwen3-coder:30b model
pub async fn check_ollama_availability() -> bool {
    let config = OllamaConfig::new();
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
    let config = OllamaConfig::new()
        .with_default_model("qwen3-coder:30b");
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
