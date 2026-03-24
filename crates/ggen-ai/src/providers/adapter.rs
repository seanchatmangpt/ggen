//! LLM provider adapter implementations

use crate::client::{LlmChunk, LlmClient, LlmConfig, LlmResponse, UsageStats};
use crate::error::Result;
use async_trait::async_trait;
use std::collections::HashMap;

/// Mock client for testing
#[derive(Debug, Clone)]
pub struct MockClient {
    responses: Vec<String>,
    current_index: usize,
    config: LlmConfig,
}

impl MockClient {
    /// Create a new mock client with predefined responses
    pub fn new(responses: Vec<String>) -> Self {
        Self {
            responses,
            current_index: 0,
            config: LlmConfig {
                model: "mock-model".to_string(),
                max_tokens: Some(1000),
                temperature: Some(0.0),
                top_p: Some(1.0),
                stop: None,
                extra: HashMap::new(),
            },
        }
    }

    /// Create a mock client with a single response
    pub fn with_response(response: &str) -> Self {
        Self::new(vec![response.to_string()])
    }
}

#[async_trait]
impl LlmClient for MockClient {
    async fn complete(&self, _prompt: &str) -> Result<LlmResponse> {
        let response = self
            .responses
            .get(self.current_index)
            .unwrap_or(&self.responses[0])
            .clone();

        let content_len = response.len();
        Ok(LlmResponse {
            content: response,
            usage: Some(UsageStats {
                prompt_tokens: 10,
                completion_tokens: content_len as u32 / 4, // Rough estimate
                total_tokens: 10 + content_len as u32 / 4,
            }),
            model: "mock-model".to_string(),
            finish_reason: Some("stop".to_string()),
            extra: HashMap::new(),
        })
    }

    async fn complete_stream(
        &self, _prompt: &str,
    ) -> Result<futures::stream::BoxStream<'static, LlmChunk>> {
        let response = self
            .responses
            .get(self.current_index)
            .unwrap_or(&self.responses[0])
            .clone();

        let content_len = response.len();
        let chunk = LlmChunk {
            content: response,
            model: "mock-model".to_string(),
            finish_reason: Some("stop".to_string()),
            usage: Some(UsageStats {
                prompt_tokens: 10,
                completion_tokens: content_len as u32 / 4,
                total_tokens: 10 + content_len as u32 / 4,
            }),
            extra: HashMap::new(),
        };

        Ok(Box::pin(futures::stream::iter(vec![chunk])))
    }

    fn get_config(&self) -> &LlmConfig {
        &self.config
    }

    fn update_config(&mut self, config: LlmConfig) {
        self.config = config;
    }
}

// Provider-specific helper functions for common configurations

/// Create Ollama client default configuration
pub fn ollama_default_config() -> LlmConfig {
    LlmConfig {
        model: "llama3.2".to_string(), // Default Ollama model
        max_tokens: Some(2048),
        temperature: Some(0.7),
        top_p: Some(0.9),
        stop: None,
        extra: HashMap::new(),
    }
}

/// Create Ollama client with qwen3-coder:30b configuration
pub fn ollama_qwen3_coder_config() -> LlmConfig {
    LlmConfig {
        model: "qwen3-coder:30b".to_string(),
        max_tokens: Some(2048),
        temperature: Some(0.7),
        top_p: Some(0.9),
        stop: None,
        extra: HashMap::new(),
    }
}

/// Create Groq default configuration — llama-3.3-70b-versatile
///
/// Best general-purpose Groq model. ~800 tokens/sec.
/// Requires `GROQ_API_KEY` environment variable.
pub fn groq_default_config() -> LlmConfig {
    LlmConfig {
        model: crate::constants::models::GROQ_DEFAULT.to_string(),
        max_tokens: Some(4096),
        temperature: Some(0.7),
        top_p: Some(0.9),
        stop: None,
        extra: HashMap::new(),
    }
}

/// Create Groq fast configuration — llama-3.1-8b-instant
///
/// Lowest latency Groq model. Ideal for agent loops, classification,
/// and tasks where speed > raw quality. ~1200 tokens/sec.
/// Requires `GROQ_API_KEY` environment variable.
pub fn groq_fast_config() -> LlmConfig {
    LlmConfig {
        model: crate::constants::models::GROQ_FAST.to_string(),
        max_tokens: Some(4096),
        temperature: Some(0.3),
        top_p: Some(0.9),
        stop: None,
        extra: HashMap::new(),
    }
}

/// Create Groq smart configuration — deepseek-r1-distill-llama-70b
///
/// DeepSeek R1 reasoning model running on Groq hardware.
/// Best for multi-step reasoning, SPARQL generation, and ontology work.
/// Requires `GROQ_API_KEY` environment variable.
pub fn groq_smart_config() -> LlmConfig {
    LlmConfig {
        model: crate::constants::models::GROQ_SMART.to_string(),
        max_tokens: Some(8192),
        temperature: Some(0.6),
        top_p: Some(0.95),
        stop: None,
        extra: HashMap::new(),
    }
}
