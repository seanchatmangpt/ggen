//! LLM provider adapter implementations

use crate::client::{GenAiClient, LlmChunk, LlmClient, LlmConfig, LlmResponse, UsageStats};
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

/// Ollama client adapter using GenAI
#[derive(Debug)]
pub struct OllamaClient {
    client: GenAiClient,
}

impl OllamaClient {
    /// Create a new Ollama client
    pub fn new(config: LlmConfig) -> Result<Self> {
        let client = GenAiClient::new(config)?;
        Ok(Self { client })
    }

    /// Create Ollama client with default configuration
    pub fn default_config() -> LlmConfig {
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
    pub fn qwen3_coder_config() -> LlmConfig {
        LlmConfig {
            model: "qwen3-coder:30b".to_string(),
            max_tokens: Some(2048),
            temperature: Some(0.7),
            top_p: Some(0.9),
            stop: None,
            extra: HashMap::new(),
        }
    }
}

#[async_trait]
impl LlmClient for OllamaClient {
    async fn complete(&self, prompt: &str) -> Result<LlmResponse> {
        self.client.complete(prompt).await
    }

    async fn complete_stream(
        &self, prompt: &str,
    ) -> Result<futures::stream::BoxStream<'static, LlmChunk>> {
        self.client.complete_stream(prompt).await
    }

    fn get_config(&self) -> &LlmConfig {
        self.client.get_config()
    }

    fn update_config(&mut self, config: LlmConfig) {
        self.client.update_config(config);
    }
}

/// OpenAI client adapter using GenAI
#[derive(Debug)]
pub struct OpenAIClient {
    client: GenAiClient,
}

impl OpenAIClient {
    /// Create a new OpenAI client
    pub fn new(config: LlmConfig) -> Result<Self> {
        let client = GenAiClient::new(config)?;
        Ok(Self { client })
    }
}

#[async_trait]
impl LlmClient for OpenAIClient {
    async fn complete(&self, prompt: &str) -> Result<LlmResponse> {
        self.client.complete(prompt).await
    }

    async fn complete_stream(
        &self, prompt: &str,
    ) -> Result<futures::stream::BoxStream<'static, LlmChunk>> {
        self.client.complete_stream(prompt).await
    }

    fn get_config(&self) -> &LlmConfig {
        self.client.get_config()
    }

    fn update_config(&mut self, config: LlmConfig) {
        self.client.update_config(config);
    }
}

/// Anthropic client adapter using GenAI
#[derive(Debug)]
pub struct AnthropicClient {
    client: GenAiClient,
}

impl AnthropicClient {
    /// Create a new Anthropic client
    pub fn new(config: LlmConfig) -> Result<Self> {
        let client = GenAiClient::new(config)?;
        Ok(Self { client })
    }
}

#[async_trait]
impl LlmClient for AnthropicClient {
    async fn complete(&self, prompt: &str) -> Result<LlmResponse> {
        self.client.complete(prompt).await
    }

    async fn complete_stream(
        &self, prompt: &str,
    ) -> Result<futures::stream::BoxStream<'static, LlmChunk>> {
        self.client.complete_stream(prompt).await
    }

    fn get_config(&self) -> &LlmConfig {
        self.client.get_config()
    }

    fn update_config(&mut self, config: LlmConfig) {
        self.client.update_config(config);
    }
}
