//! LLM client adapters for integrating with ggen-ai and other providers

use crate::Result;
use async_trait::async_trait;
use ggen_ai::LlmClient;
use serde::{Deserialize, Serialize};

/// LLM adapter trait for provider abstraction
#[async_trait]
pub trait LlmAdapter: Send + Sync {
    /// Generate completion from prompt
    async fn complete(&self, prompt: &str) -> Result<String>;

    /// Generate with structured output
    async fn complete_structured(&self, prompt: &str, schema: &str) -> Result<serde_json::Value>;

    /// Get adapter name
    fn name(&self) -> &str;
}

/// Adapter for ggen-ai client
pub struct GgenAiAdapter {
    client: ggen_ai::GenAiClient,
}

impl GgenAiAdapter {
    /// Create a new ggen-ai adapter
    pub fn new(client: ggen_ai::GenAiClient) -> Self {
        Self { client }
    }

    /// Create with default configuration
    pub fn default_client() -> Result<Self> {
        let config = ggen_ai::LlmConfig::default();
        let client = ggen_ai::GenAiClient::new(config)?;
        Ok(Self::new(client))
    }
}

#[async_trait]
impl LlmAdapter for GgenAiAdapter {
    async fn complete(&self, prompt: &str) -> Result<String> {
        let response = self.client.complete(prompt).await?;
        Ok(response.content)
    }

    async fn complete_structured(&self, prompt: &str, _schema: &str) -> Result<serde_json::Value> {
        // TODO: Implement structured output with schema validation
        let response = self.complete(prompt).await?;
        serde_json::from_str(&response).map_err(Into::into)
    }

    fn name(&self) -> &str {
        "GgenAiAdapter"
    }
}

/// Request for LLM completion
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct CompletionRequest {
    /// Prompt text
    pub prompt: String,

    /// Temperature (0.0 to 1.0)
    pub temperature: Option<f64>,

    /// Maximum tokens
    pub max_tokens: Option<usize>,

    /// Stop sequences
    pub stop: Option<Vec<String>>,
}

impl CompletionRequest {
    /// Create a new completion request
    pub fn new(prompt: impl Into<String>) -> Self {
        Self {
            prompt: prompt.into(),
            temperature: None,
            max_tokens: None,
            stop: None,
        }
    }

    /// Set temperature
    pub fn with_temperature(mut self, temperature: f64) -> Self {
        self.temperature = Some(temperature);
        self
    }

    /// Set max tokens
    pub fn with_max_tokens(mut self, max_tokens: usize) -> Self {
        self.max_tokens = Some(max_tokens);
        self
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_completion_request() {
        let request = CompletionRequest::new("Hello")
            .with_temperature(0.7)
            .with_max_tokens(100);

        assert_eq!(request.prompt, "Hello");
        assert_eq!(request.temperature, Some(0.7));
        assert_eq!(request.max_tokens, Some(100));
    }
}
