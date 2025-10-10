//! Provider adapter utilities

use crate::client::{LlmClient, LlmConfig, LlmResponse, LlmChunk};
use crate::error::{GgenAiError, Result};
use async_trait::async_trait;
use futures::stream::BoxStream;

/// Mock client for testing
pub struct MockClient {
    responses: Vec<String>,
    current_index: usize,
}

impl MockClient {
    /// Create a new mock client
    pub fn new(responses: Vec<String>) -> Self {
        Self {
            responses,
            current_index: 0,
        }
    }
    
    /// Create a mock client with a single response
    pub fn with_response(response: &str) -> Self {
        Self::new(vec![response.to_string()])
    }
}

#[async_trait]
impl LlmClient for MockClient {
    async fn complete(&self, prompt: &str, _config: Option<LlmConfig>) -> Result<LlmResponse> {
        let response = self.responses.get(self.current_index)
            .ok_or_else(|| GgenAiError::llm_provider("No more mock responses"))?;
        
        Ok(LlmResponse {
            content: response.clone(),
            usage: Some(crate::client::UsageStats {
                prompt_tokens: prompt.len() as u32 / 4, // Rough estimate
                completion_tokens: response.len() as u32 / 4,
                total_tokens: (prompt.len() + response.len()) as u32 / 4,
            }),
            model: "mock-model".to_string(),
            finish_reason: Some("stop".to_string()),
        })
    }
    
    async fn stream_complete(
        &self,
        prompt: &str,
        _config: Option<LlmConfig>,
    ) -> Result<BoxStream<'static, Result<LlmChunk>>> {
        let response = self.responses.get(self.current_index)
            .ok_or_else(|| GgenAiError::llm_provider("No more mock responses"))?;
        
        // Split response into chunks for streaming
        let chunks: Vec<LlmChunk> = response
            .chars()
            .collect::<Vec<_>>()
            .chunks(10)
            .map(|chunk| LlmChunk {
                content: chunk.iter().collect(),
                done: false,
                usage: None,
            })
            .collect();
        
        let mut final_chunks = chunks;
        if let Some(last) = final_chunks.last_mut() {
            last.done = true;
            last.usage = Some(crate::client::UsageStats {
                prompt_tokens: prompt.len() as u32 / 4,
                completion_tokens: response.len() as u32 / 4,
                total_tokens: (prompt.len() + response.len()) as u32 / 4,
            });
        }
        
        let stream = futures::stream::iter(final_chunks.into_iter().map(Ok));
        Ok(Box::pin(stream))
    }
    
    async fn embed(&self, _text: &str) -> Result<Vec<f32>> {
        // Return mock embeddings
        Ok(vec![0.1; 1536]) // OpenAI embedding size
    }
    
    fn provider_name(&self) -> &str {
        "mock"
    }
    
    fn supported_models(&self) -> Vec<String> {
        vec!["mock-model".to_string()]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_mock_client() {
        let client = MockClient::with_response("Hello, world!");
        let response = client.complete("Test prompt", None).await.unwrap();
        
        assert_eq!(response.content, "Hello, world!");
        assert_eq!(response.model, "mock-model");
        assert!(response.usage.is_some());
    }
    
    #[tokio::test]
    async fn test_mock_client_streaming() {
        let client = MockClient::with_response("Hello, world!");
        let mut stream = client.stream_complete("Test prompt", None).await.unwrap();
        
        let mut content = String::new();
        while let Some(chunk) = stream.next().await {
            let chunk = chunk.unwrap();
            content.push_str(&chunk.content);
            if chunk.done {
                break;
            }
        }
        
        assert_eq!(content, "Hello, world!");
    }
}
