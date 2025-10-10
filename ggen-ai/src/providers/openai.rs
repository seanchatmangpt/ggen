//! OpenAI provider implementation

use async_trait::async_trait;
use futures::StreamExt;
use reqwest::Client;
use serde::{Deserialize, Serialize};

use crate::client::{LlmClient, LlmConfig, LlmResponse, LlmChunk, UsageStats};
use crate::error::{GgenAiError, Result};

/// OpenAI API client
#[derive(Debug)]
pub struct OpenAIClient {
    client: Client,
    api_key: String,
    base_url: String,
}

impl OpenAIClient {
    /// Create a new OpenAI client
    pub fn new(api_key: String) -> Self {
        Self {
            client: Client::new(),
            api_key,
            base_url: "https://api.openai.com/v1".to_string(),
        }
    }
    
    /// Create a new OpenAI client with custom base URL
    pub fn with_base_url(api_key: String, base_url: String) -> Self {
        Self {
            client: Client::new(),
            api_key,
            base_url,
        }
    }
}

#[derive(Serialize)]
struct ChatCompletionRequest {
    model: String,
    messages: Vec<Message>,
    max_tokens: Option<u32>,
    temperature: Option<f32>,
    top_p: Option<f32>,
    stop: Option<Vec<String>>,
    stream: bool,
}

#[derive(Serialize, Deserialize)]
struct Message {
    role: String,
    content: String,
}

#[derive(Deserialize)]
#[allow(dead_code)]
struct ChatCompletionResponse {
    id: String,
    object: String,
    created: u64,
    model: String,
    choices: Vec<Choice>,
    usage: Option<Usage>,
}

#[derive(Deserialize)]
#[allow(dead_code)]
struct Choice {
    index: u32,
    message: Option<Message>,
    delta: Option<Delta>,
    finish_reason: Option<String>,
}

#[derive(Deserialize)]
#[allow(dead_code)]
struct Delta {
    role: Option<String>,
    content: Option<String>,
}

#[derive(Deserialize)]
struct Usage {
    prompt_tokens: u32,
    completion_tokens: u32,
    total_tokens: u32,
}

#[derive(Deserialize)]
#[allow(dead_code)]
struct EmbeddingResponse {
    data: Vec<EmbeddingData>,
    model: String,
    usage: Usage,
}

#[derive(Deserialize)]
#[allow(dead_code)]
struct EmbeddingData {
    embedding: Vec<f32>,
    index: u32,
}

#[async_trait]
impl LlmClient for OpenAIClient {
    async fn complete(&self, prompt: &str, config: Option<LlmConfig>) -> Result<LlmResponse> {
        let config = config.unwrap_or_default();
        
        let request = ChatCompletionRequest {
            model: config.model,
            messages: vec![Message {
                role: "user".to_string(),
                content: prompt.to_string(),
            }],
            max_tokens: config.max_tokens,
            temperature: config.temperature,
            top_p: config.top_p,
            stop: config.stop,
            stream: false,
        };
        
        let response = self
            .client
            .post(&format!("{}/chat/completions", self.base_url))
            .header("Authorization", format!("Bearer {}", self.api_key))
            .header("Content-Type", "application/json")
            .json(&request)
            .send()
            .await?;
        
        if !response.status().is_success() {
            let error_text = response.text().await.unwrap_or_default();
            return Err(GgenAiError::llm_provider(format!(
                "OpenAI API error: {}",
                error_text
            )));
        }
        
        let completion: ChatCompletionResponse = response.json().await?;
        
        let content = completion
            .choices
            .first()
            .and_then(|choice| choice.message.as_ref())
            .map(|msg| msg.content.clone())
            .unwrap_or_default();
        
        let usage = completion.usage.map(|u| UsageStats {
            prompt_tokens: u.prompt_tokens,
            completion_tokens: u.completion_tokens,
            total_tokens: u.total_tokens,
        });
        
        Ok(LlmResponse {
            content,
            usage,
            model: completion.model,
            finish_reason: completion
                .choices
                .first()
                .and_then(|choice| choice.finish_reason.clone()),
        })
    }
    
    async fn stream_complete(
        &self,
        prompt: &str,
        config: Option<LlmConfig>,
    ) -> Result<futures::stream::BoxStream<'static, Result<LlmChunk>>> {
        let config = config.unwrap_or_default();
        
        let request = ChatCompletionRequest {
            model: config.model,
            messages: vec![Message {
                role: "user".to_string(),
                content: prompt.to_string(),
            }],
            max_tokens: config.max_tokens,
            temperature: config.temperature,
            top_p: config.top_p,
            stop: config.stop,
            stream: true,
        };
        
        let response = self
            .client
            .post(&format!("{}/chat/completions", self.base_url))
            .header("Authorization", format!("Bearer {}", self.api_key))
            .header("Content-Type", "application/json")
            .json(&request)
            .send()
            .await?;
        
        if !response.status().is_success() {
            let error_text = response.text().await.unwrap_or_default();
            return Err(GgenAiError::llm_provider(format!(
                "OpenAI API error: {}",
                error_text
            )));
        }
        
        let stream = response
            .bytes_stream()
            .map(|chunk_result| {
                let chunk = chunk_result?;
                let text = String::from_utf8(chunk.to_vec())?;
                
                // Parse SSE format
                for line in text.lines() {
                    if line.starts_with("data: ") {
                        let data = &line[6..];
                        if data == "[DONE]" {
                            return Ok(LlmChunk {
                                content: String::new(),
                                done: true,
                                usage: None,
                            });
                        }
                        
                        if let Ok(completion) = serde_json::from_str::<ChatCompletionResponse>(data) {
                            if let Some(choice) = completion.choices.first() {
                                if let Some(delta) = &choice.delta {
                                    if let Some(content) = &delta.content {
                                        return Ok(LlmChunk {
                                            content: content.clone(),
                                            done: false,
                                            usage: None,
                                        });
                                    }
                                }
                            }
                        }
                    }
                }
                
                Ok(LlmChunk {
                    content: String::new(),
                    done: false,
                    usage: None,
                })
            });
        
        Ok(Box::pin(stream))
    }
    
    async fn embed(&self, text: &str) -> Result<Vec<f32>> {
        let request = serde_json::json!({
            "input": text,
            "model": "text-embedding-ada-002"
        });
        
        let response = self
            .client
            .post(&format!("{}/embeddings", self.base_url))
            .header("Authorization", format!("Bearer {}", self.api_key))
            .header("Content-Type", "application/json")
            .json(&request)
            .send()
            .await?;
        
        if !response.status().is_success() {
            let error_text = response.text().await.unwrap_or_default();
            return Err(GgenAiError::llm_provider(format!(
                "OpenAI API error: {}",
                error_text
            )));
        }
        
        let embedding_response: EmbeddingResponse = response.json().await?;
        
        embedding_response
            .data
            .first()
            .map(|data| data.embedding.clone())
            .ok_or_else(|| GgenAiError::llm_provider("No embedding data returned"))
    }
    
    fn provider_name(&self) -> &str {
        "openai"
    }
    
    fn supported_models(&self) -> Vec<String> {
        vec![
            "gpt-4".to_string(),
            "gpt-4-turbo".to_string(),
            "gpt-3.5-turbo".to_string(),
            "gpt-3.5-turbo-16k".to_string(),
        ]
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_openai_client_creation() {
        let client = OpenAIClient::new("test-key".to_string());
        assert_eq!(client.provider_name(), "openai");
        assert!(client.supports_model("gpt-3.5-turbo"));
        assert!(!client.supports_model("unknown-model"));
    }
    
    #[tokio::test]
    async fn test_openai_client_with_base_url() {
        let client = OpenAIClient::with_base_url(
            "test-key".to_string(),
            "https://custom.openai.com".to_string(),
        );
        assert_eq!(client.base_url, "https://custom.openai.com");
    }
}
