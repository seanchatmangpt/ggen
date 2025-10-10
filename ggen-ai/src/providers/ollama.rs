//! Ollama provider implementation for local models

use async_trait::async_trait;
use futures::StreamExt;
use reqwest::Client;
use serde::{Deserialize, Serialize};

use crate::client::{LlmClient, LlmConfig, LlmResponse, LlmChunk, UsageStats};
use crate::config::OllamaConfig;
use crate::error::{GgenAiError, Result};

/// Ollama API client for local models
/// Client for interacting with Ollama local LLM service
#[derive(Debug)]
pub struct OllamaClient {
    client: Client,
    base_url: String,
}

impl OllamaClient {
    /// Create a new Ollama client from configuration
    pub fn new(config: OllamaConfig) -> Result<Self> {
        config.validate()?;
        Ok(Self {
            client: Client::new(),
            base_url: config.base_url,
        })
    }

    /// Create a new Ollama client from environment variables
    pub fn from_env() -> Result<Self> {
        let config = OllamaConfig::from_env()?;
        Self::new(config)
    }

    /// Create a new Ollama client with default settings (deprecated - use new() with config)
    #[deprecated(since = "0.2.4", note = "Use new() with OllamaConfig instead")]
    pub fn with_defaults() -> Self {
        Self {
            client: Client::new(),
            base_url: "http://localhost:11434".to_string(),
        }
    }

    /// Create a new Ollama client with custom base URL (deprecated - use new() with config)
    #[deprecated(since = "0.2.4", note = "Use new() with OllamaConfig instead")]
    pub fn with_base_url(base_url: String) -> Self {
        Self {
            client: Client::new(),
            base_url,
        }
    }

    /// Create a configuration optimized for qwen3-coder:30b
    pub fn qwen3_coder_config() -> LlmConfig {
        LlmConfig {
            model: "qwen3-coder:30b".to_string(),
            max_tokens: Some(4096),
            temperature: Some(0.1), // Lower temperature for more deterministic code generation
            top_p: Some(0.9),
            stop: None, // Remove stop sequences to allow full template generation
            extra: std::collections::HashMap::new(),
        }
    }
}

#[derive(Serialize)]
struct GenerateRequest {
    model: String,
    prompt: String,
    stream: bool,
    options: Option<GenerateOptions>,
}

#[derive(Serialize)]
struct GenerateOptions {
    temperature: Option<f32>,
    top_p: Option<f32>,
    stop: Option<Vec<String>>,
    num_predict: Option<u32>,
}

#[derive(Deserialize)]
#[allow(dead_code)]
struct GenerateResponse {
    model: String,
    created_at: String,
    response: String,
    done: bool,
    context: Option<Vec<u32>>,
    total_duration: Option<u64>,
    load_duration: Option<u64>,
    prompt_eval_count: Option<u32>,
    prompt_eval_duration: Option<u64>,
    eval_count: Option<u32>,
    eval_duration: Option<u64>,
}

#[derive(Serialize, Deserialize)]
struct EmbeddingRequest {
    model: String,
    prompt: String,
}

#[derive(Deserialize)]
struct EmbeddingResponse {
    embedding: Vec<f32>,
}

#[async_trait]
impl LlmClient for OllamaClient {
    async fn complete(&self, prompt: &str, config: Option<LlmConfig>) -> Result<LlmResponse> {
        let config = config.unwrap_or_default();

        // Validate configuration
        config.validate()?;
        if let Some(temp) = config.temperature {
            GgenAiError::validate_temperature(temp, "temperature")?;
        }
        if let Some(top_p) = config.top_p {
            GgenAiError::validate_top_p(top_p, "top_p")?;
        }

        let model_name = config.model.clone();
        let request = GenerateRequest {
            model: config.model,
            prompt: prompt.to_string(),
            stream: false,
            options: Some(GenerateOptions {
                temperature: config.temperature,
                top_p: config.top_p,
                stop: config.stop,
                num_predict: config.max_tokens,
            }),
        };
        
        let response = self
            .client
            .post(&format!("{}/api/generate", self.base_url))
            .json(&request)
            .send()
            .await
            .map_err(|e| {
                if e.is_timeout() {
                    GgenAiError::timeout("ollama", 30)
                } else if e.is_connect() {
                    GgenAiError::network_error(format!(
                        "Failed to connect to Ollama at {}. Is Ollama running? Start with: ollama serve",
                        self.base_url
                    ))
                } else {
                    GgenAiError::network_error(e.to_string())
                }
            })?;

        if !response.status().is_success() {
            let status = response.status().as_u16();
            let error_text = response.text().await.unwrap_or_default();

            return Err(if status == 404 {
                GgenAiError::ollama(format!(
                    "Model '{}' not found. Pull it with: ollama pull {}",
                    model_name, model_name
                ))
            } else {
                GgenAiError::ollama(format!("API request failed ({}): {}", status, error_text))
            });
        }

        let generate_response: GenerateResponse = response.json().await
            .map_err(|e| GgenAiError::parse_error("ollama", format!("Failed to parse response: {}", e)))?;
        
        let usage = UsageStats {
            prompt_tokens: generate_response.prompt_eval_count.unwrap_or(0),
            completion_tokens: generate_response.eval_count.unwrap_or(0),
            total_tokens: generate_response.prompt_eval_count.unwrap_or(0) 
                + generate_response.eval_count.unwrap_or(0),
        };
        
        Ok(LlmResponse {
            content: generate_response.response,
            usage: Some(usage),
            model: generate_response.model,
            finish_reason: if generate_response.done { Some("stop".to_string()) } else { None },
        })
    }
    
    async fn stream_complete(
        &self,
        prompt: &str,
        config: Option<LlmConfig>,
    ) -> Result<futures::stream::BoxStream<'static, Result<LlmChunk>>> {
        let config = config.unwrap_or_default();

        // Validate configuration
        config.validate()?;
        if let Some(temp) = config.temperature {
            GgenAiError::validate_temperature(temp, "temperature")?;
        }
        if let Some(top_p) = config.top_p {
            GgenAiError::validate_top_p(top_p, "top_p")?;
        }

        let model_name = config.model.clone();
        let request = GenerateRequest {
            model: config.model,
            prompt: prompt.to_string(),
            stream: true,
            options: Some(GenerateOptions {
                temperature: config.temperature,
                top_p: config.top_p,
                stop: config.stop,
                num_predict: config.max_tokens,
            }),
        };
        
        let response = self
            .client
            .post(&format!("{}/api/generate", self.base_url))
            .json(&request)
            .send()
            .await
            .map_err(|e| {
                if e.is_timeout() {
                    GgenAiError::timeout("ollama", 30)
                } else if e.is_connect() {
                    GgenAiError::network_error(format!(
                        "Failed to connect to Ollama at {}. Is Ollama running? Start with: ollama serve",
                        self.base_url
                    ))
                } else {
                    GgenAiError::network_error(e.to_string())
                }
            })?;

        if !response.status().is_success() {
            let status = response.status().as_u16();
            let error_text = response.text().await.unwrap_or_default();

            return Err(if status == 404 {
                GgenAiError::ollama(format!(
                    "Model '{}' not found. Pull it with: ollama pull {}",
                    model_name, model_name
                ))
            } else {
                GgenAiError::ollama(format!("Streaming request failed ({}): {}", status, error_text))
            });
        }
        
        let stream = response
            .bytes_stream()
            .map(|chunk_result| {
                let chunk = chunk_result?;
                let text = String::from_utf8(chunk.to_vec())?;
                
                // Parse JSON lines format
                for line in text.lines() {
                    if let Ok(generate_response) = serde_json::from_str::<GenerateResponse>(line) {
                        let done = generate_response.done;
                        let content = generate_response.response;
                        
                        return Ok(LlmChunk {
                            content,
                            done,
                            usage: if done {
                                Some(UsageStats {
                                    prompt_tokens: generate_response.prompt_eval_count.unwrap_or(0),
                                    completion_tokens: generate_response.eval_count.unwrap_or(0),
                                    total_tokens: generate_response.prompt_eval_count.unwrap_or(0) 
                                        + generate_response.eval_count.unwrap_or(0),
                                })
                            } else {
                                None
                            },
                        });
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
    
    async fn embed(&self, prompt: &str) -> Result<Vec<f32>> {
        let request = EmbeddingRequest {
            model: "nomic-embed-text".to_string(), // Default embedding model
            prompt: prompt.to_string(),
        };
        
        let response = self
            .client
            .post(&format!("{}/api/embeddings", self.base_url))
            .json(&request)
            .send()
            .await
            .map_err(|e| {
                if e.is_timeout() {
                    GgenAiError::timeout("ollama", 30)
                } else if e.is_connect() {
                    GgenAiError::network_error(format!(
                        "Failed to connect to Ollama at {}. Is Ollama running? Start with: ollama serve",
                        self.base_url
                    ))
                } else {
                    GgenAiError::network_error(e.to_string())
                }
            })?;

        if !response.status().is_success() {
            let status = response.status().as_u16();
            let error_text = response.text().await.unwrap_or_default();

            return Err(if status == 404 {
                GgenAiError::ollama(
                    "Embedding model 'nomic-embed-text' not found. Pull it with: ollama pull nomic-embed-text".to_string()
                )
            } else {
                GgenAiError::ollama(format!("Embedding request failed ({}): {}", status, error_text))
            });
        }

        let embedding_response: EmbeddingResponse = response.json().await
            .map_err(|e| GgenAiError::parse_error("ollama", format!("Failed to parse embedding response: {}", e)))?;
        Ok(embedding_response.embedding)
    }
    
    fn provider_name(&self) -> &str {
        "ollama"
    }
    
    fn supported_models(&self) -> Vec<String> {
        vec![
            "qwen3-coder:30b".to_string(),
            "llama2".to_string(),
            "codellama".to_string(),
            "mistral".to_string(),
            "neural-chat".to_string(),
            "starling-lm".to_string(),
            "nomic-embed-text".to_string(),
        ]
    }
}

impl Default for OllamaClient {
    fn default() -> Self {
        Self::new(OllamaConfig::default()).expect("Failed to create default Ollama client")
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    
    #[tokio::test]
    async fn test_ollama_client_creation() {
        let config = OllamaConfig::new();
        let client = OllamaClient::new(config).unwrap();
        assert_eq!(client.provider_name(), "ollama");
        assert!(client.supports_model("llama2"));
        assert!(!client.supports_model("unknown-model"));
    }

    #[tokio::test]
    async fn test_ollama_client_with_base_url() {
        let config = OllamaConfig::new().with_base_url("http://custom:11434");
        let client = OllamaClient::new(config).unwrap();
        assert_eq!(client.base_url, "http://custom:11434");
    }
}
