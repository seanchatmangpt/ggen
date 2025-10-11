//! AI client initialization and management
//!
//! This module handles initialization of AI clients for different providers
//! (OpenAI, Anthropic, Ollama) and provides access to them.

use std::collections::HashMap;
use std::sync::Arc;
use tokio::sync::OnceCell;
use ggen_ai::{LlmConfig, LlmClient, MockClient};
use crate::error::Result;

/// Global AI clients for autonomous operation
static AI_CLIENTS: OnceCell<HashMap<String, Box<dyn LlmClient + Send + Sync>>> = OnceCell::const_new();

/// Initialize AI clients for autonomous operation
async fn init_ai_clients() -> Result<HashMap<String, Box<dyn LlmClient + Send + Sync>>> {
    let mut clients = HashMap::new();

    // Initialize OpenAI client if API key is available
    if std::env::var("OPENAI_API_KEY").is_ok() {
        let config = LlmConfig {
            model: "gpt-4o".to_string(),
            max_tokens: Some(4096),
            temperature: Some(0.7),
            top_p: Some(0.9),
            stop: None,
            extra: HashMap::new(),
        };
        let client = ggen_ai::GenAiClient::new(config).map_err(|e| {
            crate::error::GgenMcpError::Configuration(format!("OpenAI client init failed: {}", e))
        })?;
        clients.insert("openai".to_string(), Box::new(client) as Box<dyn LlmClient + Send + Sync>);
    }

    // Initialize Anthropic client if API key is available
    if std::env::var("ANTHROPIC_API_KEY").is_ok() {
        let config = LlmConfig {
            model: "claude-3-5-sonnet-20241022".to_string(),
            max_tokens: Some(4096),
            temperature: Some(0.7),
            top_p: Some(0.9),
            stop: None,
            extra: HashMap::new(),
        };
        let client = ggen_ai::GenAiClient::new(config).map_err(|e| {
            crate::error::GgenMcpError::Configuration(format!("Anthropic client init failed: {}", e))
        })?;
        clients.insert("anthropic".to_string(), Box::new(client) as Box<dyn LlmClient + Send + Sync>);
    }

    // Initialize Ollama client if available
    if std::env::var("OLLAMA_BASE_URL").is_ok() {
        let config = LlmConfig {
            model: "qwen2.5-coder:32b".to_string(),
            max_tokens: Some(4096),
            temperature: Some(0.3),
            top_p: Some(0.9),
            stop: None,
            extra: HashMap::new(),
        };
        let client = ggen_ai::GenAiClient::new(config).map_err(|e| {
            crate::error::GgenMcpError::Configuration(format!("Ollama client init failed: {}", e))
        })?;
        clients.insert("ollama".to_string(), Box::new(client) as Box<dyn LlmClient + Send + Sync>);
    }

    // Always provide a fallback mock client for testing
    if clients.is_empty() {
        let mock_client = MockClient::with_response("Mock AI response");
        clients.insert("mock".to_string(), Box::new(mock_client) as Box<dyn LlmClient + Send + Sync>);
    }

    Ok(clients)
}

/// Get AI client for specified provider
pub async fn get_ai_client(provider: &str) -> Result<Arc<dyn LlmClient>> {
    let clients = AI_CLIENTS.get_or_try_init(init_ai_clients).await?;

    // For simplicity, recreate clients as Arc
    // In production, implement a proper client pool or factory pattern
    if clients.contains_key(provider) {
        match provider {
            "mock" => Ok(Arc::new(MockClient::with_response("Mock AI response"))),
            "openai" => {
                let config = LlmConfig {
                    model: "gpt-4o".to_string(),
                    max_tokens: Some(4096),
                    temperature: Some(0.7),
                    top_p: Some(0.9),
                    stop: None,
                    extra: HashMap::new(),
                };
                let client = ggen_ai::GenAiClient::new(config).map_err(|e| {
                    crate::error::GgenMcpError::Configuration(format!("OpenAI client recreation failed: {}", e))
                })?;
                Ok(Arc::new(client))
            },
            "anthropic" => {
                let config = LlmConfig {
                    model: "claude-3-5-sonnet-20241022".to_string(),
                    max_tokens: Some(4096),
                    temperature: Some(0.7),
                    top_p: Some(0.9),
                    stop: None,
                    extra: HashMap::new(),
                };
                let client = ggen_ai::GenAiClient::new(config).map_err(|e| {
                    crate::error::GgenMcpError::Configuration(format!("Anthropic client recreation failed: {}", e))
                })?;
                Ok(Arc::new(client))
            },
            "ollama" => {
                let config = LlmConfig {
                    model: "qwen2.5-coder:32b".to_string(),
                    max_tokens: Some(4096),
                    temperature: Some(0.3),
                    top_p: Some(0.9),
                    stop: None,
                    extra: HashMap::new(),
                };
                let client = ggen_ai::GenAiClient::new(config).map_err(|e| {
                    crate::error::GgenMcpError::Configuration(format!("Ollama client recreation failed: {}", e))
                })?;
                Ok(Arc::new(client))
            },
            _ => Err(crate::error::GgenMcpError::Configuration(
                format!("AI provider '{}' not supported", provider)
            ))
        }
    } else {
        Err(crate::error::GgenMcpError::Configuration(
            format!("AI provider '{}' not available. Available: {:?}", provider, clients.keys().collect::<Vec<_>>())
        ))
    }
}

/// Get available AI providers and models
pub async fn list_providers(_params: serde_json::Value) -> Result<serde_json::Value> {
    let clients = AI_CLIENTS.get_or_try_init(init_ai_clients).await?;

    let mut providers = Vec::new();

    for (name, _client) in clients {
        providers.push(serde_json::json!({
            "name": name,
            "status": "available",
            "type": if name == "ollama" { "local" } else { "cloud" }
        }));
    }

    Ok(crate::error::success_response(serde_json::json!({
        "providers": providers,
        "total_providers": providers.len()
    })))
}
