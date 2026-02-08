//! Rig LLM Framework Integration
//!
//! This module provides integration between MCP transport and the Rig LLM framework.
//! It is feature-gated behind the "rig-integration" feature.

use crate::{A2AServerClient, Config, EmbeddingConfig, ProviderConfig};
use anyhow::Result;
use rig_core::{
    agent::AgentBuilder,
    completion::CompletionModel,
    embeddings::EmbeddingModel,
    providers::{anthropic, cohere, deepseek, gemini, ollama, openai},
};
use std::collections::HashMap;
use tokio::sync::RwLock;

/// Main Rig MCP client
///
/// Combines Rig LLM providers with MCP tool access via transport layer.
pub struct RigMcpClient {
    config: Config,
    providers: RwLock<HashMap<String, Box<dyn CompletionModel>>>,
    embeddings: Option<Box<dyn EmbeddingModel>>,
    a2a_clients: Vec<A2AServerClient>,
}

impl RigMcpClient {
    /// Create a new Rig MCP client from configuration
    pub async fn new(config: Config) -> Result<Self> {
        let mut providers = HashMap::new();
        let mut a2a_clients = Vec::new();

        // Initialize LLM providers
        for provider_config in &config.providers {
            let provider = Self::create_provider(provider_config).await?;
            providers.insert(provider_config.name.clone(), provider);
        }

        // Initialize embedding model
        let embeddings = if !config.embeddings.model.is_empty() {
            Some(Self::create_embedding_model(&config.embeddings).await?)
        } else {
            None
        };

        // Initialize A2A clients
        for server_config in &config.mcp_servers {
            let a2a_client = A2AServerClient::new(server_config.url.clone())?;
            a2a_clients.push(a2a_client);
        }

        Ok(Self {
            config,
            providers: RwLock::new(providers),
            embeddings,
            a2a_clients,
        })
    }

    /// Create an agent for the specified provider
    pub async fn agent(&self, provider_name: &str) -> Result<AgentBuilder> {
        let providers = self.providers.read().await;
        let provider = providers.get(provider_name)
            .ok_or_else(|| anyhow::anyhow!("Provider '{}' not found", provider_name))?;

        let mut builder = AgentBuilder::new(provider.as_ref().clone());

        // Add A2A tools if available
        for a2a_client in &self.a2a_clients {
            match a2a_client.get_status().await {
                Ok(status) => {
                    println!("Connected to A2A server: {} - Status: {}", a2a_client.base_url, status);
                }
                Err(e) => {
                    println!("Failed to connect to A2A server {}: {}", a2a_client.base_url, e);
                }
            }
        }

        Ok(builder)
    }

    /// Add an A2A server client
    pub async fn add_a2a_server(&mut self, server_url: String) -> Result<()> {
        let a2a_client = A2AServerClient::new(server_url.clone())?;
        self.a2a_clients.push(a2a_client);
        Ok(())
    }

    /// Get all connected A2A servers
    pub async fn list_a2a_servers(&self) -> Result<Vec<String>> {
        Ok(self.a2a_clients.iter().map(|client| client.base_url.clone()).collect())
    }

    /// Create a provider instance
    async fn create_provider(config: &ProviderConfig) -> Result<Box<dyn CompletionModel>> {
        match config.name.as_str() {
            "openai" => {
                let client = openai::Client::new(&config.api_key.as_ref().unwrap())?;
                Ok(Box::new(client.model(&config.model)))
            }
            "anthropic" => {
                let client = anthropic::Client::new(&config.api_key.as_ref().unwrap())?;
                Ok(Box::new(client.model(&config.model)))
            }
            "cohere" => {
                let client = cohere::Client::new(&config.api_key.as_ref().unwrap())?;
                Ok(Box::new(client.model(&config.model)))
            }
            "ollama" => {
                let base_url = config.base_url.as_deref().unwrap_or("http://localhost:11434");
                let client = ollama::Client::new(base_url)?;
                Ok(Box::new(client.model(&config.model)))
            }
            "deepseek" => {
                let client = deepseek::Client::new(&config.api_key.as_ref().unwrap())?;
                Ok(Box::new(client.model(&config.model)))
            }
            "gemini" => {
                let client = gemini::Client::new(&config.api_key.as_ref().unwrap())?;
                Ok(Box::new(client.model(&config.model)))
            }
            _ => Err(anyhow::anyhow!("Unknown provider: {}", config.name)),
        }
    }

    /// Create embedding model
    async fn create_embedding_model(config: &EmbeddingConfig) -> Result<Box<dyn EmbeddingModel>> {
        match config.provider.as_str() {
            "openai" => {
                let client = openai::Client::new(&config.api_key.as_ref().unwrap())?;
                Ok(Box::new(client.embedding_model(&config.model)))
            }
            "cohere" => {
                let client = cohere::Client::new(&config.api_key.as_ref().unwrap())?;
                Ok(Box::new(client.embedding_model(&config.model)))
            }
            _ => Err(anyhow::anyhow!("Unknown embedding provider: {}", config.provider)),
        }
    }
}
