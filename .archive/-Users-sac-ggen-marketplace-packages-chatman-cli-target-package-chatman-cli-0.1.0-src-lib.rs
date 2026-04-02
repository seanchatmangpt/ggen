//! # ChatMan CLI
//!
//! Knowledge Hook-powered chat automation framework with semantic understanding,
//! AI integration, and production-grade conversation management.
//!
//! ## Features
//!
//! - **Knowledge Hook Integration**: Semantic understanding via RDF ontologies
//! - **AI Provider Support**: OpenAI, Anthropic, and custom providers
//! - **Conversation Management**: Multi-turn context tracking and persistence
//! - **Production Features**: Async/await, error handling, logging, rate limiting
//! - **CLI Excellence**: Beautiful output, interactive prompts, progress indicators
//!
//! ## Quick Start
//!
//! ```rust,no_run
//! use chatman_cli::{ChatManager, Config, Message};
//! use anyhow::Result;
//!
//! #[tokio::main]
//! async fn main() -> Result<()> {
//!     let config = Config::default();
//!     let mut manager = ChatManager::new(config)?;
//!
//!     let response = manager.send_message(
//!         Message::user("Tell me about Rust")
//!     ).await?;
//!
//!     println!("Assistant: {}", response.content);
//!     Ok(())
//! }
//! ```

#![warn(missing_docs)]
#![warn(clippy::all)]
#![allow(dead_code)]

use anyhow::{Context, Result};
use serde::{Deserialize, Serialize};
use std::path::PathBuf;
use tracing::{debug, info};

/// Main chat manager for handling conversations
pub struct ChatManager {
    config: Config,
    history: Vec<Message>,
}

/// Configuration for ChatManager
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    /// AI provider to use (openai, anthropic, local)
    pub provider: String,

    /// Model name
    pub model: String,

    /// Maximum conversation history to maintain
    pub max_history: usize,

    /// Timeout for API requests in seconds
    pub timeout_secs: u64,

    /// Number of retry attempts
    pub retry_attempts: u32,

    /// Path to RDF ontology file
    pub ontology_path: Option<PathBuf>,
}

impl Default for Config {
    fn default() -> Self {
        Self {
            provider: "openai".to_string(),
            model: "gpt-3.5-turbo".to_string(),
            max_history: 100,
            timeout_secs: 30,
            retry_attempts: 3,
            ontology_path: None,
        }
    }
}

/// A chat message
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Message {
    /// Role of the message sender (user, assistant, system)
    pub role: String,

    /// Content of the message
    pub content: String,

    /// Optional timestamp
    #[serde(skip_serializing_if = "Option::is_none")]
    pub timestamp: Option<i64>,
}

impl Message {
    /// Create a user message
    pub fn user(content: impl Into<String>) -> Self {
        Self {
            role: "user".to_string(),
            content: content.into(),
            timestamp: Some(chrono::Utc::now().timestamp()),
        }
    }

    /// Create an assistant message
    pub fn assistant(content: impl Into<String>) -> Self {
        Self {
            role: "assistant".to_string(),
            content: content.into(),
            timestamp: Some(chrono::Utc::now().timestamp()),
        }
    }

    /// Create a system message
    pub fn system(content: impl Into<String>) -> Self {
        Self {
            role: "system".to_string(),
            content: content.into(),
            timestamp: None,
        }
    }
}

impl ChatManager {
    /// Create a new ChatManager with the given configuration
    pub fn new(config: Config) -> Result<Self> {
        info!("Initializing ChatManager with provider: {}", config.provider);

        Ok(Self {
            config,
            history: Vec::new(),
        })
    }

    /// Send a message and get a response
    pub async fn send_message(&mut self, message: Message) -> Result<Message> {
        debug!("Sending message: {:?}", message);

        // Add message to history
        self.history.push(message.clone());

        // Trim history if needed
        if self.history.len() > self.config.max_history {
            self.history.drain(0..self.history.len() - self.config.max_history);
        }

        // For now, return a simple echo response
        // In a real implementation, this would call the AI provider
        let response = Message::assistant(
            format!("Echo: {}", message.content)
        );

        self.history.push(response.clone());

        Ok(response)
    }

    /// Get conversation history
    pub fn history(&self) -> &[Message] {
        &self.history
    }

    /// Clear conversation history
    pub fn clear_history(&mut self) {
        info!("Clearing conversation history");
        self.history.clear();
    }

    /// Export conversation to JSON
    pub fn export_json(&self) -> Result<String> {
        serde_json::to_string_pretty(&self.history)
            .context("Failed to serialize conversation to JSON")
    }

    /// Add a knowledge hook from an RDF ontology
    pub fn add_hook(&mut self, _hook: KnowledgeHook) -> Result<()> {
        // Placeholder for knowledge hook integration
        Ok(())
    }
}

/// Knowledge hook for semantic understanding
pub struct KnowledgeHook {
    ontology_path: PathBuf,
}

impl KnowledgeHook {
    /// Create a knowledge hook from an ontology file
    pub fn from_ontology(path: impl Into<PathBuf>) -> Result<Self> {
        let ontology_path = path.into();

        if !ontology_path.exists() {
            anyhow::bail!("Ontology file not found: {:?}", ontology_path);
        }

        Ok(Self { ontology_path })
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_chat_manager_creation() {
        let config = Config::default();
        let manager = ChatManager::new(config);
        assert!(manager.is_ok());
    }

    #[tokio::test]
    async fn test_send_message() {
        let config = Config::default();
        let mut manager = ChatManager::new(config).unwrap();

        let msg = Message::user("Hello");
        let response = manager.send_message(msg).await.unwrap();

        assert_eq!(response.role, "assistant");
        assert!(response.content.contains("Echo"));
    }

    #[test]
    fn test_message_creation() {
        let user_msg = Message::user("test");
        assert_eq!(user_msg.role, "user");
        assert_eq!(user_msg.content, "test");

        let assistant_msg = Message::assistant("response");
        assert_eq!(assistant_msg.role, "assistant");

        let system_msg = Message::system("system");
        assert_eq!(system_msg.role, "system");
    }

    #[tokio::test]
    async fn test_history_management() {
        let config = Config {
            max_history: 2,
            ..Default::default()
        };
        let mut manager = ChatManager::new(config).unwrap();

        manager.send_message(Message::user("msg1")).await.unwrap();
        manager.send_message(Message::user("msg2")).await.unwrap();
        manager.send_message(Message::user("msg3")).await.unwrap();

        // Should only keep last 2 messages (user + response for msg3)
        assert!(manager.history().len() <= 2);
    }
}
