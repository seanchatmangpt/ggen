//! LLM Streaming Support via rust-genai
//!
//! This module provides type aliases and utilities for working with LLM streaming responses.
//! The actual streaming implementation uses genai's native streaming capabilities.
//!
//! ## Usage
//!
//! For streaming responses, use `LlmClient::complete_stream()` which returns a stream of `LlmChunk`:
//!
//! ```rust,no_run
//! use ggen_ai::{GenAiClient, LlmClient, LlmConfig};
//! use futures::StreamExt;
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let client = GenAiClient::new(LlmConfig::default())?;
//! let mut stream = client.complete_stream("Hello, world!").await?;
//!
//! while let Some(chunk) = stream.next().await {
//!     print!("{}", chunk.content);
//! }
//! # Ok(())
//! # }
//! ```
//!
//! The genai library natively supports streaming for all major providers:
//! - OpenAI (including tool calls and reasoning chunks)
//! - Anthropic Claude
//! - Google Gemini
//! - Ollama (local models)
//! - Groq, xAI/Grok, DeepSeek, Cohere
//!
//! No custom streaming implementation needed - genai handles everything!

use serde::{Deserialize, Serialize};

// Re-export the streaming chunk type from client module
pub use crate::client::LlmChunk as StreamChunk;

/// Configuration for streaming behavior
///
/// Note: Most streaming configuration is handled by the provider's native API.
/// This config is primarily for application-level concerns like buffering.
#[derive(Debug, Clone)]
pub struct StreamConfig {
    /// Buffer size hint for stream processing
    pub buffer_size: usize,
    /// Timeout for receiving chunks (application-level)
    pub chunk_timeout: std::time::Duration,
}

impl Default for StreamConfig {
    fn default() -> Self {
        Self {
            buffer_size: 10,
            chunk_timeout: std::time::Duration::from_secs(30),
        }
    }
}

/// Metadata for streaming chunks (deprecated - use LlmChunk directly)
#[deprecated(
    since = "0.2.0",
    note = "Use LlmChunk from client module instead. This will be removed in 0.3.0"
)]
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StreamMetadata {
    pub model: String,
    pub tokens_used: Option<usize>,
    pub finish_reason: Option<String>,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stream_config_default() {
        let config = StreamConfig::default();
        assert_eq!(config.buffer_size, 10);
        assert_eq!(config.chunk_timeout.as_secs(), 30);
    }
}
