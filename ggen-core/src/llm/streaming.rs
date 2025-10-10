//! Streaming response handling
//!
//! Provides utilities for handling streaming LLM responses with async/await
//! and proper error handling.

use serde::{Deserialize, Serialize};

/// A chunk of streamed response data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct StreamChunk {
    /// Incremental content from the stream
    pub content: String,
    /// Whether this is the final chunk
    pub is_final: bool,
    /// Optional finish reason for final chunk
    #[serde(skip_serializing_if = "Option::is_none")]
    pub finish_reason: Option<String>,
    /// Delta token count for this chunk
    #[serde(skip_serializing_if = "Option::is_none")]
    pub tokens: Option<u32>,
}

impl StreamChunk {
    /// Create a new stream chunk
    pub fn new(content: impl Into<String>) -> Self {
        Self {
            content: content.into(),
            is_final: false,
            finish_reason: None,
            tokens: None,
        }
    }

    /// Create a final stream chunk
    pub fn final_chunk(
        content: impl Into<String>,
        finish_reason: impl Into<String>,
    ) -> Self {
        Self {
            content: content.into(),
            is_final: true,
            finish_reason: Some(finish_reason.into()),
            tokens: None,
        }
    }

    /// Set token count
    pub fn with_tokens(mut self, tokens: u32) -> Self {
        self.tokens = Some(tokens);
        self
    }
}

/// Handler for accumulating streamed responses
#[derive(Default)]
pub struct StreamHandler {
    accumulated: String,
    total_tokens: u32,
}

impl StreamHandler {
    /// Create a new stream handler
    pub fn new() -> Self {
        Self::default()
    }

    /// Process a stream chunk and accumulate content
    pub fn handle_chunk(&mut self, chunk: &StreamChunk) {
        self.accumulated.push_str(&chunk.content);
        if let Some(tokens) = chunk.tokens {
            self.total_tokens += tokens;
        }
    }

    /// Get the accumulated content
    pub fn content(&self) -> &str {
        &self.accumulated
    }

    /// Get total tokens processed
    pub fn total_tokens(&self) -> u32 {
        self.total_tokens
    }

    /// Reset the handler
    pub fn reset(&mut self) {
        self.accumulated.clear();
        self.total_tokens = 0;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_stream_chunk_creation() {
        let chunk = StreamChunk::new("Hello");
        assert_eq!(chunk.content, "Hello");
        assert!(!chunk.is_final);
    }

    #[test]
    fn test_final_chunk() {
        let chunk = StreamChunk::final_chunk("Goodbye", "stop");
        assert_eq!(chunk.content, "Goodbye");
        assert!(chunk.is_final);
        assert_eq!(chunk.finish_reason, Some("stop".to_string()));
    }

    #[test]
    fn test_stream_handler() {
        let mut handler = StreamHandler::new();

        handler.handle_chunk(&StreamChunk::new("Hello ").with_tokens(2));
        handler.handle_chunk(&StreamChunk::new("World").with_tokens(1));

        assert_eq!(handler.content(), "Hello World");
        assert_eq!(handler.total_tokens(), 3);
    }

    #[test]
    fn test_handler_reset() {
        let mut handler = StreamHandler::new();
        handler.handle_chunk(&StreamChunk::new("Test").with_tokens(1));

        handler.reset();

        assert_eq!(handler.content(), "");
        assert_eq!(handler.total_tokens(), 0);
    }
}
