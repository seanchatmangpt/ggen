//! LLM integration module for multi-provider support
//!
//! This module provides a unified interface for interacting with multiple LLM providers
//! (OpenAI, Anthropic, etc.) with support for streaming responses, configuration management,
//! and robust error handling.
//!
//! # Architecture
//!
//! The module follows the adapter pattern inspired by rust-genai:
//! - `LlmProvider` trait: Unified interface for all providers
//! - Provider-specific implementations in submodules
//! - Streaming support via async/await and tokio streams
//! - Type-safe configuration with builder pattern
//!
//! # Example
//!
//! ```no_run
//! use ggen_core::llm::{LlmProvider, OpenAiProvider, ChatRequest};
//!
//! #[tokio::main]
//! async fn main() -> Result<(), Box<dyn std::error::Error>> {
//!     let provider = OpenAiProvider::new("your-api-key");
//!     let request = ChatRequest::builder()
//!         .model("gpt-4")
//!         .message("user", "Hello!")
//!         .build()?;
//!
//!     let response = provider.chat(request).await?;
//!     println!("{}", response.content);
//!     Ok(())
//! }
//! ```

pub mod config;
pub mod error;
pub mod provider;
pub mod streaming;
pub mod types;

// Provider implementations
pub mod anthropic;
pub mod openai;

// Re-exports for convenience
pub use config::{LlmConfig, ProviderConfig};
pub use error::{LlmError, LlmResult};
pub use provider::LlmProvider;
pub use streaming::{StreamChunk, StreamHandler};
pub use types::{ChatRequest, ChatResponse, Message, Role};

// Provider re-exports
pub use anthropic::AnthropicProvider;
pub use openai::OpenAiProvider;
