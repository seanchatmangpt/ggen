//! Configuration management for ggen-ai

pub mod ai;
pub mod anthropic;
pub mod global;
pub mod ollama;
pub mod openai;

pub use ai::{AiConfig, GenerationConfig};
pub use anthropic::AnthropicConfig;
pub use global::{get_global_config, init_global_config, GlobalLlmConfig, LlmProvider};
pub use ollama::OllamaConfig;
pub use openai::OpenAIConfig;

// Re-export types for convenience
pub use crate::client::LlmConfig;
