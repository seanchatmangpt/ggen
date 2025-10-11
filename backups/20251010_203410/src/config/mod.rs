//! Configuration management for ggen-ai

pub mod ollama;
pub mod openai;
pub mod anthropic;
pub mod ai;
pub mod global;

pub use ollama::OllamaConfig;
pub use openai::OpenAIConfig;
pub use anthropic::AnthropicConfig;
pub use ai::{AiConfig, GenerationConfig};
pub use global::{GlobalLlmConfig, LlmProvider, get_global_config, init_global_config};

// Re-export types for convenience
pub use crate::client::LlmConfig;
