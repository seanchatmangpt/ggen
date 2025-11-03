//! Configuration management for ggen-ai

pub mod ai;
pub mod global;

pub use ai::{AiConfig, GenerationConfig};
pub use global::{get_global_config, init_global_config, GlobalLlmConfig, LlmProvider};

// Re-export types for convenience
pub use crate::client::LlmConfig;
