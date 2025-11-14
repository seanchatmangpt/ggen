//! Configuration management for ggen-ai
//!
//! This module provides configuration management for AI operations in ggen,
//! including LLM provider settings, generation parameters, and global configuration.
//!
//! ## Features
//!
//! - **AI Configuration**: Per-operation AI settings and parameters
//! - **Global Configuration**: System-wide LLM configuration with environment support
//! - **Provider Management**: LLM provider selection and configuration
//! - **Generation Settings**: Fine-tune generation behavior and parameters
//!
//! ## Module Organization
//!
//! - `ai` - AI-specific configuration structures
//! - `global` - Global LLM configuration and initialization

pub mod ai;
pub mod global;

pub use ai::{AiConfig, GenerationConfig};
pub use global::{get_global_config, init_global_config, GlobalLlmConfig, LlmProvider};

// Re-export types for convenience
pub use crate::client::LlmConfig;
