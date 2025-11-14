//! LLM provider implementations
//!
//! This module provides adapter implementations for various LLM providers,
//! allowing ggen-ai to work with multiple LLM backends through a unified interface.
//!
//! ## Supported Providers
//!
//! The adapter module supports multiple LLM providers including:
//! - OpenAI (GPT models)
//! - Anthropic (Claude models)
//! - Ollama (local models)
//! - Google (Gemini models)
//! - DeepSeek
//! - xAI/Grok
//! - Groq
//! - Cohere
//!
//! All providers are accessed through the unified `LlmClient` trait interface,
//! making it easy to switch between providers or use multiple providers simultaneously.

pub mod adapter;

// Re-export provider types
pub use adapter::*;
