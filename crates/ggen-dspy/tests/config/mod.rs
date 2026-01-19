//! Comprehensive tests for DSPy configuration system
//!
//! Tests cover:
//! - Global configuration initialization
//! - Thread-local context overrides
//! - Context propagation to child tasks
//! - Cache operations (memory and disk)
//! - Usage tracking and cost estimation
//! - RAII cleanup and error handling
//! - Thread isolation
//! - Integration with ggen-ai GlobalLlmConfig

use ggen_dspy::{
    get_dspy_config, init_dspy_config, with_context, CacheConfig, CacheManager, DspySettings,
    UsageTracker,
};
use tempfile::TempDir;

mod cache_tests;
mod context_tests;
mod integration_tests;
mod settings_tests;
mod usage_tests;
