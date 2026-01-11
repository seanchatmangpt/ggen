//! # ggen-dspy - DSPy-inspired programming framework for LLM agents in Rust
//!
//! This crate provides a Rust implementation of DSPy concepts for building
//! self-optimizing LLM agent pipelines. It integrates with ggen-ai for LLM
//! interactions and provides a type-safe, declarative API for agent composition.
//!
//! ## Core Concepts
//!
//! - **Modules**: Composable building blocks (Predictor, ChainOfThought, ReAct, etc.)
//! - **Optimizers**: Self-improvement algorithms (BootstrapFewShot, MIPRO, etc.)
//! - **Evaluation**: Metrics and scoring for optimization loops
//! - **Assertions**: Runtime constraints and validation for LLM outputs
//! - **Adapters**: Integration with ggen-ai LLM clients
//!
//! ## Design Principles
//!
//! Following CLAUDE.md conventions:
//! - **Type-First**: All constraints expressed in types, verified by compiler
//! - **Result<T, E>**: All fallible operations return Result, no unwrap/expect in production
//! - **Zero-Cost Abstractions**: Generics over trait objects for performance
//! - **Deterministic**: Same inputs produce same outputs (with seed control)
//! - **Testable**: Clear separation of pure logic and LLM interactions
//!
//! ## Quick Start
//!
//! ```rust,no_run
//! use ggen_dspy::{Module, Predictor, Signature, InputField, OutputField};
//! use ggen_ai::{GenAiClient, LlmConfig};
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! // Define a signature (input/output schema)
//! let signature = Signature::builder()
//!     .input(InputField::new("question", "A question to answer"))
//!     .output(OutputField::new("answer", "The answer"))
//!     .build()?;
//!
//! // Create a predictor module
//! let llm_config = LlmConfig::default();
//! let client = GenAiClient::new(llm_config)?;
//! let predictor = Predictor::new(signature, client);
//!
//! // Execute the module
//! let result = predictor.forward(&[("question", "What is Rust?")]).await?;
//! println!("Answer: {}", result.get("answer")?);
//! # Ok(())
//! # }
//! ```
//!
//! ## Module Organization
//!
//! - `modules/` - Core module types (Predictor, ChainOfThought, ReAct, etc.)
//! - `optimizers/` - Optimization algorithms (BootstrapFewShot, MIPRO, etc.)
//! - `evaluation/` - Metrics and scoring for optimization
//! - `assertions/` - Runtime constraints and validation
//! - `config/` - Configuration types for modules and optimizers
//! - `adapters/` - Integration with ggen-ai and other LLM clients
//! - `patterns/` - Reusable agent patterns and templates
//! - `testing/` - Test utilities and fixtures
//!
//! ## Performance
//!
//! - **Memory Budget**: Target ≤ 100MB for typical agent pipelines
//! - **Caching**: Automatic LLM response caching with TTL and size limits
//! - **Concurrency**: Async/await with tokio for parallel agent execution
//! - **Zero-Copy**: Reference-based APIs where possible to minimize allocations

#![deny(warnings)] // Poka-Yoke: Prevent warnings at compile time
#![deny(unsafe_code)] // No unsafe code allowed
#![warn(missing_docs)] // Encourage documentation

// Core error types
pub mod error;

// Module types
pub mod modules;

// Optimization algorithms
pub mod optimizers;

// Evaluation metrics
pub mod evaluation;

// Runtime assertions
pub mod assertions;

// Configuration types
pub mod config;

// LLM client adapters
pub mod adapters;

// Reusable patterns
pub mod patterns;

// Test utilities
#[cfg(any(test, feature = "live-llm-tests"))]
pub mod testing;

// Re-export core types for convenience
pub use error::{DspyError, Result};

// Config exports (most commonly used)
pub use config::{
    get_dspy_config, init_dspy_config, with_context, CacheConfig, CacheManager, CacheStats,
    ContextBuilder, DspySettings, UsageStats, UsageTracker,
};

// Module exports
pub use modules::{
    ChainOfThought, Module, ModuleContext, ModuleOutput, Predictor, ReAct, ReactAgent,
};

// Optimizer exports
pub use optimizers::{BootstrapFewShot, MiproOptimizer, Optimizer, OptimizerConfig};

// Evaluation exports
pub use evaluation::{EvaluationMetrics, EvaluationResult, Evaluator, MetricValue};

// Assertion exports
pub use assertions::{Assert, AssertionError, Suggest};

// Adapter exports
pub use adapters::{GgenAiAdapter, LlmAdapter};

// Pattern exports
pub use patterns::{AgentPattern, PatternBuilder, PatternLibrary};

/// Version information
pub const VERSION: &str = env!("CARGO_PKG_VERSION");

/// Initialize logging for the ggen-dspy crate
pub fn init_logging() {
    use tracing_subscriber::{fmt, EnvFilter};

    if std::env::var("RUST_LOG").is_err() {
        std::env::set_var("RUST_LOG", "ggen_dspy=info");
    }

    let _ = fmt()
        .with_env_filter(EnvFilter::from_default_env())
        .with_target(false)
        .try_init();
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_version() {
        assert!(!VERSION.is_empty());
    }
}
