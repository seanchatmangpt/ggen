//! A2A predictor module for DSPy integration
//!
//! This module provides predictors that bridge A2A (Agent-to-Agent) communication
//! with DSPy-style LLM interactions, enabling agents to use ggen-ai's multi-provider
//! LLM support through a unified DSPy interface.
//!
//! ## Key Components
//!
//! - [`A2aPredictor`] - Main predictor wrapping A2A agents in DSPy interface
//! - [`A2aPredictorConfig`] - Configuration for A2A prediction
//! - [`StreamingA2aPredictor`] - Streaming variant for real-time responses
//!
//! ## Example
//!
//! ```rust,no_run
//! use ggen_dspy::predictors::{A2aPredictor, A2aPredictorConfig};
//! use a2a_generated::converged::message::ConvergedMessage;
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let config = A2aPredictorConfig::default();
//! let predictor = A2aPredictor::new(config)?;
//!
//! let message = ConvergedMessage::text("test-id", "user", "Hello, agent!");
//! let response = predictor.process_message(message).await?;
//!
//! println!("Response: {:?}", response);
//! # Ok(())
//! # }
//! ```

pub mod a2a_predictor;

pub use a2a_predictor::{
    A2aModule, A2aModuleTrait, A2aPredictor, A2aPredictorConfig,
    StreamingA2aPredictor, StreamingChunk, TokenUsage, CacheStats, ModuleOutput,
};
