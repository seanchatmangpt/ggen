//! AI domain layer - Pure business logic for AI operations
//!
//! This module provides domain logic for AI-powered code generation and analysis,
//! completely separated from CLI concerns. It uses the ggen-ai crate for LLM
//! integration and provides high-level business logic functions.
//!
//! ## Architecture
//!
//! - **No CLI dependencies**: Pure domain logic with no clap or clap-noun-verb dependencies
//! - **Async by default**: All operations are async for non-blocking execution
//! - **Error handling**: Uses `ggen_utils::error::Result` for consistent error handling
//!
//! ## Module Organization
//!
//! - `analyze` - Code and project analysis operations
//! - `generate` - AI-powered code generation operations
//! - `execute` - Layer 2 integration (async coordination, resource management)

pub mod analyze;
pub mod execute;
pub mod generate;

pub use analyze::*;
pub use generate::*;
