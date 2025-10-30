//! ggen - Language-agnostic, deterministic code projection CLI
//!
//! This library provides the core functionality for ggen, including:
//! - Knowledge graph management and RDF processing
//! - Template generation and projection
//! - Marketplace operations and package management
//! - Ultra-advanced agent system for intelligent automation
//!
//! # Architecture Overview
//!
//! ggen follows a modular architecture with clear separation of concerns:
//! - `core/` - Domain logic and knowledge processing
//! - `utils/` - Shared utilities and configuration
//! - `cli/` - Command-line interface and subcommands
//! - `agents/` - Ultra-advanced agent orchestration system
//! - `mcp/` - Model Context Protocol server integration

pub mod agents {
    pub mod simple;
}
pub mod core;
// P2P module temporarily disabled due to compilation errors (will be fixed in v1.3.0)
// pub mod p2p;
pub mod utils;

// Re-export commonly used types
pub use crate::core::*;
pub use crate::utils::*;
