//! # Rig + MCP Integration
//!
//! Exact code from official MCP Rust SDK example.
//! See: https://github.com/modelcontextprotocol/rust-sdk/tree/main/examples/rig-integration

pub mod chat;
pub mod config;
pub mod mcp_adaptor;

// Re-export everything
pub use chat::*;
pub use config::*;
pub use mcp_adaptor::*;
