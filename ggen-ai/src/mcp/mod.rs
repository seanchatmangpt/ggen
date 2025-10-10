//! MCP server integration for ggen-ai

pub mod server;
pub mod tools;

// Re-export MCP types
pub use server::GgenAiMcpServer;
pub use tools::*;

