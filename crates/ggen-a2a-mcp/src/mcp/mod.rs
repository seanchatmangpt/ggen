//! MCP (Model Context Protocol) module for ggen-a2a-mcp
//!
//! This module provides the MCP protocol implementation for A2A skill integration,
//! including tool registry, server components, and protocol handlers.

pub mod registry;
pub mod protocol;
pub mod transport;
pub mod server;
pub mod handler;
pub mod response;

// Re-export key types for convenience
pub use registry::{
    McpToolDefinition,
    McpToolRegistry as McpToolRegistryTrait,
    InMemoryToolRegistry,
    A2aSkillRegistry,
    A2aSkill,
    A2aSkillMetadata,
    SkillParameter,
    SkillParamType,
    ExecutionMode,
};

pub use server::{McpServer, McpServerConfig};
pub use transport::StdioTransport;
pub use protocol::{McpRequest, McpResponse};
