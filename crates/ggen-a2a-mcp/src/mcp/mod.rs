//! MCP (Model Context Protocol) module for ggen-a2a-mcp
//!
//! This module provides the MCP protocol implementation for A2A skill integration,
//! including tool registry, server components, and protocol handlers.

pub mod handler;
pub mod protocol;
pub mod registry;
pub mod response;
pub mod server;
pub mod transport;

// Re-export key types for convenience
pub use registry::{
    A2aSkill, A2aSkillMetadata, A2aSkillRegistry, ExecutionMode, InMemoryToolRegistry,
    McpToolDefinition, McpToolRegistry as McpToolRegistryTrait, SkillParamType, SkillParameter,
};

pub use protocol::{McpRequest, McpResponse};
pub use server::{McpServer, McpServerConfig};
pub use transport::StdioTransport;
