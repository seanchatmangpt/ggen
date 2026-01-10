//! MCP Server Interface - Model Context Protocol Integration
//!
//! This module provides the interface for integrating with the Model Context Protocol (MCP),
//! enabling LLM-driven agents to discover and invoke tools through a standardized JSON-RPC interface.
//!
//! **Phase 4 Implementation**: The MCP server implementation is deferred to Phase 4.
//! This module provides the trait definitions and type stubs for Phase 4 completion.
//!
//! # Architecture
//!
//! ```
//! LLM (Claude, GPT-4, etc)
//!   │
//!   ├─ JSON-RPC 2.0 (HTTP/WebSocket)
//!   │
//!   ▼
//! MCPToolServer (this trait)
//!   │
//!   ├─ tools/list       → Discover all tools
//!   ├─ tools/call       → Invoke tool with params
//!   └─ tools/validate   → Validate input schema
//!   │
//!   ▼
//! ToolRegistry
//!   │
//!   ├─ Tool lookup and invocation
//!   ├─ Signature validation
//!   └─ Error handling
//! ```
//!
//! # Phase 3 vs Phase 4
//!
//! **Phase 3 (Current)**:
//! - Tool Registry works end-to-end with local agents
//! - Signatures enable validation
//! - Agents invoke tools directly through registry
//!
//! **Phase 4 (Deferred)**:
//! - HTTP/WebSocket transport
//! - Full JSON-RPC 2.0 protocol
//! - MCP server implementation
//! - Authentication and rate limiting
//! - Resource management

pub mod traits;
pub mod types;

pub use traits::MCPToolServer;
pub use types::*;

/// MCP Server Configuration
///
/// Placeholder for Phase 4: Full configuration will include
/// - HTTP server settings (port, TLS, etc)
/// - Timeout configuration
/// - Rate limiting
/// - Authentication settings
#[derive(Debug, Clone)]
pub struct MCPServerConfig {
    /// Server name
    pub name: String,

    /// Server version
    pub version: String,

    /// Maximum time to wait for tool execution (Phase 4)
    pub tool_timeout_ms: u64,

    /// Maximum concurrent requests (Phase 4)
    pub max_concurrent_requests: usize,
}

impl Default for MCPServerConfig {
    fn default() -> Self {
        Self {
            name: "ggen-mcp-server".to_string(),
            version: "0.1.0".to_string(),
            tool_timeout_ms: 30000,
            max_concurrent_requests: 100,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_default_config() {
        let config = MCPServerConfig::default();
        assert_eq!(config.name, "ggen-mcp-server");
        assert_eq!(config.version, "0.1.0");
        assert_eq!(config.tool_timeout_ms, 30000);
    }
}
