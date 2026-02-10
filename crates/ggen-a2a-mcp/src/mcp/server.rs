//! MCP server implementation

use crate::error::A2aMcpResult;
use super::transport::StdioTransport;
use super::registry::InMemoryToolRegistry;

/// MCP server configuration
#[derive(Debug, Clone)]
pub struct McpServerConfig {
    pub server_name: String,
    pub server_version: String,
    pub execution_timeout_ms: u64,
}

impl Default for McpServerConfig {
    fn default() -> Self {
        Self {
            server_name: "ggen-mcp-server".to_string(),
            server_version: env!("CARGO_PKG_VERSION").to_string(),
            execution_timeout_ms: 30_000,
        }
    }
}

/// MCP STDIO server
pub struct McpServer {
    config: McpServerConfig,
    registry: InMemoryToolRegistry,
}

impl McpServer {
    pub fn new(config: McpServerConfig) -> Self {
        Self {
            config,
            registry: InMemoryToolRegistry::new(),
        }
    }

    pub fn run(&mut self) -> A2aMcpResult<()> {
        let mut transport = StdioTransport::new();
        transport.run()
    }
}
