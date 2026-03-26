//! MCP server implementation

use super::registry::InMemoryToolRegistry;
use super::transport::StdioTransport;
use crate::error::A2aMcpResult;

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
    #[allow(dead_code)]
    config: McpServerConfig,
    #[allow(dead_code)]
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
