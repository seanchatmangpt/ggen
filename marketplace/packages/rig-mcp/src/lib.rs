//! Rig MCP Integration Library
//!
//! Production-ready MCP protocol integration for ggen.
//!
//! This package provides:
//! - Multi-transport MCP support (stdio, SSE, HTTP, A2A)
//! - Production-ready patterns from official MCP Rust SDK
//! - MCP Tool Discovery Engine with caching and concurrent discovery
//! - Tool Registration with ggen-ai core registry
//! - A2A protocol integration

use serde::{Deserialize, Serialize};

// MCP Tool Discovery Engine
pub mod discovery;

// MCP Transport Layer
pub mod transport;

// Tool Registration with ggen Core
pub mod registration;

// A2A Bridge Layer (MCP to A2A protocol translation)
pub mod a2a;

// Re-export commonly used transport types
pub use transport::{
    A2AConfig, A2ATransport, A2ATransportBuilder, Content, HttpConfig, HttpSseTransport,
    HttpTransport, HttpTransportBuilder, JsonRpcError, JsonRpcRequest, JsonRpcResponse, McpMethod,
    McpTransport, McpTransportExt, Resource, ResourceContents, StdioConfig, StdioTransport,
    StdioTransportBuilder, Tool, ToolResult, TransportConfig, TransportError, TransportFactory,
    TransportResult, TransportState, TransportType,
};

// Re-export the transport builder from factory
pub use transport::factory::TransportBuilder;

// Re-export discovery types
pub use discovery::{
    extract_schemas, validate_schema, DiscoveryConfig, DiscoveryEngine, DiscoveryError,
    ParameterSchema, SchemaExtractor, ToolSchema, ToolSchemaValidationError,
};

// Re-export registration types
pub use registration::{
    ConversionConfig, ConversionError, RegistrationConfig, RegistrationError, RegistrationStats,
    SchemaConverter, SourceInfo, SourceStatistics, ToolRegistrationManager,
};

/// Configuration for MCP integration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Config {
    /// MCP servers to connect to
    pub mcp_servers: Vec<ServerConfig>,
    /// Transport configuration
    pub transport: Option<TransportConfig>,
    /// Discovery configuration
    pub discovery: Option<DiscoveryConfigOptions>,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ServerConfig {
    pub url: String,
    pub name: String,
    #[serde(default)]
    pub transport_type: String,
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DiscoveryConfigOptions {
    pub timeout_secs: u64,
    pub cache_ttl_secs: u64,
    pub max_concurrent: usize,
}

impl Default for DiscoveryConfigOptions {
    fn default() -> Self {
        Self {
            timeout_secs: 30,
            cache_ttl_secs: 300,
            max_concurrent: 10,
        }
    }
}

impl Default for Config {
    fn default() -> Self {
        Self {
            mcp_servers: Vec::new(),
            transport: None,
            discovery: None,
        }
    }
}

/// Main MCP client (transport-only, without LLM integration)
pub struct McpClient {
    config: Config,
}

impl McpClient {
    /// Create a new MCP client from configuration
    pub fn new(config: Config) -> Self {
        Self { config }
    }

    /// Get server configurations
    pub fn servers(&self) -> &[ServerConfig] {
        &self.config.mcp_servers
    }

    /// Add a server configuration
    pub fn add_server(&mut self, server: ServerConfig) {
        self.config.mcp_servers.push(server);
    }

    /// Create a transport for a specific server
    pub fn create_transport(&self, server: &ServerConfig) -> Result<TransportType, TransportError> {
        match server.transport_type.as_str() {
            "stdio" => Ok(TransportType::Stdio),
            "http" => Ok(TransportType::Http),
            "sse" => Ok(TransportType::HttpSse),
            "a2a" => Ok(TransportType::A2A),
            _ => Err(TransportError::Internal(format!(
                "Unknown transport type: {}",
                server.transport_type
            ))),
        }
    }
}

/// Example usage and utilities
pub mod prelude {
    pub use crate::transport::*;
    pub use crate::{
        Config, DiscoveryConfigOptions, McpClient, ParameterSchema, RegistrationConfig,
        RegistrationError, SchemaExtractor, ServerConfig, ToolRegistrationManager, ToolSchema,
    };
    // A2A Bridge types
    pub use crate::a2a::{
        A2aClientWrapper, A2aClientWrapperConfig, A2aMessageConverter, Bridge, BridgeConfig,
        BridgeState, ConversionContext, ConversionError, ConversionMetrics, McpToA2aBridge,
        McpToA2aConversionMode, ProtocolTranslator, ToolSchemaGenerator, TranslationError,
        TranslationResult, TranslatorConfig,
    };
    // A2A transport types from the transport submodule
    pub use crate::a2a::transport::{
        A2aTransport as A2aBridgeTransport, A2aTransportBuilder, A2aTransportConfig,
        HttpA2aTransport,
    };
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transport_type_parsing() {
        assert_eq!(
            TransportType::from_str("stdio").unwrap(),
            TransportType::Stdio
        );
        assert_eq!(
            TransportType::from_str("http").unwrap(),
            TransportType::Http
        );
        assert_eq!(
            TransportType::from_str("sse").unwrap(),
            TransportType::HttpSse
        );
        assert_eq!(TransportType::from_str("a2a").unwrap(), TransportType::A2A);
    }

    #[test]
    fn test_config_default() {
        let config = Config::default();
        assert!(config.mcp_servers.is_empty());
    }

    #[test]
    fn test_mcp_client() {
        let config = Config::default();
        let client = McpClient::new(config);
        assert!(client.servers().is_empty());
    }
}
