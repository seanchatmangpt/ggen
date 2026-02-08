//! Transport Factory
//!
//! Factory for creating MCP transport instances from configuration.

use crate::transport::{
    a2a::{A2AConfig, A2ATransport},
    error::{TransportError, TransportResult},
    http::{HttpConfig, HttpSseTransport, HttpTransport},
    stdio::{StdioConfig, StdioTransport},
    McpTransport,
};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::Arc;

/// Transport type enumeration
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum TransportType {
    /// Stdio transport (local MCP servers)
    Stdio,
    /// HTTP transport (JSON-RPC over HTTP)
    Http,
    /// SSE transport (Server-Sent Events)
    HttpSse,
    /// A2A transport (Agent-to-Agent protocol)
    #[serde(alias = "a2a")]
    A2A,
}

impl TransportType {
    /// Parse from string
    pub fn from_str(s: impl AsRef<str>) -> TransportResult<Self> {
        match s.as_ref().to_lowercase().as_str() {
            "stdio" | "stdin" | "local" => Ok(TransportType::Stdio),
            "http" | "https" | "rest" => Ok(TransportType::Http),
            "sse" | "events" => Ok(TransportType::HttpSse),
            "a2a" | "agent-to-agent" => Ok(TransportType::A2A),
            _ => Err(TransportError::InvalidEndpoint(format!(
                "Unknown transport type: {}",
                s.as_ref()
            ))),
        }
    }

    /// Convert to string
    pub fn as_str(&self) -> &str {
        match self {
            TransportType::Stdio => "stdio",
            TransportType::Http => "http",
            TransportType::HttpSse => "sse",
            TransportType::A2A => "a2a",
        }
    }
}

impl From<&str> for TransportType {
    fn from(s: &str) -> Self {
        Self::from_str(s).unwrap_or(TransportType::Stdio)
    }
}

impl From<String> for TransportType {
    fn from(s: String) -> Self {
        Self::from_str(s).unwrap_or(TransportType::Stdio)
    }
}

/// Transport configuration
///
/// Unified configuration for all transport types.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TransportConfig {
    /// Transport type
    pub transport_type: TransportType,
    /// Endpoint URL or command
    pub endpoint: String,
    /// Arguments for stdio transports
    pub args: Vec<String>,
    /// Environment variables for stdio transports
    pub env: HashMap<String, String>,
    /// Timeout in seconds
    pub timeout_secs: u64,
    /// Authentication token
    pub auth_token: Option<String>,
    /// Custom headers for HTTP transports
    pub headers: HashMap<String, String>,
    /// Max concurrent requests (HTTP only)
    pub max_concurrent: usize,
}

impl TransportConfig {
    /// Create a new transport config
    pub fn new(transport_type: TransportType, endpoint: impl Into<String>) -> Self {
        Self {
            transport_type,
            endpoint: endpoint.into(),
            args: Vec::new(),
            env: HashMap::new(),
            timeout_secs: 30,
            auth_token: None,
            headers: HashMap::new(),
            max_concurrent: 100,
        }
    }

    /// Create a stdio config from a command
    pub fn stdio(command: impl Into<String>) -> Self {
        Self::new(TransportType::Stdio, command)
    }

    /// Create an HTTP config from a URL
    pub fn http(url: impl Into<String>) -> Self {
        Self::new(TransportType::Http, url)
    }

    /// Create an SSE config from a URL
    pub fn sse(url: impl Into<String>) -> Self {
        Self::new(TransportType::HttpSse, url)
    }

    /// Create an A2A config from a URL
    pub fn a2a(url: impl Into<String>) -> Self {
        Self::new(TransportType::A2A, url)
    }

    /// Add an argument (stdio)
    pub fn arg(mut self, arg: impl Into<String>) -> Self {
        self.args.push(arg.into());
        self
    }

    /// Add arguments (stdio)
    pub fn args(mut self, args: impl IntoIterator<Item = impl Into<String>>) -> Self {
        for arg in args {
            self.args.push(arg.into());
        }
        self
    }

    /// Add an environment variable (stdio)
    pub fn env(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.env.insert(key.into(), value.into());
        self
    }

    /// Set timeout
    pub fn timeout(mut self, secs: u64) -> Self {
        self.timeout_secs = secs;
        self
    }

    /// Set auth token
    pub fn auth_token(mut self, token: impl Into<String>) -> Self {
        self.auth_token = Some(token.into());
        self
    }

    /// Add a header (HTTP)
    pub fn header(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.headers.insert(key.into(), value.into());
        self
    }

    /// Set max concurrent requests (HTTP)
    pub fn max_concurrent(mut self, max: usize) -> Self {
        self.max_concurrent = max;
        self
    }

    /// Auto-detect transport type from endpoint
    pub fn auto_detect(endpoint: impl Into<String>) -> Self {
        let endpoint = endpoint.into();
        let transport_type = if endpoint.starts_with("http://") || endpoint.starts_with("https://")
        {
            TransportType::Http
        } else {
            TransportType::Stdio
        };
        Self::new(transport_type, endpoint)
    }
}

/// Transport factory for creating MCP transport instances
pub struct TransportFactory;

impl TransportFactory {
    /// Create a transport from configuration
    pub async fn create(config: TransportConfig) -> TransportResult<Arc<dyn McpTransport>> {
        match config.transport_type {
            TransportType::Stdio => {
                let stdio_config = Self::build_stdio_config(config)?;
                let transport = StdioTransport::new(stdio_config);
                transport.connect().await?;
                Ok(Arc::new(transport))
            }
            TransportType::Http => {
                let http_config = Self::build_http_config(config)?;
                let transport = HttpTransport::new(http_config)?;
                Ok(Arc::new(transport))
            }
            TransportType::HttpSse => {
                let transport = HttpSseTransport::new(config.endpoint)?;
                Ok(Arc::new(transport))
            }
            TransportType::A2A => {
                let a2a_config = Self::build_a2a_config(config)?;
                let transport = A2ATransport::new(a2a_config);
                transport.connect().await?;
                Ok(Arc::new(transport))
            }
        }
    }

    /// Create without auto-connecting
    pub fn create_raw(config: TransportConfig) -> TransportResult<Arc<dyn McpTransport>> {
        match config.transport_type {
            TransportType::Stdio => {
                let stdio_config = Self::build_stdio_config(config)?;
                let transport = StdioTransport::new(stdio_config);
                Ok(Arc::new(transport))
            }
            TransportType::Http => {
                let http_config = Self::build_http_config(config)?;
                let transport = HttpTransport::new(http_config)?;
                Ok(Arc::new(transport))
            }
            TransportType::HttpSse => {
                let transport = HttpSseTransport::new(config.endpoint)?;
                Ok(Arc::new(transport))
            }
            TransportType::A2A => {
                let a2a_config = Self::build_a2a_config(config)?;
                let transport = A2ATransport::new(a2a_config);
                Ok(Arc::new(transport))
            }
        }
    }

    /// Create from a URL string (auto-detects transport)
    pub async fn from_url(url: impl Into<String>) -> TransportResult<Arc<dyn McpTransport>> {
        let config = TransportConfig::auto_detect(url);
        Self::create(config).await
    }

    /// Create from a command string (stdio)
    pub async fn from_command(
        command: impl Into<String>,
    ) -> TransportResult<Arc<dyn McpTransport>> {
        let command_str = command.into();
        let transport = StdioTransport::from_command(&command_str)?;
        transport.connect().await?;
        Ok(Arc::new(transport))
    }

    /// Build stdio config from transport config
    fn build_stdio_config(config: TransportConfig) -> TransportResult<StdioConfig> {
        let mut stdio_config = StdioConfig::new(config.endpoint)
            .timeout(config.timeout_secs)
            .args(config.args);

        for (key, value) in config.env {
            stdio_config = stdio_config.env(key, value);
        }

        Ok(stdio_config)
    }

    /// Build HTTP config from transport config
    fn build_http_config(config: TransportConfig) -> TransportResult<HttpConfig> {
        let mut http_config = HttpConfig::new(config.endpoint)
            .timeout(config.timeout_secs)
            .max_concurrent(config.max_concurrent);

        if let Some(token) = config.auth_token {
            http_config = http_config.auth_token(token);
        }

        for (key, value) in config.headers {
            http_config = http_config.header(key, value);
        }

        Ok(http_config)
    }

    /// Build A2A config from transport config
    fn build_a2a_config(config: TransportConfig) -> TransportResult<A2AConfig> {
        let mut a2a_config = A2AConfig::new(config.endpoint).with_timeout(config.timeout_secs);

        if let Some(token) = config.auth_token {
            a2a_config = a2a_config.with_auth(token);
        }

        Ok(a2a_config)
    }

    /// Parse a connection string into a transport config
    ///
    /// Supported formats:
    /// - `"stdio:command arg1 arg2"` - Stdio transport
    /// - `"http://localhost:8080"` - HTTP transport (auto-detected)
    /// - `"https://api.example.com/mcp"` - HTTP transport (auto-detected)
    /// - `"a2a://localhost:8080"` - A2A transport
    /// - `"sse://localhost:8080"` - SSE transport
    /// - `"command arg1 arg2"` - Stdio transport (default)
    pub fn parse_connection_string(conn: impl AsRef<str>) -> TransportResult<TransportConfig> {
        let conn = conn.as_ref();

        // Check for explicit transport prefix
        if let Some((prefix, rest)) = conn.split_once(':') {
            match prefix {
                "stdio" | "local" => {
                    return Ok(TransportConfig::stdio(rest));
                }
                "http" | "https" => {
                    // Already a full URL
                    return Ok(TransportConfig::auto_detect(conn));
                }
                "sse" | "events" => {
                    return Ok(TransportConfig::sse(rest));
                }
                "a2a" | "agent-to-agent" => {
                    return Ok(TransportConfig::a2a(rest));
                }
                _ => {
                    // Not a recognized prefix, treat as entire string
                }
            }
        }

        // Auto-detect from the connection string
        Ok(TransportConfig::auto_detect(conn))
    }
}

/// Builder for creating transports with a fluent API
pub struct TransportBuilder {
    config: TransportConfig,
}

impl TransportBuilder {
    /// Create a new builder with a transport type
    pub fn new(transport_type: TransportType, endpoint: impl Into<String>) -> Self {
        Self {
            config: TransportConfig::new(transport_type, endpoint),
        }
    }

    /// Create from URL (auto-detect)
    pub fn from_url(url: impl Into<String>) -> Self {
        Self {
            config: TransportConfig::auto_detect(url),
        }
    }

    /// Create for stdio transport
    pub fn stdio(command: impl Into<String>) -> Self {
        Self {
            config: TransportConfig::stdio(command),
        }
    }

    /// Create for HTTP transport
    pub fn http(url: impl Into<String>) -> Self {
        Self {
            config: TransportConfig::http(url),
        }
    }

    /// Create for SSE transport
    pub fn sse(url: impl Into<String>) -> Self {
        Self {
            config: TransportConfig::sse(url),
        }
    }

    /// Create for A2A transport
    pub fn a2a(url: impl Into<String>) -> Self {
        Self {
            config: TransportConfig::a2a(url),
        }
    }

    /// Add an argument (stdio)
    pub fn arg(mut self, arg: impl Into<String>) -> Self {
        self.config = self.config.arg(arg);
        self
    }

    /// Add arguments (stdio)
    pub fn args(mut self, args: impl IntoIterator<Item = impl Into<String>>) -> Self {
        self.config = self.config.args(args);
        self
    }

    /// Add an environment variable (stdio)
    pub fn env(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.config = self.config.env(key, value);
        self
    }

    /// Set timeout
    pub fn timeout(mut self, secs: u64) -> Self {
        self.config = self.config.timeout(secs);
        self
    }

    /// Set auth token
    pub fn auth_token(mut self, token: impl Into<String>) -> Self {
        self.config = self.config.auth_token(token);
        self
    }

    /// Add a header (HTTP)
    pub fn header(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.config = self.config.header(key, value);
        self
    }

    /// Set max concurrent requests (HTTP)
    pub fn max_concurrent(mut self, max: usize) -> Self {
        self.config = self.config.max_concurrent(max);
        self
    }

    /// Build the transport (without connecting)
    pub fn build(self) -> TransportResult<Arc<dyn McpTransport>> {
        TransportFactory::create_raw(self.config)
    }

    /// Build and connect the transport
    pub async fn connect(self) -> TransportResult<Arc<dyn McpTransport>> {
        TransportFactory::create(self.config).await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_transport_type_from_str() {
        assert_eq!(
            TransportType::from_str("stdio").unwrap(),
            TransportType::Stdio
        );
        assert_eq!(
            TransportType::from_str("HTTP").unwrap(),
            TransportType::Http
        );
        assert_eq!(
            TransportType::from_str("Sse").unwrap(),
            TransportType::HttpSse
        );
        assert_eq!(TransportType::from_str("a2a").unwrap(), TransportType::A2A);
        assert_eq!(
            TransportType::from_str("AGENT-TO-AGENT").unwrap(),
            TransportType::A2A
        );
        assert!(TransportType::from_str("unknown").is_err());
    }

    #[test]
    fn test_transport_config_builder() {
        let config = TransportConfig::stdio("test")
            .arg("--verbose")
            .args(["--config", "test.json"])
            .env("TEST", "value")
            .timeout(60);

        assert_eq!(config.transport_type, TransportType::Stdio);
        assert_eq!(config.endpoint, "test");
        assert_eq!(config.args, vec!["--verbose", "--config", "test.json"]);
        assert_eq!(config.env.get("TEST"), Some(&"value".to_string()));
        assert_eq!(config.timeout_secs, 60);
    }

    #[test]
    fn test_http_config_builder() {
        let config = TransportConfig::http("http://localhost:8080")
            .auth_token("secret")
            .header("X-API-Key", "test")
            .max_concurrent(50);

        assert_eq!(config.transport_type, TransportType::Http);
        assert_eq!(config.endpoint, "http://localhost:8080");
        assert_eq!(config.auth_token, Some("secret".to_string()));
        assert_eq!(config.headers.get("X-API-Key"), Some(&"test".to_string()));
        assert_eq!(config.max_concurrent, 50);
    }

    #[test]
    fn test_auto_detect() {
        let http_config = TransportConfig::auto_detect("http://localhost:8080");
        assert_eq!(http_config.transport_type, TransportType::Http);

        let stdio_config = TransportConfig::auto_detect("mcp-server");
        assert_eq!(stdio_config.transport_type, TransportType::Stdio);
    }

    #[test]
    fn test_parse_connection_string() {
        let config = TransportFactory::parse_connection_string("stdio:mcp-server --arg").unwrap();
        assert_eq!(config.transport_type, TransportType::Stdio);
        assert_eq!(config.endpoint, "mcp-server --arg");

        let config = TransportFactory::parse_connection_string("http://localhost:8080").unwrap();
        assert_eq!(config.transport_type, TransportType::Http);
    }

    #[test]
    fn test_transport_builder() {
        let result = TransportBuilder::stdio("mcp-server")
            .arg("--verbose")
            .timeout(60)
            .build();

        assert!(result.is_ok());
    }

    #[test]
    fn test_transport_type_as_str() {
        assert_eq!(TransportType::Stdio.as_str(), "stdio");
        assert_eq!(TransportType::Http.as_str(), "http");
        assert_eq!(TransportType::HttpSse.as_str(), "sse");
        assert_eq!(TransportType::A2A.as_str(), "a2a");
    }
}
