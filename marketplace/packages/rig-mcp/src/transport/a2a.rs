//! A2A Transport Implementation
//!
//! This module provides an A2A (Agent-to-Agent) protocol transport implementation
//! for the MCP transport layer. It bridges the A2A-RS client with the McpTransport trait.

use crate::transport::{
    error::{TransportError, TransportResult},
    JsonRpcRequest, JsonRpcResponse, McpTransport, TransportState,
};
use async_trait::async_trait;
use std::sync::Arc;
use tokio::sync::RwLock;

#[cfg(feature = "a2a")]
use a2a_rs::{
    adapter::transport::http::HttpClient as A2aHttpClient,
    services::client::AsyncA2AClient,
};

/// Configuration for A2A transport
#[derive(Debug, Clone)]
pub struct A2AConfig {
    /// Base URL of the A2A server
    pub base_url: String,
    /// Optional authentication token
    pub auth_token: Option<String>,
    /// Request timeout in seconds
    pub timeout_secs: u64,
}

impl A2AConfig {
    /// Create a new A2A configuration
    pub fn new(base_url: impl Into<String>) -> Self {
        Self {
            base_url: base_url.into(),
            auth_token: None,
            timeout_secs: 30,
        }
    }

    /// Set the authentication token
    pub fn with_auth(mut self, token: impl Into<String>) -> Self {
        self.auth_token = Some(token.into());
        self
    }

    /// Set the timeout
    pub fn with_timeout(mut self, secs: u64) -> Self {
        self.timeout_secs = secs;
        self
    }

    /// Build the configuration into an A2A HTTP client
    #[cfg(feature = "a2a")]
    pub fn build_client(&self) -> A2aHttpClient {
        let client = match &self.auth_token {
            Some(token) => A2aHttpClient::with_auth(self.base_url.clone(), token.clone()),
            None => A2aHttpClient::new(self.base_url.clone()),
        };
        client.with_timeout(self.timeout_secs)
    }
}

impl Default for A2AConfig {
    fn default() -> Self {
        Self::new("http://127.0.0.1:8080")
    }
}

/// Builder for creating A2A transport configurations
#[derive(Debug, Clone)]
pub struct A2ATransportBuilder {
    config: A2AConfig,
}

impl A2ATransportBuilder {
    /// Create a new builder with a base URL
    pub fn new(base_url: impl Into<String>) -> Self {
        Self {
            config: A2AConfig::new(base_url),
        }
    }

    /// Set the authentication token
    pub fn auth_token(mut self, token: impl Into<String>) -> Self {
        self.config = self.config.with_auth(token);
        self
    }

    /// Set the timeout in seconds
    pub fn timeout(mut self, secs: u64) -> Self {
        self.config = self.config.with_timeout(secs);
        self
    }

    /// Build the transport
    pub fn build(self) -> TransportResult<A2ATransport> {
        Ok(A2ATransport::new(self.config))
    }

    /// Build and connect the transport
    pub async fn connect(self) -> TransportResult<A2ATransport> {
        let transport = self.build()?;
        transport.connect().await?;
        Ok(transport)
    }
}

/// A2A Transport implementation
///
/// This transport implements the McpTransport trait using the A2A-RS client
/// for communication with A2A protocol servers.
#[derive(Debug, Clone)]
pub struct A2ATransport {
    /// Transport configuration
    config: A2AConfig,
    /// Current transport state
    state: Arc<RwLock<TransportState>>,
}

impl A2ATransport {
    /// Create a new A2A transport with the given configuration
    pub fn new(config: A2AConfig) -> Self {
        Self {
            config,
            state: Arc::new(RwLock::new(TransportState::Disconnected)),
        }
    }

    /// Create a new A2A transport from a URL string
    pub fn from_url(url: impl Into<String>) -> Self {
        Self::new(A2AConfig::new(url))
    }

    /// Get the current transport state
    pub async fn state(&self) -> TransportState {
        *self.state.read().await
    }

    /// Connect to the A2A server
    pub async fn connect(&self) -> TransportResult<()> {
        let mut state = self.state.write().await;
        *state = TransportState::Connecting;

        #[cfg(feature = "a2a")]
        {
            // Create a client to verify connectivity
            let _client = self.config.build_client();
            // In a full implementation, we would send a ping here
            *state = TransportState::Connected;
            Ok(())
        }

        #[cfg(not(feature = "a2a"))]
        {
            *state = TransportState::Connected;
            Ok(())
        }
    }

    /// Disconnect from the A2A server
    pub async fn disconnect(&self) -> TransportResult<()> {
        let mut state = self.state.write().await;
        *state = TransportState::ShuttingDown;

        // Perform cleanup here
        *state = TransportState::Disconnected;
        Ok(())
    }

    /// Get the base URL
    pub fn base_url(&self) -> &str {
        &self.config.base_url
    }

    /// Get the configuration
    pub fn config(&self) -> &A2AConfig {
        &self.config
    }
}

#[async_trait]
impl McpTransport for A2ATransport {
    /// Send a JSON-RPC request and wait for the response
    async fn send_request(&self, request: JsonRpcRequest) -> TransportResult<JsonRpcResponse> {
        // Ensure we're connected
        let state = self.state.read().await;
        if *state != TransportState::Connected {
            return Err(TransportError::NotConnected);
        }
        drop(state);

        #[cfg(feature = "a2a")]
        {
            // Convert MCP JSON-RPC request to JSON string
            let request_json = serde_json::to_string(&request)
                .map_err(|e| TransportError::Internal(format!("Failed to serialize request: {}", e)))?;

            // Create A2A client and send request
            let client = self.config.build_client();

            // Send the request and parse the response
            let response_text = client
                .send_raw_request(&request_json)
                .await
                .map_err(|e| convert_a2a_error(e))?;

            // Parse the response
            serde_json::from_str(&response_text).map_err(TransportError::from)
        }

        #[cfg(not(feature = "a2a"))]
        {
            let _ = request;
            Err(TransportError::UnsupportedOperation(
                "A2A transport requires the 'a2a' feature to be enabled".to_string(),
            ))
        }
    }

    /// Send a notification (no response expected)
    async fn send_notification(&self, notification: JsonRpcRequest) -> TransportResult<()> {
        // Ensure we're connected
        let state = self.state.read().await;
        if *state != TransportState::Connected {
            return Err(TransportError::NotConnected);
        }
        drop(state);

        #[cfg(feature = "a2a")]
        {
            let notification_json = serde_json::to_string(&notification)
                .map_err(|e| TransportError::Internal(format!("Failed to serialize notification: {}", e)))?;

            let client = self.config.build_client();

            client
                .send_raw_request(&notification_json)
                .await
                .map_err(convert_a2a_error)?;

            Ok(())
        }

        #[cfg(not(feature = "a2a"))]
        {
            let _ = notification;
            Err(TransportError::UnsupportedOperation(
                "A2A transport requires the 'a2a' feature to be enabled".to_string(),
            ))
        }
    }

    /// Check if the transport is connected
    fn is_connected(&self) -> bool {
        // Use try_read for non-blocking check
        if let Ok(state) = self.state.try_read() {
            *state == TransportState::Connected
        } else {
            false
        }
    }

    /// Close the transport connection
    async fn close(&self) -> TransportResult<()> {
        self.disconnect().await
    }
}

/// Convert A2A errors to TransportError
#[cfg(feature = "a2a")]
fn convert_a2a_error(error: a2a_rs::domain::A2AError) -> TransportError {
    use a2a_rs::domain::A2AError;
    match error {
        A2AError::JsonParse(e) => TransportError::Json(e),
        A2AError::Io(e) => TransportError::Io(e),
        A2AError::InvalidRequest(msg) => TransportError::InvalidParams(msg),
        A2AError::InvalidParams(msg) => TransportError::InvalidParams(msg),
        A2AError::MethodNotFound(msg) => TransportError::MethodNotFound(msg),
        A2AError::UnsupportedOperation(msg) => TransportError::UnsupportedOperation(msg),
        A2AError::Internal(msg) => TransportError::Internal(msg),
        A2AError::TaskNotFound(msg) => TransportError::Internal(format!("Task not found: {}", msg)),
        A2AError::JsonRpc { code, message, .. } => {
            TransportError::json_rpc(code, message)
        }
        _ => TransportError::Internal(format!("A2A error: {}", error)),
    }
}

/// Convert A2A URL to transport configuration
///
/// Parses URLs in the format:
/// - `a2a://localhost:8080`
/// - `a2a://example.com:9090`
/// - `http://localhost:8080` (auto-detected as A2A if explicitly specified)
pub fn parse_a2a_url(url: &str) -> TransportResult<A2AConfig> {
    let url = url.trim();

    // Handle a2a:// scheme
    if let Some(rest) = url.strip_prefix("a2a://") {
        return Ok(A2AConfig::new(format!("http://{}", rest)));
    }

    // Handle http:// and https:// schemes (could be A2A endpoints)
    if url.starts_with("http://") || url.starts_with("https://") {
        return Ok(A2AConfig::new(url.to_string()));
    }

    // Treat as bare URL
    Ok(A2AConfig::new(format!("http://{}", url)))
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_a2a_config_default() {
        let config = A2AConfig::default();
        assert_eq!(config.base_url, "http://127.0.0.1:8080");
        assert!(config.auth_token.is_none());
        assert_eq!(config.timeout_secs, 30);
    }

    #[test]
    fn test_a2a_config_builder() {
        let config = A2AConfig::new("http://localhost:9090")
            .with_auth("test-token")
            .with_timeout(60);

        assert_eq!(config.base_url, "http://localhost:9090");
        assert_eq!(config.auth_token, Some("test-token".to_string()));
        assert_eq!(config.timeout_secs, 60);
    }

    #[test]
    fn test_a2a_transport_builder() {
        let builder = A2ATransportBuilder::new("http://localhost:8080")
            .auth_token("token")
            .timeout(45);

        let transport = builder.build().unwrap();
        assert_eq!(transport.base_url(), "http://localhost:8080");
        assert_eq!(transport.config().auth_token, Some("token".to_string()));
    }

    #[test]
    fn test_parse_a2a_url() {
        let config = parse_a2a_url("a2a://localhost:8080").unwrap();
        assert_eq!(config.base_url, "http://localhost:8080");

        let config = parse_a2a_url("http://example.com:9090").unwrap();
        assert_eq!(config.base_url, "http://example.com:9090");

        let config = parse_a2a_url("https://api.example.com").unwrap();
        assert_eq!(config.base_url, "https://api.example.com");
    }

    #[test]
    fn test_a2a_transport_from_url() {
        let transport = A2ATransport::from_url("http://localhost:8080");
        assert_eq!(transport.base_url(), "http://localhost:8080");
    }
}
