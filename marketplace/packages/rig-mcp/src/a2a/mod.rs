//! MCP to A2A Bridge Layer
//!
//! This module provides a bidirectional bridge between the Model Context Protocol (MCP)
//! and the Agent-to-Agent (A2A) protocol, enabling seamless communication between
//! ggen agents and external MCP servers.
//!
//! ## Architecture
//!
//! The bridge layer consists of several components:
//!
//! - **Converter**: Bidirectional message format translation between MCP and A2A
//! - **Transport**: A2A transport wrapper that adapts MCP transport patterns
//! - **Client**: A2A client wrapper providing MCP-compatible interface
//! - **Bridge**: Main orchestration component coordinating all bridge operations
//!
//! ## Data Flow
//!
//! ```text
//! MCP Request (JsonRpcRequest)
//!     │
//!     ▼
//! McpToA2aBridge
//!     │
//!     ▼
//! Converter (mcp_to_a2a)
//!     │
//!     ▼
//! A2A Task (a2a_rs::Task)
//!     │
//!     ▼
//! A2A Client (AsyncA2AClient)
//!     │
//!     ▼
//! A2A Server Response
//!     │
//!     ▼
//! Converter (a2a_to_mcp)
//!     │
//!     ▼
//! MCP Response (JsonRpcResponse)
//! ```
//!
//! ## Example
//!
//! ```rust,no_run
//! use rig_mcp_integration::a2a::{McpToA2aBridge, BridgeConfig};
//! use rig_mcp_integration::transport::JsonRpcRequest;
//!
//! # async fn example() -> Result<(), Box<dyn std::error::Error>> {
//! let config = BridgeConfig::default();
//! let bridge = McpToA2aBridge::new(config).await?;
//!
//! let request = JsonRpcRequest::new(
//!     "tools/call",
//!     Some(serde_json::json!({
//!         "name": "my_tool",
//!         "arguments": {"input": "value"}
//!     }))
//! );
//!
//! let response = bridge.send_request(request).await?;
//! # Ok(())
//! # }
//! ```

pub mod client;
pub mod converter;
pub mod translator;
pub mod transport;

use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use tokio::sync::RwLock;

use crate::transport::{JsonRpcRequest, JsonRpcResponse, TransportError, TransportResult};

pub use client::{A2aClientWrapper, A2aClientWrapperConfig};
pub use converter::{
    A2aMessageConverter, ConversionContext, ConversionError, ConversionMetrics,
    McpToA2aConversionMode,
};
pub use translator::{
    ProtocolTranslator, ToolSchemaGenerator, TranslationError, TranslationResult, TranslatorConfig,
};
pub use transport::{A2aTransport, A2aTransportConfig};

/// Configuration for the MCP to A2A bridge.
///
/// This configuration controls how the bridge operates, including
/// timeouts, retry behavior, and message conversion options.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BridgeConfig {
    /// Base URL for the A2A server
    pub a2a_server_url: String,

    /// Timeout for requests in milliseconds
    #[serde(default = "default_timeout_ms")]
    pub timeout_ms: u64,

    /// Maximum number of retry attempts
    #[serde(default = "default_max_retries")]
    pub max_retries: usize,

    /// Whether to enable bidirectional conversion
    #[serde(default = "default_bidirectional")]
    pub bidirectional: bool,

    /// Whether to enable message buffering
    #[serde(default = "default_buffering")]
    pub enable_buffering: bool,

    /// Buffer size for message batching
    #[serde(default = "default_buffer_size")]
    pub buffer_size: usize,

    /// Whether to collect conversion metrics
    #[serde(default = "default_metrics")]
    pub collect_metrics: bool,

    /// Agent ID to use for A2A messages
    #[serde(default = "default_agent_id")]
    pub agent_id: String,

    /// Context ID for A2A task tracking
    #[serde(default)]
    pub context_id: Option<String>,
}

fn default_timeout_ms() -> u64 {
    30000
}

fn default_max_retries() -> usize {
    3
}

fn default_bidirectional() -> bool {
    true
}

fn default_buffering() -> bool {
    true
}

fn default_buffer_size() -> usize {
    100
}

fn default_metrics() -> bool {
    false
}

fn default_agent_id() -> String {
    "rig-mcp-bridge".to_string()
}

impl Default for BridgeConfig {
    fn default() -> Self {
        Self {
            a2a_server_url: "http://localhost:8080".to_string(),
            timeout_ms: default_timeout_ms(),
            max_retries: default_max_retries(),
            bidirectional: default_bidirectional(),
            enable_buffering: default_buffering(),
            buffer_size: default_buffer_size(),
            collect_metrics: default_metrics(),
            agent_id: default_agent_id(),
            context_id: None,
        }
    }
}

impl BridgeConfig {
    /// Create a new bridge configuration with the specified A2A server URL.
    pub fn new(a2a_server_url: impl Into<String>) -> Self {
        Self {
            a2a_server_url: a2a_server_url.into(),
            ..Default::default()
        }
    }

    /// Set the timeout for requests.
    pub fn with_timeout(mut self, timeout_ms: u64) -> Self {
        self.timeout_ms = timeout_ms;
        self
    }

    /// Set the maximum number of retry attempts.
    pub fn with_max_retries(mut self, max_retries: usize) -> Self {
        self.max_retries = max_retries;
        self
    }

    /// Enable or disable bidirectional conversion.
    pub fn with_bidirectional(mut self, bidirectional: bool) -> Self {
        self.bidirectional = bidirectional;
        self
    }

    /// Set the agent ID for A2A messages.
    pub fn with_agent_id(mut self, agent_id: impl Into<String>) -> Self {
        self.agent_id = agent_id.into();
        self
    }

    /// Set the context ID for A2A task tracking.
    pub fn with_context_id(mut self, context_id: impl Into<String>) -> Self {
        self.context_id = Some(context_id.into());
        self
    }

    /// Enable collection of conversion metrics.
    pub fn with_metrics(mut self, collect_metrics: bool) -> Self {
        self.collect_metrics = collect_metrics;
        self
    }
}

/// Bridge state tracking connection status and metrics.
#[derive(Debug, Clone, Default)]
pub struct BridgeState {
    /// Whether the bridge is connected
    pub is_connected: bool,
    /// Number of requests sent
    pub requests_sent: u64,
    /// Number of responses received
    pub responses_received: u64,
    /// Number of errors encountered
    pub errors: u64,
    /// Last error message
    pub last_error: Option<String>,
}

/// The main MCP to A2A bridge component.
///
/// This component coordinates all bridge operations, including:
/// - Message conversion between MCP and A2A formats
/// - Transport management for A2A communication
/// - Request/response correlation and tracking
/// - Error handling and retry logic
#[derive(Clone)]
pub struct McpToA2aBridge {
    /// Bridge configuration
    #[allow(dead_code)]
    config: BridgeConfig,
    /// A2A client wrapper
    client: A2aClientWrapper,
    /// Message converter
    converter: Arc<A2aMessageConverter>,
    /// Bridge state
    state: Arc<RwLock<BridgeState>>,
}

impl McpToA2aBridge {
    /// Create a new MCP to A2A bridge with the given configuration.
    ///
    /// # Errors
    ///
    /// Returns an error if the A2A client cannot be initialized.
    pub async fn new(config: BridgeConfig) -> Result<Self, TransportError> {
        let transport_config = A2aTransportConfig {
            base_url: config.a2a_server_url.clone(),
            timeout_ms: config.timeout_ms,
            max_retries: config.max_retries,
        };

        let converter = Arc::new(A2aMessageConverter::new(ConversionContext {
            agent_id: config.agent_id.clone(),
            context_id: config
                .context_id
                .clone()
                .unwrap_or_else(|| format!("ctx-{}", uuid::Uuid::new_v4())),
            bidirectional: config.bidirectional,
            collect_metrics: config.collect_metrics,
        }));

        let client =
            A2aClientWrapper::new_with_converter(transport_config, converter.clone()).await?;

        Ok(Self {
            config,
            client,
            converter,
            state: Arc::new(RwLock::new(BridgeState::default())),
        })
    }

    /// Send an MCP request through the bridge and receive the response.
    ///
    /// This method:
    /// 1. Converts the MCP request to A2A format
    /// 2. Sends the request via the A2A client
    /// 3. Converts the A2A response back to MCP format
    ///
    /// # Errors
    ///
    /// Returns an error if:
    /// - Request conversion fails
    /// - A2A communication fails
    /// - Response conversion fails
    pub async fn send_request(&self, request: JsonRpcRequest) -> TransportResult<JsonRpcResponse> {
        // Update state
        {
            let mut state = self.state.write().await;
            state.requests_sent = state.requests_sent.saturating_add(1);
        }

        // Convert MCP request to A2A task
        let task = self
            .converter
            .mcp_request_to_a2a_task(&request)
            .await
            .map_err(|e| {
                let err_msg = e.to_string();
                let mut state = self.state.blocking_write();
                state.errors = state.errors.saturating_add(1);
                state.last_error = Some(err_msg.clone());
                TransportError::Internal(format!("Request conversion failed: {}", err_msg))
            })?;

        // Send via A2A client
        let response_task = self.client.send_task(&task).await.map_err(|e| {
            let err_msg = e.to_string();
            let mut state = self.state.blocking_write();
            state.errors = state.errors.saturating_add(1);
            state.last_error = Some(err_msg.clone());
            TransportError::Internal(format!("A2A client error: {}", err_msg))
        })?;

        // Convert A2A task response back to MCP response
        let mcp_response = self
            .converter
            .a2a_task_to_mcp_response(&response_task, &request)
            .await
            .map_err(|e| {
                let err_msg = e.to_string();
                let mut state = self.state.blocking_write();
                state.errors = state.errors.saturating_add(1);
                state.last_error = Some(err_msg.clone());
                TransportError::Internal(format!("Response conversion failed: {}", err_msg))
            })?;

        // Update state
        {
            let mut state = self.state.write().await;
            state.responses_received = state.responses_received.saturating_add(1);
            state.last_error = None;
        }

        Ok(mcp_response)
    }

    /// Check if the bridge is connected to the A2A server.
    pub async fn is_connected(&self) -> bool {
        self.client.is_connected().await
    }

    /// Get the current bridge state.
    pub async fn state(&self) -> BridgeState {
        self.state.read().await.clone()
    }

    /// Get conversion metrics if metrics collection is enabled.
    pub async fn metrics(&self) -> Option<ConversionMetrics> {
        self.converter.metrics().await
    }

    /// Reset the bridge state (e.g., for testing).
    pub async fn reset_state(&self) {
        let mut state = self.state.write().await;
        *state = BridgeState::default();
    }

    /// Get a reference to the underlying A2A client.
    pub fn client(&self) -> &A2aClientWrapper {
        &self.client
    }

    /// Get a reference to the message converter.
    pub fn converter(&self) -> &A2aMessageConverter {
        &self.converter
    }
}

/// Async trait for bridge operations to allow mocking in tests.
#[async_trait]
pub trait Bridge: Send + Sync {
    /// Send a request through the bridge and receive the response.
    async fn send_request(&self, request: JsonRpcRequest) -> TransportResult<JsonRpcResponse>;

    /// Check if the bridge is connected.
    async fn is_connected(&self) -> bool;
}

#[async_trait]
impl Bridge for McpToA2aBridge {
    async fn send_request(&self, request: JsonRpcRequest) -> TransportResult<JsonRpcResponse> {
        self.send_request(request).await
    }

    async fn is_connected(&self) -> bool {
        self.is_connected().await
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::json;

    #[test]
    fn test_bridge_config_default() {
        let config = BridgeConfig::default();
        assert_eq!(config.timeout_ms, 30000);
        assert_eq!(config.max_retries, 3);
        assert_eq!(config.bidirectional, true);
        assert_eq!(config.agent_id, "rig-mcp-bridge");
    }

    #[test]
    fn test_bridge_config_builder() {
        let config = BridgeConfig::new("http://example.com")
            .with_timeout(5000)
            .with_max_retries(5)
            .with_agent_id("test-agent");

        assert_eq!(config.a2a_server_url, "http://example.com");
        assert_eq!(config.timeout_ms, 5000);
        assert_eq!(config.max_retries, 5);
        assert_eq!(config.agent_id, "test-agent");
    }

    #[test]
    fn test_bridge_config_with_context_id() {
        let config = BridgeConfig::default().with_context_id("test-context");

        assert_eq!(config.context_id, Some("test-context".to_string()));
    }

    #[test]
    fn test_bridge_state_default() {
        let state = BridgeState::default();
        assert!(!state.is_connected);
        assert_eq!(state.requests_sent, 0);
        assert_eq!(state.responses_received, 0);
        assert_eq!(state.errors, 0);
        assert!(state.last_error.is_none());
    }

    #[test]
    fn test_bridge_config_serialization() {
        let config = BridgeConfig::default();
        let json = serde_json::to_value(&config).unwrap();
        assert_eq!(json["a2aServerUrl"], "http://localhost:8080");
        assert_eq!(json["timeoutMs"], 30000);
    }
}
