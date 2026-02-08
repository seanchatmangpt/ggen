//! HTTP Transport for MCP
//!
//! Implements JSON-RPC over HTTP for communicating with MCP servers.

use crate::transport::{
    error::{TransportError, TransportResult},
    JsonRpcRequest, JsonRpcResponse, McpTransport, RequestId, TransportState,
};
use async_trait::async_trait;
use reqwest::{
    header::{HeaderMap, HeaderName, HeaderValue, CONTENT_TYPE},
    Client,
};
use std::{collections::HashMap, sync::Arc, time::Duration};
use tokio::sync::RwLock;

/// Configuration for HTTP transport
#[derive(Debug, Clone)]
pub struct HttpConfig {
    /// Base URL for the MCP server
    pub url: String,
    /// Request timeout in seconds
    pub timeout_secs: u64,
    /// Custom headers to include in requests
    pub headers: HashMap<String, String>,
    /// Authentication token (Bearer token)
    pub auth_token: Option<String>,
    /// Maximum concurrent requests
    pub max_concurrent_requests: usize,
}

impl HttpConfig {
    /// Create a new HTTP config
    pub fn new(url: impl Into<String>) -> Self {
        Self {
            url: url.into(),
            timeout_secs: 30,
            headers: HashMap::new(),
            auth_token: None,
            max_concurrent_requests: 100,
        }
    }

    /// Set the timeout
    pub fn timeout(mut self, secs: u64) -> Self {
        self.timeout_secs = secs;
        self
    }

    /// Add a custom header
    pub fn header(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.headers.insert(key.into(), value.into());
        self
    }

    /// Set the authentication token
    pub fn auth_token(mut self, token: impl Into<String>) -> Self {
        self.auth_token = Some(token.into());
        self
    }

    /// Set max concurrent requests
    pub fn max_concurrent(mut self, max: usize) -> Self {
        self.max_concurrent_requests = max;
        self
    }
}

/// Internal state for HTTP transport
struct HttpState {
    /// Client instance
    client: Client,
    /// Current transport state
    state: TransportState,
    /// Active request count (for concurrency limiting)
    active_requests: usize,
}

impl HttpState {
    fn new(client: Client) -> Self {
        Self {
            client,
            state: TransportState::Connected,
            active_requests: 0,
        }
    }
}

/// HTTP transport for MCP servers
///
/// Communicates with MCP servers over HTTP using JSON-RPC.
pub struct HttpTransport {
    /// Configuration
    config: HttpConfig,
    /// Internal state
    state: Arc<RwLock<HttpState>>,
}

impl HttpTransport {
    /// Create a new HTTP transport
    pub fn new(config: HttpConfig) -> TransportResult<Self> {
        // Build the HTTP client
        let client_builder = Client::builder()
            .timeout(Duration::from_secs(config.timeout_secs))
            .pool_max_idle_per_host(10);

        let client = client_builder
            .build()
            .map_err(|e| TransportError::Internal(format!("Failed to build HTTP client: {}", e)))?;

        Ok(Self {
            config,
            state: Arc::new(RwLock::new(HttpState::new(client))),
        })
    }

    /// Create from a URL string
    pub fn from_url(url: impl Into<String>) -> TransportResult<Self> {
        Self::new(HttpConfig::new(url))
    }

    /// Build the headers for requests
    fn build_headers(&self) -> TransportResult<HeaderMap> {
        let mut headers = HeaderMap::new();

        // Content-Type
        headers.insert(CONTENT_TYPE, HeaderValue::from_static("application/json"));

        // Custom headers
        for (key, value) in &self.config.headers {
            let header_name = HeaderName::from_bytes(key.as_bytes()).map_err(|_| {
                TransportError::InvalidEndpoint(format!("Invalid header name: {}", key))
            })?;
            let header_value = HeaderValue::from_str(value).map_err(|_| {
                TransportError::InvalidEndpoint(format!("Invalid header value: {}", value))
            })?;
            headers.insert(header_name, header_value);
        }

        // Authorization
        if let Some(token) = &self.config.auth_token {
            let auth_value = HeaderValue::from_str(&format!("Bearer {}", token)).map_err(|_| {
                TransportError::Authentication("Invalid auth token format".to_string())
            })?;
            headers.insert(reqwest::header::AUTHORIZATION, auth_value);
        }

        Ok(headers)
    }

    /// Send a raw JSON-RPC request and get the raw response
    async fn send_raw(&self, json: String) -> TransportResult<String> {
        let state = self.state.read().await;

        if state.state != TransportState::Connected {
            return Err(TransportError::NotConnected);
        }

        // Check concurrency limit
        if state.active_requests >= self.config.max_concurrent_requests {
            return Err(TransportError::Internal(
                "Max concurrent requests reached".to_string(),
            ));
        }

        // Clone the client for the request
        let client = state.client.clone();
        let url = self.config.url.clone();
        let headers = self.build_headers()?;

        // Increment active requests
        drop(state);
        let mut state = self.state.write().await;
        state.active_requests += 1;
        drop(state);

        // Send the request asynchronously
        let response = client
            .post(&url)
            .headers(headers)
            .body(json)
            .timeout(Duration::from_secs(30))
            .send()
            .await
            .map_err(|e| TransportError::ConnectionFailed(format!("HTTP request failed: {}", e)))?;

        // Decrement active requests
        let mut state = self.state.write().await;
        state.active_requests -= 1;
        drop(state);

        // Process response
        let status = response.status();
        let body = response
            .text()
            .await
            .map_err(|e| TransportError::Internal(format!("Failed to read response: {}", e)))?;

        if !status.is_success() {
            return Err(TransportError::Http {
                status: status.as_u16(),
                message: body,
            });
        }

        Ok(body)
    }
}

impl Clone for HttpTransport {
    fn clone(&self) -> Self {
        Self {
            config: self.config.clone(),
            state: Arc::clone(&self.state),
        }
    }
}

#[async_trait]
impl McpTransport for HttpTransport {
    async fn send_request(&self, request: JsonRpcRequest) -> TransportResult<JsonRpcResponse> {
        // Ensure request has an ID
        let request = if request.id.is_none() {
            JsonRpcRequest::with_id(request.method, request.params, RequestId::new())
        } else {
            request
        };

        // Serialize to JSON
        let json = request.to_json()?;

        // Send and get response
        let response_text = self.send_raw(json).await?;

        // Parse response
        let response: JsonRpcResponse =
            serde_json::from_str(&response_text).map_err(TransportError::from)?;

        Ok(response)
    }

    async fn send_notification(&self, notification: JsonRpcRequest) -> TransportResult<()> {
        // HTTP doesn't support true notifications (fire-and-forget),
        // but we can send a request and ignore the response
        let json = notification.to_json()?;
        let _ = self.send_raw(json).await?;
        Ok(())
    }

    fn is_connected(&self) -> bool {
        // HTTP is stateless, but we check if we're operational
        true
    }

    async fn close(&self) -> TransportResult<()> {
        // Mark as disconnected
        let mut state = self.state.write().await;
        state.state = TransportState::Disconnected;
        Ok(())
    }
}

/// Builder for creating HTTP transports
pub struct HttpTransportBuilder {
    config: HttpConfig,
}

impl HttpTransportBuilder {
    /// Create a new builder
    pub fn new(url: impl Into<String>) -> Self {
        Self {
            config: HttpConfig::new(url),
        }
    }

    /// Set the timeout
    pub fn timeout(mut self, secs: u64) -> Self {
        self.config = self.config.timeout(secs);
        self
    }

    /// Add a custom header
    pub fn header(mut self, key: impl Into<String>, value: impl Into<String>) -> Self {
        self.config = self.config.header(key, value);
        self
    }

    /// Set the authentication token
    pub fn auth_token(mut self, token: impl Into<String>) -> Self {
        self.config = self.config.auth_token(token);
        self
    }

    /// Set max concurrent requests
    pub fn max_concurrent(mut self, max: usize) -> Self {
        self.config = self.config.max_concurrent(max);
        self
    }

    /// Build the transport
    pub fn build(self) -> TransportResult<HttpTransport> {
        HttpTransport::new(self.config)
    }
}

/// SSE (Server-Sent Events) transport for MCP
///
/// Provides streaming support for servers that implement SSE.
pub struct HttpSseTransport {
    /// Base URL for the MCP server
    url: String,
    /// HTTP client
    client: Client,
    /// Current state
    state: Arc<RwLock<TransportState>>,
}

impl HttpSseTransport {
    /// Create a new SSE transport
    pub fn new(url: impl Into<String>) -> TransportResult<Self> {
        let url = url.into();
        let client = Client::builder()
            .timeout(Duration::from_secs(30))
            .build()
            .map_err(|e| TransportError::Internal(format!("Failed to build HTTP client: {}", e)))?;

        Ok(Self {
            url,
            client,
            state: Arc::new(RwLock::new(TransportState::Connected)),
        })
    }

    /// Get the base URL
    pub fn url(&self) -> &str {
        &self.url
    }
}

#[async_trait]
impl McpTransport for HttpSseTransport {
    async fn send_request(&self, request: JsonRpcRequest) -> TransportResult<JsonRpcResponse> {
        // Use standard HTTP for requests
        let json = request.to_json()?;

        let response = self
            .client
            .post(&self.url)
            .header(CONTENT_TYPE, "application/json")
            .body(json)
            .send()
            .await
            .map_err(|e| TransportError::ConnectionFailed(format!("HTTP request failed: {}", e)))?;

        if !response.status().is_success() {
            let status = response.status();
            let body = response.text().await.unwrap_or_default();
            return Err(TransportError::Http {
                status: status.as_u16(),
                message: body,
            });
        }

        let response_text = response
            .text()
            .await
            .map_err(|e| TransportError::Internal(format!("Failed to read response: {}", e)))?;

        serde_json::from_str(&response_text).map_err(TransportError::from)
    }

    async fn send_notification(&self, notification: JsonRpcRequest) -> TransportResult<()> {
        let json = notification.to_json()?;
        self.client
            .post(&self.url)
            .header(CONTENT_TYPE, "application/json")
            .body(json)
            .send()
            .await
            .map_err(|e| TransportError::ConnectionFailed(format!("HTTP request failed: {}", e)))?;
        Ok(())
    }

    fn is_connected(&self) -> bool {
        true
    }

    async fn close(&self) -> TransportResult<()> {
        let mut state = self.state.write().await;
        *state = TransportState::Disconnected;
        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_http_config_builder() {
        let config = HttpConfig::new("http://localhost:8080")
            .timeout(60)
            .header("X-Custom", "value")
            .auth_token("test-token")
            .max_concurrent(50);

        assert_eq!(config.url, "http://localhost:8080");
        assert_eq!(config.timeout_secs, 60);
        assert_eq!(config.headers.get("X-Custom"), Some(&"value".to_string()));
        assert_eq!(config.auth_token, Some("test-token".to_string()));
        assert_eq!(config.max_concurrent_requests, 50);
    }

    #[test]
    fn test_http_transport_builder() {
        let transport = HttpTransportBuilder::new("http://localhost:8080")
            .timeout(120)
            .auth_token("secret")
            .build()
            .unwrap();

        assert_eq!(transport.config.url, "http://localhost:8080");
        assert_eq!(transport.config.auth_token, Some("secret".to_string()));
    }

    #[test]
    fn test_transport_clone() {
        let transport = HttpTransport::from_url("http://localhost:8080").unwrap();
        let cloned = transport.clone();

        assert_eq!(transport.config.url, cloned.config.url);
    }

    #[test]
    fn test_headers_building() {
        let transport = HttpTransport::new(
            HttpConfig::new("http://localhost:8080")
                .header("X-API-Key", "test123")
                .auth_token("token"),
        )
        .unwrap();

        let headers = transport.build_headers().unwrap();
        assert_eq!(headers.get(CONTENT_TYPE).unwrap(), "application/json");
        assert_eq!(headers.get("X-API-Key").unwrap(), "test123");
        assert_eq!(
            headers.get(reqwest::header::AUTHORIZATION).unwrap(),
            "Bearer token"
        );
    }

    #[test]
    fn test_invalid_header_name() {
        let result =
            HttpTransport::new(HttpConfig::new("http://localhost:8080").header("\n", "value"));
        assert!(result.is_err());
    }
}
