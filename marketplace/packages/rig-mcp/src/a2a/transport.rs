//! A2A Transport Wrapper
//!
//! This module provides a transport layer implementation that adapts the A2A protocol
//! to work with the MCP transport abstraction.
//!
//! The transport handles:
//! - HTTP client configuration for A2A communication
//! - Request/response serialization
//! - Connection management and health checks
//! - Error handling and retry logic

use crate::transport::{TransportError, TransportResult};
use async_trait::async_trait;
use reqwest::{Client, ClientBuilder};
use serde::{Deserialize, Serialize};
use std::time::Duration;
use tokio::sync::RwLock;
use tracing::{debug, error, info, warn};

use super::converter::TaskWrapper;

/// Configuration for A2A transport.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct A2aTransportConfig {
    /// Base URL for the A2A server
    pub base_url: String,
    /// Request timeout in milliseconds
    pub timeout_ms: u64,
    /// Maximum number of retry attempts
    pub max_retries: usize,
}

impl Default for A2aTransportConfig {
    fn default() -> Self {
        Self {
            base_url: "http://localhost:8080".to_string(),
            timeout_ms: 30000,
            max_retries: 3,
        }
    }
}

impl A2aTransportConfig {
    /// Create a new configuration with the specified base URL.
    pub fn new(base_url: impl Into<String>) -> Self {
        Self {
            base_url: base_url.into(),
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

    /// Build the URL for sending messages to the A2A server.
    pub fn send_url(&self) -> String {
        format!("{}/messages/send", self.base_url.trim_end_matches('/'))
    }

    /// Build the URL for querying a task.
    pub fn task_url(&self, task_id: &str) -> String {
        format!("{}/tasks/{}", self.base_url.trim_end_matches('/'), task_id)
    }
}

/// A2A transport state.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum A2aTransportState {
    /// Transport is disconnected
    Disconnected,
    /// Transport is connecting
    Connecting,
    /// Transport is connected
    Connected,
    /// Transport has an error
    Error,
}

/// A2A HTTP transport for communicating with A2A servers.
///
/// This transport uses HTTP POST to send messages and retrieve task status.
#[async_trait]
pub trait A2aTransport: Send + Sync {
    /// Send a task to the A2A server and receive the response.
    async fn send_task(&self, task: &TaskWrapper) -> TransportResult<TaskWrapper>;

    /// Query the status of a task from the A2A server.
    async fn get_task(&self, task_id: &str) -> TransportResult<TaskWrapper>;

    /// Check if the transport is connected.
    async fn is_connected(&self) -> bool;

    /// Close the transport connection.
    async fn close(&self) -> TransportResult<()>;
}

/// HTTP-based A2A transport implementation.
pub struct HttpA2aTransport {
    /// HTTP client
    client: Client,
    /// Transport configuration
    config: A2aTransportConfig,
    /// Current transport state
    state: RwLock<A2aTransportState>,
}

impl HttpA2aTransport {
    /// Create a new HTTP A2A transport with the given configuration.
    pub fn new(config: A2aTransportConfig) -> Result<Self, TransportError> {
        let timeout = Duration::from_millis(config.timeout_ms);

        let client = ClientBuilder::new()
            .timeout(timeout)
            .connect_timeout(Duration::from_secs(10))
            .build()
            .map_err(|e| {
                TransportError::Internal(format!("Failed to create HTTP client: {}", e))
            })?;

        Ok(Self {
            client,
            config,
            state: RwLock::new(A2aTransportState::Disconnected),
        })
    }

    /// Send an HTTP POST request with retry logic.
    async fn send_with_retry<T, R>(&self, url: &str, body: &T) -> TransportResult<R>
    where
        T: Serialize + ?Sized,
        R: for<'de> Deserialize<'de>,
    {
        let mut last_error = None;

        for attempt in 0..self.config.max_retries {
            debug!(
                "Sending request to {} (attempt {}/{})",
                url,
                attempt + 1,
                self.config.max_retries
            );

            let response = self.client.post(url).json(body).send().await.map_err(|e| {
                warn!("Request failed: {}", e);
                TransportError::ConnectionFailed(format!("HTTP request failed: {}", e))
            })?;

            let status = response.status();

            if status.is_success() {
                let response_text = response.text().await.map_err(|e| {
                    TransportError::Internal(format!("Failed to read response: {}", e))
                })?;

                debug!("Received response: {}", response_text);

                return serde_json::from_str(&response_text).map_err(|e| {
                    error!("Failed to parse response: {}", e);
                    TransportError::Internal(format!("Failed to parse response: {}", e))
                });
            }

            let error_body = response
                .text()
                .await
                .unwrap_or_else(|_| "<unreadable>".to_string());

            last_error = Some(TransportError::Http {
                status: status.as_u16(),
                message: error_body,
            });

            if status.is_client_error() {
                // Client errors (4xx) should not be retried
                break;
            }

            // Exponential backoff for server errors
            let delay_ms = 100 * (2_u64.pow(attempt as u32));
            tokio::time::sleep(Duration::from_millis(delay_ms)).await;
        }

        Err(last_error.unwrap_or_else(|| {
            TransportError::Internal("Request failed after retries".to_string())
        }))
    }

    /// Send an HTTP GET request with retry logic.
    async fn get_with_retry<R>(&self, url: &str) -> TransportResult<R>
    where
        R: for<'de> Deserialize<'de>,
    {
        let mut last_error = None;

        for attempt in 0..self.config.max_retries {
            debug!(
                "GET request to {} (attempt {}/{})",
                url,
                attempt + 1,
                self.config.max_retries
            );

            let response = self.client.get(url).send().await.map_err(|e| {
                warn!("GET request failed: {}", e);
                TransportError::ConnectionFailed(format!("HTTP GET failed: {}", e))
            })?;

            let status = response.status();

            if status.is_success() {
                let response_text = response.text().await.map_err(|e| {
                    TransportError::Internal(format!("Failed to read response: {}", e))
                })?;

                debug!("Received response: {}", response_text);

                return serde_json::from_str(&response_text).map_err(|e| {
                    error!("Failed to parse response: {}", e);
                    TransportError::Internal(format!("Failed to parse response: {}", e))
                });
            }

            let error_body = response
                .text()
                .await
                .unwrap_or_else(|_| "<unreadable>".to_string());

            last_error = Some(TransportError::Http {
                status: status.as_u16(),
                message: error_body,
            });

            if status.is_client_error() {
                break;
            }

            let delay_ms = 100 * (2_u64.pow(attempt as u32));
            tokio::time::sleep(Duration::from_millis(delay_ms)).await;
        }

        Err(last_error.unwrap_or_else(|| {
            TransportError::Internal("GET request failed after retries".to_string())
        }))
    }

    /// Perform a health check on the A2A server.
    async fn health_check(&self) -> TransportResult<bool> {
        let url = format!("{}/health", self.config.base_url.trim_end_matches('/'));

        let response =
            self.client.get(&url).send().await.map_err(|e| {
                TransportError::ConnectionFailed(format!("Health check failed: {}", e))
            })?;

        Ok(response.status().is_success())
    }
}

#[async_trait]
impl A2aTransport for HttpA2aTransport {
    async fn send_task(&self, task: &TaskWrapper) -> TransportResult<TaskWrapper> {
        // Update state to connecting
        *self.state.write().await = A2aTransportState::Connecting;

        #[cfg(feature = "a2a")]
        {
            let url = self.config.send_url();
            let result = self.send_with_retry(&url, &task.0).await?;

            // Update state to connected on success
            *self.state.write().await = A2aTransportState::Connected;

            Ok(TaskWrapper(result))
        }

        #[cfg(not(feature = "a2a"))]
        {
            let _ = (task, self);
            Err(TransportError::Internal(
                "A2A feature not enabled".to_string(),
            ))
        }
    }

    async fn get_task(&self, task_id: &str) -> TransportResult<TaskWrapper> {
        #[cfg(feature = "a2a")]
        {
            let url = self.config.task_url(task_id);
            let result = self.get_with_retry(&url).await?;
            Ok(TaskWrapper(result))
        }

        #[cfg(not(feature = "a2a"))]
        {
            let _ = (task_id, self);
            Err(TransportError::Internal(
                "A2A feature not enabled".to_string(),
            ))
        }
    }

    async fn is_connected(&self) -> bool {
        // Perform health check
        match self.health_check().await {
            Ok(healthy) => {
                *self.state.write().await = if healthy {
                    A2aTransportState::Connected
                } else {
                    A2aTransportState::Error
                };
                healthy
            }
            Err(_) => {
                *self.state.write().await = A2aTransportState::Error;
                false
            }
        }
    }

    async fn close(&self) -> TransportResult<()> {
        *self.state.write().await = A2aTransportState::Disconnected;
        info!("A2A transport closed");
        Ok(())
    }
}

/// Builder for creating A2A transports.
#[derive(Default)]
pub struct A2aTransportBuilder {
    config: A2aTransportConfig,
}

impl A2aTransportBuilder {
    /// Create a new transport builder.
    pub fn new() -> Self {
        Self::default()
    }

    /// Set the base URL for the A2A server.
    pub fn with_base_url(mut self, url: impl Into<String>) -> Self {
        self.config.base_url = url.into();
        self
    }

    /// Set the request timeout.
    pub fn with_timeout(mut self, timeout_ms: u64) -> Self {
        self.config.timeout_ms = timeout_ms;
        self
    }

    /// Set the maximum number of retries.
    pub fn with_max_retries(mut self, max_retries: usize) -> Self {
        self.config.max_retries = max_retries;
        self
    }

    /// Build the HTTP transport.
    pub fn build(self) -> Result<HttpA2aTransport, TransportError> {
        HttpA2aTransport::new(self.config)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_a2a_transport_config_default() {
        let config = A2aTransportConfig::default();
        assert_eq!(config.base_url, "http://localhost:8080");
        assert_eq!(config.timeout_ms, 30000);
        assert_eq!(config.max_retries, 3);
    }

    #[test]
    fn test_a2a_transport_config_builder() {
        let config = A2aTransportConfig::new("http://example.com")
            .with_timeout(5000)
            .with_max_retries(5);

        assert_eq!(config.base_url, "http://example.com");
        assert_eq!(config.timeout_ms, 5000);
        assert_eq!(config.max_retries, 5);
    }

    #[test]
    fn test_a2a_transport_config_send_url() {
        let config = A2aTransportConfig::new("http://localhost:8080/");
        assert_eq!(config.send_url(), "http://localhost:8080/messages/send");
    }

    #[test]
    fn test_a2a_transport_config_task_url() {
        let config = A2aTransportConfig::new("http://localhost:8080/");
        assert_eq!(
            config.task_url("task-123"),
            "http://localhost:8080/tasks/task-123"
        );
    }

    #[test]
    fn test_a2a_transport_state() {
        assert_eq!(
            A2aTransportState::Disconnected,
            A2aTransportState::Disconnected
        );
        assert_ne!(
            A2aTransportState::Connected,
            A2aTransportState::Disconnected
        );
    }

    #[test]
    fn test_a2a_transport_builder_default() {
        let builder = A2aTransportBuilder::new();
        assert_eq!(builder.config.base_url, "http://localhost:8080");
    }

    #[test]
    fn test_a2a_transport_builder_methods() {
        let builder = A2aTransportBuilder::new()
            .with_base_url("http://custom.url")
            .with_timeout(10000)
            .with_max_retries(10);

        assert_eq!(builder.config.base_url, "http://custom.url");
        assert_eq!(builder.config.timeout_ms, 10000);
        assert_eq!(builder.config.max_retries, 10);
    }

    #[test]
    fn test_a2a_transport_config_serialization() {
        let config = A2aTransportConfig::default();
        let json = serde_json::to_value(&config).unwrap();
        assert_eq!(json["baseUrl"], "http://localhost:8080");
        assert_eq!(json["timeoutMs"], 30000);
    }
}
