//! A2A Client Wrapper
//!
//! This module provides a wrapper around A2A clients that integrates with the
//! MCP transport layer, enabling seamless communication between MCP and A2A protocols.
//!
//! The client wrapper handles:
//! - A2A task lifecycle management
//! - Message sending and task polling
//! - Response aggregation from A2A tasks
//! - Error handling and retry logic

use crate::transport::TransportError;
use crate::transport::TransportResult;
use async_trait::async_trait;
use serde::{Deserialize, Serialize};
use std::sync::Arc;
use std::time::Duration;
use tokio::sync::RwLock;
use tracing::{debug, info, warn};

use super::converter::{A2aMessageConverter, ConversionContext, TaskWrapper};
use super::transport::{A2aTransport, A2aTransportConfig, HttpA2aTransport};

/// Configuration for the A2A client wrapper.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct A2aClientWrapperConfig {
    /// A2A transport configuration
    pub transport: A2aTransportConfig,
    /// Conversion context
    pub conversion: ConversionContext,
    /// Maximum time to wait for task completion (milliseconds)
    pub max_wait_time_ms: u64,
    /// Polling interval for task status (milliseconds)
    pub poll_interval_ms: u64,
}

impl Default for A2aClientWrapperConfig {
    fn default() -> Self {
        Self {
            transport: A2aTransportConfig::default(),
            conversion: ConversionContext::default(),
            max_wait_time_ms: 60000, // 1 minute
            poll_interval_ms: 500,    // 500ms
        }
    }
}

impl A2aClientWrapperConfig {
    /// Create a new configuration with the specified A2A server URL.
    pub fn new(a2a_server_url: impl Into<String>) -> Self {
        Self {
            transport: A2aTransportConfig::new(a2a_server_url),
            ..Default::default()
        }
    }

    /// Set the maximum wait time for task completion.
    pub fn with_max_wait_time(mut self, max_wait_time_ms: u64) -> Self {
        self.max_wait_time_ms = max_wait_time_ms;
        self
    }

    /// Set the polling interval for task status.
    pub fn with_poll_interval(mut self, poll_interval_ms: u64) -> Self {
        self.poll_interval_ms = poll_interval_ms;
        self
    }

    /// Set the agent ID for A2A messages.
    pub fn with_agent_id(mut self, agent_id: impl Into<String>) -> Self {
        self.conversion.agent_id = agent_id.into();
        self
    }
}

/// A2A client wrapper that provides MCP-compatible interface.
///
/// This wrapper integrates A2A transport and message conversion to provide
/// a unified interface for sending requests and receiving responses.
#[derive(Clone)]
pub struct A2aClientWrapper {
    /// A2A transport
    transport: Arc<dyn A2aTransport>,
    /// Message converter
    converter: Arc<A2aMessageConverter>,
    /// Client configuration
    config: A2aClientWrapperConfig,
    /// Pending tasks map
    pending_tasks: Arc<RwLock<std::collections::HashMap<String, TaskWrapper>>>,
}

impl A2aClientWrapper {
    /// Create a new A2A client wrapper with the given configuration.
    pub async fn new(config: A2aClientWrapperConfig) -> Result<Self, TransportError> {
        let http_transport = HttpA2aTransport::new(config.transport.clone())?;

        let converter = Arc::new(A2aMessageConverter::new(config.conversion.clone()));

        Ok(Self {
            transport: Arc::new(http_transport),
            converter,
            config,
            pending_tasks: Arc::new(RwLock::new(std::collections::HashMap::new())),
        })
    }

    /// Create a new client wrapper with a specific converter.
    pub async fn new_with_converter(
        transport_config: A2aTransportConfig,
        converter: Arc<A2aMessageConverter>,
    ) -> Result<Self, TransportError> {
        let http_transport = HttpA2aTransport::new(transport_config.clone())?;

        let config = A2aClientWrapperConfig {
            transport: transport_config,
            conversion: ConversionContext::default(),
            max_wait_time_ms: 60000,
            poll_interval_ms: 500,
        };

        Ok(Self {
            transport: Arc::new(http_transport),
            converter,
            config,
            pending_tasks: Arc::new(RwLock::new(std::collections::HashMap::new())),
        })
    }

    /// Create a new client wrapper with a custom transport.
    pub fn with_transport(
        transport: Arc<dyn A2aTransport>,
        converter: Arc<A2aMessageConverter>,
    ) -> Self {
        let config = A2aClientWrapperConfig {
            transport: A2aTransportConfig::default(),
            conversion: ConversionContext::default(),
            max_wait_time_ms: 60000,
            poll_interval_ms: 500,
        };

        Self {
            transport,
            converter,
            config,
            pending_tasks: Arc::new(RwLock::new(std::collections::HashMap::new())),
        }
    }

    /// Send a task to the A2A server and wait for completion.
    ///
    /// This method:
    /// 1. Sends the task to the A2A server
    /// 2. Polls for task completion
    /// 3. Returns the completed task
    pub async fn send_task(&self, task: &TaskWrapper) -> TransportResult<TaskWrapper> {
        info!("Sending task to A2A server");

        // Send the task
        let mut response_task = self.transport.send_task(task).await?;

        #[cfg(feature = "a2a")]
        let task_id = response_task.0.id.clone();

        #[cfg(not(feature = "a2a"))]
        let task_id = String::from("unknown");

        // Poll for completion if needed
        response_task = self.wait_for_task_completion(&task_id).await?;

        // Clean up pending tasks
        let mut pending = self.pending_tasks.write().await;
        pending.remove(&task_id);

        debug!("Task {} completed", task_id);
        Ok(response_task)
    }

    /// Wait for a task to complete by polling its status.
    async fn wait_for_task_completion(&self, task_id: &str) -> TransportResult<TaskWrapper> {
        let start_time = std::time::Instant::now();
        let max_duration = Duration::from_millis(self.config.max_wait_time_ms);
        let poll_interval = Duration::from_millis(self.config.poll_interval_ms);

        loop {
            // Check timeout
            if start_time.elapsed() > max_duration {
                warn!("Task {} timed out after {:?}", task_id, max_duration);
                return Err(TransportError::Timeout);
            }

            // Get current task status
            let task = self.transport.get_task(task_id).await?;

            #[cfg(feature = "a2a")]
            {
                use a2a_rs::domain::core::task::TaskState;

                match task.0.status.state {
                    TaskState::Completed | TaskState::Failed | TaskState::Canceled => {
                        return Ok(task);
                    }
                    TaskState::Working | TaskState::Submitted | TaskState::InputRequired => {
                        // Continue polling
                        debug!("Task {} still in state {:?}", task_id, task.0.status.state);
                    }
                    _ => {
                        // Terminal states
                        return Ok(task);
                    }
                }
            }

            #[cfg(not(feature = "a2a"))]
            {
                // If A2A is not enabled, just return immediately
                return Ok(task);
            }

            // Store pending task
            {
                let mut pending = self.pending_tasks.write().await;
                pending.insert(task_id.to_string(), task);
            }

            // Wait before polling again
            tokio::time::sleep(poll_interval).await;
        }
    }

    /// Check if the client is connected to the A2A server.
    pub async fn is_connected(&self) -> bool {
        self.transport.is_connected().await
    }

    /// Get the number of pending tasks.
    pub async fn pending_task_count(&self) -> usize {
        self.pending_tasks.read().await.len()
    }

    /// Get a reference to the converter.
    pub fn converter(&self) -> &A2aMessageConverter {
        &self.converter
    }

    /// Get a reference to the transport.
    pub fn transport(&self) -> &dyn A2aTransport {
        self.transport.as_ref()
    }

    /// Close the client and release resources.
    pub async fn close(&self) -> TransportResult<()> {
        self.transport.close().await?;
        info!("A2A client wrapper closed");
        Ok(())
    }
}

/// Async trait for A2A client operations.
#[async_trait]
pub trait AsyncA2aClient: Send + Sync {
    /// Send a task and wait for completion.
    async fn send_task(&self, task: &TaskWrapper) -> TransportResult<TaskWrapper>;

    /// Check if the client is connected.
    async fn is_connected(&self) -> bool;
}

#[async_trait]
impl AsyncA2aClient for A2aClientWrapper {
    async fn send_task(&self, task: &TaskWrapper) -> TransportResult<TaskWrapper> {
        self.send_task(task).await
    }

    async fn is_connected(&self) -> bool {
        self.is_connected().await
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_a2a_client_wrapper_config_default() {
        let config = A2aClientWrapperConfig::default();
        assert_eq!(config.max_wait_time_ms, 60000);
        assert_eq!(config.poll_interval_ms, 500);
    }

    #[test]
    fn test_a2a_client_wrapper_config_builder() {
        let config = A2aClientWrapperConfig::new("http://localhost:8080")
            .with_max_wait_time(120000)
            .with_poll_interval(1000)
            .with_agent_id("test-agent");

        assert_eq!(config.max_wait_time_ms, 120000);
        assert_eq!(config.poll_interval_ms, 1000);
        assert_eq!(config.conversion.agent_id, "test-agent");
    }

    #[test]
    fn test_a2a_client_wrapper_config_serialization() {
        let config = A2aClientWrapperConfig::default();
        let json = serde_json::to_value(&config).unwrap();
        assert_eq!(json["maxWaitTimeMs"], 60000);
        assert_eq!(json["pollIntervalMs"], 500);
    }
}
