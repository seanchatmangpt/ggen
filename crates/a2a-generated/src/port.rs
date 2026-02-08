//! Port trait implementations
//!
//! This module defines the port abstractions for agent-to-agent
//! communication ports and connectors.

use async_trait::async_trait;
use std::collections::HashMap;

/// PortConfig represents the configuration for a port
#[derive(Debug, Clone)]
pub struct PortConfig {
    /// Unique identifier for the port
    pub id: String,
    /// Name of the port
    pub name: String,
    /// Type of the port
    pub port_type: PortType,
    /// Configuration for the port
    pub config: PortConfigInternal,
    /// Status of the port
    pub status: PortStatus,
    /// Metadata for the port
    pub metadata: HashMap<String, String>,
}

/// Port represents a communication endpoint for an agent
#[derive(Debug, Clone)]
pub struct PortData {
    /// Configuration for the port
    pub config: PortConfig,
    /// Status of the port
    pub status: PortStatus,
}

/// Port types supported by the A2A system
#[derive(Debug, Clone, PartialEq)]
pub enum PortType {
    /// Input port for receiving messages
    Input,
    /// Output port for sending messages
    Output,
    /// Bidirectional port for both sending and receiving
    Bidirectional,
    /// Event port for event-based communication
    Event,
    /// Queue port for message queuing
    Queue,
}

/// Port configuration
#[derive(Debug, Clone)]
pub struct PortConfigInternal {
    /// Maximum message size
    pub max_message_size: usize,
    /// Message timeout in milliseconds
    pub message_timeout: u64,
    /// Whether the port is buffered
    pub buffered: bool,
    /// Buffer size for buffered ports
    pub buffer_size: usize,
    /// Additional configuration parameters
    pub parameters: HashMap<String, serde_json::Value>,
}

impl PortConfigInternal {
    pub fn new() -> Self {
        Self {
            max_message_size: 1024 * 1024, // 1MB
            message_timeout: 30000, // 30 seconds
            buffered: true,
            buffer_size: 100,
            parameters: HashMap::new(),
        }
    }

    pub fn with_max_message_size(mut self, size: usize) -> Self {
        self.max_message_size = size;
        self
    }

    pub fn with_message_timeout(mut self, timeout: u64) -> Self {
        self.message_timeout = timeout;
        self
    }

    pub fn buffered(mut self, buffered: bool) -> Self {
        self.buffered = buffered;
        self
    }

    pub fn with_buffer_size(mut self, size: usize) -> Self {
        self.buffer_size = size;
        self
    }
}

/// Port status during operation
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PortStatus {
    /// Port is not initialized
    Uninitialized,
    /// Port is ready for communication
    Ready,
    /// Port is connected
    Connected,
    /// Port is busy
    Busy,
    /// Port is disconnected
    Disconnected,
    /// Port is in error state
    Error,
    /// Port is being shutdown
    ShuttingDown,
}

/// Port trait for communication ports
#[async_trait]
pub trait Port: Send + Sync {
    /// Port identifier
    fn id(&self) -> &str;

    /// Port name
    fn name(&self) -> &str;

    /// Port type
    fn port_type(&self) -> PortType;

    /// Port status
    fn status(&self) -> PortStatus;

    /// Initialize the port
    async fn initialize(&mut self, config: PortConfig) -> Result<(), PortError>;

    /// Connect the port to another port
    async fn connect(&mut self, target_port_id: &str) -> Result<(), PortError>;

    /// Disconnect the port
    async fn disconnect(&mut self) -> Result<(), PortError>;

    /// Send a message through the port
    async fn send(&mut self, message: &serde_json::Value) -> Result<(), PortError>;

    /// Receive a message from the port
    async fn receive(&mut self) -> Result<serde_json::Value, PortError>;

    /// Check if the port is ready for communication
    async fn is_ready(&self) -> bool;

    /// Get port statistics
    fn get_stats(&self) -> PortStats;

    /// Shutdown the port
    async fn shutdown(&mut self) -> Result<(), PortError>;
}

/// Port statistics
#[derive(Debug, Clone, Default)]
pub struct PortStats {
    /// Number of messages sent
    pub messages_sent: u64,
    /// Number of messages received
    pub messages_received: u64,
    /// Number of connection attempts
    pub connection_attempts: u64,
    /// Number of successful connections
    pub successful_connections: u64,
    /// Number of failed connections
    pub failed_connections: u64,
    /// Total bytes sent
    pub bytes_sent: u64,
    /// Total bytes received
    pub bytes_received: u64,
    /// Last message timestamp
    pub last_message_timestamp: Option<chrono::DateTime<chrono::Utc>>,
}

/// Port error types
#[derive(Debug, Clone, PartialEq)]
pub struct PortError {
    /// Error message
    pub message: String,
    /// Error type
    pub error_type: PortErrorType,
    /// Error details
    pub details: Option<serde_json::Value>,
}

/// Types of port errors
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum PortErrorType {
    /// Port not found
    PortNotFound,
    /// Port not initialized
    PortNotInitialized,
    /// Port already connected
    PortAlreadyConnected,
    /// Port not connected
    PortNotConnected,
    /// Message send failed
    MessageSendFailed,
    /// Message receive failed
    MessageReceiveFailed,
    /// Buffer overflow
    BufferOverflow,
    /// Port timeout
    PortTimeout,
    /// Invalid configuration
    InvalidConfiguration,
    /// Network error
    NetworkError,
    /// Unknown error
    Unknown,
}

impl std::fmt::Display for PortErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            PortErrorType::PortNotFound => write!(f, "Port not found"),
            PortErrorType::PortNotInitialized => write!(f, "Port not initialized"),
            PortErrorType::PortAlreadyConnected => write!(f, "Port already connected"),
            PortErrorType::PortNotConnected => write!(f, "Port not connected"),
            PortErrorType::MessageSendFailed => write!(f, "Message send failed"),
            PortErrorType::MessageReceiveFailed => write!(f, "Message receive failed"),
            PortErrorType::BufferOverflow => write!(f, "Buffer overflow"),
            PortErrorType::PortTimeout => write!(f, "Port timeout"),
            PortErrorType::InvalidConfiguration => write!(f, "Invalid configuration"),
            PortErrorType::NetworkError => write!(f, "Network error"),
            PortErrorType::Unknown => write!(f, "Unknown error"),
        }
    }
}

impl PortData {
    pub fn new(id: String, name: String, port_type: PortType) -> Self {
        Self {
            config: PortConfig {
                id,
                name,
                port_type,
                config: PortConfigInternal::default(),
                status: PortStatus::Uninitialized,
                metadata: HashMap::new(),
            },
            status: PortStatus::Uninitialized,
        }
    }

    pub fn with_metadata(mut self, key: String, value: String) -> Self {
        self.config.metadata.insert(key, value);
        self
    }
}


impl Default for PortConfigInternal {
    fn default() -> Self {
        Self::new()
    }
}

impl PortConfig {
    pub fn new(id: String, name: String, port_type: PortType) -> Self {
        Self {
            id,
            name,
            port_type,
            config: PortConfigInternal::default(),
            status: PortStatus::Uninitialized,
            metadata: HashMap::new(),
        }
    }
}

impl Default for PortConfig {
    fn default() -> Self {
        Self {
            id: "".to_string(),
            name: "".to_string(),
            port_type: PortType::Input,
            config: PortConfigInternal::default(),
            status: PortStatus::Uninitialized,
            metadata: HashMap::new(),
        }
    }
}

impl Default for PortData {
    fn default() -> Self {
        Self {
            config: PortConfig::default(),
            status: PortStatus::Uninitialized,
        }
    }
}

impl PortError {
    pub fn new(message: String, error_type: PortErrorType) -> Self {
        Self {
            message,
            error_type,
            details: None,
        }
    }

    pub fn with_details(mut self, details: serde_json::Value) -> Self {
        self.details = Some(details);
        self
    }
}

/// Basic implementation of Port trait
pub struct BasicPort {
    port: PortData,
    connected_to: Option<String>,
    stats: PortStats,
}

impl BasicPort {
    pub fn new(id: String, name: String, port_type: PortType) -> Self {
        Self {
            port: PortData::new(id, name, port_type),
            connected_to: None,
            stats: PortStats::default(),
        }
    }

    // Helper methods to access port data
    fn id(&self) -> &str {
        &self.port.config.id
    }

    fn name(&self) -> &str {
        &self.port.config.name
    }

    fn port_type(&self) -> PortType {
        self.port.config.port_type.clone()
    }

    fn status(&self) -> PortStatus {
        self.port.status.clone()
    }
}

#[async_trait]
impl Port for BasicPort {
    fn id(&self) -> &str {
        self.id()
    }

    fn name(&self) -> &str {
        self.name()
    }

    fn port_type(&self) -> PortType {
        self.port_type()
    }

    fn status(&self) -> PortStatus {
        self.status()
    }

    async fn initialize(&mut self, config: PortConfig) -> Result<(), PortError> {
        self.port.config = config;
        self.port.status = PortStatus::Ready;
        Ok(())
    }

    async fn connect(&mut self, target_port_id: &str) -> Result<(), PortError> {
        if self.port.status != PortStatus::Ready {
            return Err(PortError::new(
                "Port is not ready for connection".to_string(),
                PortErrorType::PortNotInitialized,
            ));
        }

        self.connected_to = Some(target_port_id.to_string());
        self.port.status = PortStatus::Connected;
        self.stats.connection_attempts += 1;
        self.stats.successful_connections += 1;

        Ok(())
    }

    async fn disconnect(&mut self) -> Result<(), PortError> {
        if self.port.status != PortStatus::Connected {
            return Err(PortError::new(
                "Port is not connected".to_string(),
                PortErrorType::PortNotConnected,
            ));
        }

        self.connected_to = None;
        self.port.status = PortStatus::Disconnected;
        Ok(())
    }

    async fn send(&mut self, message: &serde_json::Value) -> Result<(), PortError> {
        if self.port.status != PortStatus::Connected {
            return Err(PortError::new(
                "Port is not connected".to_string(),
                PortErrorType::PortNotConnected,
            ));
        }

        // Simulate message sending
        self.stats.messages_sent += 1;
        self.stats.bytes_sent += serde_json::to_string(message)
            .unwrap_or_default()
            .len() as u64;
        self.stats.last_message_timestamp = Some(chrono::Utc::now());

        Ok(())
    }

    async fn receive(&mut self) -> Result<serde_json::Value, PortError> {
        if self.port.status != PortStatus::Connected {
            return Err(PortError::new(
                "Port is not connected".to_string(),
                PortErrorType::PortNotConnected,
            ));
        }

        // Simulate message receiving
        self.stats.messages_received += 1;

        Ok(serde_json::json!({
            "status": "received",
            "timestamp": chrono::Utc::now().to_rfc3339()
        }))
    }

    async fn is_ready(&self) -> bool {
        self.port.status == PortStatus::Ready || self.port.status == PortStatus::Connected
    }

    fn get_stats(&self) -> PortStats {
        self.stats.clone()
    }

    async fn shutdown(&mut self) -> Result<(), PortError> {
        if self.connected_to.is_some() {
            self.disconnect().await?;
        }

        self.port.status = PortStatus::ShuttingDown;
        self.port.status = PortStatus::Uninitialized;
        Ok(())
    }
}

/// Port registry for managing multiple ports
pub struct PortRegistry {
    ports: HashMap<String, Box<dyn Port>>,
}

impl PortRegistry {
    pub fn new() -> Self {
        Self {
            ports: HashMap::new(),
        }
    }

    pub fn register_port(&mut self, port: Box<dyn Port>) {
        self.ports.insert(port.id().to_string(), port);
    }

    pub fn get_port(&self, id: &str) -> Option<&dyn Port> {
        self.ports.get(id)
            .map(|port| port.as_ref())
    }

    pub async fn connect_ports(&mut self, source_id: &str, target_id: &str) -> Result<(), PortError> {
        let source_port = self.ports.get_mut(source_id)
            .ok_or_else(|| PortError::new(
                "Source port not found".to_string(),
                PortErrorType::PortNotFound,
            ))?;

        source_port.connect(target_id).await
    }

    pub fn list_ports(&self) -> Vec<&dyn Port> {
        self.ports.values().map(|port| port.as_ref()).collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_port_creation() {
        let port = BasicPort::new("port-123".to_string(), "Test Port".to_string(), PortType::Output);
        assert_eq!(port.id(), "port-123");
        assert_eq!(port.name(), "Test Port");
        assert_eq!(port.port_type(), PortType::Output);
        assert_eq!(port.status(), PortStatus::Uninitialized);
    }

    #[tokio::test]
    async fn test_port_initialization() {
        let mut port = BasicPort::new("port-123".to_string(), "Test Port".to_string(), PortType::Output);
        let config = PortConfig::new()
            .with_max_message_size(2048)
            .with_message_timeout(3000);

        port.initialize(config).await.unwrap();
        assert_eq!(port.status(), PortStatus::Ready);
    }

    #[tokio::test]
    async fn test_port_connection() {
        let mut port1 = BasicPort::new("port-1".to_string(), "Port 1".to_string(), PortType::Output);
        let mut port2 = BasicPort::new("port-2".to_string(), "Port 2".to_string(), PortType::Input);

        port1.initialize(PortConfig::new()).await.unwrap();
        port2.initialize(PortConfig::new()).await.unwrap();

        port1.connect("port-2").await.unwrap();
        assert_eq!(port1.status(), PortStatus::Connected);
    }

    #[tokio::test]
    async fn test_port_send_receive() {
        let mut port = BasicPort::new("port-123".to_string(), "Test Port".to_string(), PortType::Bidirectional);
        port.initialize(PortConfig::new()).await.unwrap();

        let test_message = serde_json::json!({"test": "message"});
        port.send(&test_message).await.unwrap();

        let received = port.receive().await.unwrap();
        assert_eq!(received["status"].as_str().unwrap(), "received");
    }

    #[test]
    fn test_port_registry() {
        let mut registry = PortRegistry::new();

        let port = BasicPort::new("port-123".to_string(), "Test Port".to_string(), PortType::Output);
        registry.register_port(Box::new(port));

        assert!(registry.get_port("port-123").is_some());
        assert_eq!(registry.list_ports().len(), 1);
    }
}