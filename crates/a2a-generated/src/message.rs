//! Message domain structs and traits
//!
//! This module defines the message abstractions for agent-to-agent
//! communication patterns.

use std::collections::HashMap;

/// Message represents a communication between agents
#[derive(Debug, Clone, PartialEq)]
pub struct Message {
    /// Unique identifier for the message
    pub id: String,
    /// Type of the message
    pub message_type: MessageType,
    /// Source agent ID
    pub source: String,
    /// Target agent ID (None for broadcast)
    pub target: Option<String>,
    /// Message payload
    pub payload: MessagePayload,
    /// Message metadata
    pub metadata: HashMap<String, String>,
    /// Timestamp when the message was created
    pub timestamp: chrono::DateTime<chrono::Utc>,
    /// Message priority
    pub priority: MessagePriority,
    /// Message status
    pub status: MessageStatus,
}

/// Message types supported by the A2A system
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MessageType {
    /// Task request message
    TaskRequest,
    /// Task response message
    TaskResponse,
    /// Agent registration message
    AgentRegistration,
    /// Agent heartbeat message
    AgentHeartbeat,
    /// Broadcast message to all agents
    Broadcast,
    /// Direct message to specific agent
    Direct,
    /// System message (internal use)
    System,
    /// Custom message type
    Custom(String),
}

/// Message payload structure
#[derive(Debug, Clone, PartialEq)]
pub struct MessagePayload {
    /// Message content
    pub content: serde_json::Value,
    /// Message schema version
    pub schema_version: String,
    /// Content type of the message
    pub content_type: String,
}

/// Message priority levels
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord)]
pub enum MessagePriority {
    Low,
    Normal,
    High,
    Urgent,
}

/// Message status during processing
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MessageStatus {
    /// Message created and ready to send
    Created,
    /// Message is queued for delivery
    Queued,
    /// Message is in transit
    InTransit,
    /// Message has been delivered
    Delivered,
    /// Message has been processed
    Processed,
    /// Message delivery failed
    Failed,
    /// Message was cancelled
    Cancelled,
}

/// Trait for message handling
pub trait MessageHandler: Send + Sync {
    /// Handle an incoming message
    fn handle_message(&self, message: Message) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<MessageResponse, MessageError>> + Send>>;

    /// Check if this handler can process the given message type
    fn can_handle(&self, message_type: &MessageType) -> bool;

    /// Get the message handler capabilities
    fn capabilities(&self) -> MessageCapabilities;
}

/// Response to a message
#[derive(Debug, Clone, PartialEq)]
pub struct MessageResponse {
    /// Response status
    pub status: ResponseStatus,
    /// Response payload
    pub payload: Option<MessagePayload>,
    /// Response metadata
    pub metadata: HashMap<String, String>,
    /// Message ID this response is for
    pub in_reply_to: String,
}

/// Response status types
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ResponseStatus {
    /// Success response
    Success,
    /// Error response
    Error,
    /// Informational response
    Information,
    /// Acknowledgement response
    Ack,
}

/// Message handling error
#[derive(Debug, Clone, PartialEq)]
pub struct MessageError {
    /// Error message
    pub message: String,
    /// Error type
    pub error_type: MessageErrorType,
    /// Original message ID
    pub message_id: Option<String>,
}

/// Types of message errors
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum MessageErrorType {
    /// Unsupported message type
    UnsupportedMessageType,
    /// Invalid message format
    InvalidMessageFormat,
    /// Message processing failed
    ProcessingFailed,
    /// Message delivery failed
    DeliveryFailed,
    /// Handler not found
    HandlerNotFound,
    /// Timeout error
    Timeout,
    /// Unknown error
    Unknown,
}

impl std::fmt::Display for MessageErrorType {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            MessageErrorType::UnsupportedMessageType => write!(f, "Unsupported message type"),
            MessageErrorType::InvalidMessageFormat => write!(f, "Invalid message format"),
            MessageErrorType::ProcessingFailed => write!(f, "Message processing failed"),
            MessageErrorType::DeliveryFailed => write!(f, "Message delivery failed"),
            MessageErrorType::HandlerNotFound => write!(f, "Handler not found"),
            MessageErrorType::Timeout => write!(f, "Timeout error"),
            MessageErrorType::Unknown => write!(f, "Unknown error"),
        }
    }
}

/// Message handler capabilities
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct MessageCapabilities {
    /// Message types this handler can process
    pub supported_types: Vec<MessageType>,
    /// Maximum message size
    pub max_message_size: usize,
    /// Whether this handler requires acknowledgements
    pub requires_ack: bool,
}

/// Message broker for routing messages between agents
pub struct MessageBroker {
    /// Registered message handlers
    handlers: HashMap<String, Box<dyn MessageHandler>>,
}

impl MessageBroker {
    pub fn new() -> Self {
        Self {
            handlers: HashMap::new(),
        }
    }

    pub fn register_handler(&mut self, name: String, handler: Box<dyn MessageHandler>) {
        self.handlers.insert(name, handler);
    }

    pub async fn send_message(&mut self, message: Message) -> Result<(), MessageError> {
        // Find appropriate handler for the message
        for (_name, handler) in self.handlers.iter() {
            if handler.can_handle(&message.message_type) {
                let _response = handler.handle_message(message.clone()).await?;
                // Handle response logic here
                return Ok(());
            }
        }

        Err(MessageError::new(
            "No handler found for message type".to_string(),
            MessageErrorType::HandlerNotFound,
        ))
    }
}

impl Message {
    pub fn new(
        id: String,
        message_type: MessageType,
        source: String,
        target: Option<String>,
        content: serde_json::Value,
    ) -> Self {
        Self {
            id,
            message_type,
            source,
            target,
            payload: MessagePayload {
                content,
                schema_version: "1.0".to_string(),
                content_type: "application/json".to_string(),
            },
            metadata: HashMap::new(),
            timestamp: chrono::Utc::now(),
            priority: MessagePriority::Normal,
            status: MessageStatus::Created,
        }
    }

    pub fn with_metadata(mut self, key: String, value: String) -> Self {
        self.metadata.insert(key, value);
        self
    }

    pub fn with_priority(mut self, priority: MessagePriority) -> Self {
        self.priority = priority;
        self
    }

    pub fn with_content_type(mut self, content_type: String) -> Self {
        self.payload.content_type = content_type;
        self
    }
}

impl MessagePayload {
    pub fn new(content: serde_json::Value, schema_version: String) -> Self {
        Self {
            content,
            schema_version,
            content_type: "application/json".to_string(),
        }
    }
}

impl MessageError {
    pub fn new(message: String, error_type: MessageErrorType) -> Self {
        Self {
            message,
            error_type,
            message_id: None,
        }
    }

    pub fn with_message_id(mut self, message_id: String) -> Self {
        self.message_id = Some(message_id);
        self
    }
}

impl std::fmt::Display for MessageError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}: {}", self.error_type, self.message)
    }
}

impl std::error::Error for MessageError {}

impl MessageResponse {
    pub fn new(status: ResponseStatus, in_reply_to: String) -> Self {
        Self {
            status,
            payload: None,
            metadata: HashMap::new(),
            in_reply_to,
        }
    }

    pub fn with_payload(mut self, payload: MessagePayload) -> Self {
        self.payload = Some(payload);
        self
    }

    pub fn with_metadata(mut self, key: String, value: String) -> Self {
        self.metadata.insert(key, value);
        self
    }
}

/// Default message handler implementation
pub struct DefaultMessageHandler;

impl MessageHandler for DefaultMessageHandler {
    fn handle_message(&self, message: Message) -> std::pin::Pin<Box<dyn std::future::Future<Output = Result<MessageResponse, MessageError>> + Send>> {
        Box::pin(async move {
            match message.message_type {
                MessageType::TaskRequest => {
                    // Handle task request
                    Ok(MessageResponse::new(
                        ResponseStatus::Success,
                        message.id.clone(),
                    ))
                }
                MessageType::TaskResponse => {
                    // Handle task response
                    Ok(MessageResponse::new(
                        ResponseStatus::Success,
                        message.id.clone(),
                    ))
                }
                MessageType::AgentRegistration => {
                    // Handle agent registration
                    Ok(MessageResponse::new(
                        ResponseStatus::Ack,
                        message.id.clone(),
                    ))
                }
                MessageType::Broadcast => {
                    // Handle broadcast message
                    Ok(MessageResponse::new(
                        ResponseStatus::Ack,
                        message.id.clone(),
                    ))
                }
                _ => Err(MessageError::new(
                    "Unsupported message type".to_string(),
                    MessageErrorType::UnsupportedMessageType,
                )),
            }
        })
    }

    fn can_handle(&self, _message_type: &MessageType) -> bool {
        // Default handler can handle all message types
        true
    }

    fn capabilities(&self) -> MessageCapabilities {
        MessageCapabilities {
            supported_types: vec![
                MessageType::TaskRequest,
                MessageType::TaskResponse,
                MessageType::AgentRegistration,
                MessageType::Broadcast,
                MessageType::Direct,
            ],
            max_message_size: 1024 * 1024, // 1MB
            requires_ack: false,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_message_creation() {
        let message = Message::new(
            "msg-123".to_string(),
            MessageType::TaskRequest,
            "agent-1".to_string(),
            Some("agent-2".to_string()),
            serde_json::json!({"task": "test"}),
        );

        assert_eq!(message.id, "msg-123");
        assert_eq!(message.source, "agent-1");
        assert_eq!(message.target, Some("agent-2".to_string()));
        assert_eq!(message.message_type, MessageType::TaskRequest);
    }

    #[test]
    fn test_message_priority() {
        let message = Message::new(
            "msg-123".to_string(),
            MessageType::TaskRequest,
            "agent-1".to_string(),
            None,
            serde_json::json!({}),
        )
        .with_priority(MessagePriority::High);

        assert_eq!(message.priority, MessagePriority::High);
    }

    #[tokio::test]
    async fn test_message_handler() {
        let handler = DefaultMessageHandler;

        let message = Message::new(
            "msg-123".to_string(),
            MessageType::TaskRequest,
            "agent-1".to_string(),
            None,
            serde_json::json!({"task": "test"}),
        );

        let response = handler.handle_message(message).await.unwrap();
        assert_eq!(response.status, ResponseStatus::Success);
        assert_eq!(response.in_reply_to, "msg-123");
    }

    #[test]
    fn test_message_broker() {
        let mut broker = MessageBroker::new();
        broker.register_handler("default".to_string(), Box::new(DefaultMessageHandler));

        assert_eq!(broker.handlers.len(), 1);
    }
}