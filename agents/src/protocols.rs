//! # Agent Communication Protocols
//!
//! Defines the message passing protocols used for inter-agent communication.
//! Follows the 80/20 principle by focusing on the most common communication patterns.

use serde::{Deserialize, Serialize};
use uuid::Uuid;

/// Types of messages that agents can exchange
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum MessageType {
    /// Task execution request
    TaskRequest,
    /// Task execution result
    TaskResult,
    /// State synchronization
    StateSync,
    /// Capability discovery
    CapabilityQuery,
    /// Error notification
    Error,
    /// Coordination message
    Coordination,
}

/// A message passed between agents
#[derive(Debug, Clone, serde::Serialize, serde::Deserialize)]
pub struct Message {
    /// Unique identifier for this message (as string)
    pub message_id: String,

    /// Type of message
    pub message_type: MessageType,

    /// ID of the sending agent (as string)
    pub sender_id: String,

    /// ID of the target agent (None for broadcast, as string)
    pub target_id: Option<String>,

    /// Message payload
    pub payload: serde_json::Value,

    /// Message metadata
    pub metadata: HashMap<String, String>,

    /// Timestamp when message was created (as string)
    pub timestamp: String,
}

impl Message {
    /// Create a new message
    pub fn new(
        message_type: MessageType, sender_id: String, target_id: Option<String>,
        payload: serde_json::Value,
    ) -> Self {
        Self {
            message_id: Uuid::new_v4().to_string(),
            message_type,
            sender_id,
            target_id,
            payload,
            metadata: HashMap::new(),
            timestamp: chrono::Utc::now().to_rfc3339(),
        }
    }

    /// Create a task request message
    pub fn task_request(sender_id: String, task_data: serde_json::Value) -> Self {
        Self::new(MessageType::TaskRequest, sender_id, None, task_data)
    }

    /// Create a task result message
    pub fn task_result(sender_id: String, target_id: String, result: serde_json::Value) -> Self {
        Self::new(MessageType::TaskResult, sender_id, Some(target_id), result)
    }

    /// Create a state synchronization message
    pub fn state_sync(sender_id: String, state_data: serde_json::Value) -> Self {
        Self::new(MessageType::StateSync, sender_id, None, state_data)
    }

    /// Create an error message
    pub fn error(sender_id: String, error_data: serde_json::Value) -> Self {
        Self::new(MessageType::Error, sender_id, None, error_data)
    }
}

/// Protocol for agent communication
pub trait Protocol: Send + Sync {
    /// Send a message to an agent
    async fn send_message(&self, message: Message) -> crate::core::AgentResult<()>;

    /// Receive messages for an agent
    async fn receive_messages(&self, agent_id: Uuid) -> crate::core::AgentResult<Vec<Message>>;

    /// Broadcast a message to all agents
    async fn broadcast_message(&self, message: Message) -> crate::core::AgentResult<()>;
}

use std::collections::HashMap;
