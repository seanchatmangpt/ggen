//! Message queue for replay capability during recovery
//!
//! Queues unconfirmed messages and replays them on reconnection
//! to ensure exactly-once semantics in distributed systems.

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::VecDeque;
use std::sync::{Arc, Mutex};
use tracing::{debug, info, warn};
use uuid::Uuid;

/// Status of a queued message
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "UPPERCASE")]
pub enum MessageStatus {
    /// Queued for sending
    Pending,
    /// Sent but not confirmed
    Sent,
    /// Successfully delivered
    Delivered,
    /// Failed permanently
    Failed,
}

impl std::fmt::Display for MessageStatus {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::Pending => write!(f, "PENDING"),
            Self::Sent => write!(f, "SENT"),
            Self::Delivered => write!(f, "DELIVERED"),
            Self::Failed => write!(f, "FAILED"),
        }
    }
}

/// A single message in the queue
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QueuedMessage {
    /// Unique message ID
    pub id: String,
    /// Target destination
    pub destination: String,
    /// Message payload
    pub payload: Value,
    /// Current status
    pub status: MessageStatus,
    /// When message was queued
    pub queued_at: DateTime<Utc>,
    /// When message was last sent
    pub sent_at: Option<DateTime<Utc>>,
    /// Number of send attempts
    pub retry_count: usize,
}

impl QueuedMessage {
    /// Create a new queued message
    pub fn new(destination: impl Into<String>, payload: Value) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            destination: destination.into(),
            payload,
            status: MessageStatus::Pending,
            queued_at: Utc::now(),
            sent_at: None,
            retry_count: 0,
        }
    }

    /// Mark message as sent
    pub fn mark_sent(&mut self) {
        self.status = MessageStatus::Sent;
        self.sent_at = Some(Utc::now());
        self.retry_count += 1;
    }

    /// Mark message as delivered
    pub fn mark_delivered(&mut self) {
        self.status = MessageStatus::Delivered;
    }

    /// Mark message as failed
    pub fn mark_failed(&mut self) {
        self.status = MessageStatus::Failed;
    }
}

/// A wrapper for a message to send
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Message {
    /// Message ID
    pub id: String,
    /// Destination
    pub destination: String,
    /// Payload
    pub payload: Value,
}

impl Message {
    /// Create a new message
    pub fn new(destination: impl Into<String>, payload: Value) -> Self {
        Self {
            id: Uuid::new_v4().to_string(),
            destination: destination.into(),
            payload,
        }
    }
}

/// Queue for managing messages with replay capability
#[derive(Debug, Clone)]
pub struct MessageQueue {
    queue: Arc<Mutex<VecDeque<QueuedMessage>>>,
    tracking: Arc<Mutex<std::collections::HashMap<String, QueuedMessage>>>,
    name: String,
    max_retries: usize,
}

impl MessageQueue {
    /// Create a new message queue
    pub fn new(name: impl Into<String>) -> Self {
        Self {
            queue: Arc::new(Mutex::new(VecDeque::new())),
            tracking: Arc::new(Mutex::new(std::collections::HashMap::new())),
            name: name.into(),
            max_retries: 5,
        }
    }

    /// Create a new message queue with custom retry limit
    pub fn with_retries(name: impl Into<String>, max_retries: usize) -> Self {
        Self {
            queue: Arc::new(Mutex::new(VecDeque::new())),
            tracking: Arc::new(Mutex::new(std::collections::HashMap::new())),
            name: name.into(),
            max_retries,
        }
    }

    /// Enqueue a message
    pub fn enqueue(&self, destination: impl Into<String>, payload: Value) -> String {
        let message = QueuedMessage::new(destination, payload);
        let msg_id = message.id.clone();

        let mut queue = self
            .queue
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        let mut tracking = self
            .tracking
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());

        queue.push_back(message.clone());
        tracking.insert(msg_id.clone(), message);

        debug!(
            "Message {} enqueued to queue '{}', queue size: {}",
            msg_id,
            self.name,
            queue.len()
        );

        msg_id
    }

    /// Get the next message to send
    pub fn dequeue(&self) -> Option<QueuedMessage> {
        let mut queue = self
            .queue
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());

        while let Some(mut message) = queue.pop_front() {
            // Skip failed messages beyond retry limit
            if message.status == MessageStatus::Failed && message.retry_count >= self.max_retries {
                warn!(
                    "Discarding failed message {} after {} retries",
                    message.id, message.retry_count
                );
                continue;
            }

            message.mark_sent();
            let mut tracking = self
                .tracking
                .lock()
                .unwrap_or_else(|poisoned| poisoned.into_inner());
            tracking.insert(message.id.clone(), message.clone());
            debug!("Message {} dequeued from '{}'", message.id, self.name);
            return Some(message);
        }

        None
    }

    /// Mark a message as successfully delivered
    pub fn mark_delivered(&self, message_id: &str) -> Result<(), String> {
        let mut tracking = self
            .tracking
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());

        if let Some(message) = tracking.get_mut(message_id) {
            message.mark_delivered();
            info!(
                "Message {} marked as delivered in queue '{}'",
                message_id, self.name
            );
            Ok(())
        } else {
            Err(format!("Message {} not found in tracking", message_id))
        }
    }

    /// Mark a message as failed and re-queue for retry
    pub fn mark_failed_and_requeue(&self, message_id: &str) -> Result<(), String> {
        let mut tracking = self
            .tracking
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());

        if let Some(message) = tracking.get_mut(message_id) {
            if message.retry_count >= self.max_retries {
                message.mark_failed();
                warn!(
                    "Message {} exceeded retry limit in queue '{}'",
                    message_id, self.name
                );
            } else {
                message.status = MessageStatus::Pending;
                let mut queue = self
                    .queue
                    .lock()
                    .unwrap_or_else(|poisoned| poisoned.into_inner());
                queue.push_back(message.clone());
                debug!(
                    "Message {} requeued (attempt {}) in queue '{}'",
                    message_id, message.retry_count, self.name
                );
            }
            Ok(())
        } else {
            Err(format!("Message {} not found in tracking", message_id))
        }
    }

    /// Get all pending messages for replay
    pub fn get_pending_messages(&self) -> Vec<QueuedMessage> {
        let queue = self
            .queue
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());

        queue
            .iter()
            .filter(|msg| msg.status == MessageStatus::Pending || msg.status == MessageStatus::Sent)
            .cloned()
            .collect()
    }

    /// Get queue size
    pub fn size(&self) -> usize {
        let queue = self
            .queue
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        queue.len()
    }

    /// Clear the queue
    pub fn clear(&self) {
        let mut queue = self
            .queue
            .lock()
            .unwrap_or_else(|poisoned| poisoned.into_inner());
        queue.clear();
        info!("Queue '{}' cleared", self.name);
    }

    /// Replay all pending messages for a destination
    pub fn replay_for_destination(&self, destination: &str) -> Vec<QueuedMessage> {
        let pending = self.get_pending_messages();

        pending
            .into_iter()
            .filter(|msg| msg.destination == destination)
            .collect()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_message_creation() {
        let msg = Message::new("dest", Value::String("test".to_string()));
        assert_eq!(msg.destination, "dest");
    }

    #[test]
    fn test_queued_message_creation() {
        let msg = QueuedMessage::new("dest", Value::String("test".to_string()));
        assert_eq!(msg.destination, "dest");
        assert_eq!(msg.status, MessageStatus::Pending);
        assert_eq!(msg.retry_count, 0);
    }

    #[test]
    fn test_message_queue_enqueue_dequeue() {
        let queue = MessageQueue::new("test");
        let payload = Value::String("test payload".to_string());

        queue.enqueue("dest", payload);
        assert_eq!(queue.size(), 1);

        let msg = queue.dequeue().unwrap();
        assert_eq!(msg.status, MessageStatus::Sent);
        assert_eq!(msg.retry_count, 1);
    }

    #[test]
    fn test_message_queue_delivery_tracking() {
        let queue = MessageQueue::new("test");
        let msg_id = queue.enqueue("dest", Value::String("payload".to_string()));

        let _msg = queue.dequeue().unwrap();

        queue.mark_delivered(&msg_id).unwrap();
        // Message should have Delivered status
        let pending = queue.get_pending_messages();
        assert!(!pending.iter().any(|m| m.id == msg_id));
    }

    #[test]
    fn test_message_queue_retry_logic() {
        let queue = MessageQueue::with_retries("test", 2);
        let msg_id = queue.enqueue("dest", Value::String("payload".to_string()));

        // First retry
        let mut msg = queue.dequeue().unwrap();
        assert_eq!(msg.retry_count, 1);

        queue.mark_failed_and_requeue(&msg_id).unwrap();

        // Second retry
        let mut msg = queue.dequeue().unwrap();
        assert_eq!(msg.retry_count, 2);

        queue.mark_failed_and_requeue(&msg_id).unwrap();

        // Should not dequeue after max retries
        assert!(queue.dequeue().is_none());
    }

    #[test]
    fn test_replay_for_destination() {
        let queue = MessageQueue::new("test");

        queue.enqueue("dest1", Value::String("msg1".to_string()));
        queue.enqueue("dest2", Value::String("msg2".to_string()));
        queue.enqueue("dest1", Value::String("msg3".to_string()));

        let dest1_msgs = queue.replay_for_destination("dest1");
        assert_eq!(dest1_msgs.len(), 2);
        assert!(dest1_msgs.iter().all(|m| m.destination == "dest1"));
    }
}
