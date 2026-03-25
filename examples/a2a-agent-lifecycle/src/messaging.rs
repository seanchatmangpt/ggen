//! Message routing with FIFO guarantees and priority handling

use anyhow::Result;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::{HashMap, VecDeque};
use uuid::Uuid;

#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum MessageType {
    TaskRequest,
    TaskResult,
    StateTransition,
    Error,
    Ping,
    PingResponse,
}

impl MessageType {
    pub fn code(&self) -> &'static str {
        match self {
            MessageType::TaskRequest => "TASK_REQUEST",
            MessageType::TaskResult => "TASK_RESULT",
            MessageType::StateTransition => "STATE_TRANSITION",
            MessageType::Error => "ERROR",
            MessageType::Ping => "PING",
            MessageType::PingResponse => "PING_RESPONSE",
        }
    }

    pub fn requires_ack(&self) -> bool {
        !matches!(
            self,
            MessageType::PingResponse | MessageType::StateTransition
        )
    }
}

#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Message {
    pub id: String,
    pub message_type: MessageType,
    pub source: String,
    pub destination: Option<String>, // None = broadcast
    pub payload: serde_json::Value,
    pub created_at: DateTime<Utc>,
    pub delivered_at: Option<DateTime<Utc>>,
    pub requires_ack: bool,
    pub ack_received: bool,
}

impl Message {
    pub fn new(
        message_type: MessageType, source: impl Into<String>, destination: Option<String>,
        payload: serde_json::Value,
    ) -> Self {
        let id = Uuid::new_v4().to_string();
        let requires_ack = message_type.requires_ack();

        Message {
            id,
            message_type,
            source: source.into(),
            destination,
            payload,
            created_at: Utc::now(),
            delivered_at: None,
            requires_ack,
            ack_received: false,
        }
    }

    /// Mark message as delivered
    pub fn mark_delivered(&mut self) {
        self.delivered_at = Some(Utc::now());
    }

    /// Mark acknowledgement received
    pub fn mark_acked(&mut self) {
        self.ack_received = true;
    }

    /// Check if message delivery is pending acknowledgement
    pub fn awaiting_ack(&self) -> bool {
        self.requires_ack && !self.ack_received
    }

    /// Get delivery latency in milliseconds
    pub fn delivery_latency_ms(&self) -> Option<i64> {
        self.delivered_at
            .map(|delivered| (delivered - self.created_at).num_milliseconds())
    }
}

/// FIFO message queue for an agent
pub struct AgentQueue {
    agent_id: String,
    messages: VecDeque<Message>,
    pending_acks: HashMap<String, Message>, // Message ID -> Message awaiting ack
}

impl AgentQueue {
    pub fn new(agent_id: impl Into<String>) -> Self {
        AgentQueue {
            agent_id: agent_id.into(),
            messages: VecDeque::new(),
            pending_acks: HashMap::new(),
        }
    }

    /// Enqueue a message (FIFO order)
    pub fn enqueue(&mut self, message: Message) {
        if message.requires_ack {
            self.pending_acks
                .insert(message.id.clone(), message.clone());
        }
        self.messages.push_back(message);
    }

    /// Dequeue next message
    pub fn dequeue(&mut self) -> Option<Message> {
        self.messages.pop_front()
    }

    /// Peek at next message without removing
    pub fn peek(&self) -> Option<&Message> {
        self.messages.front()
    }

    /// Acknowledge a message
    pub fn ack_message(&mut self, message_id: &str) -> Result<()> {
        if let Some(mut msg) = self.pending_acks.remove(message_id) {
            msg.mark_acked();
            Ok(())
        } else {
            Err(anyhow::anyhow!(
                "Message not found or already acked: {}",
                message_id
            ))
        }
    }

    /// Get queue length
    pub fn len(&self) -> usize {
        self.messages.len()
    }

    /// Check if queue is empty
    pub fn is_empty(&self) -> bool {
        self.messages.is_empty()
    }

    /// Get pending acks count
    pub fn pending_acks_count(&self) -> usize {
        self.pending_acks.len()
    }
}

/// Routes messages between agents with FIFO guarantees
pub struct MessageRouter {
    agent_queues: HashMap<String, AgentQueue>,
    broadcast_queue: VecDeque<Message>,
    message_log: Vec<Message>,
}

impl MessageRouter {
    pub fn new() -> Self {
        MessageRouter {
            agent_queues: HashMap::new(),
            broadcast_queue: VecDeque::new(),
            message_log: Vec::new(),
        }
    }

    /// Register an agent with the router
    pub fn register_agent(&mut self, agent_id: impl Into<String>) {
        let agent_id = agent_id.into();
        if !self.agent_queues.contains_key(&agent_id) {
            self.agent_queues
                .insert(agent_id.clone(), AgentQueue::new(agent_id));
        }
    }

    /// Send a message
    pub fn send(&mut self, mut message: Message) -> Result<()> {
        message.mark_delivered();
        self.message_log.push(message.clone());

        match &message.destination {
            Some(dest) => {
                if !self.agent_queues.contains_key(dest) {
                    return Err(anyhow::anyhow!("Agent not registered: {}", dest));
                }
                self.agent_queues.get_mut(dest).unwrap().enqueue(message);
            }
            None => {
                // Broadcast to all agents except sender
                for (agent_id, queue) in self.agent_queues.iter_mut() {
                    if agent_id != &message.source {
                        queue.enqueue(message.clone());
                    }
                }
                self.broadcast_queue.push_back(message);
            }
        }

        Ok(())
    }

    /// Receive next message for an agent
    pub fn receive(&mut self, agent_id: &str) -> Result<Option<Message>> {
        if let Some(queue) = self.agent_queues.get_mut(agent_id) {
            Ok(queue.dequeue())
        } else {
            Err(anyhow::anyhow!("Agent not registered: {}", agent_id))
        }
    }

    /// Peek at next message without removing
    pub fn peek(&self, agent_id: &str) -> Result<Option<&Message>> {
        if let Some(queue) = self.agent_queues.get(agent_id) {
            Ok(queue.peek())
        } else {
            Err(anyhow::anyhow!("Agent not registered: {}", agent_id))
        }
    }

    /// Acknowledge a message
    pub fn ack(&mut self, agent_id: &str, message_id: &str) -> Result<()> {
        if let Some(queue) = self.agent_queues.get_mut(agent_id) {
            queue.ack_message(message_id)
        } else {
            Err(anyhow::anyhow!("Agent not registered: {}", agent_id))
        }
    }

    /// Get queue length for an agent
    pub fn queue_len(&self, agent_id: &str) -> Result<usize> {
        self.agent_queues
            .get(agent_id)
            .map(|q| q.len())
            .ok_or_else(|| anyhow::anyhow!("Agent not registered: {}", agent_id))
    }

    /// Get total messages routed
    pub fn message_log(&self) -> &[Message] {
        &self.message_log
    }

    /// Get message statistics
    pub fn stats(&self) -> RouterStats {
        let total_messages = self.message_log.len();
        let avg_latency = if total_messages > 0 {
            let sum: i64 = self
                .message_log
                .iter()
                .filter_map(|m| m.delivery_latency_ms())
                .sum();
            sum / total_messages as i64
        } else {
            0
        };

        RouterStats {
            total_messages,
            avg_latency_ms: avg_latency,
            pending_messages: self.agent_queues.values().map(|q| q.len()).sum(),
            pending_acks: self
                .agent_queues
                .values()
                .map(|q| q.pending_acks_count())
                .sum(),
        }
    }
}

impl Default for MessageRouter {
    fn default() -> Self {
        Self::new()
    }
}

#[derive(Debug, Clone)]
pub struct RouterStats {
    pub total_messages: usize,
    pub avg_latency_ms: i64,
    pub pending_messages: usize,
    pub pending_acks: usize,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_message_creation() {
        let msg = Message::new(
            MessageType::TaskRequest,
            "agent-1",
            Some("agent-2".to_string()),
            serde_json::json!({"task": "test"}),
        );

        assert_eq!(msg.message_type, MessageType::TaskRequest);
        assert_eq!(msg.source, "agent-1");
        assert_eq!(msg.destination, Some("agent-2".to_string()));
        assert!(msg.requires_ack);
    }

    #[test]
    fn test_message_delivery() {
        let mut msg = Message::new(
            MessageType::TaskRequest,
            "agent-1",
            Some("agent-2".to_string()),
            serde_json::json!({}),
        );

        assert!(msg.delivered_at.is_none());
        msg.mark_delivered();
        assert!(msg.delivered_at.is_some());
    }

    #[test]
    fn test_agent_queue_fifo() {
        let mut queue = AgentQueue::new("agent-1");

        let msg1 = Message::new(
            MessageType::TaskRequest,
            "agent-1",
            None,
            serde_json::json!({}),
        );
        let msg2 = Message::new(
            MessageType::TaskResult,
            "agent-1",
            None,
            serde_json::json!({}),
        );

        queue.enqueue(msg1.clone());
        queue.enqueue(msg2.clone());

        assert_eq!(queue.dequeue().unwrap().id, msg1.id);
        assert_eq!(queue.dequeue().unwrap().id, msg2.id);
        assert!(queue.dequeue().is_none());
    }

    #[test]
    fn test_message_router_direct() {
        let mut router = MessageRouter::new();
        router.register_agent("agent-1");
        router.register_agent("agent-2");

        let msg = Message::new(
            MessageType::TaskRequest,
            "agent-1",
            Some("agent-2".to_string()),
            serde_json::json!({}),
        );

        assert!(router.send(msg).is_ok());
        assert_eq!(router.queue_len("agent-2").unwrap(), 1);
        assert_eq!(router.queue_len("agent-1").unwrap(), 0);
    }

    #[test]
    fn test_message_router_broadcast() {
        let mut router = MessageRouter::new();
        router.register_agent("agent-1");
        router.register_agent("agent-2");
        router.register_agent("agent-3");

        let msg = Message::new(
            MessageType::StateTransition,
            "agent-1",
            None, // Broadcast
            serde_json::json!({}),
        );

        assert!(router.send(msg).is_ok());
        // All agents except sender should receive
        assert_eq!(router.queue_len("agent-2").unwrap(), 1);
        assert_eq!(router.queue_len("agent-3").unwrap(), 1);
        assert_eq!(router.queue_len("agent-1").unwrap(), 0);
    }

    #[test]
    fn test_message_ordering() {
        let mut router = MessageRouter::new();
        router.register_agent("agent-1");
        router.register_agent("agent-2");

        for i in 0..5 {
            let msg = Message::new(
                MessageType::TaskRequest,
                "agent-1",
                Some("agent-2".to_string()),
                serde_json::json!({"seq": i}),
            );
            router.send(msg).unwrap();
        }

        for i in 0..5 {
            let msg = router.receive("agent-2").unwrap().unwrap();
            let seq = msg.payload.get("seq").unwrap().as_i64().unwrap();
            assert_eq!(seq, i as i64);
        }
    }

    #[test]
    fn test_message_ack() {
        let mut queue = AgentQueue::new("agent-1");

        let msg = Message::new(
            MessageType::TaskRequest,
            "agent-1",
            None,
            serde_json::json!({}),
        );
        let msg_id = msg.id.clone();

        queue.enqueue(msg);
        assert_eq!(queue.pending_acks_count(), 1);

        queue.ack_message(&msg_id).unwrap();
        assert_eq!(queue.pending_acks_count(), 0);
    }

    #[test]
    fn test_invalid_ack() {
        let mut queue = AgentQueue::new("agent-1");
        assert!(queue.ack_message("nonexistent").is_err());
    }

    #[test]
    fn test_router_stats() {
        let mut router = MessageRouter::new();
        router.register_agent("agent-1");
        router.register_agent("agent-2");

        for _ in 0..3 {
            let msg = Message::new(
                MessageType::TaskRequest,
                "agent-1",
                Some("agent-2".to_string()),
                serde_json::json!({}),
            );
            router.send(msg).unwrap();
        }

        let stats = router.stats();
        assert_eq!(stats.total_messages, 3);
        assert_eq!(stats.pending_messages, 3);
    }
}
