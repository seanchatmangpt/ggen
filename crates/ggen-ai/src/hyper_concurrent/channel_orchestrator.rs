//! Channel Orchestrator for inter-agent communication
//!
//! Provides fast, lock-free channels for agents to communicate
//! with each other during parallel execution.

use dashmap::DashMap;
use flume::{Receiver, Sender, TryRecvError, TrySendError};
use parking_lot::RwLock;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use std::sync::atomic::{AtomicU64, Ordering};
use tracing::{debug, warn};

/// Message type for inter-agent communication
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct AgentMessage {
    /// Source agent ID
    pub source: String,
    /// Target agent ID (empty for broadcast)
    pub target: String,
    /// Message type/topic
    pub message_type: String,
    /// Message payload
    pub payload: serde_json::Value,
    /// Message timestamp
    pub timestamp: String,
    /// Message ID
    pub id: String,
    /// Correlation ID for request/response patterns
    pub correlation_id: Option<String>,
}

impl AgentMessage {
    /// Create a new message
    pub fn new(source: &str, target: &str, message_type: &str, payload: serde_json::Value) -> Self {
        Self {
            source: source.to_string(),
            target: target.to_string(),
            message_type: message_type.to_string(),
            payload,
            timestamp: chrono::Utc::now().to_rfc3339(),
            id: uuid::Uuid::new_v4().to_string(),
            correlation_id: None,
        }
    }

    /// Create a broadcast message
    pub fn broadcast(source: &str, message_type: &str, payload: serde_json::Value) -> Self {
        Self::new(source, "", message_type, payload)
    }

    /// Create a response message
    pub fn response(original: &AgentMessage, payload: serde_json::Value) -> Self {
        let mut msg = Self::new(&original.target, &original.source, "response", payload);
        msg.correlation_id = Some(original.id.clone());
        msg
    }

    /// Check if this is a broadcast message
    pub fn is_broadcast(&self) -> bool {
        self.target.is_empty()
    }
}

/// Channel for a single agent
#[derive(Debug)]
struct AgentChannel {
    /// Channel sender
    sender: Sender<AgentMessage>,
    /// Channel receiver
    receiver: Receiver<AgentMessage>,
    /// Messages sent count
    sent_count: AtomicU64,
    /// Messages received count
    received_count: AtomicU64,
}

impl AgentChannel {
    fn new(capacity: usize) -> Self {
        let (sender, receiver) = flume::bounded(capacity);
        Self {
            sender,
            receiver,
            sent_count: AtomicU64::new(0),
            received_count: AtomicU64::new(0),
        }
    }
}

/// Channel orchestrator for managing inter-agent communication
#[derive(Debug)]
pub struct ChannelOrchestrator {
    /// Per-agent channels
    channels: DashMap<String, AgentChannel>,
    /// Broadcast senders (for all registered agents)
    broadcast_senders: RwLock<Vec<(String, Sender<AgentMessage>)>>,
    /// Default channel capacity
    channel_capacity: usize,
    /// Statistics
    stats: OrchestratorStats,
}

/// Orchestrator statistics
#[derive(Debug, Default)]
struct OrchestratorStats {
    total_messages: AtomicU64,
    broadcast_messages: AtomicU64,
    direct_messages: AtomicU64,
    failed_sends: AtomicU64,
}

impl ChannelOrchestrator {
    /// Create a new channel orchestrator
    pub fn new() -> Self {
        Self {
            channels: DashMap::new(),
            broadcast_senders: RwLock::new(Vec::new()),
            channel_capacity: 100,
            stats: OrchestratorStats::default(),
        }
    }

    /// Create with custom channel capacity
    pub fn with_capacity(capacity: usize) -> Self {
        Self {
            channels: DashMap::new(),
            broadcast_senders: RwLock::new(Vec::new()),
            channel_capacity: capacity,
            stats: OrchestratorStats::default(),
        }
    }

    /// Create a channel for an agent
    pub fn create_channel(&self, agent_id: &str) {
        if self.channels.contains_key(agent_id) {
            return;
        }

        let channel = AgentChannel::new(self.channel_capacity);
        let sender = channel.sender.clone();

        self.channels.insert(agent_id.to_string(), channel);

        // Add to broadcast list
        let mut broadcasts = self.broadcast_senders.write();
        broadcasts.push((agent_id.to_string(), sender));

        debug!("Created channel for agent: {}", agent_id);
    }

    /// Remove a channel
    pub fn remove_channel(&self, agent_id: &str) {
        self.channels.remove(agent_id);

        let mut broadcasts = self.broadcast_senders.write();
        broadcasts.retain(|(id, _)| id != agent_id);

        debug!("Removed channel for agent: {}", agent_id);
    }

    /// Send a message to a specific agent
    pub fn send(&self, message: AgentMessage) -> Result<(), ChannelError> {
        self.stats.total_messages.fetch_add(1, Ordering::Relaxed);

        if message.is_broadcast() {
            self.broadcast(message)
        } else {
            self.send_direct(message)
        }
    }

    /// Send a direct message
    fn send_direct(&self, message: AgentMessage) -> Result<(), ChannelError> {
        let target = &message.target;

        let channel = self.channels.get(target).ok_or_else(|| {
            ChannelError::AgentNotFound(target.clone())
        })?;

        match channel.sender.try_send(message) {
            Ok(()) => {
                channel.sent_count.fetch_add(1, Ordering::Relaxed);
                self.stats.direct_messages.fetch_add(1, Ordering::Relaxed);
                Ok(())
            }
            Err(TrySendError::Full(msg)) => {
                self.stats.failed_sends.fetch_add(1, Ordering::Relaxed);
                Err(ChannelError::ChannelFull(msg.target))
            }
            Err(TrySendError::Disconnected(_)) => {
                self.stats.failed_sends.fetch_add(1, Ordering::Relaxed);
                Err(ChannelError::Disconnected(target.clone()))
            }
        }
    }

    /// Broadcast a message to all agents
    fn broadcast(&self, message: AgentMessage) -> Result<(), ChannelError> {
        let broadcasts = self.broadcast_senders.read();
        let mut failures = 0;

        for (agent_id, sender) in broadcasts.iter() {
            if agent_id == &message.source {
                continue; // Don't send to self
            }

            let mut msg = message.clone();
            msg.target = agent_id.clone();

            if sender.try_send(msg).is_err() {
                failures += 1;
            }
        }

        self.stats.broadcast_messages.fetch_add(1, Ordering::Relaxed);

        if failures > 0 {
            warn!("Broadcast had {} failures", failures);
        }

        Ok(())
    }

    /// Try to receive a message for an agent (non-blocking)
    pub fn try_receive(&self, agent_id: &str) -> Option<AgentMessage> {
        let channel = self.channels.get(agent_id)?;

        match channel.receiver.try_recv() {
            Ok(msg) => {
                channel.received_count.fetch_add(1, Ordering::Relaxed);
                Some(msg)
            }
            Err(TryRecvError::Empty) => None,
            Err(TryRecvError::Disconnected) => None,
        }
    }

    /// Receive a message for an agent (blocking)
    pub async fn receive(&self, agent_id: &str) -> Option<AgentMessage> {
        let channel = self.channels.get(agent_id)?;

        match channel.receiver.recv_async().await {
            Ok(msg) => {
                channel.received_count.fetch_add(1, Ordering::Relaxed);
                Some(msg)
            }
            Err(_) => None,
        }
    }

    /// Receive with timeout
    pub async fn receive_timeout(
        &self,
        agent_id: &str,
        timeout: std::time::Duration,
    ) -> Option<AgentMessage> {
        tokio::time::timeout(timeout, self.receive(agent_id))
            .await
            .ok()
            .flatten()
    }

    /// Get all pending messages for an agent
    pub fn drain(&self, agent_id: &str) -> Vec<AgentMessage> {
        let channel = match self.channels.get(agent_id) {
            Some(c) => c,
            None => return Vec::new(),
        };

        let mut messages = Vec::new();
        while let Ok(msg) = channel.receiver.try_recv() {
            channel.received_count.fetch_add(1, Ordering::Relaxed);
            messages.push(msg);
        }
        messages
    }

    /// Get number of registered channels
    pub fn channel_count(&self) -> usize {
        self.channels.len()
    }

    /// Get statistics
    pub fn statistics(&self) -> OrchestratorStatistics {
        OrchestratorStatistics {
            channel_count: self.channels.len(),
            total_messages: self.stats.total_messages.load(Ordering::Relaxed),
            broadcast_messages: self.stats.broadcast_messages.load(Ordering::Relaxed),
            direct_messages: self.stats.direct_messages.load(Ordering::Relaxed),
            failed_sends: self.stats.failed_sends.load(Ordering::Relaxed),
        }
    }
}

impl Default for ChannelOrchestrator {
    fn default() -> Self {
        Self::new()
    }
}

/// Channel error types
#[derive(Debug, Clone)]
pub enum ChannelError {
    /// Target agent not found
    AgentNotFound(String),
    /// Channel is full
    ChannelFull(String),
    /// Channel is disconnected
    Disconnected(String),
}

impl std::fmt::Display for ChannelError {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Self::AgentNotFound(id) => write!(f, "Agent not found: {}", id),
            Self::ChannelFull(id) => write!(f, "Channel full for: {}", id),
            Self::Disconnected(id) => write!(f, "Channel disconnected for: {}", id),
        }
    }
}

impl std::error::Error for ChannelError {}

/// Orchestrator statistics snapshot
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct OrchestratorStatistics {
    /// Number of active channels
    pub channel_count: usize,
    /// Total messages sent
    pub total_messages: u64,
    /// Broadcast messages sent
    pub broadcast_messages: u64,
    /// Direct messages sent
    pub direct_messages: u64,
    /// Failed sends
    pub failed_sends: u64,
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_orchestrator_creation() {
        let orchestrator = ChannelOrchestrator::new();
        assert_eq!(orchestrator.channel_count(), 0);
    }

    #[test]
    fn test_channel_creation() {
        let orchestrator = ChannelOrchestrator::new();
        orchestrator.create_channel("agent-1");
        orchestrator.create_channel("agent-2");
        assert_eq!(orchestrator.channel_count(), 2);
    }

    #[test]
    fn test_direct_message() {
        let orchestrator = ChannelOrchestrator::new();
        orchestrator.create_channel("agent-1");
        orchestrator.create_channel("agent-2");

        let msg = AgentMessage::new("agent-1", "agent-2", "test", serde_json::json!({"data": 123}));
        orchestrator.send(msg).unwrap();

        let received = orchestrator.try_receive("agent-2").unwrap();
        assert_eq!(received.source, "agent-1");
        assert_eq!(received.message_type, "test");
    }

    #[test]
    fn test_broadcast_message() {
        let orchestrator = ChannelOrchestrator::new();
        orchestrator.create_channel("agent-1");
        orchestrator.create_channel("agent-2");
        orchestrator.create_channel("agent-3");

        let msg = AgentMessage::broadcast("agent-1", "broadcast", serde_json::json!({}));
        orchestrator.send(msg).unwrap();

        // agent-2 and agent-3 should receive, but not agent-1 (sender)
        assert!(orchestrator.try_receive("agent-2").is_some());
        assert!(orchestrator.try_receive("agent-3").is_some());
        assert!(orchestrator.try_receive("agent-1").is_none());
    }

    #[test]
    fn test_statistics() {
        let orchestrator = ChannelOrchestrator::new();
        orchestrator.create_channel("agent-1");
        orchestrator.create_channel("agent-2");

        let msg = AgentMessage::new("agent-1", "agent-2", "test", serde_json::json!({}));
        orchestrator.send(msg).unwrap();

        let stats = orchestrator.statistics();
        assert_eq!(stats.channel_count, 2);
        assert_eq!(stats.total_messages, 1);
        assert_eq!(stats.direct_messages, 1);
    }
}
