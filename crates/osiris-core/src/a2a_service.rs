//! A2A (Agent-to-Agent) Communication Service

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::HashMap;
use std::sync::Arc;
use std::time::Instant;
use tokio::sync::RwLock;
use tracing::{debug, info, warn};

use crate::{OSIRISError, Result};

/// A2A message structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct A2AMessage {
    pub id: String,
    pub from: String,
    pub to: String,
    pub message_type: String,
    pub payload: Value,
    pub timestamp: Instant,
    pub correlation_id: Option<String>,
    pub reply_to: Option<String>,
    pub metadata: HashMap<String, Value>,
}

/// A2A service for agent communication
pub struct A2AService {
    message_queue: Arc<RwLock<Vec<A2AMessage>>>,
    subscriptions: Arc<RwLock<HashMap<String, Vec<String>>>>, // agent_id -> list of message_types
    message_handlers: Arc<RwLock<HashMap<String, Box<dyn MessageHandler + Send + Sync>>>>,
}

/// Message handler trait
#[async_trait::async_trait]
pub trait MessageHandler: Send + Sync {
    async fn handle_message(&self, message: A2AMessage) -> Result<Value>;
}

impl A2AService {
    /// Create a new A2A service
    pub fn new() -> Self {
        Self {
            message_queue: Arc::new(RwLock::new(Vec::new())),
            subscriptions: Arc::new(RwLock::new(HashMap::new())),
            message_handlers: Arc::new(RwLock::new(HashMap::new())),
        }
    }

    /// Prepare an A2A message
    pub async fn prepare_message(
        &self,
        to: &str,
        message_type: &str,
        payload: &Value,
    ) -> Result<A2AMessage> {
        let message = A2AMessage {
            id: uuid::Uuid::new_v4().to_string(),
            from: "osiris-engine".to_string(),
            to: to.to_string(),
            message_type: message_type.to_string(),
            payload: payload.clone(),
            timestamp: Instant::now(),
            correlation_id: None,
            reply_to: None,
            metadata: HashMap::new(),
        };

        Ok(message)
    }

    /// Send a message to an agent
    pub async fn send_message(&self, message: A2AMessage) -> Result<()> {
        debug!("Sending A2A message: {} -> {} ({})", message.from, message.to, message.message_type);

        // Add to message queue
        let mut queue = self.message_queue.write().await;
        queue.push(message.clone());

        // Notify subscribers
        let subs = self.subscriptions.read().await;
        if let Some(subscribers) = subs.get(&message.message_type) {
            for agent_id in subscribers {
                info!("Notifying agent {} of new message", agent_id);
            }
        }

        Ok(())
    }

    /// Send a message and wait for reply
    pub async fn send_and_wait_reply(
        &self,
        message: A2AMessage,
        timeout_ms: u64,
    ) -> Result<A2AMessage> {
        let correlation_id = message.id.clone();
        let reply_to = message.from.clone();

        let mut message = message;
        message.correlation_id = Some(correlation_id.clone());
        message.reply_to = Some(reply_to);

        // Store for correlation
        let mut handlers = self.message_handlers.write().await;
        handlers.insert(correlation_id.clone(), Box::new(ReplyHandler::new()));

        // Send message
        self.send_message(message).await?;

        // Wait for reply with timeout
        let start = Instant::now();
        loop {
            let queue = self.message_queue.read().await;
            if let Some(reply) = queue.iter().find(|m| {
                m.correlation_id == Some(correlation_id.clone())
            }) {
                return Ok(reply.clone());
            }

            if start.elapsed().as_millis() >= timeout_ms as u64 {
                return Err(OSIRISError::Timeout("Reply timeout".to_string()));
            }

            tokio::time::sleep(tokio::time::Duration::from_millis(10)).await;
        }
    }

    /// Register a message handler
    pub async fn register_handler<H>(&self, message_type: &str, handler: H) -> Result<()>
    where
        H: MessageHandler + Send + Sync + 'static,
    {
        let mut handlers = self.message_handlers.write().await;
        handlers.insert(message_type.to_string(), Box::new(handler));

        info!("Registered message handler for type: {}", message_type);
        Ok(())
    }

    /// Subscribe to message types
    pub async fn subscribe(&self, agent_id: &str, message_types: &[&str]) -> Result<()> {
        let mut subs = self.subscriptions.write().await;

        for &message_type in message_types {
            subs.entry(message_type.to_string())
                .or_insert_with(Vec::new)
                .push(agent_id.to_string());
        }

        info!("Agent {} subscribed to messages: {}", agent_id, message_types.join(", "));
        Ok(())
    }

    /// Unsubscribe from message types
    pub async fn unsubscribe(&self, agent_id: &str, message_types: &[&str]) -> Result<()> {
        let mut subs = self.subscriptions.write().await;

        for &message_type in message_types {
            if let Some(subscribers) = subs.get_mut(message_type) {
                subscribers.retain(|id| id != agent_id);
            }
        }

        info!("Agent {} unsubscribed from messages: {}", agent_id, message_types.join(", "));
        Ok(())
    }

    /// Get messages for an agent
    pub async fn get_messages(&self, agent_id: &str) -> Result<Vec<A2AMessage>> {
        let mut queue = self.message_queue.write().await;

        let mut agent_messages: Vec<A2AMessage> = queue
            .iter()
            .filter(|m| m.to == agent_id)
            .cloned()
            .collect();

        // Remove messages for this agent from queue
        queue.retain(|m| m.to != agent_id);

        Ok(agent_messages)
    }

    /// Get message statistics
    pub async fn get_stats(&self) -> Value {
        let queue = self.message_queue.read().await;
        let subs = self.subscriptions.read().await;
        let handlers = self.message_handlers.read().await;

        let message_counts: HashMap<String, usize> = subs
            .iter()
            .map(|(msg_type, agents)| (msg_type.clone(), agents.len()))
            .collect();

        json!({
            "queue_size": queue.len(),
            "subscribed_message_types": subs.len(),
            "message_counts": message_counts,
            "registered_handlers": handlers.len(),
            "timestamp": chrono::Utc::now().to_rfc3339(),
        })
    }

    /// Process messages in the queue
    pub async fn process_messages(&self) -> Result<usize> {
        let mut queue = self.message_queue.write().await;
        let mut processed_count = 0;

        let messages: Vec<_> = queue.drain(..).collect();

        for message in messages {
            debug!("Processing message: {} -> {} ({})", message.from, message.to, message.message_type);

            // Find and call handler
            let handlers = self.message_handlers.read().await;
            if let Some(handler) = handlers.get(&message.message_type) {
                match handler.handle_message(message.clone()).await {
                    Ok(response) => {
                        info!("Message processed successfully: {}", message.id);
                        // Send response if needed
                        if let Some(reply_to) = message.reply_to {
                            let reply = self.prepare_message(&reply_to, "message_reply", &response).await?;
                            self.send_message(reply).await?;
                        }
                    }
                    Err(e) => {
                        warn!("Failed to process message {}: {}", message.id, e);
                    }
                }
            } else {
                warn!("No handler found for message type: {}", message.message_type);
            }

            processed_count += 1;
        }

        Ok(processed_count)
    }

    /// Clear old messages
    pub async fn clear_old_messages(&self, max_age_seconds: u64) -> Result<usize> {
        let cutoff = Instant::now() - std::time::Duration::from_secs(max_age_seconds);
        let mut queue = self.message_queue.write().await;

        let old_count = queue.len();
        queue.retain(|m| m.timestamp > cutoff);

        let removed_count = old_count - queue.len();
        info!("Cleared {} old messages", removed_count);

        Ok(removed_count)
    }
}

/// Handler for reply messages
pub struct ReplyHandler {
    reply: Arc<RwLock<Option<A2AMessage>>>,
}

impl ReplyHandler {
    pub fn new() -> Self {
        Self {
            reply: Arc::new(RwLock::new(None)),
        }
    }
}

#[async_trait::async_trait]
impl MessageHandler for ReplyHandler {
    async fn handle_message(&self, message: A2AMessage) -> Result<Value> {
        let mut reply = self.reply.write().await;
        *reply = Some(message);
        Ok(json!({"status": "received"}))
    }
}

/// Default implementation
impl Default for A2AService {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_prepare_message() {
        let service = A2AService::new();

        let payload = json!({"test": "data"});
        let message = service.prepare_message("agent1", "test", &payload).await;
        assert!(message.is_ok());

        let message = message.unwrap();
        assert_eq!(message.to, "agent1");
        assert_eq!(message.message_type, "test");
        assert_eq!(message.payload, payload);
    }

    #[tokio::test]
    async fn test_subscription() {
        let service = A2AService::new();

        let result = service.subscribe("agent1", &["test1", "test2"]).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_message_processing() {
        let service = A2AService::new();

        // Register a simple handler
        service.register_handler("test", SimpleHandler).await.unwrap();

        let payload = json!({"test": "data"});
        let message = service.prepare_message("agent1", "test", &payload).await.unwrap();

        let send_result = service.send_message(message).await;
        assert!(send_result.is_ok());

        // Process messages
        let processed = service.process_messages().await;
        assert!(processed.is_ok());
        assert_eq!(processed.unwrap(), 1);
    }

    // Simple test handler
    struct SimpleHandler;

    #[async_trait::async_trait]
    impl MessageHandler for SimpleHandler {
        async fn handle_message(&self, message: A2AMessage) -> Result<Value> {
            Ok(json!({
                "status": "processed",
                "message_id": message.id
            }))
        }
    }
}