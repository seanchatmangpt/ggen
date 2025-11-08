// Notification & Messaging Hub - Rust Implementation
// Enterprise-scale multi-channel notification system

use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use chrono::{DateTime, Utc, Duration};
use async_trait::async_trait;

/// Message priority levels
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq, PartialOrd, Ord)]
pub enum MessagePriority {
    Low,
    Medium,
    High,
    Critical,
}

/// Delivery status enumeration
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum DeliveryStatus {
    Pending,
    Queued,
    Sending,
    Delivered,
    Failed,
    Bounced,
    Expired,
}

/// Message channel types
#[derive(Debug, Clone, Serialize, Deserialize, Hash, PartialEq, Eq)]
pub enum Channel {
    Email,
    SMS,
    Push,
    Webhook,
    InApp,
    Slack,
    Teams,
}

/// Core message structure
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Message {
    pub id: String,
    pub subject: Option<String>,
    pub body: String,
    pub html_body: Option<String>,
    pub priority: MessagePriority,
    pub channel: Channel,
    pub recipient_ids: Vec<String>,
    pub template_id: Option<String>,
    pub template_vars: HashMap<String, String>,
    pub created_at: DateTime<Utc>,
    pub scheduled_at: Option<DateTime<Utc>>,
    pub expires_at: Option<DateTime<Utc>>,
    pub metadata: HashMap<String, String>,
}

impl Message {
    pub fn new(
        id: String,
        body: String,
        channel: Channel,
        recipient_ids: Vec<String>,
    ) -> Self {
        Self {
            id,
            subject: None,
            body,
            html_body: None,
            priority: MessagePriority::Medium,
            channel,
            recipient_ids,
            template_id: None,
            template_vars: HashMap::new(),
            created_at: Utc::now(),
            scheduled_at: None,
            expires_at: None,
            metadata: HashMap::new(),
        }
    }

    pub fn with_priority(mut self, priority: MessagePriority) -> Self {
        self.priority = priority;
        self
    }

    pub fn with_template(
        mut self,
        template_id: String,
        vars: HashMap<String, String>,
    ) -> Self {
        self.template_id = Some(template_id);
        self.template_vars = vars;
        self
    }

    pub fn schedule(mut self, at: DateTime<Utc>) -> Self {
        self.scheduled_at = Some(at);
        self
    }

    pub fn is_expired(&self) -> bool {
        self.expires_at
            .map(|exp| exp < Utc::now())
            .unwrap_or(false)
    }
}

/// Delivery attempt tracking
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct DeliveryAttempt {
    pub message_id: String,
    pub attempt_number: u32,
    pub status: DeliveryStatus,
    pub provider: String,
    pub attempted_at: DateTime<Utc>,
    pub completed_at: Option<DateTime<Utc>>,
    pub failure_reason: Option<String>,
    pub response_time_ms: Option<u64>,
}

/// Rate limiting configuration
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct RateLimit {
    pub channel: Channel,
    pub max_per_minute: u32,
    pub max_per_hour: u32,
    pub max_per_day: u32,
    pub burst_size: u32,
}

impl RateLimit {
    pub fn new(channel: Channel) -> Self {
        match channel {
            Channel::Email => Self {
                channel,
                max_per_minute: 100,
                max_per_hour: 5000,
                max_per_day: 50000,
                burst_size: 200,
            },
            Channel::SMS => Self {
                channel,
                max_per_minute: 50,
                max_per_hour: 1000,
                max_per_day: 10000,
                burst_size: 100,
            },
            Channel::Push => Self {
                channel,
                max_per_minute: 1000,
                max_per_hour: 50000,
                max_per_day: 500000,
                burst_size: 2000,
            },
            _ => Self {
                channel,
                max_per_minute: 100,
                max_per_hour: 5000,
                max_per_day: 50000,
                burst_size: 200,
            },
        }
    }

    pub fn check_limit(&self, current_count: u32, window: &str) -> bool {
        match window {
            "minute" => current_count < self.max_per_minute,
            "hour" => current_count < self.max_per_hour,
            "day" => current_count < self.max_per_day,
            _ => false,
        }
    }
}

/// Message provider trait
#[async_trait]
pub trait MessageProvider: Send + Sync {
    async fn send(&self, message: &Message) -> Result<DeliveryAttempt, Box<dyn std::error::Error>>;
    async fn get_status(&self, message_id: &str) -> Result<DeliveryStatus, Box<dyn std::error::Error>>;
    fn supports_channel(&self, channel: &Channel) -> bool;
    fn provider_name(&self) -> &str;
}

/// Message queue interface
#[async_trait]
pub trait MessageQueue: Send + Sync {
    async fn enqueue(&self, message: Message) -> Result<(), Box<dyn std::error::Error>>;
    async fn dequeue(&self, count: usize) -> Result<Vec<Message>, Box<dyn std::error::Error>>;
    async fn peek(&self, count: usize) -> Result<Vec<Message>, Box<dyn std::error::Error>>;
    async fn depth(&self) -> Result<usize, Box<dyn std::error::Error>>;
    async fn acknowledge(&self, message_id: &str) -> Result<(), Box<dyn std::error::Error>>;
}

/// Notification hub coordinator
pub struct NotificationHub {
    providers: HashMap<Channel, Vec<Box<dyn MessageProvider>>>,
    queues: HashMap<Channel, Box<dyn MessageQueue>>,
    rate_limits: HashMap<Channel, RateLimit>,
}

impl NotificationHub {
    pub fn new() -> Self {
        let mut rate_limits = HashMap::new();
        for channel in [
            Channel::Email,
            Channel::SMS,
            Channel::Push,
            Channel::Webhook,
        ] {
            rate_limits.insert(channel.clone(), RateLimit::new(channel));
        }

        Self {
            providers: HashMap::new(),
            queues: HashMap::new(),
            rate_limits,
        }
    }

    pub fn register_provider(
        &mut self,
        channel: Channel,
        provider: Box<dyn MessageProvider>,
    ) {
        self.providers
            .entry(channel)
            .or_insert_with(Vec::new)
            .push(provider);
    }

    pub fn register_queue(
        &mut self,
        channel: Channel,
        queue: Box<dyn MessageQueue>,
    ) {
        self.queues.insert(channel, queue);
    }

    pub async fn send_message(
        &self,
        message: Message,
    ) -> Result<DeliveryAttempt, Box<dyn std::error::Error>> {
        // Check expiration
        if message.is_expired() {
            return Err("Message expired".into());
        }

        // Check if scheduled
        if let Some(scheduled_at) = message.scheduled_at {
            if scheduled_at > Utc::now() {
                // Queue for later delivery
                if let Some(queue) = self.queues.get(&message.channel) {
                    queue.enqueue(message).await?;
                    return Ok(DeliveryAttempt {
                        message_id: "queued".to_string(),
                        attempt_number: 0,
                        status: DeliveryStatus::Queued,
                        provider: "queue".to_string(),
                        attempted_at: Utc::now(),
                        completed_at: None,
                        failure_reason: None,
                        response_time_ms: None,
                    });
                }
            }
        }

        // Get providers for channel
        let providers = self
            .providers
            .get(&message.channel)
            .ok_or("No provider for channel")?;

        // Try providers in order
        for provider in providers {
            match provider.send(&message).await {
                Ok(attempt) => return Ok(attempt),
                Err(e) => {
                    eprintln!("Provider {} failed: {}", provider.provider_name(), e);
                    continue;
                }
            }
        }

        Err("All providers failed".into())
    }

    pub async fn process_queue(
        &self,
        channel: &Channel,
        batch_size: usize,
    ) -> Result<Vec<DeliveryAttempt>, Box<dyn std::error::Error>> {
        let queue = self
            .queues
            .get(channel)
            .ok_or("No queue for channel")?;

        let messages = queue.dequeue(batch_size).await?;
        let mut results = Vec::new();

        for message in messages {
            match self.send_message(message.clone()).await {
                Ok(attempt) => {
                    queue.acknowledge(&message.id).await?;
                    results.push(attempt);
                }
                Err(e) => {
                    eprintln!("Failed to send message {}: {}", message.id, e);
                }
            }
        }

        Ok(results)
    }

    pub fn get_rate_limit(&self, channel: &Channel) -> Option<&RateLimit> {
        self.rate_limits.get(channel)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_message_creation() {
        let message = Message::new(
            "msg-123".to_string(),
            "Test message".to_string(),
            Channel::Email,
            vec!["user-1".to_string()],
        );

        assert_eq!(message.id, "msg-123");
        assert_eq!(message.body, "Test message");
        assert_eq!(message.channel, Channel::Email);
    }

    #[test]
    fn test_message_priority() {
        let message = Message::new(
            "msg-123".to_string(),
            "Test".to_string(),
            Channel::SMS,
            vec!["user-1".to_string()],
        )
        .with_priority(MessagePriority::High);

        assert_eq!(message.priority, MessagePriority::High);
    }

    #[test]
    fn test_rate_limit() {
        let rate_limit = RateLimit::new(Channel::Email);

        assert!(rate_limit.check_limit(50, "minute"));
        assert!(!rate_limit.check_limit(150, "minute"));
    }

    #[test]
    fn test_message_expiration() {
        let mut message = Message::new(
            "msg-123".to_string(),
            "Test".to_string(),
            Channel::Email,
            vec!["user-1".to_string()],
        );

        message.expires_at = Some(Utc::now() - Duration::hours(1));
        assert!(message.is_expired());

        message.expires_at = Some(Utc::now() + Duration::hours(1));
        assert!(!message.is_expired());
    }
}
