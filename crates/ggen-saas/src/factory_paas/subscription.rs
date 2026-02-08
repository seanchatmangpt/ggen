//! SaaS subscription management and webhooks

use super::SubscriptionTier;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;
use uuid::Uuid;

/// Subscription status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum SubscriptionStatus {
    Active,
    Cancelled,
    PastDue,
    Trialing,
}

/// Subscription record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Subscription {
    /// Subscription ID
    pub id: Uuid,
    /// User ID
    pub user_id: Uuid,
    /// Subscription tier
    pub tier: SubscriptionTier,
    /// Status
    pub status: SubscriptionStatus,
    /// Created timestamp
    pub created_at: DateTime<Utc>,
    /// Updated timestamp
    pub updated_at: DateTime<Utc>,
    /// Current period start
    pub current_period_start: DateTime<Utc>,
    /// Current period end
    pub current_period_end: DateTime<Utc>,
    /// Click usage in current period
    pub clicks_used: u64,
}

impl Subscription {
    /// Create a new subscription
    pub fn new(user_id: Uuid, tier: SubscriptionTier) -> Self {
        let now = Utc::now();
        Self {
            id: Uuid::new_v4(),
            user_id,
            tier,
            status: SubscriptionStatus::Active,
            created_at: now,
            updated_at: now,
            current_period_start: now,
            current_period_end: now + chrono::Duration::days(30),
            clicks_used: 0,
        }
    }

    /// Check if quota is exceeded
    pub fn is_quota_exceeded(&self) -> bool {
        if let Some(quota) = self.tier.click_quota() {
            self.clicks_used >= quota
        } else {
            false // Unlimited
        }
    }

    /// Increment click usage
    pub fn increment_clicks(&mut self) -> Result<(), SubscriptionError> {
        if self.status != SubscriptionStatus::Active {
            return Err(SubscriptionError::InactiveSubscription);
        }

        if self.is_quota_exceeded() {
            return Err(SubscriptionError::QuotaExceeded);
        }

        self.clicks_used += 1;
        self.updated_at = Utc::now();
        Ok(())
    }

    /// Upgrade tier
    pub fn upgrade(&mut self, new_tier: SubscriptionTier) -> Result<(), SubscriptionError> {
        if self.status != SubscriptionStatus::Active {
            return Err(SubscriptionError::InactiveSubscription);
        }

        self.tier = new_tier;
        self.updated_at = Utc::now();
        Ok(())
    }

    /// Cancel subscription
    pub fn cancel(&mut self) {
        self.status = SubscriptionStatus::Cancelled;
        self.updated_at = Utc::now();
    }

    /// Reset usage for new billing period
    pub fn reset_usage(&mut self) {
        self.clicks_used = 0;
        self.current_period_start = Utc::now();
        self.current_period_end = self.current_period_start + chrono::Duration::days(30);
        self.updated_at = Utc::now();
    }
}

/// Webhook event types
#[derive(Debug, Clone, Serialize, Deserialize)]
pub enum WebhookEventType {
    SubscriptionCreated,
    SubscriptionUpdated,
    SubscriptionCancelled,
    PaymentSucceeded,
    PaymentFailed,
}

/// Webhook event
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WebhookEvent {
    /// Event ID
    pub id: Uuid,
    /// Event type
    pub event_type: WebhookEventType,
    /// Subscription ID
    pub subscription_id: Uuid,
    /// Timestamp
    pub timestamp: DateTime<Utc>,
    /// Payload
    pub payload: serde_json::Value,
}

impl WebhookEvent {
    /// Create a new webhook event
    pub fn new(
        event_type: WebhookEventType, subscription_id: Uuid, payload: serde_json::Value,
    ) -> Self {
        Self {
            id: Uuid::new_v4(),
            event_type,
            subscription_id,
            timestamp: Utc::now(),
            payload,
        }
    }
}

/// Subscription manager
pub struct SubscriptionManager {
    /// Subscriptions storage
    subscriptions: HashMap<Uuid, Subscription>,
    /// Webhook events storage
    webhook_events: HashMap<Uuid, WebhookEvent>,
}

impl SubscriptionManager {
    /// Create a new subscription manager
    pub fn new() -> Self {
        Self {
            subscriptions: HashMap::new(),
            webhook_events: HashMap::new(),
        }
    }

    /// Create a subscription
    pub fn create_subscription(&mut self, user_id: Uuid, tier: SubscriptionTier) -> Uuid {
        let subscription = Subscription::new(user_id, tier);
        let id = subscription.id;

        // Emit webhook event
        let event = WebhookEvent::new(
            WebhookEventType::SubscriptionCreated,
            id,
            serde_json::json!({ "tier": tier }),
        );
        self.webhook_events.insert(event.id, event);

        self.subscriptions.insert(id, subscription);
        id
    }

    /// Get subscription by ID
    pub fn get_subscription(&self, id: &Uuid) -> Option<&Subscription> {
        self.subscriptions.get(id)
    }

    /// Get mutable subscription by ID
    pub fn get_subscription_mut(&mut self, id: &Uuid) -> Option<&mut Subscription> {
        self.subscriptions.get_mut(id)
    }

    /// Process webhook event
    pub fn process_webhook(&mut self, event: WebhookEvent) -> Result<(), SubscriptionError> {
        let subscription = self
            .subscriptions
            .get_mut(&event.subscription_id)
            .ok_or(SubscriptionError::NotFound)?;

        match event.event_type {
            WebhookEventType::PaymentSucceeded => {
                subscription.reset_usage();
            }
            WebhookEventType::PaymentFailed => {
                subscription.status = SubscriptionStatus::PastDue;
                subscription.updated_at = Utc::now();
            }
            WebhookEventType::SubscriptionCancelled => {
                subscription.cancel();
            }
            _ => {}
        }

        self.webhook_events.insert(event.id, event);
        Ok(())
    }

    /// Get webhook events for subscription
    pub fn get_webhook_events(&self, subscription_id: &Uuid) -> Vec<&WebhookEvent> {
        self.webhook_events
            .values()
            .filter(|event| &event.subscription_id == subscription_id)
            .collect()
    }
}

impl Default for SubscriptionManager {
    fn default() -> Self {
        Self::new()
    }
}

/// Subscription errors
#[derive(Debug, Clone, thiserror::Error)]
pub enum SubscriptionError {
    #[error("Subscription not found")]
    NotFound,
    #[error("Subscription is not active")]
    InactiveSubscription,
    #[error("Quota exceeded")]
    QuotaExceeded,
}
