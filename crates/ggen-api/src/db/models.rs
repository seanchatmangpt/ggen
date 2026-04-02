//! Database models

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use sqlx::FromRow;

/// User model
#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct User {
    pub id: String,
    pub email: String,
    pub username: String,
    pub password_hash: String,
    pub tier: String, // "free", "pro", "enterprise"
    pub stripe_customer_id: Option<String>,
    pub created_at: String,
    pub updated_at: String,
}

/// API Key model
#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct ApiKey {
    pub id: String,
    pub user_id: String,
    pub key_hash: String,
    pub name: String,
    pub expires_at: Option<String>,
    pub last_used: Option<String>,
    pub active: bool,
    pub created_at: String,
}

/// Subscription model
#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct Subscription {
    pub id: String,
    pub user_id: String,
    pub tier: String,
    pub stripe_subscription_id: Option<String>,
    pub status: String, // "active", "canceled", "past_due"
    pub current_period_start: String,
    pub current_period_end: String,
    pub cancel_at_period_end: bool,
    pub created_at: String,
}

/// Payment model
#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct Payment {
    pub id: String,
    pub user_id: String,
    pub stripe_payment_id: Option<String>,
    pub amount_cents: i64,
    pub currency: String,
    pub status: String, // "pending", "succeeded", "failed"
    pub description: String,
    pub created_at: String,
}

/// Invoice model
#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct Invoice {
    pub id: String,
    pub user_id: String,
    pub stripe_invoice_id: Option<String>,
    pub amount_cents: i64,
    pub status: String,
    pub issued_at: String,
    pub due_at: String,
    pub paid_at: Option<String>,
    pub created_at: String,
}

/// Usage event model
#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct UsageEvent {
    pub id: String,
    pub user_id: String,
    pub operation: String, // "api_call", "template_install", etc.
    pub cost: i64,
    pub created_at: String,
}

/// Webhook event model (for processing Stripe events)
#[derive(Debug, Clone, Serialize, Deserialize, FromRow)]
pub struct WebhookEvent {
    pub id: String,
    pub stripe_event_id: String,
    pub event_type: String,
    pub data: String, // JSON
    pub status: String, // "pending", "processed", "failed"
    pub created_at: String,
}
