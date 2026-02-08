//! Webhook event handling

use serde::{Deserialize, Serialize};
use serde_json::Value as JsonValue;

/// Webhook event types
#[derive(Debug, Clone, Serialize, Deserialize)]
#[serde(tag = "type")]
pub enum WebhookEvent {
    #[serde(rename = "payment_intent.succeeded")]
    PaymentIntentSucceeded { data: PaymentIntentData },

    #[serde(rename = "payment_intent.payment_failed")]
    PaymentIntentFailed { data: PaymentIntentData },

    #[serde(rename = "customer.subscription.created")]
    SubscriptionCreated { data: SubscriptionData },

    #[serde(rename = "customer.subscription.updated")]
    SubscriptionUpdated { data: SubscriptionData },

    #[serde(rename = "customer.subscription.deleted")]
    SubscriptionDeleted { data: SubscriptionData },

    #[serde(rename = "invoice.paid")]
    InvoicePaid { data: InvoiceData },

    #[serde(rename = "invoice.payment_failed")]
    InvoicePaymentFailed { data: InvoiceData },

    #[serde(rename = "charge.failed")]
    ChargeFailed { data: ChargeData },

    #[serde(rename = "charge.refunded")]
    ChargeRefunded { data: ChargeData },
}

/// Payment intent data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct PaymentIntentData {
    pub id: String,
    pub customer: Option<String>,
    pub amount: i64,
    pub currency: String,
    pub status: String,
    pub client_secret: Option<String>,
}

/// Subscription data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct SubscriptionData {
    pub id: String,
    pub customer: String,
    pub status: String,
    pub current_period_start: i64,
    pub current_period_end: i64,
    pub cancel_at_period_end: bool,
}

/// Invoice data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct InvoiceData {
    pub id: String,
    pub customer: String,
    pub amount_paid: i64,
    pub amount_due: i64,
    pub status: String,
    pub paid: bool,
}

/// Charge data
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ChargeData {
    pub id: String,
    pub customer: Option<String>,
    pub amount: i64,
    pub currency: String,
    pub status: String,
    pub reason: Option<String>,
}

/// Webhook payload
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct WebhookPayload {
    pub id: String,
    pub object: String,
    pub api_version: String,
    pub created: i64,
    pub data: JsonValue,
    pub livemode: bool,
    #[serde(rename = "type")]
    pub event_type: String,
}
