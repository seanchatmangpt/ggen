//! Subscription management

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Subscription information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Subscription {
    pub id: String,
    pub customer_id: String,
    pub price_id: String,
    pub status: String, // "active", "past_due", "canceled", "unpaid"
    pub current_period_start: DateTime<Utc>,
    pub current_period_end: DateTime<Utc>,
    pub cancel_at_period_end: bool,
    pub created_at: DateTime<Utc>,
}

impl Subscription {
    pub fn is_active(&self) -> bool {
        self.status == "active"
    }

    pub fn is_canceled(&self) -> bool {
        self.status == "canceled"
    }

    pub fn is_past_due(&self) -> bool {
        self.status == "past_due"
    }
}
