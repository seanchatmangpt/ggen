//! Payment operations

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Payment information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Payment {
    pub id: String,
    pub customer_id: String,
    pub amount_cents: i64,
    pub currency: String,
    pub status: String, // "requires_payment_method", "processing", "succeeded", "failed"
    pub description: String,
    pub created_at: DateTime<Utc>,
    pub updated_at: DateTime<Utc>,
    pub client_secret: Option<String>,
}

impl Payment {
    pub fn is_succeeded(&self) -> bool {
        self.status == "succeeded"
    }

    pub fn is_failed(&self) -> bool {
        self.status == "failed"
    }

    pub fn is_processing(&self) -> bool {
        self.status == "processing"
    }

    pub fn amount_dollars(&self) -> f64 {
        self.amount_cents as f64 / 100.0
    }
}
