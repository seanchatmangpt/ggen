//! Invoice management

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Invoice information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Invoice {
    pub id: String,
    pub customer_id: String,
    pub amount_cents: i64,
    pub status: String, // "draft", "open", "paid", "void", "uncollectible"
    pub description: String,
    pub issued_at: DateTime<Utc>,
    pub due_at: DateTime<Utc>,
    pub paid_at: Option<DateTime<Utc>>,
}

impl Invoice {
    pub fn is_paid(&self) -> bool {
        self.status == "paid"
    }

    pub fn is_overdue(&self) -> bool {
        self.due_at < Utc::now() && !self.is_paid()
    }

    pub fn amount_dollars(&self) -> f64 {
        self.amount_cents as f64 / 100.0
    }
}
