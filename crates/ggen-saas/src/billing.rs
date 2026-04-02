//! Billing cycle and usage accumulation

use chrono::{DateTime, Duration, Utc};
use serde::{Deserialize, Serialize};

use crate::tier::Tier;

/// Billing cycle information
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct BillingCycle {
    pub id: String,
    pub user_id: String,
    pub tier: Tier,
    pub period_start: DateTime<Utc>,
    pub period_end: DateTime<Utc>,
    pub amount_cents: i64,
    pub status: String, // "active", "past_due", "paid", "overdue"
}

impl BillingCycle {
    /// Create a new billing cycle for user
    pub fn new(user_id: String, tier: Tier) -> Self {
        let now = Utc::now();
        let period_end = now + Duration::days(30);

        Self {
            id: uuid::Uuid::new_v4().to_string(),
            user_id,
            tier,
            period_start: now,
            period_end,
            amount_cents: (tier.price_monthly().unwrap_or(0.0) * 100.0) as i64,
            status: "active".to_string(),
        }
    }

    /// Check if billing cycle is active
    pub fn is_active(&self) -> bool {
        let now = Utc::now();
        now >= self.period_start && now <= self.period_end
    }

    /// Check if billing cycle is expired
    pub fn is_expired(&self) -> bool {
        Utc::now() > self.period_end
    }

    /// Days remaining in billing cycle
    pub fn days_remaining(&self) -> i64 {
        (self.period_end - Utc::now()).num_days()
    }
}

/// Usage accumulator for billing period
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct UsageAccumulator {
    pub billing_cycle_id: String,
    pub api_calls: u64,
    pub templates_installed: u64,
    pub storage_gb: u64,
    pub total_cost_cents: i64,
    pub last_updated: DateTime<Utc>,
}

impl UsageAccumulator {
    pub fn new(billing_cycle_id: String) -> Self {
        Self {
            billing_cycle_id,
            api_calls: 0,
            templates_installed: 0,
            storage_gb: 0,
            total_cost_cents: 0,
            last_updated: Utc::now(),
        }
    }

    /// Add API call usage
    pub fn add_api_call(&mut self, cost_cents: i64) {
        self.api_calls += 1;
        self.total_cost_cents += cost_cents;
        self.last_updated = Utc::now();
    }

    /// Add template installation
    pub fn add_template_installation(&mut self, cost_cents: i64) {
        self.templates_installed += 1;
        self.total_cost_cents += cost_cents;
        self.last_updated = Utc::now();
    }

    /// Add storage usage
    pub fn add_storage(&mut self, gb: u64, cost_cents: i64) {
        self.storage_gb += gb;
        self.total_cost_cents += cost_cents;
        self.last_updated = Utc::now();
    }

    /// Get formatted cost
    pub fn cost_dollars(&self) -> f64 {
        self.total_cost_cents as f64 / 100.0
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_billing_cycle_creation() {
        let cycle = BillingCycle::new("user1".to_string(), Tier::Pro);
        assert!(cycle.is_active());
        assert!(!cycle.is_expired());
    }

    #[test]
    fn test_usage_accumulation() {
        let mut accumulator = UsageAccumulator::new("cycle1".to_string());
        accumulator.add_api_call(1);
        accumulator.add_api_call(1);
        assert_eq!(accumulator.api_calls, 2);
        assert_eq!(accumulator.total_cost_cents, 2);
    }
}
