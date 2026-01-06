//! Quota limits per tier

use serde::{Deserialize, Serialize};

/// Resource limits for a tier
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TierLimits {
    pub api_calls_per_month: u64,
    pub marketplace_templates: u64,
    pub concurrent_installations: u64,
    pub storage_gb: u64,
    pub custom_ontologies: bool,
    pub priority_support: bool,
    pub sla_uptime: Option<f64>, // e.g., 99.9
}

/// Current usage limits for a user
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Limits {
    pub resource: String,
    pub limit: u64,
    pub used: u64,
    pub remaining: u64,
    pub reset_at: Option<chrono::DateTime<chrono::Utc>>,
    pub percentage_used: f64,
}

impl Limits {
    pub fn new(resource: String, limit: u64, used: u64, reset_at: Option<chrono::DateTime<chrono::Utc>>) -> Self {
        let remaining = limit.saturating_sub(used);
        let percentage_used = if limit > 0 {
            (used as f64 / limit as f64) * 100.0
        } else {
            0.0
        };

        Self {
            resource,
            limit,
            used,
            remaining,
            reset_at,
            percentage_used,
        }
    }

    pub fn is_exceeded(&self) -> bool {
        self.used >= self.limit
    }

    pub fn is_near_limit(&self, threshold: f64) -> bool {
        self.percentage_used >= threshold
    }

    pub fn can_use(&self, amount: u64) -> bool {
        self.used + amount <= self.limit
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_limits_calculation() {
        let limits = Limits::new("api_calls".to_string(), 100, 50, None);
        assert_eq!(limits.remaining, 50);
        assert!(limits.percentage_used >= 49.9 && limits.percentage_used <= 50.1);
    }

    #[test]
    fn test_is_near_limit() {
        let limits = Limits::new("api_calls".to_string(), 100, 95, None);
        assert!(limits.is_near_limit(90.0));
        assert!(!limits.is_near_limit(99.0));
    }
}
