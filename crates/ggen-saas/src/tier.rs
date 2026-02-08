//! SaaS tier definitions

use crate::limits::TierLimits;
use serde::{Deserialize, Serialize};

/// SaaS tier types
#[derive(Debug, Clone, Copy, Eq, PartialEq, Serialize, Deserialize, Hash)]
pub enum Tier {
    Free,
    Pro,
    Enterprise,
}

impl Tier {
    pub fn as_str(&self) -> &'static str {
        match self {
            Tier::Free => "free",
            Tier::Pro => "pro",
            Tier::Enterprise => "enterprise",
        }
    }

    pub fn from_str(s: &str) -> Option<Self> {
        match s {
            "free" => Some(Tier::Free),
            "pro" => Some(Tier::Pro),
            "enterprise" => Some(Tier::Enterprise),
            _ => None,
        }
    }

    pub fn level(&self) -> u8 {
        match self {
            Tier::Free => 1,
            Tier::Pro => 2,
            Tier::Enterprise => 3,
        }
    }

    pub fn limits(&self) -> TierLimits {
        match self {
            Tier::Free => TierLimits {
                api_calls_per_month: 1_000,
                marketplace_templates: 5,
                concurrent_installations: 2,
                storage_gb: 1,
                custom_ontologies: false,
                priority_support: false,
                sla_uptime: None,
            },
            Tier::Pro => TierLimits {
                api_calls_per_month: 100_000,
                marketplace_templates: 50,
                concurrent_installations: 20,
                storage_gb: 50,
                custom_ontologies: true,
                priority_support: true,
                sla_uptime: Some(99.5),
            },
            Tier::Enterprise => TierLimits {
                api_calls_per_month: u64::MAX,
                marketplace_templates: u64::MAX,
                concurrent_installations: u64::MAX,
                storage_gb: u64::MAX,
                custom_ontologies: true,
                priority_support: true,
                sla_uptime: Some(99.99),
            },
        }
    }

    pub fn price_monthly(&self) -> Option<f64> {
        match self {
            Tier::Free => Some(0.0),
            Tier::Pro => Some(29.0),
            Tier::Enterprise => None, // Custom pricing
        }
    }
}

/// Tier hierarchy for comparisons
pub struct TierHierarchy;

impl TierHierarchy {
    /// Check if a tier meets or exceeds a minimum requirement
    pub fn meets_requirement(user_tier: Tier, required_tier: Tier) -> bool {
        user_tier.level() >= required_tier.level()
    }

    /// Can user upgrade from one tier to another
    pub fn can_upgrade(from: Tier, to: Tier) -> bool {
        to.level() > from.level()
    }

    /// Can user downgrade from one tier to another
    pub fn can_downgrade(from: Tier, to: Tier) -> bool {
        to.level() < from.level()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tier_hierarchy() {
        assert!(TierHierarchy::meets_requirement(Tier::Pro, Tier::Free));
        assert!(TierHierarchy::meets_requirement(
            Tier::Enterprise,
            Tier::Pro
        ));
        assert!(!TierHierarchy::meets_requirement(Tier::Free, Tier::Pro));
    }

    #[test]
    fn test_tier_limits() {
        let free_limits = Tier::Free.limits();
        assert_eq!(free_limits.api_calls_per_month, 1_000);
        assert!(!free_limits.custom_ontologies);
    }
}
