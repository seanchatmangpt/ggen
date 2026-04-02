//! Authentication claims and user context

use serde::{Deserialize, Serialize};

/// User claims extracted from token
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Claims {
    pub user_id: String,
    pub email: String,
    pub tier: String,
    pub scopes: Vec<String>,
}

impl Claims {
    /// Check if user has a specific scope
    pub fn has_scope(&self, scope: &str) -> bool {
        self.scopes.contains(&scope.to_string())
    }

    /// Check if user is on a specific tier or higher
    pub fn is_tier_or_higher(&self, tier: &str) -> bool {
        let tier_level = Self::tier_level(tier);
        let user_tier_level = Self::tier_level(&self.tier);
        user_tier_level >= tier_level
    }

    /// Get numerical tier level (for comparison)
    fn tier_level(tier: &str) -> u32 {
        match tier {
            "free" => 1,
            "pro" => 2,
            "enterprise" => 3,
            _ => 0,
        }
    }
}
