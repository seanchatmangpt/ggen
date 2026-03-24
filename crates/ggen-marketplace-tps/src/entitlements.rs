//! GCP Marketplace entitlement checks.
//!
//! Type-first design: Entitlement state is strongly typed.

use crate::auth::OAuth2Manager;
use crate::error::{MarketplaceError, Result};
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Entitlement status.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "SCREAMING_SNAKE_CASE")]
pub enum EntitlementState {
    /// Entitlement is active
    Active,
    /// Entitlement is pending activation
    Pending,
    /// Entitlement is suspended
    Suspended,
    /// Entitlement is cancelled
    Cancelled,
    /// Entitlement has expired
    Expired,
}

impl EntitlementState {
    /// Check if entitlement allows access.
    #[must_use]
    pub const fn allows_access(&self) -> bool {
        matches!(self, Self::Active)
    }
}

/// Plan tier.
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "lowercase")]
pub enum PlanTier {
    /// Free tier
    Free,
    /// Basic tier
    Basic,
    /// Professional tier
    Professional,
    /// Enterprise tier
    Enterprise,
}

impl PlanTier {
    /// Get usage quota for this tier.
    #[must_use]
    pub const fn usage_quota(&self) -> u64 {
        match self {
            Self::Free => 100,
            Self::Basic => 1000,
            Self::Professional => 10_000,
            Self::Enterprise => u64::MAX,
        }
    }
}

/// GCP Marketplace entitlement.
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Entitlement {
    /// Entitlement ID
    pub id: String,
    /// Account ID
    pub account_id: String,
    /// Product ID
    pub product_id: String,
    /// Plan tier
    pub plan: PlanTier,
    /// State
    pub state: EntitlementState,
    /// Creation time
    pub create_time: DateTime<Utc>,
    /// Update time
    pub update_time: DateTime<Utc>,
    /// Usage quota
    pub usage_quota: u64,
    /// Current usage
    pub usage_current: u64,
}

impl Entitlement {
    /// Check if entitlement is valid and allows access.
    #[must_use]
    pub fn is_valid(&self) -> bool {
        self.state.allows_access() && self.usage_current < self.usage_quota
    }

    /// Check if usage is approaching quota (>80%).
    #[must_use]
    pub fn is_quota_warning(&self) -> bool {
        self.usage_quota > 0 && (self.usage_current * 100) / self.usage_quota > 80
    }

    /// Remaining quota.
    #[must_use]
    pub fn remaining_quota(&self) -> u64 {
        self.usage_quota.saturating_sub(self.usage_current)
    }
}

/// Entitlement service for GCP Marketplace.
pub struct EntitlementService {
    client: reqwest::Client,
    auth_manager: OAuth2Manager,
    api_base_url: String,
}

impl EntitlementService {
    /// Create a new entitlement service.
    #[must_use]
    pub fn new(auth_manager: OAuth2Manager, api_base_url: String) -> Self {
        Self {
            client: reqwest::Client::new(),
            auth_manager,
            api_base_url,
        }
    }

    /// Get entitlement by ID.
    ///
    /// # Errors
    /// Returns error if request fails or entitlement not found.
    pub async fn get_entitlement(&self, entitlement_id: &str) -> Result<Entitlement> {
        let token = self.auth_manager.get_valid_token().await?;
        let url = format!("{}/entitlements/{entitlement_id}", self.api_base_url);

        let response = self
            .client
            .get(&url)
            .bearer_auth(&token.access_token)
            .send()
            .await?;

        if response.status() == reqwest::StatusCode::NOT_FOUND {
            return Err(MarketplaceError::NotFound(format!(
                "Entitlement {entitlement_id} not found"
            )));
        }

        if !response.status().is_success() {
            return Err(MarketplaceError::EntitlementError(format!(
                "Failed to get entitlement: {}",
                response.status()
            )));
        }

        let entitlement: Entitlement = response.json().await?;
        Ok(entitlement)
    }

    /// List entitlements for an account.
    ///
    /// # Errors
    /// Returns error if request fails.
    pub async fn list_entitlements(&self, account_id: &str) -> Result<Vec<Entitlement>> {
        let token = self.auth_manager.get_valid_token().await?;
        let url = format!("{}/accounts/{account_id}/entitlements", self.api_base_url);

        let response = self
            .client
            .get(&url)
            .bearer_auth(&token.access_token)
            .send()
            .await?;

        if !response.status().is_success() {
            return Err(MarketplaceError::EntitlementError(format!(
                "Failed to list entitlements: {}",
                response.status()
            )));
        }

        let entitlements: Vec<Entitlement> = response.json().await?;
        Ok(entitlements)
    }

    /// Check if account has valid entitlement.
    ///
    /// # Errors
    /// Returns error if request fails.
    pub async fn check_access(&self, account_id: &str, product_id: &str) -> Result<bool> {
        let entitlements = self.list_entitlements(account_id).await?;

        let has_valid = entitlements
            .iter()
            .any(|e| e.product_id == product_id && e.is_valid());

        Ok(has_valid)
    }

    /// Report usage for an entitlement.
    ///
    /// # Errors
    /// Returns error if request fails or quota exceeded.
    pub async fn report_usage(&self, entitlement_id: &str, usage: u64) -> Result<()> {
        let token = self.auth_manager.get_valid_token().await?;
        let url = format!("{}/entitlements/{entitlement_id}/usage", self.api_base_url);

        let request_body = serde_json::json!({
            "usage": usage,
            "timestamp": Utc::now().to_rfc3339(),
        });

        let response = self
            .client
            .post(&url)
            .bearer_auth(&token.access_token)
            .json(&request_body)
            .send()
            .await?;

        if response.status() == reqwest::StatusCode::TOO_MANY_REQUESTS {
            return Err(MarketplaceError::RateLimitExceeded(
                "Usage quota exceeded".to_string()
            ));
        }

        if !response.status().is_success() {
            return Err(MarketplaceError::EntitlementError(format!(
                "Failed to report usage: {}",
                response.status()
            )));
        }

        Ok(())
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_entitlement_state_allows_access() {
        // Arrange & Act & Assert
        assert!(EntitlementState::Active.allows_access());
        assert!(!EntitlementState::Pending.allows_access());
        assert!(!EntitlementState::Suspended.allows_access());
        assert!(!EntitlementState::Cancelled.allows_access());
        assert!(!EntitlementState::Expired.allows_access());
    }

    #[test]
    fn test_plan_tier_quota() {
        // Arrange & Act & Assert
        assert_eq!(PlanTier::Free.usage_quota(), 100);
        assert_eq!(PlanTier::Basic.usage_quota(), 1000);
        assert_eq!(PlanTier::Professional.usage_quota(), 10_000);
        assert_eq!(PlanTier::Enterprise.usage_quota(), u64::MAX);
    }

    #[test]
    fn test_entitlement_validity() {
        // Arrange
        let valid = Entitlement {
            id: "ent-1".to_string(),
            account_id: "acc-1".to_string(),
            product_id: "prod-1".to_string(),
            plan: PlanTier::Professional,
            state: EntitlementState::Active,
            create_time: Utc::now(),
            update_time: Utc::now(),
            usage_quota: 1000,
            usage_current: 500,
        };

        let over_quota = Entitlement {
            id: "ent-2".to_string(),
            account_id: "acc-1".to_string(),
            product_id: "prod-1".to_string(),
            plan: PlanTier::Basic,
            state: EntitlementState::Active,
            create_time: Utc::now(),
            update_time: Utc::now(),
            usage_quota: 1000,
            usage_current: 1000,
        };

        // Act & Assert
        assert!(valid.is_valid());
        assert!(!over_quota.is_valid());
    }

    #[test]
    fn test_quota_warning() {
        // Arrange
        let warning = Entitlement {
            id: "ent-1".to_string(),
            account_id: "acc-1".to_string(),
            product_id: "prod-1".to_string(),
            plan: PlanTier::Basic,
            state: EntitlementState::Active,
            create_time: Utc::now(),
            update_time: Utc::now(),
            usage_quota: 1000,
            usage_current: 850,
        };

        let no_warning = Entitlement {
            id: "ent-2".to_string(),
            account_id: "acc-1".to_string(),
            product_id: "prod-1".to_string(),
            plan: PlanTier::Basic,
            state: EntitlementState::Active,
            create_time: Utc::now(),
            update_time: Utc::now(),
            usage_quota: 1000,
            usage_current: 500,
        };

        // Act & Assert
        assert!(warning.is_quota_warning());
        assert!(!no_warning.is_quota_warning());
    }

    #[test]
    fn test_remaining_quota() {
        // Arrange
        let entitlement = Entitlement {
            id: "ent-1".to_string(),
            account_id: "acc-1".to_string(),
            product_id: "prod-1".to_string(),
            plan: PlanTier::Professional,
            state: EntitlementState::Active,
            create_time: Utc::now(),
            update_time: Utc::now(),
            usage_quota: 10_000,
            usage_current: 3_500,
        };

        // Act
        let remaining = entitlement.remaining_quota();

        // Assert
        assert_eq!(remaining, 6_500);
    }
}
