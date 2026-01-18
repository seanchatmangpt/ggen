//! Quota management and enforcement

use async_trait::async_trait;
use chrono::Utc;
use moka::future::Cache;
use std::sync::Arc;
use std::time::Duration;

use crate::{errors::SaasError, limits::Limits, tier::Tier, SaasResult};

/// Quota state for a user
#[derive(Debug, Clone)]
pub struct QuotaState {
    pub user_id: String,
    pub tier: Tier,
    pub api_calls_used: u64,
    pub templates_used: u64,
    pub storage_used: u64,
    pub reset_at: chrono::DateTime<chrono::Utc>,
}

/// Quota manager for enforcing limits
pub struct QuotaManager {
    // Cache quotas in memory with TTL
    quota_cache: Arc<Cache<String, QuotaState>>,
    cache_ttl: Duration,
}

impl QuotaManager {
    /// Create a new quota manager
    pub fn new(cache_ttl_secs: u64) -> Self {
        let cache = Cache::builder()
            .time_to_live(Duration::from_secs(cache_ttl_secs))
            .build();

        Self {
            quota_cache: Arc::new(cache),
            cache_ttl: Duration::from_secs(cache_ttl_secs),
        }
    }

    /// Check if user can perform an operation
    pub async fn check_quota(
        &self,
        user_id: &str,
        operation: &str,
        cost: u64,
    ) -> SaasResult<QuotaState> {
        let quota = self.get_quota(user_id).await?;

        match operation {
            "api_call" => {
                let limits = quota.tier.limits();
                let remaining = limits
                    .api_calls_per_month
                    .saturating_sub(quota.api_calls_used);

                if remaining < cost {
                    return Err(SaasError::QuotaExceeded {
                        resource: "api_calls".to_string(),
                        used: quota.api_calls_used,
                        limit: limits.api_calls_per_month,
                    });
                }
            }
            "template_install" => {
                let limits = quota.tier.limits();
                let remaining = limits
                    .marketplace_templates
                    .saturating_sub(quota.templates_used);

                if remaining < cost {
                    return Err(SaasError::QuotaExceeded {
                        resource: "templates".to_string(),
                        used: quota.templates_used,
                        limit: limits.marketplace_templates,
                    });
                }
            }
            _ => {}
        }

        Ok(quota)
    }

    /// Record usage
    pub async fn record_usage(
        &self,
        user_id: &str,
        operation: &str,
        cost: u64,
    ) -> SaasResult<()> {
        let mut quota = self.get_quota(user_id).await?;

        match operation {
            "api_call" => quota.api_calls_used += cost,
            "template_install" => quota.templates_used += cost,
            "storage" => quota.storage_used += cost,
            _ => {}
        }

        // Update cache
        self.quota_cache.insert(user_id.to_string(), quota).await;

        // TODO: Persist to database

        Ok(())
    }

    /// Get quota for user
    pub async fn get_quota(&self, user_id: &str) -> SaasResult<QuotaState> {
        // Try cache first
        if let Some(quota) = self.quota_cache.get(user_id).await {
            return Ok(quota);
        }

        // TODO: Load from database if not in cache
        // For now, return default free tier
        let quota = QuotaState {
            user_id: user_id.to_string(),
            tier: Tier::Free,
            api_calls_used: 0,
            templates_used: 0,
            storage_used: 0,
            reset_at: chrono::Utc::now() + chrono::Duration::days(30),
        };

        // Cache it
        self.quota_cache.insert(user_id.to_string(), quota.clone()).await;

        Ok(quota)
    }

    /// Update user's tier
    pub async fn update_tier(&self, user_id: &str, tier: Tier) -> SaasResult<()> {
        let mut quota = self.get_quota(user_id).await?;
        quota.tier = tier;

        // Reset usage on upgrade
        if quota.api_calls_used > 0 || quota.templates_used > 0 {
            quota.api_calls_used = 0;
            quota.templates_used = 0;
            quota.storage_used = 0;
        }

        self.quota_cache.insert(user_id.to_string(), quota).await;

        // TODO: Persist to database

        Ok(())
    }

    /// Get current usage limits
    pub async fn get_limits(&self, user_id: &str, resource: &str) -> SaasResult<Limits> {
        let quota = self.get_quota(user_id).await?;
        let tier_limits = quota.tier.limits();

        let (limit, used) = match resource {
            "api_calls" => (tier_limits.api_calls_per_month, quota.api_calls_used),
            "templates" => (tier_limits.marketplace_templates, quota.templates_used),
            "storage" => (tier_limits.storage_gb, quota.storage_used),
            _ => return Err(SaasError::ConfigError("Unknown resource".to_string())),
        };

        Ok(Limits::new(
            resource.to_string(),
            limit,
            used,
            Some(quota.reset_at),
        ))
    }

    /// Clear quota cache for user
    pub async fn invalidate_cache(&self, user_id: &str) {
        self.quota_cache.remove(user_id).await;
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_quota_check() {
        let manager = QuotaManager::new(300);
        let result = manager.check_quota("user1", "api_call", 100).await;
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_quota_exceeded() {
        let manager = QuotaManager::new(300);
        let _quota = manager.get_quota("user1").await.unwrap();

        // Try to use more than free tier limit
        let result = manager
            .check_quota("user1", "api_call", 2000)
            .await;

        assert!(result.is_err());
    }
}
