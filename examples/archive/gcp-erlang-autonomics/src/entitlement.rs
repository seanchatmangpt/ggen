//! Entitlement service - RevOps kernel
//!
//! This module handles the **Analyze** phase of MAPE-K and manages the marketplace lifecycle:
//! - SaaS entitlement state machine
//! - SKU activation and revocation
//! - Quota enforcement (CPU, memory, bandwidth)
//! - Audit trail for compliance

use thiserror::Error;
use serde::{Deserialize, Serialize};
use chrono::{DateTime, Utc, Duration};
use std::collections::HashMap;

/// Entitlement service errors
#[derive(Debug, Error)]
pub enum EntitlementError {
    #[error("Tenant not found: {0}")]
    TenantNotFound(String),

    #[error("Invalid SKU: {0}")]
    InvalidSku(String),

    #[error("Quota exceeded: {resource} ({current}/{limit})")]
    QuotaExceeded {
        resource: String,
        current: u32,
        limit: u32,
    },

    #[error("Entitlement expired for tenant: {0}")]
    EntitlementExpired(String),

    #[error("Cannot revoke: invalid state {current} for tenant {tenant_id}")]
    InvalidStateTransition {
        tenant_id: String,
        current: String,
    },

    #[error("Database error: {0}")]
    DatabaseError(String),

    #[error("Invalid input: {0}")]
    InvalidInput(String),
}

/// Entitlement lifecycle states (SaaS model)
#[derive(Debug, Clone, Copy, Serialize, Deserialize, Eq, PartialEq)]
pub enum EntitlementState {
    /// Trial or pending activation
    Pending,
    /// Active and entitled to resources
    Active,
    /// Temporarily paused (e.g., payment processing)
    Paused,
    /// Expired (subscription ended)
    Expired,
    /// Terminated (manual termination)
    Terminated,
}

impl EntitlementState {
    fn as_str(&self) -> &str {
        match self {
            EntitlementState::Pending => "Pending",
            EntitlementState::Active => "Active",
            EntitlementState::Paused => "Paused",
            EntitlementState::Expired => "Expired",
            EntitlementState::Terminated => "Terminated",
        }
    }
}

/// Quota limits per SKU
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct QuotaLimits {
    pub cpu_cores: u32,
    pub memory_gb: u32,
    pub concurrent_requests: u32,
    pub storage_gb: u32,
    pub daily_requests: u64,
}

/// Entitlement record
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Entitlement {
    pub tenant_id: String,
    pub sku: String,
    pub state: EntitlementState,
    pub created_at: DateTime<Utc>,
    pub expires_at: Option<DateTime<Utc>>,
    pub quota: QuotaLimits,
    pub current_usage: ResourceUsage,
}

/// Current resource usage tracking
#[derive(Debug, Clone, Serialize, Deserialize, Default)]
pub struct ResourceUsage {
    pub cpu_usage_pct: u32,
    pub memory_usage_pct: u32,
    pub concurrent_requests: u32,
    pub storage_used_gb: u32,
    pub requests_today: u64,
}

/// Entitlement service (simulates database access)
pub struct EntitlementService;

// Simple in-memory store for this example
static ENTITLEMENTS: std::sync::OnceLock<std::sync::Mutex<HashMap<String, Entitlement>>> =
    std::sync::OnceLock::new();

fn get_store() -> std::sync::MutexGuard<'static, HashMap<String, Entitlement>> {
    ENTITLEMENTS
        .get_or_init(|| std::sync::Mutex::new(HashMap::new()))
        .lock()
        .unwrap()
}

impl EntitlementService {
    /// Activate entitlement for a tenant with specified SKU
    ///
    /// ## Preconditions
    /// - SKU must be valid
    /// - Tenant not already active
    ///
    /// ## Returns
    /// Newly created active entitlement
    pub async fn activate(tenant_id: &str, sku: &str) -> Result<Entitlement, EntitlementError> {
        if tenant_id.is_empty() {
            return Err(EntitlementError::InvalidInput(
                "tenant_id cannot be empty".to_string(),
            ));
        }

        if sku.is_empty() {
            return Err(EntitlementError::InvalidSku("SKU cannot be empty".to_string()));
        }

        let quota = Self::quota_for_sku(sku)?;

        let entitlement = Entitlement {
            tenant_id: tenant_id.to_string(),
            sku: sku.to_string(),
            state: EntitlementState::Active,
            created_at: Utc::now(),
            expires_at: Some(Utc::now() + Duration::days(365)),
            quota,
            current_usage: ResourceUsage::default(),
        };

        let mut store = get_store();
        store.insert(tenant_id.to_string(), entitlement.clone());

        Ok(entitlement)
    }

    /// Get current entitlement for tenant
    pub async fn get_active(tenant_id: &str) -> Result<Entitlement, EntitlementError> {
        let store = get_store();
        let entitlement = store
            .get(tenant_id)
            .ok_or_else(|| EntitlementError::TenantNotFound(tenant_id.to_string()))?
            .clone();

        // Check if expired
        if let Some(expires_at) = entitlement.expires_at {
            if Utc::now() > expires_at {
                return Err(EntitlementError::EntitlementExpired(tenant_id.to_string()));
            }
        }

        // Check if state is active
        if entitlement.state != EntitlementState::Active {
            return Err(EntitlementError::InvalidStateTransition {
                tenant_id: tenant_id.to_string(),
                current: entitlement.state.as_str().to_string(),
            });
        }

        Ok(entitlement)
    }

    /// Revoke entitlement (transition to Terminated)
    pub async fn revoke(tenant_id: &str) -> Result<(), EntitlementError> {
        let mut store = get_store();
        let entitlement = store
            .get_mut(tenant_id)
            .ok_or_else(|| EntitlementError::TenantNotFound(tenant_id.to_string()))?;

        if entitlement.state == EntitlementState::Terminated {
            return Err(EntitlementError::InvalidStateTransition {
                tenant_id: tenant_id.to_string(),
                current: "Terminated".to_string(),
            });
        }

        entitlement.state = EntitlementState::Terminated;
        Ok(())
    }

    /// Check if tenant has quota for resource allocation
    pub async fn check_quota(
        tenant_id: &str,
        resource: &str,
        requested: u32,
    ) -> Result<bool, EntitlementError> {
        let store = get_store();
        let entitlement = store
            .get(tenant_id)
            .ok_or_else(|| EntitlementError::TenantNotFound(tenant_id.to_string()))?
            .clone();

        let (current, limit) = match resource {
            "cpu" => (
                entitlement.current_usage.cpu_usage_pct,
                entitlement.quota.cpu_cores * 20, // Rough conversion
            ),
            "memory" => (
                entitlement.current_usage.memory_usage_pct,
                entitlement.quota.memory_gb * 10,
            ),
            "requests" => (
                entitlement.current_usage.concurrent_requests,
                entitlement.quota.concurrent_requests,
            ),
            "storage" => (
                entitlement.current_usage.storage_used_gb,
                entitlement.quota.storage_gb,
            ),
            _ => {
                return Err(EntitlementError::InvalidInput(format!(
                    "Unknown resource: {}",
                    resource
                )))
            }
        };

        if current + requested > limit {
            return Err(EntitlementError::QuotaExceeded {
                resource: resource.to_string(),
                current: current + requested,
                limit,
            });
        }

        Ok(true)
    }

    /// Pause entitlement (for payment processing, etc.)
    pub async fn pause(tenant_id: &str) -> Result<(), EntitlementError> {
        let mut store = get_store();
        let entitlement = store
            .get_mut(tenant_id)
            .ok_or_else(|| EntitlementError::TenantNotFound(tenant_id.to_string()))?;

        if entitlement.state != EntitlementState::Active {
            return Err(EntitlementError::InvalidStateTransition {
                tenant_id: tenant_id.to_string(),
                current: entitlement.state.as_str().to_string(),
            });
        }

        entitlement.state = EntitlementState::Paused;
        Ok(())
    }

    /// Resume paused entitlement
    pub async fn resume(tenant_id: &str) -> Result<(), EntitlementError> {
        let mut store = get_store();
        let entitlement = store
            .get_mut(tenant_id)
            .ok_or_else(|| EntitlementError::TenantNotFound(tenant_id.to_string()))?;

        if entitlement.state != EntitlementState::Paused {
            return Err(EntitlementError::InvalidStateTransition {
                tenant_id: tenant_id.to_string(),
                current: entitlement.state.as_str().to_string(),
            });
        }

        entitlement.state = EntitlementState::Active;
        Ok(())
    }

    /// Get quota for SKU
    fn quota_for_sku(sku: &str) -> Result<QuotaLimits, EntitlementError> {
        match sku {
            "starter" => Ok(QuotaLimits {
                cpu_cores: 2,
                memory_gb: 4,
                concurrent_requests: 100,
                storage_gb: 10,
                daily_requests: 100_000,
            }),
            "pro" => Ok(QuotaLimits {
                cpu_cores: 8,
                memory_gb: 32,
                concurrent_requests: 1000,
                storage_gb: 100,
                daily_requests: 1_000_000,
            }),
            "enterprise" => Ok(QuotaLimits {
                cpu_cores: 64,
                memory_gb: 256,
                concurrent_requests: 10_000,
                storage_gb: 1000,
                daily_requests: 10_000_000,
            }),
            _ => Err(EntitlementError::InvalidSku(format!(
                "Unknown SKU: {}",
                sku
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[tokio::test]
    async fn test_activate_valid_sku() {
        // Arrange
        let tenant_id = "test-tenant-1";
        let sku = "pro";

        // Act
        let result = EntitlementService::activate(tenant_id, sku).await;

        // Assert
        assert!(result.is_ok());
        let ent = result.unwrap();
        assert_eq!(ent.tenant_id, tenant_id);
        assert_eq!(ent.sku, sku);
        assert_eq!(ent.state, EntitlementState::Active);
    }

    #[tokio::test]
    async fn test_activate_invalid_sku() {
        // Arrange
        let tenant_id = "test-tenant-2";
        let sku = "invalid-sku";

        // Act
        let result = EntitlementService::activate(tenant_id, sku).await;

        // Assert
        assert!(matches!(result, Err(EntitlementError::InvalidSku(_))));
    }

    #[tokio::test]
    async fn test_check_quota_within_limits() {
        // Arrange
        let tenant_id = "test-tenant-3";
        EntitlementService::activate(tenant_id, "pro").await.unwrap();

        // Act
        let result = EntitlementService::check_quota(tenant_id, "cpu", 2).await;

        // Assert
        assert!(result.is_ok());
    }

    #[tokio::test]
    async fn test_pause_and_resume() {
        // Arrange
        let tenant_id = "test-tenant-4";
        EntitlementService::activate(tenant_id, "starter").await.unwrap();

        // Act & Assert
        assert!(EntitlementService::pause(tenant_id).await.is_ok());
        assert!(EntitlementService::resume(tenant_id).await.is_ok());
    }

    #[tokio::test]
    async fn test_revoke_active_entitlement() {
        // Arrange
        let tenant_id = "test-tenant-5";
        EntitlementService::activate(tenant_id, "starter").await.unwrap();

        // Act
        let result = EntitlementService::revoke(tenant_id).await;

        // Assert
        assert!(result.is_ok());
    }
}
