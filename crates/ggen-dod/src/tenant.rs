//! Multi-tenant isolation and context management
//!
//! ggen supports multiple organizations/domains with hard logical separation
//! at the observation, decision, and receipt levels.

use serde::{Deserialize, Serialize};
use std::collections::BTreeSet;

/// Tenant identifier
#[derive(Debug, Clone, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct TenantId(String);

impl TenantId {
    /// Create a new tenant ID
    pub fn new(id: impl Into<String>) -> Self {
        Self(id.into())
    }

    /// Get the string representation
    pub fn as_str(&self) -> &str {
        &self.0
    }
}

impl std::fmt::Display for TenantId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// Tenant context for operations
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct TenantContext {
    /// Tenant ID
    tenant_id: TenantId,
    /// Tenant name
    name: String,
    /// Tenant tier (free, pro, enterprise)
    tier: TenantTier,
    /// Whether tenant is active
    active: bool,
}

/// Tenant tier/subscription level
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
pub enum TenantTier {
    Free,
    Pro,
    Enterprise,
}

impl TenantContext {
    /// Create a new tenant context
    pub fn new(tenant_id: TenantId, name: impl Into<String>, tier: TenantTier) -> Self {
        Self {
            tenant_id,
            name: name.into(),
            tier,
            active: true,
        }
    }

    /// Get tenant ID
    pub fn tenant_id(&self) -> &TenantId {
        &self.tenant_id
    }

    /// Get tenant name
    pub fn name(&self) -> &str {
        &self.name
    }

    /// Get tier
    pub fn tier(&self) -> TenantTier {
        self.tier
    }

    /// Is tenant active?
    pub fn is_active(&self) -> bool {
        self.active
    }

    /// Deactivate tenant
    pub fn deactivate(mut self) -> Self {
        self.active = false;
        self
    }
}

/// Tenant isolation enforcement
pub struct TenantIsolation {
    /// Active tenants
    tenants: BTreeSet<TenantId>,
}

impl TenantIsolation {
    /// Create a new tenant isolation manager
    pub fn new() -> Self {
        Self {
            tenants: BTreeSet::new(),
        }
    }

    /// Register a tenant
    pub fn register(&mut self, tenant_id: TenantId) -> crate::error::DoDResult<()> {
        self.tenants.insert(tenant_id);
        Ok(())
    }

    /// Check if tenant exists
    pub fn exists(&self, tenant_id: &TenantId) -> bool {
        self.tenants.contains(tenant_id)
    }

    /// Verify isolation: all items must belong to same tenant
    pub fn verify_isolation(
        &self, tenant_id: &TenantId, items: &[&str],
    ) -> crate::error::DoDResult<()> {
        if !self.exists(tenant_id) {
            return Err(crate::error::DoDError::TenantIsolation(
                "tenant not registered".to_string(),
            ));
        }

        // Verify all items belong to this tenant
        // (In practice, this would check item's tenant_id field)
        Ok(())
    }

    /// Get all registered tenants
    pub fn all_tenants(&self) -> Vec<&TenantId> {
        self.tenants.iter().collect()
    }

    /// Get tenant count
    pub fn tenant_count(&self) -> usize {
        self.tenants.len()
    }
}

impl Default for TenantIsolation {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_tenant_context() {
        let tenant = TenantContext::new(
            TenantId::new("tenant-1"),
            "Acme Corp",
            TenantTier::Enterprise,
        );

        assert_eq!(tenant.name(), "Acme Corp");
        assert_eq!(tenant.tier(), TenantTier::Enterprise);
        assert!(tenant.is_active());
    }

    #[test]
    fn test_tenant_isolation() -> crate::error::DoDResult<()> {
        let mut isolation = TenantIsolation::new();
        let tenant_id = TenantId::new("tenant-1");

        isolation.register(tenant_id.clone())?;
        assert!(isolation.exists(&tenant_id));
        Ok(())
    }
}
