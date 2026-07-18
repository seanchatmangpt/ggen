//! Entitlement lifecycle state machine tests
//!
//! Chicago TDD: Tests verify state transitions and invariants using real state objects.
//! Entitlements represent customer access rights with lifecycle states.
//!
//! State transitions: Active → Paused → Active → Expired
//! Quota enforcement per tier enforced through invariants.

use std::collections::HashMap;

/// Entitlement SKU tier (subscription level)
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord)]
pub enum SkuTier {
    /// Free tier with 10GB/month quota
    Free,
    /// Professional tier with 100GB/month quota
    Professional,
    /// Enterprise tier with unlimited quota
    Enterprise,
}

impl SkuTier {
    /// Get monthly quota limit in bytes
    pub fn quota_bytes_per_month(&self) -> u64 {
        match self {
            SkuTier::Free => 10 * 1024 * 1024 * 1024,      // 10GB
            SkuTier::Professional => 100 * 1024 * 1024 * 1024, // 100GB
            SkuTier::Enterprise => u64::MAX,                  // Unlimited
        }
    }
}

/// Entitlement state
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum EntitlementState {
    /// Active and can be used
    Active,
    /// Paused, cannot be used
    Paused,
    /// Expired, cannot be used
    Expired,
}

/// Quota usage tracking
#[derive(Debug, Clone)]
pub struct QuotaUsage {
    /// Current month's usage in bytes
    pub bytes_used_this_month: u64,
    /// Reset timestamp for monthly quota
    pub reset_at: chrono::DateTime<chrono::Utc>,
}

impl QuotaUsage {
    pub fn new() -> Self {
        Self {
            bytes_used_this_month: 0,
            reset_at: chrono::Utc::now() + chrono::Duration::days(30),
        }
    }

    /// Add to usage
    pub fn add_usage(&mut self, bytes: u64) {
        self.bytes_used_this_month += bytes;
    }

    /// Check if quota exceeded
    pub fn is_exceeded(&self, tier: SkuTier) -> bool {
        self.bytes_used_this_month > tier.quota_bytes_per_month()
    }

    /// Reset quota for new month
    pub fn reset_if_expired(&mut self) {
        if chrono::Utc::now() > self.reset_at {
            self.bytes_used_this_month = 0;
            self.reset_at = chrono::Utc::now() + chrono::Duration::days(30);
        }
    }
}

impl Default for QuotaUsage {
    fn default() -> Self {
        Self::new()
    }
}

/// An entitlement: customer subscription with state, quota, and lifecycle
#[derive(Debug, Clone)]
pub struct Entitlement {
    /// Entitlement ID (unique per customer)
    pub id: String,
    /// Customer/tenant ID
    pub customer_id: String,
    /// SKU tier
    pub tier: SkuTier,
    /// Current state
    pub state: EntitlementState,
    /// Quota usage tracking
    pub quota: QuotaUsage,
    /// Created timestamp
    pub created_at: chrono::DateTime<chrono::Utc>,
    /// Expired timestamp (if applicable)
    pub expires_at: Option<chrono::DateTime<chrono::Utc>>,
    /// Last state change
    pub last_state_change: chrono::DateTime<chrono::Utc>,
}

impl Entitlement {
    /// Create a new active entitlement
    pub fn new(customer_id: impl Into<String>, tier: SkuTier) -> Self {
        let customer_str = customer_id.into();
        let now = chrono::Utc::now();
        Self {
            id: format!("{}-{}", customer_str, uuid::Uuid::new_v4()),
            customer_id: customer_str,
            tier,
            state: EntitlementState::Active,
            quota: QuotaUsage::new(),
            created_at: now,
            expires_at: None,
            last_state_change: now,
        }
    }

    /// Pause the entitlement
    pub fn pause(&mut self) -> Result<(), String> {
        match self.state {
            EntitlementState::Active => {
                self.state = EntitlementState::Paused;
                self.last_state_change = chrono::Utc::now();
                Ok(())
            }
            EntitlementState::Paused => Err("Already paused".to_string()),
            EntitlementState::Expired => Err("Cannot pause expired entitlement".to_string()),
        }
    }

    /// Resume (unpause) the entitlement
    pub fn resume(&mut self) -> Result<(), String> {
        match self.state {
            EntitlementState::Paused => {
                self.state = EntitlementState::Active;
                self.last_state_change = chrono::Utc::now();
                Ok(())
            }
            EntitlementState::Active => Err("Already active".to_string()),
            EntitlementState::Expired => Err("Cannot resume expired entitlement".to_string()),
        }
    }

    /// Expire the entitlement
    pub fn expire(&mut self) -> Result<(), String> {
        match self.state {
            EntitlementState::Expired => Err("Already expired".to_string()),
            _ => {
                self.state = EntitlementState::Expired;
                self.expires_at = Some(chrono::Utc::now());
                self.last_state_change = chrono::Utc::now();
                Ok(())
            }
        }
    }

    /// Try to use quota (fails if paused, expired, or quota exceeded)
    pub fn try_use_quota(&mut self, bytes: u64) -> Result<(), String> {
        // State check
        match self.state {
            EntitlementState::Paused => return Err("Entitlement is paused".to_string()),
            EntitlementState::Expired => return Err("Entitlement is expired".to_string()),
            EntitlementState::Active => {}
        }

        // Quota check
        if self.quota.bytes_used_this_month + bytes > self.tier.quota_bytes_per_month() {
            return Err(format!(
                "Quota exceeded: would use {} bytes, limit is {} bytes",
                self.quota.bytes_used_this_month + bytes,
                self.tier.quota_bytes_per_month()
            ));
        }

        self.quota.add_usage(bytes);
        Ok(())
    }

    /// Get remaining quota in bytes
    pub fn remaining_quota(&self) -> u64 {
        let limit = self.tier.quota_bytes_per_month();
        if self.quota.bytes_used_this_month > limit {
            0
        } else {
            limit - self.quota.bytes_used_this_month
        }
    }

    /// Check if entitlement is usable (active and not expired)
    pub fn is_usable(&self) -> bool {
        self.state == EntitlementState::Active && self.expires_at.is_none()
    }
}

/// Entitlement manager: manages all customer entitlements
pub struct EntitlementManager {
    /// Entitlements indexed by ID
    entitlements: HashMap<String, Entitlement>,
    /// Entitlements indexed by customer
    by_customer: HashMap<String, Vec<String>>,
}

impl EntitlementManager {
    pub fn new() -> Self {
        Self {
            entitlements: HashMap::new(),
            by_customer: HashMap::new(),
        }
    }

    /// Create a new entitlement for customer
    pub fn create_entitlement(&mut self, customer_id: &str, tier: SkuTier) -> String {
        let ent = Entitlement::new(customer_id, tier);
        let id = ent.id.clone();
        self.entitlements.insert(id.clone(), ent);
        self.by_customer
            .entry(customer_id.to_string())
            .or_insert_with(Vec::new)
            .push(id.clone());
        id
    }

    /// Get entitlement by ID
    pub fn get(&self, id: &str) -> Option<&Entitlement> {
        self.entitlements.get(id)
    }

    /// Get mutable entitlement
    pub fn get_mut(&mut self, id: &str) -> Option<&mut Entitlement> {
        self.entitlements.get_mut(id)
    }

    /// Get entitlements for customer
    pub fn get_by_customer(&self, customer_id: &str) -> Vec<&Entitlement> {
        self.by_customer
            .get(customer_id)
            .map(|ids| {
                ids.iter()
                    .filter_map(|id| self.entitlements.get(id))
                    .collect()
            })
            .unwrap_or_default()
    }

    /// Get count of entitlements
    pub fn count(&self) -> usize {
        self.entitlements.len()
    }

    /// Get active entitlements for customer
    pub fn active_for_customer(&self, customer_id: &str) -> Vec<&Entitlement> {
        self.get_by_customer(customer_id)
            .into_iter()
            .filter(|e| e.state == EntitlementState::Active)
            .collect()
    }
}

impl Default for EntitlementManager {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    // Test 1: Create new entitlement in Active state
    #[test]
    fn test_create_new_entitlement() {
        // Arrange
        let customer_id = "customer-123";

        // Act
        let ent = Entitlement::new(customer_id, SkuTier::Professional);

        // Assert: State verification
        assert_eq!(ent.customer_id, customer_id);
        assert_eq!(ent.tier, SkuTier::Professional);
        assert_eq!(ent.state, EntitlementState::Active);
        assert!(ent.is_usable());
        assert_eq!(ent.quota.bytes_used_this_month, 0);
        assert_eq!(ent.remaining_quota(), SkuTier::Professional.quota_bytes_per_month());
    }

    // Test 2: State transition Active → Paused
    #[test]
    fn test_pause_active_entitlement() {
        // Arrange
        let mut ent = Entitlement::new("cust-1", SkuTier::Free);
        assert_eq!(ent.state, EntitlementState::Active);

        // Act
        let result = ent.pause();

        // Assert: State changed
        assert!(result.is_ok());
        assert_eq!(ent.state, EntitlementState::Paused);
        assert!(!ent.is_usable());
    }

    // Test 3: State transition Paused → Active
    #[test]
    fn test_resume_paused_entitlement() {
        // Arrange
        let mut ent = Entitlement::new("cust-1", SkuTier::Free);
        ent.pause().unwrap();
        assert_eq!(ent.state, EntitlementState::Paused);

        // Act
        let result = ent.resume();

        // Assert: Back to Active
        assert!(result.is_ok());
        assert_eq!(ent.state, EntitlementState::Active);
        assert!(ent.is_usable());
    }

    // Test 4: Cannot pause already paused entitlement
    #[test]
    fn test_pause_already_paused_entitlement() {
        // Arrange
        let mut ent = Entitlement::new("cust-1", SkuTier::Free);
        ent.pause().unwrap();

        // Act
        let result = ent.pause();

        // Assert: Error, state unchanged
        assert!(result.is_err());
        assert_eq!(ent.state, EntitlementState::Paused);
    }

    // Test 5: Cannot use quota when paused
    #[test]
    fn test_cannot_use_quota_when_paused() {
        // Arrange
        let mut ent = Entitlement::new("cust-1", SkuTier::Professional);
        ent.pause().unwrap();
        assert_eq!(ent.state, EntitlementState::Paused);

        // Act
        let result = ent.try_use_quota(1024);

        // Assert: Fails with paused error
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("paused"));

        // State: Quota unchanged
        assert_eq!(ent.quota.bytes_used_this_month, 0);
    }

    // Test 6: Quota tracking for Free tier
    #[test]
    fn test_quota_tracking_free_tier() {
        // Arrange
        let mut ent = Entitlement::new("cust-1", SkuTier::Free);
        let quota_limit = SkuTier::Free.quota_bytes_per_month();
        let usage = quota_limit / 2; // Use half quota

        // Act
        let result = ent.try_use_quota(usage);

        // Assert
        assert!(result.is_ok());
        assert_eq!(ent.quota.bytes_used_this_month, usage);
        assert_eq!(ent.remaining_quota(), quota_limit - usage);
    }

    // Test 7: Quota enforcement - exceeding limit
    #[test]
    fn test_quota_enforcement_exceeds_limit() {
        // Arrange
        let mut ent = Entitlement::new("cust-1", SkuTier::Free);
        let quota_limit = SkuTier::Free.quota_bytes_per_month();

        // Act: Try to use more than limit
        let result = ent.try_use_quota(quota_limit + 1);

        // Assert: Fails with quota error
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("Quota exceeded"));

        // State: Quota unchanged
        assert_eq!(ent.quota.bytes_used_this_month, 0);
    }

    // Test 8: Multiple quota uses accumulate
    #[test]
    fn test_multiple_quota_uses() {
        // Arrange
        let mut ent = Entitlement::new("cust-1", SkuTier::Professional);
        let use1 = 10 * 1024 * 1024; // 10MB
        let use2 = 20 * 1024 * 1024; // 20MB
        let use3 = 15 * 1024 * 1024; // 15MB

        // Act
        ent.try_use_quota(use1).unwrap();
        ent.try_use_quota(use2).unwrap();
        ent.try_use_quota(use3).unwrap();

        // Assert: Accumulates
        assert_eq!(ent.quota.bytes_used_this_month, use1 + use2 + use3);

        // Remaining quota is correct
        let expected_remaining =
            SkuTier::Professional.quota_bytes_per_month() - (use1 + use2 + use3);
        assert_eq!(ent.remaining_quota(), expected_remaining);
    }

    // Test 9: Expire entitlement
    #[test]
    fn test_expire_entitlement() {
        // Arrange
        let mut ent = Entitlement::new("cust-1", SkuTier::Professional);
        assert_eq!(ent.state, EntitlementState::Active);
        assert!(ent.is_usable());

        // Act
        let result = ent.expire();

        // Assert
        assert!(result.is_ok());
        assert_eq!(ent.state, EntitlementState::Expired);
        assert!(!ent.is_usable());
        assert!(ent.expires_at.is_some());
    }

    // Test 10: Cannot use quota when expired
    #[test]
    fn test_cannot_use_quota_when_expired() {
        // Arrange
        let mut ent = Entitlement::new("cust-1", SkuTier::Free);
        ent.expire().unwrap();

        // Act
        let result = ent.try_use_quota(1024);

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().contains("expired"));
    }

    // Test 11: Cannot resume expired entitlement
    #[test]
    fn test_cannot_resume_expired_entitlement() {
        // Arrange
        let mut ent = Entitlement::new("cust-1", SkuTier::Free);
        ent.expire().unwrap();

        // Act
        let result = ent.resume();

        // Assert
        assert!(result.is_err());
        assert_eq!(ent.state, EntitlementState::Expired);
    }

    // Test 12: Manager creates and stores entitlements
    #[test]
    fn test_entitlement_manager_create() {
        // Arrange
        let mut mgr = EntitlementManager::new();

        // Act
        let id = mgr.create_entitlement("cust-1", SkuTier::Professional);

        // Assert: Stored and retrievable
        assert!(mgr.get(&id).is_some());
        let ent = mgr.get(&id).unwrap();
        assert_eq!(ent.customer_id, "cust-1");
        assert_eq!(ent.tier, SkuTier::Professional);
    }

    // Test 13: Manager retrieves by customer
    #[test]
    fn test_manager_get_by_customer() {
        // Arrange
        let mut mgr = EntitlementManager::new();
        let _id1 = mgr.create_entitlement("cust-1", SkuTier::Free);
        let _id2 = mgr.create_entitlement("cust-1", SkuTier::Professional);
        mgr.create_entitlement("cust-2", SkuTier::Enterprise);

        // Act
        let cust1_ents = mgr.get_by_customer("cust-1");

        // Assert: Only customer-1's entitlements
        assert_eq!(cust1_ents.len(), 2);
        assert!(cust1_ents.iter().all(|e| e.customer_id == "cust-1"));
    }

    // Test 14: Manager counts active entitlements
    #[test]
    fn test_manager_count_active() {
        // Arrange
        let mut mgr = EntitlementManager::new();
        let id1 = mgr.create_entitlement("cust-1", SkuTier::Free);
        let id2 = mgr.create_entitlement("cust-1", SkuTier::Professional);

        // Act: Pause one
        mgr.get_mut(&id1).unwrap().pause().unwrap();

        // Assert
        let active = mgr.active_for_customer("cust-1");
        assert_eq!(active.len(), 1, "Only one should be active");
        assert_eq!(active[0].id, id2);
    }

    // Test 15: Enterprise tier has unlimited quota
    #[test]
    fn test_enterprise_unlimited_quota() {
        // Arrange
        let mut ent = Entitlement::new("cust-1", SkuTier::Enterprise);

        // Act: Use massive amounts
        for _ in 0..1000 {
            assert!(ent.try_use_quota(1024 * 1024 * 1024).is_ok()); // 1GB
        }

        // Assert: Still succeeds (unlimited)
        assert_eq!(ent.remaining_quota(), u64::MAX - ent.quota.bytes_used_this_month);
    }

    // Test 16: Full state transition cycle
    #[test]
    fn test_full_lifecycle_cycle() {
        // Arrange
        let mut ent = Entitlement::new("cust-1", SkuTier::Professional);

        // Act & Assert: Active → Paused → Active → Expired
        assert_eq!(ent.state, EntitlementState::Active);

        ent.pause().unwrap();
        assert_eq!(ent.state, EntitlementState::Paused);

        ent.resume().unwrap();
        assert_eq!(ent.state, EntitlementState::Active);

        ent.expire().unwrap();
        assert_eq!(ent.state, EntitlementState::Expired);

        // Cannot change state after expired
        assert!(ent.resume().is_err());
    }

    // Test 17: Quota usage via is_usable check
    #[test]
    fn test_is_usable_reflects_state() {
        // Arrange
        let mut ent = Entitlement::new("cust-1", SkuTier::Free);

        // Active = usable
        assert!(ent.is_usable());

        // Paused = not usable
        ent.pause().unwrap();
        assert!(!ent.is_usable());

        // Resume = usable
        ent.resume().unwrap();
        assert!(ent.is_usable());

        // Expired = not usable
        ent.expire().unwrap();
        assert!(!ent.is_usable());
    }

    // Test 18: Manager maintains customer isolation
    #[test]
    fn test_manager_customer_isolation() {
        // Arrange
        let mut mgr = EntitlementManager::new();
        mgr.create_entitlement("cust-1", SkuTier::Free);
        mgr.create_entitlement("cust-2", SkuTier::Professional);
        mgr.create_entitlement("cust-3", SkuTier::Enterprise);

        // Act
        let total = mgr.count();
        let cust1 = mgr.get_by_customer("cust-1").len();
        let cust2 = mgr.get_by_customer("cust-2").len();
        let cust3 = mgr.get_by_customer("cust-3").len();

        // Assert: Each customer sees only their own
        assert_eq!(total, 3);
        assert_eq!(cust1, 1);
        assert_eq!(cust2, 1);
        assert_eq!(cust3, 1);
    }

    // Test 19: Tier-specific quota limits
    #[test]
    fn test_tier_specific_quotas() {
        // Arrange
        let free = Entitlement::new("c1", SkuTier::Free);
        let pro = Entitlement::new("c2", SkuTier::Professional);
        let ent = Entitlement::new("c3", SkuTier::Enterprise);

        // Assert: Each has correct limit
        assert_eq!(free.remaining_quota(), 10 * 1024 * 1024 * 1024); // 10GB
        assert_eq!(pro.remaining_quota(), 100 * 1024 * 1024 * 1024); // 100GB
        assert_eq!(ent.remaining_quota(), u64::MAX); // Unlimited
    }

    // Test 20: State change is timestamped
    #[test]
    fn test_state_change_timestamps() {
        // Arrange
        let mut ent = Entitlement::new("cust-1", SkuTier::Free);
        let created_at = ent.last_state_change;

        // Act: Change state
        std::thread::sleep(std::time::Duration::from_millis(10));
        ent.pause().unwrap();
        let paused_at = ent.last_state_change;

        // Assert: Timestamps differ
        assert!(paused_at > created_at);
    }
}
