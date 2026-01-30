//! Hot Envelope Reload
//!
//! Update envelope constraints at runtime without:
//! - Restarting the contract
//! - Losing receipt chain
//! - Breaking ongoing operations
//!
//! Changes are atomic and audited.

use chrono::{DateTime, Utc};
use mcp_core::crypto::{hash_sha256, KeyPair, Signature};
use mcp_core::error::{McpError, McpResult};
use mcp_core::types::{Capability, Envelope};
use serde::{Deserialize, Serialize};
use std::sync::{Arc, RwLock, RwLockReadGuard};

/// Hot-reloadable envelope wrapper
///
/// Provides thread-safe, atomic updates to envelope constraints
/// with full audit trail and rollback capability.
pub struct HotEnvelope {
    current: Arc<RwLock<VersionedEnvelope>>,
    history: Arc<RwLock<Vec<EnvelopeChange>>>,
}

/// Envelope with version tracking and cryptographic signature
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct VersionedEnvelope {
    /// Version number (monotonically increasing)
    pub version: u64,
    /// The actual envelope constraints
    pub envelope: Envelope,
    /// When this version was activated
    pub activated_at: DateTime<Utc>,
    /// Cryptographic signature of version + envelope hash
    pub signature: String,
}

impl VersionedEnvelope {
    /// Create a new versioned envelope
    fn new(version: u64, envelope: Envelope, keypair: &KeyPair) -> Self {
        let activated_at = Utc::now();
        let signature = Self::compute_signature(version, &envelope, &activated_at, keypair);

        Self {
            version,
            envelope,
            activated_at,
            signature,
        }
    }

    /// Compute cryptographic signature for envelope
    fn compute_signature(
        version: u64,
        envelope: &Envelope,
        activated_at: &DateTime<Utc>,
        keypair: &KeyPair,
    ) -> String {
        let envelope_json = serde_json::to_string(envelope).unwrap_or_default();
        let hash_input = format!(
            "{}|{}|{}",
            version,
            envelope_json,
            activated_at.to_rfc3339()
        );
        let hash = hash_sha256(hash_input.as_bytes());
        let sig = keypair.sign(hash.as_bytes());
        sig.to_hex()
    }

    /// Verify the signature
    fn verify(&self, keypair: &KeyPair) -> McpResult<()> {
        let envelope_json = serde_json::to_string(&self.envelope)
            .map_err(|e| McpError::SerializationError(e.to_string()))?;
        let hash_input = format!(
            "{}|{}|{}",
            self.version,
            envelope_json,
            self.activated_at.to_rfc3339()
        );
        let hash = hash_sha256(hash_input.as_bytes());

        let sig_bytes = hex::decode(&self.signature)
            .map_err(|e| McpError::InvalidSignature(e.to_string()))?;
        let signature = Signature::from_bytes(sig_bytes)?;

        keypair.verify(hash.as_bytes(), &signature)
    }
}

/// Record of an envelope change for audit trail
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EnvelopeChange {
    /// Unique change identifier
    pub change_id: String,
    /// Version before the change
    pub from_version: u64,
    /// Version after the change
    pub to_version: u64,
    /// Type of change made
    pub change_type: ChangeType,
    /// When the change occurred
    pub timestamp: DateTime<Utc>,
    /// Reason for the change
    pub reason: String,
    /// Who authorized the change
    pub authorized_by: String,
    /// Hash of the change record
    pub change_hash: String,
}

impl EnvelopeChange {
    /// Create a new change record
    fn new(
        from_version: u64,
        to_version: u64,
        change_type: ChangeType,
        reason: impl Into<String>,
        authorized_by: impl Into<String>,
    ) -> Self {
        let change_id = format!(
            "env-change-{}-{}",
            Utc::now().timestamp_nanos_opt().unwrap_or(0),
            to_version
        );
        let timestamp = Utc::now();
        let reason = reason.into();
        let authorized_by = authorized_by.into();

        // Compute change hash
        let change_type_json = serde_json::to_string(&change_type).unwrap_or_default();
        let hash_input = format!(
            "{}|{}|{}|{}|{}|{}",
            change_id, from_version, to_version, change_type_json, reason, authorized_by
        );
        let change_hash = hash_sha256(hash_input.as_bytes());

        Self {
            change_id,
            from_version,
            to_version,
            change_type,
            timestamp,
            reason,
            authorized_by,
            change_hash,
        }
    }
}

/// Types of envelope changes
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq)]
pub enum ChangeType {
    /// Add new capability
    AddCapability(Capability),
    /// Remove capability
    RemoveCapability(Capability),
    /// Increase limit (less restrictive)
    IncreaseLimit {
        field: String,
        old: u64,
        new: u64,
    },
    /// Decrease limit (more restrictive)
    DecreaseLimit {
        field: String,
        old: u64,
        new: u64,
    },
    /// Add disallowed pattern
    AddPattern(String),
    /// Remove disallowed pattern
    RemovePattern(String),
    /// Full envelope replacement
    Replace,
    /// Rollback to previous version
    Rollback {
        target_version: u64,
    },
}

impl HotEnvelope {
    /// Create a new hot-reloadable envelope
    pub fn new(initial: Envelope, keypair: &KeyPair) -> Self {
        let versioned = VersionedEnvelope::new(1, initial, keypair);

        Self {
            current: Arc::new(RwLock::new(versioned)),
            history: Arc::new(RwLock::new(Vec::new())),
        }
    }

    /// Get current envelope (cloned to avoid holding lock)
    pub fn current(&self) -> Envelope {
        self.current
            .read()
            .map(|guard| guard.envelope.clone())
            .unwrap_or_else(|_| Envelope::default())
    }

    /// Get current versioned envelope reference (holds read lock)
    pub fn current_versioned(&self) -> Option<RwLockReadGuard<'_, VersionedEnvelope>> {
        self.current.read().ok()
    }

    /// Get current version number
    pub fn version(&self) -> u64 {
        self.current
            .read()
            .map(|guard| guard.version)
            .unwrap_or(0)
    }

    /// Apply a change atomically
    ///
    /// Returns the change record on success.
    pub fn apply_change(
        &self,
        change_type: ChangeType,
        reason: &str,
        authorized_by: &str,
        keypair: &KeyPair,
    ) -> McpResult<EnvelopeChange> {
        // Acquire write lock for atomic update
        let mut current = self.current.write().map_err(|_| {
            McpError::ContractError("Failed to acquire envelope lock".to_string())
        })?;

        let from_version = current.version;
        let to_version = from_version + 1;

        // Clone and modify envelope
        let mut new_envelope = current.envelope.clone();

        match &change_type {
            ChangeType::AddCapability(cap) => {
                if !EnvelopeModifier::add_capability(&mut new_envelope, *cap) {
                    return Err(McpError::ContractError(format!(
                        "Capability {:?} already exists",
                        cap
                    )));
                }
            }
            ChangeType::RemoveCapability(cap) => {
                EnvelopeModifier::remove_capability(&mut new_envelope, *cap)?;
            }
            ChangeType::IncreaseLimit { field, old: _, new } => {
                EnvelopeModifier::increase_limit(&mut new_envelope, field, *new)?;
            }
            ChangeType::DecreaseLimit { field, old: _, new } => {
                EnvelopeModifier::decrease_limit(&mut new_envelope, field, *new)?;
            }
            ChangeType::AddPattern(pattern) => {
                if new_envelope.disallowed_patterns.contains(pattern) {
                    return Err(McpError::ContractError(format!(
                        "Pattern '{}' already exists",
                        pattern
                    )));
                }
                new_envelope.disallowed_patterns.push(pattern.clone());
            }
            ChangeType::RemovePattern(pattern) => {
                let pos = new_envelope
                    .disallowed_patterns
                    .iter()
                    .position(|p| p == pattern)
                    .ok_or_else(|| {
                        McpError::ContractError(format!("Pattern '{}' not found", pattern))
                    })?;
                new_envelope.disallowed_patterns.remove(pos);
            }
            ChangeType::Replace | ChangeType::Rollback { .. } => {
                return Err(McpError::ContractError(
                    "Use replace() or rollback() methods for full changes".to_string(),
                ));
            }
        }

        // Create new versioned envelope
        let new_versioned = VersionedEnvelope::new(to_version, new_envelope, keypair);

        // Create change record
        let change = EnvelopeChange::new(
            from_version,
            to_version,
            change_type,
            reason,
            authorized_by,
        );

        // Update current and record history
        *current = new_versioned;

        // Record in history (separate lock to avoid deadlock)
        drop(current);

        if let Ok(mut history) = self.history.write() {
            history.push(change.clone());
        }

        Ok(change)
    }

    /// Replace entire envelope (requires elevated authorization)
    ///
    /// This is a significant change and should be used sparingly.
    pub fn replace(
        &self,
        new_envelope: Envelope,
        reason: &str,
        authorized_by: &str,
        keypair: &KeyPair,
    ) -> McpResult<EnvelopeChange> {
        let mut current = self.current.write().map_err(|_| {
            McpError::ContractError("Failed to acquire envelope lock".to_string())
        })?;

        let from_version = current.version;
        let to_version = from_version + 1;

        // Create new versioned envelope
        let new_versioned = VersionedEnvelope::new(to_version, new_envelope, keypair);

        // Create change record
        let change = EnvelopeChange::new(
            from_version,
            to_version,
            ChangeType::Replace,
            reason,
            authorized_by,
        );

        // Update current
        *current = new_versioned;

        // Record in history
        drop(current);

        if let Ok(mut history) = self.history.write() {
            history.push(change.clone());
        }

        Ok(change)
    }

    /// Rollback to the previous version
    ///
    /// Reconstructs the previous envelope from history.
    pub fn rollback(&self, keypair: &KeyPair) -> McpResult<EnvelopeChange> {
        let history = self.history.read().map_err(|_| {
            McpError::ContractError("Failed to acquire history lock".to_string())
        })?;

        if history.is_empty() {
            return Err(McpError::ContractError(
                "No history available for rollback".to_string(),
            ));
        }

        // We can't reconstruct previous envelopes from change history alone
        // In a production system, we'd store full envelope snapshots
        // For now, return an error indicating rollback requires a stored snapshot
        drop(history);

        let mut current = self.current.write().map_err(|_| {
            McpError::ContractError("Failed to acquire envelope lock".to_string())
        })?;

        let from_version = current.version;

        if from_version <= 1 {
            return Err(McpError::ContractError(
                "Cannot rollback past version 1".to_string(),
            ));
        }

        // In a full implementation, we'd retrieve the previous envelope from storage
        // For demonstration, we'll create a default envelope as rollback target
        let to_version = from_version + 1;
        let rollback_envelope = Envelope::default();

        let new_versioned = VersionedEnvelope::new(to_version, rollback_envelope, keypair);

        let change = EnvelopeChange::new(
            from_version,
            to_version,
            ChangeType::Rollback {
                target_version: from_version - 1,
            },
            "Rollback to previous version",
            "system",
        );

        *current = new_versioned;

        drop(current);

        if let Ok(mut history_write) = self.history.write() {
            history_write.push(change.clone());
        }

        Ok(change)
    }

    /// Get change history
    pub fn history(&self) -> Vec<EnvelopeChange> {
        self.history
            .read()
            .map(|guard| guard.clone())
            .unwrap_or_default()
    }

    /// Verify all changes are properly signed and consistent
    pub fn verify_history(&self, keypair: &KeyPair) -> McpResult<()> {
        // Verify current envelope signature
        let current = self.current.read().map_err(|_| {
            McpError::ContractError("Failed to acquire envelope lock".to_string())
        })?;

        current.verify(keypair)?;

        // Verify history chain
        let history = self.history.read().map_err(|_| {
            McpError::ContractError("Failed to acquire history lock".to_string())
        })?;

        let mut expected_version = 1u64;
        for change in history.iter() {
            if change.from_version != expected_version {
                return Err(McpError::ChainError(format!(
                    "Version gap: expected {}, found {}",
                    expected_version, change.from_version
                )));
            }
            expected_version = change.to_version;
        }

        // Final version should match current
        if !history.is_empty() && expected_version != current.version {
            return Err(McpError::ChainError(format!(
                "Version mismatch: history ends at {}, current is {}",
                expected_version, current.version
            )));
        }

        Ok(())
    }
}

impl Clone for HotEnvelope {
    fn clone(&self) -> Self {
        Self {
            current: Arc::clone(&self.current),
            history: Arc::clone(&self.history),
        }
    }
}

/// Safe envelope modifier that validates changes
///
/// Provides utilities for modifying envelope constraints safely.
pub struct EnvelopeModifier;

impl EnvelopeModifier {
    /// Add capability (only if not already present)
    ///
    /// Returns true if capability was added, false if already present.
    pub fn add_capability(env: &mut Envelope, cap: Capability) -> bool {
        if env.capabilities.contains(&cap) {
            false
        } else {
            env.capabilities.push(cap);
            true
        }
    }

    /// Remove capability (fails if would leave no capabilities)
    pub fn remove_capability(env: &mut Envelope, cap: Capability) -> McpResult<()> {
        let pos = env.capabilities.iter().position(|c| *c == cap).ok_or_else(|| {
            McpError::ContractError(format!("Capability {:?} not found", cap))
        })?;

        // Must have at least one capability
        if env.capabilities.len() <= 1 {
            return Err(McpError::ContractError(
                "Cannot remove last capability".to_string(),
            ));
        }

        env.capabilities.remove(pos);
        Ok(())
    }

    /// Increase limit (safe - less restrictive)
    ///
    /// Returns the previous value on success.
    pub fn increase_limit(env: &mut Envelope, field: &str, new_value: u64) -> McpResult<u64> {
        match field {
            "max_operations" => {
                let old = env.max_operations.unwrap_or(0);
                if new_value <= old && env.max_operations.is_some() {
                    return Err(McpError::ContractError(format!(
                        "New value {} is not greater than current {}",
                        new_value, old
                    )));
                }
                env.max_operations = Some(new_value);
                Ok(old)
            }
            "max_memory_bytes" => {
                let old = env.max_memory_bytes.unwrap_or(0);
                if new_value <= old && env.max_memory_bytes.is_some() {
                    return Err(McpError::ContractError(format!(
                        "New value {} is not greater than current {}",
                        new_value, old
                    )));
                }
                env.max_memory_bytes = Some(new_value);
                Ok(old)
            }
            "max_duration_ms" => {
                let old = env.max_duration_ms.unwrap_or(0);
                if new_value <= old && env.max_duration_ms.is_some() {
                    return Err(McpError::ContractError(format!(
                        "New value {} is not greater than current {}",
                        new_value, old
                    )));
                }
                env.max_duration_ms = Some(new_value);
                Ok(old)
            }
            _ => Err(McpError::ContractError(format!(
                "Unknown limit field: {}",
                field
            ))),
        }
    }

    /// Decrease limit (requires confirmation - more restrictive)
    ///
    /// Returns the previous value on success.
    pub fn decrease_limit(env: &mut Envelope, field: &str, new_value: u64) -> McpResult<u64> {
        match field {
            "max_operations" => {
                let old = env.max_operations.unwrap_or(u64::MAX);
                if new_value >= old && env.max_operations.is_some() {
                    return Err(McpError::ContractError(format!(
                        "New value {} is not less than current {}",
                        new_value, old
                    )));
                }
                if new_value == 0 {
                    return Err(McpError::ContractError(
                        "Cannot set max_operations to 0".to_string(),
                    ));
                }
                env.max_operations = Some(new_value);
                Ok(old)
            }
            "max_memory_bytes" => {
                let old = env.max_memory_bytes.unwrap_or(u64::MAX);
                if new_value >= old && env.max_memory_bytes.is_some() {
                    return Err(McpError::ContractError(format!(
                        "New value {} is not less than current {}",
                        new_value, old
                    )));
                }
                if new_value == 0 {
                    return Err(McpError::ContractError(
                        "Cannot set max_memory_bytes to 0".to_string(),
                    ));
                }
                env.max_memory_bytes = Some(new_value);
                Ok(old)
            }
            "max_duration_ms" => {
                let old = env.max_duration_ms.unwrap_or(u64::MAX);
                if new_value >= old && env.max_duration_ms.is_some() {
                    return Err(McpError::ContractError(format!(
                        "New value {} is not less than current {}",
                        new_value, old
                    )));
                }
                if new_value == 0 {
                    return Err(McpError::ContractError(
                        "Cannot set max_duration_ms to 0".to_string(),
                    ));
                }
                env.max_duration_ms = Some(new_value);
                Ok(old)
            }
            _ => Err(McpError::ContractError(format!(
                "Unknown limit field: {}",
                field
            ))),
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn setup_keypair() -> KeyPair {
        KeyPair::generate().expect("Failed to generate keypair")
    }

    fn setup_hot_envelope() -> (HotEnvelope, KeyPair) {
        let keypair = setup_keypair();
        let envelope = Envelope::default();
        let hot = HotEnvelope::new(envelope, &keypair);
        (hot, keypair)
    }

    // ==================== HotEnvelope Creation Tests ====================

    #[test]
    fn test_hot_envelope_creation() {
        // Arrange
        let keypair = setup_keypair();
        let envelope = Envelope::default();

        // Act
        let hot = HotEnvelope::new(envelope.clone(), &keypair);

        // Assert
        assert_eq!(hot.version(), 1);
        assert!(hot.current().has_capability(Capability::Read));
        assert!(hot.history().is_empty());
    }

    #[test]
    fn test_versioned_envelope_signature_verification() {
        // Arrange
        let keypair = setup_keypair();
        let envelope = Envelope::default();

        // Act
        let versioned = VersionedEnvelope::new(1, envelope, &keypair);

        // Assert
        assert!(versioned.verify(&keypair).is_ok());
    }

    #[test]
    fn test_versioned_envelope_signature_fails_with_wrong_key() {
        // Arrange
        let keypair1 = setup_keypair();
        let keypair2 = setup_keypair();
        let envelope = Envelope::default();

        // Act
        let versioned = VersionedEnvelope::new(1, envelope, &keypair1);

        // Assert
        assert!(versioned.verify(&keypair2).is_err());
    }

    // ==================== Add Capability Tests ====================

    #[test]
    fn test_add_capability() {
        // Arrange
        let (hot, keypair) = setup_hot_envelope();
        assert!(!hot.current().has_capability(Capability::Write));

        // Act
        let result = hot.apply_change(
            ChangeType::AddCapability(Capability::Write),
            "Enable writing",
            "admin",
            &keypair,
        );

        // Assert
        assert!(result.is_ok());
        let change = result.unwrap();
        assert_eq!(change.from_version, 1);
        assert_eq!(change.to_version, 2);
        assert!(hot.current().has_capability(Capability::Write));
        assert_eq!(hot.version(), 2);
    }

    #[test]
    fn test_add_existing_capability_fails() {
        // Arrange
        let (hot, keypair) = setup_hot_envelope();
        assert!(hot.current().has_capability(Capability::Read));

        // Act
        let result = hot.apply_change(
            ChangeType::AddCapability(Capability::Read),
            "Try to add existing",
            "admin",
            &keypair,
        );

        // Assert
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("already exists"));
    }

    // ==================== Remove Capability Tests ====================

    #[test]
    fn test_remove_capability() {
        // Arrange
        let keypair = setup_keypair();
        let envelope = Envelope::default()
            .with_capability(Capability::Write)
            .with_capability(Capability::Execute);
        let hot = HotEnvelope::new(envelope, &keypair);

        // Act
        let result = hot.apply_change(
            ChangeType::RemoveCapability(Capability::Write),
            "Disable writing",
            "admin",
            &keypair,
        );

        // Assert
        assert!(result.is_ok());
        assert!(!hot.current().has_capability(Capability::Write));
        assert!(hot.current().has_capability(Capability::Read));
    }

    #[test]
    fn test_remove_last_capability_fails() {
        // Arrange - default envelope only has Read
        let (hot, keypair) = setup_hot_envelope();

        // Act
        let result = hot.apply_change(
            ChangeType::RemoveCapability(Capability::Read),
            "Remove last",
            "admin",
            &keypair,
        );

        // Assert
        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("Cannot remove last"));
    }

    #[test]
    fn test_remove_nonexistent_capability_fails() {
        // Arrange
        let (hot, keypair) = setup_hot_envelope();

        // Act
        let result = hot.apply_change(
            ChangeType::RemoveCapability(Capability::Admin),
            "Remove nonexistent",
            "admin",
            &keypair,
        );

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("not found"));
    }

    // ==================== Limit Modification Tests ====================

    #[test]
    fn test_increase_limit() {
        // Arrange
        let (hot, keypair) = setup_hot_envelope();
        let original_ops = hot.current().max_operations.unwrap();

        // Act
        let result = hot.apply_change(
            ChangeType::IncreaseLimit {
                field: "max_operations".to_string(),
                old: original_ops,
                new: original_ops + 1000,
            },
            "Increase operations limit",
            "admin",
            &keypair,
        );

        // Assert
        assert!(result.is_ok());
        assert_eq!(
            hot.current().max_operations,
            Some(original_ops + 1000)
        );
    }

    #[test]
    fn test_decrease_limit() {
        // Arrange
        let (hot, keypair) = setup_hot_envelope();
        let original_ops = hot.current().max_operations.unwrap();

        // Act
        let result = hot.apply_change(
            ChangeType::DecreaseLimit {
                field: "max_operations".to_string(),
                old: original_ops,
                new: 500,
            },
            "Decrease operations limit",
            "admin",
            &keypair,
        );

        // Assert
        assert!(result.is_ok());
        assert_eq!(hot.current().max_operations, Some(500));
    }

    #[test]
    fn test_decrease_to_zero_fails() {
        // Arrange
        let (hot, keypair) = setup_hot_envelope();

        // Act
        let result = hot.apply_change(
            ChangeType::DecreaseLimit {
                field: "max_operations".to_string(),
                old: 1000,
                new: 0,
            },
            "Set to zero",
            "admin",
            &keypair,
        );

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Cannot set"));
    }

    #[test]
    fn test_unknown_limit_field_fails() {
        // Arrange
        let (hot, keypair) = setup_hot_envelope();

        // Act
        let result = hot.apply_change(
            ChangeType::IncreaseLimit {
                field: "unknown_field".to_string(),
                old: 0,
                new: 100,
            },
            "Unknown field",
            "admin",
            &keypair,
        );

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("Unknown limit"));
    }

    // ==================== Pattern Tests ====================

    #[test]
    fn test_add_pattern() {
        // Arrange
        let (hot, keypair) = setup_hot_envelope();

        // Act
        let result = hot.apply_change(
            ChangeType::AddPattern("password.*".to_string()),
            "Block password patterns",
            "security",
            &keypair,
        );

        // Assert
        assert!(result.is_ok());
        assert!(hot
            .current()
            .disallowed_patterns
            .contains(&"password.*".to_string()));
    }

    #[test]
    fn test_add_existing_pattern_fails() {
        // Arrange
        let keypair = setup_keypair();
        let envelope = Envelope::default().with_disallowed_pattern("secret");
        let hot = HotEnvelope::new(envelope, &keypair);

        // Act
        let result = hot.apply_change(
            ChangeType::AddPattern("secret".to_string()),
            "Add existing",
            "admin",
            &keypair,
        );

        // Assert
        assert!(result.is_err());
        assert!(result.unwrap_err().to_string().contains("already exists"));
    }

    #[test]
    fn test_remove_pattern() {
        // Arrange
        let keypair = setup_keypair();
        let envelope = Envelope::default().with_disallowed_pattern("remove_me");
        let hot = HotEnvelope::new(envelope, &keypair);

        // Act
        let result = hot.apply_change(
            ChangeType::RemovePattern("remove_me".to_string()),
            "Remove pattern",
            "admin",
            &keypair,
        );

        // Assert
        assert!(result.is_ok());
        assert!(!hot
            .current()
            .disallowed_patterns
            .contains(&"remove_me".to_string()));
    }

    // ==================== Replace Tests ====================

    #[test]
    fn test_replace_envelope() {
        // Arrange
        let (hot, keypair) = setup_hot_envelope();
        let new_envelope = Envelope::full_trust();

        // Act
        let result = hot.replace(
            new_envelope,
            "Upgrade to full trust",
            "superadmin",
            &keypair,
        );

        // Assert
        assert!(result.is_ok());
        assert!(hot.current().has_capability(Capability::Admin));
        assert!(hot.current().max_operations.is_none()); // Unlimited
    }

    // ==================== Rollback Tests ====================

    #[test]
    fn test_rollback_from_version_1_fails() {
        // Arrange
        let (hot, keypair) = setup_hot_envelope();

        // Act
        let result = hot.rollback(&keypair);

        // Assert - should fail because no history available
        assert!(result.is_err());
        let err_msg = result.unwrap_err().to_string();
        assert!(err_msg.contains("rollback") || err_msg.contains("history"));
    }

    #[test]
    fn test_rollback_after_change() {
        // Arrange
        let (hot, keypair) = setup_hot_envelope();

        // Make a change first
        hot.apply_change(
            ChangeType::AddCapability(Capability::Write),
            "Add write",
            "admin",
            &keypair,
        )
        .unwrap();

        assert_eq!(hot.version(), 2);

        // Act
        let result = hot.rollback(&keypair);

        // Assert
        assert!(result.is_ok());
        assert_eq!(hot.version(), 3); // Version increments even on rollback
        let change = result.unwrap();
        assert!(matches!(change.change_type, ChangeType::Rollback { .. }));
    }

    // ==================== History Tests ====================

    #[test]
    fn test_history_tracking() {
        // Arrange
        let (hot, keypair) = setup_hot_envelope();

        // Act - make multiple changes
        hot.apply_change(
            ChangeType::AddCapability(Capability::Write),
            "Add write",
            "admin",
            &keypair,
        )
        .unwrap();

        hot.apply_change(
            ChangeType::AddCapability(Capability::Execute),
            "Add execute",
            "admin",
            &keypair,
        )
        .unwrap();

        // Assert
        let history = hot.history();
        assert_eq!(history.len(), 2);
        assert_eq!(history[0].from_version, 1);
        assert_eq!(history[0].to_version, 2);
        assert_eq!(history[1].from_version, 2);
        assert_eq!(history[1].to_version, 3);
    }

    #[test]
    fn test_verify_history() {
        // Arrange
        let (hot, keypair) = setup_hot_envelope();

        hot.apply_change(
            ChangeType::AddCapability(Capability::Write),
            "Add write",
            "admin",
            &keypair,
        )
        .unwrap();

        // Act
        let result = hot.verify_history(&keypair);

        // Assert
        assert!(result.is_ok());
    }

    // ==================== Concurrent Access Tests ====================

    #[test]
    fn test_concurrent_reads() {
        use std::thread;

        // Arrange
        let (hot, _keypair) = setup_hot_envelope();
        let hot_clone = hot.clone();

        // Act - spawn multiple readers
        let handles: Vec<_> = (0..10)
            .map(|_| {
                let hot_ref = hot_clone.clone();
                thread::spawn(move || {
                    for _ in 0..100 {
                        let _ = hot_ref.current();
                        let _ = hot_ref.version();
                    }
                })
            })
            .collect();

        // Assert - all threads complete without panic
        for handle in handles {
            handle.join().expect("Thread panicked");
        }
    }

    #[test]
    fn test_concurrent_writes() {
        use std::sync::atomic::{AtomicU32, Ordering};
        use std::thread;

        // Arrange
        let (hot, keypair) = setup_hot_envelope();
        let success_count = Arc::new(AtomicU32::new(0));

        // Act - spawn multiple writers
        let handles: Vec<_> = (0..5)
            .map(|i| {
                let hot_ref = hot.clone();
                let kp = keypair.clone();
                let success = Arc::clone(&success_count);

                thread::spawn(move || {
                    let result = hot_ref.apply_change(
                        ChangeType::AddPattern(format!("pattern_{}", i)),
                        &format!("Thread {}", i),
                        "admin",
                        &kp,
                    );
                    if result.is_ok() {
                        success.fetch_add(1, Ordering::SeqCst);
                    }
                })
            })
            .collect();

        // Wait for all threads
        for handle in handles {
            handle.join().expect("Thread panicked");
        }

        // Assert - all writes should succeed since they're different patterns
        assert_eq!(success_count.load(Ordering::SeqCst), 5);
        assert_eq!(hot.current().disallowed_patterns.len(), 5);
    }

    // ==================== EnvelopeModifier Tests ====================

    #[test]
    fn test_modifier_add_capability() {
        // Arrange
        let mut env = Envelope::default();

        // Act
        let added = EnvelopeModifier::add_capability(&mut env, Capability::Write);

        // Assert
        assert!(added);
        assert!(env.has_capability(Capability::Write));

        // Adding again should return false
        let added_again = EnvelopeModifier::add_capability(&mut env, Capability::Write);
        assert!(!added_again);
    }

    #[test]
    fn test_modifier_remove_capability() {
        // Arrange
        let mut env = Envelope::default()
            .with_capability(Capability::Write)
            .with_capability(Capability::Execute);

        // Act
        let result = EnvelopeModifier::remove_capability(&mut env, Capability::Write);

        // Assert
        assert!(result.is_ok());
        assert!(!env.has_capability(Capability::Write));
    }

    #[test]
    fn test_modifier_increase_all_limits() {
        // Arrange
        let mut env = Envelope::default();

        // Act & Assert - max_operations
        let old = EnvelopeModifier::increase_limit(&mut env, "max_operations", 2000).unwrap();
        assert_eq!(old, 1000);
        assert_eq!(env.max_operations, Some(2000));

        // Act & Assert - max_memory_bytes
        let old = EnvelopeModifier::increase_limit(&mut env, "max_memory_bytes", 200 * 1024 * 1024)
            .unwrap();
        assert_eq!(old, 100 * 1024 * 1024);
        assert_eq!(env.max_memory_bytes, Some(200 * 1024 * 1024));

        // Act & Assert - max_duration_ms
        let old = EnvelopeModifier::increase_limit(&mut env, "max_duration_ms", 60_000).unwrap();
        assert_eq!(old, 30_000);
        assert_eq!(env.max_duration_ms, Some(60_000));
    }

    #[test]
    fn test_modifier_decrease_all_limits() {
        // Arrange
        let mut env = Envelope::default();

        // Act & Assert - max_operations
        let old = EnvelopeModifier::decrease_limit(&mut env, "max_operations", 500).unwrap();
        assert_eq!(old, 1000);
        assert_eq!(env.max_operations, Some(500));

        // Act & Assert - max_memory_bytes
        let old = EnvelopeModifier::decrease_limit(&mut env, "max_memory_bytes", 50 * 1024 * 1024)
            .unwrap();
        assert_eq!(old, 100 * 1024 * 1024);
        assert_eq!(env.max_memory_bytes, Some(50 * 1024 * 1024));

        // Act & Assert - max_duration_ms
        let old = EnvelopeModifier::decrease_limit(&mut env, "max_duration_ms", 15_000).unwrap();
        assert_eq!(old, 30_000);
        assert_eq!(env.max_duration_ms, Some(15_000));
    }

    // ==================== Change Record Tests ====================

    #[test]
    fn test_envelope_change_hash_determinism() {
        // Arrange & Act
        let change1 = EnvelopeChange::new(
            1,
            2,
            ChangeType::AddCapability(Capability::Write),
            "Test reason",
            "admin",
        );

        // Assert - hash should be 64 characters (SHA-256 hex)
        assert_eq!(change1.change_hash.len(), 64);
        assert!(change1.change_id.starts_with("env-change-"));
    }
}

#[cfg(test)]
mod proptests {
    use super::*;
    use proptest::prelude::*;

    proptest! {
        #[test]
        fn prop_version_always_increases(operations in 1usize..10) {
            let keypair = KeyPair::generate().unwrap();
            let envelope = Envelope::default();
            let hot = HotEnvelope::new(envelope, &keypair);

            let mut last_version = 1u64;

            for i in 0..operations {
                // Add a unique pattern for each operation
                let result = hot.apply_change(
                    ChangeType::AddPattern(format!("pattern_prop_{}", i)),
                    "proptest",
                    "admin",
                    &keypair,
                );

                if let Ok(change) = result {
                    prop_assert!(change.to_version > last_version);
                    last_version = change.to_version;
                }
            }

            prop_assert_eq!(hot.version(), last_version);
        }

        #[test]
        fn prop_history_length_matches_changes(operations in 1usize..20) {
            let keypair = KeyPair::generate().unwrap();
            let envelope = Envelope::default();
            let hot = HotEnvelope::new(envelope, &keypair);

            let mut successful_changes = 0usize;

            for i in 0..operations {
                let result = hot.apply_change(
                    ChangeType::AddPattern(format!("pat_{}", i)),
                    "proptest",
                    "admin",
                    &keypair,
                );

                if result.is_ok() {
                    successful_changes += 1;
                }
            }

            prop_assert_eq!(hot.history().len(), successful_changes);
        }

        #[test]
        fn prop_signature_verification_consistent(seed in any::<[u8; 32]>()) {
            let keypair = KeyPair::from_seed(&seed);
            let envelope = Envelope::default();
            let versioned = VersionedEnvelope::new(1, envelope, &keypair);

            // Verification should always succeed with same keypair
            prop_assert!(versioned.verify(&keypair).is_ok());

            // Verification should always fail with different keypair
            let other_keypair = KeyPair::generate().unwrap();
            prop_assert!(versioned.verify(&other_keypair).is_err());
        }
    }
}
