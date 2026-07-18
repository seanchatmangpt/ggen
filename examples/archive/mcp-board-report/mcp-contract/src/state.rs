//! Contract state management
//!
//! Tracks contract state including:
//! - Current epoch
//! - Receipt chain
//! - Kill switch status
//! - Metrics

use chrono::{DateTime, Utc};
use mcp_core::types::Epoch;
use mcp_merkle::MerkleTree;
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Global kill switch status
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct KillSwitchState {
    /// Global kill switch active
    pub global_active: bool,
    /// Disabled contract families
    pub disabled_families: Vec<String>,
    /// Disabled capabilities
    pub disabled_capabilities: Vec<String>,
    /// Revoked epochs
    pub revoked_epochs: Vec<u64>,
}

impl KillSwitchState {
    /// Check if a contract family is killed
    pub fn is_family_killed(&self, family: &str) -> bool {
        self.global_active || self.disabled_families.contains(&family.to_string())
    }

    /// Check if an epoch is revoked
    pub fn is_epoch_revoked(&self, epoch: u64) -> bool {
        self.revoked_epochs.contains(&epoch)
    }

    /// Activate global kill switch
    pub fn activate_global(&mut self) {
        self.global_active = true;
    }

    /// Kill a specific family
    pub fn kill_family(&mut self, family: impl Into<String>) {
        let family = family.into();
        if !self.disabled_families.contains(&family) {
            self.disabled_families.push(family);
        }
    }

    /// Revoke an epoch
    pub fn revoke_epoch(&mut self, epoch: u64) {
        if !self.revoked_epochs.contains(&epoch) {
            self.revoked_epochs.push(epoch);
        }
    }
}

/// Contract runtime state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ContractState {
    /// Contract ID
    pub contract_id: String,
    /// Contract family
    pub contract_family: String,
    /// Current epoch
    pub current_epoch: Epoch,
    /// Receipt chain Merkle tree
    #[serde(skip)]
    pub receipt_tree: MerkleTree,
    /// Receipt chain root (for serialization)
    pub receipt_chain_root: String,
    /// Total operations executed
    pub total_operations: u64,
    /// Total refusals issued
    pub total_refusals: u64,
    /// Kill switch state
    pub kill_switch: KillSwitchState,
    /// Custom state (contract-specific data)
    pub custom_state: HashMap<String, serde_json::Value>,
    /// Created at
    pub created_at: DateTime<Utc>,
    /// Last updated
    pub updated_at: DateTime<Utc>,
}

impl ContractState {
    /// Create a new contract state
    pub fn new(contract_id: impl Into<String>, contract_family: impl Into<String>) -> Self {
        let now = Utc::now();
        Self {
            contract_id: contract_id.into(),
            contract_family: contract_family.into(),
            current_epoch: Epoch::new(1),
            receipt_tree: MerkleTree::new(),
            receipt_chain_root: mcp_core::GENESIS_HASH.to_string(),
            total_operations: 0,
            total_refusals: 0,
            kill_switch: KillSwitchState::default(),
            custom_state: HashMap::new(),
            created_at: now,
            updated_at: now,
        }
    }

    /// Add a receipt to the chain
    pub fn add_receipt(&mut self, receipt_hash: &[u8; 32]) {
        self.receipt_tree.add_leaf(*receipt_hash);
        self.receipt_tree.rebuild();
        self.receipt_chain_root = self.receipt_tree.root_hex()
            .unwrap_or_else(|| mcp_core::GENESIS_HASH.to_string());
        self.total_operations += 1;
        self.updated_at = Utc::now();
    }

    /// Record a refusal
    pub fn add_refusal(&mut self) {
        self.total_refusals += 1;
        self.updated_at = Utc::now();
    }

    /// Rotate to a new epoch
    pub fn rotate_epoch(&mut self) -> &Epoch {
        // Close current epoch
        let root = self.receipt_chain_root.clone();
        self.current_epoch.close(root);

        // Start new epoch
        let new_epoch_number = self.current_epoch.epoch_number + 1;
        self.current_epoch = Epoch::new(new_epoch_number);

        // Reset receipt tree for new epoch
        self.receipt_tree = MerkleTree::new();
        self.receipt_chain_root = mcp_core::GENESIS_HASH.to_string();

        self.updated_at = Utc::now();
        &self.current_epoch
    }

    /// Check if contract can execute (not killed)
    pub fn can_execute(&self) -> bool {
        !self.kill_switch.global_active
            && !self.kill_switch.is_family_killed(&self.contract_family)
            && !self.kill_switch.is_epoch_revoked(self.current_epoch.epoch_number)
            && self.current_epoch.is_active()
    }

    /// Get a custom state value
    pub fn get_custom<T: serde::de::DeserializeOwned>(&self, key: &str) -> Option<T> {
        self.custom_state.get(key)
            .and_then(|v| serde_json::from_value(v.clone()).ok())
    }

    /// Set a custom state value
    pub fn set_custom<T: Serialize>(&mut self, key: impl Into<String>, value: T) {
        if let Ok(json_value) = serde_json::to_value(value) {
            self.custom_state.insert(key.into(), json_value);
            self.updated_at = Utc::now();
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use mcp_core::crypto::hash_sha256_bytes;

    #[test]
    fn test_contract_state_creation() {
        let state = ContractState::new("contract-001", "gpt-4");

        assert_eq!(state.contract_id, "contract-001");
        assert_eq!(state.contract_family, "gpt-4");
        assert_eq!(state.current_epoch.epoch_number, 1);
        assert!(state.can_execute());
    }

    #[test]
    fn test_add_receipt() {
        let mut state = ContractState::new("c1", "family");
        let receipt_hash = hash_sha256_bytes(b"receipt-1");

        state.add_receipt(&receipt_hash);

        assert_eq!(state.total_operations, 1);
        assert_ne!(state.receipt_chain_root, mcp_core::GENESIS_HASH);
    }

    #[test]
    fn test_epoch_rotation() {
        let mut state = ContractState::new("c1", "family");
        let receipt_hash = hash_sha256_bytes(b"receipt-1");
        state.add_receipt(&receipt_hash);

        let old_root = state.receipt_chain_root.clone();
        state.rotate_epoch();

        assert_eq!(state.current_epoch.epoch_number, 2);
        assert_eq!(state.receipt_chain_root, mcp_core::GENESIS_HASH);
        assert_ne!(old_root, state.receipt_chain_root);
    }

    #[test]
    fn test_kill_switch_global() {
        let mut state = ContractState::new("c1", "family");
        assert!(state.can_execute());

        state.kill_switch.activate_global();
        assert!(!state.can_execute());
    }

    #[test]
    fn test_kill_switch_family() {
        let mut state = ContractState::new("c1", "target-family");
        assert!(state.can_execute());

        state.kill_switch.kill_family("target-family");
        assert!(!state.can_execute());

        // Different family should still work
        let other = ContractState::new("c2", "other-family");
        assert!(other.can_execute());
    }

    #[test]
    fn test_custom_state() {
        let mut state = ContractState::new("c1", "family");

        state.set_custom("counter", 42u64);
        state.set_custom("name", "test");

        assert_eq!(state.get_custom::<u64>("counter"), Some(42));
        assert_eq!(state.get_custom::<String>("name"), Some("test".to_string()));
        assert_eq!(state.get_custom::<u64>("missing"), None);
    }
}
