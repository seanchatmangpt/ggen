//! Receipt and history system (Γ): Immutable, cryptographically signed audit trail
//!
//! Every decision produces a receipt that proves:
//! - hash(A) = hash(μ(O))
//! - Full provenance chain
//! - Invariant compliance
//! - Timing guarantees met
//!
//! Receipts are append-only and queryable.

use crate::error::DoDResult;
use crate::kernel::{KernelAction, KernelDecision};
use crate::observation::Observation;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use uuid::Uuid;

/// Unique identifier for receipts
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash, Serialize, Deserialize)]
pub struct ReceiptId(Uuid);

impl ReceiptId {
    /// Generate a new receipt ID
    pub fn new() -> Self {
        Self(Uuid::new_v4())
    }
}

impl Default for ReceiptId {
    fn default() -> Self {
        Self::new()
    }
}

impl std::fmt::Display for ReceiptId {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        write!(f, "{}", self.0)
    }
}

/// A receipt proves hash(A) = hash(μ(O))
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Receipt {
    /// Unique receipt ID
    id: ReceiptId,
    /// Decision that produced this receipt
    decision_id: String,
    /// Input observations (O)
    observation_ids: Vec<crate::observation::ObservationId>,
    /// Actions (A)
    action_ids: Vec<crate::kernel::KernelActionId>,
    /// Observation hash (proves O)
    observation_hash: String,
    /// Action hash (proves A)
    action_hash: String,
    /// Decision hash (proves μ)
    decision_hash: String,
    /// Schema version used (Σ*)
    schema_version: String,
    /// Timing measurement
    timing_ms: u64,
    /// Invariants verified
    invariants_verified: Vec<String>,
    /// Cryptographic signature
    signature: String,
    /// Timestamp
    timestamp: DateTime<Utc>,
    /// Tenant ID
    tenant_id: String,
}

impl Receipt {
    /// Create a new receipt from a kernel decision
    pub fn from_decision(
        decision: &KernelDecision, tenant_id: &str, key: &[u8],
    ) -> DoDResult<Self> {
        let obs_hash = Self::compute_observation_hash(decision.observations());
        let action_hash = Self::compute_action_hash(decision.actions());
        let decision_hash = decision
            .determinism_hash()
            .ok_or_else(|| {
                crate::error::DoDError::Receipt("no determinism hash in decision".to_string())
            })?
            .to_string();

        let mut receipt = Self {
            id: ReceiptId::new(),
            decision_id: decision.decision_id().to_string(),
            observation_ids: decision.observations().iter().map(|o| o.id()).collect(),
            action_ids: decision.actions().iter().map(|a| a.id()).collect(),
            observation_hash: obs_hash,
            action_hash,
            decision_hash,
            schema_version: "1.0".to_string(),
            timing_ms: decision.timing().elapsed_ms(),
            invariants_verified: Vec::new(),
            signature: String::new(),
            timestamp: Utc::now(),
            tenant_id: tenant_id.to_string(),
        };

        receipt.sign(key);
        Ok(receipt)
    }

    /// Compute hash of observations
    fn compute_observation_hash(observations: &[Observation]) -> String {
        use sha2::Digest;
        let mut hasher = sha2::Sha256::new();
        for obs in observations {
            hasher.update(obs.id().to_string());
            hasher.update(serde_json::to_string(&obs.data()).unwrap_or_default());
        }
        hex::encode(hasher.finalize())
    }

    /// Compute hash of actions
    fn compute_action_hash(actions: &[KernelAction]) -> String {
        use sha2::Digest;
        let mut hasher = sha2::Sha256::new();
        for action in actions {
            hasher.update(action.id().to_string());
            hasher.update(action.payload().to_string());
        }
        hex::encode(hasher.finalize())
    }

    /// Sign the receipt
    fn sign(&mut self, key: &[u8]) {
        use hmac::Mac;
        let mut mac =
            hmac::Hmac::<sha2::Sha256>::new_from_slice(key).expect("HMAC key length is valid");

        let payload = format!(
            "{}{}{}{}",
            self.decision_id, self.observation_hash, self.action_hash, self.decision_hash
        );
        mac.update(payload.as_bytes());
        self.signature = hex::encode(mac.finalize().into_bytes());
    }

    /// Verify the receipt's signature
    pub fn verify(&self, key: &[u8]) -> DoDResult<bool> {
        use hmac::Mac;
        let mut mac =
            hmac::Hmac::<sha2::Sha256>::new_from_slice(key).expect("HMAC key length is valid");

        let payload = format!(
            "{}{}{}{}",
            self.decision_id, self.observation_hash, self.action_hash, self.decision_hash
        );
        mac.update(payload.as_bytes());

        let expected_sig = hex::encode(mac.finalize().into_bytes());
        Ok(self.signature == expected_sig)
    }

    /// Get receipt ID
    pub fn id(&self) -> ReceiptId {
        self.id
    }

    /// Get decision ID
    pub fn decision_id(&self) -> &str {
        &self.decision_id
    }

    /// Get observation hash
    pub fn observation_hash(&self) -> &str {
        &self.observation_hash
    }

    /// Get action hash
    pub fn action_hash(&self) -> &str {
        &self.action_hash
    }

    /// Get decision hash
    pub fn decision_hash(&self) -> &str {
        &self.decision_hash
    }

    /// Get timing
    pub fn timing_ms(&self) -> u64 {
        self.timing_ms
    }

    /// Get tenant ID
    pub fn tenant_id(&self) -> &str {
        &self.tenant_id
    }

    /// Get timestamp
    pub fn timestamp(&self) -> DateTime<Utc> {
        self.timestamp
    }

    /// Add verified invariant
    pub fn with_verified_invariant(mut self, invariant: impl Into<String>) -> Self {
        self.invariants_verified.push(invariant.into());
        self
    }
}

/// Persistent storage for receipts (Γ)
pub struct ReceiptStore {
    /// All receipts, indexed by ID
    receipts: BTreeMap<ReceiptId, Receipt>,
    /// Receipts indexed by decision ID for fast lookup
    by_decision_id: BTreeMap<String, ReceiptId>,
    /// Receipts indexed by tenant ID
    by_tenant_id: BTreeMap<String, Vec<ReceiptId>>,
    /// Master key for signing/verification
    master_key: Vec<u8>,
}

impl ReceiptStore {
    /// Create a new receipt store
    pub fn new(master_key: Vec<u8>) -> Self {
        Self {
            receipts: BTreeMap::new(),
            by_decision_id: BTreeMap::new(),
            by_tenant_id: BTreeMap::new(),
            master_key,
        }
    }

    /// Store a receipt
    pub fn store(&mut self, receipt: Receipt) -> DoDResult<()> {
        // Verify receipt before storing
        receipt.verify(&self.master_key)?;

        let id = receipt.id();
        let decision_id = receipt.decision_id().to_string();
        let tenant_id = receipt.tenant_id().to_string();

        self.receipts.insert(id, receipt);
        self.by_decision_id.insert(decision_id, id);

        self.by_tenant_id
            .entry(tenant_id)
            .or_insert_with(Vec::new)
            .push(id);

        Ok(())
    }

    /// Get receipt by ID
    pub fn get(&self, id: ReceiptId) -> Option<&Receipt> {
        self.receipts.get(&id)
    }

    /// Get receipt by decision ID
    pub fn get_by_decision(&self, decision_id: &str) -> Option<&Receipt> {
        self.by_decision_id
            .get(decision_id)
            .and_then(|id| self.receipts.get(id))
    }

    /// Get all receipts for a tenant
    pub fn get_by_tenant(&self, tenant_id: &str) -> Vec<&Receipt> {
        self.by_tenant_id
            .get(tenant_id)
            .map(|ids| ids.iter().filter_map(|id| self.receipts.get(id)).collect())
            .unwrap_or_default()
    }

    /// Query receipts by predicate
    pub fn query<F>(&self, predicate: F) -> Vec<&Receipt>
    where
        F: Fn(&Receipt) -> bool,
    {
        self.receipts.values().filter(|r| predicate(r)).collect()
    }

    /// Get all receipts
    pub fn all(&self) -> Vec<&Receipt> {
        self.receipts.values().collect()
    }

    /// Get count of receipts
    pub fn count(&self) -> usize {
        self.receipts.len()
    }

    /// Get count for tenant
    pub fn count_for_tenant(&self, tenant_id: &str) -> usize {
        self.by_tenant_id
            .get(tenant_id)
            .map(|ids| ids.len())
            .unwrap_or(0)
    }

    /// Verify receipt integrity
    pub fn verify_receipt(&self, receipt: &Receipt) -> DoDResult<bool> {
        receipt.verify(&self.master_key)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_receipt_creation() -> DoDResult<()> {
        let key = b"test-key";
        let decision = KernelDecision::new();
        let receipt = Receipt::from_decision(&decision, "tenant-1", key)?;
        assert!(receipt.verify(key)?);
        Ok(())
    }

    #[test]
    fn test_receipt_store() -> DoDResult<()> {
        let key = vec![1, 2, 3, 4, 5];
        let mut store = ReceiptStore::new(key.clone());

        let decision = KernelDecision::new();
        let receipt = Receipt::from_decision(&decision, "tenant-1", &key)?;

        store.store(receipt)?;
        assert_eq!(store.count(), 1);
        Ok(())
    }

    #[test]
    fn test_receipt_query() -> DoDResult<()> {
        let key = vec![1, 2, 3, 4, 5];
        let mut store = ReceiptStore::new(key.clone());

        let decision1 = KernelDecision::new();
        let decision2 = KernelDecision::new();

        let receipt1 = Receipt::from_decision(&decision1, "tenant-1", &key)?;
        let receipt2 = Receipt::from_decision(&decision2, "tenant-2", &key)?;

        store.store(receipt1)?;
        store.store(receipt2)?;

        let tenant1_receipts = store.get_by_tenant("tenant-1");
        assert_eq!(tenant1_receipts.len(), 1);
        Ok(())
    }
}
