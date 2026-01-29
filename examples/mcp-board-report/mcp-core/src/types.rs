//! Core types for MCP+
//!
//! Shared types used across all MCP+ crates.

use crate::crypto::{hash_sha256, KeyPair, Signature};
use crate::error::McpResult;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashMap;

/// Contract capability flags
#[derive(Debug, Clone, Copy, PartialEq, Eq, Hash, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum Capability {
    /// Read data
    Read,
    /// Write data
    Write,
    /// Execute operations
    Execute,
    /// Manage resources
    Manage,
    /// Admin operations
    Admin,
}

impl Capability {
    pub fn as_str(&self) -> &'static str {
        match self {
            Self::Read => "read",
            Self::Write => "write",
            Self::Execute => "execute",
            Self::Manage => "manage",
            Self::Admin => "admin",
        }
    }
}

/// Envelope constraints that bound contract behavior
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Envelope {
    /// Maximum operations per epoch
    pub max_operations: Option<u64>,
    /// Maximum memory usage in bytes
    pub max_memory_bytes: Option<u64>,
    /// Maximum execution time in milliseconds
    pub max_duration_ms: Option<u64>,
    /// Allowed capabilities
    pub capabilities: Vec<Capability>,
    /// Allowed output types
    pub allowed_outputs: Vec<String>,
    /// Disallowed patterns (regex)
    pub disallowed_patterns: Vec<String>,
    /// Custom constraints
    pub custom_constraints: HashMap<String, serde_json::Value>,
}

impl Default for Envelope {
    fn default() -> Self {
        Self {
            max_operations: Some(1000),
            max_memory_bytes: Some(100 * 1024 * 1024), // 100 MB
            max_duration_ms: Some(30_000),              // 30 seconds
            capabilities: vec![Capability::Read],
            allowed_outputs: vec![],
            disallowed_patterns: vec![],
            custom_constraints: HashMap::new(),
        }
    }
}

impl Envelope {
    /// Create an envelope with full capabilities (for trusted contracts)
    pub fn full_trust() -> Self {
        Self {
            max_operations: None,
            max_memory_bytes: None,
            max_duration_ms: None,
            capabilities: vec![
                Capability::Read,
                Capability::Write,
                Capability::Execute,
                Capability::Manage,
                Capability::Admin,
            ],
            allowed_outputs: vec![],
            disallowed_patterns: vec![],
            custom_constraints: HashMap::new(),
        }
    }

    /// Create a minimal read-only envelope
    pub fn read_only() -> Self {
        Self {
            capabilities: vec![Capability::Read],
            ..Default::default()
        }
    }

    /// Check if a capability is allowed
    pub fn has_capability(&self, cap: Capability) -> bool {
        self.capabilities.contains(&cap)
    }

    /// Add a capability
    pub fn with_capability(mut self, cap: Capability) -> Self {
        if !self.capabilities.contains(&cap) {
            self.capabilities.push(cap);
        }
        self
    }

    /// Set max operations
    pub fn with_max_operations(mut self, max: u64) -> Self {
        self.max_operations = Some(max);
        self
    }

    /// Add disallowed pattern
    pub fn with_disallowed_pattern(mut self, pattern: impl Into<String>) -> Self {
        self.disallowed_patterns.push(pattern.into());
        self
    }
}

/// Cryptographic receipt for an operation
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Receipt {
    /// Unique receipt ID
    pub receipt_id: String,
    /// Receipt sequence number in the chain
    pub sequence: u64,
    /// Hash of the previous receipt (chain link)
    pub prev_hash: String,
    /// Operation performed
    pub operation: String,
    /// Contract ID
    pub contract_id: String,
    /// Input hash (text-blind: we don't store the actual input)
    pub input_hash: String,
    /// Output hash (text-blind)
    pub output_hash: String,
    /// Execution metrics
    pub metrics: ExecutionMetrics,
    /// Timestamp
    pub timestamp: DateTime<Utc>,
    /// Receipt hash
    pub receipt_hash: String,
    /// Signature
    #[serde(skip_serializing_if = "Option::is_none")]
    pub signature: Option<String>,
}

/// Execution metrics for a receipt
#[derive(Debug, Clone, Default, Serialize, Deserialize)]
pub struct ExecutionMetrics {
    /// Duration in microseconds
    pub duration_us: u64,
    /// Memory used in bytes
    pub memory_bytes: u64,
    /// Number of operations
    pub operations: u64,
    /// Envelope utilization (0.0 - 1.0)
    pub envelope_utilization: f64,
}

impl Receipt {
    /// Create a new receipt
    pub fn new(
        sequence: u64,
        prev_hash: impl Into<String>,
        operation: impl Into<String>,
        contract_id: impl Into<String>,
        input_hash: impl Into<String>,
        output_hash: impl Into<String>,
        metrics: ExecutionMetrics,
    ) -> Self {
        let receipt_id = format!("rcpt-{}-{}", Utc::now().timestamp_nanos_opt().unwrap_or(0), sequence);
        let timestamp = Utc::now();
        let prev_hash = prev_hash.into();
        let operation = operation.into();
        let contract_id = contract_id.into();
        let input_hash = input_hash.into();
        let output_hash = output_hash.into();

        // Compute receipt hash
        let hash_input = format!(
            "{}|{}|{}|{}|{}|{}|{}|{}|{}",
            receipt_id,
            sequence,
            prev_hash,
            operation,
            contract_id,
            input_hash,
            output_hash,
            metrics.duration_us,
            timestamp.to_rfc3339()
        );
        let receipt_hash = hash_sha256(hash_input.as_bytes());

        Self {
            receipt_id,
            sequence,
            prev_hash,
            operation,
            contract_id,
            input_hash,
            output_hash,
            metrics,
            timestamp,
            receipt_hash,
            signature: None,
        }
    }

    /// Sign the receipt
    pub fn sign(&mut self, keypair: &KeyPair) {
        let sig = keypair.sign(self.receipt_hash.as_bytes());
        self.signature = Some(sig.to_hex());
    }

    /// Verify the receipt signature
    pub fn verify(&self, keypair: &KeyPair) -> McpResult<()> {
        let sig_hex = self.signature.as_ref()
            .ok_or_else(|| crate::error::McpError::InvalidSignature("No signature present".to_string()))?;

        let sig_bytes = hex::decode(sig_hex)
            .map_err(|e| crate::error::McpError::InvalidSignature(e.to_string()))?;

        let signature = Signature::from_bytes(sig_bytes)?;
        keypair.verify(self.receipt_hash.as_bytes(), &signature)
    }

    /// Get the hash for chaining to next receipt
    pub fn chain_hash(&self) -> &str {
        &self.receipt_hash
    }
}

/// Epoch state
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct Epoch {
    /// Epoch number
    pub epoch_number: u64,
    /// Start timestamp
    pub start_time: DateTime<Utc>,
    /// End timestamp (None if current epoch)
    pub end_time: Option<DateTime<Utc>>,
    /// Root hash of receipts in this epoch
    pub merkle_root: String,
    /// Total operations in epoch
    pub total_operations: u64,
    /// Total refusals in epoch
    pub total_refusals: u64,
    /// Epoch status
    pub status: EpochStatus,
}

/// Epoch status
#[derive(Debug, Clone, Copy, PartialEq, Eq, Serialize, Deserialize)]
#[serde(rename_all = "snake_case")]
pub enum EpochStatus {
    /// Epoch is currently active
    Active,
    /// Epoch is closed
    Closed,
    /// Epoch has been revoked (kill switch)
    Revoked,
}

impl Epoch {
    /// Create a new epoch
    pub fn new(epoch_number: u64) -> Self {
        Self {
            epoch_number,
            start_time: Utc::now(),
            end_time: None,
            merkle_root: crate::GENESIS_HASH.to_string(),
            total_operations: 0,
            total_refusals: 0,
            status: EpochStatus::Active,
        }
    }

    /// Close the epoch
    pub fn close(&mut self, merkle_root: String) {
        self.end_time = Some(Utc::now());
        self.merkle_root = merkle_root;
        self.status = EpochStatus::Closed;
    }

    /// Revoke the epoch (kill switch)
    pub fn revoke(&mut self) {
        self.end_time = Some(Utc::now());
        self.status = EpochStatus::Revoked;
    }

    /// Check if epoch is active
    pub fn is_active(&self) -> bool {
        self.status == EpochStatus::Active
    }
}

/// Evidence bundle (.mcpb format)
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct EvidenceBundle {
    /// Bundle version
    pub version: String,
    /// Bundle ID
    pub bundle_id: String,
    /// Creation timestamp
    pub created_at: DateTime<Utc>,
    /// Period start
    pub period_start: DateTime<Utc>,
    /// Period end
    pub period_end: DateTime<Utc>,
    /// Contract family
    pub contract_family: String,
    /// Receipt chain Merkle root
    pub receipt_chain_root: String,
    /// Number of receipts
    pub receipt_count: u64,
    /// Number of refusals
    pub refusal_count: u64,
    /// Bundle hash
    pub bundle_hash: String,
    /// Attestations (signed by auditors)
    pub attestations: Vec<String>,
}

impl EvidenceBundle {
    /// Create an empty bundle
    pub fn new_empty(contract_family: impl Into<String>) -> Self {
        let bundle_id = format!("bundle-{}", uuid::Uuid::new_v4());
        let now = Utc::now();
        let period_start = now - chrono::Duration::hours(24);
        let contract_family = contract_family.into();

        // Empty bundle has zero hash
        let receipt_chain_root = crate::GENESIS_HASH.to_string();

        // Compute bundle hash
        let hash_input = format!(
            "1.0.0|{}|{}|{}|{}|{}|{}|0|0",
            bundle_id, now, period_start, now, contract_family, receipt_chain_root
        );
        let bundle_hash = hash_sha256(hash_input.as_bytes());

        Self {
            version: "1.0.0".to_string(),
            bundle_id,
            created_at: now,
            period_start,
            period_end: now,
            contract_family,
            receipt_chain_root,
            receipt_count: 0,
            refusal_count: 0,
            bundle_hash,
            attestations: vec![],
        }
    }

    /// Add an attestation (signed by auditor)
    pub fn add_attestation(&mut self, attestation: String) {
        self.attestations.push(attestation);
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_envelope_default() {
        let env = Envelope::default();
        assert!(env.has_capability(Capability::Read));
        assert!(!env.has_capability(Capability::Write));
        assert_eq!(env.max_operations, Some(1000));
    }

    #[test]
    fn test_envelope_full_trust() {
        let env = Envelope::full_trust();
        assert!(env.has_capability(Capability::Admin));
        assert!(env.max_operations.is_none());
    }

    #[test]
    fn test_envelope_builder() {
        let env = Envelope::read_only()
            .with_capability(Capability::Write)
            .with_max_operations(500)
            .with_disallowed_pattern("password");

        assert!(env.has_capability(Capability::Read));
        assert!(env.has_capability(Capability::Write));
        assert_eq!(env.max_operations, Some(500));
        assert!(env.disallowed_patterns.contains(&"password".to_string()));
    }

    #[test]
    fn test_receipt_creation() {
        let metrics = ExecutionMetrics {
            duration_us: 1000,
            memory_bytes: 1024,
            operations: 1,
            envelope_utilization: 0.5,
        };

        let receipt = Receipt::new(
            1,
            crate::GENESIS_HASH,
            "execute",
            "contract-001",
            "input-hash",
            "output-hash",
            metrics,
        );

        assert!(receipt.receipt_id.starts_with("rcpt-"));
        assert_eq!(receipt.sequence, 1);
        assert_eq!(receipt.receipt_hash.len(), 64);
    }

    #[test]
    fn test_receipt_signing() {
        let keypair = KeyPair::generate().unwrap();
        let metrics = ExecutionMetrics::default();

        let mut receipt = Receipt::new(
            1,
            crate::GENESIS_HASH,
            "test",
            "contract",
            "in",
            "out",
            metrics,
        );

        receipt.sign(&keypair);
        assert!(receipt.signature.is_some());
        assert!(receipt.verify(&keypair).is_ok());
    }

    #[test]
    fn test_epoch_lifecycle() {
        let mut epoch = Epoch::new(1);
        assert!(epoch.is_active());
        assert_eq!(epoch.status, EpochStatus::Active);

        epoch.close("new-merkle-root".to_string());
        assert!(!epoch.is_active());
        assert_eq!(epoch.status, EpochStatus::Closed);
        assert!(epoch.end_time.is_some());
    }

    #[test]
    fn test_epoch_revocation() {
        let mut epoch = Epoch::new(1);
        epoch.revoke();
        assert_eq!(epoch.status, EpochStatus::Revoked);
    }

    #[test]
    fn test_evidence_bundle_creation() {
        let bundle = EvidenceBundle::new_empty("gpt-4");

        assert!(bundle.bundle_id.starts_with("bundle-"));
        assert_eq!(bundle.receipt_count, 0);
        assert_eq!(bundle.bundle_hash.len(), 64);
    }

    #[test]
    fn test_evidence_bundle_attestation() {
        let mut bundle = EvidenceBundle::new_empty("test");
        bundle.add_attestation("auditor-signature-001".to_string());
        assert_eq!(bundle.attestations.len(), 1);
    }
}
