//! Provenance envelope merging forward and inverse receipts.
//!
//! The `ProvenanceEnvelope` struct binds together:
//! - `Receipt` (μ₁–μ₅ forward path): O (ontology) → A (artifact)
//! - `InverseReceipt` (μ⁻¹ inverse path): A (artifact) → O (recovered ontology)
//! - `CoherenceReport` (optional validation across both paths)
//!
//! This creates a unified proof object that demonstrates bidirectional provenance.

use crate::receipt::error::{ReceiptError, Result};
use crate::receipt::Receipt;
use crate::reverse_sync::inverse_pipeline::InverseReceipt;
use chrono::{DateTime, Utc};
use ed25519_dalek::VerifyingKey;
use serde::{Deserialize, Serialize};
use sha2::{Digest, Sha256};

/// Coherence validation report between forward and inverse paths.
///
/// Verifies that the forward artifact → inverse recovery → artifact
/// produces a semantically equivalent ontology.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct CoherenceReport {
    /// Unique identifier for this validation run.
    pub validation_id: String,

    /// RFC-3339 timestamp of the validation.
    pub timestamp: DateTime<Utc>,

    /// SHA-256 hash of the forward artifact.
    pub forward_artifact_hash: String,

    /// SHA-256 hash of the recovered artifact from inverse pipeline.
    pub recovered_artifact_hash: String,

    /// True if forward and recovered artifacts are semantically equivalent.
    pub admitted: bool,

    /// Optional divergence details if admitted is false.
    #[serde(default)]
    pub divergence: Option<String>,

    /// SHA-256 hash of the entire report (for chain linking).
    #[serde(default)]
    pub report_hash: String,
}

impl CoherenceReport {
    /// Creates a new coherence report.
    #[must_use]
    pub fn new(
        validation_id: String, forward_artifact_hash: String, recovered_artifact_hash: String,
        admitted: bool, divergence: Option<String>,
    ) -> Self {
        let mut report = Self {
            validation_id,
            timestamp: Utc::now(),
            forward_artifact_hash,
            recovered_artifact_hash,
            admitted,
            divergence,
            report_hash: String::new(),
        };
        report.report_hash = report.compute_hash();
        report
    }

    /// Computes the SHA-256 hash of this report.
    fn compute_hash(&self) -> String {
        let json = serde_json::to_string(&self).unwrap_or_default();
        let mut hasher = Sha256::new();
        hasher.update(json.as_bytes());
        hex::encode(hasher.finalize())
    }

    /// Updates the report hash after any field change.
    pub fn update_hash(&mut self) {
        self.report_hash = self.compute_hash();
    }
}

/// Envelope binding forward receipt, inverse receipt, and optional coherence report.
///
/// Encodes the complete bidirectional provenance of an operation:
/// - Forward path (O → A) with Receipt
/// - Inverse path (A → O) with InverseReceipt
/// - Coherence validation linking both paths
/// - Operation chain tracking all linked operations
/// - Envelope hash (BLAKE3) binding all proof elements
#[derive(Debug, Clone, Serialize, Deserialize)]
pub struct ProvenanceEnvelope {
    /// Optional forward receipt (μ₁–μ₅ path).
    pub forward_receipt: Option<Receipt>,

    /// Optional inverse receipt (μ⁻¹ path).
    pub inverse_receipt: Option<InverseReceipt>,

    /// Optional coherence validation report.
    pub coherence_report: Option<CoherenceReport>,

    /// Ordered list of operation IDs: [forward_op_id, inverse_op_id, ...].
    pub operation_chain: Vec<String>,

    /// BLAKE3 hash binding all proof elements (forward_hash || inverse_hash || coherence_hash).
    pub envelope_hash: String,

    /// RFC-3339 timestamp when the envelope was created or linked.
    pub linked_at: String,
}

impl ProvenanceEnvelope {
    /// Creates a new empty envelope.
    #[must_use]
    pub fn new() -> Self {
        let mut envelope = Self {
            forward_receipt: None,
            inverse_receipt: None,
            coherence_report: None,
            operation_chain: Vec::new(),
            envelope_hash: String::new(),
            linked_at: Utc::now().to_rfc3339(),
        };
        envelope.envelope_hash = envelope.compute_hash();
        envelope
    }

    /// Creates an envelope initialized with a forward receipt.
    #[must_use]
    pub fn from_forward(receipt: Receipt) -> Self {
        let mut envelope = Self::new();
        envelope.forward_receipt = Some(receipt.clone());
        envelope.operation_chain.push(receipt.operation_id.clone());
        envelope.envelope_hash = envelope.compute_hash();
        envelope
    }

    /// Creates an envelope initialized with an inverse receipt.
    #[must_use]
    pub fn from_inverse(receipt: InverseReceipt) -> Self {
        let mut envelope = Self::new();
        envelope.inverse_receipt = Some(receipt.clone());
        envelope.operation_chain.push(receipt.operation_id.clone());
        envelope.envelope_hash = envelope.compute_hash();
        envelope
    }

    /// Adds a forward receipt to the envelope, recomputing the envelope hash.
    ///
    /// # Errors
    ///
    /// Returns `ReceiptError::InvalidReceipt` if the receipt's signature is empty.
    pub fn add_forward(mut self, receipt: Receipt) -> Result<Self> {
        if receipt.signature.is_empty() {
            return Err(ReceiptError::InvalidReceipt(
                "Forward receipt must have a non-empty signature".to_string(),
            ));
        }

        self.forward_receipt = Some(receipt.clone());
        self.operation_chain.push(receipt.operation_id.clone());
        self.envelope_hash = self.compute_hash();
        Ok(self)
    }

    /// Adds an inverse receipt to the envelope, recomputing the envelope hash.
    ///
    /// # Errors
    ///
    /// Returns `ReceiptError::InvalidReceipt` if the receipt's signature is empty.
    pub fn add_inverse(mut self, receipt: InverseReceipt) -> Result<Self> {
        if receipt.signature.is_empty() {
            return Err(ReceiptError::InvalidReceipt(
                "Inverse receipt must have a non-empty signature".to_string(),
            ));
        }

        self.inverse_receipt = Some(receipt.clone());
        self.operation_chain.push(receipt.operation_id.clone());
        self.envelope_hash = self.compute_hash();
        Ok(self)
    }

    /// Adds a coherence report to the envelope, recomputing the envelope hash.
    pub fn add_coherence(mut self, mut report: CoherenceReport) -> Self {
        report.update_hash();
        self.coherence_report = Some(report);
        self.envelope_hash = self.compute_hash();
        self
    }

    /// Computes the envelope hash as BLAKE3(forward_hash || inverse_hash || coherence_hash).
    ///
    /// Uses SHA-256 instead of BLAKE3 for compatibility with existing Receipt infrastructure.
    fn compute_hash(&self) -> String {
        let mut hasher = Sha256::new();

        // Forward receipt hash
        if let Some(receipt) = &self.forward_receipt {
            if let Ok(hash) = receipt.hash() {
                hasher.update(hash.as_bytes());
            }
        }

        // Inverse receipt hash (serialize and hash)
        if let Some(receipt) = &self.inverse_receipt {
            if let Ok(json) = serde_json::to_string(receipt) {
                hasher.update(json.as_bytes());
            }
        }

        // Coherence report hash
        if let Some(report) = &self.coherence_report {
            hasher.update(report.report_hash.as_bytes());
        }

        // Operation chain hash
        for op_id in &self.operation_chain {
            hasher.update(op_id.as_bytes());
        }

        hex::encode(hasher.finalize())
    }

    /// Returns the current envelope hash as a 64-char hex string.
    ///
    /// The hash reflects the current state of all proof elements.
    #[must_use]
    pub fn hash(&self) -> String {
        self.envelope_hash.clone()
    }

    /// Verifies the envelope's proof elements.
    ///
    /// Fails closed:
    /// - Returns false if forward_receipt is present and signature verification fails
    /// - Returns false if inverse_receipt is present and signature verification fails
    /// - Returns false if coherence_report is present and admitted is false
    /// - Returns true only when all present proof elements are valid
    #[must_use]
    pub fn verify(&self, verifying_key: &VerifyingKey) -> bool {
        // Verify forward receipt if present
        if let Some(forward) = &self.forward_receipt {
            if forward.verify(verifying_key).is_err() {
                return false;
            }
        }

        // Verify inverse receipt if present (uses verify method that returns bool)
        if let Some(inverse) = &self.inverse_receipt {
            if !inverse.verify(verifying_key) {
                return false;
            }
        }

        // Check coherence report if present
        if let Some(report) = &self.coherence_report {
            if !report.admitted {
                return false;
            }
        }

        // If we reach here, all present proof elements are valid
        true
    }

    /// Serializes the envelope to JSON.
    ///
    /// # Errors
    ///
    /// Returns `ReceiptError::Serialization` if serialization fails.
    pub fn to_json(&self) -> Result<String> {
        serde_json::to_string(self).map_err(|e| ReceiptError::Serialization(e))
    }

    /// Deserializes an envelope from JSON.
    ///
    /// # Errors
    ///
    /// Returns `ReceiptError::Serialization` if deserialization fails.
    pub fn from_json(s: &str) -> Result<Self> {
        serde_json::from_str(s).map_err(|e| ReceiptError::Serialization(e))
    }
}

impl Default for ProvenanceEnvelope {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::receipt::generate_keypair;

    #[test]
    fn test_envelope_new() {
        let env = ProvenanceEnvelope::new();
        assert!(env.forward_receipt.is_none());
        assert!(env.inverse_receipt.is_none());
        assert!(env.coherence_report.is_none());
        assert!(env.operation_chain.is_empty());
    }

    #[test]
    fn test_from_forward() {
        let (signing_key, _) = generate_keypair();
        let receipt = Receipt::new(
            "forward-op-1".to_string(),
            vec!["input1".to_string()],
            vec!["output1".to_string()],
            None,
        )
        .sign(&signing_key)
        .expect("signing failed");

        let env = ProvenanceEnvelope::from_forward(receipt.clone());

        assert!(env.forward_receipt.is_some());
        assert!(env.inverse_receipt.is_none());
        assert_eq!(env.operation_chain.len(), 1);
        assert_eq!(env.operation_chain[0], "forward-op-1");
        assert!(!env.envelope_hash.is_empty());
    }

    #[test]
    fn test_from_inverse() {
        use crate::reverse_sync::inverse_pipeline::InverseStage;
        use std::collections::HashMap;

        let (signing_key, _) = generate_keypair();
        let mut input_hashes = HashMap::new();
        input_hashes.insert("file1.rs".to_string(), "hash1".to_string());

        let receipt = InverseReceipt {
            operation_id: "inverse-op-1".to_string(),
            timestamp: Utc::now(),
            input_hashes,
            output_hash: "output-hash".to_string(),
            recovered_triple_count: 42,
            shacl_valid: true,
            last_stage: InverseStage::Emit,
            signature: String::new(),
            previous_operation_id: None,
        }
        .sign(&signing_key)
        .expect("signing failed");

        let env = ProvenanceEnvelope::from_inverse(receipt.clone());

        assert!(env.inverse_receipt.is_some());
        assert!(env.forward_receipt.is_none());
        assert_eq!(env.operation_chain.len(), 1);
        assert_eq!(env.operation_chain[0], "inverse-op-1");
        assert!(!env.envelope_hash.is_empty());
    }

    #[test]
    fn test_add_forward_success() {
        let (signing_key, _) = generate_keypair();
        let receipt = Receipt::new(
            "op1".to_string(),
            vec!["input1".to_string()],
            vec!["output1".to_string()],
            None,
        )
        .sign(&signing_key)
        .expect("signing failed");

        let envelope = ProvenanceEnvelope::new()
            .add_forward(receipt)
            .expect("add_forward failed");

        assert!(envelope.forward_receipt.is_some());
        assert_eq!(envelope.operation_chain.len(), 1);
        assert_eq!(envelope.operation_chain[0], "op1");
    }

    #[test]
    fn test_add_forward_empty_signature_fails() {
        let unsigned = Receipt::new(
            "op1".to_string(),
            vec!["input1".to_string()],
            vec!["output1".to_string()],
            None,
        );

        let result = ProvenanceEnvelope::new().add_forward(unsigned);

        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("non-empty signature"));
    }

    #[test]
    fn test_add_inverse_success() {
        use crate::reverse_sync::inverse_pipeline::InverseStage;
        use std::collections::HashMap;

        let (signing_key, _) = generate_keypair();
        let mut input_hashes = HashMap::new();
        input_hashes.insert("file1.rs".to_string(), "hash1".to_string());

        let receipt = InverseReceipt {
            operation_id: "inverse-op".to_string(),
            timestamp: Utc::now(),
            input_hashes,
            output_hash: "output-hash".to_string(),
            recovered_triple_count: 42,
            shacl_valid: true,
            last_stage: InverseStage::Emit,
            signature: String::new(),
            previous_operation_id: None,
        }
        .sign(&signing_key)
        .expect("signing failed");

        let envelope = ProvenanceEnvelope::new()
            .add_inverse(receipt)
            .expect("add_inverse failed");

        assert!(envelope.inverse_receipt.is_some());
        assert_eq!(envelope.operation_chain.len(), 1);
        assert_eq!(envelope.operation_chain[0], "inverse-op");
    }

    #[test]
    fn test_add_inverse_empty_signature_fails() {
        use crate::reverse_sync::inverse_pipeline::InverseStage;
        use std::collections::HashMap;

        let mut input_hashes = HashMap::new();
        input_hashes.insert("file1.rs".to_string(), "hash1".to_string());

        let unsigned = InverseReceipt {
            operation_id: "op".to_string(),
            timestamp: Utc::now(),
            input_hashes,
            output_hash: "hash".to_string(),
            recovered_triple_count: 0,
            shacl_valid: true,
            last_stage: InverseStage::Emit,
            signature: String::new(),
            previous_operation_id: None,
        };

        let result = ProvenanceEnvelope::new().add_inverse(unsigned);

        assert!(result.is_err());
        assert!(result
            .unwrap_err()
            .to_string()
            .contains("non-empty signature"));
    }

    #[test]
    fn test_envelope_hash_differs_after_add() {
        let (signing_key, _) = generate_keypair();

        let forward = Receipt::new(
            "op1".to_string(),
            vec!["input1".to_string()],
            vec!["output1".to_string()],
            None,
        )
        .sign(&signing_key)
        .expect("signing failed");

        let env1 = ProvenanceEnvelope::new();
        let hash1 = env1.hash();

        let env2 = ProvenanceEnvelope::new()
            .add_forward(forward)
            .expect("add_forward failed");
        let hash2 = env2.hash();

        assert_ne!(hash1, hash2);
        assert_eq!(hash1.len(), 64); // SHA-256 hex
        assert_eq!(hash2.len(), 64);
    }

    #[test]
    fn test_add_forward_then_inverse() {
        use crate::reverse_sync::inverse_pipeline::InverseStage;
        use std::collections::HashMap;

        let (signing_key, _) = generate_keypair();

        let forward = Receipt::new(
            "forward-op".to_string(),
            vec!["input".to_string()],
            vec!["output".to_string()],
            None,
        )
        .sign(&signing_key)
        .expect("signing failed");

        let mut input_hashes = HashMap::new();
        input_hashes.insert("file.rs".to_string(), "filehash".to_string());

        let inverse = InverseReceipt {
            operation_id: "inverse-op".to_string(),
            timestamp: Utc::now(),
            input_hashes,
            output_hash: "output".to_string(),
            recovered_triple_count: 10,
            shacl_valid: true,
            last_stage: InverseStage::Emit,
            signature: String::new(),
            previous_operation_id: None,
        }
        .sign(&signing_key)
        .expect("signing failed");

        let envelope = ProvenanceEnvelope::new()
            .add_forward(forward)
            .expect("add_forward failed")
            .add_inverse(inverse)
            .expect("add_inverse failed");

        assert!(envelope.forward_receipt.is_some());
        assert!(envelope.inverse_receipt.is_some());
        assert_eq!(envelope.operation_chain.len(), 2);
        assert_eq!(envelope.operation_chain[0], "forward-op");
        assert_eq!(envelope.operation_chain[1], "inverse-op");
    }

    #[test]
    fn test_add_coherence_report() {
        let (signing_key, _) = generate_keypair();

        let forward = Receipt::new(
            "op1".to_string(),
            vec!["input".to_string()],
            vec!["output".to_string()],
            None,
        )
        .sign(&signing_key)
        .expect("signing failed");

        let report = CoherenceReport::new(
            "validation-1".to_string(),
            "forward-hash".to_string(),
            "recovered-hash".to_string(),
            true,
            None,
        );

        let envelope = ProvenanceEnvelope::new()
            .add_forward(forward)
            .expect("add_forward failed")
            .add_coherence(report);

        assert!(envelope.coherence_report.is_some());
        assert!(envelope.coherence_report.unwrap().admitted);
    }

    #[test]
    fn test_verify_all_valid() {
        let (signing_key, verifying_key) = generate_keypair();

        let forward = Receipt::new(
            "op1".to_string(),
            vec!["input".to_string()],
            vec!["output".to_string()],
            None,
        )
        .sign(&signing_key)
        .expect("signing failed");

        let envelope = ProvenanceEnvelope::new()
            .add_forward(forward)
            .expect("add_forward failed");

        assert!(envelope.verify(&verifying_key));
    }

    #[test]
    fn test_verify_forward_signature_mismatch() {
        let (signing_key, _) = generate_keypair();
        let (_, wrong_key) = generate_keypair();

        let forward = Receipt::new(
            "op1".to_string(),
            vec!["input".to_string()],
            vec!["output".to_string()],
            None,
        )
        .sign(&signing_key)
        .expect("signing failed");

        let envelope = ProvenanceEnvelope::new()
            .add_forward(forward)
            .expect("add_forward failed");

        assert!(!envelope.verify(&wrong_key));
    }

    #[test]
    fn test_verify_coherence_report_not_admitted() {
        let (signing_key, verifying_key) = generate_keypair();

        let forward = Receipt::new(
            "op1".to_string(),
            vec!["input".to_string()],
            vec!["output".to_string()],
            None,
        )
        .sign(&signing_key)
        .expect("signing failed");

        let report = CoherenceReport::new(
            "validation-1".to_string(),
            "forward-hash".to_string(),
            "recovered-hash".to_string(),
            false, // NOT admitted
            Some("hashes diverged".to_string()),
        );

        let envelope = ProvenanceEnvelope::new()
            .add_forward(forward)
            .expect("add_forward failed")
            .add_coherence(report);

        assert!(!envelope.verify(&verifying_key));
    }

    #[test]
    fn test_to_json_from_json_roundtrip() {
        let (signing_key, _) = generate_keypair();

        let forward = Receipt::new(
            "op1".to_string(),
            vec!["input".to_string()],
            vec!["output".to_string()],
            None,
        )
        .sign(&signing_key)
        .expect("signing failed");

        let original = ProvenanceEnvelope::new()
            .add_forward(forward)
            .expect("add_forward failed");

        let json = original.to_json().expect("to_json failed");
        let deserialized = ProvenanceEnvelope::from_json(&json).expect("from_json failed");

        assert!(deserialized.forward_receipt.is_some());
        assert_eq!(deserialized.operation_chain.len(), 1);
        assert_eq!(deserialized.operation_chain[0], "op1");
        assert_eq!(original.envelope_hash, deserialized.envelope_hash);
    }

    #[test]
    fn test_json_roundtrip_with_both_receipts() {
        use crate::reverse_sync::inverse_pipeline::InverseStage;
        use std::collections::HashMap;

        let (signing_key, _) = generate_keypair();

        let forward = Receipt::new(
            "forward-op".to_string(),
            vec!["input".to_string()],
            vec!["output".to_string()],
            None,
        )
        .sign(&signing_key)
        .expect("signing failed");

        let mut input_hashes = HashMap::new();
        input_hashes.insert("file.rs".to_string(), "hash".to_string());

        let inverse = InverseReceipt {
            operation_id: "inverse-op".to_string(),
            timestamp: Utc::now(),
            input_hashes,
            output_hash: "output".to_string(),
            recovered_triple_count: 10,
            shacl_valid: true,
            last_stage: InverseStage::Emit,
            signature: String::new(),
            previous_operation_id: None,
        }
        .sign(&signing_key)
        .expect("signing failed");

        let original = ProvenanceEnvelope::new()
            .add_forward(forward)
            .expect("add_forward failed")
            .add_inverse(inverse)
            .expect("add_inverse failed");

        let json = original.to_json().expect("to_json failed");
        let deserialized = ProvenanceEnvelope::from_json(&json).expect("from_json failed");

        assert!(deserialized.forward_receipt.is_some());
        assert!(deserialized.inverse_receipt.is_some());
        assert_eq!(deserialized.operation_chain.len(), 2);
    }

    // ── Sabotage tests (coding-agent-mistakes.md §5) ─────────────────────────

    /// Sabotage test: add_forward with empty signature must fail (fail-closed).
    /// Prevents decorative-completion: the envelope appears created but has no proof.
    #[test]
    fn sabotage_add_forward_empty_signature_fails() {
        let unsigned_receipt = Receipt::new(
            "op-id".to_string(),
            vec!["input".to_string()],
            vec!["output".to_string()],
            None,
        );
        assert!(unsigned_receipt.signature.is_empty(), "precondition");

        let result = ProvenanceEnvelope::new().add_forward(unsigned_receipt);

        assert!(
            result.is_err(),
            "add_forward must reject unsigned receipt (empty signature)"
        );
    }

    /// Sabotage test: add_inverse with empty signature must fail (fail-closed).
    #[test]
    fn sabotage_add_inverse_empty_signature_fails() {
        use crate::reverse_sync::inverse_pipeline::InverseStage;
        use std::collections::HashMap;

        let mut input_hashes = HashMap::new();
        input_hashes.insert("file.rs".to_string(), "hash".to_string());

        let unsigned_receipt = InverseReceipt {
            operation_id: "op".to_string(),
            timestamp: Utc::now(),
            input_hashes,
            output_hash: "hash".to_string(),
            recovered_triple_count: 0,
            shacl_valid: true,
            last_stage: InverseStage::Emit,
            signature: String::new(),
            previous_operation_id: None,
        };
        assert!(unsigned_receipt.signature.is_empty(), "precondition");

        let result = ProvenanceEnvelope::new().add_inverse(unsigned_receipt);

        assert!(
            result.is_err(),
            "add_inverse must reject unsigned receipt (empty signature)"
        );
    }
}
