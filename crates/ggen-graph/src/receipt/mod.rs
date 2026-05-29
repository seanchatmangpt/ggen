//! Cryptographic receipts, validation, replay verification, and transaction bundling.

use crate::GraphError;
use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};
use std::collections::HashSet;

/// Cryptographically bound receipt of a deterministic graph state transition.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct GraphReceipt {
    /// Receipt format version.
    pub version: u8,
    /// Time when the transition occurred.
    pub timestamp: DateTime<Utc>,
    /// BLAKE3 hash of the pre-state graph.
    pub pre_state_hash: [u8; 32],
    /// BLAKE3 hash of the post-state graph.
    pub post_state_hash: [u8; 32],
    /// BLAKE3 hash of the RdfDelta applied.
    pub delta_hash: [u8; 32],
    /// Cryptographic checksum binding all transition fields.
    pub signature_or_hash: [u8; 32],
}

/// Alias for GraphReceipt to support TransitionReceipt interfaces.
pub type TransitionReceipt = GraphReceipt;

impl GraphReceipt {
    /// Generate a cryptographically bound transition receipt.
    pub fn new(pre_state_hash: [u8; 32], post_state_hash: [u8; 32], delta_hash: [u8; 32]) -> Self {
        let timestamp = Utc::now();
        let version = 1;
        let mut hasher = blake3::Hasher::new();
        hasher.update(&[version]);
        hasher.update(&pre_state_hash);
        hasher.update(&post_state_hash);
        hasher.update(&delta_hash);
        hasher.update(timestamp.to_rfc3339().as_bytes());
        let signature_or_hash = hasher.finalize().into();

        Self {
            version,
            timestamp,
            pre_state_hash,
            post_state_hash,
            delta_hash,
            signature_or_hash,
        }
    }

    /// Verifies the cryptographic integrity of the receipt.
    ///
    /// # Errors
    ///
    /// Returns `GraphError::VerificationFailed` if signature verification fails.
    pub fn verify(&self) -> Result<(), GraphError> {
        let mut hasher = blake3::Hasher::new();
        hasher.update(&[self.version]);
        hasher.update(&self.pre_state_hash);
        hasher.update(&self.post_state_hash);
        hasher.update(&self.delta_hash);
        hasher.update(self.timestamp.to_rfc3339().as_bytes());
        let expected_hash: [u8; 32] = hasher.finalize().into();

        if self.signature_or_hash != expected_hash {
            return Err(GraphError::VerificationFailed(
                "Cryptographic signature mismatch on GraphReceipt".to_string(),
            ));
        }

        Ok(())
    }
}

/// Cryptographic receipt for the execution and validation of a specific knowledge hook.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct HookReceipt {
    /// Receipt format version.
    pub version: u8,
    /// Name of the hook.
    pub hook_name: String,
    /// SPARQL ASK or SELECT query that defines the constraint.
    pub sparql_query: String,
    /// Whether the hook validation passed.
    pub passed: bool,
    /// Time when the hook was executed.
    pub timestamp: DateTime<Utc>,
    /// BLAKE3 hash of the graph state on which the hook was run.
    pub graph_state_hash: [u8; 32],
    /// Cryptographic checksum binding all fields.
    pub signature_or_hash: [u8; 32],
}

impl HookReceipt {
    /// Generate a cryptographically bound hook execution receipt.
    pub fn new(
        hook_name: String, sparql_query: String, passed: bool, graph_state_hash: [u8; 32],
    ) -> Self {
        let timestamp = Utc::now();
        let version = 1;
        let mut hasher = blake3::Hasher::new();
        hasher.update(&[version]);
        hasher.update(hook_name.as_bytes());
        hasher.update(sparql_query.as_bytes());
        hasher.update(&[passed as u8]);
        hasher.update(&graph_state_hash);
        hasher.update(timestamp.to_rfc3339().as_bytes());
        let signature_or_hash = hasher.finalize().into();

        Self {
            version,
            hook_name,
            sparql_query,
            passed,
            timestamp,
            graph_state_hash,
            signature_or_hash,
        }
    }

    /// Verifies the cryptographic integrity of the hook receipt.
    ///
    /// # Errors
    ///
    /// Returns `GraphError::VerificationFailed` if validation fails.
    pub fn verify(&self) -> Result<(), GraphError> {
        let mut hasher = blake3::Hasher::new();
        hasher.update(&[self.version]);
        hasher.update(self.hook_name.as_bytes());
        hasher.update(self.sparql_query.as_bytes());
        hasher.update(&[self.passed as u8]);
        hasher.update(&self.graph_state_hash);
        hasher.update(self.timestamp.to_rfc3339().as_bytes());
        let expected_hash: [u8; 32] = hasher.finalize().into();

        if self.signature_or_hash != expected_hash {
            return Err(GraphError::VerificationFailed(
                "Cryptographic signature mismatch on HookReceipt".to_string(),
            ));
        }

        Ok(())
    }
}

/// Verification utility to prevent replay of transition receipts and enforce linear history.
#[derive(Debug, Clone, Default)]
pub struct ReplayVerifier {
    seen_signatures: HashSet<[u8; 32]>,
    last_state_hash: Option<[u8; 32]>,
}

impl ReplayVerifier {
    /// Create a new replay verifier with an optional initial state hash.
    pub fn new(initial_state_hash: Option<[u8; 32]>) -> Self {
        Self {
            seen_signatures: HashSet::new(),
            last_state_hash: initial_state_hash,
        }
    }

    /// Verifies a transition receipt against replay attacks and history.
    /// Checks that the receipt signature has not been seen before, and optionally
    /// that the receipt's pre-state hash matches the expected current state hash.
    ///
    /// # Errors
    ///
    /// Returns `GraphError::VerificationFailed` if replay or chain discontinuity is detected.
    pub fn verify_transition(&mut self, receipt: &GraphReceipt) -> Result<(), GraphError> {
        // 1. Verify cryptographic integrity of the receipt itself
        receipt.verify()?;

        // 2. Check if we've seen this signature/hash before
        if !self.seen_signatures.insert(receipt.signature_or_hash) {
            return Err(GraphError::VerificationFailed(format!(
                "Replay detected: Transition receipt with signature {:?} has already been applied",
                receipt.signature_or_hash
            )));
        }

        // 3. Enforce linear continuity if last_state_hash is tracked
        if let Some(expected_pre_hash) = self.last_state_hash {
            if receipt.pre_state_hash != expected_pre_hash {
                return Err(GraphError::VerificationFailed(
                    format!(
                        "Chain discontinuity: receipt expects pre-state hash {:?}, but current state hash is {:?}",
                        receipt.pre_state_hash, expected_pre_hash
                    )
                ));
            }
        }

        // Update the tracked state hash to post-state
        self.last_state_hash = Some(receipt.post_state_hash);
        Ok(())
    }
}

/// A bundle of multiple graph transition receipts and hook execution receipts.
#[derive(Debug, Clone, Serialize, Deserialize, PartialEq, Eq)]
pub struct TransactionBundle {
    /// List of graph state transition receipts in the bundle.
    pub receipts: Vec<GraphReceipt>,
    /// List of hook execution receipts in the bundle.
    pub hook_receipts: Vec<HookReceipt>,
    /// Cryptographic checksum binding the entire bundle.
    pub bundle_hash: [u8; 32],
    /// Timestamp of the bundle creation.
    pub timestamp: DateTime<Utc>,
}

impl TransactionBundle {
    /// Create and cryptographically bind a new transaction bundle.
    pub fn new(receipts: Vec<GraphReceipt>, hook_receipts: Vec<HookReceipt>) -> Self {
        let timestamp = Utc::now();
        let mut hasher = blake3::Hasher::new();
        hasher.update(timestamp.to_rfc3339().as_bytes());
        for r in &receipts {
            hasher.update(&r.signature_or_hash);
        }
        for h in &hook_receipts {
            hasher.update(&h.signature_or_hash);
        }
        let bundle_hash = hasher.finalize().into();
        Self {
            receipts,
            hook_receipts,
            bundle_hash,
            timestamp,
        }
    }

    /// Verifies the entire transaction bundle:
    /// - Checks cryptographic integrity of the bundle hash
    /// - Verifies every nested graph receipt
    /// - Verifies every nested hook receipt
    ///
    /// # Errors
    ///
    /// Returns `GraphError::VerificationFailed` if validation fails.
    pub fn verify(&self) -> Result<(), GraphError> {
        let mut hasher = blake3::Hasher::new();
        hasher.update(self.timestamp.to_rfc3339().as_bytes());
        for r in &self.receipts {
            hasher.update(&r.signature_or_hash);
            r.verify()?;
        }
        for h in &self.hook_receipts {
            hasher.update(&h.signature_or_hash);
            h.verify()?;
        }
        let expected: [u8; 32] = hasher.finalize().into();
        if self.bundle_hash != expected {
            return Err(GraphError::VerificationFailed(
                "Transaction bundle cryptographic hash mismatch".to_string(),
            ));
        }
        Ok(())
    }
}
