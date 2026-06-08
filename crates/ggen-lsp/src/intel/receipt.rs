//! `RepairReceipt` — binds a diagnostic episode to its repair outcome.
//!
//! Mirrors ggen-graph's BLAKE3 self-binding receipt pattern but adds the
//! diagnostic linkage `GraphReceipt` lacks: which diagnostic was repaired, the
//! pre/post file-state hashes, and whether the gate passed afterward. A sequence
//! of these forms a linear repair history (post_n == pre_{n+1}).

use chrono::{DateTime, Utc};
use serde::{Deserialize, Serialize};

/// Cryptographic record of one repair episode's outcome.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct RepairReceipt {
    /// Schema version.
    pub version: u8,
    /// When the receipt was minted.
    pub timestamp: DateTime<Utc>,
    /// `OCELEvent.id` of the `DiagnosticRaised` that opened this episode.
    pub diagnostic_event_id: String,
    /// The E00XX code being repaired.
    pub diagnostic_code: String,
    /// BLAKE3 of the file buffer before the repair.
    pub pre_state_hash: [u8; 32],
    /// BLAKE3 of the file buffer after the repair.
    pub post_state_hash: [u8; 32],
    /// Whether `ggen lsp check` cleared the code afterward.
    pub gate_pass: bool,
    /// BLAKE3 binding of all fields above.
    pub signature_or_hash: [u8; 32],
}

impl RepairReceipt {
    /// Mint a receipt, computing the binding hash over all fields.
    #[must_use]
    pub fn new(
        diagnostic_event_id: String, diagnostic_code: String, pre_state_hash: [u8; 32],
        post_state_hash: [u8; 32], gate_pass: bool,
    ) -> Self {
        let timestamp = Utc::now();
        let version = 1u8;
        let signature_or_hash = Self::compute_hash(
            version,
            &timestamp,
            &diagnostic_event_id,
            &diagnostic_code,
            &pre_state_hash,
            &post_state_hash,
            gate_pass,
        );
        Self {
            version,
            timestamp,
            diagnostic_event_id,
            diagnostic_code,
            pre_state_hash,
            post_state_hash,
            gate_pass,
            signature_or_hash,
        }
    }

    /// Hex of the binding hash (for receipt filenames / display).
    #[must_use]
    pub fn signature_hex(&self) -> String {
        use std::fmt::Write as _;
        self.signature_or_hash
            .iter()
            .fold(String::new(), |mut acc, b| {
                let _ = write!(acc, "{b:02x}");
                acc
            })
    }

    /// Verify the binding hash matches the fields (tamper detection).
    #[must_use]
    pub fn verify(&self) -> bool {
        let expected = Self::compute_hash(
            self.version,
            &self.timestamp,
            &self.diagnostic_event_id,
            &self.diagnostic_code,
            &self.pre_state_hash,
            &self.post_state_hash,
            self.gate_pass,
        );
        expected == self.signature_or_hash
    }

    #[allow(clippy::too_many_arguments)]
    fn compute_hash(
        version: u8, timestamp: &DateTime<Utc>, diagnostic_event_id: &str, diagnostic_code: &str,
        pre: &[u8; 32], post: &[u8; 32], gate_pass: bool,
    ) -> [u8; 32] {
        let mut h = blake3::Hasher::new();
        h.update(&[version]);
        h.update(timestamp.to_rfc3339().as_bytes());
        h.update(diagnostic_event_id.as_bytes());
        h.update(diagnostic_code.as_bytes());
        h.update(pre);
        h.update(post);
        h.update(&[u8::from(gate_pass)]);
        h.finalize().into()
    }
}

/// BLAKE3 hash of file content, for pre/post state hashes.
#[must_use]
pub fn hash_content(content: &str) -> [u8; 32] {
    blake3::hash(content.as_bytes()).into()
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn receipt_verifies_and_detects_tamper() {
        let pre = hash_content("before");
        let post = hash_content("after");
        let mut r = RepairReceipt::new("ev1".into(), "E0010".into(), pre, post, true);
        assert!(r.verify(), "fresh receipt verifies");
        r.gate_pass = false; // tamper
        assert!(!r.verify(), "tampered receipt fails verification");
    }

    #[test]
    fn chain_links_pre_to_post() {
        let s0 = hash_content("v0");
        let s1 = hash_content("v1");
        let s2 = hash_content("v2");
        let r1 = RepairReceipt::new("e1".into(), "E0010".into(), s0, s1, true);
        let r2 = RepairReceipt::new("e2".into(), "E0011".into(), s1, s2, true);
        // Linear history: post of r1 == pre of r2.
        assert_eq!(r1.post_state_hash, r2.pre_state_hash);
    }
}
