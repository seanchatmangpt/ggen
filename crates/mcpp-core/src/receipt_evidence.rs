//! BLAKE3 receipt evidence — canonical receipt-evidence law.
//!
//! Promoted from absorbed source `/Users/sac/chatmangpt/mcpp/speckit-ralph/src/main.rs`
//! (portfolio-obl-0002, promotion id `blake3-receipt-evidence`).
//!
//! Doctrine:
//! - Receipts are not narrative. They are evidence-bearing artifacts.
//! - Every receipt-producing or receipt-verifying output attaches BLAKE3
//!   hashes covering the inputs, outputs, and prior chained receipt.
//! - Hashing is deterministic: same bytes in, same hash out.
//! - Receipt evidence defects are typed and emitted as JSON envelopes
//!   (the `RECEIPT_DEFECT` class).

use crate::envelope::Envelope;
use serde::{Deserialize, Serialize};
use std::collections::BTreeMap;
use std::fs;
use std::path::Path;

/// Stable schema identifier for the canonical receipt evidence record.
pub const RECEIPT_EVIDENCE_SCHEMA: &str = "chatmangpt.mcpp.receipt.evidence.v1";

/// Class string emitted in `Envelope::fail` when receipt evidence is
/// missing, malformed, non-deterministic, or otherwise invalid.
pub const RECEIPT_DEFECT_CLASS: &str = "RECEIPT_DEFECT";

/// BLAKE3 hash of the empty byte string (sentinel; useful for tests).
pub const BLAKE3_EMPTY: &str =
    "blake3:af1349b9f5f9a1a6a0404dea36dcc9499bcb25c9adc112b7cc9a93cae41f3262";

/// Compute the canonical BLAKE3 hash of arbitrary bytes.
///
/// Output format: `blake3:<hex>` (lowercase, 64 chars). The prefix is
/// part of the contract so consumers can multiplex hash algorithms in
/// the same evidence map without ambiguity.
pub fn blake3_bytes(bytes: &[u8]) -> String {
    format!("blake3:{}", blake3::hash(bytes).to_hex())
}

/// Hash a UTF-8 string under BLAKE3.
pub fn blake3_str(s: &str) -> String {
    blake3_bytes(s.as_bytes())
}

/// Hash the contents of a file under BLAKE3. Returns `None` if the
/// path is unreadable; the caller decides whether absence is a
/// `RECEIPT_DEFECT` or a recorded gap.
pub fn blake3_file(path: &Path) -> Option<String> {
    fs::read(path).ok().map(|b| blake3_bytes(&b))
}

/// Builder for the canonical evidence map.
///
/// Insertion order does not affect the final composite `evidence_hash`
/// because the underlying store is a `BTreeMap`, which iterates in key
/// order. Two equal evidence sets always produce equal composite
/// hashes — that is the determinism the law requires.
#[derive(Debug, Default, Clone)]
pub struct EvidenceBuilder {
    entries: BTreeMap<String, String>,
}

impl EvidenceBuilder {
    pub fn new() -> Self {
        Self::default()
    }

    /// Add a named hash (full `blake3:<hex>` string expected).
    pub fn insert<K: Into<String>, V: Into<String>>(&mut self, key: K, value: V) -> &mut Self {
        self.entries.insert(key.into(), value.into());
        self
    }

    /// Hash a byte input and add it under `key`.
    pub fn add_bytes<K: Into<String>>(&mut self, key: K, bytes: &[u8]) -> &mut Self {
        self.insert(key, blake3_bytes(bytes))
    }

    /// Hash a string input and add it under `key`.
    pub fn add_str<K: Into<String>>(&mut self, key: K, s: &str) -> &mut Self {
        self.insert(key, blake3_str(s))
    }

    /// Hash a file's contents and add it under `key`. Returns `Err` on
    /// I/O failure so the caller can decide whether to record a gap or
    /// to fail the receipt.
    pub fn add_file<K: Into<String>>(&mut self, key: K, path: &Path) -> std::io::Result<&mut Self> {
        let bytes = fs::read(path)?;
        Ok(self.insert(key, blake3_bytes(&bytes)))
    }

    /// View the underlying entries for serialization.
    pub fn entries(&self) -> &BTreeMap<String, String> {
        &self.entries
    }

    /// Finalize into a `ReceiptEvidence`.
    pub fn finalize(self, work_unit: impl Into<String>) -> ReceiptEvidence {
        let entries = self.entries;
        let composite = composite_hash(&entries);
        ReceiptEvidence {
            schema: RECEIPT_EVIDENCE_SCHEMA.into(),
            work_unit: work_unit.into(),
            entries,
            evidence_hash: composite,
        }
    }
}

/// Compose the deterministic composite hash over a sorted evidence map.
///
/// The composite is `blake3` of `key1\thash1\nkey2\thash2\n...` with
/// keys in lexicographic order. Two evidence maps with identical
/// (key, hash) pairs always produce identical composites.
pub fn composite_hash(entries: &BTreeMap<String, String>) -> String {
    let mut payload = String::new();
    for (k, v) in entries {
        payload.push_str(k);
        payload.push('\t');
        payload.push_str(v);
        payload.push('\n');
    }
    blake3_str(&payload)
}

/// A finalized receipt-evidence record. Serializable, deterministic,
/// and intended to live inside an `Envelope.data` payload or a
/// receipt artifact.
#[derive(Debug, Serialize, Deserialize, Clone)]
pub struct ReceiptEvidence {
    pub schema: String,
    pub work_unit: String,
    pub entries: BTreeMap<String, String>,
    pub evidence_hash: String,
}

impl ReceiptEvidence {
    /// Re-validate the composite hash against the entries. Returns
    /// `true` iff the recorded `evidence_hash` matches the deterministic
    /// composite of the current entries.
    pub fn verify(&self) -> bool {
        composite_hash(&self.entries) == self.evidence_hash
    }
}

/// Build a `RECEIPT_DEFECT` failure envelope.
pub fn receipt_defect(command: &str, target: &str, message: &str) -> Envelope {
    Envelope::fail(command, target, RECEIPT_DEFECT_CLASS, message)
}

#[cfg(test)]
mod tests {
    use super::*;
    use serde_json::Value;

    #[test]
    fn empty_blake3_constant_matches_empty_input() {
        assert_eq!(blake3_bytes(b""), BLAKE3_EMPTY);
    }

    #[test]
    fn blake3_is_deterministic() {
        let a = blake3_str("portfolio-obl-0002");
        let b = blake3_str("portfolio-obl-0002");
        assert_eq!(a, b, "same input must produce same hash");
        let c = blake3_str("portfolio-obl-0003");
        assert_ne!(a, c, "different input must produce different hash");
    }

    #[test]
    fn blake3_output_format_is_prefixed_hex() {
        let h = blake3_str("x");
        assert!(h.starts_with("blake3:"), "must include algorithm prefix");
        let hex = h.trim_start_matches("blake3:");
        assert_eq!(hex.len(), 64, "blake3 hex digest is 64 chars");
        assert!(hex
            .chars()
            .all(|c| c.is_ascii_hexdigit() && !c.is_uppercase()));
    }

    #[test]
    fn evidence_builder_composite_is_order_independent() {
        let mut a = EvidenceBuilder::new();
        a.add_str("alpha", "AAA")
            .add_str("beta", "BBB")
            .add_str("gamma", "CCC");
        let mut b = EvidenceBuilder::new();
        b.add_str("gamma", "CCC")
            .add_str("alpha", "AAA")
            .add_str("beta", "BBB");
        let ea = a.finalize("wu");
        let eb = b.finalize("wu");
        assert_eq!(ea.evidence_hash, eb.evidence_hash);
    }

    #[test]
    fn evidence_verify_round_trips() {
        let mut b = EvidenceBuilder::new();
        b.add_str("input", "hello").add_str("output", "world");
        let ev = b.finalize("portfolio-obl-0002");
        assert!(ev.verify(), "freshly built evidence must verify");
        let s = serde_json::to_string(&ev).unwrap();
        let parsed: ReceiptEvidence = serde_json::from_str(&s).unwrap();
        assert!(parsed.verify(), "round-tripped evidence must still verify");
    }

    #[test]
    fn evidence_verify_detects_tampering() {
        let mut b = EvidenceBuilder::new();
        b.add_str("input", "hello");
        let mut ev = b.finalize("portfolio-obl-0002");
        ev.entries.insert("input".into(), blake3_str("tampered"));
        assert!(!ev.verify(), "tampered entries must fail verify");
    }

    #[test]
    fn receipt_defect_envelope_is_typed_json() {
        let env = receipt_defect("mcpp.receipt.verify", "mcpp", "evidence missing");
        let s = env.to_json();
        let v: Value = serde_json::from_str(&s).expect("must be valid JSON");
        assert_eq!(v["status"], "fail");
        assert_eq!(v["errors"][0]["class"], "RECEIPT_DEFECT");
        assert_eq!(v["errors"][0]["message"], "evidence missing");
    }

    #[test]
    fn evidence_carries_required_keys_inside_envelope() {
        let mut b = EvidenceBuilder::new();
        b.add_str("accepted_delta_hash", "delta-bytes")
            .add_str("spec_hash", "spec-bytes")
            .add_str("plan_hash", "plan-bytes")
            .add_str("tasks_hash", "tasks-bytes")
            .add_str("test_result_hash", "test-bytes")
            .add_str("verify_report_hash", "verify-bytes")
            .add_str("state_before_hash", "before-bytes")
            .add_str("state_after_hash", "after-bytes");
        let ev = b.finalize("mcpp-first-control-loop");
        let env = Envelope::pass("mcpp.receipt.verify", "mcpp")
            .with_data(serde_json::to_value(&ev).unwrap());
        let v: Value = serde_json::from_str(&env.to_json()).unwrap();
        for key in [
            "accepted_delta_hash",
            "spec_hash",
            "plan_hash",
            "tasks_hash",
            "test_result_hash",
            "verify_report_hash",
            "state_before_hash",
            "state_after_hash",
        ] {
            assert!(
                v["data"]["entries"][key].is_string(),
                "envelope.data.entries.{key} must be present"
            );
        }
        assert_eq!(v["data"]["schema"], RECEIPT_EVIDENCE_SCHEMA);
    }
}
