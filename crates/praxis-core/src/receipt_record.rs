//! `ReceiptRecord` — a persisted, replayable snapshot of everything
//! [`crate::law::LawObject::receipt_with_record`] computes.
//!
//! A `ReceiptRecord` is deliberately independent of the `Payload`/`Law` type
//! parameters on [`crate::law::LawObject`], so it can be serialized, stored
//! (see [`crate::receipt_store`]), and validated (see
//! [`crate::receipt_validator`]) without needing the original typed object —
//! only its hashes, metadata, and Andon outcome survive to the ledger.

use serde::{Deserialize, Serialize};

use crate::{
    error::CoreError,
    law::{build_admission_frame, chain_from_frame, Andon, ReceiptMeta},
};

/// Current schema version for [`ReceiptRecord`]. Checked by
/// `crate::receipt_validator`'s `schema` stage; bump this if the wire shape
/// ever changes in a way that would break `recompute_chain_hash` against
/// records written by an older version.
pub const RECEIPT_RECORD_VERSION: u32 = 1;

/// A persisted snapshot of one `receipt()` call: enough to append to a JSONL
/// ledger, re-verify its chain hash later without the original `LawObject`,
/// and replay its lifecycle through the POWL token model.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub struct ReceiptRecord {
    /// Schema version; see [`RECEIPT_RECORD_VERSION`].
    pub version: u32,
    /// Monotonically increasing step identity within a run.
    pub instruction_id: u64,
    /// Index into the activity table for this step's activity.
    pub activity_idx: u16,
    /// Resolved human-readable label for `activity_idx`, if the caller has an
    /// activity table available. Not part of the chain-hash computation —
    /// purely descriptive.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub activity: Option<String>,
    /// Classifier byte for the POWL node kind (XOR, SEQ, LOOP, etc.).
    pub node_kind: u8,
    /// Wall-clock timestamp in nanoseconds (resolved at emission time; never
    /// `None` once persisted).
    pub ts_ns: u64,
    /// Optional wall-clock duration of the admission this receipt seals, in
    /// milliseconds. `None` when the emitting path did not measure a span:
    /// praxis's law layer records the emission instant [`Self::ts_ns`], not a
    /// duration, so this is `None` on the live `receipt_with_record` path;
    /// callers that time admission may populate it. Descriptive only — not
    /// part of the chain-hash computation (like [`Self::activity`]). Added for
    /// the `SharedReceiptV1` bridge, where it maps to `sr:duration_ms` (see
    /// `receipt_shacl` in the root crate); absent records read back as `None`.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub duration_ms: Option<u64>,
    /// BLAKE3 hash of the canonical JSON payload bytes, as 64 lowercase hex characters.
    pub payload_hash_hex: String,
    /// The chain hash this record was chained onto, as 64 lowercase hex characters.
    pub prev_chain_hash_hex: String,
    /// The resulting chain hash after this record, as 64 lowercase hex characters.
    pub chain_hash_hex: String,
    /// The Andon outcome at receipt time (`Green`/`Halted`/`Overridden`).
    pub andon: Andon,
    /// Number of obligations attached to the law object at receipt time.
    pub obligation_count: u32,
    /// OCEL object identifiers this receipt governs (E2O links). Defaults to
    /// a single synthetic `law:<payload_hash[..16]>` identifier when the
    /// caller doesn't supply richer object identity.
    #[serde(default)]
    pub object_ids: Vec<String>,
    /// Hex-encoded ed25519 signature over [`Self::chain_hash_hex`], present only when the record has been signed.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub signature_hex: Option<String>,
}

/// Decode a 64-lowercase-hex-character string into 32 raw bytes.
fn decode_hex32(field: &str, s: &str) -> Result<[u8; 32], CoreError> {
    let bytes = hex::decode(s).map_err(|e| CoreError::HexDecodeFailed(format!("{field}: {e}")))?;
    bytes.try_into().map_err(|v: Vec<u8>| {
        CoreError::HexDecodeFailed(format!("{field}: expected 32 bytes, got {}", v.len()))
    })
}

impl ReceiptRecord {
    /// Decode [`Self::payload_hash_hex`] into raw bytes.
    pub fn payload_hash(&self) -> Result<[u8; 32], CoreError> {
        decode_hex32("payload_hash_hex", &self.payload_hash_hex)
    }

    /// Decode [`Self::prev_chain_hash_hex`] into raw bytes.
    pub fn prev_chain_hash(&self) -> Result<[u8; 32], CoreError> {
        decode_hex32("prev_chain_hash_hex", &self.prev_chain_hash_hex)
    }

    /// Decode [`Self::chain_hash_hex`] into raw bytes.
    pub fn chain_hash(&self) -> Result<[u8; 32], CoreError> {
        decode_hex32("chain_hash_hex", &self.chain_hash_hex)
    }

    /// Rebuild the [`crate::law::ReceiptMeta`] this record was chained with
    /// (denial always resolves to `ADMITTED`: a receipt only ever exists for
    /// an object that reached the `Admitted` stage, so the frame that
    /// produced `chain_hash_hex` always carried `DenialPolarity::ADMITTED` —
    /// non-`ADMITTED` denial words are a `receipt()`-time detail this record
    /// does not currently persist).
    fn receipt_meta(&self) -> ReceiptMeta {
        ReceiptMeta {
            instruction_id: self.instruction_id,
            activity_idx: self.activity_idx,
            node_kind: self.node_kind,
            ts_ns: Some(self.ts_ns),
            andon: self.andon.clone(),
            object_ids: self.object_ids.clone(),
            obligation_count: self.obligation_count,
            ..Default::default()
        }
    }

    /// Recompute `chain_hash` from this record's own fields, using the exact
    /// same [`build_admission_frame`]/[`chain_from_frame`] construction
    /// `LawObject::receipt`/`receipt_with_record` use at emission time — so
    /// this can never silently diverge from the live emission path.
    ///
    /// If the result doesn't match [`Self::chain_hash_hex`], the record was
    /// tampered with (or the crate's chain rule changed incompatibly).
    pub fn recompute_chain_hash(&self) -> Result<[u8; 32], CoreError> {
        let payload_hash = self.payload_hash()?;
        let prev_chain_hash = self.prev_chain_hash()?;
        let meta = self.receipt_meta();
        let frame = build_admission_frame(&payload_hash, &prev_chain_hash, &meta, self.ts_ns);
        Ok(chain_from_frame(&prev_chain_hash, &frame))
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    fn sample() -> ReceiptRecord {
        ReceiptRecord {
            version: RECEIPT_RECORD_VERSION,
            instruction_id: 1,
            activity_idx: 0,
            activity: None,
            node_kind: 0,
            ts_ns: 42,
            duration_ms: None,
            payload_hash_hex: "11".repeat(32),
            prev_chain_hash_hex: "0".repeat(64),
            chain_hash_hex: String::new(), // filled in below
            andon: Andon::Green,
            obligation_count: 0,
            object_ids: vec!["law:1111111111111111".to_string()],
            signature_hex: None,
        }
    }

    #[test]
    fn recompute_matches_a_freshly_computed_chain_hash() {
        let mut record = sample();
        let chain_hash = record.recompute_chain_hash().expect("recompute");
        record.chain_hash_hex = hex::encode(chain_hash);
        // Recomputing again from the now-filled-in record must agree.
        assert_eq!(
            record.recompute_chain_hash().expect("recompute"),
            chain_hash
        );
    }

    #[test]
    fn tampered_payload_hash_changes_recomputed_chain_hash() {
        let mut record = sample();
        let original = record.recompute_chain_hash().expect("recompute");
        record.payload_hash_hex = "22".repeat(32);
        let tampered = record.recompute_chain_hash().expect("recompute");
        assert_ne!(original, tampered);
    }

    #[test]
    fn malformed_hex_field_is_an_error() {
        let mut record = sample();
        record.payload_hash_hex = "not-hex".to_string();
        assert!(record.recompute_chain_hash().is_err());
    }

    #[test]
    fn wrong_length_hex_field_is_an_error() {
        let mut record = sample();
        record.payload_hash_hex = "ab".to_string(); // 1 byte, not 32
        assert!(record.recompute_chain_hash().is_err());
    }
}
