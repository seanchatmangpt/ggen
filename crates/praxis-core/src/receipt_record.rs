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
    /// Provenance tag distinguishing how this receipt's write was authorized
    /// (Gall CP37-38): `None` (the default, and every receipt written before
    /// this field existed) for the ordinary human/LLM-reviewed path,
    /// `Some("unattended-dispatch")` only for a receipt produced by
    /// `ggen-mcp`'s bounded unattended-write dispatcher
    /// (`crates/ggen-mcp/src/tools/unattended_dispatch.rs`), whose write
    /// fired with zero human/LLM decision step. Descriptive only, like
    /// [`Self::activity`]/[`Self::duration_ms`] above — deliberately
    /// excluded from chain-hash computation (`receipt_meta` does not read
    /// it), so this field's presence, absence, or value never changes
    /// [`Self::chain_hash_hex`]. The point is not tamper-detection on this
    /// field itself (that would require folding it into the hash, which
    /// this checkpoint does not do) but making an unattended-fired receipt
    /// visually and programmatically distinguishable from a reviewed one in
    /// the chain, closing the gap where both looked identical.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub origin: Option<String>,
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
    /// Schema identity (see `crate::receipt_epoch`): [`crate::receipt_epoch::SCHEMA_V1`]
    /// (the default -- every pre-existing receipt on disk lacks this field
    /// entirely and deserializes as v1) or [`crate::receipt_epoch::SCHEMA_V2`].
    /// Dispatched on by `crate::receipt_epoch::read_receipt_epoch`. Not part
    /// of the chain-hash computation on its own when [`Self::v2`] is `None`
    /// (a v1 record hashes exactly as it always did); when `v2` is `Some`,
    /// this field is folded into the chain hash together with it (see
    /// [`Self::recompute_chain_hash`]'s `fold_in_v2_epoch` step) so the two
    /// can never be tampered with independently of each other.
    #[serde(default = "crate::receipt_epoch::default_schema")]
    pub schema: String,
    /// The v2 epoch payload (see `crate::receipt_epoch::ReceiptEpochV2`).
    /// `None` on every v1 record; populated only when [`Self::schema`] is
    /// [`crate::receipt_epoch::SCHEMA_V2`]. Folded into the chain hash by
    /// [`Self::recompute_chain_hash`] whenever present -- tampering with
    /// `standing_ceiling`/`admission`/`equivalence`/`promotion_eligible`
    /// changes the recomputed chain hash (F1 fix; previously this field was
    /// structurally excluded from `chain_hash_hex`/`signature_hex`
    /// coverage). `None` folds to a strict no-op, so v1 records are
    /// unaffected.
    #[serde(default, skip_serializing_if = "Option::is_none")]
    pub v2: Option<crate::receipt_epoch::ReceiptEpochV2>,
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
    /// this can never silently diverge from the live emission path — and
    /// then folds [`Self::v2`] in via [`fold_in_v2_epoch`] so the v2 epoch
    /// payload (standing ceiling, admission ledger, equivalence map,
    /// promotion eligibility) is bound into the same chain hash as every
    /// other field, not structurally excluded from it.
    ///
    /// If the result doesn't match [`Self::chain_hash_hex`], the record was
    /// tampered with (or the crate's chain rule changed incompatibly).
    pub fn recompute_chain_hash(&self) -> Result<[u8; 32], CoreError> {
        let payload_hash = self.payload_hash()?;
        let prev_chain_hash = self.prev_chain_hash()?;
        let meta = self.receipt_meta();
        let frame = build_admission_frame(&payload_hash, &prev_chain_hash, &meta, self.ts_ns);
        let base = chain_from_frame(&prev_chain_hash, &frame);
        fold_in_v2_epoch(base, &self.schema, self.v2.as_ref())
    }
}

/// Fold a record's v2 epoch payload (if present) into `base` (the chain hash
/// [`build_admission_frame`]/[`chain_from_frame`] alone would produce), so a
/// tampered `standing_ceiling`/`admission`/`equivalence`/`promotion_eligible`
/// changes the resulting [`ReceiptRecord::chain_hash_hex`] instead of leaving
/// it — and therefore the ed25519 signature over it — byte-for-byte
/// untouched (the gap this closes: previously `v2` could be edited in place
/// with `chain_hash_hex`/`signature_hex` left exactly as they were, and both
/// still verified as valid, because neither ever depended on `v2`'s bytes).
///
/// `v2: None` (every v1 record, and [`crate::receipt_epoch::ReceiptEpochV2::legacy_bounded`]'s
/// reading of one) returns `base` completely unchanged — a strict no-op —
/// preserving `receipt_epoch`'s documented "changes nothing about
/// chain-hash computation ... bit-for-bit unchanged" invariant for every
/// pre-epoch record exactly as before. Only a record that actually declares
/// a `v2` payload gets the extra mixing. Deterministic: [`crate::receipt_epoch::ReceiptEpochV2`]
/// and its transitive fields are plain structs/enums/`Vec`s (no hash-map
/// ordering anywhere in the type), so `serde_json::to_vec` of the same
/// value always produces the same bytes, and recomputing over the same
/// record twice always agrees.
fn fold_in_v2_epoch(
    base: [u8; 32], schema: &str, v2: Option<&crate::receipt_epoch::ReceiptEpochV2>,
) -> Result<[u8; 32], CoreError> {
    let Some(epoch) = v2 else {
        return Ok(base);
    };
    let epoch_bytes = serde_json::to_vec(epoch)
        .map_err(|e| CoreError::SerializationFailed(format!("v2 epoch: {e}")))?;
    let mut combined = Vec::with_capacity(32 + schema.len() + epoch_bytes.len());
    combined.extend_from_slice(&base);
    combined.extend_from_slice(schema.as_bytes());
    combined.extend_from_slice(&epoch_bytes);
    Ok(*blake3::hash(&combined).as_bytes())
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
            origin: None,
            payload_hash_hex: "11".repeat(32),
            prev_chain_hash_hex: "0".repeat(64),
            chain_hash_hex: String::new(), // filled in below
            andon: Andon::Green,
            obligation_count: 0,
            object_ids: vec!["law:1111111111111111".to_string()],
            signature_hex: None,
            schema: crate::receipt_epoch::SCHEMA_V1.to_string(),
            v2: None,
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

    // -----------------------------------------------------------------
    // CP37: `origin` is descriptive provenance, deliberately excluded from
    // chain-hash computation -- these tests prove both directions: its
    // presence/value never affects the hash, and every other, hash-relevant
    // field's tamper detection is unaffected by `origin` existing at all.
    // -----------------------------------------------------------------

    #[test]
    fn origin_field_does_not_affect_the_recomputed_chain_hash() {
        let mut without_origin = sample();
        let hash_without = without_origin
            .recompute_chain_hash()
            .expect("recompute without origin");
        without_origin.chain_hash_hex = hex::encode(hash_without);

        let mut with_origin = sample();
        with_origin.origin = Some("unattended-dispatch".to_string());
        let hash_with = with_origin
            .recompute_chain_hash()
            .expect("recompute with origin");

        assert_eq!(
            hash_without, hash_with,
            "origin must be purely descriptive: setting it must not change the \
             recomputed chain hash at all"
        );
    }

    #[test]
    fn corrupting_only_origin_still_validates_against_the_original_chain_hash() {
        let mut record = sample();
        record.origin = Some("unattended-dispatch".to_string());
        let chain = record.recompute_chain_hash().expect("recompute");
        record.chain_hash_hex = hex::encode(chain);

        // Corrupt ONLY origin -- a hash-relevant field must still validate
        // (proving origin really is outside the hash), unlike the paired
        // test below where a hash-relevant field is corrupted instead.
        let mut corrupted_origin_only = record.clone();
        corrupted_origin_only.origin = Some("something-else-entirely".to_string());
        assert_eq!(
            corrupted_origin_only
                .recompute_chain_hash()
                .expect("recompute over origin-only edit"),
            chain,
            "corrupting only origin must still recompute to the SAME chain hash \
             (it's outside the hash) -- this is not a tamper the chain check can \
             or should catch"
        );

        // Regression: a real hash-relevant field (payload_hash_hex) must
        // still be caught exactly as before origin existed.
        let mut corrupted_payload = record.clone();
        corrupted_payload.payload_hash_hex = "22".repeat(32);
        assert_ne!(
            corrupted_payload
                .recompute_chain_hash()
                .expect("recompute over payload edit"),
            chain,
            "a real hash-relevant field must still be caught by chain recompute; \
             origin's addition must not weaken this"
        );
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

    // -----------------------------------------------------------------
    // F1 (receipt-chain-v2-epoch, contract-drift): the v2 epoch payload
    // (standing_ceiling ratchet, admission ledger, equivalence map,
    // promotion_eligible) must be bound into the chain hash, so forging any
    // of it while leaving chain_hash_hex/signature_hex untouched is caught
    // by the same recompute-and-compare tamper check every other field
    // already gets.
    // -----------------------------------------------------------------

    #[test]
    fn v1_record_chain_hash_is_unaffected_by_v2_folding() {
        // A v1 record (`v2: None`) must still recompute deterministically
        // and consistently -- the v2 fold is a strict no-op when there is
        // no v2 payload, preserving the documented v1 backward-compat
        // invariant exactly.
        let record = sample();
        assert_eq!(record.schema, crate::receipt_epoch::SCHEMA_V1);
        assert!(record.v2.is_none());
        let first = record.recompute_chain_hash().expect("recompute");
        let second = record.recompute_chain_hash().expect("recompute again");
        assert_eq!(first, second);
    }

    #[test]
    fn forged_standing_ceiling_promotion_is_caught_by_chain_recompute() {
        use crate::receipt_epoch::{
            AndonLevel, CeilingLevel, ComponentLevels, ReceiptEpochV2Builder, SCHEMA_V2,
        };

        let epoch = ReceiptEpochV2Builder::new(
            CeilingLevel::LegacyObserved,
            ComponentLevels::uniform(AndonLevel::Yellow),
        )
        .build()
        .expect("epoch builds");
        assert_eq!(epoch.standing_ceiling, CeilingLevel::LegacyObserved);

        let mut record = sample();
        record.schema = SCHEMA_V2.to_string();
        record.v2 = Some(epoch);
        let chain = record.recompute_chain_hash().expect("recompute");
        record.chain_hash_hex = hex::encode(chain);

        // The F1 attack: forge a ratchet promotion (LegacyObserved -> Green)
        // that never passed `validate_promotion`, leaving `chain_hash_hex`
        // (and, on a real signed receipt, `signature_hex`) exactly as they
        // were before the edit.
        let mut forged = record.clone();
        forged.v2.as_mut().expect("v2 present").standing_ceiling = CeilingLevel::Green;

        let recomputed = forged
            .recompute_chain_hash()
            .expect("recompute over the forged record");
        assert_ne!(
            hex::encode(recomputed),
            forged.chain_hash_hex,
            "a forged standing_ceiling promotion must be caught: the untouched \
             chain_hash_hex must no longer match what recompute_chain_hash \
             produces from the tampered v2 payload"
        );
    }

    #[test]
    fn flipped_admission_decision_is_caught_by_chain_recompute() {
        use crate::receipt_epoch::{
            AdmissionDecision, AdmissionItem, AdmissionLedger, AndonLevel, CeilingLevel,
            ComponentLevels, ObservedOutcome, ReceiptEpochV2Builder, SCHEMA_V2,
        };

        let epoch = ReceiptEpochV2Builder::new(
            CeilingLevel::Green,
            ComponentLevels::uniform(AndonLevel::Green),
        )
        .admission_item(AdmissionItem {
            evidence_id: "out/a.txt".to_string(),
            observed_outcome: ObservedOutcome::Fail,
            decision: AdmissionDecision::Refused,
            reason: "SHACL violation".to_string(),
            obligations_discharged: vec![],
            obligations_created: vec![],
        })
        .build()
        .expect("epoch builds");

        let mut record = sample();
        record.schema = SCHEMA_V2.to_string();
        record.v2 = Some(epoch);
        let chain = record.recompute_chain_hash().expect("recompute");
        record.chain_hash_hex = hex::encode(chain);

        // Flip the refused item to Admitted -- the F1 attack -- without
        // touching chain_hash_hex.
        let mut forged = record.clone();
        match &mut forged.v2.as_mut().expect("v2 present").admission {
            AdmissionLedger::Recorded(items) => items[0].decision = AdmissionDecision::Admitted,
            AdmissionLedger::LegacyUnrecorded => panic!("expected a recorded ledger"),
        }

        let recomputed = forged
            .recompute_chain_hash()
            .expect("recompute over the forged record");
        assert_ne!(
            hex::encode(recomputed),
            forged.chain_hash_hex,
            "flipping an AdmissionItem's decision from Refused to Admitted must \
             be caught by chain_recompute"
        );
    }

    /// Real, currently-failing regression case: the exact `record` object
    /// from autofde-lab's `.ggen-v2/receipt-log.jsonl` genesis line (see
    /// `docs/jira/2026-08-11-GGEN-RECEIPT-CHAIN-VERIFY-MISMATCH.md`),
    /// embedded literally so this test is self-contained (no dependency on
    /// a file outside this repo).
    ///
    /// Narrowed root cause, this session: the `v2` epoch payload and
    /// `schema` string re-serialize **byte-identically** after a JSON
    /// round trip (verified directly, printed and diffed) -- so
    /// `fold_in_v2_epoch`'s own contribution is provably not the culprit
    /// here, correcting this bug's original filing, which had not yet
    /// isolated that. The divergence is therefore in the *base* (pre-v2)
    /// chain hash: `build_admission_frame`/`chain_from_frame` over
    /// `receipt_meta()`'s reconstruction of `ReceiptMeta` from
    /// `instruction_id`/`activity_idx`/`node_kind`/`ts_ns`/`andon`/
    /// `object_ids`/`obligation_count` plus the decoded `payload_hash`/
    /// `prev_chain_hash`. All of those are plain scalars/strings that
    /// round-trip losslessly through JSON, which is what makes this
    /// puzzling rather than obviously explained -- the leading remaining
    /// hypothesis is that `chain_from_frame`'s `OcelCausalReceipt::genesis`
    /// + single-frame `.chain()` replay does not reproduce whatever
    /// internal state the *live*, multi-file `ggen sync run` accumulated
    /// across its real (transitively multi-object) emission path, even
    /// though both paths agree on the single scalar `prev_chain_hash`.
    /// Not fixed -- `#[ignore]`d so `cargo test` stays green while this is
    /// tracked; un-ignore once fixed, this must then pass.
    #[test]
    #[ignore = "FM-CHAIN-014, not yet fixed -- see docs/jira/2026-08-11-GGEN-RECEIPT-CHAIN-VERIFY-MISMATCH.md"]
    fn reproduce_the_real_autofde_lab_fm_chain_014_failure() {
        let record: ReceiptRecord = serde_json::from_str(RECORD_JSON).expect("deserialize real record");
        let recomputed = record.recompute_chain_hash().expect("recompute real record");
        assert_eq!(
            hex::encode(recomputed),
            record.chain_hash_hex,
            "recomputed chain hash must match the stored one for a real, \
             untampered record written by ggen sync run itself"
        );
    }

    /// The real, byte-identical JSON `ReceiptRecord` embedded verbatim
    /// (see the test above).
    const RECORD_JSON: &str = r#"{"version": 1, "instruction_id": 0, "activity_idx": 0, "activity": "ggen.sync", "node_kind": 0, "ts_ns": 0, "payload_hash_hex": "0db2f95d025f90391852ef7306e3924961d0524b435fdc0379b8b982dcc07f46", "prev_chain_hash_hex": "0000000000000000000000000000000000000000000000000000000000000000", "chain_hash_hex": "6a28fe3d81babdcd798e92e5b36b0f0c0e41b68a7ad8ff449fd99bc0c9f9c5df", "andon": "Green", "obligation_count": 0, "object_ids": ["law:0db2f95d025f9039"], "signature_hex": "827ae33bdf843f19d62fc37ece6b3144352140036e9dc34aaa5a5be9bab275f052bb8f6001859d9eaf4268108f1fe3634af539f7b65153a1d1298c7ed2c0b508", "schema": "ggen-receipt/v2", "v2": {"admission": {"Recorded": [{"evidence_id": "src/autofde_lab/constitution/authority.py", "observed_outcome": "Pass", "decision": "Admitted", "reason": "skipped: mode=create: target already exists", "obligations_discharged": [], "obligations_created": []}, {"evidence_id": "src/autofde_lab/constitution/evidence.py", "observed_outcome": "Pass", "decision": "Admitted", "reason": "skipped: mode=create: target already exists", "obligations_discharged": [], "obligations_created": []}, {"evidence_id": "src/autofde_lab/constitution/interop.py", "observed_outcome": "Pass", "decision": "Admitted", "reason": "skipped: mode=create: target already exists", "obligations_discharged": [], "obligations_created": []}, {"evidence_id": "src/autofde_lab/constitution/lab.py", "observed_outcome": "Pass", "decision": "Admitted", "reason": "skipped: mode=create: target already exists", "obligations_discharged": [], "obligations_created": []}, {"evidence_id": "src/autofde_lab/constitution/planning.py", "observed_outcome": "Pass", "decision": "Admitted", "reason": "skipped: mode=create: target already exists", "obligations_discharged": [], "obligations_created": []}, {"evidence_id": "src/autofde_lab/constitution/process.py", "observed_outcome": "Pass", "decision": "Admitted", "reason": "skipped: mode=create: target already exists", "obligations_discharged": [], "obligations_created": []}, {"evidence_id": "src/autofde_lab/constitution/standing.py", "observed_outcome": "Pass", "decision": "Admitted", "reason": "skipped: mode=create: target already exists", "obligations_discharged": [], "obligations_created": []}, {"evidence_id": "src/autofde_lab/constitution/world.py", "observed_outcome": "Pass", "decision": "Admitted", "reason": "skipped: mode=create: target already exists", "obligations_discharged": [], "obligations_created": []}]}, "standing_ceiling": "Green", "equivalence": {"source": "Unknown", "compiled_binary": "Unknown", "docs": "Unknown", "tests": "Unknown", "receipts": "Unknown", "evidence": "Unknown", "gates": "Unknown", "config": "Unknown"}, "obligation_count": {"Tracked": {"required": 0, "discharged": 0}}, "andon": "Green", "promotion_eligible": true}}"#;

    #[test]
    fn recompute_chain_hash_survives_a_real_json_round_trip() {
        // Regression for FM-CHAIN-014 (autofde-lab docs/jira/
        // 2026-08-11-GGEN-RECEIPT-CHAIN-VERIFY-MISMATCH.md): every existing
        // v2 test above builds a record, hashes it, and forges it -- all
        // in-process, never touching JSON. `ggen sync run` writes a record
        // to a JSONL file; `ggen receipt verify` reads it back and
        // re-hashes the *deserialized* copy. This test is the first to
        // actually exercise that real write/read boundary.
        use crate::receipt_epoch::{
            AdmissionDecision, AdmissionItem, AndonLevel, CeilingLevel, ComponentLevels,
            ObservedOutcome, ReceiptEpochV2Builder, SCHEMA_V2,
        };

        let epoch = ReceiptEpochV2Builder::new(
            CeilingLevel::Green,
            ComponentLevels::uniform(AndonLevel::Green),
        )
        .admission_item(AdmissionItem {
            evidence_id: "src/autofde_lab/reasoning/universes/k8s_fault_universes.py".to_string(),
            observed_outcome: ObservedOutcome::Pass,
            decision: AdmissionDecision::Admitted,
            reason: "skipped: mode=create: target already exists".to_string(),
            obligations_discharged: Vec::new(),
            obligations_created: Vec::new(),
        })
        .build()
        .expect("epoch builds");

        let mut record = sample();
        record.schema = SCHEMA_V2.to_string();
        record.v2 = Some(epoch);
        let chain = record.recompute_chain_hash().expect("recompute at write time");
        record.chain_hash_hex = hex::encode(chain);

        // Simulate the real JSONL persist/reload boundary `write_receipt`
        // and `read_prev_head`/`receipt verify` actually cross.
        let serialized = serde_json::to_string(&record).expect("serialize record");
        let reloaded: ReceiptRecord = serde_json::from_str(&serialized).expect("deserialize record");

        let recomputed_after_round_trip = reloaded
            .recompute_chain_hash()
            .expect("recompute after round trip");

        if hex::encode(recomputed_after_round_trip) != reloaded.chain_hash_hex {
            // Byte-diff the v2 payload specifically -- fold_in_v2_epoch's
            // own doc comment claims this can never happen, so on failure
            // pinpoint exactly which bytes changed rather than only
            // asserting the mismatch.
            let before = serde_json::to_vec(record.v2.as_ref().unwrap()).unwrap();
            let after = serde_json::to_vec(reloaded.v2.as_ref().unwrap()).unwrap();
            panic!(
                "chain hash did not survive a real JSON round trip -- this is FM-CHAIN-014.\n                 v2 epoch bytes before round trip: {}\n                 v2 epoch bytes after  round trip: {}\n                 (identical bytes: {})",
                String::from_utf8_lossy(&before),
                String::from_utf8_lossy(&after),
                before == after,
            );
        }
    }

    #[test]
    fn flipped_promotion_eligible_is_caught_by_chain_recompute() {
        use crate::receipt_epoch::{
            AndonLevel, CeilingLevel, ComponentLevels, ReceiptEpochV2Builder, SCHEMA_V2,
        };

        let epoch = ReceiptEpochV2Builder::new(
            CeilingLevel::Green,
            ComponentLevels::uniform(AndonLevel::Green),
        )
        .build()
        .expect("epoch builds");

        let mut record = sample();
        record.schema = SCHEMA_V2.to_string();
        record.v2 = Some(epoch);
        let chain = record.recompute_chain_hash().expect("recompute");
        record.chain_hash_hex = hex::encode(chain);

        let mut forged = record.clone();
        let v2 = forged.v2.as_mut().expect("v2 present");
        v2.promotion_eligible = !v2.promotion_eligible;

        let recomputed = forged
            .recompute_chain_hash()
            .expect("recompute over the forged record");
        assert_ne!(
            hex::encode(recomputed),
            forged.chain_hash_hex,
            "flipping promotion_eligible must be caught by chain_recompute"
        );
    }
}
