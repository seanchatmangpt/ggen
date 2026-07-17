//! Fused Law Object: obligation + lifecycle + receipt + OCEL in one type.

use std::{
    marker::PhantomData,
    time::{SystemTime, UNIX_EPOCH},
};

use bcinr_powl_receipt::{
    causal_receipt::{OcelCausalFrame, OcelCausalReceipt, PackedObjRef},
    denial::DenialPolarity,
};
use serde::{Deserialize, Serialize};

use crate::lifecycle::{sealed::Stage, Admitted, Raw, Receipted, Validated};

/// Precondition or blocking constraint a LawObject must satisfy before admission.
/// Hashable and dispatchable: obligations are first-class values, not closures.
#[derive(Debug, Clone, PartialEq, Eq, Hash, Serialize, Deserialize)]
pub enum Obligation {
    /// A predicate that must be satisfied.
    Precondition {
        /// Identifier for the predicate being checked.
        predicate_id: String,
        /// Hash of the parameters passed to the predicate.
        params_hash: [u8; 32],
    },
    /// A hard constraint that blocks progress until lifted.
    BlockingConstraint {
        /// Human-readable reason for the block.
        reason: String,
    },
    /// External evidence must be provided.
    EvidenceRequired {
        /// Type or category of evidence needed.
        evidence_type: String,
    },
}

/// Convert an Obligation::Precondition into a PDDL precondition for wasm4pm-compat.
/// BlockingConstraint and EvidenceRequired variants cannot be converted and return an error.
impl TryFrom<Obligation> for wasm4pm_compat::pddl::Precondition {
    type Error = String;

    fn try_from(obligation: Obligation) -> Result<Self, Self::Error> {
        match obligation {
            Obligation::Precondition {
                predicate_id,
                params_hash,
            } => Ok(wasm4pm_compat::pddl::Precondition {
                predicate_id,
                params_hash: Some(params_hash),
            }),
            Obligation::BlockingConstraint { reason } => Err(format!(
                "Cannot convert BlockingConstraint to Precondition: {}",
                reason
            )),
            Obligation::EvidenceRequired { evidence_type } => Err(format!(
                "Cannot convert EvidenceRequired to Precondition: {}",
                evidence_type
            )),
        }
    }
}

/// Halt/override signal: unmet obligations halt progress.
/// Halted state persists until explicitly cleared by a receipt or logged override.
#[derive(Debug, Clone, PartialEq, Serialize, Deserialize)]
pub enum Andon {
    /// All obligations satisfied; proceed.
    Green,
    /// Obligations unmet; halted with timestamp.
    Halted {
        /// Obligations blocking progress.
        unmet: Vec<Obligation>,
        /// Refusal-taxonomy classification of *why* progress halted (see
        /// [`crate::refusal`]). `unmet` and `refusals` are not required to be
        /// the same length: `refusals` also carries non-obligation halts
        /// (prolog8 Kernel denials, andon-ring invariant violations) that
        /// have no corresponding `Obligation` value.
        ///
        /// `#[serde(default)]` so JSON produced before this field existed
        /// still deserializes (empty `refusals`).
        #[serde(default)]
        refusals: Vec<crate::refusal::RefusalScenario>,
        /// Timestamp (typically milliseconds since epoch) when halt occurred.
        at: u64,
    },
    /// Obligations were overridden; halt lifted with reason and timestamp.
    Overridden {
        /// Who or what authorized the override.
        by: String,
        /// Reason for override.
        reason: String,
        /// Timestamp when override occurred.
        at: u64,
    },
}

/// Fused obligation/lifecycle/receipt/event object.
///
/// Type parameters:
/// - `Payload` — domain value under judgment.
/// - `S` — lifecycle stage (Raw → Validated → Admitted → Receipted), compile-time enforced.
/// - `Law` — zero-sized marker identifying the obligation set/policy in force.
///
/// Fields carry:
/// - Obligation list (Law Object pattern).
/// - Andon halt/override state (Andon defect signaling).
/// - Chain hash + signature (Receipt Chain pattern).
/// - Type parameters encode compile-time lifecycle transitions (Typestate pattern).
/// - OCEL event conversion available when receipted (feature-gated).
pub struct LawObject<Payload, S: Stage, Law> {
    /// The domain payload under judgment.
    pub payload: Payload,
    /// Obligations that must be satisfied for admission.
    pub obligations: Vec<Obligation>,
    /// Halt/override status.
    pub andon: Andon,
    /// Chain hash, set once receipted (append-only audit chain).
    pub chain_hash: Option<[u8; 32]>,
    /// Ed25519 signature, set iff `signed` feature used and object receipted.
    pub signature: Option<Vec<u8>>,
    /// Phantom marker for lifecycle stage.
    _stage: PhantomData<S>,
    /// Phantom marker for law/policy identity.
    _law: PhantomData<Law>,
}

/// Trait for evaluating obligations and transitioning Raw → Validated.
/// Failure returns the object in Andon::Halted state, not a bare Err.
pub trait Judge {
    /// The domain payload type.
    type Payload;
    /// The law/policy marker type.
    type Law;
    /// Error type (for diagnostic context, not for obligation failure).
    type Error;

    /// Evaluate all obligations on a raw object.
    /// Returns Ok(validated) if all pass; Err(raw_in_halted_state) if any fail.
    ///
    /// The `Err` variant intentionally carries the full `LawObject` back (not a
    /// lightweight error) so callers can inspect `Andon::Halted` on the object
    /// itself; pre-existing/typestate-shaped, not a new design in this change.
    #[allow(clippy::type_complexity, clippy::result_large_err)]
    fn judge(
        raw: LawObject<Self::Payload, Raw, Self::Law>,
    ) -> Result<
        LawObject<Self::Payload, Validated, Self::Law>,
        LawObject<Self::Payload, Raw, Self::Law>,
    >;
}

/// Trait for admitting a validated object.
pub trait Admit {
    /// The domain payload type.
    type Payload;
    /// The law/policy marker type.
    type Law;
    /// Witness/evidence type (for future subpoena or audit use).
    type Witness;

    /// Admit a validated object to the admitted state.
    /// Returns Ok(admitted) or Err(andon) if admission is denied.
    fn admit(
        validated: LawObject<Self::Payload, Validated, Self::Law>,
    ) -> Result<LawObject<Self::Payload, Admitted, Self::Law>, Andon>;
}

/// Parameters that bind a receipt to its position in an OCEL run.
///
/// These are the fields of [`OcelCausalFrame`] that identify *where* this
/// admission event sits in a larger process (as opposed to *what* payload it
/// carries, which is bound separately via `obj_refs`). Passing them in
/// explicitly (rather than hardcoding zero) lets callers place receipts at
/// the correct sequence position, activity, and POWL node classification.
#[derive(Debug, Clone)]
pub struct ReceiptMeta {
    /// Monotonically increasing step identity within a run.
    pub instruction_id: u64,
    /// Index into the activity table for this step's activity.
    pub activity_idx: u16,
    /// Classifier byte for the POWL node kind (XOR, SEQ, LOOP, etc.).
    pub node_kind: u8,
    /// Wall-clock timestamp in nanoseconds. `None` uses `SystemTime::now()`;
    /// `Some` allows deterministic/reproducible receipts (e.g. in tests).
    pub ts_ns: Option<u64>,
    /// The honest denial polarity for this receipt. `receipt()` writes this
    /// straight into the frame's `denial`/`fired_mask` instead of
    /// hardcoding `ADMITTED` — callers that observed non-fatal refusals
    /// (e.g. an `Andon::Overridden` admission, or Kernel/andon-ring
    /// refusals that didn't block) should compose them (see
    /// [`crate::refusal::compose_denials`]) and pass the result here so the
    /// receipt records what was actually waved through, not a fiction.
    /// Defaults to `DenialPolarity::ADMITTED`.
    pub denial: DenialPolarity,
    /// Halt/override status.
    pub andon: Andon,
    /// Associated object IDs.
    pub object_ids: Vec<String>,
    /// Number of obligations.
    pub obligation_count: u32,
}

impl Default for ReceiptMeta {
    /// `DenialPolarity` has no `Default` impl of its own (it's an external
    /// bitfield newtype with named constants, not a derived enum), so this
    /// is written by hand rather than derived; every other field's default
    /// matches what `#[derive(Default)]` would have produced.
    fn default() -> Self {
        Self {
            instruction_id: 0,
            activity_idx: 0,
            node_kind: 0,
            ts_ns: None,
            denial: DenialPolarity::ADMITTED,
            andon: Andon::Green,
            object_ids: Vec::new(),
            obligation_count: 0,
        }
    }
}

/// Build the [`OcelCausalFrame`] for one admission receipt.
///
/// This is the **single construction site** for the frame, shared by the
/// live emission path ([`LawObject::receipt`] / [`LawObject::receipt_with_record`]
/// below) and the persisted-replay path
/// (`crate::receipt_record::ReceiptRecord::recompute_chain_hash`), so the two
/// can never drift from each other — a receipt computed at emission time and
/// one recomputed later from its stored fields always agree on the chain
/// hash, or `chain_recompute` validation (tamper detection) correctly fails.
///
/// `ts_ns` must already be resolved by the caller (this function has no
/// access to a clock, by design: that's what makes recompute fully
/// deterministic from stored fields alone).
///
/// Packs the 32-byte `payload_hash` into the frame's 8 `obj_refs` slots as 8
/// little-endian u32 words, using the `PackedObjRef` tuple constructor
/// directly (not `PackedObjRef::new`, which packs a type index into the high
/// 8 bits and truncates the id to 24 bits) so the full 256-bit hash survives
/// intact as a payload commitment. This repurposes `obj_refs` (normally OCEL
/// object references) as a content commitment; no changes to
/// `bcinr-powl-receipt` are required since `to_hash_bytes()` already hashes
/// all 8 words verbatim.
///
/// `fired_mask`/`denial` are taken from `meta.denial` (default `ADMITTED`)
/// rather than hardcoded, so a receipt for an object that was overridden past
/// non-fatal refusals honestly records which lanes fired instead of always
/// claiming a clean admission.
pub(crate) fn build_admission_frame(
    payload_hash: &[u8; 32],
    prev_chain_hash: &[u8; 32],
    meta: &ReceiptMeta,
    ts_ns: u64,
) -> OcelCausalFrame {
    let meta_json =
        serde_json::to_vec(&(&meta.andon, &meta.object_ids, &meta.obligation_count)).unwrap();
    let mut combined = Vec::with_capacity(32 + meta_json.len());
    combined.extend_from_slice(payload_hash);
    combined.extend_from_slice(&meta_json);
    let mixed_hash = *blake3::hash(&combined).as_bytes();

    let mut obj_refs = [PackedObjRef::default(); 8];
    for (i, word) in mixed_hash.chunks_exact(4).enumerate() {
        let w = u32::from_le_bytes([word[0], word[1], word[2], word[3]]);
        obj_refs[i] = PackedObjRef(w);
    }

    OcelCausalFrame {
        instruction_id: meta.instruction_id,
        fired_mask: meta.denial.to_fired_mask(),
        denial: meta.denial,
        obj_refs,
        ts_ns,
        activity_idx: meta.activity_idx,
        node_kind: meta.node_kind,
        pad: [0u8; 5],
        prior_hash: *prev_chain_hash,
    }
}

/// Chain `frame` onto `prev_chain_hash` using bcinr-powl-receipt's causal
/// chain rule, returning the resulting chain hash.
///
/// NOTE: `prev_chain_hash` is intentionally mixed in twice here: once as
/// `frame.prior_hash` (hashed as part of the 99-byte frame body via
/// `to_hash_bytes()`), and again as the receipt's own seeded `chain_hash`
/// before `chain()` prepends it a second time (`chain_hash(t+1) =
/// BLAKE3(chain_hash(t) || frame_bytes(t+1))`). This double-mixing predates
/// this refactor and is kept unchanged for chain compatibility with any
/// receipts already computed by this code path; it does not weaken the
/// binding (it's still a deterministic function of `prev_chain_hash` and the
/// frame), it's simply more mixing than the minimal formula would require.
pub(crate) fn chain_from_frame(prev_chain_hash: &[u8; 32], frame: &OcelCausalFrame) -> [u8; 32] {
    let mut receipt = OcelCausalReceipt::genesis([0u8; 32]);
    receipt.chain_hash = *prev_chain_hash;
    receipt.chain(frame);
    receipt.chain_hash
}

impl<Payload: Serialize, Law> LawObject<Payload, Admitted, Law> {
    /// Shared core of `receipt`/`receipt_with_record`: hash the payload,
    /// resolve the timestamp, build the admission frame via
    /// [`build_admission_frame`], and chain it via [`chain_from_frame`].
    ///
    /// Returns `(payload_hash, resolved_ts_ns, chain_hash)`. Both public
    /// methods below share this so they can never compute different chain
    /// hashes for the same inputs.
    fn resolve_receipt(
        &self,
        prev_chain_hash: &[u8; 32],
        meta: &ReceiptMeta,
    ) -> Result<([u8; 32], u64, [u8; 32]), crate::error::CoreError> {
        // Serialize payload to canonical bytes via JSON, then bind it into the
        // frame by hashing it with blake3. Without this, two receipts for
        // different payloads (same everything else) would be indistinguishable.
        let payload_bytes = serde_json::to_vec(&self.payload)
            .map_err(|e| crate::error::CoreError::SerializationFailed(e.to_string()))?;
        let payload_hash: [u8; 32] = *blake3::hash(&payload_bytes).as_bytes();
        let payload_hash_hex = hex::encode(payload_hash);

        let mut meta = meta.clone();
        meta.andon = self.andon.clone();
        meta.obligation_count = self.obligations.len() as u32;
        if meta.object_ids.is_empty() {
            meta.object_ids = vec![format!("law:{}", &payload_hash_hex[..16])];
        }

        // Get current timestamp in nanoseconds since UNIX_EPOCH, unless the
        // caller supplied a deterministic timestamp via `meta.ts_ns`.
        let ts_ns = match meta.ts_ns {
            Some(t) => t,
            None => SystemTime::now()
                .duration_since(UNIX_EPOCH)
                .map_err(|e| {
                    crate::error::CoreError::SerializationFailed(format!("system time error: {e}"))
                })?
                .as_nanos() as u64,
        };

        let frame = build_admission_frame(&payload_hash, prev_chain_hash, &meta, ts_ns);
        let chain_hash = chain_from_frame(prev_chain_hash, &frame);

        Ok((payload_hash, ts_ns, chain_hash))
    }

    /// Consume an Admitted object and emit a Receipted object with computed chain hash.
    ///
    /// This is the admission-to-receipt transition: it appends to the chain
    /// using bcinr-powl-receipt's OCEL causal frame mechanism.
    /// The chain hash is computed as: BLAKE3(prev_chain_hash || ocel_frame_bytes)
    /// where the ocel_frame encodes the payload and admission metadata.
    ///
    /// Consumes self so an Admitted object cannot be receipted twice.
    pub fn receipt(
        mut self,
        prev_chain_hash: &[u8; 32],
        meta: ReceiptMeta,
    ) -> Result<LawObject<Payload, Receipted, Law>, crate::error::CoreError> {
        let (_payload_hash, _ts_ns, chain_hash) = self.resolve_receipt(prev_chain_hash, &meta)?;
        self.chain_hash = Some(chain_hash);

        // If the `signed` feature is enabled, sign the chain hash and fail
        // closed: a missing/invalid signing key must abort the receipt
        // rather than silently emitting an unsigned one. The feature exists
        // to *guarantee* a signature, so a soft failure here would defeat
        // the point.
        #[cfg(feature = "signed")]
        {
            self.signature = Some(crate::signing::sign_chain_hash(&chain_hash)?);
        }

        Ok(LawObject {
            payload: self.payload,
            obligations: self.obligations,
            andon: self.andon,
            chain_hash: self.chain_hash,
            signature: self.signature,
            _stage: PhantomData,
            _law: PhantomData,
        })
    }

    /// Same admission-to-receipt transition as [`LawObject::receipt`], but
    /// additionally returns a [`crate::receipt_record::ReceiptRecord`]
    /// snapshot of everything computed along the way — suitable for JSONL
    /// persistence ([`crate::receipt_store::ReceiptStore`]) and later replay
    /// verification ([`crate::receipt_validator::ReceiptValidator`]).
    ///
    /// Non-breaking addition alongside `receipt()`: both share
    /// [`LawObject::resolve_receipt`], so they can never compute different
    /// chain hashes for the same inputs.
    pub fn receipt_with_record(
        mut self,
        prev_chain_hash: &[u8; 32],
        meta: ReceiptMeta,
    ) -> Result<
        (
            LawObject<Payload, Receipted, Law>,
            crate::receipt_record::ReceiptRecord,
        ),
        crate::error::CoreError,
    > {
        let payload_bytes = serde_json::to_vec(&self.payload)
            .map_err(|e| crate::error::CoreError::SerializationFailed(e.to_string()))?;
        let payload_hash: [u8; 32] = *blake3::hash(&payload_bytes).as_bytes();
        let payload_hash_hex = hex::encode(payload_hash);

        let mut meta = meta;
        meta.andon = self.andon.clone();
        if meta.object_ids.is_empty() {
            meta.object_ids = vec![format!("law:{}", &payload_hash_hex[..16])];
        }

        let (_payload_hash, ts_ns, chain_hash) = self.resolve_receipt(prev_chain_hash, &meta)?;
        self.chain_hash = Some(chain_hash);

        #[cfg(feature = "signed")]
        {
            self.signature = Some(crate::signing::sign_chain_hash(&chain_hash)?);
        }

        let record = crate::receipt_record::ReceiptRecord {
            version: crate::receipt_record::RECEIPT_RECORD_VERSION,
            instruction_id: meta.instruction_id,
            activity_idx: meta.activity_idx,
            activity: None,
            node_kind: meta.node_kind,
            ts_ns,
            // The law layer records the emission instant, not a span; callers
            // that measure admission duration may set this on the record.
            duration_ms: None,
            payload_hash_hex: payload_hash_hex.clone(),
            prev_chain_hash_hex: hex::encode(prev_chain_hash),
            chain_hash_hex: hex::encode(chain_hash),
            andon: meta.andon.clone(),
            obligation_count: self.obligations.len() as u32,
            object_ids: meta.object_ids.clone(),
            signature_hex: None,
        };

        let receipted = LawObject {
            payload: self.payload,
            obligations: self.obligations,
            andon: self.andon,
            chain_hash: self.chain_hash,
            signature: self.signature,
            _stage: PhantomData,
            _law: PhantomData,
        };

        Ok((receipted, record))
    }
}

impl<Payload, S: Stage, Law> LawObject<Payload, S, Law> {
    /// Create a new raw (unevaluated) law object.
    pub fn new(payload: Payload, obligations: Vec<Obligation>) -> LawObject<Payload, Raw, Law> {
        LawObject {
            payload,
            obligations,
            andon: Andon::Green,
            chain_hash: None,
            signature: None,
            _stage: PhantomData,
            _law: PhantomData,
        }
    }

    /// Extract the payload from a law object (works at any stage).
    pub fn into_payload(self) -> Payload {
        self.payload
    }

    /// Borrow the payload.
    pub fn payload(&self) -> &Payload {
        &self.payload
    }

    /// Get the current Andon status.
    pub fn andon(&self) -> &Andon {
        &self.andon
    }

    /// Get the obligations list.
    pub fn obligations(&self) -> &[Obligation] {
        &self.obligations
    }

    /// Get the chain hash if set (only on Receipted objects).
    pub fn chain_hash(&self) -> Option<&[u8; 32]> {
        self.chain_hash.as_ref()
    }

    /// Rebuild this object with a different stage phantom marker.
    ///
    /// `_stage` and `_law` are private to this module, so external
    /// implementations of [`Judge`] and [`Admit`] (e.g. `DefaultLaw` in
    /// `default_law.rs`) cannot construct a `LawObject` directly in a new
    /// stage. This crate-internal helper performs the otherwise-mechanical
    /// Raw→Validated / Validated→Admitted transition without exposing the
    /// phantom fields outside `law.rs`. Callers are responsible for ensuring
    /// the transition is semantically valid (this helper does not itself
    /// enforce lifecycle ordering).
    pub(crate) fn transition<S2: Stage>(self) -> LawObject<Payload, S2, Law> {
        LawObject {
            payload: self.payload,
            obligations: self.obligations,
            andon: self.andon,
            chain_hash: self.chain_hash,
            signature: self.signature,
            _stage: PhantomData,
            _law: PhantomData,
        }
    }
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::lifecycle::Admitted;

    /// Test-only law marker; receipt() is generic over Law so any zero-sized
    /// type works here.
    struct TestLaw;

    fn admitted(payload: serde_json::Value) -> LawObject<serde_json::Value, Admitted, TestLaw> {
        LawObject {
            payload,
            obligations: vec![],
            andon: Andon::Green,
            chain_hash: None,
            signature: None,
            _stage: PhantomData,
            _law: PhantomData,
        }
    }

    fn fixed_meta(instruction_id: u64) -> ReceiptMeta {
        ReceiptMeta {
            instruction_id,
            activity_idx: 2,
            node_kind: 3,
            ts_ns: Some(42),
            ..Default::default()
        }
    }

    /// Set `PRAXIS_SIGNING_KEY` for the duration of the returned guard; see
    /// `signing::test_support::with_test_signing_key`.
    #[cfg(feature = "signed")]
    fn test_signing_env() -> std::sync::MutexGuard<'static, ()> {
        crate::signing::test_support::with_test_signing_key()
    }

    #[test]
    fn receipt_is_deterministic_for_identical_inputs() {
        #[cfg(feature = "signed")]
        let _guard = test_signing_env();
        let meta = fixed_meta(1);
        let prev = [7u8; 32];
        let r1 = admitted(serde_json::json!({"a": 1}))
            .receipt(&prev, meta.clone())
            .expect("receipt should succeed");
        let r2 = admitted(serde_json::json!({"a": 1}))
            .receipt(&prev, meta)
            .expect("receipt should succeed");
        assert_eq!(r1.chain_hash(), r2.chain_hash());
    }

    /// Regression test: before the Task 2 fix, payload bytes were serialized
    /// then discarded, so two receipts for different payloads (with the same
    /// prev hash, instruction_id, and timestamp) produced identical chain
    /// hashes. This must now fail (i.e. the hashes must differ).
    #[test]
    fn receipt_differs_for_different_payloads() {
        #[cfg(feature = "signed")]
        let _guard = test_signing_env();
        let meta = fixed_meta(1);
        let prev = [7u8; 32];
        let r1 = admitted(serde_json::json!({"a": 1}))
            .receipt(&prev, meta.clone())
            .expect("receipt should succeed");
        let r2 = admitted(serde_json::json!({"a": 2}))
            .receipt(&prev, meta)
            .expect("receipt should succeed");
        assert_ne!(r1.chain_hash(), r2.chain_hash());
    }

    #[test]
    fn receipt_differs_for_different_prev_hash() {
        #[cfg(feature = "signed")]
        let _guard = test_signing_env();
        let meta = fixed_meta(1);
        let r1 = admitted(serde_json::json!({"a": 1}))
            .receipt(&[1u8; 32], meta.clone())
            .expect("receipt should succeed");
        let r2 = admitted(serde_json::json!({"a": 1}))
            .receipt(&[2u8; 32], meta)
            .expect("receipt should succeed");
        assert_ne!(r1.chain_hash(), r2.chain_hash());
    }

    #[test]
    fn receipt_differs_for_different_instruction_id() {
        #[cfg(feature = "signed")]
        let _guard = test_signing_env();
        let prev = [7u8; 32];
        let r1 = admitted(serde_json::json!({"a": 1}))
            .receipt(&prev, fixed_meta(1))
            .expect("receipt should succeed");
        let r2 = admitted(serde_json::json!({"a": 1}))
            .receipt(&prev, fixed_meta(2))
            .expect("receipt should succeed");
        assert_ne!(r1.chain_hash(), r2.chain_hash());
    }

    /// When the `signed` feature is enabled, `receipt()` must populate
    /// `signature` with something that verifies against the chain hash it
    /// just computed (fail-open would be silently useless; this confirms
    /// the wiring actually happened rather than leaving `signature: None`).
    #[cfg(feature = "signed")]
    #[test]
    fn receipt_sets_verifiable_signature_when_signed() {
        let _guard = test_signing_env();
        let meta = fixed_meta(1);
        let prev = [7u8; 32];
        let r = admitted(serde_json::json!({"a": 1}))
            .receipt(&prev, meta)
            .expect("receipt should succeed");
        let sig = r
            .signature
            .as_ref()
            .expect("signature must be set when `signed` is enabled");
        let hash = r
            .chain_hash()
            .expect("chain hash must be set on a receipted object");
        assert!(crate::signing::verify_chain_hash(hash, sig).is_ok());
    }

    /// Fail-closed: if `receipt()` is asked to sign but no key is available,
    /// the whole receipt operation must fail rather than silently emitting
    /// an unsigned receipt.
    #[cfg(feature = "signed")]
    #[test]
    fn receipt_fails_closed_when_signing_key_missing() {
        let guard = crate::signing::test_support::env_lock();
        let had_key = std::env::var("PRAXIS_SIGNING_KEY").ok();
        let had_file = std::env::var("PRAXIS_SIGNING_KEY_FILE").ok();
        std::env::remove_var("PRAXIS_SIGNING_KEY");
        std::env::remove_var("PRAXIS_SIGNING_KEY_FILE");

        let result = admitted(serde_json::json!({"a": 1})).receipt(&[7u8; 32], fixed_meta(1));

        if let Some(v) = had_key {
            std::env::set_var("PRAXIS_SIGNING_KEY", v);
        }
        if let Some(v) = had_file {
            std::env::set_var("PRAXIS_SIGNING_KEY_FILE", v);
        }
        drop(guard);

        assert!(matches!(
            result,
            Err(crate::error::CoreError::SigningFailed(_))
        ));
    }

    /// `receipt()` must honor `meta.denial`: a non-`ADMITTED` denial must
    /// produce a non-zero `fired_mask`/`denial` on the frame, not the
    /// previously-hardcoded `ADMITTED`. Determinism (same inputs -> same
    /// chain hash) is unaffected by which denial is set.
    #[test]
    fn receipt_uses_meta_denial() {
        #[cfg(feature = "signed")]
        let _guard = test_signing_env();
        use bcinr_powl_receipt::denial::DenialPolarity;

        let mut meta = fixed_meta(1);
        meta.denial = DenialPolarity::PRECONDITION_FAILED;
        let prev = [7u8; 32];

        let admitted_denied = admitted(serde_json::json!({"a": 1}));
        let receipted = admitted_denied
            .receipt(&prev, meta)
            .expect("receipt should succeed even when denied");
        assert!(receipted.chain_hash().is_some());

        // Same inputs, ADMITTED default meta, must differ in chain hash from
        // the PRECONDITION_FAILED receipt above (the denial word is mixed
        // into the frame that feeds the chain hash).
        let admitted_clean = admitted(serde_json::json!({"a": 1}));
        let receipted_clean = admitted_clean
            .receipt(&prev, fixed_meta(1))
            .expect("receipt should succeed with default (ADMITTED) meta");
        assert_ne!(receipted.chain_hash(), receipted_clean.chain_hash());
    }
}
