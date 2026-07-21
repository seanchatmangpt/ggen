//! Boundary bridge to external process substrates (CE-BRIDGE lane).
//!
//! Bridges the chatman engine to three external tape representations:
//!
//! 1. **PDDL8** — `wasm4pm_compat::pddl::Pddl8Tape` (the canonical home is
//!    `wasm4pm_core::pddl`; `wasm4pm_compat` re-exports it verbatim via
//!    `pub use wasm4pm_core::pddl`, and `bcinr_pddl` re-exports the same
//!    types from `wasm4pm_compat`. praxis resolves the type through
//!    `wasm4pm_compat::pddl` because `wasm4pm-core` is not a direct
//!    dependency of this crate — see the duplicate-copy detector below,
//!    which proves at compile time that `bcinr_pddl::Pddl8Tape` and
//!    `wasm4pm_compat::pddl::Pddl8Tape` are one type, not two copies).
//! 2. **POWL v2** — `bcinr_powl::tape::v2::PowlTape` (64-byte cache-line
//!    ops, ≤ 64 slots).
//! 3. **Legacy POWL** — `bcinr_powl::tape::PowlTape`, projected from the
//!    same v2 source ops. `bcinr-powl` exposes no public v2→legacy
//!    conversion surface (verified against `bcinr-powl/src/tape.rs`: the
//!    legacy module only offers `new`/`alloc`; the v2 module offers
//!    `new`/`push`/`ready_mask`), so the projection here is built op-by-op
//!    from the v2 source via [`map_op_kind`], never by reinterpreting an
//!    unrelated tape.
//!
//! Identity discipline: all hashing routes through `wasm4pm_compat::hash`
//! (`blake3_combined` / `blake3_hex` / `canonical_json`). No wall clock
//! anywhere in this module; replay frames carry caller-supplied ordinals.

use serde::{Deserialize, Serialize};
use wasm4pm_compat::hash::{blake3_combined, blake3_hex, blake3_string, canonical_json};

use super::abi::Refusal;

use bcinr_powl::tape as legacy_tape;
use bcinr_powl::tape::v2;
use bcinr_powl_receipt::replay::PowlReplayFrame;
use wasm4pm_compat::pddl::Pddl8Tape;

// ─── Duplicate-copy detector ─────────────────────────────────────────────────

/// Compile-time proof that `bcinr_pddl::Pddl8Tape` and
/// `wasm4pm_compat::pddl::Pddl8Tape` are the *same* type (the identity
/// coercion below only type-checks if the two paths name one struct).
///
/// If a second structural copy of the canonical tape type ever appears in
/// the dependency graph, this constant stops compiling; the runtime analogue
/// is [`Refusal::DuplicateCanonicalTapeType`], raised by lanes that discover
/// a duplicate at data boundaries rather than at type boundaries.
///
/// Path note: the constitutional home of the type is `wasm4pm_core::pddl`;
/// `wasm4pm-core` is not a direct dependency of praxis-graphlaw, so the
/// canonical side is named through its verbatim re-export
/// `wasm4pm_compat::pddl` (`wasm4pm-compat/src/lib.rs`:
/// `pub use wasm4pm_core::pddl;`).
const _: fn(bcinr_pddl::Pddl8Tape) -> wasm4pm_compat::pddl::Pddl8Tape = |t| t;

// ─── Law witnesses ───────────────────────────────────────────────────────────

/// Witness for the *hot-conditions* law: at most 8 primary condition bits.
///
/// The constitutional owner of this law is
/// `wasm4pm_compat::law::ConditionCell<8>` — attempting `ConditionCell<9>`
/// is a compile error upstream (`Require<{BITS <= 8}>: IsTrue`). That bound
/// requires `generic_const_exprs`, which praxis-graphlaw does not enable
/// (confirmed E0277 by the admission8 lane; see the identical `Law8` witness
/// in `super::admission8`), so this zero-sized witness stands in and the
/// 8-bit bound is enforced at runtime via [`Refusal::WarmPathRequired`] by
/// the admission layer.
#[derive(Debug, Clone, Copy, Default, PartialEq, Eq)]
pub struct HotConditions;

/// Typed causal-chain shape for hot-path bridges: exactly 64 causal links,
/// matching the 64-slot POWL tape bound. Unlike `ConditionCell`,
/// `wasm4pm_compat::causality::CausalChain` carries no
/// `generic_const_exprs` bound upstream, so the alias imports cleanly.
pub type HotCausalChain = wasm4pm_compat::causality::CausalChain<64>;

// ─── Orchestrated plan ───────────────────────────────────────────────────────

/// One mapped step of an [`OrchestratedPlan`].
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct PlanStep {
    /// Slot index on the tape (0..64).
    pub index: u8,
    /// Legacy op-kind name this v2 op mapped to
    /// (`"Atom" | "Silent" | "XorDispatch" | "Join" | "LoopRedo"`).
    pub kind: String,
    /// Bitmask of slots that must complete before this step.
    pub pred_mask: u64,
    /// Bitmask of slots activated when this step completes.
    pub succ_mask: u64,
}

/// A workflow plan derived from the bridged tapes: the lawful projection of
/// the POWL v2 ops into legacy op kinds, sealed with the bridge tape hash.
#[derive(Debug, Clone, PartialEq, Eq, Serialize, Deserialize)]
pub struct OrchestratedPlan {
    /// Mapped steps in tape slot order (already canonical: slot index order).
    pub steps: Vec<PlanStep>,
    /// [`TapeBridge::tape_hash`] of the source tapes (hex BLAKE3).
    pub tape_hash: String,
}

// ─── Op-kind mapping ─────────────────────────────────────────────────────────

/// Map a POWL v2 op kind onto the legacy tape vocabulary.
///
/// Lawful mappings (semantics preserved):
/// - `Activity` → `Atom`, `Silent` → `Silent`, `XorChoice` → `XorDispatch`,
///   `Parallel` → `Join` (legacy `Join` carries the all-predecessors
///   semantics), `Loop` → `LoopRedo`.
///
/// `StrictPartial`, `ChoiceGraph`, and `Concur` have no legacy counterpart;
/// mapping them silently would change the process geometry, so they refuse
/// with [`Refusal::TraceUnlawful`] naming the slot.
///
/// # Complexity
/// O(1) — a single match on a fieldless discriminant.
fn map_op_kind(kind: v2::OpKind, slot: u8) -> Result<legacy_tape::OpKind, Refusal> {
    match kind {
        v2::OpKind::Activity => Ok(legacy_tape::OpKind::Atom),
        v2::OpKind::Silent => Ok(legacy_tape::OpKind::Silent),
        v2::OpKind::XorChoice => Ok(legacy_tape::OpKind::XorDispatch),
        v2::OpKind::Parallel => Ok(legacy_tape::OpKind::Join),
        v2::OpKind::Loop => Ok(legacy_tape::OpKind::LoopRedo),
        v2::OpKind::StrictPartial | v2::OpKind::ChoiceGraph | v2::OpKind::Concur => {
            Err(Refusal::TraceUnlawful(format!(
                "POWL v2 op kind {kind:?} at slot {slot} has no legacy tape counterpart; \
                 bridging it would alter the process geometry"
            )))
        }
    }
}

/// Legacy op-kind display name (closed vocabulary, matches `PlanStep::kind`).
///
/// # Complexity
/// O(1).
fn legacy_kind_name(kind: legacy_tape::OpKind) -> &'static str {
    match kind {
        legacy_tape::OpKind::Atom => "Atom",
        legacy_tape::OpKind::Silent => "Silent",
        legacy_tape::OpKind::XorDispatch => "XorDispatch",
        legacy_tape::OpKind::Join => "Join",
        legacy_tape::OpKind::LoopRedo => "LoopRedo",
    }
}

// ─── Canonical bytes ─────────────────────────────────────────────────────────

/// Domain-separation tag for the POWL v2 canonical byte encoding.
const POWL_V2_CANON_TAG: &[u8] = b"praxis-bridge-powl-v2-canon-v1";

/// Canonical, platform-independent byte encoding of a POWL v2 tape.
///
/// Layout (all integers little-endian): tag ‖ len ‖ entry_op ‖ exit_op ‖
/// per-op `(pred_mask, succ_mask, ctrl, op_kind, choice_group, depth,
/// fan_out)` for slots `0..len` in slot order ‖ slab_len ‖ slab bytes.
/// Slot order *is* the canonical order (the tape is a positional
/// structure), so no sort is required; the encoding is injective over the
/// encoded fields because every field is fixed-width and the slab is
/// length-prefixed.
///
/// # Complexity
/// O(len) ops + O(slab_len) bytes; len ≤ 64, slab ≤ 1024.
fn powl_v2_canonical_bytes(t: &v2::PowlTape) -> Vec<u8> {
    let mut bytes = Vec::with_capacity(POWL_V2_CANON_TAG.len() + 5 + t.len as usize * 27 + 1024);
    bytes.extend_from_slice(POWL_V2_CANON_TAG);
    bytes.push(t.len);
    bytes.push(t.entry_op);
    bytes.push(t.exit_op);
    // O(len): fixed-width record per valid slot, in slot (canonical) order.
    for op in t.ops.iter().take(t.len as usize) {
        bytes.extend_from_slice(&op.pred_mask.to_le_bytes());
        bytes.extend_from_slice(&op.succ_mask.to_le_bytes());
        bytes.extend_from_slice(&op.ctrl.to_le_bytes());
        bytes.push(op.op_kind as u8);
        bytes.push(op.choice_group);
        bytes.push(op.depth);
        bytes.push(op.fan_out);
    }
    bytes.extend_from_slice(&t.label_slab.len.to_le_bytes());
    bytes.extend_from_slice(&t.label_slab.data[..t.label_slab.len as usize]);
    bytes
}

// ─── TapeBridge ──────────────────────────────────────────────────────────────

/// Bridge over one candidate plan's three tape views plus its replay frames.
///
/// `legacy` is a *projection* of `powl_v2` built op-by-op in the
/// constructor (see module docs: `bcinr-powl` exposes no public v2→legacy
/// conversion), so the two POWL views are guaranteed to describe the same
/// source ops or the constructor refuses.
// No derived Debug: `v2::PowlTape` (via `LabelSlab`) implements neither
// Debug nor Clone upstream, so Debug is written by hand below and Clone is
// intentionally absent (rebuild via [`TapeBridge::new`] instead).
pub struct TapeBridge<'a> {
    /// PDDL8 geometry (canonical type; see the duplicate-copy detector).
    pub pddl: &'a Pddl8Tape,
    /// POWL v2 op tape (cache-line layout).
    pub powl_v2: &'a v2::PowlTape,
    /// Legacy POWL projection of `powl_v2` (same source ops).
    pub legacy: legacy_tape::PowlTape,
    /// High-level replay frames for this plan (ordinal timestamps only —
    /// callers must not feed wall-clock values into receipt paths).
    pub frames: Vec<PowlReplayFrame>,
}

impl core::fmt::Debug for TapeBridge<'_> {
    /// Summarized Debug (upstream `v2::PowlTape` has no Debug impl):
    /// tape lengths and frame count only.
    ///
    /// # Complexity
    /// O(1) — fixed number of scalar fields.
    fn fmt(&self, f: &mut core::fmt::Formatter<'_>) -> core::fmt::Result {
        f.debug_struct("TapeBridge")
            .field("pddl_ops", &self.pddl.ops.len())
            .field("powl_v2_len", &self.powl_v2.len)
            .field("legacy_len", &self.legacy.len)
            .field("frames", &self.frames.len())
            .finish()
    }
}

impl<'a> TapeBridge<'a> {
    /// Build a bridge, projecting `powl_v2` into a legacy tape.
    ///
    /// Refuses with [`Refusal::TraceUnlawful`] when any v2 op has no lawful
    /// legacy counterpart (see [`map_op_kind`]) or when the projection
    /// overflows the 64-slot legacy tape (unreachable from a well-formed v2
    /// tape, whose `len ≤ 64`, but refused rather than assumed).
    ///
    /// # Complexity
    /// O(len) over the v2 ops; len ≤ 64.
    pub fn new(
        pddl: &'a Pddl8Tape, powl_v2: &'a v2::PowlTape, frames: Vec<PowlReplayFrame>,
    ) -> Result<Self, Refusal> {
        let mut legacy = legacy_tape::PowlTape::new();
        // O(len): one lawful kind-mapping and field copy per source op.
        for (i, op) in powl_v2.ops.iter().take(powl_v2.len as usize).enumerate() {
            let slot = i as u8;
            let kind = map_op_kind(op.op_kind, slot)?;
            let idx = match legacy.alloc(kind) {
                Some(idx) => idx,
                None => {
                    return Err(Refusal::TraceUnlawful(format!(
                        "legacy POWL tape overflow while projecting v2 slot {slot} \
                         (legacy capacity is 64 ops)"
                    )));
                }
            };
            let dst = &mut legacy.ops[idx as usize];
            dst.pred_mask = op.pred_mask;
            dst.succ_mask = op.succ_mask;
            // Legacy branch semantics exist only on XorDispatch; a v2
            // XorChoice's successors are exactly its branch entries.
            if kind == legacy_tape::OpKind::XorDispatch {
                dst.branch_mask = op.succ_mask;
                dst.branch_count = op.fan_out;
            }
        }
        // Entry mask: the v2 tape names a single entry op; the masked shift
        // is branchless and cannot overflow (entry_op < 64 for a valid tape).
        legacy.entry_mask = if powl_v2.len == 0 {
            0
        } else {
            1u64 << (powl_v2.entry_op & 63)
        };
        Ok(Self {
            pddl,
            powl_v2,
            legacy,
            frames,
        })
    }

    /// Combined identity hash over both source tapes, fixed order
    /// `[pddl, powl_v2]`, via `wasm4pm_compat::hash::blake3_combined`
    /// (length-prefixed, injective over the pair).
    ///
    /// The PDDL side hashes the sorted-key canonical JSON of the tape; the
    /// POWL side hashes [`powl_v2_canonical_bytes`]. Both encodings are
    /// deterministic and platform-independent; no wall clock enters.
    ///
    /// Refuses with [`Refusal::ValidationFailed`] if the PDDL tape cannot
    /// be canonically serialized (non-finite numeric payload upstream).
    ///
    /// # Complexity
    /// O(|pddl| + len + slab_len) serialization plus BLAKE3 over the bytes.
    pub fn tape_hash(&self) -> Result<String, Refusal> {
        let pddl_json = canonical_json(self.pddl).map_err(|e| {
            Refusal::ValidationFailed(format!(
                "PDDL8 tape refused canonical JSON serialization: {e}"
            ))
        })?;
        let pddl_hash = blake3_string(&pddl_json);
        let powl_hash = blake3_hex(&powl_v2_canonical_bytes(self.powl_v2));
        Ok(blake3_combined(&[&pddl_hash, &powl_hash]))
    }

    /// Map the bridged tapes to an [`OrchestratedPlan`].
    ///
    /// Every v2 op must map to a legacy kind; an unmappable step refuses
    /// with [`Refusal::TraceUnlawful`] (already guaranteed by construction
    /// via [`Self::new`], and re-checked here so the plan mapping stands on
    /// its own law rather than on constructor history).
    ///
    /// # Complexity
    /// O(len) over the v2 ops; len ≤ 64.
    pub fn map_to_workflow(&self) -> Result<OrchestratedPlan, Refusal> {
        let tape_hash = self.tape_hash()?;
        let mut steps = Vec::with_capacity(self.powl_v2.len as usize);
        // O(len): one kind mapping per op, emitted in canonical slot order.
        for (i, op) in self
            .powl_v2
            .ops
            .iter()
            .take(self.powl_v2.len as usize)
            .enumerate()
        {
            let slot = i as u8;
            let kind = map_op_kind(op.op_kind, slot)?;
            steps.push(PlanStep {
                index: slot,
                kind: legacy_kind_name(kind).to_owned(),
                pred_mask: op.pred_mask,
                succ_mask: op.succ_mask,
            });
        }
        Ok(OrchestratedPlan { steps, tape_hash })
    }
}

// ─── CausalFrameEntry adapter (test-support) ─────────────────────────────────

/// Test-support adapter implementing chicago-tdd-tools'
/// `Blake3ReceiptEntry` over the 128-byte
/// `bcinr_powl_receipt::causal_receipt::OcelCausalFrame`.
///
/// Location note: this lives in `bridge.rs` but is `#[cfg(test)]`-scoped
/// because `chicago-tdd-tools` is a dev-dependency of praxis-graphlaw; a
/// non-test impl would require promoting it to a regular dependency. Other
/// lanes needing the adapter in their tests can lift this module verbatim
/// into shared test support.
///
/// Chain rule (validator side): `stored = BLAKE3(prev ‖ content)` where
/// `prev` is the frame's `prior_hash` (the parent hash) and `content` is
/// the 67-byte little-endian frame payload — the same field layout as the
/// frame's private `to_hash_bytes` *minus* its trailing `prior_hash`
/// (`OcelCausalFrame::to_hash_bytes` appends `prior_hash` last, i.e.
/// `content ‖ prev`; `Blake3ChainValidator` prepends it, i.e.
/// `prev ‖ content` — the adapter adopts the validator's order, and the
/// tests compute stored hashes under that same rule).
#[cfg(test)]
pub mod causal_adapter {
    use bcinr_powl_receipt::causal_receipt::OcelCausalFrame;
    use chicago_tdd_tools::observability::receipt::Blake3ReceiptEntry;

    /// One chained causal frame: the raw 128-byte frame plus its stored
    /// chain hash (the frame itself stores only its *parent's* hash).
    #[derive(Clone)]
    pub struct CausalFrameEntry {
        /// The 128-byte causal frame (parent hash lives in `prior_hash`).
        pub frame: OcelCausalFrame,
        /// Stored chain hash for this entry: `BLAKE3(prior_hash ‖ payload)`.
        pub chain_hash: [u8; 32],
    }

    /// 67-byte little-endian frame payload (frame bytes without the parent
    /// hash): `instruction_id ‖ fired_mask ‖ denial ‖ obj_refs(8×u32) ‖
    /// ts_ns ‖ activity_idx ‖ node_kind`.
    ///
    /// # Complexity
    /// O(1) — fixed 67-byte encoding.
    pub fn frame_payload_bytes(frame: &OcelCausalFrame) -> Vec<u8> {
        let mut v = Vec::with_capacity(67);
        v.extend_from_slice(&frame.instruction_id.to_le_bytes());
        v.extend_from_slice(&frame.fired_mask.to_le_bytes());
        v.extend_from_slice(&frame.denial.0.to_le_bytes());
        // O(1): exactly 8 packed object refs per frame.
        for r in &frame.obj_refs {
            v.extend_from_slice(&r.0.to_le_bytes());
        }
        v.extend_from_slice(&frame.ts_ns.to_le_bytes());
        v.extend_from_slice(&frame.activity_idx.to_le_bytes());
        v.push(frame.node_kind);
        v
    }

    impl Blake3ReceiptEntry for CausalFrameEntry {
        /// Parent hash: the frame's own `prior_hash` field.
        fn prev_hash(&self) -> [u8; 32] {
            self.frame.prior_hash
        }

        /// Payload: the 67-byte frame bytes (parent hash excluded — the
        /// validator prepends `prev_hash` itself).
        fn content_bytes(&self) -> Vec<u8> {
            frame_payload_bytes(&self.frame)
        }

        /// The stored chain hash carried alongside the frame.
        fn stored_hash(&self) -> [u8; 32] {
            self.chain_hash
        }
    }
}

// ─── Tests ───────────────────────────────────────────────────────────────────

#[cfg(test)]
#[path = "bridge_test.rs"]
mod tests;
