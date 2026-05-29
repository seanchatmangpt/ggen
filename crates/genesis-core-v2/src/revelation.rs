//! # Revelation Doctrine
//!
//! Revelation translated into code means: **unveiling.**
//!
//! The hidden thing becomes visible.
//! The false authority is exposed.
//! The unreceipted system collapses.
//! Only what can pass judgment remains.
//!
//! This module implements the **audit apocalypse of the codebase**:
//!
//! - Seven Churches → the seven project families, each told: "I know your works."
//! - Sealed Scroll → the closed architecture, opened only by evidence.
//! - Lamb-authority → receipted consequence, not narrative.
//! - Plagues → refusal artifacts that expose what the system was hiding.
//! - Babylon → the downstream illusion; projections pretending to be sources.
//! - New Jerusalem → the finished, ordered, receipted system.
//!
//! Nothing here is commentary. Everything here is law.

use crate::primitives::{Pair2, Receipt, Refusal, RefusalReason};

// ─── THE SEVEN CHURCHES ──────────────────────────────────────────────────────
//
// Each project family receives the same message: "I know your works."
// The verdict is determined by compile evidence, test evidence, and receipt evidence.
// Not by claims. Not by documentation. Not by the appearance of completion.

/// The seven project families subjected to the audit judgment.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Church {
    /// 1. Genesis — the pure consequence kernel (A = μ(O))
    Genesis = 1,
    /// 2. ggen — foundry and membrane (contact, not consequence)
    Ggen = 2,
    /// 3. AtomVM/WASM — custody and portability bodies
    AtomVmWasm = 3,
    /// 4. Truex — lifecycle, promotion, accounting ledger
    Truex = 4,
    /// 5. wasm4pm/pictl — process evidence validators
    Wasm4pm = 5,
    /// 6. open-ontologies / public vocabulary — survivability checkpoint
    OpenOntologies = 6,
    /// 7. BCINR/unibit/Field8/INSA — physical bounded-motion substrate
    BcinrSubstrate = 7,
}

/// The verdict delivered to each church: praise (works confirmed) or rebuke (works absent).
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum ChurchVerdict {
    /// Works confirmed: compiles, tests pass, receipts exist, boundaries hold.
    Overcomes = 1,
    /// Works present but boundary-incomplete: partial implementation, missing tests.
    Partial = 2,
    /// Doc-only: documented but not implemented. Lampstand at risk.
    DocOnly = 3,
    /// False completion: claims made without evidence. Lampstand removed.
    LampstandRemoved = 4,
}

/// The evidence record for a single church judgment.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct ChurchJudgment {
    pub church: Church,
    pub verdict: ChurchVerdict,
    /// The epoch at which this judgment was issued.
    pub epoch: u64,
    /// A BLAKE3 receipt over the evidence that produced this judgment.
    /// All-zero means no evidence was examined — this itself is a plague.
    pub evidence_receipt: [u8; 32],
}

impl ChurchJudgment {
    pub const fn new(
        church: Church, verdict: ChurchVerdict, epoch: u64, evidence_receipt: [u8; 32],
    ) -> Self {
        Self {
            church,
            verdict,
            epoch,
            evidence_receipt,
        }
    }

    /// A judgment with a zero receipt is itself a plague: no evidence was examined.
    pub fn is_evidenced(&self) -> bool {
        self.evidence_receipt != [0u8; 32]
    }
}

// ─── THE SEALED SCROLL ───────────────────────────────────────────────────────
//
// The scroll is sealed when agents cannot understand the architecture.
// The seal opens only when the agent can prove:
//   what exists / what is missing / what is boundary-safe /
//   what compiles / what tests / what replays / what refuses.

/// The seven seals that guard the closed architecture.
/// Each seal must be broken by real evidence, not narrative.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Seal {
    /// 1. Existence: the code exists on disk and compiles.
    Existence = 1,
    /// 2. Boundary: the boundary law holds (IO-free Genesis, strict membrane).
    Boundary = 2,
    /// 3. Receipt: every transition emits a BLAKE3 receipt.
    Receipt = 3,
    /// 4. Replay: given initial state + receipts, outcome is deterministic.
    Replay = 4,
    /// 5. Refusal: illegal states produce Refusal, not panic or silence.
    Refusal = 5,
    /// 6. Observation: every boundary crossing produces externalizable evidence.
    Observation = 6,
    /// 7. Causality: claim X caused Y is proven by ≥3 corroborating surfaces.
    Causality = 7,
}

/// A seal is broken when real evidence is presented.
/// An unbroken seal at seal 7 means the architecture remains closed.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct SealState {
    pub seal: Seal,
    pub broken: bool,
    /// The receipt that breaks this seal. Zero = seal remains closed.
    pub breaking_receipt: [u8; 32],
}

impl SealState {
    pub const fn sealed(seal: Seal) -> Self {
        Self {
            seal,
            broken: false,
            breaking_receipt: [0u8; 32],
        }
    }

    pub const fn broken_by(seal: Seal, receipt: [u8; 32]) -> Self {
        Self {
            seal,
            broken: true,
            breaking_receipt: receipt,
        }
    }
}

// ─── THE PLAGUES ─────────────────────────────────────────────────────────────
//
// A plague is a refusal artifact that exposes what the system was hiding.
// Plagues are not catastrophes. They are revelations.
// Need9 is a plague. Need257 is a plague. Replay mismatch is a plague.
// Missing source address is a plague. Boundary IO inside Genesis is a plague.
//
// Each plague category maps to a RefusalReason and an exposure surface.

/// The eight plague categories — each exposes a specific class of hidden defect.
#[derive(Debug, Clone, Copy, PartialEq, Eq, PartialOrd, Ord, Hash)]
#[repr(u8)]
pub enum Plague {
    /// 1. Source corrupted — source evidence invalid, cannot be traced.
    WaterCorrupted = 1,
    /// 2. Darkness — missing observability or replay surface.
    Darkness = 2,
    /// 3. Sores — boundary leakage (IO inside Genesis, JSON inside kernel).
    Sores = 3,
    /// 4. Fire — unsafe runtime side effect (non-determinism, filesystem metadata).
    Fire = 4,
    /// 5. Sea death — projection collapse (OCEL from non-existent trace).
    SeaDeath = 5,
    /// 6. False prophet — agent claim without file evidence.
    FalseProphet = 6,
    /// 7. Earthquake — architecture boundary break (Genesis/ggen conflation).
    Earthquake = 7,
    /// 8. Hail — hard failure from violated invariant (Need9, Need257, ReceiptMismatch).
    Hail = 8,
}

impl Plague {
    /// Classify a `RefusalReason` as the appropriate plague.
    pub fn from_refusal(reason: RefusalReason) -> Self {
        match reason {
            RefusalReason::PageFull => Plague::Hail,
            RefusalReason::ConstructFull => Plague::Hail,
            RefusalReason::OutofOrderEpoch => Plague::WaterCorrupted,
            RefusalReason::ReceiptMismatch => Plague::Earthquake,
            RefusalReason::ConstraintViolation => Plague::Hail,
            RefusalReason::DuplicateRelation => Plague::WaterCorrupted,
        }
    }

    /// Classify a missing observability surface as the Darkness plague.
    pub const fn darkness() -> Self {
        Plague::Darkness
    }

    /// Classify an IO boundary violation inside Genesis as the Sores plague.
    pub const fn boundary_io_in_genesis() -> Self {
        Plague::Sores
    }
}

/// A materialized plague record — the refusal artifact that exposes the defect.
///
/// A `PlagueRecord` is **not** an error. It is a receipt of what was hidden.
/// Systems that suppress plagues (catch-all error handlers, silent panics) carry
/// the mark of the beast: unreceipted completion.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct PlagueRecord {
    pub plague: Plague,
    pub epoch: u64,
    /// The Pair2 that triggered the plague, if applicable.
    pub trigger: Pair2,
    /// BLAKE3 receipt over the evidence that materialized this plague.
    pub exposure_receipt: [u8; 32],
}

impl PlagueRecord {
    pub const fn new(plague: Plague, epoch: u64, trigger: Pair2) -> Self {
        Self {
            plague,
            epoch,
            trigger,
            exposure_receipt: [0u8; 32],
        }
    }

    /// Produce a receipted plague from a real `Refusal`.
    pub fn from_refusal(refusal: &Refusal, previous_receipt: &[u8; 32]) -> Self {
        use crate::primitives::Construct8;

        let plague = Plague::from_refusal(refusal.reason);

        // Materialize a Construct8 act representing the refused construction
        // so we can generate a real BLAKE3 receipt over the exposure event.
        let mut act = Construct8::new(refusal.epoch, plague as u32);
        act.push(refusal.failed_pair).ok();

        let receipt = Receipt::generate(&act, previous_receipt);

        Self {
            plague,
            epoch: refusal.epoch,
            trigger: refusal.failed_pair,
            exposure_receipt: receipt.signature,
        }
    }

    /// A plague record is genuine only if its exposure receipt is non-zero.
    /// A zero-receipt plague record is itself a plague (FalseProphet).
    pub fn is_genuine(&self) -> bool {
        self.exposure_receipt != [0u8; 32]
    }
}

// ─── BABYLON ─────────────────────────────────────────────────────────────────
//
// Babylon is the downstream data economy pretending it is the source of truth.
// Lakehouses, dashboards, AI context windows, event logs, graph stores —
// all say "we are reality." Genesis says: "No. Show construction."
//
// A Babylon claim is an assertion of authority without upstream receipt evidence.

/// A `BabylonClaim` represents any artifact that asserts completion, truth,
/// or authority without upstream BLAKE3 receipt chain evidence.
///
/// The only thing that can dissolve a Babylon claim is a receipted refusal.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(u8)]
pub enum BabylonClaim {
    /// "I am done" — completion claimed without test evidence.
    UnreceiptedCompletion = 1,
    /// "This is real" — data asserted as truth without source addressing.
    UnsourcedData = 2,
    /// "The schema is correct" — structural claim without SHACL/constraint validation.
    UnvalidatedSchema = 3,
    /// "The trace proves it" — telemetry cited without replay evidence.
    UnreplayableTrace = 4,
    /// "The agent verified it" — narrative presented as evidence.
    NarrativeAsEvidence = 5,
}

// ─── THE LAMB AUTHORITY (Receipted Consequence) ──────────────────────────────
//
// Only receipted consequence can open the scroll.
// Not agent confidence. Not narrative. Not "I'm done."
// Not docs alone. Not clean-looking code.
// Only: compile, test, receipt, replay, refusal, observed-vs-planned matrix.

/// Verify that an artifact carries lawful construction — i.e., it is not Babylon.
///
/// Returns `Ok(())` if the receipt chain is non-zero and causally valid.
/// Returns `Err(PlagueRecord)` exposing the specific defect.
pub fn verify_lamb_authority(
    evidence_receipt: &[u8; 32], previous_receipt: &[u8; 32], epoch: u64,
) -> Result<(), PlagueRecord> {
    if evidence_receipt == &[0u8; 32] {
        // Zero receipt = no evidence was examined = FalseProphet plague
        return Err(PlagueRecord::new(
            Plague::FalseProphet,
            epoch,
            Pair2::new(0, 0),
        ));
    }

    // Verify the receipt is not simply a copy of the previous receipt
    // (which would indicate no new work was done)
    if evidence_receipt == previous_receipt {
        return Err(PlagueRecord::new(
            Plague::FalseProphet,
            epoch,
            Pair2::new(0, 0),
        ));
    }

    Ok(())
}

// ─── NEW JERUSALEM ───────────────────────────────────────────────────────────
//
// New Jerusalem is the ordered city after false systems fall.
// Its gates are the interfaces. Its foundations are the core laws.
// Its measurements are exact. Its light is evidence.
// Nothing unclean enters — refusal prevents invalid construction.

/// The seven foundations of New Jerusalem — the seven laws that must hold
/// for any project to enter the finished architecture.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
#[repr(C)]
pub struct JerusalemGate {
    /// Which of the seven foundations this gate enforces.
    pub foundation: Seal,
    /// Whether this gate is passable (foundation law holds).
    pub passable: bool,
    /// The receipt that confirms the gate is passable.
    pub gate_receipt: [u8; 32],
}

impl JerusalemGate {
    pub const fn closed(foundation: Seal) -> Self {
        Self {
            foundation,
            passable: false,
            gate_receipt: [0u8; 32],
        }
    }

    pub const fn open(foundation: Seal, receipt: [u8; 32]) -> Self {
        Self {
            foundation,
            passable: true,
            gate_receipt: receipt,
        }
    }

    /// Nothing unclean enters. A gate is clean only if it bears a non-zero receipt.
    pub fn nothing_unclean_enters(&self) -> bool {
        self.passable && self.gate_receipt != [0u8; 32]
    }
}

/// Determine if a project family has earned entry into New Jerusalem
/// by passing all seven gates.
pub fn passes_all_gates(gates: &[JerusalemGate; 7]) -> bool {
    gates.iter().all(|g| g.nothing_unclean_enters())
}

#[cfg(test)]
mod tests {
    use super::*;
    use crate::primitives::{Construct8, Pair2, Receipt, Refusal, RefusalReason};

    #[test]
    fn test_plague_from_receipt_mismatch_is_earthquake() {
        let refusal = Refusal::new(1, RefusalReason::ReceiptMismatch, Pair2::new(10, 20));
        let prev = [0u8; 32];
        let record = PlagueRecord::from_refusal(&refusal, &prev);

        assert_eq!(record.plague, Plague::Earthquake);
        assert_eq!(record.epoch, 1);
        assert_eq!(record.trigger, Pair2::new(10, 20));
        // The plague record itself must be receipted (genuine)
        assert!(
            record.is_genuine(),
            "PlagueRecord must carry a real BLAKE3 receipt"
        );
    }

    #[test]
    fn test_plague_from_page_full_is_hail() {
        let refusal = Refusal::new(5, RefusalReason::PageFull, Pair2::new(255, 255));
        let prev = [42u8; 32];
        let record = PlagueRecord::from_refusal(&refusal, &prev);

        assert_eq!(record.plague, Plague::Hail);
        assert!(record.is_genuine());
    }

    #[test]
    fn test_zero_evidence_receipt_fails_lamb_authority() {
        let zero = [0u8; 32];
        let prev = [1u8; 32];
        let result = verify_lamb_authority(&zero, &prev, 1);
        assert!(result.is_err());
        let plague = result.unwrap_err();
        assert_eq!(plague.plague, Plague::FalseProphet);
    }

    #[test]
    fn test_copy_receipt_fails_lamb_authority() {
        // Same receipt as previous = no new work
        let receipt = [99u8; 32];
        let result = verify_lamb_authority(&receipt, &receipt, 1);
        assert!(result.is_err());
        let plague = result.unwrap_err();
        assert_eq!(plague.plague, Plague::FalseProphet);
    }

    #[test]
    fn test_real_receipt_passes_lamb_authority() {
        let mut act = Construct8::new(1, 42);
        act.push(Pair2::new(10, 20)).unwrap();
        let prev = [0u8; 32];
        let receipt = Receipt::generate(&act, &prev);

        let result = verify_lamb_authority(&receipt.signature, &prev, 1);
        assert!(
            result.is_ok(),
            "Real BLAKE3 receipt must pass lamb authority"
        );
    }

    #[test]
    fn test_jerusalem_gate_nothing_unclean_enters() {
        let zero_gate = JerusalemGate::closed(Seal::Receipt);
        assert!(!zero_gate.nothing_unclean_enters());

        let real_receipt = [7u8; 32]; // non-zero stands for real receipt in this unit test
        let open_gate = JerusalemGate::open(Seal::Receipt, real_receipt);
        assert!(open_gate.nothing_unclean_enters());
    }

    #[test]
    fn test_passes_all_gates_requires_all_seven() {
        let real_receipt = [7u8; 32];
        let gates: [JerusalemGate; 7] = [
            JerusalemGate::open(Seal::Existence, real_receipt),
            JerusalemGate::open(Seal::Boundary, real_receipt),
            JerusalemGate::open(Seal::Receipt, real_receipt),
            JerusalemGate::open(Seal::Replay, real_receipt),
            JerusalemGate::open(Seal::Refusal, real_receipt),
            JerusalemGate::open(Seal::Observation, real_receipt),
            JerusalemGate::open(Seal::Causality, real_receipt),
        ];
        assert!(passes_all_gates(&gates));

        // Close one gate — must fail
        let mut broken_gates = gates;
        broken_gates[3] = JerusalemGate::closed(Seal::Replay);
        assert!(!passes_all_gates(&broken_gates));
    }

    #[test]
    fn test_church_judgment_with_zero_receipt_is_not_evidenced() {
        let judgment = ChurchJudgment::new(Church::Genesis, ChurchVerdict::Overcomes, 1, [0u8; 32]);
        assert!(
            !judgment.is_evidenced(),
            "Zero-receipt judgment is itself a plague"
        );
    }

    #[test]
    fn test_seven_churches_are_distinct() {
        // All seven church variants must be distinct enum values
        let churches = [
            Church::Genesis as u8,
            Church::Ggen as u8,
            Church::AtomVmWasm as u8,
            Church::Truex as u8,
            Church::Wasm4pm as u8,
            Church::OpenOntologies as u8,
            Church::BcinrSubstrate as u8,
        ];
        let mut sorted: Vec<u8> = churches.to_vec();
        sorted.sort();
        sorted.dedup();
        assert_eq!(
            sorted.len(),
            7,
            "All seven churches must have distinct byte values"
        );
    }
}
