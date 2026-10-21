//! Envelope + replay-court surface (v26.10.20 wave-2, lane I2; seam pinned RED
//! by `tests/envelope_courts.rs`).
//!
//! An [`Envelope`] binds the bounded-resource context of one manufacture: the
//! ontology hash, the graph digest, a [`CapTable`] of declared cap ceilings, and
//! the [`ManufactureReceipt`] the kernel produced. [`envelope_digest`] is the
//! canonical digest of the whole envelope — every field, including every cap
//! entry, moves it (same `canonical_json_bytes` discipline as
//! [`crate::canonical_digest`]).
//!
//! [`envelope_court`] replays the receipt against the declared caps and returns
//! a [`ReplayVerdict`]:
//!
//! * `Pass` — every cap the receipt consumed is declared and exactly binds the
//!   receipt's measured output count.
//! * `VacuousGate` — the receipt consumed a cap id absent from the cap table.
//!   An undeclared cap is vacuity, never a pass: exit code 1, typed, loud
//!   (RESOLUTIONS.md refusal registry `VACUOUS_COURT` family).
//! * `Refused` — a declared cap does not match the receipt-bound contract
//!   ceiling (e.g. an x10-raised cap): exit code 2.

use crate::{canonical_digest, CapTable, Envelope, ManufactureReceipt, ReplayVerdict};

/// Suffix of the cap id a receipt consumes: `<capability>.max_artifacts`.
const CAP_SUFFIX: &str = ".max_artifacts";

/// The cap id the receipt actually consumed.
fn consumed_cap_id(r: &ManufactureReceipt) -> String {
    format!("{}{CAP_SUFFIX}", r.capability)
}

/// Canonical, deterministic digest of an envelope.
///
/// Serializes the whole envelope (serde_json maps are key-sorted;
/// `canonical_json_bytes` re-sorts under `preserve_order`), so every field —
/// ontology hash, graph digest, each cap entry, each receipt field — moves the
/// digest.
pub fn envelope_digest(env: &Envelope) -> String {
    canonical_digest(env)
}

/// Replay the envelope's receipt against its declared cap table.
pub fn envelope_court(env: &Envelope) -> ReplayVerdict {
    let r = &env.receipt;
    let base_cap = r.capability.clone();
    let cap_id = consumed_cap_id(r);
    let measured = r.outputs.len() as u64;
    match env.cap_table.caps.get(&cap_id) {
        None => {
            let detail = format!(
                "receipt consumed cap {base_cap} (outputs={measured}) but the envelope declares no such cap"
            );
            ReplayVerdict::VacuousGate {
                undeclared: Some(base_cap),
                detail,
            }
        }
        Some(&declared) if declared == measured => ReplayVerdict::Pass,
        Some(&declared) => ReplayVerdict::Refused {
            code: "CAP_CEILING_MISMATCH".into(),
            detail: format!(
                "cap {cap_id} declares {declared}, receipt measured {measured} outputs"
            ),
        },
    }
}

impl Envelope {
    /// Read-only view of the declared cap table.
    pub fn cap_table(&self) -> &CapTable {
        &self.cap_table
    }
    /// Mutable view of the declared cap table (court mutants use this).
    pub fn cap_table_mut(&mut self) -> &mut CapTable {
        &mut self.cap_table
    }
}

impl ReplayVerdict {
    pub fn is_pass(&self) -> bool {
        matches!(self, ReplayVerdict::Pass)
    }
    /// Shell semantics: Pass=0, VacuousGate=1 (VACUOUS GATE, loud), Refused=2.
    pub fn exit_code(&self) -> i32 {
        match self {
            ReplayVerdict::Pass => 0,
            ReplayVerdict::VacuousGate { .. } => 1,
            ReplayVerdict::Refused { .. } => 2,
        }
    }
}
