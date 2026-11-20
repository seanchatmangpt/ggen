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
//! * `Pass` — every cap the receipt consumed is declared and the receipt's
//!   measured output count is within the declared ceiling (HIGH-1 fix,
//!   v26.11.3: a ceiling is a maximum, not an equality — the court previously
//!   inverted this and passed only `declared == measured`).
//! * `VacuousGate` — the receipt consumed a cap id absent from the cap table.
//!   An undeclared cap is vacuity, never a pass: exit code 1, typed, loud
//!   (RESOLUTIONS.md refusal registry `VACUOUS_COURT` family).
//! * `Refused` — a declared cap sits below the receipt's measured output count
//!   (e.g. a ceiling clamped under what was manufactured), or the envelope's
//!   graph digest does not bind the receipt's graph digest (HIGH-2,
//!   `ENVELOPE_GRAPH_MISMATCH`): exit code 2.
//!
//! Freshness (WO-06 R8, registered): ontology-hash / timestamp / generation
//! freshness semantics are DELEGATED to the kernel's
//! [`crate::verify_receipt`] (`src/lib.rs:1056`), which owns them. The court
//! does not duplicate freshness checks — one calculus, one implementation
//! (the N-implementations-of-one-calculus drift class).

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

/// Replay the envelope's receipt against its declared cap table and graph
/// digest binding.
pub fn envelope_court(env: &Envelope) -> ReplayVerdict {
    let r = &env.receipt;
    // HIGH-2: the envelope and the receipt it embeds must bind the same graph
    // subject. An envelope assembled around a receipt from a different graph
    // is a smuggled subject, not a replay.
    if env.graph_digest != r.graph_digest {
        return ReplayVerdict::Refused {
            code: "ENVELOPE_GRAPH_MISMATCH".into(),
            detail: format!(
                "envelope binds graph {}, receipt was manufactured against graph {}",
                env.graph_digest, r.graph_digest
            ),
        };
    }
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
        // HIGH-1 fix (v26.11.3): CapTable is documented "cap id -> maximum
        // value", so the lawful relation is `measured <= declared`. The
        // previous `declared == measured` inverted ceiling semantics: an
        // envelope declaring headroom passed only when the receipt happened to
        // fill the ceiling exactly. Receipt claims below the ceiling pass; a
        // receipt measuring more than the ceiling refuses.
        Some(&declared) if measured <= declared => ReplayVerdict::Pass,
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
