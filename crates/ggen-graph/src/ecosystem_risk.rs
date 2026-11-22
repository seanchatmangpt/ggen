//! ECOSYSTEM-SPEC-27.1.1 §11 — R-01…R-07 resolution contract.
//!
//! Risk *bodies* are not mounted in this checkout (D7 / §11). This module
//! encodes only the **resolution contract**:
//!
//! - R-01…R-07 are hard GA gates.
//! - Closure requires a typed receipt **or** typed refusal before GA.
//! - Do not invent risk bodies; until an admitted risk register supplies
//!   prose, treat “closed” claims as [`ResolutionRefusal::Ungrounded`]
//!   (`REFUSED_UNGROUNDED`).
//!
//! Per-ID risk-body closure remains out-of-repo / `BLOCKED` — see
//! `docs/rfc/v27.1.1/R01-R07-STANDING.md`.

use thiserror::Error;

/// Closed GA risk identifiers from ECOSYSTEM-SPEC-27.1.1 §11.
pub const RISK_IDS: [&str; 7] = ["R-01", "R-02", "R-03", "R-04", "R-05", "R-06", "R-07"];

/// Next verifier for every R-* body until the risk register is admitted.
pub const NEXT_VERIFIER_RISK_REGISTER: &str = concat!(
    "Admit risk-register bodies for R-01…R-07 from the sovereign risk register ",
    "(do not invent prose), then per-ID typed receipt or typed REFUSED_* before GA 2027-01-01"
);

/// Closure claim under the §11 resolution contract.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ClosureClaim {
    /// Still open — no closure asserted.
    Open,
    /// Asserts closed with an evidence coordinate (requires admitted body).
    Closed { evidence: String },
}

/// Typed refusal of an illegal resolution transition.
#[derive(Debug, Clone, PartialEq, Eq, Error)]
pub enum ResolutionRefusal {
    /// Identifier is outside the closed R-01…R-07 set.
    #[error("REFUSED_UNKNOWN_RISK_ID: {0}")]
    UnknownId(String),
    /// “Closed” asserted without an admitted risk-register body.
    #[error(
        "REFUSED_UNGROUNDED: closed claim for {risk_id} without admitted risk-register body"
    )]
    Ungrounded { risk_id: String },
}

/// Standing for a risk *body* (not the resolution-contract fence).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RiskBodyStanding {
    /// Body not mounted — cannot close from this repo yet.
    Blocked,
}

/// True when `id` is one of R-01…R-07.
pub fn is_known_risk_id(id: &str) -> bool {
    RISK_IDS.iter().any(|known| *known == id)
}

/// Resolve a closure claim under ECOSYSTEM-SPEC §11.
///
/// `risk_body_admitted` is true only when the admitted risk register has
/// mounted prose for this ID. Until then, [`ClosureClaim::Closed`] yields
/// [`ResolutionRefusal::Ungrounded`].
pub fn resolve_closure(
    risk_id: &str,
    claim: ClosureClaim,
    risk_body_admitted: bool,
) -> Result<(), ResolutionRefusal> {
    if !is_known_risk_id(risk_id) {
        return Err(ResolutionRefusal::UnknownId(risk_id.to_string()));
    }
    match claim {
        ClosureClaim::Open => Ok(()),
        ClosureClaim::Closed { .. } if !risk_body_admitted => Err(ResolutionRefusal::Ungrounded {
            risk_id: risk_id.to_string(),
        }),
        ClosureClaim::Closed { .. } => {
            // Body admitted: evidence path is open. Actual receipt/refusal
            // courts land with the register — this fence only clears the
            // ungrounded gate.
            Ok(())
        }
    }
}

/// Current body standing for a known R-* ID (always `Blocked` until register).
pub fn current_body_standing(risk_id: &str) -> Result<RiskBodyStanding, ResolutionRefusal> {
    if !is_known_risk_id(risk_id) {
        return Err(ResolutionRefusal::UnknownId(risk_id.to_string()));
    }
    Ok(RiskBodyStanding::Blocked)
}
