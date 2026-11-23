//! ECOSYSTEM-SPEC-27.1.1 §11 — R-01…R-07 resolution contract.
//!
//! Risk *bodies* are admitted from the sovereign risk-register mount
//! (`docs/rfc/v27.1.1/fixtures/r01-r07-sovereign-risk-register.md`,
//! provenance: `zcode-cli` `docs/ecosystem-spec-27.1.1.md` §4). This module
//! encodes the **resolution contract**:
//!
//! - R-01…R-07 are hard GA gates.
//! - Closure requires a typed receipt **or** typed refusal before GA.
//! - Do not invent risk bodies; “closed” without an admitted body is
//!   [`ResolutionRefusal::Ungrounded`] (`REFUSED_UNGROUNDED`).
//! - Body *admission* ≠ body *closure*. R-06 may close with evidence;
//!   others remain open until their contracts exit **0**.
//!
//! See `docs/rfc/v27.1.1/R01-R07-STANDING.md`.

use thiserror::Error;

/// Closed GA risk identifiers from ECOSYSTEM-SPEC-27.1.1 §11.
pub const RISK_IDS: [&str; 7] = ["R-01", "R-02", "R-03", "R-04", "R-05", "R-06", "R-07"];

/// In-repo admitted mount for R-* body prose (not invented).
pub const ADMITTED_RISK_REGISTER_REL: &str =
    "docs/rfc/v27.1.1/fixtures/r01-r07-sovereign-risk-register.md";

/// Next verifier for open R-* bodies after register admit.
pub const NEXT_VERIFIER_RISK_REGISTER: &str = concat!(
    "Bodies admitted from fixtures/r01-r07-sovereign-risk-register.md ",
    "(zcode-cli docs/ecosystem-spec-27.1.1.md §4). Per open ID: execute that ",
    "row's Mandatory Resolution Contract → typed receipt or typed REFUSED_* ",
    "before GA 2027-01-01"
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
    #[error("REFUSED_UNGROUNDED: closed claim for {risk_id} without admitted risk-register body")]
    Ungrounded { risk_id: String },
}

/// Standing for a risk *body* (not the resolution-contract fence).
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum RiskBodyStanding {
    /// Body prose not mounted.
    Blocked,
    /// Body prose admitted; closure contract still open.
    AdmittedOpen,
    /// Body admitted and closure evidenced (typed receipt).
    Closed,
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
    risk_id: &str, claim: ClosureClaim, risk_body_admitted: bool,
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

/// Current body standing for a known R-* ID.
///
/// Bodies are admitted from [`ADMITTED_RISK_REGISTER_REL`]. R-06 is **Closed**
/// on the zcode-cli evidence pin (commits `0cff19a`/`7b2c181` + Chicago
/// `powl-ocel-chicago` EXIT 0). Other IDs remain [`RiskBodyStanding::AdmittedOpen`].
pub fn current_body_standing(risk_id: &str) -> Result<RiskBodyStanding, ResolutionRefusal> {
    if !is_known_risk_id(risk_id) {
        return Err(ResolutionRefusal::UnknownId(risk_id.to_string()));
    }
    Ok(match risk_id {
        "R-06" => RiskBodyStanding::Closed,
        _ => RiskBodyStanding::AdmittedOpen,
    })
}

/// Whether the in-repo admitted register mounts prose for `risk_id`.
pub fn risk_body_is_admitted(risk_id: &str) -> bool {
    is_known_risk_id(risk_id)
}
