//! zcode-cli POWL/OCEL autonomic remediation contract — IO-free typed interface.
//!
//! Crown claim (ECOSYSTEM-SPEC-27.1.1 photolithography hop 5): sibling
//! `seanchatmangpt/zcode-cli` drives bounded POWL→OCEL remediations iff the
//! admitted Chicago court verifier exits **0** in that checkout. This module
//! never shells out and never mounts the sibling: callers supply a
//! [`ZcodeRemediationObservation`]. Missing observation refuses ALIVE
//! promotion (`CheckoutUnmounted`); wrong subject refuses as
//! `SubjectMismatch`.
//!
//! Court: `cargo test -p ggen-sjira --test zcode_powl_ocel_court`
//! Standing ledger: `docs/rfc/v27.1.1/ZCODE-POWL-OCEL-STANDING.md`
//! Bound vocabulary (retry/write-scope caps): `ggen_graph::layout::remediation_bound`

use std::fmt;

/// Canonical subject for the hop-5 autonomic remediation crown claim.
pub const SUBJECT_REPO: &str = "seanchatmangpt/zcode-cli";

/// Exact next verifier that can promote the crown claim to `ALIVE`.
pub const VERIFIER_COMMAND: &str = "bun test test/powl-ocel-chicago.test.ts";

/// Layout-court companion verifier (consume ggen `AotSealReceipt`; fail closed).
pub const UI_LAYOUT_COURT_COMMAND: &str = "pnpm test -- ui-layout-court";

/// Predecessor hop-4 verifier (ggen_igniter clean-room; not this subject).
pub const PREDECESSOR_IGNITER_VERIFIER: &str = "git diff --exit-code";

/// Observation of a single zcode POWL/OCEL court probe. IO-free: the exit
/// code is supplied by an external actuator; this crate does not run bun/pnpm.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ZcodeRemediationObservation {
    /// Must equal [`SUBJECT_REPO`] for the crown claim.
    pub subject: String,
    /// `None` means the sibling checkout was not mounted / not probed.
    /// `Some(code)` is the process exit status of [`VERIFIER_COMMAND`].
    pub chicago_court_exit_code: Option<i32>,
}

/// Successful adjudication: remediation standing may be recorded as `ALIVE`
/// for this observation only (not ambient authority for DO).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ZcodeRemediationAlive {
    pub subject: String,
    pub verifier: &'static str,
}

/// Typed hop-5 refusals. Never promote to `ALIVE` under these.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ZcodeRemediationRefusal {
    /// Wrong or empty subject — refuse rather than silently retarget.
    SubjectMismatch { observed: String },
    /// Sibling not mounted / probe not run — standing stays `UNKNOWN`/`BLOCKED`.
    CheckoutUnmounted,
    /// Verifier ran and reported failure (`exit_code != 0`).
    CourtFailed { exit_code: i32 },
}

impl fmt::Display for ZcodeRemediationRefusal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SubjectMismatch { observed } => write!(
                f,
                "REFUSED_ZCODE_SUBJECT_MISMATCH: observed {observed:?}, \
                 required {SUBJECT_REPO:?} — remediation: probe only the admitted \
                 sibling subject; predecessor hop-4 uses `{PREDECESSOR_IGNITER_VERIFIER}` \
                 on seanchatmangpt/ggen_igniter, not this court"
            ),
            Self::CheckoutUnmounted => write!(
                f,
                "REFUSED_ZCODE_CHECKOUT_UNMOUNTED: {SUBJECT_REPO} is not \
                 mounted in this workspace — remediation: materialize the sibling \
                 checkout and run `{VERIFIER_COMMAND}` (exit 0 required for ALIVE); \
                 layout consume companion: `{UI_LAYOUT_COURT_COMMAND}`"
            ),
            Self::CourtFailed { exit_code } => write!(
                f,
                "REFUSED_ZCODE_COURT_FAILED: `{VERIFIER_COMMAND}` exited \
                 {exit_code} in {SUBJECT_REPO} — remediation: fix POWL/OCEL \
                 autonomic bounds (retry/cycle/write-scope) until the Chicago \
                 court exits 0; do not claim ALIVE from ggen vocab alone"
            ),
        }
    }
}

impl std::error::Error for ZcodeRemediationRefusal {}

/// Standing label implied by an observation **before** ALIVE promotion.
/// Distinct from [`adjudicate`]: this never invents `ALIVE` without a
/// successful adjudicate path.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum ZcodeRemediationStandingLabel {
    Unknown,
    Blocked,
    Alive,
    RefusedCourt,
}

/// Map an observation to a standing label without claiming manufacture.
pub fn standing_label(obs: &ZcodeRemediationObservation) -> ZcodeRemediationStandingLabel {
    if obs.subject != SUBJECT_REPO {
        return ZcodeRemediationStandingLabel::Unknown;
    }
    match obs.chicago_court_exit_code {
        None => ZcodeRemediationStandingLabel::Blocked,
        Some(0) => ZcodeRemediationStandingLabel::Alive,
        Some(_) => ZcodeRemediationStandingLabel::RefusedCourt,
    }
}

/// Admit hop-5 `ALIVE` only when the exact Chicago court exited 0 on the
/// admitted subject. Fail-closed otherwise.
pub fn adjudicate(
    obs: &ZcodeRemediationObservation,
) -> Result<ZcodeRemediationAlive, ZcodeRemediationRefusal> {
    if obs.subject != SUBJECT_REPO {
        return Err(ZcodeRemediationRefusal::SubjectMismatch {
            observed: obs.subject.clone(),
        });
    }
    match obs.chicago_court_exit_code {
        None => Err(ZcodeRemediationRefusal::CheckoutUnmounted),
        Some(0) => Ok(ZcodeRemediationAlive {
            subject: SUBJECT_REPO.to_string(),
            verifier: VERIFIER_COMMAND,
        }),
        Some(code) => Err(ZcodeRemediationRefusal::CourtFailed { exit_code: code }),
    }
}

#[cfg(test)]
mod unit {
    use super::*;

    #[test]
    fn exit_zero_is_alive() {
        let obs = ZcodeRemediationObservation {
            subject: SUBJECT_REPO.into(),
            chicago_court_exit_code: Some(0),
        };
        assert!(adjudicate(&obs).is_ok());
        assert_eq!(standing_label(&obs), ZcodeRemediationStandingLabel::Alive);
    }
}
