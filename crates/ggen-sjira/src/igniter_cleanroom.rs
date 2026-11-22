//! ggen_igniter clean-room contract — IO-free typed interface.
//!
//! Crown claim (ECOSYSTEM-SPEC-27.1.1 sibling lane): the sibling repo
//! `seanchatmangpt/ggen_igniter` is clean-room iff the admitted verifier
//!
//! ```text
//! git diff --exit-code
//! ```
//!
//! exits **0** in that checkout. This module never shells out and never
//! mounts the sibling: callers supply a [`CleanRoomObservation`]. Missing
//! observation refuses ALIVE promotion (`CheckoutUnmounted`); nonzero exit
//! refuses as `DirtyTree`.
//!
//! Court: `cargo test -p ggen-sjira --test igniter_cleanroom_court`
//! Standing ledger: `docs/rfc/v27.1.1/IGNITER-CLEANROOM-STANDING.md`

use std::fmt;

/// Canonical subject for the clean-room crown claim.
pub const SUBJECT_REPO: &str = "seanchatmangpt/ggen_igniter";

/// Exact next verifier that can promote the crown claim to `ALIVE`.
pub const VERIFIER_COMMAND: &str = "git diff --exit-code";

/// Observation of a single clean-room probe. IO-free: the exit code is
/// supplied by an external actuator; this crate does not run git.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CleanRoomObservation {
    /// Must equal [`SUBJECT_REPO`] for the crown claim.
    pub subject: String,
    /// `None` means the sibling checkout was not mounted / not probed.
    /// `Some(code)` is the process exit status of [`VERIFIER_COMMAND`].
    pub git_diff_exit_code: Option<i32>,
}

/// Successful adjudication: clean-room standing may be recorded as `ALIVE`
/// for this observation only (not ambient authority for DO).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CleanRoomAlive {
    pub subject: String,
    pub verifier: &'static str,
}

/// Typed clean-room refusals. Never promote to `ALIVE` under these.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum CleanRoomRefusal {
    /// Wrong or empty subject — refuse rather than silently retarget.
    SubjectMismatch { observed: String },
    /// Sibling not mounted / probe not run — standing stays `UNKNOWN`/`BLOCKED`.
    CheckoutUnmounted,
    /// Verifier ran and reported a dirty tree (`exit_code != 0`).
    DirtyTree { exit_code: i32 },
}

impl fmt::Display for CleanRoomRefusal {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        match self {
            Self::SubjectMismatch { observed } => write!(
                f,
                "REFUSED_CLEANROOM_SUBJECT_MISMATCH: observed {observed:?}, \
                 required {SUBJECT_REPO:?} — remediation: probe only the admitted \
                 sibling subject"
            ),
            Self::CheckoutUnmounted => write!(
                f,
                "REFUSED_CLEANROOM_CHECKOUT_UNMOUNTED: {SUBJECT_REPO} is not \
                 mounted in this workspace — remediation: materialize the sibling \
                 checkout and run `{VERIFIER_COMMAND}` (exit 0 required for ALIVE)"
            ),
            Self::DirtyTree { exit_code } => write!(
                f,
                "REFUSED_CLEANROOM_DIRTY_TREE: `{VERIFIER_COMMAND}` exited \
                 {exit_code} in {SUBJECT_REPO} — remediation: finish or revert WIP \
                 until the verifier exits 0 (historical IG-1 note: ~98 dirty files)"
            ),
        }
    }
}

impl std::error::Error for CleanRoomRefusal {}

/// Standing label implied by an observation **before** ALIVE promotion.
/// Distinct from [`adjudicate`]: this never invents `ALIVE` without a
/// successful adjudicate path.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum CleanRoomStandingLabel {
    Unknown,
    Blocked,
    Alive,
    RefusedDirty,
}

/// Map an observation to a standing label without claiming manufacture.
pub fn standing_label(obs: &CleanRoomObservation) -> CleanRoomStandingLabel {
    if obs.subject != SUBJECT_REPO {
        return CleanRoomStandingLabel::Unknown;
    }
    match obs.git_diff_exit_code {
        None => CleanRoomStandingLabel::Blocked,
        Some(0) => CleanRoomStandingLabel::Alive,
        Some(_) => CleanRoomStandingLabel::RefusedDirty,
    }
}

/// Admit clean-room `ALIVE` only when the exact verifier exited 0 on the
/// admitted subject. Fail-closed otherwise.
pub fn adjudicate(obs: &CleanRoomObservation) -> Result<CleanRoomAlive, CleanRoomRefusal> {
    if obs.subject != SUBJECT_REPO {
        return Err(CleanRoomRefusal::SubjectMismatch {
            observed: obs.subject.clone(),
        });
    }
    match obs.git_diff_exit_code {
        None => Err(CleanRoomRefusal::CheckoutUnmounted),
        Some(0) => Ok(CleanRoomAlive {
            subject: SUBJECT_REPO.to_string(),
            verifier: VERIFIER_COMMAND,
        }),
        Some(code) => Err(CleanRoomRefusal::DirtyTree { exit_code: code }),
    }
}

#[cfg(test)]
mod unit {
    use super::*;

    #[test]
    fn exit_zero_is_alive() {
        let obs = CleanRoomObservation {
            subject: SUBJECT_REPO.into(),
            git_diff_exit_code: Some(0),
        };
        assert!(adjudicate(&obs).is_ok());
        assert_eq!(standing_label(&obs), CleanRoomStandingLabel::Alive);
    }
}
