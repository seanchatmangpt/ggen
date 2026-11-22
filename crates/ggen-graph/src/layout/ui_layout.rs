//! Gate-1 ui-layout consume fence (ECOSYSTEM-SPEC-27.1.1 D5 in-repo slice).
//!
//! Consumes [`super::taffy_aot::AotSealReceipt`] digests without re-implementing
//! Taffy geometry law. Mirrors the fail-closed contract of sibling
//! `zcode-cli` `ui-layout-court.ts` (missing / stale / mismatched digests).
//! Does **not** authorize DO and does not claim the TypeScript adapter is
//! mounted or `ALIVE` out-of-repo.
//!
//! See `docs/rfc/v27.1.1/PHOTOLITHOGRAPHY-STANDING.md` hop 3→5.

use super::remediation_bound::{admit_remediation_bound, DEFAULT_MAX_REMEDIATION_ATTEMPTS};
use super::taffy_aot::{
    preflight_column_six_widths, AotColumn, AotRefusal, AotSealReceipt, WCAG_1412_TEXT_SPACING,
};

/// Standing for a successful in-repo ui-layout consume court.
#[derive(Debug, Clone, Copy, PartialEq, Eq)]
pub enum UiLayoutStanding {
    /// Digest consume fence passed on a live Taffy AOT seal.
    Alive,
}

impl UiLayoutStanding {
    pub fn as_str(self) -> &'static str {
        match self {
            Self::Alive => "ALIVE",
        }
    }
}

/// Machine-readable admit receipt for a successful ui-layout consume.
#[derive(Debug, Clone, PartialEq)]
pub struct UiLayoutReceipt {
    pub seal: AotSealReceipt,
    pub digest: String,
    pub standing: UiLayoutStanding,
    /// Remediation budget recorded on refuse paths (parity with zcode default).
    pub max_remediation_attempts: u32,
}

/// Typed refusals for the ui-layout consume fence (not geometry).
#[derive(Debug, Clone, PartialEq)]
pub enum UiLayoutRefusal {
    /// No seal was supplied — fail closed (never ambient admit).
    MissingSeal,
    /// Pinned or embedded digest absent/empty.
    MissingDigest,
    /// Caller pin does not equal the seal's embedded digest.
    MismatchedDigest { pinned: String, observed: String },
    /// Embedded digest does not match recomputation over sealed fields (stale/tamper).
    StaleDigest {
        embedded: String,
        recomputed: String,
    },
    /// Geometry law refused before a seal could be consumed.
    Geometry(AotRefusal),
    /// Remediation budget itself is unbounded (0) — refuse, do not loop.
    UnboundedRemediation { detail: &'static str },
}

impl UiLayoutRefusal {
    pub fn code(&self) -> &'static str {
        match self {
            Self::MissingSeal => "REFUSED_UI_LAYOUT_MISSING_SEAL",
            Self::MissingDigest => "REFUSED_UI_LAYOUT_MISSING_DIGEST",
            Self::MismatchedDigest { .. } => "REFUSED_UI_LAYOUT_MISMATCHED_DIGEST",
            Self::StaleDigest { .. } => "REFUSED_UI_LAYOUT_STALE_DIGEST",
            Self::Geometry(_) => "REFUSED_UI_LAYOUT_GEOMETRY",
            Self::UnboundedRemediation { .. } => "REFUSED_UNBOUNDED_REMEDIATION",
        }
    }
}

/// Consume an AOT seal against a pinned digest — fail closed.
///
/// Does not re-run Taffy layout. Integrity is the embedded digest matching
/// recomputation; identity is the pin matching the embedded digest.
pub fn consume_aot_seal(
    seal: Option<&AotSealReceipt>, pinned_digest: Option<&str>,
) -> Result<UiLayoutReceipt, UiLayoutRefusal> {
    let seal = seal.ok_or(UiLayoutRefusal::MissingSeal)?;
    let pinned = pinned_digest
        .map(str::trim)
        .filter(|s| !s.is_empty())
        .ok_or(UiLayoutRefusal::MissingDigest)?;
    if seal.digest.trim().is_empty() {
        return Err(UiLayoutRefusal::MissingDigest);
    }

    let recomputed = seal.recompute_digest();
    if seal.digest != recomputed {
        return Err(UiLayoutRefusal::StaleDigest {
            embedded: seal.digest.clone(),
            recomputed,
        });
    }
    if pinned != seal.digest {
        return Err(UiLayoutRefusal::MismatchedDigest {
            pinned: pinned.to_string(),
            observed: seal.digest.clone(),
        });
    }

    let max_remediation_attempts = admit_remediation_bound(DEFAULT_MAX_REMEDIATION_ATTEMPTS)
        .map_err(|e| match e {
            super::remediation_bound::RemediationBoundRefusal::UnboundedRemediation { detail } => {
                UiLayoutRefusal::UnboundedRemediation { detail }
            }
            other => UiLayoutRefusal::UnboundedRemediation {
                detail: other.code(),
            },
        })?;

    Ok(UiLayoutReceipt {
        seal: seal.clone(),
        digest: seal.digest.clone(),
        standing: UiLayoutStanding::Alive,
        max_remediation_attempts,
    })
}

/// Seal `column` under Gate-1 six widths @ WCAG 1.4.12 1.5×, then self-consume.
///
/// Geometry refusals surface as [`UiLayoutRefusal::Geometry`]. A successful
/// path proves the photolithography consume fence without mounting zcode-cli.
pub fn court_ui_layout_column(column: &AotColumn) -> Result<UiLayoutReceipt, UiLayoutRefusal> {
    let seal = preflight_column_six_widths(column, WCAG_1412_TEXT_SPACING)
        .map_err(UiLayoutRefusal::Geometry)?;
    let digest = seal.digest.clone();
    consume_aot_seal(Some(&seal), Some(&digest))
}
