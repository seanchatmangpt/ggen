//! Clean-state replay court for the portable receipt envelope (RFC §53/§57).
//!
//! Ordinary manufacture reports `replay.status = UNKNOWN` forever ("reports
//! standing, does not confer it"). Only this court promotes: it independently
//! re-hashes every consequence named in an [`PortableReceiptEnvelope`]
//! straight off disk and compares against the bound digest. Promotion rule:
//!
//! * `standing == "ALIVE"` **and** every consequence re-hashes equally
//!   → [`ReplayVerdict::Pass`] (with recompute timestamp + per-file digests);
//! * any consequence missing on disk → typed [`ReplayVerdict::Refused`]
//!   (`REPLAY_ABSENT`);
//! * any digest mismatch → typed [`ReplayVerdict::Refused`]
//!   (`REPLAY_MISMATCH`);
//! * `standing != "ALIVE"` (`PARTIAL_ALIVE`, `REFUSED:*`, anything else)
//!   → [`ReplayVerdict::Unknown`] — never promotes, never collapses.
//!
//! There is no silent pass: the court either produces PASS with evidence or a
//! typed non-PASS with the failing target bound in the verdict.

use std::path::Path;
use std::time::{SystemTime, UNIX_EPOCH};

use serde::Deserialize;

use crate::error::{AppError, Result};

/// Replay relation status values the court may produce (§82 vocabulary,
/// never collapsed).
pub const REPLAY_PASS: &str = "PASS";
/// Replay status written on typed refusal (court ran, evidence failed).
pub const REPLAY_REFUSED: &str = "REFUSED";
/// Replay status when the court cannot even evaluate (non-ALIVE standing).
pub const REPLAY_UNKNOWN: &str = "UNKNOWN";

/// One consequence re-verified by the court.
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct VerifiedConsequence {
    /// Root-relative target path (as bound in the envelope).
    pub target: String,
    /// The digest bound in the envelope (`sha256:<hex>` payload is bare hex).
    pub bound_sha256: String,
    /// The digest independently recomputed off disk.
    pub recomputed_sha256: String,
}

/// Typed verdict of the replay court. A non-`Pass` verdict always carries the
/// reason and — where one exists — the single first failing target.
#[derive(Debug, Clone, PartialEq, Eq)]
pub enum ReplayVerdict {
    /// All consequences re-verified equally off disk.
    Pass {
        /// Unix-millis timestamp of the recompute.
        verified_at_unix_ms: u128,
        /// Per-file evidence, in envelope (target-sorted) order.
        digests: Vec<VerifiedConsequence>,
    },
    /// Typed refusal: evidence failed. `code` ∈ {`REPLAY_ABSENT`,
    /// `REPLAY_MISMATCH`, `REPLAY_UNREADABLE`}.
    Refused {
        /// Typed refusal identity.
        code: String,
        /// First failing target, when one is identified.
        target: Option<String>,
        /// Human-readable detail.
        detail: String,
    },
    /// The court cannot evaluate this envelope (standing is not ALIVE).
    Unknown {
        /// Why promotion is impossible.
        reason: String,
    },
}

impl ReplayVerdict {
    /// `true` only for [`ReplayVerdict::Pass`].
    #[must_use]
    pub fn is_pass(&self) -> bool {
        matches!(self, Self::Pass { .. })
    }
}

/// Minimal deserializable view of the persisted envelope. Deliberately a
/// separate type: [`crate::portable_receipt::PortableReceiptEnvelope`] is
/// Serialize-only by design (the engine never reads back its own claims as
/// authority), while the court reads a persisted envelope as evidence input.
#[derive(Debug, Clone, Deserialize)]
pub struct EnvelopeView {
    /// Reported standing (§82 vocabulary).
    pub standing: String,
    /// Manufactured consequences, target-sorted.
    pub consequences: Vec<ConsequenceView>,
}

/// Deserialized view of one manufactured consequence for re-hashing.
#[derive(Debug, Clone, Deserialize)]
pub struct ConsequenceView {
    /// Root-relative target path.
    pub target: String,
    /// SHA-256 hex bound at manufacture.
    pub sha256: String,
}

/// Execute the court against a persisted envelope on disk.
///
/// # Errors
/// Typed `[FM-CHAIN-019]` when the envelope cannot be read/parsed as JSON.
pub fn execute_replay_court(root: &Path, envelope_path: &Path) -> Result<ReplayVerdict> {
    let bytes = std::fs::read(envelope_path).map_err(|e| {
        AppError::fm_chain(
            19,
            format!(
                "replay court: cannot read envelope {}: {e}",
                envelope_path.display()
            ),
        )
    })?;
    let envelope: EnvelopeView = serde_json::from_slice(&bytes).map_err(|e| {
        AppError::fm_chain(
            19,
            format!(
                "replay court: cannot parse envelope {}: {e}",
                envelope_path.display()
            ),
        )
    })?;
    Ok(verdict_for(root, &envelope))
}

/// The promotion rule, pure: no I/O beyond the consequence re-hash reads.
#[must_use]
pub fn verdict_for(root: &Path, envelope: &EnvelopeView) -> ReplayVerdict {
    // Promotion precondition: only ALIVE envelopes may promote. PARTIAL_ALIVE
    // never promotes (the run itself admitted incomplete landing); REFUSED:*
    // never promotes (admission refused the manufacture).
    if envelope.standing != "ALIVE" {
        return ReplayVerdict::Unknown {
            reason: format!(
                "replay court promotes ALIVE envelopes only; this envelope \
                 reports `{}` — PARTIAL_ALIVE and REFUSED never promote",
                envelope.standing
            ),
        };
    }

    let mut digests = Vec::with_capacity(envelope.consequences.len());
    for consequence in &envelope.consequences {
        use sha2::Digest as _;
        let path = root.join(&consequence.target);
        let bytes = match std::fs::read(&path) {
            Ok(bytes) => bytes,
            Err(e) if e.kind() == std::io::ErrorKind::NotFound => {
                return ReplayVerdict::Refused {
                    code: "REPLAY_ABSENT".to_string(),
                    target: Some(consequence.target.clone()),
                    detail: format!(
                        "replay court: consequence `{}` is absent from disk; \
                         clean-state reconstruction failed",
                        consequence.target
                    ),
                }
            }
            Err(e) => {
                return ReplayVerdict::Refused {
                    code: "REPLAY_UNREADABLE".to_string(),
                    target: Some(consequence.target.clone()),
                    detail: format!(
                        "replay court: consequence `{}` cannot be read: {e}",
                        consequence.target
                    ),
                }
            }
        };
        let recomputed = hex::encode(sha2::Sha256::digest(&bytes));
        if recomputed != consequence.sha256 {
            return ReplayVerdict::Refused {
                code: "REPLAY_MISMATCH".to_string(),
                target: Some(consequence.target.clone()),
                detail: format!(
                    "replay court: consequence `{}` digest mismatch (bound \
                     `{}`, recomputed `{}`) — landed state diverged from the \
                     envelope's bound identity",
                    consequence.target, consequence.sha256, recomputed
                ),
            };
        }
        digests.push(VerifiedConsequence {
            target: consequence.target.clone(),
            bound_sha256: consequence.sha256.clone(),
            recomputed_sha256: recomputed,
        });
    }

    let verified_at_unix_ms = SystemTime::now()
        .duration_since(UNIX_EPOCH)
        .map(|d| d.as_millis())
        .unwrap_or_default();
    ReplayVerdict::Pass {
        verified_at_unix_ms,
        digests,
    }
}

/// Execute the court and, only on [`ReplayVerdict::Pass`], rewrite the
/// persisted envelope's `replay.status` to `PASS`. Any non-Pass verdict is
/// never persisted as PASS (and the envelope is left untouched — the court
/// reports, it does not manufacture).
///
/// # Errors
/// Propagates [`execute_replay_court`] errors; typed `[FM-CHAIN-020]` on
/// persistence I/O failure.
pub fn execute_and_persist(root: &Path, envelope_path: &Path) -> Result<ReplayVerdict> {
    let verdict = execute_replay_court(root, envelope_path)?;
    if !verdict.is_pass() {
        return Ok(verdict);
    }
    let bytes = std::fs::read(envelope_path).map_err(|e| {
        AppError::fm_chain(
            20,
            format!(
                "replay court: cannot re-read envelope {}: {e}",
                envelope_path.display()
            ),
        )
    })?;
    let mut value: serde_json::Value = serde_json::from_slice(&bytes).map_err(|e| {
        AppError::fm_chain(
            20,
            format!(
                "replay court: cannot re-parse envelope {}: {e}",
                envelope_path.display()
            ),
        )
    })?;
    value["replay"]["status"] = serde_json::Value::String(REPLAY_PASS.to_string());
    std::fs::write(envelope_path, serde_json::to_vec_pretty(&value)?).map_err(|e| {
        AppError::fm_chain(
            20,
            format!(
                "replay court: cannot persist promoted envelope {}: {e}",
                envelope_path.display()
            ),
        )
    })?;
    Ok(verdict)
}
