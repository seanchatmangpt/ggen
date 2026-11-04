//! Verified receipt -> lawful canonical-state transition - port of
//! `GgenIgniter.SemanticJira.Reconciler`
//! (`ggen_igniter/lib/ggen_igniter/semantic_jira/reconciler.ex`).
//!
//! `reconcile(order, receipt, ledger_path, opts)` consumes a receipt map
//! (`definition_digest`, `snapshot_digest`, `target`, `candidate_sha`,
//! `evidence`, `vc`) and the raw WorkOrder JSON:
//!
//! 1. The ledger read is the first court: a tampered or undecodable ledger
//!    refuses ([`ReconcileRefusal::Ledger`]) before any admission.
//! 2. `definition_digest` must equal the order's definition digest, else
//!    [`ReconcileRefusal::DefinitionMismatch`]. The snapshot may be STALE
//!    (standing moved since the receipt was made) - a stale snapshot with a
//!    matching definition is admissible, so sibling transitions are not
//!    invalidated; the snapshot digest is only checked for well-formedness.
//! 3. `snapshot_digest` must be well-formed `sha256:<64 lowercase hex>`,
//!    else [`ReconcileRefusal::InvalidSnapshotDigest`].
//! 4. Replaying a receipt already recorded in the log is idempotent: the
//!    recorded event is returned `Ok` with `AppendStatus::AlreadyRecorded`
//!    - [`ledger::AppendStatus`] is REUSED, never re-invented.
//! 5. The order is admitted; a malformed order refuses
//!    [`ReconcileRefusal::Unadmitted`].
//! 6. The origin-authority law (SJ-002, closes the A1 class): the order's
//!    `origin_authority` must resolve in the supplied
//!    [`authority::AuthorityIndex`] - the same law frontier selection
//!    applies. Missing index refuses
//!    [`ReconcileRefusal::AuthorityIndexUnavailable`]; a resolution refusal
//!    maps to [`ReconcileRefusal::OriginNotAdmitted`] carrying the typed
//!    [`authority::OriginRefusal`].
//! 7. Vector-clock conflict law (loops-of-loops spec section 1 Loop 2): an
//!    incoming receipt carrying `vc` is refused when it is CONCURRENT with
//!    the last event's `vc` for the same identity
//!    ([`ReconcileRefusal::VcConcurrent`]) - before any byte is written.
//!    When ordered, the receipt's vc is stamped onto the appended event.
//!    Events and receipts without a `vc` never trip the law.
//! 8. `candidate_sha` law: when the raw order JSON carries `candidate_sha`,
//!    the receipt's must equal it, else
//!    [`ReconcileRefusal::CandidateShaMismatch`]. Disclosed divergences: (a) the law
//!    is checked BEFORE admission - the core fifteen-field `WorkOrder`
//!    struct carries no `candidate_sha` field and admission refuses unknown
//!    fields, so a candidate_sha-carrying order would otherwise die at
//!    admission with the less precise refusal; the mismatch is diagnosed
//!    first, and the order then still refuses `Unadmitted`. (b) The law
//!    binds against the raw order JSON; the Elixir law treats a nil order
//!    candidate_sha as vacuously satisfied, which this port preserves.
//! 9. Projection over the log reuses the frontier's progression law
//!    (UNKNOWN never skips to ALIVE in one event) and the standing
//!    vocabulary; a malformed log refuses rather than silently projecting.
//!    [`promote::promote`] is the pure admission calculus (courts,
//!    evidence class/ceiling, falsifiers, replay identity); its refusal is
//!    [`ReconcileRefusal::PromotionRefused`]. Dependency evidence comes
//!    from the log through the projection, never from the receipt.
//! 10. On admit an event is appended through [`ledger::append`]; the
//!     ledger stamps seq/epoch/vc/event_digest, so replay of the same
//!     receipt is idempotent through the recorded `receipt_digest` check.
//!
//! Authority stays NONE; this module does not perform DO.
//!
//! IO through the ledger only. `#![forbid(unsafe_code)]` crate-wide; no
//! unwrap on external input; typed refusals fail closed.

use std::collections::BTreeMap;
use std::path::Path;

use serde_json::{json, Map, Value};

use crate::admit::{admit_work_order, valid_digest};
use crate::authority::{AuthorityIndex, OriginRefusal};
use crate::ledger::{self, AppendOpts, AppendStatus, LedgerError};
use crate::promote::{promote, PromoteRefusal};
use crate::work_order::digest_of;

/// The all-zeros genesis digest (`@genesis`, reconciler.ex line 160) - the
/// chain tail of an empty log. Same all-zeros grammar as
/// [`crate::receipts::GENESIS_PREV_HASH`] but with the `sha256:` prefix the
/// event-digest form carries.
pub const GENESIS_DIGEST: &str =
    "sha256:0000000000000000000000000000000000000000000000000000000000000000";

/// Typed refusal for every reconcile path - the `{:error, {:refused, _}}`
/// family of reconciler.ex, one variant per refusal reason.
#[derive(Debug, Clone, PartialEq, thiserror::Error)]
pub enum ReconcileRefusal {
    /// The ledger cannot be decoded or an event's digest no longer
    /// recomputes (tampered) - refused before any admission.
    #[error("ledger_refused: {0}")]
    Ledger(LedgerError),

    /// The receipt's `definition_digest` differs from the order's.
    #[error("definition_mismatch: receipt {found:?} != order {expected:?}")]
    DefinitionMismatch { expected: String, found: String },

    /// The receipt's `snapshot_digest` is not a well-formed digest.
    #[error("invalid_snapshot_digest: {found:?}")]
    InvalidSnapshotDigest { found: String },

    /// The work order itself does not admit.
    #[error("unadmitted: {0}")]
    Unadmitted(crate::admit::Refusal),

    /// No authority index was supplied (`opts.authority` is `None`).
    #[error("authority_index_unavailable: {detail}")]
    AuthorityIndexUnavailable { detail: String },

    /// The order's `origin_authority` does not resolve in the index - the
    /// origin law: an order that could never be selected can never be
    /// promoted either (SJ-002 AC-04).
    #[error("origin_not_admitted: {0}")]
    OriginNotAdmitted(OriginRefusal),

    /// The receipt's `candidate_sha` differs from the order's.
    #[error("candidate_sha_mismatch: order {expected:?} receipt {found:?}")]
    CandidateShaMismatch { expected: String, found: String },

    /// The receipt's vector clock is concurrent with the last event's clock
    /// for the same identity - neither dominates. Refused before any byte
    /// is written.
    #[error("vc_concurrent: incoming {incoming:?} vs last {last:?}")]
    VcConcurrent { incoming: Value, last: Value },

    /// The promotion calculus refused (courts/evidence/falsifiers/replay).
    #[error("promotion_refused: {0}")]
    PromotionRefused(PromoteRefusal),
}

/// Options for [`reconcile`] (port of the Elixir `opts` keyword list).
#[derive(Debug, Clone, Default)]
pub struct ReconcileOpts {
    /// The authority index the origin law resolves against. `None` refuses
    /// [`ReconcileRefusal::AuthorityIndexUnavailable`] - fail closed: the
    /// Elixir default (the canonical semantic-jira-pack index) has no Rust
    /// equivalent, so a caller MUST supply one.
    pub authority: Option<AuthorityIndex>,
}

/// A successful reconcile: the appended (or replayed) event as it exists in
/// the ledger (seq/epoch/vc/event_digest included), and how it got there.
#[derive(Debug, Clone, PartialEq)]
pub struct Reconciled {
    /// The stamped event (seq/epoch/vc/event_digest included).
    pub event: Value,
    pub status: AppendStatus,
}

/// The reconcile calculus (`reconcile/4`, reconciler.ex lines 47-95).
#[allow(clippy::result_large_err)] // ReconcileRefusal carries typed wire Values; boxing would obscure the refusal payload the court needs to read.
pub fn reconcile(
    order: &Value, receipt: &Value, ledger_path: &Path, opts: &ReconcileOpts,
) -> Result<Reconciled, ReconcileRefusal> {
    // 1. The ledger read is the first court (tampered/unreadable refuses).
    let events = ledger::fetch(ledger_path)?;

    // 8. candidate_sha law against the raw order JSON (module doc, pt 8).
    if let (Some(order_sha), Some(receipt_sha)) = (
        order.get("candidate_sha").and_then(|v| v.as_str()),
        receipt.get("candidate_sha").and_then(|v| v.as_str()),
    ) {
        if order_sha != receipt_sha {
            return Err(ReconcileRefusal::CandidateShaMismatch {
                expected: order_sha.to_string(),
                found: receipt_sha.to_string(),
            });
        }
    }

    // 5->2. Admit once; both digests come from admission.
    let admitted = admit_work_order(order).map_err(ReconcileRefusal::Unadmitted)?;

    // 2. definition_digest must match the order's.
    let receipt_def = receipt
        .get("definition_digest")
        .and_then(|v| v.as_str())
        .unwrap_or("");
    if receipt_def != admitted.definition_digest {
        return Err(ReconcileRefusal::DefinitionMismatch {
            expected: admitted.definition_digest.clone(),
            found: receipt_def.to_string(),
        });
    }

    // 3. Snapshot well-formedness only (staleness is admissible).
    let snapshot = receipt
        .get("snapshot_digest")
        .and_then(|v| v.as_str())
        .unwrap_or("");
    if !valid_digest(snapshot) {
        return Err(ReconcileRefusal::InvalidSnapshotDigest {
            found: snapshot.to_string(),
        });
    }

    // 4. Idempotent replay: a receipt already in the log returns its event.
    let receipt_digest = digest_of(receipt);
    if let Some(existing) = events
        .iter()
        .find(|e| e.get("receipt_digest").and_then(|d| d.as_str()) == Some(receipt_digest.as_str()))
    {
        return Ok(Reconciled {
            event: existing.clone(),
            status: AppendStatus::AlreadyRecorded,
        });
    }

    // 7. Vector-clock conflict law, before any byte is written.
    let incoming_vc = receipt.get("vc").and_then(|v| v.as_object());
    let last = events.iter().rev().find(|e| {
        e.get("identity").and_then(|v| v.as_str()) == Some(admitted.work_order.identity.as_str())
            && e.get("vc").map(|v| v.is_object()).unwrap_or(false)
    });
    if let (Some(incoming), Some(last)) = (incoming_vc, last) {
        let last_vc = last
            .get("vc")
            .and_then(|v| v.as_object())
            .expect("vc is object");
        if ledger::vc_concurrent(incoming, last_vc) {
            return Err(ReconcileRefusal::VcConcurrent {
                incoming: Value::Object(incoming.clone()),
                last: Value::Object(last_vc.clone()),
            });
        }
    }

    // 6. The origin law, against the caller-supplied index.
    let index =
        opts.authority
            .as_ref()
            .ok_or_else(|| ReconcileRefusal::AuthorityIndexUnavailable {
                detail: "no authority index supplied (ReconcileOpts.authority is None)".to_string(),
            })?;
    index
        .resolve(&admitted.work_order.origin_authority)
        .map_err(ReconcileRefusal::OriginNotAdmitted)?;

    // 9. Projection over the log: current standing + dependency evidence.
    //    Non-transition events (no identity/to) are not standing
    //    transitions; they still participated in the recorded/vc courts.
    let mut current: Option<&Value> = None;
    let mut dependency_evidence: BTreeMap<String, Value> = BTreeMap::new();
    for event in &events {
        let (Some(identity), Some(to)) = (
            event.get("identity").and_then(|v| v.as_str()),
            event.get("to").and_then(|v| v.as_str()),
        ) else {
            continue;
        };
        // Standing vocabulary + progression law (UNKNOWN never skips to
        // ALIVE in one event) - the frontier's projection law, applied so a
        // malformed log refuses rather than silently projecting.
        if let Some(from) = event.get("from").and_then(|v| v.as_str()) {
            if from == "UNKNOWN" && to == "ALIVE" {
                return Err(ReconcileRefusal::Ledger(LedgerError::Unreadable(format!(
                    "illegal_progression: UNKNOWN -> ALIVE skip on identity {identity:?}"
                ))));
            }
        }
        if crate::work_order::Standing::parse(to).is_err() {
            return Err(ReconcileRefusal::Ledger(LedgerError::Unreadable(format!(
                "malformed_transition: to {to:?} not in the standing vocabulary"
            ))));
        }
        if identity == admitted.work_order.identity {
            current = Some(event);
        }
        dependency_evidence.insert(
            identity.to_string(),
            json!({
                "standing": to,
                "receipt_digest": event.get("receipt_digest").cloned().unwrap_or(Value::Null),
            }),
        );
    }
    let from = current
        .map(|e| {
            e.get("to")
                .and_then(|v| v.as_str())
                .unwrap_or("UNKNOWN")
                .to_string()
        })
        .unwrap_or_else(|| admitted.work_order.standing.as_str());

    // Evidence: the receipt's evidence plus the derived bindings.
    let mut evidence = receipt
        .get("evidence")
        .cloned()
        .unwrap_or(Value::Object(Map::new()));
    {
        let ev = evidence
            .as_object_mut()
            .ok_or(PromoteRefusal::ReplayIdentityMismatch)?; // non-object evidence -> promotion refuses
        ev.insert(
            "work_order_digest".to_string(),
            Value::String(admitted.work_order_digest.clone()),
        );
        ev.insert(
            "dependency_evidence".to_string(),
            Value::Object(dependency_evidence.into_iter().collect()),
        );
    }

    // 9. The promotion calculus is the pure admission function.
    let target = receipt.get("target").and_then(|v| v.as_str()).unwrap_or("");
    let intent = promote(&admitted, target, &evidence)?;

    // 10. The standing-transition event, appended through the ledger.
    let mut event = Map::new();
    event.insert("kind".to_string(), json!("standing_transition_event"));
    event.insert("identity".to_string(), json!(admitted.work_order.identity));
    event.insert(
        "definition_digest".to_string(),
        json!(admitted.definition_digest),
    );
    event.insert("snapshot_digest".to_string(), json!(snapshot));
    event.insert("from".to_string(), json!(from));
    event.insert("to".to_string(), json!(target));
    event.insert("receipt_digest".to_string(), json!(receipt_digest));
    event.insert(
        "transition_digest".to_string(),
        json!(intent.transition_digest),
    );
    event.insert("authority".to_string(), json!("NONE"));

    // The receipt's vector clock rides to the ledger (stamped or omitted).
    let vc = incoming_vc.and_then(vc_to_btmap);
    let appended = ledger::append(
        ledger_path,
        &event,
        &AppendOpts {
            vc,
            ..Default::default()
        },
    )?;
    Ok(Reconciled {
        event: appended.event,
        status: appended.status,
    })
}

fn vc_to_btmap(vc: &Map<String, Value>) -> Option<BTreeMap<String, u64>> {
    let mut out = BTreeMap::new();
    for (k, v) in vc {
        out.insert(k.clone(), v.as_u64()?);
    }
    Some(out)
}

/// Chain tail: the last event's `event_digest`, or [`GENESIS_DIGEST`] for an
/// empty log (`tail_digest/1`, reconciler.ex lines 183-188).
pub fn tail_digest(events: &[Value]) -> String {
    events
        .last()
        .and_then(|e| e.get("event_digest"))
        .and_then(|d| d.as_str())
        .map(|s| s.to_string())
        .unwrap_or_else(|| GENESIS_DIGEST.to_string())
}

// SLA minimal-unblock (DS-2 lane, 2026-11-03): `?` conversion impls the
// promote/ledger hops above need; disclosed in the DS-2 and DS-1 receipts.
impl From<LedgerError> for ReconcileRefusal {
    fn from(e: LedgerError) -> Self {
        Self::Ledger(e)
    }
}

impl From<PromoteRefusal> for ReconcileRefusal {
    fn from(e: PromoteRefusal) -> Self {
        Self::PromotionRefused(e)
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn genesis_is_sha256_prefixed_all_zeros() {
        assert!(GENESIS_DIGEST.starts_with("sha256:"));
        assert_eq!(GENESIS_DIGEST.len(), 71);
        assert_eq!(tail_digest(&[]), GENESIS_DIGEST);
    }
}
