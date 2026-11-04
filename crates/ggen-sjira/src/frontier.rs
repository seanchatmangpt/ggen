//! Frontier selection over the projected standing chain — port of the
//! `GgenIgniter.SemanticJira` frontier slice: `frontier/4`,
//! `frontier_from_events/4`, `project/2`, `project_standing/1`, and
//! `schedule/4` restricted to ggen-sjira's core-slice surface
//! (semantic_jira.ex lines 159-395 + the progression law at lines 833-838).
//!
//! Eligibility law (AC-04, SJ-002): a work order is frontier-eligible iff
//! (1) its chain-tip standing is UNKNOWN, (2) its `origin_authority`
//! resolves in the supplied authority index, and (3) every typed
//! dependency edge is satisfied by evidence. Selection fails CLOSED: a
//! malformed transition log, an illegal standing skip, or an unavailable
//! authority index blocks every order rather than silently selecting.
//!
//! IO-free. Authority is consumed from an index the caller supplies;
//! this kernel never creates authority.

use serde::{Deserialize, Serialize};
use serde_json::Value;
use std::collections::BTreeMap;

use crate::admit::{admit_work_order, Refusal};
use crate::work_order::Standing;

/// The nine typed dependency classes (`@dependency_types`,
/// semantic_jira.ex line 20).
pub const DEPENDENCY_TYPES: [&str; 9] = [
    "requiresSemanticIdentity",
    "requiresProjection",
    "requiresCapability",
    "requiresReceipt",
    "requiresRuntime",
    "requiresVerifier",
    "requiresObservation",
    "requiresPostcondition",
    "requiresPublication",
];

/// One typed dependency edge (`requiresSemanticIdentity` etc.).
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
#[serde(deny_unknown_fields)]
pub struct DependencyEdge {
    /// One of [`DEPENDENCY_TYPES`].
    pub kind: String,
    /// Identity of the upstream work order this edge requires.
    pub upstream: String,
    /// Optional standing the upstream evidence must carry.
    pub required_standing: Option<String>,
    /// Optional receipt digest the upstream evidence must deliver.
    pub required_receipt_digest: Option<String>,
}

/// One frontier candidate: the raw work-order JSON (admitted inside
/// [`frontier`]) plus the frontier-specific surface the core WorkOrder
/// struct does not carry.
#[derive(Debug, Clone)]
pub struct FrontierInput {
    /// Raw work-order JSON, admitted via `admit_work_order`.
    pub work_order: Value,
    /// Typed dependency edges (`requiresSemanticIdentity`, ...).
    pub dependencies: Vec<DependencyEdge>,
    /// Bounded path scope for lease-conflict fencing.
    pub path_scope: Vec<String>,
    /// Concurrency key for schedule fencing (lease issuance deferred).
    pub concurrency_key: String,
}

/// A standing-transition event from the append-only log.
#[derive(Debug, Clone, PartialEq, Eq, Deserialize)]
pub struct TransitionEvent {
    pub work_order_id: String,
    /// Standing before the transition (checked by the progression law).
    #[serde(default)]
    pub from_standing: Option<String>,
    /// Standing after the transition — the chain tip.
    pub to_standing: String,
    /// Log order within a generation. Higher seq wins.
    #[serde(default)]
    pub seq: u64,
    /// Receipt the transition records, matched by dependency evidence.
    #[serde(default)]
    pub receipt_digest: Option<String>,
}

/// Upstream evidence per work-order identity, consulted by
/// `dependency_satisfied?/2` (semantic_jira.ex line 1307).
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct Evidence {
    pub standing: Option<String>,
    pub receipt_digest: Option<String>,
}

/// Origin-authority index: IRI -> admission digest. The Rust slice has no
/// RDF authority store; resolution is exact-membership in this map.
pub type AuthorityIndex = BTreeMap<String, String>;

/// Per-order chain-tip standing from [`project_standing`].
pub type Standings = BTreeMap<String, String>;

/// Frontier result: eligible candidates, per-order blocked reasons, and
/// the projected chain-tip standings.
#[derive(Debug, Clone, PartialEq, Eq, Default)]
pub struct FrontierResult {
    pub eligible: Vec<EligibleCandidate>,
    pub blocked: Vec<BlockedEntry>,
    pub standings: Standings,
}

/// An eligible candidate carrying the immutable digests (`authority`
/// stays NONE — selection never creates authority).
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct EligibleCandidate {
    pub identity: String,
    pub title: String,
    pub subject: String,
    pub repository: String,
    pub base_sha: String,
    pub work_order_digest: String,
    pub definition_digest: String,
    pub origin_authority: String,
    pub origin_admission_digest: String,
    pub path_scope: Vec<String>,
    pub concurrency_key: String,
    /// Always NONE: selection never creates authority.
    pub authority: String,
}

/// A blocked order with its typed reason.
#[derive(Debug, Clone, PartialEq, Eq, Serialize)]
pub struct BlockedEntry {
    /// None when the whole selection failed closed (no specific order).
    pub identity: Option<String>,
    pub reason: String,
    /// Unsatisfied upstream identities when reason is
    /// `dependencies_unsatisfied`.
    #[serde(skip_serializing_if = "Vec::is_empty")]
    pub dependencies: Vec<String>,
}

/// Frontier over the event-sourced standing projection: each order's
/// standing is the latest logged `to_standing` (last transition wins;
/// `seq` orders the log), and dependency evidence is the logged
/// standing/receipt of each upstream (caller evidence is overridden).
/// Port of `frontier/4` + `frontier_from_events/4` folded into one
/// entry point.
///
/// Fails closed: a `None` authority index blocks every order with
/// `authority_index_unavailable`; a malformed event or an illegal
/// progression fails the whole selection closed with
/// `malformed_transition_log`.
pub fn frontier(
    orders: &[FrontierInput], events: &[TransitionEvent],
    evidence_by_id: &BTreeMap<String, Evidence>, authority: Option<&AuthorityIndex>,
) -> FrontierResult {
    let Some(index) = authority else {
        return FrontierResult {
            blocked: orders
                .iter()
                .map(|o| BlockedEntry {
                    identity: raw_identity(&o.work_order),
                    reason: "authority_index_unavailable".to_string(),
                    dependencies: Vec::new(),
                })
                .collect(),
            ..Default::default()
        };
    };

    // project/2: logged standing + receipt evidence per identity. The
    // progression law is enforced in project_standing; a refusal here
    // fails the whole selection closed.
    let (projected, logged) = match project(events) {
        Ok(p) => p,
        Err(refusal) => {
            let _ = refusal; // typed refusal carried in the reason only
            return FrontierResult {
                blocked: vec![BlockedEntry {
                    identity: None,
                    reason: "malformed_transition_log".to_string(),
                    dependencies: Vec::new(),
                }],
                ..Default::default()
            };
        }
    };

    // Caller evidence is overridden by logged evidence (frontier_from_events).
    let mut evidence: BTreeMap<String, Evidence> = BTreeMap::new();
    for (id, ev) in evidence_by_id {
        evidence.insert(id.clone(), ev.clone());
    }
    for (id, ev) in &logged {
        evidence.insert(id.clone(), ev.clone());
    }

    let mut result = FrontierResult {
        standings: projected.clone(),
        ..Default::default()
    };

    for input in orders {
        let identity = raw_identity(&input.work_order);

        let admitted = match admit_work_order(&input.work_order) {
            Ok(a) => a,
            Err(refusal) => {
                result.blocked.push(BlockedEntry {
                    identity,
                    reason: format!("admission_refused: {refusal}"),
                    dependencies: Vec::new(),
                });
                continue;
            }
        };

        // Chain tip wins over the order's declared standing.
        let standing_str = projected
            .get(&admitted.work_order.identity)
            .cloned()
            .unwrap_or_else(|| admitted.work_order.standing.as_str());
        if standing_str != "UNKNOWN" {
            result.blocked.push(BlockedEntry {
                identity: Some(admitted.work_order.identity.clone()),
                reason: format!("standing={standing_str}"),
                dependencies: Vec::new(),
            });
            continue;
        }

        // AC-04: syntactic presence of an IRI is not origin admission.
        match index.get(&admitted.work_order.origin_authority) {
            Some(origin_digest) => {
                let missing: Vec<String> = input
                    .dependencies
                    .iter()
                    .filter(|dep| !dependency_satisfied(dep, &evidence))
                    .map(|dep| dep.upstream.clone())
                    .collect();

                if missing.is_empty() {
                    result.eligible.push(EligibleCandidate {
                        identity: admitted.work_order.identity.clone(),
                        title: admitted.work_order.title.clone(),
                        subject: admitted.work_order.subject.clone(),
                        repository: admitted.work_order.repository.clone(),
                        base_sha: admitted.work_order.base_sha.clone(),
                        work_order_digest: admitted.work_order_digest,
                        definition_digest: admitted.definition_digest,
                        origin_authority: admitted.work_order.origin_authority.clone(),
                        origin_admission_digest: origin_digest.clone(),
                        path_scope: input.path_scope.clone(),
                        concurrency_key: input.concurrency_key.clone(),
                        authority: "NONE".to_string(),
                    });
                } else {
                    result.blocked.push(BlockedEntry {
                        identity: Some(admitted.work_order.identity.clone()),
                        reason: "dependencies_unsatisfied".to_string(),
                        dependencies: missing,
                    });
                }
            }
            None => {
                result.blocked.push(BlockedEntry {
                    identity: Some(admitted.work_order.identity.clone()),
                    reason: "origin_not_admitted".to_string(),
                    dependencies: Vec::new(),
                });
            }
        }
    }

    result
}

/// Projects current standing from the append-only transition log: the
/// latest `to_standing` per work-order id (log order is time; `seq`
/// orders within a generation). Port of `project_standing/1` + `project/2`
/// plus the progression law (semantic_jira.ex lines 833-838): the
/// strongest standing is never skipped from UNKNOWN in one event.
/// Any violation refuses the whole projection; events are never silently
/// dropped.
pub fn project_standing(events: &[TransitionEvent]) -> Result<Standings, Refusal> {
    let mut ordered: Vec<&TransitionEvent> = events.iter().collect();
    ordered.sort_by_key(|e| e.seq);

    let mut acc: Standings = BTreeMap::new();
    for event in ordered {
        // Standing vocabulary check (standing/1, line 17).
        let to = Standing::parse(&event.to_standing).map_err(|_| {
            Refusal::MalformedWorkOrder(format!(
                "malformed_transition: to_standing {:?} is not in the standing vocabulary",
                event.to_standing
            ))
        })?;

        // Progression law: the strongest standing is never skipped from
        // UNKNOWN in one event — explicit UNKNOWN -> PARTIAL_ALIVE -> ALIVE.
        if event.from_standing.as_deref() == Some("UNKNOWN") && event.to_standing == "ALIVE" {
            return Err(Refusal::MalformedWorkOrder(format!(
                "illegal_progression: UNKNOWN -> ALIVE skip on work order {:?} — \
                 remediation: transition through PARTIAL_ALIVE (SHACL progression law)",
                event.work_order_id
            )));
        }

        // Chain tip: the latest logged `to` wins (last transition wins).
        acc.insert(event.work_order_id.clone(), to.as_str());
    }
    Ok(acc)
}

/// project/2 restricted: the logged standing and receipt per identity.
fn project(events: &[TransitionEvent]) -> Result<(Standings, BTreeMap<String, Evidence>), Refusal> {
    let standings = project_standing(events)?;

    let mut ordered: Vec<&TransitionEvent> = events.iter().collect();
    ordered.sort_by_key(|e| e.seq);

    let mut logged: BTreeMap<String, Evidence> = BTreeMap::new();
    for event in ordered {
        logged.insert(
            event.work_order_id.clone(),
            Evidence {
                standing: Some(event.to_standing.clone()),
                receipt_digest: event.receipt_digest.clone(),
            },
        );
    }
    Ok((standings, logged))
}

/// Lease-conflict fencing stub over concurrency keys. ggen-sjira has no
/// lease issuance yet (`lease_request` deferred); this ports the schedule
/// law (`schedule/4` + `conflict?/2` + `overlap?/2`) fenced on key
/// uniqueness: an eligible candidate collides with an ACTIVE lease when
/// the concurrency keys match, or the repositories match and the path
/// scopes overlap (an empty side overlaps everything).
pub fn schedule(
    orders: &[FrontierInput], events: &[TransitionEvent],
    evidence_by_id: &BTreeMap<String, Evidence>, authority: Option<&AuthorityIndex>,
    active_leases: &[ActiveLease],
) -> FrontierResult {
    let selected = frontier(orders, events, evidence_by_id, authority);

    let mut result = selected;
    let mut collisions = Vec::new();
    result.eligible.retain(|candidate| {
        if active_leases.iter().any(|lease| conflict(candidate, lease)) {
            collisions.push(candidate.clone());
            false
        } else {
            true
        }
    });

    for candidate in collisions {
        result.blocked.push(BlockedEntry {
            identity: Some(candidate.identity),
            reason: "conflicting_active_lease".to_string(),
            dependencies: Vec::new(),
        });
    }
    result
}

/// An active lease record for fencing (issuance deferred; read-only use).
#[derive(Debug, Clone, PartialEq, Eq)]
pub struct ActiveLease {
    /// `None` or `Some("active")` is active; anything else is not.
    pub status: Option<String>,
    pub repository: String,
    pub scope: Vec<String>,
    /// Primary fencing key (unique per in-flight work).
    pub concurrency_key: String,
}

/// `conflict?/2` (semantic_jira.ex line 1319): ACTIVE lease, key match OR
/// repository match with scope overlap.
fn conflict(candidate: &EligibleCandidate, lease: &ActiveLease) -> bool {
    let active = matches!(lease.status.as_deref(), None | Some("active"));
    active
        && (lease.concurrency_key == candidate.concurrency_key
            || (lease.repository == candidate.repository
                && overlap(&candidate.path_scope, &lease.scope)))
}

/// `overlap?/2`: an empty side overlaps everything (the lease covers the
/// whole repo); otherwise any shared path.
fn overlap(left: &[String], right: &[String]) -> bool {
    if left.is_empty() || right.is_empty() {
        return true;
    }
    left.iter().any(|a| right.contains(a))
}

/// `dependency_satisfied?/2` (semantic_jira.ex line 1307): upstream
/// evidence must EXIST and satisfy both optional constraints.
fn dependency_satisfied(dep: &DependencyEdge, evidence: &BTreeMap<String, Evidence>) -> bool {
    let Some(observed) = evidence.get(&dep.upstream) else {
        return false;
    };
    // The igniter refuses an empty evidence map (`observed != %{}`); the
    // struct-form degenerate case is both fields absent.
    if observed.standing.is_none() && observed.receipt_digest.is_none() {
        return false;
    }
    let standing_ok = match (&dep.required_standing, &observed.standing) {
        (Some(req), Some(got)) => req == got,
        (Some(_), None) => false,
        (None, _) => true,
    };
    let receipt_ok = match (&dep.required_receipt_digest, &observed.receipt_digest) {
        (Some(req), Some(got)) => req == got,
        (Some(_), None) => false,
        (None, _) => true,
    };
    standing_ok && receipt_ok
}

/// Identity of a raw work-order JSON value when admission has not run.
fn raw_identity(raw: &Value) -> Option<String> {
    raw.get("identity")
        .and_then(|v| v.as_str())
        .map(String::from)
}
