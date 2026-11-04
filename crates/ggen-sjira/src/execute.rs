//! The execution loop — Rust port of `GgenIgniter.SemanticJira.Execute`
//! (ggen_igniter/lib/ggen_igniter/semantic_jira/execute.ex) restricted to
//! ggen-sjira's admitted-plan surface: the chain
//! frontier -> authority -> plan -> receipt -> reconcile, driven to
//! fixed point over a REAL file ledger.
//!
//! Per the igniter's honest-ceiling law (execute.ex lines 57-62): a
//! repository-local execution caps at PARTIAL_ALIVE — `head_verified` is
//! never true and `outcome` is never "alive"; ALIVE stays
//! external-fabric-only. This module encodes that law structurally: the
//! only promotion this loop can write is `UNKNOWN -> PARTIAL_ALIVE`
//! ([`check_promotion`]); anything else is a typed
//! [`ExecRefusal`] with reason `standing_ceiling`.
//!
//! Loop shape (each pass is one bounded iteration):
//!
//! 1. **frontier** — [`ledger::fetch`] the REAL ledger (a tampered or
//!    undecodable ledger refuses `ledger_refused` BEFORE anything runs —
//!    the ledger is read, never written, at this hop) and decode the typed
//!    [`TransitionEvent`]s; [`frontier`] selects. Fails closed on a
//!    malformed log.
//! 2. **authority** — each eligible candidate's `origin_authority` must
//!    resolve in the typed [`AuthorityIndex`](crate::authority::AuthorityIndex)
//!    (built pinned from the caller's `AuthorityGraph`) — frontier's
//!    map-membership gate PLUS the digest/pin law of authority.rs. A
//!    candidate that fails the typed law refuses `authority_refused`
//!    (`R_missing_authority`), fail closed.
//! 3. **plan** — the order's projections and required_courts ARE the
//!    admitted plan ([`ExecutionPlan`]): the Go-side execution itself is
//!    the consumer's (the local backend caps at partial_alive); sjira's
//!    contract is the ADMITTED plan + receipt. The plan digest (canonical
//!    sha256 over the plan) is the receipt digest the transition records;
//!    downstream `requiresReceipt` edges match against it.
//! 4. **receipt** — every executed order is journaled in a blake3
//!    [`TransitionJournal`] as one `Decision::Admitted` transition
//!    (authority `NONE` — this kernel never creates authority).
//! 5. **reconcile** — the stamped event is appended to the REAL ledger
//!    via [`ledger::append`] (the LAST write: every earlier refusal
//!    leaves the ledger byte-unchanged). The append stamps seq/epoch/
//!    event_digest; the next pass re-reads the ledger, so downstream
//!    dependents become eligible exactly when their upstream evidence
//!    (standing + receipt digest) is on disk.
//!
//! Termination: the loop ends when the frontier empties — every remaining
//! order blocked, with [`Executed::blocked`] carrying the typed reasons —
//! or refuses typed [`Exhausted`](ExecRefusal) when a pass makes no
//! progress or `opts.max_iterations` passes have run with eligible orders
//! still present. Never a silent stop.
//!
//! `#![forbid(unsafe_code)]` is inherited from the crate root. The one IO
//! is the ledger path the caller names; everything else is in-process.
//!
//! Modeled on `ggen_igniter/lib/ggen_igniter/semantic_jira/execute.ex`
//! (the port spec).

use std::collections::BTreeMap;
use std::path::Path;

use serde_json::{Map, Value};

use crate::admit::{admit_work_order, AdmittedWorkOrder};
use crate::authority::AuthorityGraph;
use crate::frontier::{frontier, EligibleCandidate, FrontierInput};
use crate::ledger::{self, AppendOpts, AppendStatus};
use crate::receipts::{Decision, TransitionJournal};
use crate::work_order::digest_of;
use crate::{BlockedEntry, Standings, TransitionEvent};

/// The standing the execution loop may write. ALIVE is external-fabric-only
/// (execute.ex lines 57-62): a local admission caps at PARTIAL_ALIVE.
pub const EXECUTION_CEILING: &str = "PARTIAL_ALIVE";

/// The standing an executed order transitions FROM.
pub const EXECUTION_FROM: &str = "UNKNOWN";

/// Default iteration budget for [`run`].
pub const DEFAULT_MAX_ITERATIONS: usize = 64;

/// One refused execution, typed with the igniter's `broken_term`
/// vocabulary (execute.ex lines 104-118): `standing`, `reason`,
/// `broken_term`, `hop`, `detail`.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
#[error("REFUSED {reason} (hop={hop}, broken_term={broken_term}): {detail}")]
pub struct ExecRefusal {
    /// Always `REFUSED`.
    pub standing: String,
    pub reason: String,
    pub broken_term: String,
    /// The named hop that refused: `frontier` | `authority` | `reconcile`.
    pub hop: String,
    pub detail: String,
}

impl ExecRefusal {
    fn new(hop: &str, reason: &str, broken_term: &str, detail: String) -> Self {
        Self {
            standing: "REFUSED".to_string(),
            reason: reason.to_string(),
            broken_term: broken_term.to_string(),
            hop: hop.to_string(),
            detail,
        }
    }
}

/// The admitted execution plan for one order — sjira's execution contract.
/// The consumer (fabric/backend) executes the plan; this kernel's receipt
/// commits to the plan bytes, never to a fabricated outcome.
#[derive(Debug, Clone, PartialEq, Eq, serde::Serialize)]
pub struct ExecutionPlan {
    pub identity: String,
    pub repository: String,
    pub base_sha: String,
    /// The order's deterministic projection classes.
    pub projections: Vec<String>,
    pub required_courts: Vec<String>,
    pub path_scope: Vec<String>,
    pub concurrency_key: String,
    /// Always `NONE` — this kernel never creates authority.
    pub authority: String,
    /// Always `PARTIAL_ALIVE` — the honest ceiling; see the module doc.
    pub target_standing: String,
    /// Always `false` — head verification is external-fabric-only.
    pub head_verified: bool,
    /// Canonical sha256 over the plan — the receipt digest the transition
    /// records; downstream `requiresReceipt` edges match against it.
    pub plan_digest: String,
}

/// The typed lease-gate refusal (DS-10). The gate binds an admitted
/// [`AdmittedLease`](crate::lease::AdmittedLease) and an epoch watermark to
/// each order's PLAN+RECEIPT emission; `into()` renders it as an
/// [`ExecRefusal`] that leaves the ledger byte-unchanged (the gate sits
/// BEFORE the plan step and the append is still the last write).
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum LeaseGateRefusal {
    /// The lease does not bind this order: either it is not admitted
    /// (`admitted != true`) or its `subject_iri` does not resolve to the
    /// order's identity.
    #[error("LeaseNotBound: {0}")]
    LeaseNotBound(String),
    /// The epoch gate refused: the order's plan carries pre-watermark
    /// implementation (or carries an epoch with no usable watermark).
    #[error("EpochStale: {0}")]
    EpochStale(String),
}

impl From<LeaseGateRefusal> for ExecRefusal {
    fn from(r: LeaseGateRefusal) -> Self {
        match r {
            LeaseGateRefusal::LeaseNotBound(detail) => {
                ExecRefusal::new("lease", "lease_not_bound", "R_missing_authority", detail)
            }
            LeaseGateRefusal::EpochStale(detail) => {
                ExecRefusal::new("lease", "epoch_stale", "mu_on_O", detail)
            }
        }
    }
}

/// Options for [`run`].
#[derive(Debug, Clone)]
pub struct ExecuteOpts {
    /// Maximum loop passes before the typed `exhausted` refusal.
    pub max_iterations: usize,
    /// Optional admitted sovereign lease (lease.rs). When `Some`, EVERY
    /// executed order's plan emission is gated on the lease binding the
    /// order (`subject_iri` resolving to the order identity) and on
    /// `admitted == true` — otherwise the typed `LeaseNotBound` refusal.
    /// `None` preserves the legacy (un-gated) behavior byte-identically.
    pub lease: Option<crate::lease::AdmittedLease>,
    /// Optional decoded epoch watermark (`watermark.json` shape). Judged
    /// together with [`Self::epoch_extensions`] through [`epoch::check`];
    /// any epoch refusal is the typed `EpochStale` — including an
    /// epoch-carrying extension with NO watermark (fail closed). `None`
    /// plus empty `epoch_extensions` preserves the legacy behavior.
    pub epoch_watermark: Option<Value>,
    /// Per-order epoch extension JSON (`identity` → `{epoch, plan_touches,
    /// manufacture_plan, source_artifacts}`). The strict WorkOrder schema
    /// refuses unknown fields, so the epoch extension rides beside it, at
    /// the run level. An entry whose `epoch` is nonempty is judged through
    /// [`epoch::check`] against [`Self::epoch_watermark`] — no watermark
    /// supplied means the typed `EpochStale` (fail closed).
    pub epoch_extensions: std::collections::BTreeMap<String, Value>,
}

impl ExecuteOpts {
    /// Options with the default iteration budget and no lease/epoch gate.
    pub fn legacy() -> Self {
        Self {
            max_iterations: DEFAULT_MAX_ITERATIONS,
            lease: None,
            epoch_watermark: None,
            epoch_extensions: std::collections::BTreeMap::new(),
        }
    }
}

impl Default for ExecuteOpts {
    fn default() -> Self {
        Self::legacy()
    }
}

/// The outcome of a completed [`run`].
#[derive(Debug, Clone)]
pub struct Executed {
    /// Executed plans in execution order.
    pub plans: Vec<ExecutionPlan>,
    /// The stamped ledger events the loop appended, in append order.
    pub events: Vec<Value>,
    /// The blake3 journal: one `Admitted` transition per executed plan.
    pub journal: TransitionJournal,
    /// Orders still blocked when the frontier emptied, with typed reasons.
    pub blocked: Vec<BlockedEntry>,
    /// Projected chain-tip standings after the final frontier pass.
    pub standings: Standings,
    /// Number of passes that executed at least one order.
    pub iterations: usize,
}

/// The typed promotion law: this loop may only write
/// `UNKNOWN -> PARTIAL_ALIVE`. Any other promotion — ALIVE
/// (external-fabric-only per the igniter's honest-ceiling law), a skip,
/// a repeat, or a regressed standing — is a typed refusal.
pub fn check_promotion(from: &str, to: &str) -> Result<(), ExecRefusal> {
    if from == EXECUTION_FROM && to == EXECUTION_CEILING {
        return Ok(());
    }
    Err(ExecRefusal::new(
        "reconcile",
        "standing_ceiling",
        "R_missing_standing",
        format!(
            "promotion {from} -> {to} refused: the local execution ceiling is \
             {EXECUTION_CEILING} (ALIVE is external-fabric-only); only \
             {EXECUTION_FROM} -> {EXECUTION_CEILING} is writable by this loop"
        ),
    ))
}

/// Executes the frontier -> authority -> plan -> receipt -> reconcile chain
/// to fixed point over the REAL ledger at `ledger_path`.
///
/// `orders` are the raw [`FrontierInput`]s (admitted inside [`frontier`]).
/// `authority_graph` is the typed authority source (authority.rs); its
/// pinned index is the ONE origin law every candidate resolves through.
pub fn run(
    orders: &[FrontierInput], ledger_path: &Path, authority_graph: &AuthorityGraph,
    opts: &ExecuteOpts,
) -> Result<Executed, ExecRefusal> {
    let typed_index = crate::authority::AuthorityIndex::build_pinned(authority_graph);
    // The map-membership index frontier() consumes (iri -> admission digest),
    // projected from the SAME pinned typed index — one origin law.
    let mut map_index = BTreeMap::new();
    for (iri, a) in typed_index.admitted.iter() {
        map_index.insert(iri.clone(), a.admission_digest.clone());
    }

    let mut plans: Vec<ExecutionPlan> = Vec::new();
    let mut events: Vec<Value> = Vec::new();
    let mut journal = TransitionJournal::new();

    for iteration in 1..=opts.max_iterations {
        // ── frontier: read the REAL ledger (never written at this hop) ────
        let ledger_events = ledger::fetch(ledger_path).map_err(|e| {
            ExecRefusal::new(
                "frontier",
                "ledger_refused",
                "R_missing_replay",
                format!("the standing ledger refused before anything ran: {e}"),
            )
        })?;

        let mut typed_events: Vec<TransitionEvent> = Vec::new();
        for e in &ledger_events {
            match serde_json::from_value::<TransitionEvent>(e.clone()) {
                Ok(ev) => typed_events.push(ev),
                // A transition-shaped event that does not decode must not be
                // silently dropped; a non-transition event (e.g. an external
                // annotation) is out of this loop's scope and is skipped.
                Err(_) if is_transition_shape(e) => {
                    return Err(ExecRefusal::new(
                        "frontier",
                        "ledger_refused",
                        "R_missing_replay",
                        "malformed transition in the ledger: a transition-shaped \
                         event does not decode into the typed TransitionEvent"
                            .to_string(),
                    ));
                }
                Err(_) => continue,
            }
        }

        let selected = frontier(orders, &typed_events, &BTreeMap::new(), Some(&map_index));

        if selected.eligible.is_empty() {
            // minimal unblock (SLA, E80-03 lane): compute the count before
            // `plans` is moved into the result struct.
            let iterations = plans.len();
            return Ok(Executed {
                plans,
                events,
                journal,
                blocked: selected.blocked,
                standings: selected.standings,
                iterations,
            });
        }

        // ── authority + plan + receipt + reconcile, per eligible candidate ─
        let mut progressed = false;
        for candidate in &selected.eligible {
            // The typed origin law, per candidate (frontier's map gate has
            // already passed; resolve re-derives through authority.rs).
            if let Err(r) = typed_index.resolve(&candidate.origin_authority) {
                return Err(ExecRefusal::new(
                    "authority",
                    "authority_refused",
                    "R_missing_authority",
                    format!(
                        "eligible candidate {} failed the typed origin law: {r}",
                        candidate.identity
                    ),
                ));
            }

            let raw = orders
                .iter()
                .find(|o| raw_identity(o) == Some(candidate.identity.clone()))
                .map(|o| o.work_order.clone())
                .ok_or_else(|| {
                    ExecRefusal::new(
                        "frontier",
                        "unknown_identity",
                        "R_missing_identity",
                        format!(
                            "eligible candidate {} has no raw work order in the graph",
                            candidate.identity
                        ),
                    )
                })?;

            // ── lease gate (DS-10): BEFORE re-admission and plan emission;
            // a refusal here leaves the ledger byte-unchanged. It precedes
            // re-admission because the epoch extension fields (`epoch`,
            // `plan_touches`, `manufacture_plan`, `source_artifacts`) ride
            // OUTSIDE the strict WorkOrder schema (deny_unknown_fields) —
            // the gate judges them before the strict envelope is decoded.
            // Honest scoping: this kernel has NO write authority (execute
            // caps at PARTIAL_ALIVE, the Go-side execution is the
            // consumer's), so the gate admits the PLAN+RECEIPT emission —
            // there is no disk write to gate.
            check_lease_gate(&candidate.identity, opts)?;

            let admitted = admit_work_order(&raw).map_err(|r| {
                ExecRefusal::new(
                    "frontier",
                    "admission_refused",
                    "R_missing_identity",
                    format!(
                        "eligible candidate {} failed re-admission: {r}",
                        candidate.identity
                    ),
                )
            })?;

            let plan = ExecutionPlan {
                identity: candidate.identity.clone(),
                repository: candidate.repository.clone(),
                base_sha: admitted.work_order.base_sha.clone(),
                projections: admitted.work_order.projections.clone(),
                required_courts: admitted.work_order.required_courts.clone(),
                path_scope: candidate.path_scope.clone(),
                concurrency_key: candidate.concurrency_key.clone(),
                authority: "NONE".to_string(),
                target_standing: EXECUTION_CEILING.to_string(),
                // The honest ceiling, structurally encoded.
                head_verified: false,
                plan_digest: plan_digest(candidate, &admitted),
            };

            // The typed promotion law, checked BEFORE any write.
            let from = selected
                .standings
                .get(&candidate.identity)
                .cloned()
                .unwrap_or_else(|| EXECUTION_FROM.to_string());
            check_promotion(&from, EXECUTION_CEILING)?;

            // ── receipt: journal the execution (blake3 chain) ─────────────
            journal.append(
                admitted.work_order_digest.clone(),
                Decision::Admitted {
                    authority: "NONE".to_string(),
                },
                Some(plan.plan_digest.clone()),
            );

            // ── reconcile: the append is the LAST write ───────────────────
            let event = event_value(&plan);
            let appended =
                ledger::append(ledger_path, &event, &AppendOpts::default()).map_err(|e| {
                    ExecRefusal::new(
                        "reconcile",
                        "ledger_refused",
                        "R_missing_replay",
                        format!("the ledger refused the transition append: {e}"),
                    )
                })?;

            // Idempotent replay: an event whose digest is already recorded
            // is not a second execution.
            if appended.status == AppendStatus::Appended {
                plans.push(plan);
                events.push(appended.event);
                progressed = true;
            }
        }

        if !progressed {
            // Every eligible candidate replayed to AlreadyRecorded: the
            // frontier cannot empty — refuse instead of spinning.
            return Err(exhausted(opts.max_iterations, iteration));
        }
    }

    Err(exhausted(opts.max_iterations, opts.max_iterations))
}

/// The DS-10 lease gate: binds the run's optional admitted lease and epoch
/// watermark to ONE order's plan emission. Called per eligible candidate,
/// BEFORE the plan is built and BEFORE the journal/append — any refusal
/// leaves the ledger byte-unchanged (the append is still the last write).
///
/// * lease law: when `opts.lease` is `Some`, it must carry
///   `admitted == true` (a non-admitted lease is not authority) and its
///   `subject_iri` must resolve to the order identity (the final `/`
///   segment equals the identity, e.g. `sj:WorkOrder/OA` binds `OA`).
/// * epoch law: delegated to [`crate::epoch::check`] with the run's
///   watermark; an epoch-carrying order with no watermark refuses — fail
///   closed, same as the admit-side gate.
///
/// With neither field set this function is a no-op: the legacy behavior is
/// byte-identical.
fn check_lease_gate(identity: &str, opts: &ExecuteOpts) -> Result<(), LeaseGateRefusal> {
    if let Some(lease) = &opts.lease {
        let binds = lease
            .subject_iri
            .rsplit('/')
            .next()
            .map(|seg| seg == identity)
            .unwrap_or(false);
        if !lease.admitted || !binds {
            return Err(LeaseGateRefusal::LeaseNotBound(format!(
                "lease {} does not bind order {}: admitted={} subject_iri={} — \
                 only an admitted lease whose subject resolves to the order \
                 identity admits the plan emission",
                lease.lease_id, identity, lease.admitted, lease.subject_iri
            )));
        }
    }

    let epoch_ext = opts.epoch_extensions.get(identity);
    let epoch_carried = epoch_ext
        .and_then(|ext| ext.get("epoch"))
        .and_then(|v| v.as_str())
        .map(|s| !s.is_empty())
        .unwrap_or(false);
    if epoch_carried {
        crate::epoch::check(
            epoch_ext.expect("checked above"),
            opts.epoch_watermark.as_ref(),
        )
        .map_err(|e| {
            LeaseGateRefusal::EpochStale(format!(
                "order {} failed the epoch gate: {e} — the plan carries \
                 pre-watermark implementation into a post-watermark epoch",
                identity
            ))
        })?;
    }
    Ok(())
}

fn exhausted(max: usize, at: usize) -> ExecRefusal {
    ExecRefusal::new(
        "frontier",
        "exhausted",
        "mu_on_O",
        format!(
            "the frontier did not empty within the iteration budget (pass {at} of \
             {max}) — remediation: raise max_iterations or split the work-order \
             graph"
        ),
    )
}

/// The transition event one executed plan writes to the ledger:
/// `UNKNOWN -> PARTIAL_ALIVE` with the plan digest as receipt evidence.
fn event_value(plan: &ExecutionPlan) -> Map<String, Value> {
    let mut m = Map::new();
    m.insert(
        "work_order_id".to_string(),
        Value::String(plan.identity.clone()),
    );
    m.insert(
        "from_standing".to_string(),
        Value::String(EXECUTION_FROM.to_string()),
    );
    m.insert(
        "to_standing".to_string(),
        Value::String(EXECUTION_CEILING.to_string()),
    );
    m.insert(
        "receipt_digest".to_string(),
        Value::String(plan.plan_digest.clone()),
    );
    m
}

fn plan_digest(candidate: &EligibleCandidate, admitted: &AdmittedWorkOrder) -> String {
    let mut m = Map::new();
    m.insert(
        "base_sha".to_string(),
        Value::String(admitted.work_order.base_sha.clone()),
    );
    m.insert(
        "concurrency_key".to_string(),
        Value::String(candidate.concurrency_key.clone()),
    );
    m.insert(
        "identity".to_string(),
        Value::String(candidate.identity.clone()),
    );
    m.insert(
        "origin_admission_digest".to_string(),
        Value::String(candidate.origin_admission_digest.clone()),
    );
    m.insert("path_scope".to_string(), json_vec(&candidate.path_scope));
    m.insert(
        "projections".to_string(),
        json_vec(&admitted.work_order.projections),
    );
    m.insert(
        "required_courts".to_string(),
        json_vec(&admitted.work_order.required_courts),
    );
    m.insert(
        "repository".to_string(),
        Value::String(candidate.repository.clone()),
    );
    m.insert(
        "target_standing".to_string(),
        Value::String(EXECUTION_CEILING.to_string()),
    );
    m.insert(
        "work_order_digest".to_string(),
        Value::String(candidate.work_order_digest.clone()),
    );
    digest_of(&Value::Object(m))
}

fn json_vec(v: &[String]) -> Value {
    Value::Array(v.iter().map(|s| Value::String(s.to_string())).collect())
}

fn raw_identity(input: &FrontierInput) -> Option<String> {
    input
        .work_order
        .get("identity")
        .and_then(|v| v.as_str())
        .map(String::from)
}

fn is_transition_shape(event: &Value) -> bool {
    event.get("work_order_id").is_some() && event.get("to_standing").is_some()
}
