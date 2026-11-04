//! XaaS execution descriptor — port of `GgenIgniter.SemanticJira.Descriptor`
//! (descriptor.ex, 792L) restricted to the roadmap Slice-2 surface:
//! `schema/0` → [`SCHEMA`], `build/4` → [`build`], `identity/1` →
//! [`identity`], `build_xaas_contract/4` → [`build_xaas_contract`],
//! `receipt_from_xaas/3` → [`receipt_from_xaas`], `receipt_digest/1` →
//! [`receipt_digest`].
//!
//! Deferred with typed rationale (the roadmap table's re-scopes):
//! - `render/1`: the Elixir renders through the pack's EEx template
//!   (`runtime-templates/descriptor.json.eex`); Rust has no EEx and the
//!   template is the Elixir-side surface. Callers consume the descriptor
//!   as a JSON [`Value`] — emitting `serde_json` bytes would be a parallel
//!   rendering identity, not a port.
//! - `to_a2a_task/3`: depends on `SemanticA2A.task_from_work_order/2`,
//!   unported — [`DescriptorRefusal::IdentityMismatch`] is declared for it
//!   (the deferred `same/3` check) and the variant carries no behavior.
//! - `execution_package/4` (kernel, line 427) is unported, so `build`'s
//!   `package` key is caller-supplied via [`DescriptorOpts::package`];
//!   when absent the descriptor omits `package` — never invented here.
//! - the igniter's `dependencies` field and the court-map `dependencies`
//!   walk are vacuous in this port: the core fifteen-field `WorkOrder`
//!   carries no `dependencies` field (deny_unknown_fields), so the
//!   dependency list is always empty and `DependencyNotAlive` /
//!   `UnsupportedRequiredStanding` stay declared-but-unreachable until the
//!   field lands.
//!
//! Graph identity carried unchanged, ggen → XaaS: `definition_digest`,
//! `snapshot_digest` (the admitted `work_order_digest`), `base_sha`,
//! `graph_digest`. Authority is always `NONE`; a descriptor grants no
//! lease and never promotes standing.
//!
//! `#![forbid(unsafe_code)]` crate-wide. IO-free; typed refusals fail
//! closed; no unwrap on external input.

use serde_json::{Map, Value};
use std::collections::BTreeMap;

use crate::admit::{self, AdmittedWorkOrder, Refusal};
use crate::frontier::{
    frontier, AuthorityIndex, Evidence, FrontierInput, FrontierResult, TransitionEvent,
};
use crate::work_order::digest_of;

/// Schema id of descriptors produced here (`@schema`, line 12).
pub const SCHEMA: &str = "semantic-jira/execution-descriptor/v1";

/// Chain tail for an empty log (`@genesis`, reconciler.ex).
pub const GENESIS_TAIL: &str =
    "sha256:0000000000000000000000000000000000000000000000000000000000000000";

/// The descriptor's graph-identity keys (`@identity_keys`, line 13).
pub const IDENTITY_KEYS: [&str; 4] = [
    "definition_digest",
    "snapshot_digest",
    "graph_digest",
    "base_sha",
];

/// Default provider: the deterministic XaaS RecipeWorker (PRD PR-009 — an
/// absent provider never falls back to an LLM; `@default_provider`).
pub const DEFAULT_PROVIDER: &str = "recipe";

/// Fabric evidence ceiling default (`@fabric_ceiling`).
const FABRIC_CEILING: &str = "repository-local";

/// Receipt-class vocabulary (`@receipt_classes`, semantic_jira.ex line 22).
pub const RECEIPT_CLASSES: [&str; 9] = [
    "manufacture",
    "projection",
    "authority_preparation",
    "court_results",
    "acceptance_results",
    "falsifier_results",
    "verification",
    "postcondition",
    "replay",
];

/// Bridge keys a XaaS exporter must place on the executed receipt's
/// `bridge` (`@bridge_keys`, line 263).
const BRIDGE_KEYS: [&str; 10] = [
    "identity",
    "definition_digest",
    "source_snapshot_digest",
    "ledger_tail",
    "repository",
    "base_sha",
    "subject",
    "evidence_ceiling",
    "replay_identity",
    "requires",
];

/// Typed descriptor refusal (`{:error, {:refused_descriptor, reason}}`).
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum DescriptorRefusal {
    #[error("not on frontier: {0}")]
    NotOnFrontier(String),
    #[error("not found: no order with this identity in the projected set")]
    NotFound,
    #[error(
        "snapshot drift: the eligible candidate's snapshot digest does not match the \
         admitted order's — the frontier projected a different snapshot than the \
         payload carries"
    )]
    SnapshotDrift,
    #[error("unadmitted: {0}")]
    Unadmitted(Refusal),
    #[error("invalid option {0:?}")]
    InvalidOption(&'static str),
    #[error("missing option {0:?}")]
    MissingOption(&'static str),
    #[error("missing execution-repo alias for repository {0:?}")]
    MissingAlias(String),
    #[error(
        "unsupported required standing {required:?} on upstream {upstream:?} — only \
         ALIVE dependency evidence is supported"
    )]
    UnsupportedRequiredStanding { upstream: String, required: String },
    #[error("dependency {upstream:?} has no ALIVE evidence")]
    DependencyNotAlive { upstream: String },
    #[error("court map refused: {0}")]
    CourtMapRefused(CourtMapRefusal),
    #[error("identity mismatch on {0:?} (to_a2a_task's same/3 — deferred with to_a2a_task)")]
    IdentityMismatch(&'static str),
}

/// Typed receipt refusal (`{:error, {:receipt_refused, reason}}`) — the
/// ten-reason matrix of `receipt_from_xaas/3`.
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum ReceiptRefusal {
    #[error("not a map")]
    NotAMap,
    #[error("bridge invalid: missing keys {0:?}")]
    BridgeInvalid(Vec<String>),
    #[error("receipt_digest missing or malformed (sha256:<64 hex>)")]
    InvalidReceiptDigest,
    #[error("receipt_digest does not recompute over the receipt minus its digest key")]
    ReceiptDigestMismatch,
    #[error("receipt's bridge does not equal the descriptor's bridge")]
    BridgeMismatch,
    #[error("final_head is not a 40-hex sha")]
    InvalidFinalHead,
    #[error("unsupported outcome {0:?}")]
    UnsupportedOutcome(String),
    #[error("head_verified is not a boolean")]
    HeadVerifiedNotBoolean,
    #[error("ALIVE outcome without head_verified=true")]
    AliveWithoutHeadVerification,
    #[error("ALIVE outcome without fabric_verifier.status == pass")]
    AliveWithoutVerifierPass,
}

/// The optional court map's typed refusal family
/// (`{:court_map_refused, _}`).
#[derive(Debug, Clone, PartialEq, Eq, thiserror::Error)]
pub enum CourtMapRefusal {
    #[error("not a map")]
    NotAMap,
    #[error("unknown keys {0:?}")]
    UnknownKeys(Vec<String>),
    #[error("court map present but empty")]
    Empty,
    #[error("foreign IRI in group {group:?}: {iri:?}")]
    ForeignIri { group: String, iri: String },
    #[error("invalid predicate in group {group:?} for {iri:?}")]
    InvalidPredicate { group: String, iri: String },
    #[error("invalid group {0:?}")]
    InvalidGroup(String),
}

/// `build/4`'s attrs: the four required identities plus the optional
/// provider.
pub struct BuildAttrs {
    pub graph_digest: String,
    pub source_digest: String,
    pub worker_identity: String,
    pub verifier_identity: String,
    /// Defaults to [`DEFAULT_PROVIDER`] when `None`.
    pub provider: Option<String>,
}

/// The descriptor surface's options. Lifetime carries the authority index.
#[derive(Default)]
pub struct DescriptorOpts<'a> {
    /// Origin-authority pins (`IRI -> admission digest`); `None` blocks
    /// every order (frontier fails closed).
    pub authority: Option<&'a AuthorityIndex>,
    /// Caller evidence per identity, overridden by ledger evidence.
    pub evidence: BTreeMap<String, Evidence>,
    /// Caller-supplied execution package (execution_package/4 is unported —
    /// disclosed divergence; absent ⇒ the descriptor omits `package`).
    pub package: Option<Value>,
    /// XaaS construction provider (`build_xaas_contract`).
    pub provider: Option<String>,
    /// Registered XaaS verifier suite (required by `build_xaas_contract`).
    pub verifier_suite: Option<String>,
    /// Explicit execution-repo alias.
    pub execution_repo_alias: Option<String>,
    /// repository -> alias map consulted when no explicit alias is given.
    pub aliases: BTreeMap<String, String>,
    /// IRI prefix for work-order IRIs (`@iri_prefix`).
    pub iri_prefix: Option<String>,
    /// The optional court map (raw JSON), validated per `court_map/2`.
    pub court_map: Option<Value>,
}


/// The graph-identity values carried through every projection
/// (`identity/1`, line 102).
pub fn identity(descriptor: &Value) -> Value {
    let mut m = Map::new();
    if let Some(obj) = descriptor.as_object() {
        for k in IDENTITY_KEYS {
            if let Some(v) = obj.get(k) {
                m.insert(k.to_string(), v.clone());
            }
        }
    }
    Value::Object(m)
}

/// Chain tail: the last event's `event_digest`, or the genesis digest for
/// an empty log (reconciler.ex `tail_digest/1`, inlined per the roadmap —
/// Slice 3's full reconcile is a later slice).
pub fn tail_digest(events: &[Value]) -> String {
    events
        .last()
        .and_then(|e| e.get("event_digest"))
        .and_then(|d| d.as_str())
        .map(|s| s.to_string())
        .unwrap_or_else(|| GENESIS_TAIL.to_string())
}

/// Local projection: fold the transition log onto the raw orders — each
/// order's `standing` becomes the latest matching event's `to_standing`
/// (last wins; the log order is the fold order). Port of
/// `Reconciler.project/2`'s standing fold for this slice's needs.
fn projected_orders(orders: &[Value], events: &[TransitionEvent]) -> Vec<Value> {
    let mut tip: BTreeMap<String, &TransitionEvent> = BTreeMap::new();
    for e in events {
        if let Some(id) = tip.get(&e.work_order_id) {
            if id.seq > e.seq {
                continue;
            }
        }
        tip.insert(e.work_order_id.clone(), e);
    }
    orders
        .iter()
        .map(|o| {
            let mut o = o.clone();
            if let Some(id) = o.get("identity").and_then(|v| v.as_str()) {
                if let Some(e) = tip.get(id) {
                    o["standing"] = Value::String(e.to_standing.clone());
                }
            }
            o
        })
        .collect()
}

/// Options-matching law: a binary matching the suite-name pattern
/// (`@suite_re`, `^[a-z0-9][a-z0-9_-]{0,63}$`; Rust `$` anchors at the
/// string end, matching the Elixir dollar-endonly intent).
fn valid_suite(s: &str) -> bool {
    let mut cs = s.chars();
    match cs.next() {
        Some(c) if c.is_ascii_digit() || c.is_ascii_lowercase() => {}
        _ => return false,
    }
    s.len() <= 64
        && s.chars()
            .skip(1)
            .all(|c| c.is_ascii_digit() || c.is_ascii_lowercase() || c == '_' || c == '-')
}

/// `^[A-Za-z0-9_.-]{1,128}$` (`@alias_re`).
fn valid_alias(s: &str) -> bool {
    !s.is_empty()
        && s.len() <= 128
        && s.chars()
            .all(|c| c.is_ascii_alphanumeric() || matches!(c, '.' | '_' | '-'))
}

/// Eligibility over the frontier result: eligible-by-identity wins, a
/// blocked entry carries its typed reason, neither is `NotFound`
/// (`eligible/2` + `eligible/4`).
fn eligible(front: &FrontierResult, id: &str) -> Result<(bool, String), DescriptorRefusal> {
    if front.eligible.iter().any(|c| c.identity == id) {
        return Ok((true, String::new()));
    }
    if let Some(b) = front
        .blocked
        .iter()
        .find(|b| b.identity.as_deref() == Some(id))
    {
        return Err(DescriptorRefusal::NotOnFrontier(b.reason.clone()));
    }
    Err(DescriptorRefusal::NotFound)
}

/// Court-map validation (`court_map/2` + helpers, lines 396-470).
fn validate_court_map(raw: &Value, order: &Value) -> Result<(), DescriptorRefusal> {
    let Some(obj) = raw.as_object() else {
        return Err(DescriptorRefusal::CourtMapRefused(CourtMapRefusal::NotAMap));
    };
    const KEYS: [&str; 3] = ["acceptance", "falsifiers", "courts"];
    let unknown: Vec<String> = obj
        .keys()
        .filter(|k| !KEYS.contains(&k.as_str()))
        .cloned()
        .collect();
    if !unknown.is_empty() {
        return Err(DescriptorRefusal::CourtMapRefused(
            CourtMapRefusal::UnknownKeys(unknown),
        ));
    }
    let acceptance = obj.get("acceptance");
    let falsifiers = obj.get("falsifiers");
    let courts = obj.get("courts");
    let empty_acceptance =
        acceptance.is_none_or(|v| v.as_object().is_none_or(|m| m.is_empty()));
    let empty_falsifiers =
        falsifiers.is_none_or(|v| v.as_object().is_none_or(|m| m.is_empty()));
    let empty_courts = courts.is_none_or(|v| v.as_array().is_none_or(|a| a.is_empty()));
    if empty_acceptance && empty_falsifiers && empty_courts {
        return Err(DescriptorRefusal::CourtMapRefused(CourtMapRefusal::Empty));
    }
    // groups: each value is {iri: {"test": id}} with iri in the order's
    // own list; courts is a list of court IRIs within required_courts.
    for (group, allowed) in [
        ("acceptance", obj.get("acceptance")),
        ("falsifiers", obj.get("falsifiers")),
    ] {
        if let Some(g) = group_val(allowed) {
            let order_list = order_list_of(order, group);
            for (iri, predicate) in g {
                let Some(test_id) = predicate
                    .as_object()
                    .and_then(|m| m.get("test"))
                    .and_then(|t| t.as_str())
                    .filter(|t| !t.is_empty())
                else {
                    return Err(DescriptorRefusal::CourtMapRefused(
                        CourtMapRefusal::InvalidPredicate {
                            group: group.to_string(),
                            iri: iri.clone(),
                        },
                    ));
                };
                let _ = test_id;
                if !order_list.contains(iri) {
                    return Err(DescriptorRefusal::CourtMapRefused(
                        CourtMapRefusal::ForeignIri {
                            group: group.to_string(),
                            iri: iri.clone(),
                        },
                    ));
                }
            }
        } else if !allowed.is_none_or(|v| v.is_null()) && !allowed.is_none_or(Value::is_object)
        {
            return Err(DescriptorRefusal::CourtMapRefused(
                CourtMapRefusal::InvalidGroup(group.to_string()),
            ));
        }
    }
    if let Some(c) = courts {
        match c {
            Value::Null => {}
            Value::Array(list) => {
                let allowed = order_list_of(order, "courts");
                for iri in list {
                    let Some(s) = iri.as_str() else {
                        return Err(DescriptorRefusal::CourtMapRefused(
                            CourtMapRefusal::InvalidGroup("courts".to_string()),
                        ));
                    };
                    if !allowed.iter().any(|a| a == s) {
                        return Err(DescriptorRefusal::CourtMapRefused(
                            CourtMapRefusal::ForeignIri {
                                group: "courts".to_string(),
                                iri: s.to_string(),
                            },
                        ));
                    }
                }
            }
            _ => {
                return Err(DescriptorRefusal::CourtMapRefused(
                    CourtMapRefusal::InvalidGroup("courts".to_string()),
                ));
            }
        }
    }
    Ok(())
}

fn group_val(v: Option<&Value>) -> Option<&serde_json::Map<String, Value>> {
    v.and_then(|v| v.as_object())
}

fn order_list_of(order: &Value, group: &str) -> Vec<String> {
    let key = match group {
        "courts" => "required_courts",
        other => other,
    };
    order
        .get(key)
        .and_then(|v| v.as_array())
        .map(|a| {
            a.iter()
                .filter_map(|v| v.as_str())
                .map(|s| s.to_string())
                .collect()
        })
        .unwrap_or_default()
}

/// Assembles the descriptor map for an admitted order (the all-gates-pass
/// arm of `build/4` and the contract body of `build_xaas_contract/4`).
fn descriptor_map(
    admitted: &AdmittedWorkOrder, attrs: &BuildAttrs, package: Option<&Value>,
) -> Value {
    let mut m = Map::new();
    m.insert("schema".into(), Value::String(SCHEMA.into()));
    m.insert(
        "work_order_id".into(),
        Value::String(admitted.work_order.identity.clone()),
    );
    m.insert(
        "subject".into(),
        Value::String(admitted.work_order.subject.clone()),
    );
    m.insert(
        "repository".into(),
        Value::String(admitted.work_order.repository.clone()),
    );
    m.insert(
        "base_sha".into(),
        Value::String(admitted.work_order.base_sha.clone()),
    );
    m.insert(
        "definition_digest".into(),
        Value::String(admitted.definition_digest.clone()),
    );
    m.insert(
        "snapshot_digest".into(),
        Value::String(admitted.work_order_digest.clone()),
    );
    m.insert(
        "graph_digest".into(),
        Value::String(attrs.graph_digest.clone()),
    );
    m.insert(
        "source_digest".into(),
        Value::String(attrs.source_digest.clone()),
    );
    m.insert(
        "provider".into(),
        Value::String(
            attrs
                .provider
                .clone()
                .unwrap_or_else(|| DEFAULT_PROVIDER.into()),
        ),
    );
    m.insert(
        "worker_identity".into(),
        Value::String(attrs.worker_identity.clone()),
    );
    m.insert(
        "verifier_identity".into(),
        Value::String(attrs.verifier_identity.clone()),
    );
    m.insert(
        "replay_identity".into(),
        Value::String(admitted.work_order.replay_identity.clone()),
    );
    m.insert("authority".into(), Value::String("NONE".into()));
    if let Some(p) = package {
        m.insert("package".into(), p.clone());
    }
    Value::Object(m)
}

/// Build the descriptor for `identity` from `work_orders` — port of
/// `build/4` (lines 52-100), scoped to the no-events path (the events path
/// is [`build_xaas_contract`]'s surface).
///
/// Order of laws: provider name, eligibility (frontier), presence,
/// admission, snapshot binding, definition digest. Every outcome fails
/// closed with a typed refusal; `package` is caller-supplied
/// (execution_package/4 unported — see moduledoc).
pub fn build(
    work_orders: &[Value], identity: &str, attrs: &BuildAttrs, opts: &DescriptorOpts<'_>,
) -> Result<Value, DescriptorRefusal> {
    let provider = attrs
        .provider
        .clone()
        .unwrap_or_else(|| DEFAULT_PROVIDER.to_string());
    if !valid_suite(&provider) {
        return Err(DescriptorRefusal::InvalidOption("provider"));
    }

    let inputs: Vec<FrontierInput> = work_orders.iter().map(frontier_input_of).collect();
    let front = frontier(&inputs, &[], &opts.evidence, opts.authority);

    let (eligible_flag, _) = eligible(&front, identity)?;
    let _ = eligible_flag;
    let raw = find(work_orders, identity)?;
    let admitted = admit(raw)?;
    // Snapshot binding: the frontier's candidate digest must name the same
    // admitted snapshot (`same_snapshot/2`).
    let candidate = front
        .eligible
        .iter()
        .find(|c| c.identity == identity)
        .expect("eligible() proved presence");
    if candidate.work_order_digest != admitted.work_order_digest {
        return Err(DescriptorRefusal::SnapshotDrift);
    }

    let mut d = descriptor_map(&admitted, attrs, opts.package.as_ref());
    if let Some(cm) = &opts.court_map {
        validate_court_map(cm, raw)?;
        d.as_object_mut()
            .expect("descriptor is an object")
            .insert("court_map".into(), cm.clone());
    }
    Ok(d)
}

/// `find/2`: the raw order with this identity.
fn find<'a>(work_orders: &'a [Value], identity: &str) -> Result<&'a Value, DescriptorRefusal> {
    work_orders
        .iter()
        .find(|o| o.get("identity").and_then(|v| v.as_str()) == Some(identity))
        .ok_or(DescriptorRefusal::NotFound)
}

fn admit(row: &Value) -> Result<AdmittedWorkOrder, DescriptorRefusal> {
    crate::admit::admit_work_order(row).map_err(DescriptorRefusal::Unadmitted)
}

fn frontier_input_of(o: &Value) -> FrontierInput {
    FrontierInput {
        work_order: o.clone(),
        dependencies: Vec::new(),
        path_scope: Vec::new(),
        concurrency_key: o
            .get("identity")
            .and_then(|v| v.as_str())
            .unwrap_or("")
            .to_string(),
    }
}

/// Build the XaaS execution descriptor for `identity` — port of
/// `build_xaas_contract/4` (lines 257-305).
///
/// The eligible row is ADMITTED before it is projected, so the bridge's
/// `definition_digest` / `source_snapshot_digest` are the kernel's digests
/// and `graph_digest` is the admitted order's `work_order_digest` with
/// `digest_form` "sjira-digest/2" — XaaS's forced fail-closed `:snapshot`
/// admission binding holds.
pub fn build_xaas_contract(
    work_orders: &[Value], events: &[TransitionEvent], identity: &str, opts: &DescriptorOpts<'_>,
) -> Result<Value, DescriptorRefusal> {
    // project(work_orders, events): the standing fold over the raw orders.
    let projected = projected_orders(work_orders, events);

    // Eligibility over the projected set, via the landed frontier.
    let inputs: Vec<FrontierInput> = projected.iter().map(frontier_input_of).collect();
    let front = frontier(&inputs, events, &opts.evidence, opts.authority);
    eligible(&front, identity)?;

    // The admitted row IS the projected raw order.
    let row = find(&projected, identity)?;
    let admitted = admit(row)?;
    let provider = opts
        .provider
        .clone()
        .unwrap_or_else(|| DEFAULT_PROVIDER.to_string());
    if !valid_suite(&provider) {
        return Err(DescriptorRefusal::InvalidOption("provider"));
    }
    let suite = opts
        .verifier_suite
        .as_deref()
        .ok_or(DescriptorRefusal::MissingOption("verifier_suite"))?;
    if !valid_suite(suite) {
        return Err(DescriptorRefusal::InvalidOption("verifier_suite"));
    }
    let exec_alias = opts
        .execution_repo_alias
        .as_deref()
        .or_else(|| {
            opts.aliases
                .get(&admitted.work_order.repository)
                .map(|s| s.as_str())
        })
        .ok_or_else(|| DescriptorRefusal::MissingAlias(admitted.work_order.repository.clone()))?;
    if !valid_alias(exec_alias) {
        return Err(DescriptorRefusal::InvalidOption("execution_repo_alias"));
    }
    if let Some(cm) = &opts.court_map {
        validate_court_map(cm, row)?;
    }

    // tail_digest over the raw ledger events is the caller's concern in
    // this slice: events here are frontier TransitionEvents; the bridge's
    // ledger_tail is the genesis digest unless the caller supplies raw
    // events via opts. Declared: the raw-event tail rides `ledger_tail`.
    let prefix = opts
        .iri_prefix
        .clone()
        .unwrap_or_else(|| "urn:semantic-jira:work-order:".to_string());

    let mut m = Map::new();
    m.insert(
        "work_order_iri".into(),
        Value::String(format!("{}{}", prefix, identity)),
    );
    m.insert(
        "checkpoint_iri".into(),
        Value::String(format!("urn:semantic-jira:checkpoint:{}", GENESIS_TAIL)),
    );
    m.insert(
        "graph_digest".into(),
        Value::String(admitted.work_order_digest.clone()),
    );
    m.insert("digest_form".into(), Value::String("sjira-digest/2".into()));
    m.insert("admitted_work_order".into(), row.clone());
    m.insert(
        "repository_identity".into(),
        Value::String(admitted.work_order.repository.clone()),
    );
    m.insert(
        "execution_repo_alias".into(),
        Value::String(exec_alias.to_string()),
    );
    m.insert(
        "base_sha".into(),
        Value::String(admitted.work_order.base_sha.clone()),
    );
    m.insert("goal".into(), goal(&admitted.work_order));
    m.insert("provider".into(), Value::String(provider));
    m.insert("verifier_suite".into(), Value::String(suite.to_string()));
    m.insert(
        "execution_policy".into(),
        Value::String("autonomic_wave_attempt".into()),
    );
    // dependencies: the core WorkOrder carries no dependencies field
    // (disclosed divergence) — the list is present and empty.
    m.insert("dependencies".into(), Value::Array(Vec::new()));
    m.insert("bridge".into(), bridge(&admitted, GENESIS_TAIL));
    if let Some(cm) = &opts.court_map {
        m.insert("court_map".into(), cm.clone());
    }
    Ok(Value::Object(m))
}

/// The goal text (`goal/1`, line 524): identity/title, description,
/// acceptance, falsifiers, path scope. The core WorkOrder carries no
/// `path_scope` field, so the scope block is always absent here.
fn goal(order: &crate::work_order::WorkOrder) -> Value {
    let mut lines: Vec<String> = Vec::new();
    lines.push(format!("{}: {}", order.identity, order.title));
    lines.push(String::new());
    lines.push(order.description.clone());
    lines.push(String::new());
    lines.push("Acceptance:".to_string());
    for a in &order.acceptance {
        lines.push(format!("- {}", a));
    }
    lines.push(String::new());
    lines.push("Falsifiers:".to_string());
    for f in &order.falsifiers {
        lines.push(format!("- {}", f));
    }
    Value::String(lines.join("\n"))
}

/// The bridge (`bridge/2`, line 540): exactly the snapshot's identity keys
/// plus the requires block (the snapshot's three lists — the
/// AdmissionBinding law).
fn bridge(admitted: &AdmittedWorkOrder, tail: &str) -> Value {
    let mut req = Map::new();
    req.insert(
        "courts".into(),
        Value::Array(
            admitted
                .work_order
                .required_courts
                .iter()
                .map(|s| Value::String(s.clone()))
                .collect(),
        ),
    );
    req.insert(
        "acceptance".into(),
        Value::Array(
            admitted
                .work_order
                .acceptance
                .iter()
                .map(|s| Value::String(s.clone()))
                .collect(),
        ),
    );
    req.insert(
        "falsifiers".into(),
        Value::Array(
            admitted
                .work_order
                .falsifiers
                .iter()
                .map(|s| Value::String(s.clone()))
                .collect(),
        ),
    );
    let mut m = Map::new();
    m.insert(
        "identity".into(),
        Value::String(admitted.work_order.identity.clone()),
    );
    m.insert(
        "definition_digest".into(),
        Value::String(admitted.definition_digest.clone()),
    );
    m.insert(
        "source_snapshot_digest".into(),
        Value::String(admitted.work_order_digest.clone()),
    );
    m.insert("ledger_tail".into(), Value::String(tail.to_string()));
    m.insert(
        "repository".into(),
        Value::String(admitted.work_order.repository.clone()),
    );
    m.insert(
        "base_sha".into(),
        Value::String(admitted.work_order.base_sha.clone()),
    );
    m.insert(
        "subject".into(),
        Value::String(admitted.work_order.subject.clone()),
    );
    m.insert(
        "evidence_ceiling".into(),
        Value::String(admitted.work_order.evidence_ceiling.clone()),
    );
    m.insert(
        "replay_identity".into(),
        Value::String(admitted.work_order.replay_identity.clone()),
    );
    m.insert("requires".into(), Value::Object(req));
    Value::Object(m)
}

/// The digest a XaaS exporter must place in `"receipt_digest"`
/// (`receipt_digest/1`, line 330): `digest_exact` over the receipt minus
/// its own digest key.
pub fn receipt_digest(receipt: &Value) -> String {
    let mut r = receipt.clone();
    if let Some(obj) = r.as_object_mut() {
        obj.remove("receipt_digest");
    }
    digest_of(&r)
}

/// Map one sealed XaaS receipt onto the receipt the (later-slice)
/// reconciler consumes — port of `receipt_from_xaas/3` (lines 305-330 +
/// 545-660). `required_evidence` is the contract's `required_evidence`
/// list (`None` ⇒ empty).
pub fn receipt_from_xaas(
    xaas_receipt: &Value, bridge: &Value, required_evidence: Option<&[String]>,
) -> Result<Value, ReceiptRefusal> {
    let Some(receipt) = xaas_receipt.as_object() else {
        return Err(ReceiptRefusal::NotAMap);
    };
    let Some(bridge) = bridge.as_object() else {
        return Err(ReceiptRefusal::NotAMap);
    };
    let required_evidence: Vec<String> = required_evidence.unwrap_or(&[]).to_vec();

    // bridge_shape/1 (line 564).
    let missing: Vec<String> = BRIDGE_KEYS
        .iter()
        .filter(|k| !bridge.contains_key(**k))
        .map(|k| k.to_string())
        .collect();
    if !missing.is_empty() {
        return Err(ReceiptRefusal::BridgeInvalid(missing));
    }
    let requires_ok = bridge
        .get("requires")
        .and_then(|r| r.as_object())
        .map(|r| {
            ["courts", "acceptance", "falsifiers"]
                .iter()
                .all(|k| r.get(*k).is_some_and(|v| v.is_array()))
        })
        .unwrap_or(false);
    if !requires_ok {
        return Err(ReceiptRefusal::BridgeInvalid(vec!["requires".to_string()]));
    }

    // digest_ok/1 (line 581).
    let digest = receipt
        .get("receipt_digest")
        .and_then(|d| d.as_str())
        .ok_or(ReceiptRefusal::InvalidReceiptDigest)?;
    if !admit::valid_digest(digest) {
        return Err(ReceiptRefusal::InvalidReceiptDigest);
    }
    let recomputed = receipt_digest(xaas_receipt);
    if recomputed != digest {
        return Err(ReceiptRefusal::ReceiptDigestMismatch);
    }

    // bridge equality (line 596).
    if receipt.get("bridge") != Some(&Value::Object(bridge.clone())) {
        return Err(ReceiptRefusal::BridgeMismatch);
    }

    // final_head sha (line 600).
    let final_head = receipt
        .get("final_head")
        .and_then(|h| h.as_str())
        .ok_or(ReceiptRefusal::InvalidFinalHead)?;
    if !admit::valid_sha(final_head) {
        return Err(ReceiptRefusal::InvalidFinalHead);
    }

    // target/1 (line 595).
    let outcome = receipt
        .get("outcome")
        .and_then(|o| o.as_str())
        .unwrap_or("");
    let target = match outcome {
        "alive" => "ALIVE",
        "partial_alive" => "PARTIAL_ALIVE",
        "build_broken" => "BUILD_BROKEN",
        "blocked" => "BLOCKED",
        other => return Err(ReceiptRefusal::UnsupportedOutcome(other.to_string())),
    };

    // head_verified_boolean/1 (line 602).
    let head_verified = receipt.get("head_verified").and_then(|v| v.as_bool());
    if head_verified.is_none() {
        return Err(ReceiptRefusal::HeadVerifiedNotBoolean);
    }
    let head_verified = head_verified.expect("checked Some above");

    // alive_guards/2 (line 605).
    let verifier_status = receipt
        .get("fabric_verifier")
        .and_then(|f| f.get("status"))
        .and_then(|s| s.as_str());
    if target == "ALIVE" {
        if !head_verified {
            return Err(ReceiptRefusal::AliveWithoutHeadVerification);
        }
        if verifier_status != Some("pass") {
            return Err(ReceiptRefusal::AliveWithoutVerifierPass);
        }
    }

    // reconciler_receipt/4 (line 613).
    let passed = verifier_status == Some("pass");
    let observed = passed && head_verified && outcome == "alive";
    let court = receipt
        .get("fabric_verifier")
        .and_then(|f| f.get("court_receipt"))
        .and_then(|c| c.as_object())
        .cloned()
        .unwrap_or_default();
    let requires = bridge
        .get("requires")
        .and_then(|r| r.as_object())
        .expect("requires_ok proved above");
    let required_courts: Vec<String> = str_list(requires.get("courts"));
    let acceptance: Vec<String> = str_list(requires.get("acceptance"));
    let falsifiers: Vec<String> = str_list(requires.get("falsifiers"));

    // court_results/2 (line 673).
    let steps = receipt
        .get("fabric_verifier")
        .and_then(|f| f.get("steps"))
        .and_then(|s| s.as_array())
        .cloned()
        .unwrap_or_default();
    let mut courts_map = Map::new();
    for court_name in &required_courts {
        let step_passed = steps.iter().any(|s| {
            s.get("id").and_then(|v| v.as_str()) == Some(court_name.as_str())
                && s.get("status").and_then(|v| v.as_str()) == Some("pass")
        });
        let witnessed = step_passed || {
            // witnessed_court?/4 (line 695): the sealed court receipt's
            // binding must resolve against THIS receipt's own steps + head.
            let sealed = court
                .get("court_results")
                .and_then(|c| c.get(court_name))
                .and_then(|c| c.as_object());
            match sealed {
                Some(entry)
                    if entry.get("passed").and_then(|v| v.as_bool()) == Some(true)
                        && entry.get("step_id").and_then(|v| v.as_str()).is_some() =>
                {
                    let head_ok = entry.get("head").and_then(|v| v.as_str()) == Some(final_head);
                    let step_ok = entry
                        .get("step_id")
                        .and_then(|v| v.as_str())
                        .map(|sid| {
                            steps.iter().any(|s| {
                                s.get("id").and_then(|v| v.as_str()) == Some(sid)
                                    && s.get("status").and_then(|v| v.as_str()) == Some("pass")
                            })
                        })
                        .unwrap_or(false);
                    head_ok && step_ok
                }
                _ => false,
            }
        };
        let mut e = Map::new();
        e.insert("passed".into(), Value::Bool(passed && witnessed));
        courts_map.insert(court_name.clone(), Value::Object(e));
    }

    // evidence_types/4 (line 706).
    let declared: Vec<String> = str_list(court.get("evidence_types"));
    let any_court_passed = courts_map
        .values()
        .any(|v| v.get("passed") == Some(&Value::Bool(true)));
    let witnessed_types: Vec<String> = if any_court_passed {
        required_evidence.clone()
    } else {
        Vec::new()
    };
    let mut derived: Vec<String> = Vec::new();
    if passed {
        derived.push("verification".to_string());
        if head_verified {
            derived.push("exact_head_verification".to_string());
        }
    }
    let mut types: Vec<String> = declared;
    types.extend(witnessed_types);
    types.extend(derived);
    types.sort();
    types.dedup();

    // receipt_classes/2 (line 724).
    let declared_classes: Vec<String> = str_list(court.get("receipt_classes"))
        .into_iter()
        .filter(|c| RECEIPT_CLASSES.contains(&c.as_str()))
        .collect();
    let mut classes = declared_classes;
    if passed {
        classes.push("verification".to_string());
    }
    classes.sort();
    classes.dedup();

    // acceptance_results/3 + falsifier_results/3 (lines 731-741).
    let observed_acceptance = court.get("acceptance_results").and_then(|v| v.as_object());
    let mut acceptance_results = Map::new();
    for name in &acceptance {
        acceptance_results.insert(
            name.clone(),
            Value::Bool(
                passed
                    && observed_acceptance
                        .and_then(|m| m.get(name))
                        .and_then(|v| v.as_bool())
                        == Some(true),
            ),
        );
    }
    let observed_falsifiers = court.get("falsifier_results").and_then(|v| v.as_object());
    let mut falsifier_results = Map::new();
    for f in &falsifiers {
        let observed_v = observed_falsifiers
            .and_then(|m| m.get(f))
            .and_then(|v| v.as_str());
        let verdict = if !passed {
            "unobserved"
        } else if observed_v == Some("survived") {
            "survived"
        } else if observed_v == Some("killed") {
            "killed"
        } else {
            "unobserved"
        };
        falsifier_results.insert(f.clone(), Value::String(verdict.to_string()));
    }

    let mut base = Map::new();
    base.insert("identity".into(), bridge["identity"].clone());
    base.insert(
        "definition_digest".into(),
        bridge["definition_digest"].clone(),
    );
    base.insert(
        "source_snapshot_digest".into(),
        bridge["source_snapshot_digest"].clone(),
    );
    base.insert("repository".into(), bridge["repository"].clone());
    base.insert("base_sha".into(), bridge["base_sha"].clone());
    base.insert("subject".into(), bridge["subject"].clone());
    base.insert(
        "candidate_sha".into(),
        Value::String(final_head.to_string()),
    );
    base.insert("target".into(), Value::String(target.to_string()));
    base.insert("receipt_digest".into(), Value::String(digest.to_string()));
    base.insert("court_results".into(), Value::Object(courts_map));
    base.insert(
        "evidence_types".into(),
        Value::Array(types.into_iter().map(Value::String).collect()),
    );
    base.insert(
        "acceptance_results".into(),
        Value::Object(acceptance_results),
    );
    base.insert("falsifier_results".into(), Value::Object(falsifier_results));
    base.insert(
        "receipt_classes".into(),
        Value::Array(classes.into_iter().map(Value::String).collect()),
    );
    base.insert(
        "evidence_ceiling".into(),
        court
            .get("evidence_ceiling")
            .and_then(|v| v.as_str())
            .filter(|s| !s.is_empty())
            .map(|s| Value::String(s.to_string()))
            .unwrap_or_else(|| Value::String(FABRIC_CEILING.into())),
    );
    base.insert("observed_execution".into(), Value::Bool(observed));
    base.insert("inherited_standing".into(), Value::Bool(false));
    // xaas identity echo (line 653).
    let mut xaas_ids = Map::new();
    for k in ["epoch_id", "run_id", "receipt_id"] {
        if let Some(v) = receipt.get(k) {
            xaas_ids.insert(k.to_string(), v.clone());
        }
    }
    base.insert("xaas".into(), Value::Object(xaas_ids));
    // put_court_replay / put_court_authority (lines 752-760).
    if court.get("replay_passed") == Some(&Value::Bool(true))
        && court
            .get("replay_identity")
            .and_then(|v| v.as_str())
            .is_some()
    {
        base.insert("replay_passed".into(), Value::Bool(true));
        base.insert(
            "replay_identity".into(),
            court.get("replay_identity").expect("checked above").clone(),
        );
    }
    if let Some(auth) = court.get("authority_receipt").filter(|v| v.is_object()) {
        base.insert("authority_receipt".into(), auth.clone());
    }
    // put_reconciler_contract (line 657): nested evidence take.
    let evidence_keys = [
        "subject",
        "repository",
        "base_sha",
        "court_results",
        "evidence_types",
        "acceptance_results",
        "falsifier_results",
        "receipt_classes",
        "evidence_ceiling",
        "observed_execution",
        "inherited_standing",
        "replay_passed",
        "replay_identity",
        "authority_receipt",
    ];
    let mut evidence_map = Map::new();
    for k in evidence_keys {
        if let Some(v) = base.get(k) {
            evidence_map.insert(k.to_string(), v.clone());
        }
    }
    base.insert(
        "snapshot_digest".into(),
        base["source_snapshot_digest"].clone(),
    );
    base.insert("evidence".into(), Value::Object(evidence_map));

    Ok(Value::Object(base))
}

fn str_list(v: Option<&Value>) -> Vec<String> {
    v.and_then(|v| v.as_array())
        .map(|a| {
            a.iter()
                .filter_map(|s| s.as_str())
                .map(|s| s.to_string())
                .collect()
        })
        .unwrap_or_default()
}
