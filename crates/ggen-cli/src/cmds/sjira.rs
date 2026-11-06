//! Semantic Jira noun — the CLI surface over `ggen-sjira`'s admission
//! kernel (native port of the ggen_igniter `SemanticJira` core slice).
//!
//! Verbs this slice:
//!
//! * `admit <file>` — admit one WorkOrder JSON file.
//! * `frontier --orders <file> [--events] [--evidence] [--authority]` —
//!   frontier selection over real files.
//! * `execute --orders <file> --ledger <path> --authority <file> [--max N]`
//!   — the kernel's frontier→authority→plan→receipt→reconcile loop over a
//!   REAL file ledger.
//! * `lease-admit --lease <file> --ontology-digest <s> --keys <file>` —
//!   sovereign-ceiling lease admission (multi-party Ed25519).
//! * `epoch-check --order <file> [--watermark <file>]` — the FreshEpoch
//!   gate. Refusals carry the `REFUSED_STALE_LEASE` marker, which the CLI
//!   boundary (src/main.rs, v26.10.20 refusal envelope mapping) maps to
//!   exit 5 (the documented Andon class); every other typed sjira refusal
//!   exits 1 (validation class). Exits 2/3/4 are other error families'
//!   (SparqlError/TemplateError/OutputInvalid) and are never emitted here.
//!
//! `reconcile` is deferred: DS-1's kernel reconcile API has not landed in
//! ggen-sjira yet (no `pub` reconcile entry point in this snapshot); this
//! verb is added when the kernel surface lands.
//!
//! Every verb is a thin wrapper: read real files, call the kernel pub API,
//! render the typed result or the typed refusal (with remediation) — the
//! kernel decides, the CLI never re-implements admission law.

use clap_noun_verb::{NounVerbError, Result};
use clap_noun_verb_macros::verb;
use serde_json::Value;
use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use ggen_sjira::epoch::Refusal as EpochRefusal;
use ggen_sjira::frontier::{DependencyEdge, FrontierInput};
use ggen_sjira::{
    execute, frontier as kernel_frontier, lease, AdmittedWorkOrder, Evidence, FrontierResult,
    TransitionEvent,
};

// ── refusal helpers ───────────────────────────────────────────────────────

/// A typed kernel refusal surfaces as exit 1 at the CLI boundary (the
/// validation class — exit 2 is SparqlError's and is never emitted by
/// sjira verbs). The `REFUSED:SEMANTIC_JIRA` prefix keeps the refusal
/// machine-greppable in output.
fn refused(msg: impl std::fmt::Display) -> NounVerbError {
    NounVerbError::argument_error(format!("REFUSED:SEMANTIC_JIRA: {msg}"))
}

/// Epoch-gate refusals carry the `REFUSED_STALE_LEASE` marker so the CLI
/// boundary's existing v26.10.20 mapping (src/main.rs) exits 5 — the
/// documented Andon signal for a stale epoch, not a retryable warning.
fn refused_stale_lease(msg: impl std::fmt::Display) -> NounVerbError {
    NounVerbError::argument_error(format!("REFUSED_STALE_LEASE:SEMANTIC_JIRA: {msg}"))
}

/// Read one real JSON file (fail closed on unreadable / non-JSON).
fn read_json(path: &Path, what: &str) -> Result<Value> {
    let bytes = std::fs::read(path).map_err(|e| {
        NounVerbError::argument_error(format!("cannot read {what} {}: {e}", path.display()))
    })?;
    serde_json::from_slice(&bytes).map_err(|e| {
        NounVerbError::argument_error(format!(
            "not valid JSON in {} ({}): {e}",
            path.display(),
            what
        ))
    })
}

// ── decoding: CLI JSON shapes → kernel types ─────────────────────────────

/// Decode the frontier/execute `--orders` file: a JSON array of
/// `{work_order: {...}, dependencies?: [...], path_scope?: [...],
/// concurrency_key?: "..."}` objects. `dependencies` elements are the
/// kernel's typed `DependencyEdge` (decode through serde, so a malformed
/// edge fails closed); the other two are optional with kernel-honest
/// defaults.
fn decode_orders(value: &Value) -> Result<Vec<FrontierInput>> {
    let arr = value.as_array().ok_or_else(|| {
        NounVerbError::argument_error(
            "orders file must be a JSON array of {work_order, dependencies?, \
             path_scope?, concurrency_key?} objects",
        )
    })?;
    let mut inputs = Vec::with_capacity(arr.len());
    for (i, el) in arr.iter().enumerate() {
        let work_order = el
            .get("work_order")
            .ok_or_else(|| refused(format!("orders[{i}]: missing `work_order` object")))?;
        let dependencies = match el.get("dependencies") {
            Some(v) => serde_json::from_value::<Vec<DependencyEdge>>(v.clone())
                .map_err(|e| refused(format!("orders[{i}].dependencies: {e}")))?,
            None => Vec::new(),
        };
        let path_scope: Vec<String> = match el.get("path_scope") {
            Some(v) => serde_json::from_value(v.clone())
                .map_err(|e| refused(format!("orders[{i}].path_scope: {e}")))?,
            None => Vec::new(),
        };
        let concurrency_key = el
            .get("concurrency_key")
            .and_then(|v| v.as_str())
            .unwrap_or("global")
            .to_string();
        inputs.push(FrontierInput {
            work_order: work_order.clone(),
            dependencies,
            path_scope,
            concurrency_key,
        });
    }
    Ok(inputs)
}

/// Decode the optional `--events` file: a JSON array of typed
/// `TransitionEvent`s. A malformed event fails closed (the kernel's
/// fail-closed law, enforced at decode time).
fn decode_events(value: &Value) -> Result<Vec<TransitionEvent>> {
    serde_json::from_value(value.clone()).map_err(|e| refused(format!("events file: {e}")))
}

/// Decode the optional `--evidence` file: a JSON object mapping upstream
/// work-order identity → `{standing?, receipt_digest?}`.
fn decode_evidence(value: Option<&Value>) -> Result<BTreeMap<String, Evidence>> {
    let Some(v) = value else {
        return Ok(BTreeMap::new());
    };
    let obj = v.as_object().ok_or_else(|| {
        refused("evidence file must be a JSON object of identity → {standing?, receipt_digest?}")
    })?;
    let mut out = BTreeMap::new();
    for (id, ev) in obj {
        let standing = ev
            .get("standing")
            .and_then(|s| s.as_str())
            .map(String::from);
        let receipt_digest = ev
            .get("receipt_digest")
            .and_then(|s| s.as_str())
            .map(String::from);
        out.insert(
            id.clone(),
            Evidence {
                standing,
                receipt_digest,
            },
        );
    }
    Ok(out)
}

/// Decode the optional `--authority` file for `frontier`: a JSON object
/// mapping origin-authority IRI → admission digest (the kernel's
/// map-membership `AuthorityIndex`).
fn decode_authority_index(value: Option<&Value>) -> Result<Option<BTreeMap<String, String>>> {
    let Some(v) = value else {
        return Ok(None);
    };
    let obj = v
        .as_object()
        .ok_or_else(|| refused("authority file must be a JSON object of IRI → admission digest"))?;
    let mut index = BTreeMap::new();
    for (iri, digest) in obj {
        let Some(d) = digest.as_str() else {
            return Err(refused(format!(
                "authority[\"{iri}\"]: digest must be a string"
            )));
        };
        index.insert(iri.clone(), d.to_string());
    }
    Ok(Some(index))
}

/// Decode `SovereignLease` from JSON. The kernel type is
/// `Serialize`-only, so the CLI owns this decode; unknown keys are
/// ignored, missing keys refuse.
fn decode_lease(v: &Value) -> Result<lease::SovereignLease> {
    let obj = v
        .as_object()
        .ok_or_else(|| refused("lease must be a JSON object"))?;
    let get_str = |k: &str| {
        obj.get(k)
            .and_then(|x| x.as_str())
            .map(String::from)
            .ok_or_else(|| refused(format!("lease: missing string field `{k}`")))
    };
    let signatures = match obj.get("signatures") {
        Some(Value::Array(sigs)) => {
            let mut out = Vec::new();
            for s in sigs {
                let signer = s
                    .get("signer")
                    .and_then(|x| x.as_str())
                    .ok_or_else(|| refused("lease.signatures[i]: missing `signer`"))?;
                let alg = s
                    .get("alg")
                    .and_then(|x| x.as_str())
                    .ok_or_else(|| refused("lease.signatures[i]: missing `alg`"))?;
                let sig = s
                    .get("sig")
                    .and_then(|x| x.as_str())
                    .ok_or_else(|| refused("lease.signatures[i]: missing `sig`"))?;
                out.push(lease::SignatureRecord {
                    signer: signer.to_string(),
                    alg: alg.to_string(),
                    sig: sig.to_string(),
                });
            }
            out
        }
        _ => return Err(refused("lease: missing `signatures` array")),
    };
    Ok(lease::SovereignLease {
        lease_id: get_str("lease_id")?,
        subject_iri: get_str("subject_iri")?,
        shapes: obj
            .get("shapes")
            .and_then(|x| x.as_array())
            .map(|a| {
                a.iter()
                    .filter_map(|s| s.as_str().map(String::from))
                    .collect()
            })
            .unwrap_or_default(),
        ontology_digest: get_str("ontology_digest")?,
        granted_at: get_str("granted_at")?,
        signatures,
    })
}

/// Decode the `--keys` file for `lease-admit`: a JSON object mapping
/// signer id → lowercase-hex Ed25519 verifying key (32 bytes).
fn decode_verifying_keys(v: &Value) -> Result<BTreeMap<String, ed25519_dalek::VerifyingKey>> {
    let obj = v.as_object().ok_or_else(|| {
        refused("keys file must be a JSON object of signer → hex Ed25519 verifying key")
    })?;
    let mut keys = BTreeMap::new();
    for (signer, hex) in obj {
        let Some(h) = hex.as_str() else {
            return Err(refused(format!("keys[\"{signer}\"]: must be a hex string")));
        };
        let bytes = unhex(h)
            .ok_or_else(|| refused(format!("keys[\"{signer}\"]: not valid lowercase hex")))?;
        let len = bytes.len();
        let arr: [u8; 32] = bytes.try_into().map_err(|_| {
            refused(format!(
                "keys[\"{signer}\"]: Ed25519 verifying key must be 32 bytes, got {len}"
            ))
        })?;
        let key = ed25519_dalek::VerifyingKey::from_bytes(&arr)
            .map_err(|e| refused(format!("keys[\"{signer}\"]: {e}")))?;
        keys.insert(signer.clone(), key);
    }
    Ok(keys)
}

/// Lowercase-hex decode (no hex crate in this crate's dep tree).
fn unhex(s: &str) -> Option<Vec<u8>> {
    if s.len() % 2 != 0 || !s.bytes().all(|b| b.is_ascii_hexdigit()) {
        return None;
    }
    (0..s.len())
        .step_by(2)
        .map(|i| u8::from_str_radix(&s[i..i + 2], 16).ok())
        .collect()
}

// ── result rendering ──────────────────────────────────────────────────────

/// Render a frontier result as JSON (eligible / blocked / standings).
/// Built field-wise: `EligibleCandidate`/`BlockedEntry` are `Serialize`,
/// `FrontierResult` itself is not.
fn render_frontier(result: &FrontierResult) -> Value {
    let eligible: Vec<Value> = result
        .eligible
        .iter()
        .map(|c| serde_json::to_value(c).expect("EligibleCandidate serializes infallibly"))
        .collect();
    let blocked: Vec<Value> = result
        .blocked
        .iter()
        .map(|b| serde_json::to_value(b).expect("BlockedEntry serializes infallibly"))
        .collect();
    let standings: Value = result
        .standings
        .iter()
        .map(|(k, v)| (k.clone(), Value::from(v.as_str())))
        .collect::<serde_json::Map<String, Value>>()
        .into();
    serde_json::json!({
        "eligible": eligible,
        "blocked": blocked,
        "standings": standings,
    })
}

/// Decode an `AdmittedLease` JSON file (the kernel type is
/// `Serialize`-only; the CLI owns the decode, fail closed on missing
/// fields).
fn decode_admitted_lease(v: &Value) -> Result<lease::AdmittedLease> {
    let obj = v
        .as_object()
        .ok_or_else(|| refused("admitted lease must be a JSON object"))?;
    let get_str = |k: &str| {
        obj.get(k)
            .and_then(|x| x.as_str())
            .map(String::from)
            .ok_or_else(|| refused(format!("admitted lease: missing string field `{k}`")))
    };
    let get_bool = |k: &str| {
        obj.get(k)
            .and_then(|x| x.as_bool())
            .ok_or_else(|| refused(format!("admitted lease: missing bool field `{k}`")))
    };
    Ok(lease::AdmittedLease {
        lease_id: get_str("lease_id")?,
        subject_iri: get_str("subject_iri")?,
        shapes: obj
            .get("shapes")
            .and_then(|x| x.as_array())
            .map(|a| {
                a.iter()
                    .filter_map(|s| s.as_str().map(String::from))
                    .collect()
            })
            .unwrap_or_default(),
        ontology_digest: get_str("ontology_digest")?,
        granted_at: get_str("granted_at")?,
        verified_signers: obj
            .get("verified_signers")
            .and_then(|x| x.as_array())
            .map(|a| {
                a.iter()
                    .filter_map(|s| s.as_str().map(String::from))
                    .collect()
            })
            .unwrap_or_default(),
        admitted: get_bool("admitted")?,
    })
}

// ── verbs ─────────────────────────────────────────────────────────────────

/// `ggen sjira admit <file>`: admit one WorkOrder JSON file without
/// creating authority. The error path carries the kernel's typed refusal
/// Display (including remediation text).
#[verb]
pub fn admit(#[arg(index = 1)] file: PathBuf) -> Result<Value> {
    let raw = read_json(&file, "work order")?;

    match ggen_sjira::admit_work_order(&raw) {
        Ok(admitted) => Ok(render_admitted(&admitted)),
        Err(refusal) => Err(refused(format!("MalformedWorkOrder refused: {refusal}"))),
    }
}

fn render_admitted(admitted: &AdmittedWorkOrder) -> Value {
    serde_json::json!({
        "admitted": true,
        "authority": admitted.authority,
        "work_order_digest": admitted.work_order_digest,
        "definition_digest": admitted.definition_digest,
        "identity": admitted.work_order.identity,
        "standing": admitted.work_order.standing.as_str(),
    })
}

/// `ggen sjira frontier`: frontier selection over real files.
///
/// With no `--authority` file the kernel fails closed (every order blocked with
/// `authority_index_unavailable`) — surfaced as a successful JSON result,
/// because "blocked" IS the kernel's typed answer, not a CLI error.
#[verb]
pub fn frontier(
    orders_path: PathBuf, events_path: Option<PathBuf>, evidence_path: Option<PathBuf>,
    authority_path: Option<PathBuf>,
) -> Result<Value> {
    let orders = decode_orders(&read_json(&orders_path, "orders")?)?;
    let events = match &events_path {
        Some(p) => decode_events(&read_json(p, "events")?)?,
        None => Vec::new(),
    };
    let evidence = decode_evidence(read_json_opt(evidence_path.as_deref())?.as_ref())?;
    let authority = decode_authority_index(read_json_opt(authority_path.as_deref())?.as_ref())?;

    let result = kernel_frontier(&orders, &events, &evidence, authority.as_ref());
    let eligible_count = result.eligible.len();
    let mut out = render_frontier(&result);
    out["eligible_count"] = Value::from(eligible_count);
    Ok(out)
}

/// Read a file that may legitimately not exist (optional inputs). A
/// present-but-unreadable or non-JSON file still fails closed.
fn read_json_opt(path: Option<&Path>) -> Result<Option<Value>> {
    match path {
        None => Ok(None),
        Some(p) => {
            if !p.exists() {
                return Err(NounVerbError::argument_error(format!(
                    "cannot read {}: no such file",
                    p.display()
                )));
            }
            Ok(Some(read_json(p, "input")?))
        }
    }
}

/// `ggen sjira execute`: the kernel's execution loop over a real ledger.
///
/// Runs frontier → authority → plan → receipt → reconcile to fixed
/// point. `--max` bounds the loop passes (kernel default 64). The
/// reconcile hop appends to the ledger — a refusal before reconcile
/// leaves it byte-unchanged.
#[verb]
pub fn execute(
    orders_path: PathBuf, ledger_path: PathBuf, authority_path: PathBuf, max: Option<usize>,
    lease_path: Option<PathBuf>, epoch_watermark_path: Option<PathBuf>,
    epoch_extensions_path: Option<PathBuf>,
) -> Result<Value> {
    execute_impl(
        orders_path,
        ledger_path,
        authority_path,
        max,
        lease_path,
        epoch_watermark_path,
        epoch_extensions_path,
    )
}

/// Domain helper for the execute verb (the Poka-Yoke verb-complexity
/// guard caps the #[verb] fn itself; the wrapper stays one delegation).
#[allow(clippy::too_many_arguments)]
fn execute_impl(
    orders_path: PathBuf, ledger_path: PathBuf, authority_path: PathBuf, max: Option<usize>,
    lease_path: Option<PathBuf>, epoch_watermark_path: Option<PathBuf>,
    epoch_extensions_path: Option<PathBuf>,
) -> Result<Value> {
    let orders = decode_orders(&read_json(&orders_path, "orders")?)?;
    let authority_value = read_json(&authority_path, "authority graph")?;
    let graph = ggen_sjira::authority::AuthorityGraph::from_json(&authority_value)
        .map_err(|e| refused(format!("authority graph: {e}")))?;
    let mut opts = execute::ExecuteOpts::legacy();
    opts.max_iterations = max.unwrap_or(execute::DEFAULT_MAX_ITERATIONS);
    if let Some(p) = &lease_path {
        let v = read_json(p, "admitted lease")?;
        opts.lease = Some(decode_admitted_lease(&v)?);
    }
    if let Some(p) = &epoch_watermark_path {
        opts.epoch_watermark = Some(read_json(p, "epoch watermark")?);
    }
    if let Some(p) = &epoch_extensions_path {
        let v = read_json(p, "epoch extensions")?;
        let obj = v.as_object().ok_or_else(|| {
            refused(
                "epoch extensions file must be a JSON object of identity → \
                 {epoch, plan_touches, manufacture_plan, source_artifacts}",
            )
        })?;
        let mut ext = BTreeMap::new();
        for (id, e) in obj {
            ext.insert(id.clone(), e.clone());
        }
        opts.epoch_extensions = ext;
    }

    match execute::run(&orders, &ledger_path, &graph, &opts) {
        Ok(executed) => Ok(serde_json::json!({
            "executed": executed.plans.len(),
            "iterations": executed.iterations,
            "blocked": executed.blocked,
            "standings": executed.standings,
            "journal_head": executed.journal.head(),
            "journal_len": executed.journal.len(),
            "events": executed.events,
        })),
        Err(r) => Err(refused(format!(
            "execution refused at hop `{}` reason={} ({}): {}",
            r.hop, r.reason, r.broken_term, r.detail
        ))),
    }
}

/// `ggen sjira lease-admit`: admit a sovereign-ceiling lease.
///
/// Admits the lease against the current pack ontology digest, verifying
/// every signature against the supplied public keys. Multi-party law: >= 2
/// signatures from >= 2 distinct signers, Ed25519 over the canonical
/// lease bytes.
#[verb]
pub fn lease_admit(
    lease_path: PathBuf, ontology_digest: String, keys_path: PathBuf,
) -> Result<Value> {
    let lease_value = read_json(&lease_path, "lease")?;
    let lease = decode_lease(&lease_value)?;
    let keys = decode_verifying_keys(&read_json(&keys_path, "keys")?)?;

    match lease::admit(&lease, &ontology_digest, &keys) {
        Ok(admitted) => Ok(admitted.to_json()),
        Err(r) => Err(refused(format!("sovereign lease refused: {r}"))),
    }
}

/// `ggen sjira epoch-check`: judge one order against a stamped watermark.
///
/// With no `--watermark` and an order that declares `epoch`, the gate
/// refuses `EPOCH_WATERMARK_UNAVAILABLE` (fail closed). Refusals exit 5
/// at the CLI boundary (the documented stale-epoch Andon class).
#[verb]
pub fn epoch_check(order_path: PathBuf, watermark_path: Option<PathBuf>) -> Result<Value> {
    let order = read_json(&order_path, "order")?;
    let watermark = read_json_opt(watermark_path.as_deref())?;

    match ggen_sjira::epoch::check(&order, watermark.as_ref()) {
        Ok(()) => Ok(serde_json::json!({"epoch": "ok"})),
        Err(code) => Err(refused_stale_lease(format!(
            "epoch gate refused ({}): {}",
            code,
            match code {
                EpochRefusal::LegacyEdit =>
                    "the plan edits a legacy (pre-watermark) file without a fresh \
                     manufacture plan — supply `manufacture_plan` or drop the path",
                EpochRefusal::UnattributedImplementation =>
                    "the plan touches a non-legacy file inside an implementation \
                     glob without a fresh manufacture plan",
                EpochRefusal::ReusesPreWatermarkArtifact =>
                    "a source artifact predates the watermark and is not freshly \
                     manufactured — regenerate it under this epoch",
                EpochRefusal::WatermarkUnavailable =>
                    "the order declares `epoch` but no watermark was supplied — \
                     pass --watermark <file>",
            }
        ))),
    }
}
