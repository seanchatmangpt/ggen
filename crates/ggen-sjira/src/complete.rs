//! The project-completion driver — Rust port of
//! `GgenIgniter.SemanticJira.Complete` (complete.ex, 349 lines), PG-2
//! Slice 4. The loop: over an admitted work-order backlog, recompute the
//! frontier over the CURRENT ledger, drive the FIRST eligible identity
//! through the execution chain (`execute::run` over that single-order
//! slice — its own frontier loop closes exactly that order and returns
//! Ok, or refuses with the ledger byte-unchanged), then recheck. Zero
//! shell-outs: the whole loop is function calls into
//! [`crate::ledger::fetch`], [`crate::frontier::frontier`], and
//! [`crate::execute::run`];
//! eligibility seen here is eligibility execution enforces — so strictly
//! that a per-order refusal mid-loop is structurally unreachable through
//! this composition (the driver and the executor re-derive the identical
//! frontier from the same ledger); the branch exists fail-closed anyway.
//!
//! Exit vocabulary (`run` returns `(u8, Value)`):
//!
//! * `0` — `complete`: the frontier is empty. Blocked orders are listed
//!   with the kernel's own reason, minus the identities this run closed
//!   (an order this run closed stands PARTIAL_ALIVE and is settled, not
//!   blocked).
//! * `1` — `refused`, one of: `order_refused` (a per-order exit refused;
//!   the ledger holds only orders closed before the refusal), `max_
//!   iterations_exhausted` (with `remaining_eligible`), `frontier_refused`
//!   (the standing ledger refused — tampered/undecodable — before
//!   anything ran), `authority_index_unavailable` (no authority graph
//!   was supplied).
//! * `2` — `invalid_invocation`, before the loop starts: the pack-source
//!   exclusivity boundary (`pack_dir` XOR `packs_root`, the same boundary
//!   as Execute.run/1's backend exclusivity).
//!
//! Fail-closed laws:
//!
//! * The FIRST per-order exit-1 refusal STOPS the loop.
//! * `max_iterations` (default 1000, complete.ex line 49) bounds the
//!   loop; exhausting it with eligible work remaining is a typed exit-1
//!   refusal, never a silent partial.
//! * An exit-2 invalid invocation happens before any ledger IO.
//!
//! Disclosed deferred scope (PG-2):
//!
//! * The additive security pre-step (complete.ex lines 169-245,
//!   `SecurityReview.run/2`) is OUT — ggen-engine owns diff review;
//!   this kernel has no worktree to gate.
//! * Pack dirs are CARRIED, not actuated: the local kernel has no pack
//!   execution backend, so the per-order resolution
//!   (`packs_root/<target_pack>`) is computed and surfaced in the
//!   per-order receipt echo (`resolved_pack_dir`), never consumed. The
//!   target pack rides `CompleteOpts.target_packs` (identity-keyed) —
//!   the typed `WorkOrder` refuses unknown fields, so the Elixir
//!   `order["target_pack"]` shape does not port literally (disclosed
//!   deviation at [`CompleteOpts::target_packs`]).
//! * The authority graph is supplied typed
//!   ([`AuthorityGraph`]); file parsing lives in
//!   `authority.rs` (`from_path`/`from_json`), where a
//!   present-but-unparseable graph already refuses typed.
//!
//! Modeled on `ggen_igniter/lib/ggen_igniter/semantic_jira/complete.ex`
//! (the port spec).

use std::collections::BTreeMap;
use std::path::{Path, PathBuf};

use serde_json::{json, Map, Value};

use crate::authority::AuthorityGraph;
use crate::execute::{self, ExecuteOpts};
use crate::frontier::{frontier, FrontierInput, TransitionEvent};
use crate::ledger;

/// Default iteration budget (complete.ex line 49: `@default_max 1000`).
pub const DEFAULT_MAX: usize = 1000;

/// Options for [`run`].
#[derive(Debug, Clone, Default)]
pub struct CompleteOpts {
    /// Iteration budget (`--max`); each iteration closes at most one
    /// order. Exhausting it with eligible work remaining is a typed
    /// exit-1 refusal (`max_iterations_exhausted`, with
    /// `remaining_eligible`).
    pub max_iterations: Option<usize>,
    /// The fixed pack directory (`--pack-dir`). Mutually exclusive with
    /// `packs_root` — both present is exit 2.
    pub pack_dir: Option<PathBuf>,
    /// Per-order pack resolution root (`--packs-root`). Mutually
    /// exclusive with `pack_dir` — both present is exit 2.
    pub packs_root: Option<PathBuf>,
    /// Per-order pack resolution (`sj:targetPack`), keyed by order
    /// identity. DISCLOSED DEVIATION from complete.ex lines 135-153:
    /// there the target pack rides the loose order map
    /// (`order["target_pack"]`); the typed Rust `WorkOrder` refuses
    /// unknown fields (`deny_unknown_fields`), so the resolution input
    /// rides the opts instead. The resolution itself is unchanged:
    /// `<packs_root>/<target_pack>` when the identity names a pack.
    pub target_packs: BTreeMap<String, String>,
}

/// The completion receipt one closed order echoes under `"orders"`:
/// the executed [`ExecutionPlan`](crate::execute::ExecutionPlan) plus the
/// per-order pack resolution this driver computed.
fn order_receipt(plan: &execute::ExecutionPlan, resolved_pack: Option<&Path>) -> Value {
    let mut v = serde_json::to_value(plan).expect("ExecutionPlan is Serialize");
    if let Some(p) = resolved_pack {
        v["resolved_pack_dir"] = json!(p.to_string_lossy());
    }
    v
}

/// The per-order receipt file name (complete.ex line 289): the identity
/// with every character outside `[A-Za-z0-9._-]` replaced by `_`.
pub fn sanitize(identity: &str) -> String {
    identity
        .chars()
        .map(|c| {
            if c.is_ascii_alphanumeric() || matches!(c, '.' | '_' | '-') {
                c
            } else {
                '_'
            }
        })
        .collect()
}

/// Per-order pack resolution (complete.ex lines 135-153): under
/// `packs_root`, an order naming `sj:targetPack` resolves to
/// `<root>/<target_pack>`; otherwise no resolution (the fixed `pack_dir`
/// wins when the executed order names no target pack). The target pack
/// rides `CompleteOpts.target_packs` (identity-keyed) — see the
/// disclosed deviation there.
fn resolved_pack_dir(opts: &CompleteOpts, input: &FrontierInput) -> Option<PathBuf> {
    let root = opts.packs_root.as_ref()?;
    let target = opts
        .target_packs
        .get(input.work_order["identity"].as_str()?)?;
    Some(root.join(target))
}

/// The typed transition events decoded from the REAL ledger
/// (`TransitionLog.fetch/1` hop). A ledger error, or a transition-shaped
/// event that does not decode, refuses the loop before anything ran.
fn frontier_events(ledger_path: &Path) -> Result<Vec<TransitionEvent>, String> {
    let events = ledger::fetch(ledger_path)
        .map_err(|e| format!("the standing ledger refused before anything ran: {e}"))?;
    let mut typed = Vec::new();
    for e in &events {
        match serde_json::from_value::<TransitionEvent>(e.clone()) {
            Ok(ev) => typed.push(ev),
            Err(_) if is_transition_shape(e) => {
                return Err(
                    "malformed transition in the ledger: a transition-shaped event \
                     does not decode into the typed TransitionEvent"
                        .to_string(),
                );
            }
            Err(_) => continue,
        }
    }
    Ok(typed)
}

fn is_transition_shape(event: &Value) -> bool {
    event.get("work_order_id").is_some() && event.get("to_standing").is_some()
}

fn exit1(reason: &str, extra: Map<String, Value>) -> (u8, Value) {
    let mut m = Map::new();
    m.insert("status".to_string(), json!("refused"));
    m.insert("reason".to_string(), json!(reason));
    for (k, v) in extra {
        m.insert(k, v);
    }
    (1, Value::Object(m))
}

/// The invalid-invocation receipt (exit 2).
fn invalid(reason: &str) -> (u8, Value) {
    (2, json!({"status": "invalid_invocation", "reason": reason}))
}

/// Runs the completion loop: frontier -> execute (one order) -> recheck,
/// over the REAL ledger at `ledger_path`, until the frontier is empty or
/// a typed refusal fires. Returns `(exit, receipt)` — the igniter's
/// three-exit vocabulary. See the module doc for the exact semantics and
/// the disclosed deferred scope.
pub fn run(
    orders: &[FrontierInput], ledger_path: &Path, authority: Option<&AuthorityGraph>,
    opts: &CompleteOpts,
) -> (u8, Value) {
    // ── invocation boundary (before any ledger IO) ────────────────────────
    if opts.pack_dir.is_some() && opts.packs_root.is_some() {
        return invalid("--pack-dir and --packs-root are mutually exclusive");
    }

    let max = opts.max_iterations.unwrap_or(DEFAULT_MAX);

    // The map-membership index frontier() consumes (iri -> admission
    // digest), the ONE origin law (authority.rs build_pinned). None
    // supplied -> authority_index_unavailable, the same boundary
    // Execute.run/1 draws for an unparseable authority graph.
    let map_index = authority.map(|g| {
        let mut m = BTreeMap::new();
        let typed = crate::authority::AuthorityIndex::build_pinned(g);
        for (iri, a) in typed.admitted.iter() {
            m.insert(iri.clone(), a.admission_digest.clone());
        }
        m
    });

    let mut closed: Vec<String> = Vec::new();
    let mut order_receipts: Vec<Value> = Vec::new();

    loop {
        // ── frontier hop over the CURRENT ledger ──────────────────────────
        let events = match frontier_events(ledger_path) {
            Ok(e) => e,
            Err(detail) => return exit1("frontier_refused", detail_map(&detail)),
        };
        let selected = frontier(orders, &events, &BTreeMap::new(), map_index.as_ref());

        // A whole-selection failure (malformed log / missing index) comes
        // back as an identity-less blocked row — refuse, never spin.
        if let Some(head) = selected.blocked.iter().find(|b| b.identity.is_none()) {
            return exit1("frontier_refused", detail_map(&head.reason));
        }

        if authority.is_none() {
            return exit1(
                "authority_index_unavailable",
                detail_map(
                    "no authority graph was supplied: every order is blocked \
                 authority_index_unavailable",
                ),
            );
        }

        match selected.eligible.len() {
            0 => {
                // An order this run closed stands PARTIAL_ALIVE, so the
                // projection lists it under blocked — those rows are
                // SETTLED, not blocked (complete.ex lines 247-264).
                let open_blocked: Vec<Value> = selected
                    .blocked
                    .iter()
                    .filter(|b| {
                        b.identity
                            .as_ref()
                            .map(|i| !closed.contains(i))
                            .unwrap_or(false)
                    })
                    .map(|b| serde_json::to_value(b).expect("BlockedEntry is Serialize"))
                    .collect();
                return (
                    0,
                    json!({
                        "status": "complete",
                        "closed": closed,
                        "orders": order_receipts,
                        "blocked": open_blocked,
                        "ledger": ledger_path.to_string_lossy(),
                        "closed_count": closed.len(),
                        "blocked_count": open_blocked.len(),
                    }),
                );
            }
            _ if closed.len() >= max => {
                let mut extra = detail_map(&format!(
                    "--max {max} iterations reached with eligible work remaining"
                ));
                extra.insert(
                    "remaining_eligible".to_string(),
                    json!(selected.eligible.len()),
                );
                extra.insert("closed".to_string(), json!(closed));
                return exit1("max_iterations_exhausted", extra);
            }
            _ => {}
        }

        // ── execute the FIRST eligible identity, verbatim ─────────────────
        let candidate = &selected.eligible[0];
        let input = match orders
            .iter()
            .find(|o| raw_identity(o) == Some(candidate.identity.clone()))
        {
            Some(i) => i,
            None => {
                return exit1(
                    "frontier_refused",
                    detail_map(&format!(
                        "eligible candidate {} has no raw work order in the graph",
                        candidate.identity
                    )),
                )
            }
        };

        let resolution = resolved_pack_dir(opts, input);

        // Single-order slice: execute's own loop closes exactly this order
        // (its pass-2 frontier is empty) or refuses with the ledger
        // byte-unchanged — the per-order fail-closed property.
        match execute::run(
            std::slice::from_ref(input),
            ledger_path,
            authority.expect("checked above"),
            &ExecuteOpts::default(),
        ) {
            Ok(executed) if !executed.plans.is_empty() => {
                closed.push(candidate.identity.clone());
                order_receipts.push(order_receipt(&executed.plans[0], resolution.as_deref()));
            }
            Ok(_) => {
                return exit1(
                    "frontier_refused",
                    detail_map("the executor reported an empty plan for an eligible order"),
                )
            }
            Err(refusal) => {
                let mut extra = detail_map(&format!("{refusal}"));
                extra.insert("identity".to_string(), json!(candidate.identity));
                extra.insert("verdict".to_string(), json!(refusal.reason));
                extra.insert("closed".to_string(), json!(closed));
                extra.insert("closed_count".to_string(), json!(closed.len()));
                return exit1("order_refused", extra);
            }
        }
    }
}

fn detail_map(detail: &str) -> Map<String, Value> {
    let mut m = Map::new();
    m.insert("detail".to_string(), json!(detail));
    m
}

fn raw_identity(input: &FrontierInput) -> Option<String> {
    input
        .work_order
        .get("identity")
        .and_then(|v| v.as_str())
        .map(String::from)
}
